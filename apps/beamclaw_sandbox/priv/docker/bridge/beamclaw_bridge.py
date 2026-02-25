"""
BeamClaw bridge — tool discovery and invocation from sandbox scripts.

The bridge communicates with the BeamClaw host via a Unix domain socket
using length-prefixed JSON-RPC 2.0 messages.

Usage inside sandbox scripts:

    from beamclaw_bridge import search_tools, get_tool, call_tool

    # Progressive discovery
    names = search_tools("names")            # just tool names
    descs = search_tools("descriptions")     # name -> description
    schemas = search_tools("schemas")        # name -> full schema

    # Get specific tool schema
    schema = get_tool("read_file")

    # Call a tool
    result = call_tool("read_file", path="/etc/hostname")

Also supports filesystem-based discovery when /tools/ is mounted:

    names = search_tools("names")  # reads /tools/index.txt if available
"""

import json
import os
import socket
import struct

BRIDGE_SOCKET = os.environ.get("BEAMCLAW_BRIDGE_SOCKET", "/tmp/bridge.sock")
TOOLS_DIR = "/tools"

_next_id = 0


def _send_rpc(method, params=None):
    """Send a JSON-RPC 2.0 request over the Unix bridge socket."""
    global _next_id
    _next_id += 1

    request = {
        "jsonrpc": "2.0",
        "id": _next_id,
        "method": method,
        "params": params or {},
    }

    payload = json.dumps(request).encode("utf-8")
    frame = struct.pack(">I", len(payload)) + payload

    sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    try:
        sock.connect(BRIDGE_SOCKET)
        sock.sendall(frame)

        # Read length-prefixed response
        header = _recv_exact(sock, 4)
        if header is None:
            raise ConnectionError("Bridge closed connection")
        resp_len = struct.unpack(">I", header)[0]
        resp_data = _recv_exact(sock, resp_len)
        if resp_data is None:
            raise ConnectionError("Bridge closed connection during response")

        response = json.loads(resp_data.decode("utf-8"))
        if "error" in response:
            err = response["error"]
            raise RuntimeError(f"Bridge error {err.get('code')}: {err.get('message')}")
        return response.get("result")
    finally:
        sock.close()


def _recv_exact(sock, n):
    """Receive exactly n bytes from socket."""
    data = b""
    while len(data) < n:
        chunk = sock.recv(n - len(data))
        if not chunk:
            return None
        data += chunk
    return data


def search_tools(detail="names"):
    """
    Progressive tool discovery.

    Args:
        detail: Level of detail.
            "names" - list of tool name strings
            "descriptions" - dict of name -> description
            "schemas" - dict of name -> full JSON schema

    Returns:
        Tool information at the requested detail level.
    """
    # Try filesystem first (faster, no socket needed)
    if detail == "names" and os.path.exists(os.path.join(TOOLS_DIR, "index.txt")):
        with open(os.path.join(TOOLS_DIR, "index.txt")) as f:
            return [line.strip() for line in f if line.strip()]

    if detail == "descriptions" and os.path.isdir(TOOLS_DIR):
        names = search_tools("names")
        result = {}
        for name in names:
            desc_path = os.path.join(TOOLS_DIR, name, "description.txt")
            if os.path.exists(desc_path):
                with open(desc_path) as f:
                    result[name] = f.read().strip()
        if result:
            return result

    if detail == "schemas" and os.path.isdir(TOOLS_DIR):
        names = search_tools("names")
        result = {}
        for name in names:
            schema_path = os.path.join(TOOLS_DIR, name, "schema.json")
            if os.path.exists(schema_path):
                with open(schema_path) as f:
                    result[name] = json.load(f)
        if result:
            return result

    # Fall back to bridge socket
    return _send_rpc("search_tools", {"detail": detail})


def get_tool(name):
    """
    Get the full schema for a specific tool.

    Args:
        name: Tool name string.

    Returns:
        Tool definition dict with name, description, parameters.
    """
    # Try filesystem first
    schema_path = os.path.join(TOOLS_DIR, name, "schema.json")
    if os.path.exists(schema_path):
        with open(schema_path) as f:
            return json.load(f)

    return _send_rpc("get_tool_schema", {"name": name})


def call_tool(name, **kwargs):
    """
    Call a tool through the BeamClaw bridge.

    Args:
        name: Tool name string.
        **kwargs: Tool arguments.

    Returns:
        Tool execution result (string).
    """
    return _send_rpc("call_tool", {"name": name, "args": kwargs})
