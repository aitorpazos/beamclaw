# BeamClaw
A fault-tolerant AI agent gateway and MCP host implemented in Erlang/OTP. Inspired by OpenClaw.

## Product Requirements
- Lean, secure, fault-tolerant, easy to use AI agent.
- Low memory footprint
- Multi-user support, session separation.

## Technical Design

### Technology Stack
Erlang/OTP 28
Build: rebar3

### Archicture Plan
Umbrella project, multi-app design. Main apps:

beamclaw_core: The "Brain." It handles the conversation state, LLM API calls, and the agentic loop.

beamclaw_mcp: The "Connector." It manages the Model Context Protocol (MCP) state, connecting to external tool servers.

beamclaw_gateway: The "Interface." This handles webhooks for WhatsApp, Slack, or Telegram.

## MVP (Minimum Viable Product)

- Support for OpenRouter, OpenAI
- Telegram integration
- TUI (Terminal User Interface)
- Basic tools e.g. terminal command, curl, jq, bash etc.
- MCP support
- Logging
