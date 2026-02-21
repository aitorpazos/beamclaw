# Contributing to BeamClaw

BeamClaw is a fault-tolerant, security-conscious AI agent gateway and MCP host built in
Erlang/OTP 28. Contributions — code, tests, documentation, bug reports — are welcome.

---

## Code of Conduct

Be respectful and inclusive. Critique ideas, not people. Assume good faith.
Harassment or discrimination of any kind will not be tolerated.

---

## Getting Started

**Prerequisites**

- Erlang/OTP 28 (recommended: install via [asdf](https://asdf-vm.com/) with the
  `asdf-erlang` plugin)
- [rebar3](https://rebar3.org/) (3.23 or newer)

Full setup instructions are in [`docs/building.md`](docs/building.md).

**Bootstrap**

```bash
git clone https://github.com/yourorg/beamclaw.git
cd beamclaw
rebar3 compile
```

**Start the dev shell**

```bash
rebar3 shell
```

See [`docs/running.md`](docs/running.md) for channel configuration and MCP server setup.

---

## Development Workflow

Every PR must pass all four gates before merging:

```bash
rebar3 eunit       # all tests must pass; no new failures
rebar3 dialyzer    # must be 0 warnings (PLT built on first run, takes ~5 min)
rebar3 lint        # Elvis; must be clean
rebar3 release     # release must assemble without errors
```

Run them in the order shown above. Dialyzer requires a compiled PLT; if you have not
run it before, allow extra time for the initial build.

---

## Coding Standards

### Erlang/OTP conventions

- **Module prefix**: all internal modules are named `bc_<name>`.
- **OTP patterns**: use `gen_server`, `gen_statem`, `supervisor`, and friends. Avoid
  raw `spawn` unless you own the process lifecycle completely.
- **Public APIs**: every gen_server exposes named wrapper functions. Do not call
  `gen_server:call/2` from outside the owning module.
- **Behaviours**: document callbacks with `-callback` specs. Do not declare a custom
  behaviour on a module that already declares `gen_server` or `gen_statem` (avoids
  conflicting-callbacks warnings from OTP).

### Data types and records

- Records are defined in `apps/beamclaw_core/include/bc_types.hrl`.
- Include them with: `-include_lib("beamclaw_core/include/bc_types.hrl").`
- Config keys: atoms. Session IDs: binaries (UUID v4).

### Dependency graph

The six OTP apps form a strictly acyclic dependency chain:

```
beamclaw_obs → beamclaw_memory → beamclaw_tools → beamclaw_mcp
→ beamclaw_core → beamclaw_gateway
```

`beamclaw_obs` has **zero** sibling dependencies. Never introduce a cycle.
Consult [`DECISIONS.md`](DECISIONS.md) before adding a new inter-app dependency.

### Dialyzer and lint

- Do not add a `-dialyzer` suppression annotation without a comment explaining why
  the suppression is necessary and why fixing it in the type system would be wrong.
- Do not disable an Elvis lint rule without a comment explaining the intentional
  pattern that triggers it.

---

## Security Rules

Security is a first-class project goal. These rules are non-negotiable.

- **No hardcoded secrets.** API keys, tokens, and passwords must use
  `{env, "VAR_NAME"}` tuples in `config/sys.config`, resolved at runtime by
  `bc_config:get/2`.
- **`bc_scrubber:scrub/1` on every tool result.** Every value returned by a tool
  and every LLM response must pass through `bc_scrubber` before being stored in
  session history or logged.
- **Observability events must not contain raw secrets.** If you add a
  `bc_obs:emit/2` call that includes `args` or `result` fields, ensure the values
  have been scrubbed first.
- **New API integrations**: if the new service's keys have a recognizable prefix
  (e.g. `xoxb-` for Slack), add a pattern to `bc_scrubber` in the same PR.

### PR security checklist

Before marking a PR ready for review, confirm:

- [ ] No hardcoded secrets in `*.erl`, `*.config`, `*.hrl`, or docs
- [ ] `bc_scrubber:scrub/1` called on all new tool result paths
- [ ] New API keys have a scrubber pattern if the key format has a recognizable prefix
- [ ] `{env, "VAR"}` used for any new config secret value
- [ ] No new files that could contain real secrets (check `git diff --name-only`)

Full detail is in [`CLAUDE.md`](CLAUDE.md) under **Security Rules**.

---

## Architectural Rules

- **Supervision trees**: read [`docs/architecture.md`](docs/architecture.md) and
  [`CLAUDE.md`](CLAUDE.md) before touching supervision trees or `bc_loop`. The
  `bc_session` (permanent) / `bc_loop` (transient) split is intentional — see
  [ADR-003](DECISIONS.md#adr-003).
- **New OTP apps**: must fit the acyclic dependency graph; open an issue or draft
  PR for discussion before adding a new app.
- **Architectural decisions**: significant design changes require a new ADR entry in
  [`DECISIONS.md`](DECISIONS.md). Add the ADR before merging, not after. Reference
  the ADR number in the PR description.
- **New behaviours**: export only `-callback` specs and helper functions from
  behaviour modules.

---

## Submitting a Pull Request

**Branch naming**

| Type | Pattern |
|------|---------|
| Feature | `feature/<short-name>` |
| Bug fix | `fix/<short-name>` |
| Docs | `docs/<short-name>` |
| Tests | `test/<short-name>` |

**Commit style**

- Subject line: imperative mood, ≤72 characters (e.g. `Add bc_tool_read_file`).
- Body: explain _why_, not _what_. The diff shows what changed.
- One logical change per commit.

**PR expectations**

- One logical change per PR. Test-only PRs are explicitly welcome.
- All four gates (`eunit`, `dialyzer`, `lint`, `release`) must be green.
- Update [`STATUS.md`](STATUS.md) if the PR completes or advances a milestone task.
- Reference any relevant ADR in the PR description.
- Architectural changes must include or reference an ADR in [`DECISIONS.md`](DECISIONS.md).

---

## Reporting Bugs and Feature Requests

**Bugs**

Open a GitHub issue and include:

- Erlang/OTP version (`erl -version`)
- rebar3 version (`rebar3 --version`)
- Minimal reproduction steps or a failing test case
- Any relevant log output (scrub secrets before pasting)

**Feature requests**

Open a GitHub issue labelled `proposal`. If the feature touches the architecture
(new app, new behaviour, new inter-app dependency), reference or draft an ADR.

---

## AI-Assisted Contributions

BeamClaw is itself an AI agent gateway, so AI-assisted development is both expected
and welcome. This section is the canonical guidance for AI agents contributing to the
project.

**Read these three files before making any change:**

1. [`CLAUDE.md`](CLAUDE.md) — architecture, security rules, coding conventions, and
   the PR checklist. This is the single source of truth for the project's conventions.
2. [`STATUS.md`](STATUS.md) — current milestone state. Update it when you complete
   a task.
3. [`DECISIONS.md`](DECISIONS.md) — ADR log. Consult before proposing architectural
   changes; do not contradict an accepted ADR without opening a new one.

**Non-negotiable rules that agents must not bypass:**

- Secrets via `{env, "VAR"}` only — never hardcoded in source or config.
- `bc_scrubber:scrub/1` on every tool result before it enters session history.
- All four gates (`eunit`, `dialyzer`, `lint`, `release`) must be green before
  marking work done.
- Never commit `.env`, `*.env`, `*.secret`, or `priv/secrets/` files.

**Tooling note**

`CLAUDE.md` is auto-loaded by Claude Code at the start of every session. If you are
using a different agent framework (Codex, Cursor, etc.), treat `CLAUDE.md` as the
canonical `AGENTS.md` for this repository — do not create a separate file that
duplicates its content, as that would create drift.

---

## License

See [`LICENSE`](LICENSE) for terms. By contributing, you agree that your
contributions will be licensed under the same terms.
