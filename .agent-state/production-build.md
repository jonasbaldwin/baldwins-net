# Production build — 2026-10-08

Request: build for the public site. Baseline: 82579f5, with existing root/dist analytics-removal edits and untracked files preserved. Scope: regenerate production assets locally; no publication, deployment, commit, or push authorized or performed.

Ran expanded and compressed Dart Sass compilation with source maps, then `npm run build` (clean generated JS, sync canonical CSV, Shadow CLJS release app). All three build commands passed. Release completed: 222 files, 1 compiled, 16.89 seconds; reported dependency warnings for transit.js undeclared `module`, deprecated spec-tools OpenAPI API, and Java dependency/runtime warnings. No application-source change or configuration change required.

Asset verification passed: served CSV exactly matches canonical source, production JS nonempty and lacks the development `SHADOW_ENV.evalLoad` loader, CSS/maps/entry point present. Scoped diff whitespace check passed. Browser smoke against the production output on `http://localhost:3000/#events/baldwin` passed: 61 events, nonzero rendered heights, birthday/anniversary content, October background rgb(255, 0, 153), current October text white. Preview left open. Public hosting remains unchanged; broader feature UAT was not requested.
