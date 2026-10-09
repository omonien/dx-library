# CLAUDE.md

Guidance for AI coding agents working in this repository.

## Project Decisions

*Assumed - derived from the repository on 2026-10-10 - correct as needed.*

1. **Language** - English for identifiers, XML doc comments, README and commit subjects. Some older comments and commit bodies are German.
2. **Tests** - yes. DUnitX projects in `Tests/`: `Tests.dproj` (core units, configuration), `TestsNew.dproj` (logger, VCL configuration dialog). Bug fixes test-first.
3. **License** - MIT (`License.txt`). `README.md` still mentions the Mozilla Public License 2.0 - known deviation, not corrected.
4. **Project type (Delphi)** - library, consumed as source units via the search path; no packages.
5. **UI framework** - FMX and VCL; the units live side by side in the repository root.
6. **Workflow** - A (direct), assumed; changes made by agents on request go through a branch and a PR.
7. **External review on `main` PRs** - no.

## Build & Test (Windows, Delphi 13)

DUnitX is not a submodule: the test projects expect the IDE environment variable `DUnitX`
(path to the DUnitX `Source` folder), which a command-line build does not define. Set it first:

```powershell
$env:DUnitX = 'C:\Projekte\DUnitX\Source'
DelphiBuildDPROJ.ps1 -ProjectFile Tests\Tests.dproj -Config Debug -Platform Win32
Tests\Win32\Debug\Tests.exe --exitbehavior:Continue   # exit code <> 0 = failures
```

`DelphiBuildDPROJ.ps1` comes from [omonien/DelphiStandards](https://github.com/omonien/DelphiStandards);
this repository has no copy of it.

Known failures on `master` (2026-10-10), unrelated to each other: `THashTests.TestSHA256`,
`THashTests.TestSHA512` (expected values are empty) and both `TConfigTests.Encoding` tests.

## Known deviations from the DX rule set (documented, not corrected)

- No `.gitattributes`; line endings rely on `core.autocrlf`.
- Units have no unit header; some `.pas` files have no UTF-8 BOM (e.g. `DX.Lib.Configuration.pas`).
- Build output goes to `<project>\$(Platform)\$(Config)`, not `/build/...`.
