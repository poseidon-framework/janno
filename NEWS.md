# janno NEWS

## janno 1.1.0

- Switched to a new schema version: Poseidon v3.0.0.
- Implemented special handling of `_Note` columns, as they are no longer individually defined in the schema, but should still be treated as "defined" columns.
- Clarified and simplified various info-, warning-, and validation messages.
- Avoided sorting for the "defined" columns in the reading process.

## janno 1.0.0

- Switched to a new schema version: Poseidon v2.7.0.
- Changed the package name from poseidonR to janno.
- Removed the `upgrade_janno()` function.
- Simplified validation issue reporting in `read_janno()`.
- Added a start-up message highlighting that janno supports only a single Poseidon version.
- Various minor improvements, including clearer error message phrasing.

## janno 0.11.1

- Unspecified `.janno` columns are now reported as issues in the `validate_janno()` output table, rather than only via command-line messages.

## janno 0.11.0

- Disabled reading of `.janno` columns with empty headers.

## janno 0.10.1

- Fixed a regression introduced in v0.10.0 where `write_janno()` could not properly handle `NULL` values.

## janno 0.10.0

- Changed the representation of empty string list column values in `as_janno()`.
  - Previously, columns with mixed empty and non-empty values produced `NA` entries.
  - Empty string list entries are now consistently represented as `NULL`.

## janno 0.9.1

- Minor adjustments to `upgrade_janno()`.
- Updated source data for Poseidon v2.6.0.

## janno 0.9.0

- Improved the algorithm for HDR determination in radiocarbon calibration.
- This affects the `"Start"` and `"Stop"` outputs of both `process_age()` and `quickcalibrate()`.

## janno 0.8.0

- `process_age()` now supports simple start and stop date outputs, useful for plotting.

## janno 0.7.2

- Disabled double escaping of quotes in `write_janno()`.

## janno 0.7.1

- Removed post–R 4.1.0 syntax to restore compatibility with older R versions.

## janno 0.7.0

- Added `upgrade_janno()` to update janno files from Poseidon v2.4.0 to v2.5.0.

## janno 0.6.0

- Updated the package to support Poseidon v2.5.0.

## janno 0.5.0

- Added support for passing additional arguments to **Bchron** in `quickcalibrate()`.

## janno 0.4.2

- Fixed a broken documentation link.

## janno 0.4.1

- Improved messaging for additional columns in `validate_janno()` with a suggestion mechanism.

## janno 0.4.0

- Introduced `write_janno()` to write janno objects back to `.janno` files.