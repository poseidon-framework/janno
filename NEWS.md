- V 1.0.0: Multiple changes that justify a new major version number
  - switched to a new schema version Poseidon v2.7.0
  - changed the package name from poseidonR to just janno
  - removed the `upgrade_janno` function
  - simplified the validation issue reporting in `read_janno`
  - added a start-up message that highlights that the janno package only supports one Poseidon version
  - other minor changes, e.g. to the phrasing of error messages
- V 0.11.1: Unspecified .janno columns are now reported as an issue in the `validate_janno` output table, and not just with a message on the command line.
- V 0.11.0: Turned off reading .janno columns with an empty header.
- V 0.10.1: The change in v0.10.0 broke `write_janno`, which could not handle the NULL values properly. This should be fixed now.
- V 0.10.0: Changed the representation of empty list column values (in `as_janno`). So far when all values in a string list column in a package were empty, then the resulting janno object (after reading) had only entries of type NULL. If some values were non-empty, though, then the empty values were vectors with one element of type NA. Now empty string list entries are always represented by NULL.
- V 0.9.1: Small adjustments in `upgrade_janno` and update of the source data for Poseidon v2.6.0
- V 0.9.0: Fixed (changed to a better algorithm) the HDR determination for the radiocarbon calibration, which effects the "Start" and "Stop" output of both `process_age` and `quickcalibrate`
- V 0.8.0: `process_age` now supports a simple start and stop date output, which can be useful for plotting
- V 0.7.2: Turned off double escaping of quotes in `write_janno`
- V 0.7.1: Removed post-R-4.1.0 syntax to make the package compatible again with older R versions
- V 0.7.0: Added a function `upgrade_janno` to update janno files from Poseidon v2.4.0 to v2.5.0
- V 0.6.0: Adjusted the package to Poseidon v2.5.0
- V 0.5.0: Added support for additional arguments to be passed to Bchron in `quickcalibrate`
- V 0.4.2: Fixed link in documentation
- V 0.4.1: Improved the message for additional columns in `validate_janno` with a suggestion mechanism
- V 0.4.0: Introduced a function to easily write janno objects back to .janno files: `write_janno`