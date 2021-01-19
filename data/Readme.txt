There are 18 rows in the block, of which 10 are the "gala" cultivar and 8 are the "honeycrisp" cultivar.

Each row was scanned on its east side and its west side. All scans were performed the same day, close to harvest.

Goal: Estimate the honeycrisp and gala yield that was on the trees that day. Note that yield is not measured in count but is measured in pounds, a measure of *weight*.

For all of the even rows, a "calibration sample" was marked and counted by hand. This consisted of a few trees near the end of the row (but not at the very end). For each sample 4 "typical" fruit were selected and their size was measured with a hand calipers. The calibration counts and sizes are found in hand-counts-and-sizes.xls

Each row in a csv is a single fruit that was detected in the first stage of the counting system.

lat: estimated latitude of the fruit
lon: estimated longitude of the fruit

height: estimated height of the fruit in meters, compared to sea level
height-off-ground: estimated height of the fruit, compared with the ground beneath it

radius-in-mm: estimated radius of the fruit, approximating it as a sphere

num-obs: number of times the fruit was seen by the system
confidence: a measure of confidence that the fruit was estimated correctly, where 1 is most confident and 0 is least confident.

side: E or W indicating if fruit was detected from the east side or the west side of the row.

in-calibration-sample: TRUE or FALSE indicating whether the fruit was included in the calibration sample for the row. Since only the even rows had calibration samples, only the even rows will contain fruit where this is marked TRUE.

