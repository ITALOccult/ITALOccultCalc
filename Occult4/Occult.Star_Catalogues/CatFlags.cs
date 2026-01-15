namespace Occult.Star_Catalogues
{
	public enum CatFlags
	{
		None = 0,
		ErrRecCosDec = 1,
		PmRecCosDec = 2,
		ErrPmRecCosDec = 4,
		AllCosDec = 7,
		EpochInDaysFromMJD51263 = 4096,
		EpochInYearsSince1800 = 8192,
		EpochInYearsSince1900 = 12288,
		EpochInCentiYearsSince1900 = 16384,
		EpochFixedJ2000 = 20480,
		EpochFixed1991_25 = 24576,
		EpochInDaysSinceJD2451264 = 28672,
		EpochFixed1875 = 32768,
		EpochFixed1855 = 36864,
		EpochAverage1950_62 = 40960,
		EpochAverage1998 = 45056,
		EpochInYearsSince1990 = 49152,
		FixedFK5_J2000_2000 = 53248,
		ProperMotionRelativeToYS4_0_Catalog = 268435456
	}
}
