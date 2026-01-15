namespace Occult
{
	internal class deltaTAvalues
	{
		private double year;

		private double ut1;

		private int leapsecond;

		internal double Year
		{
			get
			{
				return year;
			}
			set
			{
				year = value;
			}
		}

		internal double dUT1
		{
			get
			{
				return ut1;
			}
			set
			{
				ut1 = value;
			}
		}

		internal int LeapSecond
		{
			get
			{
				return leapsecond;
			}
			set
			{
				leapsecond = value;
			}
		}
	}
}
