namespace Occult.Eclipses
{
	internal static class CraterEvents
	{
		private static string date = "";

		private static string crater = "";

		private static string observer = "";

		private static int y = 0;

		private static int m = 0;

		private static int d = 0;

		private static int hr = 0;

		private static int min = 0;

		private static int sec = 0;

		private static int arg1;

		private static int dayoffset = 0;

		private static int c_event = 0;

		internal static string Date
		{
			get
			{
				return date;
			}
			set
			{
				date = value;
			}
		}

		internal static string Crater
		{
			get
			{
				return crater;
			}
			set
			{
				crater = value;
			}
		}

		internal static string Observer
		{
			get
			{
				return observer;
			}
			set
			{
				observer = value;
			}
		}

		internal static int Hr
		{
			get
			{
				return hr;
			}
			set
			{
				hr = value;
			}
		}

		internal static int Min
		{
			get
			{
				return min;
			}
			set
			{
				min = value;
			}
		}

		internal static int Sec
		{
			get
			{
				return sec;
			}
			set
			{
				sec = value;
			}
		}

		internal static int Dayoffset
		{
			get
			{
				return dayoffset;
			}
			set
			{
				dayoffset = value;
			}
		}

		internal static double JD_UT => Utilities.JD_from_Date(y, m, (double)(d + dayoffset) + ((double)hr + ((double)min + (double)sec / 60.0) / 60.0) / 24.0);

		internal static double dT => Utilities.delta_T(y, m, d) / 86400.0;

		internal static int C_event
		{
			get
			{
				return c_event;
			}
			set
			{
				c_event = value;
			}
		}

		public static void DecodeLine(string Inline)
		{
			date = Inline.Substring(0, 10);
			if (!int.TryParse(Inline.Substring(0, 4), out arg1))
			{
				arg1 = 0;
			}
			y = arg1;
			if (!int.TryParse(Inline.Substring(5, 2), out arg1))
			{
				arg1 = 0;
			}
			m = arg1;
			if (!int.TryParse(Inline.Substring(8, 2), out arg1))
			{
				arg1 = 0;
			}
			d = arg1;
			crater = Inline.Substring(11, 20);
			if (!int.TryParse(Inline.Substring(31, 1), out arg1))
			{
				arg1 = 0;
			}
			c_event = arg1;
			dayoffset = "p f".IndexOf(Inline.Substring(33, 1)) - 1;
			if (!int.TryParse(Inline.Substring(34, 2), out arg1))
			{
				arg1 = 0;
			}
			hr = arg1;
			if (!int.TryParse(Inline.Substring(37, 2), out arg1))
			{
				arg1 = 0;
			}
			min = arg1;
			if (!int.TryParse(Inline.Substring(40, 2), out arg1))
			{
				arg1 = 0;
			}
			sec = arg1;
			observer = Inline.Substring(59);
		}
	}
}
