using System;

namespace Occult.Lunar_Occultations
{
	internal class BAAPredictions : IComparable
	{
		private string dateStar = "";

		private string uT = "";

		private string uT1 = "";

		private string uT2 = "";

		private string uT3 = "";

		private string starEvent = "";

		private double days;

		private string ShortMonths = "JanFebMarAprMayJunJulAugSepOctNovDec";

		internal string DateStar
		{
			get
			{
				return dateStar;
			}
			set
			{
				dateStar = value;
			}
		}

		internal string UT
		{
			get
			{
				return uT;
			}
			set
			{
				uT = value;
			}
		}

		internal string UT1
		{
			get
			{
				return uT1;
			}
			set
			{
				uT1 = value;
			}
		}

		internal string UT2
		{
			get
			{
				return uT2;
			}
			set
			{
				uT2 = value;
			}
		}

		internal string UT3
		{
			get
			{
				return uT3;
			}
			set
			{
				uT3 = value;
			}
		}

		internal string StarEvent
		{
			get
			{
				return starEvent;
			}
			set
			{
				starEvent = value;
			}
		}

		internal double Days
		{
			get
			{
				return days;
			}
			set
			{
				days = value;
			}
		}

		public int CompareTo(object other)
		{
			if (Math.Abs(days - ((BAAPredictions)other).Days) > 2.0)
			{
				return days.CompareTo(((BAAPredictions)other).Days);
			}
			return starEvent.CompareTo(((BAAPredictions)other).starEvent);
		}

		internal bool ReadLine(string InLine)
		{
			if (InLine.Length < 50)
			{
				return false;
			}
			if (!int.TryParse(InLine.Substring(0, 1), out var _))
			{
				return false;
			}
			dateStar = InLine.Substring(0, 30);
			uT = InLine.Substring(30);
			starEvent = InLine.Substring(12, 4) + InLine.Substring(22, 2);
			if (!int.TryParse(InLine.Substring(0, 2), out var result2))
			{
				result2 = 0;
			}
			result2 += 2000;
			int num = ShortMonths.IndexOf(InLine.Substring(3, 3)) / 3;
			if (!int.TryParse(InLine.Substring(7, 2), out var result3))
			{
				result3 = 0;
			}
			if (!int.TryParse(InLine.Substring(30, 2), out var result4))
			{
				result4 = 0;
			}
			if (!int.TryParse(InLine.Substring(33, 2), out var result5))
			{
				result5 = 0;
			}
			days = Utilities.JD_from_Date(result2, num, (double)result3 + (double)result4 / 24.0 + (double)num / 1440.0);
			return true;
		}

		public override string ToString()
		{
			return DateStar + " " + UT1.PadRight(24) + " " + UT2.PadRight(24) + " " + UT3.PadRight(24);
		}
	}
}
