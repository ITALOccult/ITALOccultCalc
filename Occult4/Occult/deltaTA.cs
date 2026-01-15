using System;
using System.Text;

namespace Occult
{
	public class deltaTA : IComparable
	{
		private float year;

		private float duT;

		private int dtA;

		public float Year
		{
			get
			{
				return year;
			}
			set
			{
				year = (float)(Math.Floor(value * 4f) / 4.0);
			}
		}

		public int dTA
		{
			get
			{
				return dtA;
			}
			set
			{
				dtA = value;
			}
		}

		public float dUT
		{
			get
			{
				return duT;
			}
			set
			{
				duT = value;
			}
		}

		public void AddYearEntry(string X)
		{
			if (!float.TryParse(X.Substring(0, 7), out year))
			{
				year = 9999f;
			}
			year = (float)(Math.Floor(year * 4f) / 4.0);
			if (!int.TryParse(X.Substring(8, 4), out dtA))
			{
				dtA = 0;
			}
			if (!float.TryParse(X.Substring(12), out duT))
			{
				duT = 0f;
			}
		}

		public int CompareTo(object other)
		{
			if (year > ((deltaTA)other).Year)
			{
				return 1;
			}
			if (year == ((deltaTA)other).Year)
			{
				return 0;
			}
			return -1;
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendFormat("{0,7:F2}", year);
			stringBuilder.AppendFormat(":{0,4:F0}", dtA);
			stringBuilder.AppendFormat("{0,7:F2}", duT);
			return stringBuilder.ToString();
		}
	}
}
