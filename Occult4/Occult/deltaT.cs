using System;
using System.Text;

namespace Occult
{
	public class deltaT : IComparable
	{
		private int year;

		private float dt;

		public int Year
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

		public float dT
		{
			get
			{
				return dt;
			}
			set
			{
				dt = value;
			}
		}

		public void AddYearEntry(string X)
		{
			if (!int.TryParse(X.Substring(0, 6), out year))
			{
				year = 9999;
			}
			if (!float.TryParse(X.Substring(7), out dt))
			{
				dt = 0f;
			}
		}

		public int CompareTo(object other)
		{
			if (year > ((deltaT)other).Year)
			{
				return 1;
			}
			if (year == ((deltaT)other).Year)
			{
				return 0;
			}
			return -1;
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendFormat("{0,6:F0}", year);
			stringBuilder.AppendFormat(":{0,8:F1}", dt);
			return stringBuilder.ToString();
		}
	}
}
