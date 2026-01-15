using System;

namespace Occult
{
	public class GaiaMags : IComparable
	{
		private double jd;

		private double magG = 25.0;

		private double magEphem = 25.0;

		private int astNum;

		public int AstNum
		{
			get
			{
				return astNum;
			}
			set
			{
				astNum = value;
			}
		}

		public double JD
		{
			get
			{
				return jd;
			}
			set
			{
				jd = value;
			}
		}

		public double MagG
		{
			get
			{
				return magG;
			}
			set
			{
				magG = value;
			}
		}

		public double MagEphem
		{
			get
			{
				return magEphem;
			}
			set
			{
				magEphem = value;
			}
		}

		public int CompareTo(object other)
		{
			return jd.CompareTo(((GaiaMags)other).jd);
		}

		public override string ToString()
		{
			return string.Format("{0}  {1,5:f2}  {2,5:f2}    {3,5:f2}", Utilities.Date_from_JD(JD, 2), MagG, MagEphem, MagEphem - MagG);
		}
	}
}
