using System;

namespace Occult
{
	internal class GaiaSepn : IComparable
	{
		private int recnum;

		private double separation;

		private double magDiff;

		private double gMag;

		internal int RecNum
		{
			get
			{
				return recnum;
			}
			set
			{
				recnum = value;
			}
		}

		internal double Sepn
		{
			get
			{
				return separation;
			}
			set
			{
				separation = value;
			}
		}

		internal double MagDiff
		{
			get
			{
				return magDiff;
			}
			set
			{
				magDiff = value;
			}
		}

		internal double Gmag
		{
			get
			{
				return gMag;
			}
			set
			{
				gMag = value;
			}
		}

		public int CompareTo(object other)
		{
			return Sepn.CompareTo(((GaiaSepn)other).Sepn);
		}
	}
}
