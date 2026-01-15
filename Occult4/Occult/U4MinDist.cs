using System;

namespace Occult
{
	internal class U4MinDist : IComparable
	{
		private string num;

		private double distance;

		private double rA_UCAC4_atEpoch;

		private double dec_UCAC4_atEpoch;

		private double uCAC4EpochRA;

		private double uCAC4EpochDec;

		private double brightestMag;

		private double magV;

		private double magR;

		private int recnum;

		private int zone;

		internal string Num
		{
			get
			{
				return num;
			}
			set
			{
				num = value;
			}
		}

		internal double Dist
		{
			get
			{
				return distance;
			}
			set
			{
				distance = value;
			}
		}

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

		internal int Zone
		{
			get
			{
				return zone;
			}
			set
			{
				zone = value;
			}
		}

		internal double RA_UCAC4_atEpoch
		{
			get
			{
				return rA_UCAC4_atEpoch;
			}
			set
			{
				rA_UCAC4_atEpoch = value;
			}
		}

		internal double Dec_UCAC4_atEpoch
		{
			get
			{
				return dec_UCAC4_atEpoch;
			}
			set
			{
				dec_UCAC4_atEpoch = value;
			}
		}

		internal double UCAC4EpochRA
		{
			get
			{
				return uCAC4EpochRA;
			}
			set
			{
				uCAC4EpochRA = value;
			}
		}

		internal double UCAC4EpochDec
		{
			get
			{
				return uCAC4EpochDec;
			}
			set
			{
				uCAC4EpochDec = value;
			}
		}

		internal double BrightestMag
		{
			get
			{
				return brightestMag;
			}
			set
			{
				brightestMag = value;
			}
		}

		internal double MagV
		{
			get
			{
				return magV;
			}
			set
			{
				magV = value;
			}
		}

		internal double MagR
		{
			get
			{
				return magR;
			}
			set
			{
				magR = value;
			}
		}

		public int CompareTo(object other)
		{
			return Dist.CompareTo(((U4MinDist)other).Dist);
		}
	}
}
