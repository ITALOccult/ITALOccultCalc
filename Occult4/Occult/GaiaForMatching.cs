using System;

namespace Occult
{
	public class GaiaForMatching : IComparable
	{
		private const double Radian = 180.0 / Math.PI;

		private const double MilliSecInDeg = 3600000.0;

		private const double MicroSecInDeg = 3600000000.0;

		private const int recordLength = 48;

		private int rAmas;

		private int dECmas;

		private byte rAmuas;

		private byte dECmuas;

		private int pmRA_muasec_yrCosDec;

		private int pmDec_muasec_yr;

		private ushort parallax_masX80;

		private ushort sDev_RA_10muas;

		private ushort sDev_Dec_10muas;

		private ushort sDev_Par_muas;

		private ushort sDev_pmRA_muas_yr;

		private ushort sDev_pmDec_muas_yr;

		private short epochRA_myrs2000;

		private short epochDec_myrs2000;

		private short rV_kms;

		private short mRed;

		private short mG;

		private uint catNumber;

		private byte sDev_RV;

		private byte spec;

		private byte catID;

		private byte supNum;

		public double RA_dr2 => RAdeg + PMRAdeg * (15.5 - EpochRA_yrs);

		public double RAdeg => ((double)rAmas + (double)(int)rAmuas / 250.0) / 3600000.0;

		public double Dec_rad => Decdeg / (180.0 / Math.PI);

		public double Decdeg => ((double)dECmas + (double)(int)dECmuas / 250.0) / 3600000.0;

		public double PMRAdeg => (double)pmRA_muasec_yrCosDec / 3600000000.0 / Math.Cos(Dec_rad);

		public double EpochRA_yrs => (double)epochRA_myrs2000 / 1000.0;

		public int RecordLength => 48;

		public int CompareTo(object other)
		{
			return RA_dr2.CompareTo(((GaiaForMatching)other).RA_dr2);
		}

		private string CurrentStarID()
		{
			switch (catID)
			{
			case 2:
				return "HIP " + catNumber;
			case 3:
				return "TYC " + catNumber / 100000u + "-" + catNumber % 100000u + "-" + supNum;
			case 4:
				return "UCAC4 " + catNumber / 1000000u + "-" + catNumber % 1000000u;
			case 5:
			{
				int startIndex = (int)(catNumber / 100000000u);
				string text = ((supNum >= 101) ? ("n" + Math.Abs(supNum - 101)) : ("s" + Math.Abs(supNum - 100)));
				text += "abcd".Substring(startIndex, 1);
				return "Zone-" + catNumber % 10000000u;
			}
			default:
				return "";
			}
		}
	}
}
