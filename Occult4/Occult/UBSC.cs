using System;

namespace Occult
{
	internal class UBSC
	{
		private byte gaiaVersion = 9;

		private int hippNum;

		private short epoch = -8750;

		private ushort errRA;

		private ushort errDec;

		private ushort errpmRA;

		private ushort errpmDec;

		private int rAmas;

		private int dECmas;

		private int pmRA;

		private int pmDec;

		private double ra;

		private double dec;

		private byte rAmuas;

		private byte dECmuas;

		private byte flag;

		private ushort parallax_masX80;

		private static double MilliSecInDeg = 3600000.0;

		internal int HipNum
		{
			get
			{
				return hippNum;
			}
			set
			{
				hippNum = value;
			}
		}

		internal double RA
		{
			get
			{
				return ra;
			}
			set
			{
				ra = value;
				double num = ra * MilliSecInDeg;
				rAmas = (int)Math.Floor(num);
				rAmuas = (byte)((num - (double)rAmas) * 250.0);
			}
		}

		internal int RAmas => rAmas;

		internal byte RAmuas => rAmuas;

		internal double Dec
		{
			get
			{
				return dec;
			}
			set
			{
				dec = value;
				double num = dec * MilliSecInDeg;
				dECmas = (int)Math.Floor(num);
				dECmuas = (byte)((num - (double)dECmas) * 250.0);
			}
		}

		internal int Decmas => dECmas;

		internal byte Decmuas => dECmuas;

		internal int pmRA_muasec_yrCosDec
		{
			get
			{
				return pmRA;
			}
			set
			{
				pmRA = value;
			}
		}

		internal int pmDec_muasec_yr
		{
			get
			{
				return pmDec;
			}
			set
			{
				pmDec = value;
			}
		}

		internal ushort Parallax_masX80
		{
			get
			{
				return parallax_masX80;
			}
			set
			{
				parallax_masX80 = value;
			}
		}

		internal ushort sDev_RA_10muas
		{
			get
			{
				return errRA;
			}
			set
			{
				errRA = value;
			}
		}

		internal ushort sDev_Dec_10muas
		{
			get
			{
				return errDec;
			}
			set
			{
				errDec = value;
			}
		}

		internal ushort sDev_pmRA_muas_yr
		{
			get
			{
				return errpmRA;
			}
			set
			{
				errpmRA = value;
			}
		}

		internal ushort sDev_pmDec_muas_yr
		{
			get
			{
				return errpmDec;
			}
			set
			{
				errpmDec = value;
			}
		}

		internal byte flags
		{
			get
			{
				return flag;
			}
			set
			{
				flag = value;
			}
		}

		internal short epoch2000 => epoch;

		internal byte GaiaVersion => gaiaVersion;

		internal void DecodeUBSCline(string InLine)
		{
			HipNum = int.Parse(InLine.Substring(0, 6));
			RA = double.Parse(InLine.Substring(46, 12));
			Dec = double.Parse(InLine.Substring(59, 12));
			double.TryParse(InLine.Substring(72, 7), out var result);
			if (result > 0.0)
			{
				parallax_masX80 = Convert.ToUInt16(result * 80.0);
			}
			pmRA_muasec_yrCosDec = Convert.ToInt32(double.Parse(InLine.Substring(80, 8)) * 1000.0);
			pmDec_muasec_yr = Convert.ToInt32(double.Parse(InLine.Substring(89, 8)) * 1000.0);
			sDev_RA_10muas = Convert.ToUInt16(double.Parse(InLine.Substring(98, 4)) * 100.0);
			sDev_Dec_10muas = Convert.ToUInt16(double.Parse(InLine.Substring(103, 4)) * 100.0);
			sDev_pmRA_muas_yr = Convert.ToUInt16(double.Parse(InLine.Substring(113, 4)) * 1000.0);
			sDev_pmDec_muas_yr = Convert.ToUInt16(double.Parse(InLine.Substring(118, 4)) * 1000.0);
			flags = 0;
			if ("1234567".Contains(InLine.Substring(22, 1)))
			{
				flags = 1;
			}
		}
	}
}
