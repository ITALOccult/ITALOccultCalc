using System;
using System.Text;

namespace Occult
{
	internal class GAIAsource : IComparable
	{
		internal static bool SortByRA = true;

		private const double MilliSec_InRadian = 648000000.0 / Math.PI;

		private const double EpochDiff = -16.0;

		private double rAmas;

		private double dECmas;

		private ulong source_ID;

		private double pmRA_mas;

		private double pmDec_mas;

		private double cosDecFactor;

		private double parallax_mas;

		private double sDev_RA_mas;

		private double sDev_Dec_mas;

		private double sDev_Par_mas;

		private double sDev_pmRA_mas;

		private double sDev_pmDec_mas;

		private double epoch_from_2000;

		private double rV_kms;

		private double mBlue = 25.0;

		private double mRed = 25.0;

		private double mGreen;

		private double starDiameter_mas;

		private int catID;

		private int gaiaVersion = 3;

		private uint catNumber;

		private double sDev_RV;

		private double reliability;

		private int duplicateSource;

		private int noProperMotion;

		private int pmFromUCAC4;

		private int poorUCAC4Match;

		public double RAmas
		{
			get
			{
				return rAmas;
			}
			set
			{
				rAmas = value;
			}
		}

		public double CosDecFactor => cosDecFactor;

		public double RA2000mas => rAmas + -16.0 * pmRA_mas / cosDecFactor;

		public double DECmas => dECmas;

		public double DEC2000mas => dECmas + -16.0 * pmDec_mas;

		public double Parallax_mas => parallax_mas;

		public double PMRA_mas
		{
			get
			{
				return pmRA_mas;
			}
			set
			{
				pmRA_mas = value;
			}
		}

		public double PMDec_mas
		{
			get
			{
				return pmDec_mas;
			}
			set
			{
				pmDec_mas = value;
			}
		}

		public double RadialVelocityKmSec
		{
			get
			{
				return rV_kms;
			}
			set
			{
				rV_kms = value;
			}
		}

		public double Epoch_From_2000
		{
			get
			{
				return epoch_from_2000;
			}
			set
			{
				epoch_from_2000 = value;
			}
		}

		public double MagBlue
		{
			get
			{
				return mBlue;
			}
			set
			{
				mBlue = value;
			}
		}

		public double MagGreen
		{
			get
			{
				return mGreen;
			}
			set
			{
				mGreen = value;
			}
		}

		public double MagRed
		{
			get
			{
				return mRed;
			}
			set
			{
				mRed = value;
			}
		}

		public double SDev_RA_mas
		{
			get
			{
				return sDev_RA_mas;
			}
			set
			{
				sDev_RA_mas = value;
			}
		}

		public double SDev_Dec_mas
		{
			get
			{
				return sDev_Dec_mas;
			}
			set
			{
				sDev_Dec_mas = value;
			}
		}

		public double SDev_Parallax_mas
		{
			get
			{
				return sDev_Par_mas;
			}
			set
			{
				sDev_Par_mas = value;
			}
		}

		public double SDev_pmRA_mas_yr
		{
			get
			{
				return sDev_pmRA_mas;
			}
			set
			{
				sDev_pmRA_mas = value;
			}
		}

		public double SDev_pmDec_mas_yr
		{
			get
			{
				return sDev_pmDec_mas;
			}
			set
			{
				sDev_pmDec_mas = value;
			}
		}

		public double SDev_RV
		{
			get
			{
				return sDev_RV;
			}
			set
			{
				sDev_RV = value;
			}
		}

		public int GaiaVersion
		{
			get
			{
				return gaiaVersion;
			}
			set
			{
				gaiaVersion = value;
			}
		}

		public ulong Source_ID
		{
			get
			{
				return source_ID;
			}
			set
			{
				source_ID = value;
			}
		}

		public double Reliability
		{
			get
			{
				return reliability;
			}
			set
			{
				reliability = value;
			}
		}

		public int DuplicateSource
		{
			get
			{
				return duplicateSource;
			}
			set
			{
				duplicateSource = value;
			}
		}

		public int NoProperMotion
		{
			get
			{
				return noProperMotion;
			}
			set
			{
				noProperMotion = value;
			}
		}

		public int PMFromUCAC4
		{
			get
			{
				return pmFromUCAC4;
			}
			set
			{
				pmFromUCAC4 = value;
			}
		}

		public int PoorUCAC4Match
		{
			get
			{
				return poorUCAC4Match;
			}
			set
			{
				poorUCAC4Match = value;
			}
		}

		public int Flags => DuplicateSource + 2 * NoProperMotion + 4 * PMFromUCAC4 + 8 * PoorUCAC4Match;

		public double StarDiameter_mas
		{
			get
			{
				return starDiameter_mas;
			}
			set
			{
				starDiameter_mas = value;
			}
		}

		public uint StarNumber
		{
			get
			{
				return catNumber;
			}
			set
			{
				catNumber = value;
			}
		}

		public int StarCatID
		{
			get
			{
				return catID;
			}
			set
			{
				catID = value;
			}
		}

		public string ImportStarFromGaiaZone
		{
			set
			{
				string[] array = value.Split(new char[1] { ',' });
				double.TryParse(array[0], out rAmas);
				double.TryParse(array[1], out dECmas);
				cosDecFactor = Math.Cos(dECmas / (648000000.0 / Math.PI));
				double.TryParse(array[2], out parallax_mas);
				double.TryParse(array[3], out pmRA_mas);
				double.TryParse(array[4], out pmDec_mas);
				double.TryParse(array[5], out rV_kms);
				double.TryParse(array[6], out var result);
				epoch_from_2000 = result - 2000.0;
				double.TryParse(array[7], out mBlue);
				if (mBlue == 0.0)
				{
					mBlue = 25.0;
				}
				double.TryParse(array[8], out mGreen);
				double.TryParse(array[9], out mRed);
				if (mRed == 0.0)
				{
					mRed = 25.0;
				}
				starDiameter_mas = 0.0;
				double.TryParse(array[10], out sDev_RA_mas);
				double.TryParse(array[11], out sDev_Dec_mas);
				double.TryParse(array[12], out sDev_Par_mas);
				double.TryParse(array[13], out sDev_pmRA_mas);
				double.TryParse(array[14], out sDev_pmDec_mas);
				double.TryParse(array[15], out sDev_RV);
				double.TryParse(array[16], out reliability);
				int.TryParse(array[17], out duplicateSource);
				if (reliability == 0.0)
				{
					noProperMotion = 1;
				}
				gaiaVersion = 3;
				ulong.TryParse(array[18], out source_ID);
				catID = 0;
				catNumber = 0u;
			}
		}

		public string ImportStarFromTGAS_or_HIP
		{
			set
			{
				string[] array = value.Split(new char[1] { ',' });
				double.TryParse(array[0], out rAmas);
				double.TryParse(array[1], out dECmas);
				cosDecFactor = Math.Cos(DECmas / (648000000.0 / Math.PI));
				double.TryParse(array[2], out parallax_mas);
				double.TryParse(array[3], out pmRA_mas);
				double.TryParse(array[4], out pmDec_mas);
				double.TryParse(array[5], out rV_kms);
				double.TryParse(array[6], out epoch_from_2000);
				double.TryParse(array[7], out mBlue);
				if (mBlue == 0.0)
				{
					mBlue = 25.0;
				}
				double.TryParse(array[8], out mGreen);
				double.TryParse(array[9], out mRed);
				if (mRed == 0.0)
				{
					mRed = 25.0;
				}
				starDiameter_mas = 0.0;
				double.TryParse(array[10], out sDev_RA_mas);
				double.TryParse(array[11], out sDev_Dec_mas);
				double.TryParse(array[12], out sDev_Par_mas);
				double.TryParse(array[13], out sDev_pmRA_mas);
				double.TryParse(array[14], out sDev_pmDec_mas);
				double.TryParse(array[15], out sDev_RV);
				double.TryParse(array[16], out reliability);
				int.TryParse(array[17], out duplicateSource);
				int.TryParse(array[19], out gaiaVersion);
				ulong.TryParse(array[20], out source_ID);
				int.TryParse(array[21], out catID);
				uint.TryParse(array[22], out catNumber);
			}
		}

		public int CompareTo(object other)
		{
			if (SortByRA)
			{
				return RAmas.CompareTo(((GAIAsource)other).RAmas);
			}
			return Source_ID.CompareTo(((GAIAsource)other).Source_ID);
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(Utilities.DEGtoDMS(RAmas / 3600000.0 / 15.0, 2, 4, MinutesOnly: false));
			stringBuilder.Append(" " + Utilities.DEGtoDMS(DECmas / 3600000.0, 3, 3, MinutesOnly: false, IncludeLeadingZeros: false, IncludePlusSymbol: true));
			stringBuilder.AppendFormat("  {0,4:f2}", parallax_mas);
			stringBuilder.AppendFormat("  {0,6:f2}", PMRA_mas);
			stringBuilder.AppendFormat("  {0,6:f2}", PMDec_mas);
			stringBuilder.AppendFormat("  {0,5:f2}", MagGreen);
			stringBuilder.AppendFormat("  {0,5:f2}", sDev_RA_mas);
			stringBuilder.AppendFormat("  {0,5:f2}", sDev_Dec_mas);
			stringBuilder.AppendFormat("  {0,5:f2}", sDev_pmRA_mas);
			stringBuilder.AppendFormat("  {0,5:f2}", sDev_pmDec_mas);
			stringBuilder.AppendFormat("  {0,5:f2}", reliability);
			stringBuilder.AppendFormat("  {0,4:f0}", duplicateSource);
			stringBuilder.Append("   " + catID);
			stringBuilder.Append(" " + catNumber);
			return stringBuilder.ToString();
		}
	}
}
