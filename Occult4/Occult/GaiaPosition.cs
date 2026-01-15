using System;
using System.Text;

namespace Occult
{
	internal class GaiaPosition : IComparable
	{
		private const double Radian = 180.0 / Math.PI;

		private const double MilliSecInDeg = 3600000.0;

		private static int DesigField;

		private static int EpochField;

		private static int RAfield;

		private static int DecField;

		private static int ParallaxField;

		private static int RVField;

		private static int RApmField;

		private static int DecPMField;

		private static int RAUncertField;

		private static int DecUncertField;

		private static int PiUncertField;

		private static int pmRAUncertField;

		private static int pmDecUncertField;

		private static int mbField;

		private static int mgField;

		private static int mrField;

		private static int RUWEField;

		private static int DupField;

		private static int RadiusField;

		internal static double ReferenceRAdeg;

		internal static double ReferenceDecdeg;

		private double epoch;

		private double raDeg;

		private double decDeg;

		private double parallax_mas;

		private double rv;

		private double pm_RA;

		private double pm_Dec;

		private double raCatUncertainty_mas;

		private double decCatUncertainty_mas;

		private double piUncert_mas;

		private double raPMUncertainty_mas;

		private double decPMUncertainty_mas;

		private double mb;

		private double mg;

		private double mr;

		private double solarRadius;

		private double offsetFromCenter;

		private double rUWEvalue;

		private bool rUWE;

		private bool duplicateFlag;

		private string designation;

		internal double Epoch
		{
			get
			{
				return epoch;
			}
			set
			{
				epoch = value;
			}
		}

		internal double RA_rad => RADeg / (180.0 / Math.PI);

		internal double RADeg
		{
			get
			{
				return raDeg;
			}
			set
			{
				raDeg = value;
			}
		}

		internal double Dec_rad => DecDeg / (180.0 / Math.PI);

		internal double DecDeg
		{
			get
			{
				return decDeg;
			}
			set
			{
				decDeg = value;
			}
		}

		internal double Parallax_mas
		{
			get
			{
				return parallax_mas;
			}
			set
			{
				parallax_mas = value;
			}
		}

		internal double Parallax_asec => Parallax_mas / 1000.0;

		internal double Parallax_Radians => parallax_mas / 1000.0 / 3600.0 / (180.0 / Math.PI);

		internal double RadialVelocity
		{
			get
			{
				return rv;
			}
			set
			{
				rv = value;
			}
		}

		internal double PM_RA_masYr
		{
			get
			{
				return pm_RA;
			}
			set
			{
				pm_RA = value;
			}
		}

		internal double PM_RA_RadYr => pm_RA / 1000.0 / 3600.0 / Math.Cos(Dec_rad) / (180.0 / Math.PI);

		internal double PM_Dec_masYr
		{
			get
			{
				return pm_Dec;
			}
			set
			{
				pm_Dec = value;
			}
		}

		internal double PM_Dec_RadYr
		{
			get
			{
				return pm_Dec / 1000.0 / 3600.0 / (180.0 / Math.PI);
			}
			set
			{
				pm_Dec = value;
			}
		}

		internal double RA_CatUncertainty_mas
		{
			get
			{
				return raCatUncertainty_mas;
			}
			set
			{
				raCatUncertainty_mas = value;
			}
		}

		internal double Dec_CatUncertainty_mas
		{
			get
			{
				return decCatUncertainty_mas;
			}
			set
			{
				decCatUncertainty_mas = value;
			}
		}

		internal double Pi_Uncertainty_mas
		{
			get
			{
				return piUncert_mas;
			}
			set
			{
				piUncert_mas = value;
			}
		}

		internal double RA_PMUncertainty_mas
		{
			get
			{
				return raPMUncertainty_mas;
			}
			set
			{
				raPMUncertainty_mas = value;
			}
		}

		internal double Dec_PMUncertainty_mas
		{
			get
			{
				return decPMUncertainty_mas;
			}
			set
			{
				decPMUncertainty_mas = value;
			}
		}

		internal double Mb
		{
			get
			{
				return mb;
			}
			set
			{
				mb = value;
			}
		}

		internal double Mg
		{
			get
			{
				return mg;
			}
			set
			{
				mg = value;
			}
		}

		internal double Mr
		{
			get
			{
				return mr;
			}
			set
			{
				mr = value;
			}
		}

		internal bool LowRUWE
		{
			get
			{
				return rUWE;
			}
			set
			{
				rUWE = value;
			}
		}

		internal double RUWEvalue
		{
			get
			{
				return rUWEvalue;
			}
			set
			{
				rUWEvalue = value;
			}
		}

		internal bool DuplicateFlag
		{
			get
			{
				return duplicateFlag;
			}
			set
			{
				duplicateFlag = value;
			}
		}

		internal int Issues
		{
			get
			{
				int num = 0;
				if (!LowRUWE)
				{
					num++;
				}
				if (DuplicateFlag)
				{
					num += 2;
				}
				return num;
			}
		}

		internal string Gaia_id
		{
			get
			{
				return designation;
			}
			set
			{
				designation = value;
			}
		}

		internal double StarDia_mas
		{
			get
			{
				return 0.009304 * Parallax_mas * solarRadius;
			}
			set
			{
				solarRadius = value;
			}
		}

		internal double OffsetFromCenter
		{
			get
			{
				return offsetFromCenter;
			}
			set
			{
				offsetFromCenter = value;
			}
		}

		public int CompareTo(object other)
		{
			return offsetFromCenter.CompareTo(((GaiaPosition)other).offsetFromCenter);
		}

		public bool DecodeStarEntry(string Line, double Expected_Gmag, double JD)
		{
			string[] array = Line.Split(new char[1] { ',' });
			Gaia_id = array[DesigField];
			double.TryParse(array[EpochField], out epoch);
			double.TryParse(array[RAfield], out raDeg);
			double.TryParse(array[DecField], out decDeg);
			double.TryParse(array[ParallaxField], out parallax_mas);
			double.TryParse(array[RVField], out rv);
			double.TryParse(array[RApmField], out pm_RA);
			double.TryParse(array[DecPMField], out pm_Dec);
			double.TryParse(array[RAUncertField], out raCatUncertainty_mas);
			double.TryParse(array[DecUncertField], out decCatUncertainty_mas);
			double.TryParse(array[PiUncertField], out piUncert_mas);
			double.TryParse(array[pmRAUncertField], out raPMUncertainty_mas);
			double.TryParse(array[pmDecUncertField], out decPMUncertainty_mas);
			double.TryParse(array[mbField], out mb);
			double.TryParse(array[mgField], out mg);
			double.TryParse(array[mrField], out mr);
			double.TryParse(array[RUWEField], out var result);
			RUWEvalue = result;
			LowRUWE = RUWEvalue < 1.4;
			DuplicateFlag = array[DupField] == "1";
			double.TryParse(array[RadiusField], out solarRadius);
			double num = Utilities.BesselianYear(JD) - Epoch;
			Utilities.Distance(ReferenceRAdeg / (180.0 / Math.PI), ReferenceDecdeg / (180.0 / Math.PI), RADeg / (180.0 / Math.PI) + num * PM_RA_RadYr, DecDeg / (180.0 / Math.PI) + num * PM_Dec_RadYr, out var Distance, out var _);
			OffsetFromCenter = Distance * (180.0 / Math.PI) * 3600.0;
			return Math.Abs(Mg - Expected_Gmag) < 2.0;
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(Gaia_id.PadLeft(18) + " ");
			stringBuilder.AppendFormat("  B {0,5:f2}  G {1,5:f2}  R {2,5:f2} ", Mb, Mg, Mr);
			stringBuilder.AppendFormat(" Epoch {0,6:f1} ", Epoch);
			stringBuilder.Append(Utilities.DEGtoDMS(RADeg / 15.0, 2, 6, MinutesOnly: false, IncludeLeadingZeros: true, IncludePlusSymbol: false) + string.Format("  {0,9:f5}mas/yr   ", PM_RA_masYr));
			stringBuilder.Append(Utilities.DEGtoDMS(DecDeg, 3, 5, MinutesOnly: false, IncludeLeadingZeros: true, IncludePlusSymbol: true) + string.Format("  {0,9:f5}mas/yr   ", PM_Dec_masYr));
			stringBuilder.AppendFormat(" pi {0,6:f3} ±{1,6:f4}mas  rv {2,5:f2}km/sec   dia {3,5:f2}mas", Parallax_mas, Pi_Uncertainty_mas, RadialVelocity, StarDia_mas);
			stringBuilder.AppendFormat("  Errors RA {0,6:f4}mas   {1,6:f4}mas/yr", RA_CatUncertainty_mas, RA_PMUncertainty_mas);
			stringBuilder.AppendFormat("  Errors Dec {0,6:f4}mas   {1,6:f4}mas/yr", Dec_CatUncertainty_mas, Dec_PMUncertainty_mas);
			stringBuilder.AppendFormat("  Dist {0,6:f1}", OffsetFromCenter);
			return stringBuilder.ToString();
		}

		public void GetGaiaPosition(out double RA, out double Dec, out double pmRA, out double pmDec, out double MagV, out double MagB, out double MagR, out double Parallax_Asec, out double epoch, out bool UsedGaia, out int GaiaVersion, out string SourceFile, out double radialVelocity, out double UncertRA, out double UncertDec, out double UncertPMRA, out double UncertPMDec, out double StarDiameter_mas, out double StarReliability, out int DuplicateSource, out int NoGaiaPM, out int GaiaPMfromUCAC4)
		{
			RA = RA_rad;
			pmRA = PM_RA_RadYr;
			Dec = Dec_rad;
			pmDec = PM_Dec_RadYr;
			MagB = Mb;
			MagV = Mg;
			MagR = Mr;
			Parallax_Asec = Parallax_asec;
			epoch = 2000.0 + Epoch;
			UsedGaia = true;
			GaiaVersion = 3;
			SourceFile = " from ESA";
			radialVelocity = RadialVelocity;
			UncertRA = RA_CatUncertainty_mas;
			UncertDec = Dec_CatUncertainty_mas;
			UncertPMRA = RA_PMUncertainty_mas;
			UncertPMDec = Dec_PMUncertainty_mas;
			StarDiameter_mas = StarDia_mas;
			StarReliability = RUWEvalue;
			DuplicateSource = Convert.ToInt32(DuplicateFlag);
			NoGaiaPM = Convert.ToInt32(RUWEvalue == 0.0);
			GaiaPMfromUCAC4 = 0;
		}

		public string PositionOfDate_J2000(double JD, out double Offset, out double EndRA, out double EndDec, out double TotalUncertRA, out double TotalUncertDec, out double StarDia, out int IssuesFlag, out double Magb, out double Magg, out double Magr, out string ID)
		{
			Offset = OffsetFromCenter;
			double rA = RADeg / (180.0 / Math.PI);
			double dec = DecDeg / (180.0 / Math.PI);
			double num = Utilities.BesselianYear(JD) - Epoch;
			Utilities.ProperMotion(rA, dec, PM_RA_RadYr, PM_Dec_RadYr, Parallax_Radians, RadialVelocity, num, out var RAatEpoch, out var DecatEpoch);
			Utilities.StellarParallaxCoefficients(JD, RAatEpoch, DecatEpoch, out var ParallaxCoeff_RA, out var ParallaxCoeff_Dec);
			RAatEpoch += Parallax_Radians * ParallaxCoeff_RA;
			DecatEpoch += Parallax_Radians * ParallaxCoeff_Dec;
			double num2 = num * RA_PMUncertainty_mas;
			double num3 = ParallaxCoeff_RA * Pi_Uncertainty_mas;
			TotalUncertRA = Math.Sqrt(raCatUncertainty_mas * raCatUncertainty_mas + num2 * num2 + num3 * num3);
			double num4 = num * Dec_PMUncertainty_mas;
			double num5 = ParallaxCoeff_Dec * Pi_Uncertainty_mas;
			TotalUncertDec = Math.Sqrt(decCatUncertainty_mas * decCatUncertainty_mas + num4 * num4 + num5 * num5);
			StarDia = StarDia_mas;
			IssuesFlag = Issues;
			Magb = Mb;
			Magg = Mg;
			Magr = Mr;
			ID = Gaia_id;
			EndRA = RAatEpoch * (180.0 / Math.PI) / 15.0;
			EndDec = DecatEpoch * (180.0 / Math.PI);
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(ID.PadLeft(18) + " ");
			stringBuilder.AppendFormat("  B {0,5:f2}  G {1,5:f2}  R {2,5:f2} ", Magb, Magg, Magr);
			stringBuilder.Append(Utilities.DEGtoDMS(RAatEpoch * (180.0 / Math.PI) / 15.0, 2, 6, MinutesOnly: false, IncludeLeadingZeros: true, IncludePlusSymbol: false) + string.Format(" ±{0,4:f2}mas   ", TotalUncertRA));
			stringBuilder.Append(Utilities.DEGtoDMS(DecatEpoch * (180.0 / Math.PI), 3, 5, MinutesOnly: false, IncludeLeadingZeros: true, IncludePlusSymbol: true) + string.Format(" ±{0,4:f2}mas   ", TotalUncertDec));
			stringBuilder.AppendFormat(" dia {0,5:f2}mas  {1} issues    Dist {2,6:f1}", StarDia_mas, IssuesFlag, OffsetFromCenter);
			return stringBuilder.ToString();
		}

		internal static void SetFields(string FieldID)
		{
			string[] array = FieldID.Split(new char[1] { ',' });
			int num = array.Length;
			for (int i = 0; i < num; i++)
			{
				if (array[i].Trim() == "source_id")
				{
					DesigField = i;
					break;
				}
			}
			for (int j = 0; j < num; j++)
			{
				if (array[j].Trim() == "ref_epoch")
				{
					EpochField = j;
					break;
				}
			}
			for (int k = 0; k < num; k++)
			{
				if (array[k].Trim() == "ra")
				{
					RAfield = k;
					break;
				}
			}
			for (int l = 0; l < num; l++)
			{
				if (array[l].Trim() == "dec")
				{
					DecField = l;
					break;
				}
			}
			for (int m = 0; m < num; m++)
			{
				if (array[m].Trim() == "parallax")
				{
					ParallaxField = m;
					break;
				}
			}
			for (int n = 0; n < num; n++)
			{
				if (array[n].Trim() == "radial_velocity")
				{
					RVField = n;
					break;
				}
			}
			for (int num2 = 0; num2 < num; num2++)
			{
				if (array[num2].Trim() == "pmra")
				{
					RApmField = num2;
					break;
				}
			}
			for (int num3 = 0; num3 < num; num3++)
			{
				if (array[num3].Trim() == "pmdec")
				{
					DecPMField = num3;
					break;
				}
			}
			for (int num4 = 0; num4 < num; num4++)
			{
				if (array[num4].Trim() == "ra_error")
				{
					RAUncertField = num4;
					break;
				}
			}
			for (int num5 = 0; num5 < num; num5++)
			{
				if (array[num5].Trim() == "dec_error")
				{
					DecUncertField = num5;
					break;
				}
			}
			for (int num6 = 0; num6 < num; num6++)
			{
				if (array[num6].Trim() == "parallax_error")
				{
					PiUncertField = num6;
					break;
				}
			}
			for (int num7 = 0; num7 < num; num7++)
			{
				if (array[num7].Trim() == "pmra_error")
				{
					pmRAUncertField = num7;
					break;
				}
			}
			for (int num8 = 0; num8 < num; num8++)
			{
				if (array[num8].Trim() == "pmdec_error")
				{
					pmDecUncertField = num8;
					break;
				}
			}
			for (int num9 = 0; num9 < num; num9++)
			{
				if (array[num9].Trim() == "phot_bp_mean_mag")
				{
					mbField = num9;
					break;
				}
			}
			for (int num10 = 0; num10 < num; num10++)
			{
				if (array[num10].Trim() == "phot_g_mean_mag")
				{
					mgField = num10;
					break;
				}
			}
			for (int num11 = 0; num11 < num; num11++)
			{
				if (array[num11].Trim() == "phot_rp_mean_mag")
				{
					mrField = num11;
					break;
				}
			}
			for (int num12 = 0; num12 < num; num12++)
			{
				if (array[num12].Trim() == "ruwe")
				{
					RUWEField = num12;
					break;
				}
			}
			for (int num13 = 0; num13 < num; num13++)
			{
				if (array[num13].Trim() == "duplicated_source")
				{
					DupField = num13;
					break;
				}
			}
			for (int num14 = 0; num14 < num; num14++)
			{
				if (array[num14].Trim() == "radius_val")
				{
					RadiusField = num14;
					break;
				}
			}
		}
	}
}
