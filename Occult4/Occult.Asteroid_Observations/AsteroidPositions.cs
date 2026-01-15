using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;
using GaiaDoubles;

namespace Occult.Asteroid_Observations
{
	internal class AsteroidPositions : IComparable
	{
		public static int SortField = 2;

		public static string[] GaiaVersions = new string[10] { " Hip2", "  DR1", " DR2", " eDR3", " DR3", "", "", "", "", "_UBSC" };

		private List<SatellitePositions> satellitedata = new List<SatellitePositions>();

		private string asteroidNumber;

		private string asteroidID;

		private string starID;

		private string starCat;

		private string IAU_AsteroidID = "";

		private string gaiaVersion;

		private string gaiaNumber;

		private string fitCode = "";

		private string xyz_DecimalPlaces = "1";

		private double refHour;

		private double refTimeUncert_Secs;

		private double refHour_forAnalysis_AcrossPathUncertainty_mas;

		private double midTforMotions;

		private double pA_Motion;

		private double fPlane_X;

		private double fPlane_Y;

		private double centerOfMass_X;

		private double centerOfMass_Y;

		private double equatorial_X;

		private double equatorial_Y;

		private double equatorial_Z;

		private double dRAfromStar;

		private double dDecfromStar;

		private double deflection_RA_fromStar;

		private double deflection_Dec_fromStar;

		private double alongTrackMotion_masSec;

		private double sdev_dRAfromStar;

		private double sdev_dDecfromStar;

		private double asteroidIRDiameter;

		private double ra_Offset_DoubleStar_mas;

		private double dec_Offset_DoubleStar_mas;

		private double ra_Offset_DoubleStar_sDev_mas;

		private double dec_Offset_DoubleStar_sDev_mas;

		private double frameRotation_RA_asec;

		private double frameRotationUncert_RA_asec;

		private double frameRotation_Dec_asec;

		private double frameRotationUncert_Dec_asec;

		private double starRA_Uncert_mas;

		private double starDec_Uncert_mas;

		private double day_Conj;

		private double starRA_Deg;

		private double starDec_Deg;

		private double starApparentRA_Deg;

		private double starApparentDec_Deg;

		private double asteroidParallax;

		private double starMag;

		private double sep_Conj;

		private double pA_Conj_J2000;

		private double sdev_Sep_Conj;

		private double sdev_T_Conj;

		private double sdev_AlongTrack_fromFit;

		private double ruwe;

		private int duplicateSource;

		private int noGaiaPM;

		private int pmUsingUCAC4;

		private int doubleStar_NumSolns;

		private int year_Conj;

		private int month_Conj;

		private int number_Chords;

		private int refYear = 2000;

		private int refMonth = 1;

		private int refDay = 1;

		private int fitQuality;

		private int gaiaIssues;

		private int isParentPlanet;

		private int Arg1;

		private int numSats;

		private bool unseenPrimary;

		private bool shapeModelCentered;

		private bool solveX;

		private bool solveY;

		public static bool IncludeExtraDataInXYZ = false;

		public List<SatellitePositions> Satellites
		{
			get
			{
				return satellitedata;
			}
			set
			{
				satellitedata = value;
			}
		}

		public double MidTforMotions
		{
			get
			{
				return midTforMotions;
			}
			set
			{
				midTforMotions = value;
			}
		}

		public int RefYear
		{
			get
			{
				return refYear;
			}
			set
			{
				refYear = value;
			}
		}

		public int RefMonth
		{
			get
			{
				return refMonth;
			}
			set
			{
				refMonth = value;
			}
		}

		public int RefDay
		{
			get
			{
				return refDay;
			}
			set
			{
				refDay = value;
			}
		}

		public double RefHour
		{
			get
			{
				return refHour;
			}
			set
			{
				refHour = value;
			}
		}

		public double RefTimeUncert_Secs
		{
			get
			{
				return refTimeUncert_Secs;
			}
			set
			{
				refTimeUncert_Secs = value;
			}
		}

		public double RefHour_forAnalysis_AcrossPathUncertainty_mas
		{
			get
			{
				return refHour_forAnalysis_AcrossPathUncertainty_mas;
			}
			set
			{
				refHour_forAnalysis_AcrossPathUncertainty_mas = value;
			}
		}

		public int Year_Conj
		{
			get
			{
				return year_Conj;
			}
			set
			{
				year_Conj = value;
			}
		}

		public int Month_Conj
		{
			get
			{
				return month_Conj;
			}
			set
			{
				month_Conj = value;
			}
		}

		public double Day_Conj
		{
			get
			{
				return day_Conj;
			}
			set
			{
				day_Conj = value;
			}
		}

		public string RefDayHMS => FormatDayFractionToHMS(RefHour);

		public double AsteroidIRDiameter
		{
			get
			{
				return asteroidIRDiameter;
			}
			set
			{
				asteroidIRDiameter = value;
			}
		}

		public double AsteroidParallax
		{
			get
			{
				return asteroidParallax;
			}
			set
			{
				asteroidParallax = value;
			}
		}

		public double AlongTrackMotion_masSec
		{
			get
			{
				return alongTrackMotion_masSec;
			}
			set
			{
				alongTrackMotion_masSec = value;
			}
		}

		public string AsteroidNumber
		{
			get
			{
				return asteroidNumber;
			}
			set
			{
				asteroidNumber = value;
			}
		}

		private int AsteroidNo
		{
			get
			{
				int.TryParse(AsteroidNumber, out Arg1);
				return Arg1;
			}
		}

		public string AsteroidID
		{
			get
			{
				return asteroidID;
			}
			set
			{
				asteroidID = value;
			}
		}

		public string IAUAsteroidID
		{
			get
			{
				return IAU_AsteroidID;
			}
			set
			{
				IAU_AsteroidID = value;
			}
		}

		public bool SolveX
		{
			get
			{
				return solveX;
			}
			set
			{
				solveX = value;
			}
		}

		public bool SolveY
		{
			get
			{
				return solveY;
			}
			set
			{
				solveY = value;
			}
		}

		public double FPlane_X
		{
			get
			{
				return fPlane_X;
			}
			set
			{
				fPlane_X = value;
			}
		}

		public double FPlane_Y
		{
			get
			{
				return fPlane_Y;
			}
			set
			{
				fPlane_Y = value;
			}
		}

		public double CenterOfMass_X
		{
			get
			{
				return centerOfMass_X;
			}
			set
			{
				centerOfMass_X = value;
			}
		}

		public double CenterOfMass_Y
		{
			get
			{
				return centerOfMass_Y;
			}
			set
			{
				centerOfMass_Y = value;
			}
		}

		public string XYZ_DecimalPlaces
		{
			get
			{
				return xyz_DecimalPlaces;
			}
			set
			{
				xyz_DecimalPlaces = value;
			}
		}

		public double Equatorial_X
		{
			get
			{
				return equatorial_X;
			}
			set
			{
				equatorial_X = value;
			}
		}

		public double Equatorial_Y
		{
			get
			{
				return equatorial_Y;
			}
			set
			{
				equatorial_Y = value;
			}
		}

		public double Equatorial_Z
		{
			get
			{
				return equatorial_Z;
			}
			set
			{
				equatorial_Z = value;
			}
		}

		public double dRA_fromStar
		{
			get
			{
				return dRAfromStar;
			}
			set
			{
				dRAfromStar = value;
			}
		}

		public double dDec_fromStar
		{
			get
			{
				return dDecfromStar;
			}
			set
			{
				dDecfromStar = value;
			}
		}

		public double SeparationAtEvent => Math.Sqrt(dRA_fromStar * dRA_fromStar + dDec_fromStar * dDec_fromStar);

		public double Sdev_dRA_fromStar
		{
			get
			{
				if (!ShapeModelCentered)
				{
					return sdev_dRAfromStar;
				}
				return AsteroidIRDiameter / 50.0;
			}
			set
			{
				sdev_dRAfromStar = value;
			}
		}

		public double Sdev_dDec_fromStar
		{
			get
			{
				if (!ShapeModelCentered)
				{
					return sdev_dDecfromStar;
				}
				return AsteroidIRDiameter / 50.0;
			}
			set
			{
				sdev_dDecfromStar = value;
			}
		}

		public double Sep_fromStar => Math.Sqrt(Math.Pow(dRAfromStar, 2.0) + Math.Pow(dDec_fromStar, 2.0));

		public double PA_fromStar
		{
			get
			{
				double num = Math.Atan2(dRAfromStar, dDecfromStar) * (180.0 / Math.PI);
				if (num < 0.0)
				{
					num += 360.0;
				}
				return num;
			}
		}

		public string FitCode
		{
			get
			{
				return fitCode;
			}
			set
			{
				fitCode = value;
			}
		}

		public double Sep_Conj
		{
			get
			{
				return sep_Conj;
			}
			set
			{
				sep_Conj = value;
			}
		}

		public double PA_Conj_J2000
		{
			get
			{
				return pA_Conj_J2000;
			}
			set
			{
				pA_Conj_J2000 = value;
			}
		}

		public double PA_Motion
		{
			get
			{
				return pA_Motion;
			}
			set
			{
				pA_Motion = value;
			}
		}

		public double PA_AlongTrack_2000 => Utilities.Normalise_0to180(PA_Conj_J2000 + 90.0);

		public double Sdev_T_Conj
		{
			get
			{
				return sdev_T_Conj;
			}
			set
			{
				sdev_T_Conj = value;
			}
		}

		public double Sdev_AlongTrack_fromFit
		{
			get
			{
				return sdev_AlongTrack_fromFit;
			}
			set
			{
				sdev_AlongTrack_fromFit = value;
			}
		}

		public double Sdev_AcrossTrack_fromFit
		{
			get
			{
				return sdev_Sep_Conj;
			}
			set
			{
				sdev_Sep_Conj = value;
			}
		}

		public double Deflection_RA_fromStar
		{
			get
			{
				return deflection_RA_fromStar;
			}
			set
			{
				deflection_RA_fromStar = value;
			}
		}

		public double Deflection_Dec_fromStar
		{
			get
			{
				return deflection_Dec_fromStar;
			}
			set
			{
				deflection_Dec_fromStar = value;
			}
		}

		public double TotalUncertainty => Math.Sqrt(Sdev_dRA_fromStar * Sdev_dRA_fromStar + Sdev_dDec_fromStar * Sdev_dDec_fromStar);

		public string StarID
		{
			get
			{
				return starID;
			}
			set
			{
				starID = value;
			}
		}

		public string StarCat
		{
			get
			{
				return starCat;
			}
			set
			{
				starCat = value;
			}
		}

		public string StarCatForMPC
		{
			get
			{
				if (GaiaVersion == " ")
				{
					if (starCat == "UCAC4")
					{
						return "UCAC4";
					}
					if (starCat == "UCAC3")
					{
						return "UCAC3";
					}
					if (starCat == "UCAC2")
					{
						return "UCAC2";
					}
					if (starCat == "Tycho2")
					{
						return "Tyc2";
					}
					if (starCat == "USNO-B1")
					{
						return "USNOB1";
					}
					return "    ";
				}
				if (GaiaVersion == "0")
				{
					return "Hip2";
				}
				if (GaiaVersion == "1")
				{
					return "Gaia1";
				}
				if (GaiaVersion == "2")
				{
					return "Gaia2";
				}
				if (GaiaVersion == "3")
				{
					return "Gaia3E";
				}
				if (GaiaVersion == "4")
				{
					return "Gaia4";
				}
				if (GaiaVersion == "9")
				{
					return "UBSC";
				}
				return "    ";
			}
		}

		public string GaiaVersion
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

		public string GaiaNumber
		{
			get
			{
				return gaiaNumber;
			}
			set
			{
				gaiaNumber = value;
			}
		}

		public string GaiaNumber_Version
		{
			get
			{
				int.TryParse(GaiaVersion, out var result);
				return GaiaNumber.PadLeft(19) + GaiaVersions[result];
			}
		}

		public int GaiaIssues
		{
			get
			{
				return gaiaIssues;
			}
			set
			{
				gaiaIssues = value;
			}
		}

		public double RUWE
		{
			get
			{
				return ruwe;
			}
			set
			{
				ruwe = value;
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

		public int NoGaiaPM
		{
			get
			{
				return noGaiaPM;
			}
			set
			{
				noGaiaPM = value;
			}
		}

		public int PMUsingUCAC4
		{
			get
			{
				return pmUsingUCAC4;
			}
			set
			{
				pmUsingUCAC4 = value;
			}
		}

		public double StarMag
		{
			get
			{
				return starMag;
			}
			set
			{
				starMag = value;
			}
		}

		public double StarJ2000RA_Deg
		{
			get
			{
				return starRA_Deg;
			}
			set
			{
				starRA_Deg = value;
			}
		}

		public double StarJ2000Dec_Deg
		{
			get
			{
				return starDec_Deg;
			}
			set
			{
				starDec_Deg = value;
			}
		}

		public double StarApparentRA_Deg
		{
			get
			{
				return starApparentRA_Deg;
			}
			set
			{
				starApparentRA_Deg = value;
			}
		}

		public double StarApparentDec_Deg
		{
			get
			{
				return starApparentDec_Deg;
			}
			set
			{
				starApparentDec_Deg = value;
			}
		}

		public double RA_Offset_DoubleStar_mas
		{
			get
			{
				return ra_Offset_DoubleStar_mas;
			}
			set
			{
				ra_Offset_DoubleStar_mas = value;
			}
		}

		public double RA_Offset_DoubleStar_deg => RA_Offset_DoubleStar_mas / 1000.0 / Math.Cos(StarJ2000Dec_Deg / (180.0 / Math.PI)) / 3600.0;

		public double Dec_Offset_DoubleStar_mas
		{
			get
			{
				return dec_Offset_DoubleStar_mas;
			}
			set
			{
				dec_Offset_DoubleStar_mas = value;
			}
		}

		public double Dec_Offset_DoubleStar_deg => Dec_Offset_DoubleStar_mas / 1000.0 / 3600.0;

		public int DoubleStar_NumSolns
		{
			get
			{
				return doubleStar_NumSolns;
			}
			set
			{
				doubleStar_NumSolns = value;
			}
		}

		public double GaiaDR2_FrameRotation_RA_asec
		{
			get
			{
				return frameRotation_RA_asec;
			}
			set
			{
				frameRotation_RA_asec = value;
			}
		}

		public double GaiaDR2_FrameRotation_Dec_asec
		{
			get
			{
				return frameRotation_Dec_asec;
			}
			set
			{
				frameRotation_Dec_asec = value;
			}
		}

		public double StarRA_Uncert_mas
		{
			get
			{
				return starRA_Uncert_mas;
			}
			set
			{
				starRA_Uncert_mas = value;
			}
		}

		public double StarDec_Uncert_mas
		{
			get
			{
				return starDec_Uncert_mas;
			}
			set
			{
				starDec_Uncert_mas = value;
			}
		}

		public double RA_Offset_DoubleStar_sDev_mas
		{
			get
			{
				return ra_Offset_DoubleStar_sDev_mas;
			}
			set
			{
				ra_Offset_DoubleStar_sDev_mas = value;
			}
		}

		public double Dec_Offset_DoubleStar_sDev_mas
		{
			get
			{
				return dec_Offset_DoubleStar_sDev_mas;
			}
			set
			{
				dec_Offset_DoubleStar_sDev_mas = value;
			}
		}

		public double GaiaDR2_FrameRotationUncert_RA_asec
		{
			get
			{
				return frameRotationUncert_RA_asec;
			}
			set
			{
				frameRotationUncert_RA_asec = value;
			}
		}

		public double GaiaDR2_FrameRotationUncert_RA_mas => frameRotationUncert_RA_asec * 1000.0;

		public double GaiaDR2_FrameRotationUncert_Dec_asec
		{
			get
			{
				return frameRotationUncert_Dec_asec;
			}
			set
			{
				frameRotationUncert_Dec_asec = value;
			}
		}

		public double GaiaDR2_FrameRotationUncert_Dec_mas => frameRotationUncert_Dec_asec * 1000.0;

		public string FormattedStarRA => Utilities.DEGtoDMS((StarJ2000RA_Deg + RA_Offset_DoubleStar_deg + GaiaDR2_FrameRotation_RA_asec / 3600.0) / 15.0, 2, 5, MinutesOnly: false);

		public string FormattedStarDec => Utilities.DEGtoDMS(StarJ2000Dec_Deg + Dec_Offset_DoubleStar_deg + GaiaDR2_FrameRotation_Dec_asec / 3600.0, 3, 4, MinutesOnly: false, IncludeLeadingZeros: false, IncludePlusSymbol: true);

		public string FormattedStarRA_CSV => Utilities.DEGtoDMS((StarJ2000RA_Deg + RA_Offset_DoubleStar_deg + GaiaDR2_FrameRotation_RA_asec / 3600.0) / 15.0, 2, 6, MinutesOnly: false);

		public string FormattedStarDec_CSV => Utilities.DEGtoDMS(StarJ2000Dec_Deg + Dec_Offset_DoubleStar_deg + GaiaDR2_FrameRotation_Dec_asec / 3600.0, 3, 5, MinutesOnly: false, IncludeLeadingZeros: false, IncludePlusSymbol: true);

		public string MPCLine => getMPCLine(SpecifyEquatorial_xyz: false);

		public string MPCLine_xyz => getMPCLine(SpecifyEquatorial_xyz: true);

		public int IsParentPlanet => isParentPlanet;

		public int Number_Chords
		{
			get
			{
				return number_Chords;
			}
			set
			{
				number_Chords = value;
			}
		}

		public int FitQuality
		{
			get
			{
				return fitQuality;
			}
			set
			{
				fitQuality = value;
			}
		}

		public bool ShapeModelCentered
		{
			get
			{
				return shapeModelCentered | (CenterOfMass_X != 0.0) | (CenterOfMass_Y != 0.0);
			}
			set
			{
				shapeModelCentered = value;
			}
		}

		public double SortDate => (double)(RefYear * 10000 + RefMonth * 100 + RefDay) + RefHour / 24.0;

		public double SortDateConj => (double)(Year_Conj * 10000 + Month_Conj * 100) + Day_Conj;

		public bool UnseenPrimary
		{
			get
			{
				return unseenPrimary;
			}
			set
			{
				unseenPrimary = value;
			}
		}

		public int NumSatellites
		{
			get
			{
				return numSats;
			}
			set
			{
				numSats = value;
			}
		}

		private string FormatDayFractionToHMS(double h)
		{
			int num = (int)Math.Floor(h);
			double num2 = 60.0 * (h - (double)num);
			int num3 = (int)Math.Floor(num2);
			double num4 = 60.0 * (num2 - (double)num3);
			if (num4 > 59.99)
			{
				num4 = 59.99;
			}
			return num.ToString().PadLeft(2, '0') + ":" + num3.ToString().PadLeft(2, '0') + ":" + string.Format("{0,3:f2}Z", num4).PadLeft(6, '0');
		}

		private string getMPCLine(bool SpecifyEquatorial_xyz)
		{
			//IL_0ac3: Unknown result type (might be due to invalid IL or missing references)
			_ = new string[3] { "High RUWE", "Duplicate source", "High RUWE, Duplicate source" };
			string text = "";
			isParentPlanet = 0;
			double num = RA_Offset_DoubleStar_mas / 1000.0 / Math.Cos(StarJ2000Dec_Deg / (180.0 / Math.PI)) / 3600.0;
			double num2 = Dec_Offset_DoubleStar_mas / 1000.0 / 3600.0;
			StringBuilder stringBuilder = new StringBuilder();
			if (!UnseenPrimary)
			{
				if (AsteroidNumber.PadRight(1).Substring(0, 1) == "P")
				{
					int num3 = int.Parse(AsteroidNumber.Substring(1, 1));
					int num4 = int.Parse(AsteroidNumber.Substring(3));
					if (num3 == 9)
					{
						if (num4 == 0)
						{
							stringBuilder.Append("    143430|                 |");
						}
						else
						{
							stringBuilder.Append("(143430) " + num4 + "|                 |");
							isParentPlanet = 3;
						}
					}
					else
					{
						string text2 = Utilities.Planets[num3];
						if (num4 > 0)
						{
							text2 = text2 + " " + num4;
							isParentPlanet = 2;
						}
						else
						{
							isParentPlanet = 1;
						}
						stringBuilder.Append(text2.PadLeft(10) + "|");
						stringBuilder.Append("".PadRight(17) + "|");
					}
				}
				else if (AsteroidNumber.EndsWith("P") | AsteroidNumber.EndsWith("I"))
				{
					stringBuilder.Append(AsteroidNumber.PadLeft(10) + "|");
					stringBuilder.Append("".PadRight(17) + "|");
				}
				else if (AsteroidNumber.Contains("P/") | AsteroidNumber.Contains("C/"))
				{
					stringBuilder.Append("".PadLeft(10) + "|");
					stringBuilder.Append(AsteroidNumber.PadRight(17) + "|");
				}
				else if (AsteroidNo > 0)
				{
					stringBuilder.Append(AsteroidNumber.ToString().PadLeft(10) + "|");
					stringBuilder.Append("".PadRight(17) + "|");
				}
				else
				{
					stringBuilder.Append(" ".PadRight(10) + "|");
					stringBuilder.Append(AsteroidID.ToString().PadRight(17) + "|");
				}
				stringBuilder.Append(" OCC|");
				if (SpecifyEquatorial_xyz)
				{
					stringBuilder.Append("275|");
				}
				else
				{
					stringBuilder.Append("244|");
				}
				if (RefHour >= 24.0)
				{
					Utilities.Date_from_JD(Utilities.JD_from_Date(RefYear, RefMonth, (double)RefDay + RefHour / 24.0), out var Year, out var Month, out var day);
					int num5 = Convert.ToInt32(day);
					double h = (day - (double)num5) * 24.0;
					stringBuilder.Append(Year + "-" + Month.ToString().PadLeft(2, '0') + "-" + num5.ToString().PadLeft(2, '0') + "T" + FormatDayFractionToHMS(h) + "|");
				}
				else
				{
					stringBuilder.Append(RefYear + "-" + RefMonth.ToString().PadLeft(2, '0') + "-" + RefDay.ToString().PadLeft(2, '0') + "T" + RefDayHMS + "|");
				}
				stringBuilder.AppendFormat("{0,13:f9}|", StarJ2000RA_Deg + num + GaiaDR2_FrameRotation_RA_asec / 3600.0 / Math.Cos(StarJ2000Dec_Deg / (180.0 / Math.PI)));
				stringBuilder.AppendFormat("{0,13:f9}|", StarJ2000Dec_Deg + num2 + GaiaDR2_FrameRotation_Dec_asec / 3600.0);
				if (SpecifyEquatorial_xyz)
				{
					stringBuilder.Append("  0.0000|  0.0000|");
					stringBuilder.Append("ICRF_KM|399|");
					stringBuilder.Append(string.Format("{0,9:f3}", Equatorial_X) + "|");
					stringBuilder.Append(string.Format("{0,9:f3}", Equatorial_Y) + "|");
					stringBuilder.Append(string.Format("{0,9:f3}", Equatorial_Z) + "|");
				}
				else
				{
					stringBuilder.AppendFormat("{0,8:f4}|", dRAfromStar);
					stringBuilder.AppendFormat("{0,8:f4}|", dDecfromStar);
				}
				Asteroid_Observations_Reports.CalculateUncertainties(StarRA_Uncert_mas, StarDec_Uncert_mas, ra_Offset_DoubleStar_sDev_mas, dec_Offset_DoubleStar_sDev_mas, GaiaDR2_FrameRotationUncert_RA_mas, GaiaDR2_FrameRotationUncert_Dec_mas, RefTimeUncert_Secs, refHour_forAnalysis_AcrossPathUncertainty_mas, AlongTrackMotion_masSec, sdev_AlongTrack_fromFit, Sdev_AcrossTrack_fromFit, PA_AlongTrack_2000, out var rmsRA_Total_mas, out var rmsDec_Total_mas, out var Correlation, out var rmsStarRA_Total_mas, out var rmsStarDec_Total_mas, out var rmsAlongTrack_Total_mas);
				stringBuilder.Append(string.Format("{0,6:f4}|", Utilities.QuadratureAddition(rmsRA_Total_mas, rmsStarRA_Total_mas) / 1000.0).Substring(0, 7));
				stringBuilder.Append(string.Format("{0,6:f4}|", Utilities.QuadratureAddition(rmsDec_Total_mas, rmsStarDec_Total_mas) / 1000.0).Substring(0, 7));
				stringBuilder.Append(string.Format("{0,7:f3}|", Correlation));
				stringBuilder.Append(StarCatForMPC.Trim().PadLeft(8) + "|");
				if (ShapeModelCentered)
				{
					stringBuilder.Append("1       |");
				}
				else
				{
					stringBuilder.Append("0       |");
				}
				stringBuilder.Append(string.Format("StarUncert{0,8:f5},{1,8:f5};", rmsStarRA_Total_mas / 1000.0, rmsStarDec_Total_mas / 1000.0));
				stringBuilder.Append(" Fit " + FitCode.PadRight(2) + ";");
				if ((RUWE == 0.0) | (RUWE > 1.4) | IncludeExtraDataInXYZ)
				{
					stringBuilder.AppendFormat(" RUWE={0,2:f2};", RUWE);
				}
				if ((ra_Offset_DoubleStar_sDev_mas != 0.0) | (dec_Offset_DoubleStar_sDev_mas != 0.0))
				{
					stringBuilder.AppendFormat(" {0,1}-solution double star;", DoubleStar_NumSolns);
				}
				if (DuplicateSource == 1)
				{
					stringBuilder.Append(" Duplicate source;");
				}
				if (NoGaiaPM == 1)
				{
					stringBuilder.Append(" No Gaia PM;");
				}
				if (PMUsingUCAC4 == 1)
				{
					stringBuilder.Append(" Proper motion using UCAC4;");
				}
				if (IncludeExtraDataInXYZ)
				{
					_ = asteroidNumber.Trim() == "105";
					Utilities.QuickSolarElongation_PhaseAngle(Utilities.JD_from_Date(RefYear, RefMonth, (double)RefDay + RefHour / 24.0), starRA_Deg / (180.0 / Math.PI), starDec_Deg / (180.0 / Math.PI), 8.794143836182533 / asteroidParallax, out var _, out var PhaseAngle, out var _);
					int length = stringBuilder.ToString().Length;
					if (length < 237)
					{
						stringBuilder.Append("".PadRight(237 - length));
					}
					stringBuilder.AppendFormat(" |Extras: Gaia# {0},  AstDia {1,1:f4},  AlongTrack {2,1:f4} asec = {3,1:f2} secs,  AcrossTrack {4,1:f4}, PA of Track {5,3:f0}°, PhaseAngle {6,1:f0}°", GaiaNumber_Version, AsteroidIRDiameter, rmsAlongTrack_Total_mas / 1000.0, rmsAlongTrack_Total_mas / alongTrackMotion_masSec, Sdev_AcrossTrack_fromFit / 1000.0, PA_Motion, PhaseAngle);
					if (global::GaiaDoubles.GaiaDoubles.GaiaDouble_Match(starRA_Deg, starDec_Deg, GaiaCatIDOnly: true, DetailsOnly: false, out var FullDetails))
					{
						stringBuilder.Append(", Non-Single " + FullDetails);
					}
				}
			}
			text = stringBuilder.ToString();
			int numSatellites = NumSatellites;
			for (int i = 0; i < numSatellites; i++)
			{
				if (Satellites[i].SatelliteFitQuality <= -1001)
				{
					continue;
				}
				stringBuilder = new StringBuilder();
				string text3 = Satellites[i].IAUSatelliteID;
				if (text3.Contains("?"))
				{
					continue;
				}
				int num6 = text3.LastIndexOf(" ");
				int num7 = text3.LastIndexOf(")");
				if (text3.Contains("Pluto"))
				{
					text3 = text3.Replace("V", "5").Replace("IV", "4").Replace("III", "3")
						.Replace("II", "2")
						.Replace("I", "1");
					num6 = text3.LastIndexOf(" ");
					int num8 = text3.IndexOf(" ");
					if (num8 > 0 && num6 > num8)
					{
						text3 = text3.Substring(0, num6) + " =";
					}
					isParentPlanet = 3;
				}
				else
				{
					if (num6 - num7 > 6 || num6 < 0 || num7 < 0)
					{
						MessageBox.Show("The satellite name\r\n" + text3 + "\r\n does not confirm to the standard naming rules. \r\nYou will need to correct the name, and regenerate the report.\r\n\r\n The event was on\r\n" + RefYear + "-" + RefMonth.ToString().PadLeft(2, '0') + "-" + RefDay.ToString().PadLeft(2, '0'), "Satellite name", (MessageBoxButtons)0, (MessageBoxIcon)48);
					}
					if (num6 > 0 && num6 - num7 > 1)
					{
						text3 = text3.Substring(0, num6);
					}
				}
				int num9 = text3.IndexOf("=");
				if (num9 < 1)
				{
					stringBuilder.Append(" ".PadRight(10) + "|");
					stringBuilder.Append(text3.PadRight(17) + "|");
				}
				else
				{
					stringBuilder.Append(text3.Substring(0, num9 - 1).Trim().PadLeft(10) + "|");
					stringBuilder.Append("".PadRight(17) + "|");
				}
				stringBuilder.Append(" OCC|");
				if (SpecifyEquatorial_xyz)
				{
					stringBuilder.Append("275|");
				}
				else
				{
					stringBuilder.Append("244|");
				}
				if (RefHour >= 24.0)
				{
					Utilities.Date_from_JD(Utilities.JD_from_Date(RefYear, RefMonth, (double)RefDay + RefHour / 24.0), out var Year2, out var Month2, out var day2);
					int num10 = Convert.ToInt32(day2);
					double h2 = (day2 - (double)num10) * 24.0;
					stringBuilder.Append(Year2 + "-" + Month2.ToString().PadLeft(2, '0') + "-" + num10.ToString().PadLeft(2, '0') + "T" + FormatDayFractionToHMS(h2) + "|");
				}
				stringBuilder.Append(RefYear + "-" + RefMonth.ToString().PadLeft(2, '0') + "-" + RefDay.ToString().PadLeft(2, '0') + "T" + RefDayHMS + "|");
				stringBuilder.AppendFormat("{0,13:f9}|", StarJ2000RA_Deg + GaiaDR2_FrameRotation_RA_asec / 3600.0 / Math.Cos(StarJ2000Dec_Deg / (180.0 / Math.PI)));
				stringBuilder.AppendFormat("{0,13:f9}|", StarJ2000Dec_Deg + GaiaDR2_FrameRotation_Dec_asec / 3600.0);
				if (SpecifyEquatorial_xyz)
				{
					stringBuilder.Append("  0.0000|  0.0000|");
					stringBuilder.Append("ICRF_KM|399|");
					stringBuilder.Append(string.Format("{0,9:f3}", Satellites[i].Equatorial_X) + "|");
					stringBuilder.Append(string.Format("{0,9:f3}", Satellites[i].Equatorial_Y) + "|");
					stringBuilder.Append(string.Format("{0,9:f3}", Satellites[i].Equatorial_Z) + "|");
				}
				else
				{
					stringBuilder.AppendFormat("{0,8:f4}|", Satellites[i].dRA_fromStar_Satellite);
					stringBuilder.AppendFormat("{0,8:f4}|", Satellites[i].dDec_fromStar_Satellite);
				}
				Asteroid_Observations_Reports.CalculateUncertainties(StarRA_Uncert_mas, StarDec_Uncert_mas, ra_Offset_DoubleStar_sDev_mas, dec_Offset_DoubleStar_mas, GaiaDR2_FrameRotationUncert_RA_asec, GaiaDR2_FrameRotation_Dec_asec, RefTimeUncert_Secs, refHour_forAnalysis_AcrossPathUncertainty_mas, AlongTrackMotion_masSec, Satellites[i].Sdev_AlongTrack_fromFit, Satellites[i].AcrossTrack_fromFit_Satellite, PA_AlongTrack_2000, out var rmsRA_Total_mas2, out var rmsDec_Total_mas2, out var Correlation2, out var rmsStarRA_Total_mas2, out var rmsStarDec_Total_mas2, out var rmsAlongTrack_Total_mas2);
				stringBuilder.Append(string.Format("{0,6:f4}|", rmsRA_Total_mas2 / 1000.0).Substring(0, 7));
				stringBuilder.Append(string.Format("{0,6:f4}|", rmsDec_Total_mas2 / 1000.0).Substring(0, 7));
				stringBuilder.Append(string.Format("{0,7:f3}|", Correlation2));
				stringBuilder.Append(StarCatForMPC.Trim().PadLeft(8) + "|");
				stringBuilder.Append("0       |");
				stringBuilder.Append(string.Format("StarUncert{0,8:f5},{1,8:f5};", rmsStarRA_Total_mas2 / 1000.0, rmsStarDec_Total_mas2 / 1000.0));
				stringBuilder.Append(" Fit " + FitCode.PadRight(2) + ";");
				if ((RUWE == 0.0) | (RUWE > 1.4))
				{
					stringBuilder.AppendFormat(" RUWE={0,2:f2};", RUWE);
				}
				if (DuplicateSource == 1)
				{
					stringBuilder.Append(" Duplicate source;");
				}
				if (NoGaiaPM == 1)
				{
					stringBuilder.Append(" No Gaia PM;");
				}
				if (PMUsingUCAC4 == 1)
				{
					stringBuilder.Append(" Proper motion using UCAC4;");
				}
				if (IncludeExtraDataInXYZ)
				{
					int length2 = stringBuilder.ToString().Length;
					if (length2 < 237)
					{
						stringBuilder.Append("".PadRight(237 - length2));
					}
					stringBuilder.AppendFormat(" |Extras: Gaia# {4},  AstDia {3,1:f4},  AlongTrack_asec {0,1:f4} = {5,1:f3}secs,  AcrossTrack {1,1:f4}, PA of Track/Motion {2,3:f0}°", rmsAlongTrack_Total_mas2 / 1000.0, Sdev_AcrossTrack_fromFit / 1000.0, PA_Motion, AsteroidIRDiameter, GaiaNumber_Version, rmsAlongTrack_Total_mas2 / alongTrackMotion_masSec);
					if (global::GaiaDoubles.GaiaDoubles.GaiaDouble_Match(starRA_Deg, starDec_Deg, GaiaCatIDOnly: true, DetailsOnly: false, out var FullDetails2))
					{
						stringBuilder.Append(", Non-Single " + FullDetails2);
					}
				}
				text = (UnseenPrimary ? stringBuilder.ToString() : (text + "\r\n" + stringBuilder.ToString()));
			}
			return text;
		}

		public int CompareTo(object other)
		{
			switch (SortField)
			{
			case 0:
				if (AsteroidID.CompareTo(((AsteroidPositions)other).AsteroidID) == 0)
				{
					return SortDate.CompareTo(((AsteroidPositions)other).SortDate);
				}
				return AsteroidID.CompareTo(((AsteroidPositions)other).AsteroidID);
			case 1:
				if (AsteroidNumber.PadRight(1).Substring(0, 1) == "P")
				{
					if (((AsteroidPositions)other).AsteroidNumber.PadRight(1).Substring(0, 1) == "P")
					{
						if (AsteroidNumber.CompareTo(((AsteroidPositions)other).AsteroidNumber) == 0)
						{
							return SortDate.CompareTo(((AsteroidPositions)other).SortDate);
						}
						return AsteroidNumber.CompareTo(((AsteroidPositions)other).AsteroidNumber);
					}
					return -AsteroidNo.CompareTo(((AsteroidPositions)other).AsteroidNo);
				}
				if (((AsteroidPositions)other).AsteroidNumber.PadRight(1).Substring(0, 1) == "P")
				{
					if (AsteroidNumber.CompareTo(((AsteroidPositions)other).AsteroidNumber) == 0)
					{
						return SortDate.CompareTo(((AsteroidPositions)other).SortDate);
					}
					return AsteroidNumber.CompareTo(((AsteroidPositions)other).AsteroidNumber);
				}
				if (AsteroidNo.CompareTo(((AsteroidPositions)other).AsteroidNo) == 0)
				{
					return SortDate.CompareTo(((AsteroidPositions)other).SortDate);
				}
				return AsteroidNo.CompareTo(((AsteroidPositions)other).AsteroidNo);
			case 2:
				return SortDate.CompareTo(((AsteroidPositions)other).SortDate);
			case 3:
				if (dRA_fromStar.CompareTo(((AsteroidPositions)other).dRA_fromStar) == 0)
				{
					return AsteroidNumber.CompareTo(((AsteroidPositions)other).AsteroidNumber);
				}
				return dRA_fromStar.CompareTo(((AsteroidPositions)other).dRA_fromStar);
			case 4:
				if (dDec_fromStar.CompareTo(((AsteroidPositions)other).dDec_fromStar) == 0)
				{
					return AsteroidNumber.CompareTo(((AsteroidPositions)other).AsteroidNumber);
				}
				return dDec_fromStar.CompareTo(((AsteroidPositions)other).dDec_fromStar);
			case 5:
				return SeparationAtEvent.CompareTo(((AsteroidPositions)other).SeparationAtEvent);
			case 6:
				if (Sdev_AcrossTrack_fromFit.CompareTo(((AsteroidPositions)other).TotalUncertainty) == 0)
				{
					return AsteroidNumber.CompareTo(((AsteroidPositions)other).AsteroidNumber);
				}
				return Sdev_AcrossTrack_fromFit.CompareTo(((AsteroidPositions)other).Sdev_AcrossTrack_fromFit);
			case 7:
				if (GaiaVersion.CompareTo(((AsteroidPositions)other).GaiaVersion) == 0)
				{
					return SortDate.CompareTo(((AsteroidPositions)other).SortDate);
				}
				return GaiaVersion.CompareTo(((AsteroidPositions)other).GaiaVersion);
			case 8:
				return Sep_Conj.CompareTo(((AsteroidPositions)other).Sep_Conj);
			case 9:
				return SortDateConj.CompareTo(((AsteroidPositions)other).SortDateConj);
			default:
				return 0;
			}
		}

		public override string ToString()
		{
			string CSVline;
			return MakeEventString(Satellite: false, 0, XYZcoords: false, out CSVline);
		}

		public string MakeEventString(bool Satellite, int SatNum, bool XYZcoords, out string CSVline)
		{
			StringBuilder stringBuilder = new StringBuilder();
			StringBuilder stringBuilder2 = new StringBuilder();
			CSVline = "";
			if (!Satellite & UnseenPrimary)
			{
				return "";
			}
			if (Satellite)
			{
				if (Satellites[SatNum].IAUSatelliteID.Trim() != "?")
				{
					stringBuilder.Append(asteroidNumber.PadLeft(7) + " " + Satellites[SatNum].IAUSatelliteID.PadRight(13).Substring(0, 13));
					stringBuilder2.Append(asteroidNumber.Trim() + "," + Satellites[SatNum].IAUSatelliteID.Trim());
				}
				else
				{
					stringBuilder.Append(asteroidNumber.PadLeft(7) + " (" + (asteroidNumber.Trim() + ") satellite").PadRight(12).Substring(0, 12));
					stringBuilder2.Append(asteroidNumber.Trim() + ",(" + asteroidNumber.Trim() + ") satellite");
				}
			}
			else
			{
				stringBuilder.Append((asteroidNumber.PadLeft(7) + " " + asteroidID.PadRight(13).Substring(0, 13)).Substring(0, 21));
				stringBuilder2.Append(asteroidNumber.Trim() + "," + asteroidID.Trim());
			}
			stringBuilder.AppendFormat("  {0,4:F0}{1,3:F0}{2,3:F0} ", RefYear, RefMonth, RefDay);
			stringBuilder.Append(Utilities.DEGtoDMS(RefHour, 2, 3, MinutesOnly: false, IncludeLeadingZeros: true));
			stringBuilder2.AppendFormat(",{0,1:F0},{1,1:F0},{2,1:F0},", RefYear, RefMonth, RefDay);
			stringBuilder2.Append(Utilities.DEGtoDMS(RefHour, 2, 3, MinutesOnly: false, IncludeLeadingZeros: true));
			if (Satellite)
			{
				if (XYZcoords)
				{
					stringBuilder.AppendFormat(" {0,10:f3} {1,10:f3} {2,10:f3}", Satellites[SatNum].Equatorial_X, Satellites[SatNum].Equatorial_Y, Satellites[SatNum].Equatorial_Z);
					stringBuilder2.AppendFormat(",{0,10:f3},{1,10:f3},{2,10:f3}", Satellites[SatNum].Equatorial_X, Satellites[SatNum].Equatorial_Y, Satellites[SatNum].Equatorial_Z);
				}
				else
				{
					stringBuilder.Append("   " + Utilities.DEGtoDMS((StarJ2000RA_Deg + RA_Offset_DoubleStar_deg + (Satellites[SatNum].dRA_fromStar_Satellite + GaiaDR2_FrameRotation_RA_asec) / 3600.0 / Math.Cos(StarJ2000Dec_Deg / (180.0 / Math.PI))) / 15.0, 2, 5, MinutesOnly: false, IncludeLeadingZeros: true));
					stringBuilder.Append(" " + Utilities.DEGtoDMS(StarJ2000Dec_Deg + Dec_Offset_DoubleStar_deg + (Satellites[SatNum].dDec_fromStar_Satellite + GaiaDR2_FrameRotation_Dec_asec) / 3600.0, 3, 4, MinutesOnly: false, IncludeLeadingZeros: true));
					stringBuilder2.Append("," + Utilities.DEGtoDMS((StarJ2000RA_Deg + RA_Offset_DoubleStar_deg + (Satellites[SatNum].dRA_fromStar_Satellite + GaiaDR2_FrameRotation_RA_asec) / 3600.0 / Math.Cos(StarJ2000Dec_Deg / (180.0 / Math.PI))) / 15.0, 2, 6, MinutesOnly: false, IncludeLeadingZeros: true));
					stringBuilder2.Append("," + Utilities.DEGtoDMS(StarJ2000Dec_Deg + Dec_Offset_DoubleStar_deg + (Satellites[SatNum].dDec_fromStar_Satellite + GaiaDR2_FrameRotation_Dec_asec) / 3600.0, 3, 5, MinutesOnly: false, IncludeLeadingZeros: true));
				}
			}
			else if (XYZcoords)
			{
				stringBuilder.AppendFormat(" {0,10:f3} {1,10:f3} {2,10:f3}", Equatorial_X, Equatorial_Y, Equatorial_Z);
				stringBuilder2.AppendFormat(",{0,10:f3},{1,10:f3},{2,10:f3}", Equatorial_X, Equatorial_Y, Equatorial_Z);
			}
			else
			{
				stringBuilder.Append("   " + Utilities.DEGtoDMS((StarJ2000RA_Deg + RA_Offset_DoubleStar_deg + (dRAfromStar + GaiaDR2_FrameRotation_RA_asec) / 3600.0 / Math.Cos(StarJ2000Dec_Deg / (180.0 / Math.PI))) / 15.0, 2, 5, MinutesOnly: false, IncludeLeadingZeros: true));
				stringBuilder.Append(" " + Utilities.DEGtoDMS(StarJ2000Dec_Deg + Dec_Offset_DoubleStar_deg + (dDecfromStar + GaiaDR2_FrameRotation_Dec_asec) / 3600.0, 3, 4, MinutesOnly: false, IncludeLeadingZeros: true));
				stringBuilder2.Append("," + Utilities.DEGtoDMS((StarJ2000RA_Deg + RA_Offset_DoubleStar_deg + (dRAfromStar + GaiaDR2_FrameRotation_RA_asec) / 3600.0 / Math.Cos(StarJ2000Dec_Deg / (180.0 / Math.PI))) / 15.0, 2, 6, MinutesOnly: false, IncludeLeadingZeros: true));
				stringBuilder2.Append("," + Utilities.DEGtoDMS(StarJ2000Dec_Deg + Dec_Offset_DoubleStar_deg + (dDecfromStar + GaiaDR2_FrameRotation_Dec_asec) / 3600.0, 3, 5, MinutesOnly: false, IncludeLeadingZeros: true));
			}
			stringBuilder.Append("   " + FormattedStarRA);
			stringBuilder.Append(" " + FormattedStarDec);
			stringBuilder2.Append("," + FormattedStarRA_CSV);
			stringBuilder2.Append("," + FormattedStarDec_CSV);
			stringBuilder.Append("  ");
			string text = ((GaiaVersion == "0") ? "Hip2  " : ((GaiaVersion == "1") ? "Gaia1 " : ((GaiaVersion == "2") ? "Gaia2 " : ((GaiaVersion == "3") ? "Gaia3E" : ((GaiaVersion == "4") ? "Gaia4 " : ((!(GaiaVersion == "9")) ? "NA" : "UBSC  "))))));
			stringBuilder.Append(text);
			stringBuilder2.Append("," + text.Trim());
			if (Satellite)
			{
				stringBuilder.AppendFormat("{0,4:F0}", Satellites[SatNum].Number_ChordsSatellite);
				stringBuilder2.AppendFormat(",{0,1:F0}", Satellites[SatNum].Number_ChordsSatellite);
			}
			else
			{
				stringBuilder.AppendFormat("{0,4:F0}", number_Chords);
				stringBuilder2.AppendFormat(",{0,1:F0}", number_Chords);
			}
			if (Satellite)
			{
				if (!XYZcoords)
				{
					stringBuilder.AppendFormat("  {0,8:+0.0000;-0.0000} {1,8:+0.0000;-0.0000}", Satellites[SatNum].dRA_fromStar_Satellite, Satellites[SatNum].dDec_fromStar_Satellite + Deflection_Dec_fromStar);
				}
				stringBuilder.AppendFormat("  {0,7:f3}", Satellites[SatNum].PAofTrack_2000);
				if (!XYZcoords)
				{
					stringBuilder2.AppendFormat(",{0,1:+0.00000;-0.00000},{1,1:+0.00000;-0.00000}", Satellites[SatNum].dRA_fromStar_Satellite, Satellites[SatNum].dDec_fromStar_Satellite + Deflection_Dec_fromStar);
				}
				stringBuilder2.AppendFormat(",{0,1:f4}", Satellites[SatNum].PAofTrack_2000);
			}
			else
			{
				if (!XYZcoords)
				{
					stringBuilder.AppendFormat("  {0,8:+0.0000;-0.0000} {1,8:+0.0000;-0.0000}", dRA_fromStar, dDec_fromStar + Deflection_Dec_fromStar);
				}
				stringBuilder.AppendFormat("  {0,7:f3}", PA_AlongTrack_2000);
				if (!XYZcoords)
				{
					stringBuilder2.AppendFormat(",{0,1:+0.00000;-0.00000},{1,1:+0.00000;-0.00000}", dRA_fromStar, dDec_fromStar + Deflection_Dec_fromStar);
				}
				stringBuilder2.AppendFormat(",{0,1:f4}", PA_AlongTrack_2000);
			}
			stringBuilder.Append(string.Format(" {0,7:f2}", RefTimeUncert_Secs).Substring(0, 8));
			stringBuilder2.Append(string.Format(",{0,1:f3}", RefTimeUncert_Secs));
			if (Satellite)
			{
				stringBuilder.Append(string.Format(" {0,7:f2}", Satellites[SatNum].Sdev_T_Conj * 86400.0).Substring(0, 8));
				stringBuilder.Append(string.Format(" {0,5:f1}", Satellites[SatNum].Sdev_AlongTrack_fromFit).Substring(0, 6));
				stringBuilder2.Append(string.Format(",{0,1:f3}", Satellites[SatNum].Sdev_T_Conj * 86400.0));
				stringBuilder2.Append(string.Format(",{0,1:f2}", Satellites[SatNum].Sdev_AlongTrack_fromFit));
				stringBuilder.Append(string.Format(" {0,5:f1}", Satellites[SatNum].AcrossTrack_fromFit_Satellite).Substring(0, 6));
				stringBuilder2.Append(string.Format(",{0,1:f2}", Satellites[SatNum].AcrossTrack_fromFit_Satellite));
			}
			else
			{
				stringBuilder.Append(string.Format(" {0,7:f2}", Sdev_T_Conj * 86400.0).Substring(0, 8));
				stringBuilder.Append(string.Format(" {0,5:f1}", Sdev_AlongTrack_fromFit).Substring(0, 6));
				stringBuilder2.Append(string.Format(",{0,1:f3}", Sdev_T_Conj * 86400.0));
				stringBuilder2.Append(string.Format(",{0,1:f2}", Sdev_AlongTrack_fromFit));
				stringBuilder.Append(string.Format(" {0,5:f1}", Sdev_AcrossTrack_fromFit).Substring(0, 6));
				stringBuilder2.Append(string.Format(",{0,1:f2}", Sdev_AcrossTrack_fromFit));
			}
			Asteroid_Observations_Reports.CalculateUncertainties(StarRA_Uncert_mas, StarDec_Uncert_mas, ra_Offset_DoubleStar_sDev_mas, dec_Offset_DoubleStar_sDev_mas, GaiaDR2_FrameRotationUncert_RA_mas, GaiaDR2_FrameRotationUncert_Dec_mas, RefTimeUncert_Secs, refHour_forAnalysis_AcrossPathUncertainty_mas, AlongTrackMotion_masSec, sdev_AlongTrack_fromFit, Sdev_AcrossTrack_fromFit, PA_AlongTrack_2000, out var rmsRA_Total_mas, out var rmsDec_Total_mas, out var Correlation, out var rmsStarRA_Total_mas, out var rmsStarDec_Total_mas, out var _);
			stringBuilder.Append(string.Format("  {0,5:f1}", rmsRA_Total_mas).Substring(0, 7));
			stringBuilder.Append(string.Format("  {0,5:f1}", rmsDec_Total_mas).Substring(0, 7));
			stringBuilder.Append(string.Format("  {0,5:f2}", Correlation).Substring(0, 7));
			stringBuilder.Append(string.Format(" {0,5:f1}", rmsStarRA_Total_mas).Substring(0, 6));
			stringBuilder.Append(string.Format(" {0,5:f1}", rmsStarDec_Total_mas).Substring(0, 6));
			stringBuilder2.Append(string.Format(",{0,1:f2}", Sdev_AlongTrack_fromFit));
			stringBuilder2.Append(string.Format(",{0,1:f2}", Sdev_AcrossTrack_fromFit));
			stringBuilder2.Append(string.Format(",{0,1:f2}", Correlation));
			stringBuilder2.Append(string.Format(",{0,5:f2}", rmsStarRA_Total_mas));
			stringBuilder2.Append(string.Format(",{0,5:f2}", rmsStarDec_Total_mas));
			if ((GaiaIssues & 2) == 2)
			{
				stringBuilder.Append("   x");
			}
			else
			{
				stringBuilder.Append("   -");
			}
			if ((GaiaIssues & 1) == 1)
			{
				stringBuilder.Append("   x");
			}
			else
			{
				stringBuilder.Append("   -");
			}
			if (ShapeModelCentered)
			{
				stringBuilder.Append("   §");
			}
			else
			{
				stringBuilder.Append("   -");
			}
			if ((GaiaIssues & 2) == 2)
			{
				stringBuilder2.Append(",x");
			}
			else
			{
				stringBuilder2.Append(",-");
			}
			if ((GaiaIssues & 1) == 1)
			{
				stringBuilder2.Append(",x");
			}
			else
			{
				stringBuilder2.Append(",-");
			}
			if (ShapeModelCentered)
			{
				stringBuilder2.Append(",§");
			}
			else
			{
				stringBuilder2.Append(",-");
			}
			CSVline = stringBuilder2.ToString();
			return stringBuilder.ToString();
		}

		public string MakeConjunctionString(bool Satellite, int SatNum, out string CSVline)
		{
			StringBuilder stringBuilder = new StringBuilder();
			StringBuilder stringBuilder2 = new StringBuilder();
			CSVline = "";
			if (!Satellite & UnseenPrimary)
			{
				return "";
			}
			if (Satellite)
			{
				if (Satellites[SatNum].IAUSatelliteID.Trim() != "?")
				{
					stringBuilder.Append(asteroidNumber.PadLeft(7) + " " + Satellites[SatNum].IAUSatelliteID.PadRight(13).Substring(0, 13));
				}
				else
				{
					stringBuilder.Append(asteroidNumber.PadLeft(7) + " (" + (asteroidNumber.Trim() + ") satellite").PadRight(12).Substring(0, 12));
				}
				stringBuilder.AppendFormat("  {0,4:F0}", Satellites[SatNum].Year_Conj);
				stringBuilder.AppendFormat("{0,3:F0}", Satellites[SatNum].Month_Conj);
				stringBuilder.AppendFormat("{0,3:F0} ", Math.Floor(Satellites[SatNum].Day_Conj));
				stringBuilder.Append(Utilities.DEGtoDMS((Satellites[SatNum].Day_Conj - Math.Floor(Satellites[SatNum].Day_Conj)) * 24.0, 2, 2, MinutesOnly: false, IncludeLeadingZeros: true));
				stringBuilder2.Append(asteroidNumber.Trim() + "," + Satellites[SatNum].IAUSatelliteID.Trim());
				stringBuilder2.AppendFormat(",{0,1:F0}", Satellites[SatNum].Year_Conj);
				stringBuilder2.AppendFormat(",{0,1:F0}", Satellites[SatNum].Month_Conj);
				stringBuilder2.AppendFormat(",{0,1:F0}", Math.Floor(Satellites[SatNum].Day_Conj));
				stringBuilder2.Append("," + Utilities.DEGtoDMS((Satellites[SatNum].Day_Conj - Math.Floor(Satellites[SatNum].Day_Conj)) * 24.0, 2, 2, MinutesOnly: false, IncludeLeadingZeros: true));
			}
			else
			{
				stringBuilder.Append(asteroidNumber.PadLeft(7) + " " + asteroidID.PadRight(13).Substring(0, 13));
				stringBuilder.AppendFormat("  {0,4:F0}", Year_Conj);
				stringBuilder.AppendFormat("{0,3:F0}", Month_Conj);
				stringBuilder.AppendFormat("{0,3:F0} ", Math.Floor(Day_Conj));
				stringBuilder.Append(Utilities.DEGtoDMS((Day_Conj - Math.Floor(Day_Conj)) * 24.0, 2, 2, MinutesOnly: false, IncludeLeadingZeros: true));
				stringBuilder2.Append(asteroidNumber.Trim() + "," + asteroidID.Trim());
				stringBuilder2.AppendFormat(",{0,1:F0}", Year_Conj);
				stringBuilder2.AppendFormat(",{0,1:F0}", Month_Conj);
				stringBuilder2.AppendFormat(",{0,1:F0}", Math.Floor(Day_Conj));
				stringBuilder2.Append("," + Utilities.DEGtoDMS((Day_Conj - Math.Floor(Day_Conj)) * 24.0, 2, 3, MinutesOnly: false, IncludeLeadingZeros: true));
			}
			if (Satellite)
			{
				stringBuilder.Append("   " + Utilities.DEGtoDMS((StarJ2000RA_Deg + (GaiaDR2_FrameRotation_RA_asec / 3600.0 + Satellites[SatNum].Sep_Conj_Star / 3600.0 * Math.Sin(Satellites[SatNum].PA_Conj_Star2000 / (180.0 / Math.PI))) / Math.Cos(StarJ2000Dec_Deg / (180.0 / Math.PI))) / 15.0, 2, 5, MinutesOnly: false, IncludeLeadingZeros: true));
				stringBuilder.Append(" " + Utilities.DEGtoDMS(StarJ2000Dec_Deg + GaiaDR2_FrameRotation_Dec_asec / 3600.0 + Satellites[SatNum].Sep_Conj_Star / 3600.0 * Math.Cos(Satellites[SatNum].PA_Conj_Star2000 / (180.0 / Math.PI)), 3, 4, MinutesOnly: false, IncludeLeadingZeros: true));
				stringBuilder2.Append("," + Utilities.DEGtoDMS((StarJ2000RA_Deg + (GaiaDR2_FrameRotation_RA_asec / 3600.0 + Satellites[SatNum].Sep_Conj_Star / 3600.0 * Math.Sin(Satellites[SatNum].PA_Conj_Star2000 / (180.0 / Math.PI))) / Math.Cos(StarJ2000Dec_Deg / (180.0 / Math.PI))) / 15.0, 2, 5, MinutesOnly: false, IncludeLeadingZeros: true));
				stringBuilder2.Append("," + Utilities.DEGtoDMS(StarJ2000Dec_Deg + GaiaDR2_FrameRotation_Dec_asec / 3600.0 + Satellites[SatNum].Sep_Conj_Star / 3600.0 * Math.Cos(Satellites[SatNum].PA_Conj_Star2000 / (180.0 / Math.PI)), 3, 4, MinutesOnly: false, IncludeLeadingZeros: true));
			}
			else
			{
				stringBuilder.Append("   " + Utilities.DEGtoDMS((StarJ2000RA_Deg + (GaiaDR2_FrameRotation_RA_asec / 3600.0 + Sep_Conj / 3600.0 * Math.Sin(PA_Conj_J2000 / (180.0 / Math.PI))) / Math.Cos(StarJ2000Dec_Deg / (180.0 / Math.PI))) / 15.0, 2, 5, MinutesOnly: false, IncludeLeadingZeros: true));
				stringBuilder.Append(" " + Utilities.DEGtoDMS(StarJ2000Dec_Deg + GaiaDR2_FrameRotation_Dec_asec / 3600.0 + Sep_Conj / 3600.0 * Math.Cos(PA_Conj_J2000 / (180.0 / Math.PI)), 3, 4, MinutesOnly: false, IncludeLeadingZeros: true));
				stringBuilder2.Append("," + Utilities.DEGtoDMS((StarJ2000RA_Deg + (GaiaDR2_FrameRotation_RA_asec / 3600.0 + Sep_Conj / 3600.0 * Math.Sin(PA_Conj_J2000 / (180.0 / Math.PI))) / Math.Cos(StarJ2000Dec_Deg / (180.0 / Math.PI))) / 15.0, 2, 6, MinutesOnly: false, IncludeLeadingZeros: true));
				stringBuilder2.Append("," + Utilities.DEGtoDMS(StarJ2000Dec_Deg + GaiaDR2_FrameRotation_Dec_asec / 3600.0 + Sep_Conj / 3600.0 * Math.Cos(PA_Conj_J2000 / (180.0 / Math.PI)), 3, 5, MinutesOnly: false, IncludeLeadingZeros: true));
			}
			stringBuilder.Append("   " + FormattedStarRA);
			stringBuilder.Append(" " + FormattedStarDec);
			stringBuilder2.Append("," + FormattedStarRA_CSV);
			stringBuilder2.Append("," + FormattedStarDec_CSV);
			stringBuilder.Append("  ");
			string text = ((GaiaVersion == "0") ? "Hip2  " : ((GaiaVersion == "1") ? "Gaia1 " : ((GaiaVersion == "2") ? "Gaia2 " : ((GaiaVersion == "3") ? "Gaia3E" : ((GaiaVersion == "4") ? "Gaia4 " : ((!(GaiaVersion == "9")) ? "NA" : "UBSC  "))))));
			stringBuilder.Append(text);
			stringBuilder2.Append("," + text.Trim());
			if (Satellite)
			{
				stringBuilder.AppendFormat("{0,4:F0}", Satellites[SatNum].Number_ChordsSatellite);
				stringBuilder2.AppendFormat(",{0,1:F0}", Satellites[SatNum].Number_ChordsSatellite);
			}
			else
			{
				stringBuilder.AppendFormat("{0,4:F0}", number_Chords);
				stringBuilder2.AppendFormat(",{0,1:F0}", number_Chords);
			}
			if (Satellite)
			{
				stringBuilder.AppendFormat("  {0,8:f4} {1,7:F3}", Satellites[SatNum].Sep_Conj_Star, Satellites[SatNum].PA_Conj_Star2000);
				stringBuilder.AppendFormat("  {0,7:f3}", Satellites[SatNum].PAofTrack_2000);
				stringBuilder2.AppendFormat(",{0,1:f5},{1,1:F4}", Satellites[SatNum].Sep_Conj_Star, Satellites[SatNum].PA_Conj_Star2000);
				stringBuilder2.AppendFormat(",{0,1:f4}", Satellites[SatNum].PAofTrack_2000);
			}
			else
			{
				stringBuilder.AppendFormat("  {0,8:f4} {1,7:F3}", Sep_Conj, PA_Conj_J2000);
				stringBuilder.AppendFormat("  {0,7:f3}", PA_AlongTrack_2000);
				stringBuilder2.AppendFormat(",{0,1:f5},{1,1:F4}", Sep_Conj, PA_Conj_J2000);
				stringBuilder2.AppendFormat(",{0,1:f4}", PA_AlongTrack_2000);
			}
			stringBuilder.Append(string.Format(" {0,7:f2}", RefTimeUncert_Secs).Substring(0, 8));
			stringBuilder2.Append(string.Format(",{0,1:f3}", RefTimeUncert_Secs));
			if (Satellite)
			{
				stringBuilder.Append(string.Format(" {0,7:f2}", Satellites[SatNum].Sdev_T_Conj * 86400.0).Substring(0, 8));
				stringBuilder.Append(string.Format(" {0,5:f1}", Satellites[SatNum].Sdev_AlongTrack_fromFit).Substring(0, 6));
				stringBuilder2.Append(string.Format(",{0,1:f3}", Satellites[SatNum].Sdev_T_Conj * 86400.0));
				stringBuilder2.Append(string.Format(",{0,1:f2}", Satellites[SatNum].Sdev_AlongTrack_fromFit));
				stringBuilder.Append(string.Format(" {0,5:f1}", Satellites[SatNum].AcrossTrack_fromFit_Satellite).Substring(0, 6));
				stringBuilder2.Append(string.Format(",{0,1:f2}", Satellites[SatNum].AcrossTrack_fromFit_Satellite));
			}
			else
			{
				stringBuilder.Append(string.Format(" {0,7:f2}", Sdev_T_Conj * 86400.0).Substring(0, 8));
				stringBuilder.Append(string.Format(" {0,5:f1}", Sdev_AlongTrack_fromFit).Substring(0, 6));
				stringBuilder2.Append(string.Format(",{0,1:f3}", Sdev_T_Conj * 86400.0));
				stringBuilder2.Append(string.Format(",{0,1:f2}", Sdev_AlongTrack_fromFit));
				stringBuilder.Append(string.Format(" {0,5:f1}", Sdev_AcrossTrack_fromFit).Substring(0, 6));
				stringBuilder2.Append(string.Format(",{0,1:f2}", Sdev_AcrossTrack_fromFit));
			}
			stringBuilder.Append(string.Format(" {0,5:f1}", Math.Sqrt(StarRA_Uncert_mas * StarRA_Uncert_mas + RA_Offset_DoubleStar_sDev_mas * RA_Offset_DoubleStar_sDev_mas + GaiaDR2_FrameRotationUncert_RA_asec * 1000.0 * GaiaDR2_FrameRotationUncert_RA_asec * 1000.0)).Substring(0, 6));
			stringBuilder.Append(string.Format(" {0,5:f1}", Math.Sqrt(StarDec_Uncert_mas * StarDec_Uncert_mas + Dec_Offset_DoubleStar_sDev_mas * Dec_Offset_DoubleStar_sDev_mas + GaiaDR2_FrameRotationUncert_Dec_asec * 1000.0 * GaiaDR2_FrameRotationUncert_Dec_asec * 1000.0)).Substring(0, 6));
			stringBuilder2.Append(string.Format(",{0,5:f2}", Math.Sqrt(StarRA_Uncert_mas * StarRA_Uncert_mas + RA_Offset_DoubleStar_sDev_mas * RA_Offset_DoubleStar_sDev_mas + GaiaDR2_FrameRotationUncert_RA_asec * 1000.0 * GaiaDR2_FrameRotationUncert_RA_asec * 1000.0)));
			stringBuilder2.Append(string.Format(",{0,5:f2}", Math.Sqrt(StarDec_Uncert_mas * StarDec_Uncert_mas + Dec_Offset_DoubleStar_sDev_mas * Dec_Offset_DoubleStar_sDev_mas + GaiaDR2_FrameRotationUncert_Dec_asec * 1000.0 * GaiaDR2_FrameRotationUncert_Dec_asec * 1000.0)));
			if ((GaiaIssues & 2) == 2)
			{
				stringBuilder.Append("   x");
			}
			else
			{
				stringBuilder.Append("   -");
			}
			if ((GaiaIssues & 1) == 1)
			{
				stringBuilder.Append("   x");
			}
			else
			{
				stringBuilder.Append("   -");
			}
			if (ShapeModelCentered)
			{
				stringBuilder.Append("    §");
			}
			else
			{
				stringBuilder.Append("    -");
			}
			if ((GaiaIssues & 2) == 2)
			{
				stringBuilder2.Append(",x");
			}
			else
			{
				stringBuilder2.Append(",-");
			}
			if ((GaiaIssues & 1) == 1)
			{
				stringBuilder2.Append(",x");
			}
			else
			{
				stringBuilder2.Append(",-");
			}
			if (ShapeModelCentered)
			{
				stringBuilder2.Append(",§");
			}
			else
			{
				stringBuilder2.Append(",-");
			}
			CSVline = stringBuilder2.ToString();
			return stringBuilder.ToString();
		}
	}
}
