using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;

namespace Occult.Asteroid_Observations
{
	internal static class EventDetails
	{
		private static List<ObserverData> observers = new List<ObserverData>();

		private static List<DoubleData> doubledata = new List<DoubleData>();

		private static List<SatelliteData> satellitedata = new List<SatelliteData>();

		private static List<ShapeModelData> shapes = new List<ShapeModelData>();

		private static bool sourceFileIsXML = true;

		private const double Radian = 180.0 / Math.PI;

		private static string asteroidNumber = "";

		private static string asteroidID = "";

		private static int year;

		private static int month;

		private static int day;

		private static string starCat = "";

		private static string starNumber = "";

		private static string gaia_id = "";

		private static double midT = 0.0;

		private static double refHour_forAnalysis = 0.0;

		private static double refHour_forAnalysis_Uncert = 0.0;

		private static double refHour_forAnalysis_AcrossPathUncertainty_mas = 0.0;

		private static double rA_Star_Apparent = 0.0;

		private static double dec_Star_Apparent = 0.0;

		private static double rA_Star_2000 = 0.0;

		private static double dec_Star_2000 = 0.0;

		private static double rA_Star_uncertainty = 0.0;

		private static double dec_Star_uncertainty;

		private static double starDia_mas = 0.0;

		private static int issues = 0;

		private static int gaiaVersion = 0;

		private static double d_RA = 0.0;

		private static double d2_RA = 0.0;

		private static double d3_RA = 0.0;

		private static double d_Dec = 0.0;

		private static double d2_Dec = 0.0;

		private static double d3_Dec = 0.0;

		private static double parallax = 1E-06;

		private static double d_parallax = 1E-06;

		private static double d_X = 0.0;

		private static double d2_X = 0.0;

		private static double d3_X = 0.0;

		private static double d_Y = 0.0;

		private static double d2_Y = 0.0;

		private static double d3_Y = 0.0;

		private static int yearAdded = 1900;

		private static int monthAdded = 1;

		private static int dayAdded = 1;

		private static int yearEdited = 1900;

		private static int monthEdited = 1;

		private static int dayEdited = 1;

		private static double mbStar = 25.0;

		private static double mgStar = 25.0;

		private static double mrStar = 25.0;

		private static double mvAsteroid = 20.0;

		private static double asteroidNominalDiameter = 0.0;

		private static double diameterUncertainty = 1.0;

		private static long kepler2ID = 0L;

		private static double x_Dia = 0.0001;

		private static double y_Dia = 0.0001;

		private static double x = 0.0;

		private static double y = 0.0;

		private static double pA_Ellipse = 0.0;

		private static double centreOfMass_Offset_X = 0.0;

		private static double centreOfMass_Offset_Y;

		private static double draCosDec_atEvent = 0.0;

		private static double ddec_atEvent = 0.0;

		private static double sdev_draCosDec_atEvent = 0.0;

		private static double sdev_ddec_atEvent = 0.0;

		private static int quality = 0;

		private static int numberOfDoubleSolutions = 0;

		private static int numberOfSatellites = 0;

		private static int flagForReview = 0;

		private static double sdev_Major = 0.0;

		private static double sdev_Minor = 0.0;

		private static double sdev_PA_Ellipse = 0.0;

		private static double sdev_Sep = 0.0;

		private static double sdev_PA_Star = 0.0;

		private static double sdev_X = 0.0;

		private static double sdev_Y = 0.0;

		private static bool nullHeader;

		private static bool sdev_Major_Set;

		private static bool sdev_Minor_Set;

		private static bool sdev_PA_Ellipse_Set;

		private static bool sdev_Sep_Set;

		private static bool sdev_PA_Star_Set;

		private static bool sdev_X_Set;

		private static bool sdev_Y_Set;

		private static bool inc_Miss = true;

		private static bool solve_X = true;

		private static bool solve_Y = true;

		private static bool solve_Major = true;

		private static bool solve_Minor = true;

		private static bool solve_PA_Ellipse = true;

		private static bool solve_Sep = false;

		private static bool solve_PA_Star = false;

		private static bool solve_Circular = false;

		private static bool usedAssumedDiameter = false;

		private static bool starIsDouble = false;

		private static bool asteroidHasSatellite = false;

		private static bool unseenBinaryPrimary = false;

		private static bool shapeModelFitted = false;

		private static bool astrometryShapeModelCentered = false;

		private static string jDSO_SubmittedDate = "";

		private static string jDSO_Vol_Num_Pg = "";

		private static string mpcDate = "";

		private static string mpcNumber = "";

		private static string mpcSubmissionID;

		private static string fitUncertaintyCategory = "";

		private static double adjusted_sDev_AlongTrack = 0.0;

		private static double adjusted_sDev_AcrossTrack = 0.0;

		private static double maxPlusHit = 9.0;

		private static double maxMinusHit = -9.0;

		private static double minPlusMiss = -9.0;

		private static double minMinusMiss = 9.0;

		private static double starReliability;

		private static int duplicatedSource = -1;

		private static int noGaiaPM = -1;

		private static int gaiaPMfromUCAC4 = -1;

		private static int brightnessRatio_UncertaintyPercent = 10;

		private static double ra_Offset_DoubleStar_mas = 0.0;

		private static double dec_Offset_DoubleStar_mas = 0.0;

		private static double ra_Offset_DoubleStar_sDev_mas = 0.0;

		private static double dec_Offset_DoubleStar_sDev_mas = 0.0;

		private static double brightnessRatio_SolnStar_to_Companion = 1.2;

		private static string knownPair_ID = "";

		internal static int[] Quality_to_cmbQuality = new int[7] { 0, 3, 4, 5, 6, 2, 1 };

		internal static int[] Quality_from_cmbQuality = new int[7] { 0, 6, 5, 1, 2, 3, 4 };

		private static double x_Geo_atEvent;

		private static double y_Geo_atEvent;

		private static double x_Geo_atConj;

		private static double y_Geo_atConj;

		private static double sep_km_atConj;

		private static int year_Conj;

		private static int month_Conj;

		private static int number_Chords = 0;

		private static int number_SatelliteChords = 0;

		private static double day_Conj;

		private static double sdev_T_Conj = 0.0;

		private static double sDev_AlongTrack = 0.0;

		private static double sep_Conj;

		private static double sdev_Sep_Conj = 0.0;

		private static double pA_Conj_2000;

		public static List<ObserverData> Observers
		{
			get
			{
				return observers;
			}
			set
			{
				observers = value;
			}
		}

		public static List<DoubleData> Doubles
		{
			get
			{
				return doubledata;
			}
			set
			{
				doubledata = value;
			}
		}

		public static List<SatelliteData> Satellites
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

		public static List<ShapeModelData> ShapeData
		{
			get
			{
				return shapes;
			}
			set
			{
				shapes = value;
			}
		}

		public static string AsteroidNumber
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

		public static int AsteroidNo
		{
			get
			{
				int.TryParse(asteroidNumber, out var result);
				return result;
			}
		}

		public static string AsteroidID
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

		public static string StarCat
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

		public static string StarNumber
		{
			get
			{
				return starNumber;
			}
			set
			{
				starNumber = value;
			}
		}

		public static string StarCatwithNumber
		{
			get
			{
				if (starCat.Contains("UCAC"))
				{
					return "UCAC4 " + starNumber;
				}
				if (starCat == "HIP")
				{
					return "HIP " + starNumber;
				}
				if (starCat.Contains("TYC"))
				{
					return "TYC " + starNumber;
				}
				if (starCat.Contains("Tycho2"))
				{
					return "TYC " + starNumber;
				}
				if (starCat == "J-coords")
				{
					return "J" + starNumber;
				}
				return starCat + " " + starNumber;
			}
		}

		public static int Year
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

		public static int Month
		{
			get
			{
				return month;
			}
			set
			{
				month = value;
			}
		}

		public static int Day
		{
			get
			{
				return day;
			}
			set
			{
				day = value;
			}
		}

		public static double MidT_forMotions
		{
			get
			{
				return midT;
			}
			set
			{
				midT = value;
			}
		}

		public static double dX
		{
			get
			{
				return d_X;
			}
			set
			{
				d_X = value;
			}
		}

		public static double d2X
		{
			get
			{
				return d2_X;
			}
			set
			{
				d2_X = value;
			}
		}

		public static double d3X
		{
			get
			{
				return d3_X;
			}
			set
			{
				d3_X = value;
			}
		}

		public static double dY
		{
			get
			{
				return d_Y;
			}
			set
			{
				d_Y = value;
			}
		}

		public static double d2Y
		{
			get
			{
				return d2_Y;
			}
			set
			{
				d2_Y = value;
			}
		}

		public static double d3Y
		{
			get
			{
				return d3_Y;
			}
			set
			{
				d3_Y = value;
			}
		}

		public static double Parallax
		{
			get
			{
				return parallax;
			}
			set
			{
				parallax = value;
			}
		}

		public static double dParallax
		{
			get
			{
				return d_parallax;
			}
			set
			{
				d_parallax = value;
			}
		}

		public static double dRA
		{
			get
			{
				return d_RA;
			}
			set
			{
				d_RA = value;
			}
		}

		public static double d2RA
		{
			get
			{
				return d2_RA;
			}
			set
			{
				d2_RA = value;
			}
		}

		public static double d3RA
		{
			get
			{
				return d3_RA;
			}
			set
			{
				d3_RA = value;
			}
		}

		public static double dDec
		{
			get
			{
				return d_Dec;
			}
			set
			{
				d_Dec = value;
			}
		}

		public static double d2Dec
		{
			get
			{
				return d2_Dec;
			}
			set
			{
				d2_Dec = value;
			}
		}

		public static double d3Dec
		{
			get
			{
				return d3_Dec;
			}
			set
			{
				d3_Dec = value;
			}
		}

		public static int GaiaVersion
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

		public static string Gaia_ID
		{
			get
			{
				return gaia_id;
			}
			set
			{
				gaia_id = value;
			}
		}

		public static int YearAdded
		{
			get
			{
				return yearAdded;
			}
			set
			{
				yearAdded = value;
			}
		}

		public static int MonthAdded
		{
			get
			{
				return monthAdded;
			}
			set
			{
				monthAdded = value;
			}
		}

		public static int DayAdded
		{
			get
			{
				return dayAdded;
			}
			set
			{
				dayAdded = value;
			}
		}

		public static int YearEdited
		{
			get
			{
				return yearEdited;
			}
			set
			{
				yearEdited = value;
			}
		}

		public static int MonthEdited
		{
			get
			{
				return monthEdited;
			}
			set
			{
				monthEdited = value;
			}
		}

		public static int DayEdited
		{
			get
			{
				return dayEdited;
			}
			set
			{
				dayEdited = value;
			}
		}

		public static bool SourceFileIsXML
		{
			get
			{
				return sourceFileIsXML;
			}
			set
			{
				sourceFileIsXML = value;
			}
		}

		public static double SortDate => (double)(year * 10000 + month * 100 + day) + midT / 24.0;

		public static double JD_EventDate => Utilities.JD_from_Date(Year, Month, (double)Day + midT / 24.0);

		public static string FormattedDate => string.Format("{0,2:F0}{1,3:F0}{2,3:F0}", year, month, day);

		public static double RA_Star_2000
		{
			get
			{
				return rA_Star_2000;
			}
			set
			{
				rA_Star_2000 = value;
			}
		}

		public static double Dec_Star_2000
		{
			get
			{
				return dec_Star_2000;
			}
			set
			{
				dec_Star_2000 = value;
			}
		}

		public static double RA_Star_Uncertainty_mas
		{
			get
			{
				return rA_Star_uncertainty;
			}
			set
			{
				rA_Star_uncertainty = value;
			}
		}

		public static double Dec_Star_Uncertainty_mas
		{
			get
			{
				return dec_Star_uncertainty;
			}
			set
			{
				dec_Star_uncertainty = value;
			}
		}

		public static double StarDia_mas
		{
			get
			{
				return starDia_mas;
			}
			set
			{
				starDia_mas = value;
			}
		}

		public static int IssuesFlag
		{
			get
			{
				return issues;
			}
			set
			{
				issues = value;
			}
		}

		public static double RA_Star_Apparent
		{
			get
			{
				return rA_Star_Apparent;
			}
			set
			{
				rA_Star_Apparent = value;
			}
		}

		public static double Dec_Star_Apparent
		{
			get
			{
				return dec_Star_Apparent;
			}
			set
			{
				dec_Star_Apparent = value;
			}
		}

		public static double MbStar
		{
			get
			{
				return mbStar;
			}
			set
			{
				mbStar = value;
			}
		}

		public static double MgStar
		{
			get
			{
				return mgStar;
			}
			set
			{
				mgStar = value;
			}
		}

		public static double MrStar
		{
			get
			{
				return mrStar;
			}
			set
			{
				mrStar = value;
			}
		}

		public static double MvAsteroid
		{
			get
			{
				return mvAsteroid;
			}
			set
			{
				mvAsteroid = value;
			}
		}

		public static double AsteroidNominalDiameter
		{
			get
			{
				return asteroidNominalDiameter;
			}
			set
			{
				asteroidNominalDiameter = value;
			}
		}

		public static double DiameterUncertainty
		{
			get
			{
				return diameterUncertainty;
			}
			set
			{
				diameterUncertainty = value;
			}
		}

		public static double StarReliability
		{
			get
			{
				return starReliability;
			}
			set
			{
				starReliability = value;
			}
		}

		public static int DuplicatedSource
		{
			get
			{
				return duplicatedSource;
			}
			set
			{
				duplicatedSource = value;
			}
		}

		public static int NoGaiaPM
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

		public static int GaiaPMfromUCAC4
		{
			get
			{
				return gaiaPMfromUCAC4;
			}
			set
			{
				gaiaPMfromUCAC4 = value;
			}
		}

		public static double RA_Offset_DoubleStar_mas
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

		public static double Dec_Offset_DoubleStar_mas
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

		public static double RA_Offset_DoubleStar_sDev_mas
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

		public static double Dec_Offset_DoubleStar_sDev_mas
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

		public static double BrightnessRatio_SolnStar_to_Companion
		{
			get
			{
				return brightnessRatio_SolnStar_to_Companion;
			}
			set
			{
				brightnessRatio_SolnStar_to_Companion = value;
			}
		}

		public static int BrightnessRatio_UncertaintyPercent
		{
			get
			{
				if (brightnessRatio_UncertaintyPercent < 10)
				{
					brightnessRatio_UncertaintyPercent = 10;
				}
				return brightnessRatio_UncertaintyPercent;
			}
			set
			{
				brightnessRatio_UncertaintyPercent = value;
			}
		}

		public static string KnownPair_ID
		{
			get
			{
				return knownPair_ID;
			}
			set
			{
				knownPair_ID = value;
			}
		}

		public static double X
		{
			get
			{
				return x;
			}
			set
			{
				x = value;
			}
		}

		public static double Y
		{
			get
			{
				return y;
			}
			set
			{
				y = value;
			}
		}

		public static double CentreOfMass_Offset_X
		{
			get
			{
				return centreOfMass_Offset_X;
			}
			set
			{
				centreOfMass_Offset_X = value;
			}
		}

		public static double CentreOfMass_Offset_Y
		{
			get
			{
				return centreOfMass_Offset_Y;
			}
			set
			{
				centreOfMass_Offset_Y = value;
			}
		}

		public static double X_Dia
		{
			get
			{
				return x_Dia;
			}
			set
			{
				x_Dia = value;
			}
		}

		public static double Y_Dia
		{
			get
			{
				return y_Dia;
			}
			set
			{
				y_Dia = value;
			}
		}

		public static double PA_Ellipse
		{
			get
			{
				return pA_Ellipse;
			}
			set
			{
				pA_Ellipse = value;
			}
		}

		public static double X_Geo_atEvent
		{
			get
			{
				return x_Geo_atEvent;
			}
			set
			{
				x_Geo_atEvent = value;
			}
		}

		public static double Y_Geo_atEvent
		{
			get
			{
				return y_Geo_atEvent;
			}
			set
			{
				y_Geo_atEvent = value;
			}
		}

		public static double X_Geo_CenterOfMass_atEvent => X_Geo_atEvent + CentreOfMass_Offset_X;

		public static double Y_Geo_CenterOfMass_atEvent => Y_Geo_atEvent + CentreOfMass_Offset_Y;

		public static double dRACosDec_atEvent
		{
			get
			{
				return draCosDec_atEvent;
			}
			set
			{
				draCosDec_atEvent = value;
			}
		}

		public static double dDec_atEvent
		{
			get
			{
				return ddec_atEvent;
			}
			set
			{
				ddec_atEvent = value;
			}
		}

		public static double Sdev_dRACosDec_atEvent
		{
			get
			{
				return sdev_draCosDec_atEvent;
			}
			set
			{
				sdev_draCosDec_atEvent = value;
			}
		}

		public static double Sdev_dDec_atEvent
		{
			get
			{
				return sdev_ddec_atEvent;
			}
			set
			{
				sdev_ddec_atEvent = value;
			}
		}

		public static double MaxHitPlus
		{
			get
			{
				return maxPlusHit;
			}
			set
			{
				maxPlusHit = value;
			}
		}

		public static double MaxHitMinus
		{
			get
			{
				return maxMinusHit;
			}
			set
			{
				maxMinusHit = value;
			}
		}

		public static double MinMissPlus
		{
			get
			{
				return minPlusMiss;
			}
			set
			{
				minPlusMiss = value;
			}
		}

		public static double MinMissMinus
		{
			get
			{
				return minMinusMiss;
			}
			set
			{
				minMinusMiss = value;
			}
		}

		public static int Quality
		{
			get
			{
				return quality;
			}
			set
			{
				quality = value;
			}
		}

		public static int Quality_To_cmbQuality => Quality_to_cmbQuality[quality];

		public static int Quality_From_cmbQuality
		{
			set
			{
				quality = Quality_from_cmbQuality[value];
			}
		}

		public static int FlagForReview
		{
			get
			{
				return flagForReview;
			}
			set
			{
				flagForReview = value;
			}
		}

		public static bool AstrometryShapeModelCentered
		{
			get
			{
				return astrometryShapeModelCentered;
			}
			set
			{
				astrometryShapeModelCentered = value;
			}
		}

		public static bool UnseenBinaryPrimary
		{
			get
			{
				return unseenBinaryPrimary;
			}
			set
			{
				unseenBinaryPrimary = value;
			}
		}

		public static double Sdev_Major
		{
			get
			{
				return sdev_Major;
			}
			set
			{
				sdev_Major = value;
			}
		}

		public static double Sdev_Minor
		{
			get
			{
				return sdev_Minor;
			}
			set
			{
				sdev_Minor = value;
			}
		}

		public static double Sdev_PA_Ellipse
		{
			get
			{
				return sdev_PA_Ellipse;
			}
			set
			{
				sdev_PA_Ellipse = value;
			}
		}

		public static double Sdev_Sep
		{
			get
			{
				return sdev_Sep;
			}
			set
			{
				sdev_Sep = value;
			}
		}

		public static double Sdev_PA_Star
		{
			get
			{
				return sdev_PA_Star;
			}
			set
			{
				sdev_PA_Star = value;
			}
		}

		public static double Sdev_X
		{
			get
			{
				return sdev_X;
			}
			set
			{
				sdev_X = value;
			}
		}

		public static double Sdev_Y
		{
			get
			{
				return sdev_Y;
			}
			set
			{
				sdev_Y = value;
			}
		}

		public static double AdjustmentTo_sDev_AlongTrack
		{
			get
			{
				return adjusted_sDev_AlongTrack;
			}
			set
			{
				adjusted_sDev_AlongTrack = value;
			}
		}

		public static double AdjustmentTo_sDev_AcrossTrack
		{
			get
			{
				return adjusted_sDev_AcrossTrack;
			}
			set
			{
				adjusted_sDev_AcrossTrack = value;
			}
		}

		public static string FitUncertaintyCategory
		{
			get
			{
				return fitUncertaintyCategory;
			}
			set
			{
				fitUncertaintyCategory = value;
			}
		}

		public static bool Sdev_Major_Set
		{
			get
			{
				return sdev_Major_Set;
			}
			set
			{
				sdev_Major_Set = value;
			}
		}

		public static bool Sdev_Minor_Set
		{
			get
			{
				return sdev_Minor_Set;
			}
			set
			{
				sdev_Minor_Set = value;
			}
		}

		public static bool Sdev_PA_Ellipse_Set
		{
			get
			{
				return sdev_PA_Ellipse_Set;
			}
			set
			{
				sdev_PA_Ellipse_Set = value;
			}
		}

		public static bool Sdev_Sep_Set
		{
			get
			{
				return sdev_Sep_Set;
			}
			set
			{
				sdev_Sep_Set = value;
			}
		}

		public static bool Sdev_PA_Star_Set
		{
			get
			{
				return sdev_PA_Star_Set;
			}
			set
			{
				sdev_PA_Star_Set = value;
			}
		}

		public static bool Sdev_X_Set
		{
			get
			{
				return sdev_X_Set;
			}
			set
			{
				sdev_X_Set = value;
			}
		}

		public static bool Sdev_Y_Set
		{
			get
			{
				return sdev_Y_Set;
			}
			set
			{
				sdev_Y_Set = value;
			}
		}

		public static bool ShapeModelFitted
		{
			get
			{
				return shapeModelFitted;
			}
			set
			{
				shapeModelFitted = value;
			}
		}

		public static bool StarIsDouble
		{
			get
			{
				return starIsDouble;
			}
			set
			{
				starIsDouble = value;
			}
		}

		public static int NumberOfDoubleSolutions
		{
			get
			{
				return numberOfDoubleSolutions;
			}
			set
			{
				numberOfDoubleSolutions = value;
			}
		}

		public static string JDSO_SubmitDate
		{
			get
			{
				return jDSO_SubmittedDate;
			}
			set
			{
				jDSO_SubmittedDate = value;
			}
		}

		public static string JDSO_Vol_Num_Pg
		{
			get
			{
				return jDSO_Vol_Num_Pg;
			}
			set
			{
				jDSO_Vol_Num_Pg = value;
			}
		}

		public static bool AsteroidHasSatellite
		{
			get
			{
				return asteroidHasSatellite;
			}
			set
			{
				asteroidHasSatellite = value;
			}
		}

		public static int NumberOfSatellites
		{
			get
			{
				return numberOfSatellites;
			}
			set
			{
				numberOfSatellites = value;
			}
		}

		public static long Kepler2ID
		{
			get
			{
				return kepler2ID;
			}
			set
			{
				kepler2ID = value;
			}
		}

		public static bool Inc_Miss
		{
			get
			{
				return inc_Miss;
			}
			set
			{
				inc_Miss = value;
			}
		}

		public static bool Solve_X
		{
			get
			{
				return solve_X;
			}
			set
			{
				solve_X = value;
			}
		}

		public static bool Solve_Y
		{
			get
			{
				return solve_Y;
			}
			set
			{
				solve_Y = value;
			}
		}

		public static bool Solve_Major
		{
			get
			{
				return solve_Major;
			}
			set
			{
				solve_Major = value;
			}
		}

		public static bool Solve_Minor
		{
			get
			{
				return solve_Minor;
			}
			set
			{
				solve_Minor = value;
			}
		}

		public static bool Solve_PA
		{
			get
			{
				return solve_PA_Ellipse;
			}
			set
			{
				solve_PA_Ellipse = value;
			}
		}

		public static bool Solve_CompanionSep
		{
			get
			{
				return solve_Sep;
			}
			set
			{
				solve_Sep = value;
			}
		}

		public static bool Solve_CompanionPA
		{
			get
			{
				return solve_PA_Star;
			}
			set
			{
				solve_PA_Star = value;
			}
		}

		public static bool Solve_Circular
		{
			get
			{
				return solve_Circular;
			}
			set
			{
				solve_Circular = value;
			}
		}

		public static bool UsedAssumedDiameter
		{
			get
			{
				return usedAssumedDiameter;
			}
			set
			{
				usedAssumedDiameter = value;
			}
		}

		public static string MPCDate
		{
			get
			{
				return mpcDate;
			}
			set
			{
				mpcDate = value;
			}
		}

		public static string MPCNumber
		{
			get
			{
				return mpcNumber;
			}
			set
			{
				mpcNumber = value;
			}
		}

		public static string MPCsubmissionID
		{
			get
			{
				return mpcSubmissionID;
			}
			set
			{
				mpcSubmissionID = value;
			}
		}

		public static double RefHour_forAnalysis
		{
			get
			{
				return refHour_forAnalysis;
			}
			set
			{
				refHour_forAnalysis = value;
			}
		}

		public static double RefHour_forAnalysis_Uncert_secs
		{
			get
			{
				return refHour_forAnalysis_Uncert;
			}
			set
			{
				refHour_forAnalysis_Uncert = value;
			}
		}

		public static double RefHour_forAnalysis_AcrossPathUncertainty_mas
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

		public static double X_Geo_atConj
		{
			get
			{
				return x_Geo_atConj;
			}
			set
			{
				x_Geo_atConj = value;
			}
		}

		public static double Y_Geo_atConj
		{
			get
			{
				return y_Geo_atConj;
			}
			set
			{
				y_Geo_atConj = value;
			}
		}

		public static double Sep_km_atConj
		{
			get
			{
				return sep_km_atConj;
			}
			set
			{
				sep_km_atConj = value;
			}
		}

		public static int Year_Conj
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

		public static int Month_Conj
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

		public static double Day_Conj
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

		public static double Sep_Conj
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

		public static double PA_Conj_2000
		{
			get
			{
				return pA_Conj_2000;
			}
			set
			{
				pA_Conj_2000 = value;
			}
		}

		public static double Sdev_Sep_Conj
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

		public static double Sdev_T_Conj
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

		public static double Sdev_AlongTrack
		{
			get
			{
				return sDev_AlongTrack;
			}
			set
			{
				sDev_AlongTrack = value;
			}
		}

		public static int Number_Chords
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

		public static int Number_SatelliteChords
		{
			get
			{
				return number_SatelliteChords;
			}
			set
			{
				number_SatelliteChords = value;
			}
		}

		public static bool NullHeader
		{
			get
			{
				return nullHeader;
			}
			set
			{
				nullHeader = value;
			}
		}

		public static string PDS_List_Line(bool MarkFieldsWithPipes, out bool IsAsteroid)
		{
			//IL_07c0: Unknown result type (might be due to invalid IL or missing references)
			IsAsteroid = !AsteroidNumber.Contains("P");
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append("|" + string.Format("{0,4:F0}-{1,2:F0}-{2,2:F0}", Year, Month, Day).Replace(' ', '0') + "|");
			if (IsAsteroid)
			{
				stringBuilder.Append(AsteroidNumber.PadLeft(7) + "|");
				if (!int.TryParse(AsteroidID.PadRight(4).Substring(0, 4), out var _))
				{
					stringBuilder.Append(AsteroidID.PadRight(17) + "|-".PadRight(11) + "|");
				}
				else
				{
					stringBuilder.Append("-".PadRight(17) + "|" + AsteroidID.PadRight(10) + "|");
				}
			}
			else
			{
				stringBuilder.Append(" mVEMJSUNP".Substring(int.Parse(AsteroidNumber.Substring(1, 1)), 1) + AsteroidNumber.Substring(3, 2));
				stringBuilder.Append("|" + AsteroidID.PadRight(8) + "|");
			}
			string text = StarCat + " " + StarNumber;
			if ((StarCat == "J-coords") | (StarCat == "G-coords"))
			{
				if (StarNumber.Length > 15)
				{
					int num = StarNumber.IndexOf("+");
					if (num < 0)
					{
						num = StarNumber.IndexOf("-");
					}
					if (num >= 8)
					{
						StarNumber = StarNumber.Substring(0, 8) + StarNumber.Substring(num);
					}
					if (StarNumber.Length > 15)
					{
						StarNumber = StarNumber.Substring(0, 15);
					}
				}
				text = "Gaia " + StarNumber;
			}
			stringBuilder.Append(text.PadRight(20) + "|");
			char[] array = (Utilities.DEGtoDMS(RA_Star_2000 * (180.0 / Math.PI) / 15.0, 2, 5, MinutesOnly: false, IncludeLeadingZeros: true) + "|" + Utilities.DEGtoDMS(Dec_Star_2000 * (180.0 / Math.PI), 3, 4, MinutesOnly: false, IncludeLeadingZeros: true, IncludePlusSymbol: true).Insert(1, "|")).ToCharArray();
			char c;
			array[19] = (c = (array[22] = '|'));
			array[16] = (c = c);
			array[14] = (c = c);
			array[5] = (c = c);
			array[2] = c;
			stringBuilder.Append(new string(array));
			if (GaiaVersion == 0)
			{
				stringBuilder.Append("|9");
			}
			else
			{
				stringBuilder.Append("|" + GaiaVersion);
			}
			stringBuilder.AppendFormat("|{0,4:F1}|", MidT_forMotions);
			stringBuilder.Append(dX.ToString("+00.00000;-00.00000; 00.00000").Substring(0, 9) + "|");
			stringBuilder.Append(d2X.ToString("+0.00000;-0.00000; 0.00000").Substring(0, 8) + "|");
			stringBuilder.Append(d3X.ToString("+0.00000;-0.00000; 0.00000").Substring(0, 8) + "|");
			stringBuilder.Append(dY.ToString("+00.00000;-00.00000; 00.00000").PadLeft(9).Substring(0, 9) + "|");
			stringBuilder.Append(d2Y.ToString("+0.00000;-0.00000; 0.00000").Substring(0, 8) + "|");
			stringBuilder.Append(d3Y.ToString("+0.00000;-0.00000; 0.00000").Substring(0, 8) + "|");
			stringBuilder.AppendFormat("{0,7:F4}|", Parallax);
			stringBuilder.AppendFormat("{0,7:F4}|", dParallax);
			if ((YearAdded > 0) & (YearAdded != 1900))
			{
				stringBuilder.Append(string.Format("{0,4:F0}-{1,2:F0}-{2,2:F0}", YearAdded, MonthAdded, DayAdded).Replace(' ', '0') + "|");
			}
			else
			{
				stringBuilder.Append("9999-01-01|");
			}
			if ((YearEdited > 0) & (YearEdited != 1900))
			{
				stringBuilder.Append(string.Format("{0,4:F0}-{1,2:F0}-{2,2:F0}", YearEdited, MonthEdited, DayEdited).Replace(' ', '0') + "|");
			}
			else
			{
				stringBuilder.Append("9999-01-01|");
			}
			stringBuilder.AppendFormat("{0,5:F2}|", MgStar);
			stringBuilder.AppendFormat("{0,5:F2}|", MvAsteroid);
			stringBuilder.AppendFormat("{0,7:F0}|", AsteroidNominalDiameter);
			stringBuilder.AppendFormat("{0,4:F0}|", DiameterUncertainty);
			stringBuilder.AppendFormat("{0,9:F1}|", X);
			stringBuilder.AppendFormat("{0,9:F1}|", Y);
			if (UsedAssumedDiameter)
			{
				stringBuilder.Append("1|");
			}
			else
			{
				stringBuilder.Append("0|");
			}
			if (Solve_Circular | UsedAssumedDiameter)
			{
				stringBuilder.Append("1|");
			}
			else
			{
				stringBuilder.Append("0|");
			}
			stringBuilder.AppendFormat("{0,9:F1}|", X_Dia);
			stringBuilder.AppendFormat("{0,9:F1}|", Y_Dia);
			stringBuilder.AppendFormat("{0,5:F1}|", PA_Ellipse);
			stringBuilder.AppendFormat("{0,1:F0}|", Quality);
			if (Sdev_Major_Set)
			{
				stringBuilder.AppendFormat("{0,6:F1}|", Sdev_Major);
			}
			else
			{
				stringBuilder.Append("-999.9|");
			}
			if (Sdev_Minor_Set)
			{
				stringBuilder.AppendFormat("{0,6:F1}|", Sdev_Minor);
			}
			else
			{
				stringBuilder.Append("-999.9|");
			}
			if (Sdev_PA_Ellipse_Set)
			{
				stringBuilder.AppendFormat("{0,6:F1}|", Sdev_PA_Ellipse);
			}
			else
			{
				stringBuilder.Append("-999.9|");
			}
			if (Sdev_X_Set)
			{
				stringBuilder.Append(string.Format("{0,5:F1}", Sdev_X).Substring(0, 5) + "|");
			}
			else
			{
				stringBuilder.Append("-99.9|");
			}
			if (Sdev_Y_Set)
			{
				stringBuilder.Append(string.Format("{0,5:F1}", Sdev_Y).Substring(0, 5) + "|");
			}
			else
			{
				stringBuilder.Append("-99.9|");
			}
			if ((Math.Abs(Sdev_X) > 999.0) | (Math.Abs(Sdev_Y) > 999.0))
			{
				MessageBox.Show("Review X/Y errors for\r\n" + AsteroidNumber.Trim() + " " + AsteroidID.Trim() + " " + FormattedDate, "Review", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
			}
			if (Kepler2ID > 0)
			{
				stringBuilder.Append(string.Format("{0,9:F0}|", Kepler2ID));
			}
			else
			{
				stringBuilder.Append("-99999999|");
			}
			stringBuilder.AppendFormat("{0,1}|", Convert.ToInt16(Inc_Miss));
			stringBuilder.AppendFormat("{0,1}|", Convert.ToInt16(Solve_Major));
			stringBuilder.AppendFormat("{0,1}|", Convert.ToInt16(Solve_Minor));
			stringBuilder.AppendFormat("{0,1}|", Convert.ToInt16(Solve_PA));
			stringBuilder.AppendFormat("{0,1}|", Convert.ToInt16(Solve_CompanionSep));
			stringBuilder.AppendFormat("{0,1}|", Convert.ToInt16(Solve_CompanionPA));
			double dRA_asec = 0.0;
			double dDec_asec = 0.0;
			double dRA_asec_Uncert = 0.0;
			double dDec_asec_Uncert = 0.0;
			if (RefHour_forAnalysis == 0.0)
			{
				stringBuilder.Append("-99.999999|-999999.9|-99999.9|-99.9999|-99.9999|-9.9999|-9.9999|-9.9999|-9.9999|");
			}
			else
			{
				Utilities.Relativistic_Differential_Correction(JD_EventDate, RA_Star_2000, Dec_Star_2000, 8.794143836182533 / Parallax, out var dRA_ComparedToStar, out var dDec_ComparedToStar);
				if (GaiaVersion > 1)
				{
					Gaia.Gaia_FrameRotationCorrections(GaiaVersion, JD_EventDate, RA_Star_2000, Dec_Star_2000, MgStar, out dRA_asec, out dDec_asec, out dRA_asec_Uncert, out dDec_asec_Uncert);
				}
				stringBuilder.AppendFormat("{0,10:F6}|", RefHour_forAnalysis);
				stringBuilder.AppendFormat("{0,9:F1}|", X_Geo_atEvent);
				stringBuilder.AppendFormat("{0,8:F1}|", Y_Geo_atEvent);
				stringBuilder.AppendFormat("{0,8:F4}|", dRACosDec_atEvent + dRA_asec);
				stringBuilder.AppendFormat("{0,8:F4}|", dDec_atEvent + dDec_asec);
				stringBuilder.AppendFormat("{0,7:F4}|", (0.0 - dRA_ComparedToStar) * (180.0 / Math.PI) * 3600.0 * Math.Cos(Dec_Star_2000));
				stringBuilder.AppendFormat("{0,7:F4}|", (0.0 - dDec_ComparedToStar) * (180.0 / Math.PI) * 3600.0);
				stringBuilder.AppendFormat("{0,7:F4}|", Sdev_dRACosDec_atEvent);
				stringBuilder.AppendFormat("{0,7:F4}|", Sdev_dDec_atEvent);
			}
			if (IsAsteroid)
			{
				if (RefHour_forAnalysis == 0.0)
				{
					stringBuilder.Append("-");
				}
				else if (AstrometryShapeModelCentered)
				{
					stringBuilder.Append("1");
				}
				else
				{
					stringBuilder.Append("0");
				}
			}
			if (IsAsteroid)
			{
				for (int i = 0; i < 10; i++)
				{
					if (i < ShapeData.Count)
					{
						stringBuilder.Append("|" + ShapeData[i].Source.Substring(0, 1));
						stringBuilder.Append("|" + ShapeData[i].ID.PadLeft(4));
						stringBuilder.AppendFormat("|{0,1:f0}", ShapeData[i].FitQuality);
						if (ShapeData[i].DiaMin == 0.0)
						{
							stringBuilder.Append("|-99.9");
						}
						else
						{
							stringBuilder.AppendFormat("|{0,5:f1}", ShapeData[i].DiaMin);
						}
						if (ShapeData[i].DiaMax == 0.0)
						{
							stringBuilder.Append("|-99.9");
						}
						else
						{
							stringBuilder.AppendFormat("|{0,5:f1}", ShapeData[i].DiaMax);
						}
					}
					else
					{
						stringBuilder.Append("|-|-999|-|-99.9|-99.9");
					}
				}
			}
			if (Number_Chords != 0)
			{
				if (IsAsteroid)
				{
					stringBuilder.AppendFormat("|{0,3:F0}", Number_Chords);
				}
				else
				{
					stringBuilder.AppendFormat("{0,3:F0}", Number_Chords);
				}
			}
			else if (IsAsteroid)
			{
				stringBuilder.Append("|-99");
			}
			else
			{
				stringBuilder.Append("-99");
			}
			if (IsAsteroid)
			{
				if (!AsteroidHasSatellite)
				{
					stringBuilder.Append("|" + "-".PadRight(18) + "|-9.9999|-99.9|-9.9999|-99.9|-999|-999|-99|-|");
				}
				else
				{
					for (int j = 0; j < 2; j++)
					{
						if (Satellites.Count < j + 1)
						{
							stringBuilder.Append("|" + "-".PadRight(18) + "|-9.9999|-99.9|-9.9999|-99.9|-999|-999|-99|-|");
							continue;
						}
						stringBuilder.Append("|" + Satellites[j].CompanionIAUname.PadRight(18).Substring(0, 18) + "|");
						stringBuilder.AppendFormat("{0,7:F4}|", Satellites[j].SatelliteSeparation / 1000.0);
						stringBuilder.AppendFormat("{0,5:F1}|", Satellites[j].SatellitePA_2000);
						stringBuilder.AppendFormat("{0,7:F4}|", Satellites[j].Sat_Sep_Uncertainty / 1000.0);
						stringBuilder.AppendFormat("{0,5:F1}|", Satellites[j].Sat_PA_Uncertainty);
						stringBuilder.AppendFormat("{0,4:F0}|", Satellites[j].MajorAxisSatellite);
						stringBuilder.AppendFormat("{0,4:F0}|", Satellites[j].MinorAxisSatellite);
						stringBuilder.AppendFormat("{0,3:F0}|", Satellites[j].PAAxisSatellite);
						stringBuilder.AppendFormat("{0,1:F0}|", Satellites[j].SatelliteQuality);
					}
				}
			}
			else
			{
				stringBuilder.Append("|");
			}
			for (int k = 0; k < 4; k++)
			{
				if (Doubles[k].Companion_Set)
				{
					stringBuilder.AppendFormat("{0,6:F1}|{1,6:F1}|", Doubles[k].Sep_Companion, Doubles[k].PA_Companion);
				}
				else
				{
					stringBuilder.Append("-999.9|-999.9|");
				}
				if (k == 0)
				{
					if (Sdev_Sep_Set)
					{
						stringBuilder.AppendFormat("{0,6:F1}|", Sdev_Sep);
					}
					else
					{
						stringBuilder.Append("-999.9|");
					}
					if (Sdev_PA_Star_Set)
					{
						stringBuilder.AppendFormat("{0,6:F1}|", Sdev_PA_Star);
					}
					else
					{
						stringBuilder.Append("-999.9|");
					}
				}
			}
			if (MarkFieldsWithPipes)
			{
				return stringBuilder.ToString();
			}
			return stringBuilder.ToString().Replace("|", " ");
		}

		public static void SortObservers(int Field)
		{
			ObserverData.SortField = Field;
			Observers.Sort();
		}

		public static void SequentiallyNumberObservations()
		{
			for (int i = 0; i < Observers.Count; i++)
			{
				Observers[i].SeqNumber = i + 1;
			}
		}

		public static bool DeleteObservationLine(int RecordNumber)
		{
			//IL_0105: Unknown result type (might be due to invalid IL or missing references)
			//IL_010b: Invalid comparison between Unknown and I4
			string text = "Are you sure you want to delete record # " + Observers[RecordNumber].SeqNumber + "\r\n\r\nfor : " + Observers[RecordNumber].Observer1;
			if (Observers[RecordNumber].Observer2.Length > 0)
			{
				text = text + " and " + Observers[RecordNumber].Observer2;
			}
			text = ((Observers[RecordNumber].NearTo.Trim().Length <= 0) ? (text + "\r\n\r\nlocated in: " + Observers[RecordNumber].StateCountry) : (text + "\r\n\r\nlocated near: " + Observers[RecordNumber].NearTo.Trim() + ", in " + Observers[RecordNumber].StateCountry));
			if ((int)MessageBox.Show(text, "Confirm deletion", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return false;
			}
			if ((RecordNumber >= 0) & (RecordNumber < Observers.Count))
			{
				Observers.Remove(Observers[RecordNumber]);
				return true;
			}
			return false;
		}

		public static int AddNewObservationLine()
		{
			ObserverData observerData = new ObserverData();
			observerData.SeqNumber = Observers.Count + 1;
			Observers.Add(observerData);
			return Observers.Count - 1;
		}

		internal static void PasteAnObservationLine()
		{
			string[] array = Clipboard.GetText().Split(new char[1] { '\n' });
			if (array.GetUpperBound(0) < 0 || array[0].Length < 10)
			{
				return;
			}
			for (int i = 0; i <= array.GetUpperBound(0); i++)
			{
				if (array[i].Trim().Length > 0)
				{
					ObserverData.ObserverLine = new ObserverData();
					ObserverData.ObserverLine.DecodeOldObserverLine(array[i]);
					ObserverData.ObserverLine.SeqNumber = Observers.Count + 1;
					Observers.Add(ObserverData.ObserverLine);
				}
			}
		}

		internal static void PasteAPredictionLine()
		{
			string[] array = Clipboard.GetText().Split(new char[1] { '^' });
			string[] array2 = array[0].Split(new char[1] { '\n' });
			if (array2.GetUpperBound(0) < 0 || array2[0].Length < 10)
			{
				return;
			}
			for (int i = 0; i <= array2.GetUpperBound(0); i++)
			{
				if (array2[i].Trim().Length > 0)
				{
					ObserverData.Prediction = new ObserverData();
					ObserverData.Prediction.DecodePredictionLine(array2[i]);
					if (array.Length > 1)
					{
						ObserverData.Prediction.FreeText = array[1];
					}
					ObserverData.Prediction.SeqNumber = Observers.Count + 1;
					Observers.Add(ObserverData.Prediction);
				}
			}
		}

		internal static void PasteAnUnseenPrimaryLine()
		{
			string[] array = Clipboard.GetText().Split(new char[1] { '\n' });
			if (array.GetUpperBound(0) < 0 || array[0].Length < 10)
			{
				return;
			}
			for (int i = 0; i <= array.GetUpperBound(0); i++)
			{
				if (array[i].Trim().Length > 0)
				{
					ObserverData.ObserverLine = new ObserverData();
					ObserverData.ObserverLine.DecodePredictionLine(array[i]);
					ObserverData.ObserverLine.Observer1 = "Unseen_Primary_Event";
					ObserverData.ObserverLine.Observer2 = "";
					ObserverData.ObserverLine.Event_D = "e";
					ObserverData.ObserverLine.Event_R = "f";
					ObserverData.ObserverLine.SeqNumber = Observers.Count + 1;
					Observers.Add(ObserverData.ObserverLine);
					ObserverData.Prediction = new ObserverData();
					ObserverData.Prediction.DecodePredictionLine(array[i]);
					ObserverData.Prediction.SeqNumber = Observers.Count + 1;
					Observers.Add(ObserverData.Prediction);
				}
			}
		}
	}
}
