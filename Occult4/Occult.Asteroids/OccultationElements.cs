using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Asteroids
{
	public class OccultationElements : IComparable
	{
		public static double SiteLongitudeTest = 0.0;

		public static double SiteLatitudeTest = 0.0;

		public static double MagDropTestValue = 0.0;

		public static double PathDistanceKMTest = 100.0;

		public static double PathDistanceArcSecTest = 0.5;

		public static double ProbabilityTest = 5.0;

		public static double MinimumDurationTest = 0.0;

		public static double MinimumDistanceTest = 1.0;

		public static double[] RegionLongitude = new double[4];

		public static double[] RegionLatitude = new double[4];

		public static int[] LocalHorizon = new int[24];

		public static double DiameterTest = 10.0;

		public static double MagnitudeTest = 25.0;

		public static double JDStartTest = 0.0;

		public static double JDEndTest = 0.0;

		public static double AltitudeTest = 10.0;

		public static double SolarElongationTest = 15.0;

		public static double ApertureTest = 20.0;

		public static double UncertaintyTest = 0.6;

		public static string AsteroidNumberTest = "";

		public static string AsteroidNameTest = "";

		public static string AsteroidClassTest = "";

		public static string StarIDTest = "";

		public static int PlanetTest = 5;

		public static bool TestSite = false;

		public static bool TestSiteDistanceKM = false;

		public static bool TestSiteDistanceArcSec = false;

		public static bool TestProbability = false;

		public static bool TestInRegion = false;

		public static bool TestMagDrop = false;

		public static bool TestStarMag = false;

		public static bool TestLocalAlt = false;

		public static bool TestLocalHorizon = false;

		public static bool TestMaxDuration;

		public static bool TestDiameter = false;

		public static bool TestShapeModel = false;

		public static bool TestAsteroidNumber = false;

		public static bool TestAsteroidName = false;

		public static bool TestAsteroidClass = false;

		public static bool TestStarID = false;

		public static bool TestBinaryAsteroid = false;

		public static bool TestUncertainty = false;

		public static bool TestPlanet = false;

		public static bool TestDateRange = false;

		public static bool TestSolarElongation = false;

		public static bool TestAperture = false;

		public static bool TestIntegratingCamera = true;

		public static bool TestRings = false;

		public static bool TestMinimumD = false;

		public static int SortField;

		public static bool SiteNeedsUpdating = true;

		public static bool RegionNeedsUpdating = true;

		public static bool UseCombinedMag = false;

		public static bool FilterOnArcSec = true;

		private int epochYear;

		private int epochMonth;

		private int eventYear;

		private int eventMonth;

		private int eventDay;

		private int doubleStarCode;

		private int planet;

		private int satellite;

		private int numAsteroidRings;

		private int numAsteroidMoons;

		private int recordNumber;

		private double eventJD;

		private double jD_EventClosest;

		private double predictionMJD;

		private double midTime;

		private double mv;

		private double mb;

		private double mr;

		private double rAStar2000;

		private double decStar2000;

		private double rAStarApparent;

		private double decStarApparent;

		private double magAsteroid;

		private double magVAsteroid = -10.0;

		private double magRAsteroid = -10.0;

		private double combinedMagnitudeV;

		private double combinedMagnitudeR;

		private double asteroidDiameter;

		private double asteroidDiameterUncertainty;

		private double asteroidShadowDiameter_Penumbral;

		private double asteroidShadowDiameter_Umbral;

		private double asteroidKmSec;

		private double asteroidDiameterArcSec;

		private double maxDurn;

		private double distAsteroid;

		private double deltaRA;

		private double deltaDec;

		private double earthL_deg;

		private double decAsteroid_deg;

		private double substellarLongitude_OfDate_deg;

		private double substellarLatitude_OfDate_deg;

		private double xatMin;

		private double dx;

		private double d2x;

		private double d3x;

		private double yatMin;

		private double dy;

		private double d2y;

		private double d3y;

		private double sunL_deg;

		private double sunB_deg;

		private double equinox;

		private double epochDay;

		private double meanAnomaly;

		private double perihelion;

		private double node;

		private double I;

		private double E;

		private double A;

		private double Q;

		private double magConst;

		private double magCoeff_R;

		private double magCoeff_G;

		private double deltaMeanAnomaly;

		private double deltaPerihelion;

		private double deltaNode;

		private double deltaI;

		private double deltaE;

		private double deltaA;

		private double deltaQ;

		private double errorEllipseMajor;

		private double errorEllipseMinor;

		private double errorEllipsePA;

		private double knownSigma_StarAsterPosns;

		private double error_AsIncreaseInPathWidths;

		private double starDiamMAS;

		private double starReliability = -1.0;

		private double probability;

		private double pABrightLimb;

		private double planetPhaseAngle;

		private double planetIlluminationPercent;

		private double planetocentricEarthDec;

		private double pAPlanetPole;

		private double[] diameter_AsterMoon = new double[5];

		private double[] distKM_AsterMoon = new double[5];

		private double[] pA_AsterMoon = new double[5];

		private int nonGaiaPM = -1;

		private int duplicateSource = -1;

		private int gaiaUCAC4PM = -1;

		private bool planetMoonInPlanetShadow;

		private List<BinaryAsteroidElements> asteroidMoons = new List<BinaryAsteroidElements>();

		private bool isKepler2Star;

		private bool isAsteroid;

		private bool isPlanet;

		private bool isPlanetaryMoon;

		private bool hasShapeModel;

		private bool usesApparentStar = true;

		private bool inRegion;

		private bool aboveHorizonInRegion;

		private bool forJWST;

		private string objectNumber = "";

		private string objectName = "";

		private string starCatName = "";

		private string starIdentifier = "";

		private string eventID;

		private string orbitSource = "";

		private string errorBasis = "";

		private string asteroidClass = "";

		private string uTDate = "";

		private int asteroidNumber;

		private string nearbyStars = "";

		private bool nearbyStars_MagAdjusted;

		private int nearbyStars_All;

		private int nearbyStars_Bright;

		private double solarElongation;

		private double tClosest;

		private double tClosestRegional;

		private double minD;

		private double minDArcSec;

		private double minDistKM;

		private double altitudeAtEvent;

		private double azimuthAtEvent;

		private double altitudeSunAtEvent;

		private double altitudeMoonAtEvent;

		private double starMoonElongation_deg;

		private double moonPhase_percent;

		private double magDropV;

		private double magDropR;

		private double magLimit;

		private string planet_Local_Event_Times = "";

		private const double Radian = 180.0 / Math.PI;

		private const string EOL = "\r\n";

		internal static readonly string[] Tags = new string[10] { "<Event>", "<Elements>", "<Earth>", "<Star>", "<Object>", "<Moons>", "<Orbit>", "<PlanetPhysical>", "<Errors>", "<ID>" };

		internal static readonly string[] EndTags = new string[10] { "</Event>", "</Elements>", "</Earth>", "</Star>", "</Object>", "</Moons>", "</Orbit>", "</PlanetPhysical>", "</Errors>", "</ID>" };

		internal const string InitialTag = "<Occultations>";

		internal const string FinalTag = "</Occultations>";

		internal const string MoonTag = "      <Moon>";

		internal const string EndMoonTag = "</Moon>";

		public double EventJD
		{
			get
			{
				return eventJD;
			}
			set
			{
				eventJD = value;
				uTDate = Utilities.Date_from_JD(eventJD, 0);
			}
		}

		public string UTDate => uTDate;

		public double JD_EventClosest => jD_EventClosest;

		public double PredictionMJD
		{
			get
			{
				return predictionMJD;
			}
			set
			{
				predictionMJD = value;
			}
		}

		public string ObjectName
		{
			get
			{
				return objectName;
			}
			set
			{
				objectName = value;
			}
		}

		public string ObjectNumber
		{
			get
			{
				return objectNumber;
			}
			set
			{
				objectNumber = value;
			}
		}

		public bool IsAsteroid
		{
			get
			{
				return isAsteroid;
			}
			set
			{
				isAsteroid = value;
			}
		}

		public bool IsPlanet
		{
			get
			{
				return isPlanet;
			}
			set
			{
				isPlanet = value;
			}
		}

		public bool IsPlanetaryMoon
		{
			get
			{
				return isPlanetaryMoon;
			}
			set
			{
				isPlanetaryMoon = value;
			}
		}

		public bool HasShapeModel
		{
			get
			{
				return hasShapeModel;
			}
			set
			{
				hasShapeModel = value;
			}
		}

		public string EventID
		{
			get
			{
				return eventID;
			}
			set
			{
				eventID = value;
			}
		}

		public string AsteroidClass
		{
			get
			{
				return asteroidClass;
			}
			set
			{
				asteroidClass = value;
			}
		}

		public double MidTime_Hrs
		{
			get
			{
				return midTime;
			}
			set
			{
				midTime = value;
			}
		}

		public int EventYear
		{
			get
			{
				return eventYear;
			}
			set
			{
				eventYear = value;
			}
		}

		public int EventMonth
		{
			get
			{
				return eventMonth;
			}
			set
			{
				eventMonth = value;
			}
		}

		public int EventDay
		{
			get
			{
				return eventDay;
			}
			set
			{
				eventDay = value;
			}
		}

		public int EventHour => (int)Math.Floor(MidTime_Hrs);

		public double EventMin => 60.0 * (MidTime_Hrs - (double)EventHour);

		public double XatMin
		{
			get
			{
				return xatMin;
			}
			set
			{
				xatMin = value;
			}
		}

		public double dX
		{
			get
			{
				return dx;
			}
			set
			{
				dx = value;
			}
		}

		public double d2X
		{
			get
			{
				return d2x;
			}
			set
			{
				d2x = value;
			}
		}

		public double d3X
		{
			get
			{
				return d3x;
			}
			set
			{
				d3x = value;
			}
		}

		public double YatMin
		{
			get
			{
				return yatMin;
			}
			set
			{
				yatMin = value;
			}
		}

		public double dY
		{
			get
			{
				return dy;
			}
			set
			{
				dy = value;
			}
		}

		public double d2Y
		{
			get
			{
				return d2y;
			}
			set
			{
				d2y = value;
			}
		}

		public double d3Y
		{
			get
			{
				return d3y;
			}
			set
			{
				d3y = value;
			}
		}

		public double SubstellarLongitude_J2000_deg
		{
			get
			{
				return earthL_deg;
			}
			set
			{
				earthL_deg = value;
			}
		}

		public double SubstellarLatitude_J2000_deg
		{
			get
			{
				return decAsteroid_deg;
			}
			set
			{
				decAsteroid_deg = value;
			}
		}

		public double SubstellarLongitude_OfDate_deg
		{
			get
			{
				return substellarLongitude_OfDate_deg;
			}
			set
			{
				substellarLongitude_OfDate_deg = value;
			}
		}

		public double SubstellarLatitude_OfDate_deg
		{
			get
			{
				return substellarLatitude_OfDate_deg;
			}
			set
			{
				substellarLatitude_OfDate_deg = value;
			}
		}

		public double SubSolarLongitude_OfDate_deg
		{
			get
			{
				return sunL_deg;
			}
			set
			{
				sunL_deg = value;
			}
		}

		public double SubSolarLatitude_OfDate_deg
		{
			get
			{
				return sunB_deg;
			}
			set
			{
				sunB_deg = value;
			}
		}

		public bool ForJWST
		{
			get
			{
				return forJWST;
			}
			set
			{
				forJWST = value;
			}
		}

		public string ErrorBasis
		{
			get
			{
				return errorBasis;
			}
			set
			{
				errorBasis = value;
			}
		}

		public double ErrorEllipseMajorAxis
		{
			get
			{
				return errorEllipseMajor;
			}
			set
			{
				errorEllipseMajor = value;
			}
		}

		public double ErrorEllipseMinorAxis
		{
			get
			{
				return errorEllipseMinor;
			}
			set
			{
				errorEllipseMinor = value;
			}
		}

		public double ErrorEllipsePA
		{
			get
			{
				return errorEllipsePA;
			}
			set
			{
				errorEllipsePA = value;
			}
		}

		public double ErrorAcrossPath => DisplayMPOccultations.ErrorAcrossPath(dX, dY, errorEllipseMajor, errorEllipseMinor, errorEllipsePA);

		public double KnownSigma_StarAsterPosns
		{
			get
			{
				return knownSigma_StarAsterPosns;
			}
			set
			{
				knownSigma_StarAsterPosns = value;
			}
		}

		public int NoGaiaPM
		{
			get
			{
				return nonGaiaPM;
			}
			set
			{
				nonGaiaPM = value;
			}
		}

		public int GaiaUCAC4PM
		{
			get
			{
				return gaiaUCAC4PM;
			}
			set
			{
				gaiaUCAC4PM = value;
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

		public double Error_AsIncreaseInPathWidths
		{
			get
			{
				return error_AsIncreaseInPathWidths;
			}
			set
			{
				error_AsIncreaseInPathWidths = value;
			}
		}

		public double Probability
		{
			get
			{
				return probability;
			}
			set
			{
				probability = value;
			}
		}

		public double Equinox
		{
			get
			{
				return equinox;
			}
			set
			{
				equinox = value;
			}
		}

		public int EpochYear
		{
			get
			{
				return epochYear;
			}
			set
			{
				epochYear = value;
			}
		}

		public int EpochMonth
		{
			get
			{
				return epochMonth;
			}
			set
			{
				epochMonth = value;
			}
		}

		public double EpochDay
		{
			get
			{
				return epochDay;
			}
			set
			{
				epochDay = value;
			}
		}

		public double MeanAnomaly
		{
			get
			{
				return meanAnomaly;
			}
			set
			{
				meanAnomaly = value;
			}
		}

		public double Perihelion
		{
			get
			{
				return perihelion;
			}
			set
			{
				perihelion = value;
			}
		}

		public double Node
		{
			get
			{
				return node;
			}
			set
			{
				node = value;
			}
		}

		public double i
		{
			get
			{
				return I;
			}
			set
			{
				I = value;
			}
		}

		public double e
		{
			get
			{
				return E;
			}
			set
			{
				E = value;
			}
		}

		public double a
		{
			get
			{
				return A;
			}
			set
			{
				A = value;
			}
		}

		public double q
		{
			get
			{
				return Q;
			}
			set
			{
				Q = value;
			}
		}

		public double MagConst_H0
		{
			get
			{
				return magConst;
			}
			set
			{
				magConst = value;
			}
		}

		public double MagCoeff_logR
		{
			get
			{
				return magCoeff_R;
			}
			set
			{
				magCoeff_R = value;
			}
		}

		public double MagSlopeConst_G
		{
			get
			{
				return magCoeff_G;
			}
			set
			{
				magCoeff_G = value;
			}
		}

		public double delta_MeanAnomaly
		{
			get
			{
				return deltaMeanAnomaly;
			}
			set
			{
				deltaMeanAnomaly = value;
			}
		}

		public double delta_Perihelion
		{
			get
			{
				return deltaPerihelion;
			}
			set
			{
				deltaPerihelion = value;
			}
		}

		public double delta_Node
		{
			get
			{
				return deltaNode;
			}
			set
			{
				deltaNode = value;
			}
		}

		public double delta_i
		{
			get
			{
				return deltaI;
			}
			set
			{
				deltaI = value;
			}
		}

		public double delta_e
		{
			get
			{
				return deltaE;
			}
			set
			{
				deltaE = value;
			}
		}

		public double delta_a
		{
			get
			{
				return deltaA;
			}
			set
			{
				deltaA = value;
			}
		}

		public double delta_q
		{
			get
			{
				return deltaQ;
			}
			set
			{
				deltaQ = value;
			}
		}

		public string OrbitSource
		{
			get
			{
				return orbitSource;
			}
			set
			{
				orbitSource = value;
			}
		}

		public double hourly_dRA
		{
			get
			{
				return deltaRA;
			}
			set
			{
				deltaRA = value;
			}
		}

		public double hourly_dDec
		{
			get
			{
				return deltaDec;
			}
			set
			{
				deltaDec = value;
			}
		}

		public double DistAsteroid
		{
			get
			{
				return distAsteroid;
			}
			set
			{
				distAsteroid = value;
			}
		}

		public double MaxDurn
		{
			get
			{
				return maxDurn;
			}
			set
			{
				maxDurn = value;
			}
		}

		public double AsteroidKmSec => asteroidDiameter / maxDurn;

		public double MagAsteroid
		{
			get
			{
				return magAsteroid;
			}
			set
			{
				magAsteroid = value;
			}
		}

		public double MagVAsteroid
		{
			get
			{
				return magVAsteroid;
			}
			set
			{
				magVAsteroid = value;
			}
		}

		public double MagRAsteroid
		{
			get
			{
				return magRAsteroid;
			}
			set
			{
				magRAsteroid = value;
			}
		}

		public double CombinedMagnitudeV
		{
			get
			{
				return combinedMagnitudeV;
			}
			set
			{
				combinedMagnitudeV = value;
			}
		}

		public double CombinedMagnitudeR
		{
			get
			{
				return combinedMagnitudeR;
			}
			set
			{
				combinedMagnitudeR = value;
			}
		}

		public int AsteroidNumber
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

		public double AsteroidDiameter
		{
			get
			{
				return asteroidDiameter;
			}
			set
			{
				asteroidDiameter = value;
			}
		}

		public double AsteroidDiameterUncertainty
		{
			get
			{
				return asteroidDiameterUncertainty;
			}
			set
			{
				asteroidDiameterUncertainty = value;
			}
		}

		public double AsteroidShadowDiameter_Penumbral
		{
			get
			{
				return asteroidShadowDiameter_Penumbral;
			}
			set
			{
				asteroidShadowDiameter_Penumbral = value;
			}
		}

		public double AsteroidShadowDiameter_Umbral
		{
			get
			{
				return asteroidShadowDiameter_Umbral;
			}
			set
			{
				asteroidShadowDiameter_Umbral = value;
			}
		}

		public int NumAsteroidRings
		{
			get
			{
				return numAsteroidRings;
			}
			set
			{
				numAsteroidRings = value;
			}
		}

		public int NumAsteroidMoons
		{
			get
			{
				return numAsteroidMoons;
			}
			set
			{
				numAsteroidMoons = value;
			}
		}

		internal List<BinaryAsteroidElements> AsteroidMoons
		{
			get
			{
				return asteroidMoons;
			}
			set
			{
				asteroidMoons = value;
			}
		}

		public string StarCatName
		{
			get
			{
				return starCatName;
			}
			set
			{
				starCatName = value;
			}
		}

		public string StarIdentifier
		{
			get
			{
				string text = Utilities.StarIdentifier_ToMag6(RA_Star2000 * (180.0 / Math.PI), Dec_Star2000 * (180.0 / Math.PI), mV, WithConstellation: true);
				string text2 = StarNameFromHip;
				if (text2.Length > 0)
				{
					text2 += ", ";
				}
				if (text.Length > 1)
				{
					return " (" + text2 + text + ")";
				}
				return "";
			}
		}

		public string StarNameFromHip
		{
			get
			{
				if (!StarCatName.ToUpper().Contains("HIP"))
				{
					return "";
				}
				int.TryParse(StarCatName.Substring(3), out var result);
				if (result == 0)
				{
					return "";
				}
				return Utilities.StarNameFromHip(result, mV);
			}
		}

		public double RA_Star2000
		{
			get
			{
				return rAStar2000;
			}
			set
			{
				rAStar2000 = value;
			}
		}

		public double Dec_Star2000
		{
			get
			{
				return decStar2000;
			}
			set
			{
				decStar2000 = value;
			}
		}

		public bool UsesApparentStar
		{
			get
			{
				return usesApparentStar;
			}
			set
			{
				usesApparentStar = value;
			}
		}

		public double RA_Star_Apparent
		{
			get
			{
				return rAStarApparent;
			}
			set
			{
				rAStarApparent = value;
			}
		}

		public double Dec_Star_Apparent
		{
			get
			{
				return decStarApparent;
			}
			set
			{
				decStarApparent = value;
			}
		}

		public double mV
		{
			get
			{
				return mv;
			}
			set
			{
				mv = value;
			}
		}

		public double mB
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

		public double mR
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

		public double MagDropV
		{
			get
			{
				return magDropV;
			}
			set
			{
				magDropV = value;
			}
		}

		public double MagDropR
		{
			get
			{
				return magDropR;
			}
			set
			{
				magDropR = value;
			}
		}

		public string MagDopV_540
		{
			get
			{
				int num = (int)(magDropV * 10.0) + 48;
				if (num > 127)
				{
					num = 127;
				}
				return ((char)num).ToString();
			}
			set
			{
				string text = value.PadRight(1).Substring(0, 1);
				MagDropV = ((double)(int)text.ToCharArray()[0] - 48.0) / 10.0;
				if (MagDropV < 0.0)
				{
					MagDropV = 0.0;
				}
			}
		}

		public string MagDopR_540
		{
			get
			{
				if (MagDropR < 0.0)
				{
					return MagDopV_540;
				}
				int num = (int)(magDropR * 10.0) + 48;
				if (num > 127)
				{
					num = 127;
				}
				return ((char)num).ToString();
			}
			set
			{
				string text = value.PadRight(1).Substring(0, 1);
				MagDropR = ((double)(int)text.ToCharArray()[0] - 48.0) / 10.0;
				if (MagDropR < 0.0)
				{
					MagDropR = 0.0;
				}
			}
		}

		public double StarReliability
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

		public string RUWE_540
		{
			get
			{
				if (starReliability < 0.0)
				{
					return " ";
				}
				double num = starReliability * 20.0 + 48.0;
				if (num > 127.0)
				{
					num = 127.0;
				}
				return ((char)num).ToString();
			}
			set
			{
				if (value == " ")
				{
					StarReliability = -1.0;
					return;
				}
				string text = value.PadRight(1).Substring(0, 1);
				StarReliability = ((double)(int)text.ToCharArray()[0] - 48.0) / 20.0;
			}
		}

		public double StarDiamMAS
		{
			get
			{
				return starDiamMAS;
			}
			set
			{
				starDiamMAS = value;
			}
		}

		public bool IsKepler2Star
		{
			get
			{
				return isKepler2Star;
			}
			set
			{
				isKepler2Star = value;
			}
		}

		public int DoubleStarCode
		{
			get
			{
				return doubleStarCode;
			}
			set
			{
				doubleStarCode = value;
			}
		}

		public bool NearbyStars_MagAdjusted
		{
			get
			{
				return nearbyStars_MagAdjusted;
			}
			set
			{
				nearbyStars_MagAdjusted = value;
			}
		}

		public int NearbyStars_Bright
		{
			get
			{
				return nearbyStars_Bright;
			}
			set
			{
				nearbyStars_Bright = value;
			}
		}

		public int NearbyStars_All
		{
			get
			{
				return nearbyStars_All;
			}
			set
			{
				nearbyStars_All = value;
			}
		}

		public string NearbyStars540
		{
			get
			{
				int num = 0;
				if (NearbyStars_MagAdjusted)
				{
					num = 1;
				}
				if (NearbyStars_All >= 0)
				{
					num = ((NearbyStars_All >= 8) ? (num + 56) : (num + 8 * NearbyStars_All));
					num = ((NearbyStars_Bright >= 4) ? (num + 6) : (num + 2 * NearbyStars_Bright));
				}
				num += 48;
				return ((char)num).ToString();
			}
			set
			{
				int num = value.PadRight(1).Substring(0, 1).ToCharArray()[0] - 48;
				if (num >= 0)
				{
					NearbyStars_MagAdjusted = num % 2 == 1;
					NearbyStars_Bright = num / 2 % 4;
					NearbyStars_All = num / 8 % 8;
				}
				else
				{
					NearbyStars_MagAdjusted = false;
					NearbyStars_Bright = 0;
					NearbyStars_All = 0;
				}
			}
		}

		public int Planet
		{
			get
			{
				return planet;
			}
			set
			{
				planet = value;
			}
		}

		public int PlanetaryMoon
		{
			get
			{
				return satellite;
			}
			set
			{
				satellite = value;
			}
		}

		public double PABrightLimb
		{
			get
			{
				return pABrightLimb;
			}
			set
			{
				pABrightLimb = value;
			}
		}

		public double PlanetPhaseAngle
		{
			get
			{
				return planetPhaseAngle;
			}
			set
			{
				planetPhaseAngle = value;
				planetIlluminationPercent = 50.0 * (1.0 + Math.Cos(value / (180.0 / Math.PI)));
			}
		}

		public double PlanetIlluminationPercent
		{
			get
			{
				return planetIlluminationPercent;
			}
			set
			{
				planetIlluminationPercent = value;
			}
		}

		public double PlanetocentricEarthDec
		{
			get
			{
				return planetocentricEarthDec;
			}
			set
			{
				planetocentricEarthDec = value;
			}
		}

		public double PAPlanetPole
		{
			get
			{
				return pAPlanetPole;
			}
			set
			{
				pAPlanetPole = value;
			}
		}

		public bool PlanetMoonInPlanetShadow
		{
			get
			{
				return planetMoonInPlanetShadow;
			}
			set
			{
				planetMoonInPlanetShadow = value;
			}
		}

		public double MagLimit => magLimit;

		public double SolarElongation
		{
			get
			{
				return solarElongation;
			}
			set
			{
				solarElongation = value;
			}
		}

		public double AsteroidDiameterArcSec
		{
			get
			{
				return asteroidDiameterArcSec;
			}
			set
			{
				asteroidDiameterArcSec = value;
			}
		}

		public string Planet_Local_Event_Times
		{
			get
			{
				return planet_Local_Event_Times;
			}
			set
			{
				planet_Local_Event_Times = value;
			}
		}

		public double n => Math.Sqrt(dX * dX + dY * dY);

		public double MinGeocentricD => Math.Abs((XatMin * dY - YatMin * dX) / n);

		public double VerticalPathScale
		{
			get
			{
				if (MinGeocentricD < 0.9)
				{
					return Math.Sqrt(1.0 - MinGeocentricD * MinGeocentricD) / 6378.137;
				}
				return 6.834111590892451E-05;
			}
		}

		public double VerticalScaleFactor
		{
			get
			{
				if (MinGeocentricD < 0.9988)
				{
					return Math.Sqrt(1.0 - MinGeocentricD * MinGeocentricD);
				}
				return 0.05;
			}
		}

		public double TClosest => tClosest;

		public double TClosestRegional => tClosestRegional;

		public double MinD => minD;

		public double MinDArcSec => minDArcSec;

		public double MinDistKM => minDistKM;

		public double AltitudeAtEvent => altitudeAtEvent;

		public double AzimuthAtEvent => azimuthAtEvent;

		public double AltitudeSunAtEvent => altitudeSunAtEvent;

		public double AltitudeMoonAtEvent => altitudeMoonAtEvent;

		public double StarMoonElongation_deg
		{
			get
			{
				return starMoonElongation_deg;
			}
			set
			{
				starMoonElongation_deg = value;
			}
		}

		public double MoonPhase_percent
		{
			get
			{
				return moonPhase_percent;
			}
			set
			{
				moonPhase_percent = value;
			}
		}

		public int RecordNumber
		{
			get
			{
				return recordNumber;
			}
			set
			{
				recordNumber = value;
			}
		}

		public bool IsEventForOutput
		{
			get
			{
				if (!ForJWST)
				{
					if (TestSite)
					{
						if (mV > 4.0)
						{
							if (AltitudeSunAtEvent > -1.0)
							{
								return false;
							}
							if ((AltitudeSunAtEvent > -9.0) & (AltitudeAtEvent < 5.0))
							{
								return false;
							}
							if (AltitudeSunAtEvent > -10.0 && mV > 15.0 - 75.0 * (Math.Sin(AltitudeSunAtEvent / (180.0 / Math.PI)) + 0.17))
							{
								return false;
							}
						}
						if (TestSiteDistanceArcSec && MinDArcSec > PathDistanceArcSecTest + AsteroidDiameterArcSec / 2.0)
						{
							return false;
						}
						if (TestSiteDistanceKM && MinDistKM > PathDistanceKMTest + AsteroidDiameter / 2.0)
						{
							return false;
						}
						if (AltitudeAtEvent < 0.0)
						{
							return false;
						}
						if (TestLocalAlt && AltitudeAtEvent < AltitudeTest)
						{
							return false;
						}
						if (TestLocalHorizon)
						{
							int num = (int)(AzimuthAtEvent / 15.0);
							if (num > 23)
							{
								num = 23;
							}
							if (num < 0)
							{
								num = 0;
							}
							if (AltitudeAtEvent < (double)LocalHorizon[num])
							{
								return false;
							}
						}
					}
					if (TestInRegion && (!inRegion | !aboveHorizonInRegion))
					{
						return false;
					}
				}
				if (TestMagDrop && MagDropV < MagDropTestValue)
				{
					return false;
				}
				if (TestStarMag)
				{
					if (UseCombinedMag)
					{
						if (CombinedMagnitudeV > MagnitudeTest)
						{
							return false;
						}
					}
					else if (mV > MagnitudeTest)
					{
						return false;
					}
				}
				if (TestDiameter && AsteroidShadowDiameter_Penumbral < DiameterTest)
				{
					return false;
				}
				if (TestMaxDuration && MaxDurn < MinimumDurationTest)
				{
					return false;
				}
				if (TestProbability && ProbabilityTest >= Probability + 0.01)
				{
					return false;
				}
				if (TestAperture)
				{
					double num2 = 0.0367;
					double num3 = 3.0;
					double num4 = 5.5 + 5.0 * Math.Log10(ApertureTest);
					if (!TestIntegratingCamera)
					{
						if (mV > num4)
						{
							return false;
						}
						if (MaxDurn < num2 * num3)
						{
							return false;
						}
					}
					else
					{
						double num5 = ((!(mV > num4)) ? 1.0 : Math.Pow(2.512, mV - num4));
						if (MagDropV < 0.5)
						{
							num3 = 0.75 / MagDropV / MagDropV;
						}
						if (MaxDurn < num3 * num5 * num2)
						{
							return false;
						}
					}
				}
				if (TestSolarElongation && solarElongation < SolarElongationTest)
				{
					return false;
				}
				if (TestUncertainty && ErrorEllipseMajorAxis / 8.794143836182533 * DistAsteroid > UncertaintyTest)
				{
					return false;
				}
				if (TestDateRange && ((EventJD < JDStartTest) | (EventJD > JDEndTest)))
				{
					return false;
				}
				if (TestPlanet && PlanetTest != Planet)
				{
					return false;
				}
				if (TestBinaryAsteroid && ((NumAsteroidMoons == 0) & (asteroidClass != "Binary")))
				{
					return false;
				}
				if (TestShapeModel && !HasShapeModel)
				{
					return false;
				}
				if (TestRings && NumAsteroidRings < 1)
				{
					return false;
				}
				if (TestAsteroidName && !ObjectName.ToUpper().Contains(AsteroidNameTest))
				{
					return false;
				}
				if (TestAsteroidNumber && ObjectNumber != AsteroidNumberTest)
				{
					return false;
				}
				if (TestAsteroidClass && AsteroidClass != AsteroidClassTest)
				{
					return false;
				}
				if (TestStarID && StarCatName != StarIDTest)
				{
					return false;
				}
				if (TestMinimumD && Math.Sqrt(XatMin * XatMin + YatMin * YatMin) > MinimumDistanceTest)
				{
					return false;
				}
				return true;
			}
		}

		public string Summary_Line
		{
			get
			{
				AsteroidSummaryLine asteroidSummaryLine = new AsteroidSummaryLine();
				asteroidSummaryLine.IsAsteroid = IsAsteroid;
				asteroidSummaryLine.IsPlanetaryMoon = IsPlanetaryMoon;
				asteroidSummaryLine.IsPlanet = IsPlanet;
				asteroidSummaryLine.PlanetTimes = Planet_Local_Event_Times.Length > 0;
				asteroidSummaryLine.PlanetEventTimes = Planet_Local_Event_Times;
				asteroidSummaryLine.Global = !TestSite;
				double num = MidTime_Hrs;
				if (TestSite)
				{
					num += TClosest;
				}
				else if (TestInRegion)
				{
					num += TClosestRegional;
				}
				double jD = (asteroidSummaryLine.JDEventDate = EventJD + num / 24.0);
				asteroidSummaryLine.UTDate = Utilities.Date_from_JD(jD, 0);
				Utilities.Date_from_JD(jD, out var _, out var _, out var day);
				double num3 = (day - Math.Floor(day)) * 24.0;
				double eventMin = (num3 - Math.Floor(num3)) * 60.0;
				asteroidSummaryLine.EventHour = Math.Floor(num3);
				asteroidSummaryLine.EventMin = eventMin;
				asteroidSummaryLine.ForJWST = forJWST;
				asteroidSummaryLine.AsteroidDiameter = asteroidDiameter;
				asteroidSummaryLine.AsteroidRate = asteroidDiameter / MaxDurn;
				asteroidSummaryLine.AsteroidDiameterArcSec = AsteroidDiameterArcSec;
				asteroidSummaryLine.ErrorEarthRadii = ErrorEllipseMajorAxis / 8.794143836182533 * DistAsteroid;
				asteroidSummaryLine.MinEarthSep = Math.Sqrt(XatMin * XatMin + YatMin * YatMin);
				asteroidSummaryLine.Mv = mV;
				asteroidSummaryLine.MagCombined = Utilities.CombinedMagnitude(mV, magAsteroid);
				asteroidSummaryLine.NearbyStars = NearbyStars_All;
				asteroidSummaryLine.MagAdjusted = NearbyStars_MagAdjusted;
				asteroidSummaryLine.SunElong = SolarElongation;
				asteroidSummaryLine.MoonElongation_deg = StarMoonElongation_deg;
				asteroidSummaryLine.MoonIllumination_percent = MoonPhase_percent;
				asteroidSummaryLine.MaxDurn = MaxDurn;
				asteroidSummaryLine.Mdrop = MagDropV;
				asteroidSummaryLine.MdropRed = MagDropR;
				asteroidSummaryLine.PlanetIllum = PlanetIlluminationPercent;
				asteroidSummaryLine.StarNo = StarCatName;
				asteroidSummaryLine.DoubleStarFlag = DoubleStarCode;
				if (IsKepler2Star)
				{
					asteroidSummaryLine.DoubleStarFlag = 16;
				}
				if (StarDiamMAS >= 0.1)
				{
					asteroidSummaryLine.DoubleStarFlag = DoubleStarCode + 17;
				}
				asteroidSummaryLine.RUWE = StarReliability;
				asteroidSummaryLine.AsteroidNumber = ObjectNumber;
				asteroidSummaryLine.AsteroidName = ObjectName;
				asteroidSummaryLine.Rings = NumAsteroidRings;
				asteroidSummaryLine.Moons = NumAsteroidMoons;
				asteroidSummaryLine.AsteroidClass = AsteroidClass;
				asteroidSummaryLine.HasShapeModel = HasShapeModel;
				asteroidSummaryLine.RADec = "  " + Utilities.DEGtoDMS(RA_Star2000 * (180.0 / Math.PI) / 15.0, 2, 3, MinutesOnly: false) + " " + Utilities.DEGtoDMS(Dec_Star2000 * (180.0 / Math.PI), 3, 2, MinutesOnly: false);
				asteroidSummaryLine.Probability = DisplayMPOccultations.Probability(AsteroidDiameter / 2.0 / DistAsteroid / 6378.137 * 8.794143836182533, ErrorAcrossPath, MinDArcSec);
				if (TestSite)
				{
					asteroidSummaryLine.Alt = AltitudeAtEvent;
					asteroidSummaryLine.Azimuth = AzimuthAtEvent;
					if (TestSiteDistanceKM)
					{
						asteroidSummaryLine.MinD = -1.0;
					}
					else
					{
						asteroidSummaryLine.MinD = MinDArcSec;
					}
					asteroidSummaryLine.Dkm = MinDistKM;
					asteroidSummaryLine.SunAlt = AltitudeSunAtEvent;
					asteroidSummaryLine.MoonAlt = AltitudeMoonAtEvent;
				}
				else if (!IsAsteroid)
				{
					double num4 = SubstellarLongitude_OfDate_deg - SubSolarLongitude_OfDate_deg;
					if (num4 < -180.0)
					{
						num4 += 360.0;
					}
					if (num4 > 180.0)
					{
						num4 -= 360.0;
					}
					if (num4 > 0.0)
					{
						asteroidSummaryLine.SunL90 = Utilities.NormaliseDegrees(SubSolarLongitude_OfDate_deg + 90.0);
						asteroidSummaryLine.L90 = Utilities.NormaliseDegrees(SubstellarLongitude_OfDate_deg + 90.0);
					}
					else
					{
						asteroidSummaryLine.SunL90 = Utilities.NormaliseDegrees(SubstellarLongitude_OfDate_deg + 90.0);
						asteroidSummaryLine.L90 = Utilities.NormaliseDegrees(SubSolarLongitude_OfDate_deg + 90.0);
					}
				}
				asteroidSummaryLine.RecordNumber = RecordNumber;
				return asteroidSummaryLine.ToString();
			}
		}

		public string ElementsIn540Format
		{
			get
			{
				StringBuilder stringBuilder = new StringBuilder();
				stringBuilder.Append(" " + Utilities.Date_from_JD(eventJD, 0));
				StringBuilder stringBuilder2 = new StringBuilder();
				stringBuilder2.Append(ObjectNumber.PadLeft(6).Substring(0, 6));
				stringBuilder2.Append(" ");
				stringBuilder2.Append(ObjectName.PadRight(15).Substring(0, 15));
				StringBuilder stringBuilder3 = new StringBuilder();
				int num = (int)MidTime_Hrs;
				double num2 = 60.0 * (MidTime_Hrs - (double)num);
				stringBuilder3.AppendFormat("{0,2}h", num);
				stringBuilder3.AppendFormat("{0,5:F1}m UT", num2);
				string text = Utilities.DEGtoDMS(RA_Star2000 * (180.0 / Math.PI) / 15.0, 3, 3, MinutesOnly: false);
				string text2 = Utilities.DEGtoDMS(Dec_Star2000 * (180.0 / Math.PI), 3, 2, MinutesOnly: false);
				double num3 = (RA_Star2000 * (180.0 / Math.PI) / 15.0 - Utilities.DMStoDeg(" " + text)) * 36000000.0;
				double num4 = (Dec_Star2000 * (180.0 / Math.PI) - Utilities.DMStoDeg(" " + text2)) * 3600000.0;
				if (Math.Abs(num3) > 9.49)
				{
					num3 = 0.0;
				}
				if (Math.Abs(num4) > 9.49)
				{
					num4 = 0.0;
				}
				StringBuilder stringBuilder4 = new StringBuilder();
				stringBuilder4.AppendFormat("{0,6:F1}", mV);
				stringBuilder4.Append("  " + text);
				stringBuilder4.Append(text2);
				stringBuilder4.Append(StarCatName.PadRight(20));
				stringBuilder4.AppendFormat("{0,6:F1}", mR);
				StringBuilder stringBuilder5 = new StringBuilder();
				stringBuilder5.AppendFormat("{0,4:F1}", MagAsteroid);
				stringBuilder5.AppendFormat("{0,7:F0}", AsteroidShadowDiameter_Penumbral);
				if (MaxDurn < 10.0)
				{
					stringBuilder5.AppendFormat("{0,7:F2}", MaxDurn);
				}
				else
				{
					stringBuilder5.AppendFormat("{0,7:F1}", MaxDurn);
				}
				stringBuilder5.AppendFormat("{0,8:F3}", DistAsteroid);
				stringBuilder5.AppendFormat("{0,10:F3}", hourly_dRA);
				stringBuilder5.AppendFormat("{0,10:F2}", hourly_dDec);
				StringBuilder stringBuilder6 = new StringBuilder();
				stringBuilder6.AppendFormat("{0,9:F4}", SubstellarLongitude_J2000_deg);
				stringBuilder6.AppendFormat("{0,8:F3}", SubstellarLatitude_J2000_deg);
				stringBuilder6.AppendFormat("{0,9:F2}", SubSolarLongitude_OfDate_deg);
				stringBuilder6.AppendFormat("{0,8:F2}", SubSolarLatitude_OfDate_deg);
				StringBuilder stringBuilder7 = new StringBuilder();
				stringBuilder7.AppendFormat("{0,8:F4}", MidTime_Hrs);
				stringBuilder7.Append(string.Format("{0,9:F5}", XatMin).Substring(0, 9));
				stringBuilder7.Append(string.Format("{0,9:F5}", YatMin).Substring(0, 9));
				stringBuilder7.Append(string.Format("{0,9:F5}", dX).Substring(0, 9));
				stringBuilder7.Append(string.Format("{0,9:F5}", dY).Substring(0, 9));
				stringBuilder7.AppendFormat("{0,9:F5}", d2X);
				stringBuilder7.AppendFormat("{0,9:F5}", d2Y);
				StringBuilder stringBuilder8 = new StringBuilder();
				if (!isAsteroid)
				{
					stringBuilder8.AppendFormat("P{0,1}", Planet);
					if (PlanetaryMoon == 0)
					{
						stringBuilder8.AppendFormat("{0,7:F1}", PABrightLimb);
						stringBuilder8.AppendFormat("{0,7:F1}", PlanetPhaseAngle);
						stringBuilder8.AppendFormat("{0,7:F1}", PlanetocentricEarthDec);
						stringBuilder8.AppendFormat("{0,7:F1}", PAPlanetPole);
						stringBuilder8.Append("".PadRight(74));
					}
					else
					{
						stringBuilder8.Append("M" + PlanetaryMoon.ToString().PadLeft(2, '0'));
						stringBuilder8.Append("".PadRight(99));
					}
				}
				else
				{
					stringBuilder8.Append("2000    ");
					stringBuilder8.AppendFormat("{0,10:F6}", MeanAnomaly * (180.0 / Math.PI));
					stringBuilder8.AppendFormat("{0,4:f0}{1,2:f0}{2,8:f5}", EpochYear, EpochMonth, EpochDay);
					stringBuilder8.AppendFormat("{0,10:F6}", Perihelion * (180.0 / Math.PI));
					stringBuilder8.AppendFormat("{0,10:F6}", Node * (180.0 / Math.PI));
					stringBuilder8.AppendFormat("{0,10:F6}", i * (180.0 / Math.PI));
					stringBuilder8.AppendFormat("{0,10:F8}", e);
					stringBuilder8.Append(string.Format("{0,10:F7}", a).Substring(0, 10));
					stringBuilder8.Append(string.Format("{0,10:F7}", q).Substring(0, 10));
					stringBuilder8.AppendFormat("{0,4:F1}", MagConst_H0);
					stringBuilder8.AppendFormat("{0,4:F1}", MagCoeff_logR);
					if (MagSlopeConst_G >= 10.0)
					{
						stringBuilder8.AppendFormat("{0,4:F1}", MagSlopeConst_G);
					}
					else if (MagSlopeConst_G >= 0.0)
					{
						stringBuilder8.AppendFormat("{0,4:F2}", MagSlopeConst_G);
					}
					else
					{
						stringBuilder8.Append(MagSlopeConst_G.ToString(".00"));
					}
				}
				StringBuilder stringBuilder9 = new StringBuilder();
				if (!isAsteroid)
				{
					stringBuilder9.Append("".PadRight(71));
				}
				else
				{
					stringBuilder9.Append("RATE");
					stringBuilder9.Append(string.Format("{0,10:F7}", delta_MeanAnomaly * (180.0 / Math.PI) * 10.0).Substring(0, 10));
					stringBuilder9.Append(string.Format("{0,9:F6}", delta_Perihelion * (180.0 / Math.PI) * 10.0).Substring(0, 9));
					stringBuilder9.AppendFormat("{0,9:F6}", delta_Node * (180.0 / Math.PI) * 10.0);
					stringBuilder9.AppendFormat("{0,9:F6}", delta_i * (180.0 / Math.PI) * 10.0);
					stringBuilder9.AppendFormat("{0,10:F7}", delta_e * 10.0);
					stringBuilder9.AppendFormat("{0,10:F7}", delta_a * 10.0);
					stringBuilder9.AppendFormat("{0,10:F7}", delta_q * 10.0);
				}
				StringBuilder stringBuilder10 = new StringBuilder();
				if (KnownSigma_StarAsterPosns > 0.0)
				{
					stringBuilder10.Append("Meas ");
				}
				else
				{
					stringBuilder10.Append("Assum");
				}
				stringBuilder10.Append(string.Format("{0,6:F3}", ErrorEllipseMajorAxis).Substring(0, 6));
				stringBuilder10.Append(string.Format("{0,6:F3}", ErrorEllipseMinorAxis).Substring(0, 6));
				stringBuilder10.Append(string.Format("{0,6:F0}", ErrorEllipsePA).Substring(0, 6));
				stringBuilder10.Append(string.Format("{0,6:F3}", Error_AsIncreaseInPathWidths).Substring(0, 6));
				stringBuilder10.Append(string.Format("{0,6:F3}", KnownSigma_StarAsterPosns).Substring(0, 6));
				stringBuilder10.AppendFormat("{0,1:F0}", NumAsteroidRings);
				stringBuilder10.AppendFormat("{0,1:F0}", NumAsteroidMoons);
				if (AsteroidMoons.Count > 0)
				{
					stringBuilder10.AppendFormat("{0,4:F0}{1,6:F0}    ", AsteroidMoons[0].Diameter, AsteroidMoons[0].A);
				}
				else
				{
					stringBuilder10.Append("".PadRight(14));
				}
				if (AsteroidMoons.Count > 1)
				{
					stringBuilder10.AppendFormat("{0,4:F0}{1,6:F0} ", AsteroidMoons[1].Diameter, AsteroidMoons[1].A);
				}
				else
				{
					stringBuilder10.Append("".PadRight(11));
				}
				stringBuilder10.Append(RUWE_540);
				stringBuilder10.Append(MagDopV_540);
				stringBuilder10.Append(MagDopR_540);
				stringBuilder10.Append(NearbyStars540.PadRight(1).Substring(0, 1));
				if (IsKepler2Star)
				{
					stringBuilder10.Append("K");
				}
				else
				{
					stringBuilder10.Append(" ");
				}
				stringBuilder10.Append(StarDiamMAS.ToString().PadLeft(2).Substring(0, 2));
				stringBuilder10.AppendFormat("{0,2:F0}", num3);
				stringBuilder10.AppendFormat("{0,2:F0}", num4);
				if (StarCatName.Contains("noPM"))
				{
					stringBuilder10.Append("8");
				}
				else if (StarCatName.Substring(0, 1) == "G")
				{
					if (StarCatName.Substring(7, 1) == ".")
					{
						stringBuilder10.Append("8");
					}
					else
					{
						stringBuilder10.Append(" ");
					}
				}
				else
				{
					stringBuilder10.Append(" ");
				}
				stringBuilder10.Append(DoubleStarCode.ToString().PadRight(1).Substring(0, 1));
				stringBuilder10.AppendFormat("{0,8:F2}", Utilities.MJD_now());
				stringBuilder10.Append(EventID.PadLeft(19).Substring(4));
				return stringBuilder.ToString() + "\r\n" + stringBuilder2.ToString() + "\r\n" + stringBuilder3.ToString() + "\r\n" + stringBuilder4.ToString() + "\r\n" + stringBuilder5.ToString() + "\r\n" + stringBuilder6.ToString() + "\r\n" + stringBuilder7.ToString() + "\r\n" + stringBuilder8.ToString() + "\r\n" + stringBuilder9.ToString() + "\r\n" + stringBuilder10.ToString() + "\r\n";
			}
		}

		public int CompareTo(object other)
		{
			switch (SortField)
			{
			case 0:
				if (EventJD == ((OccultationElements)other).EventJD)
				{
					return MidTime_Hrs.CompareTo(((OccultationElements)other).MidTime_Hrs);
				}
				return EventJD.CompareTo(((OccultationElements)other).EventJD);
			case 1:
				if (asteroidDiameter == ((OccultationElements)other).asteroidDiameter)
				{
					return EventJD.CompareTo(((OccultationElements)other).EventJD);
				}
				return ((OccultationElements)other).asteroidDiameter.CompareTo(asteroidDiameter);
			case 2:
				if (asteroidDiameterArcSec == ((OccultationElements)other).asteroidDiameterArcSec)
				{
					return EventJD.CompareTo(((OccultationElements)other).EventJD);
				}
				return ((OccultationElements)other).asteroidDiameterArcSec.CompareTo(asteroidDiameterArcSec);
			case 3:
				if (maxDurn == ((OccultationElements)other).maxDurn)
				{
					return EventJD.CompareTo(((OccultationElements)other).EventJD);
				}
				return ((OccultationElements)other).maxDurn.CompareTo(maxDurn);
			case 4:
				if (mv == ((OccultationElements)other).mv)
				{
					return EventJD.CompareTo(((OccultationElements)other).EventJD);
				}
				return mv.CompareTo(((OccultationElements)other).mv);
			case 5:
				if (magDropV == ((OccultationElements)other).magDropV)
				{
					return EventJD.CompareTo(((OccultationElements)other).EventJD);
				}
				return ((OccultationElements)other).magDropV.CompareTo(magDropV);
			case 6:
				if (SolarElongation == ((OccultationElements)other).SolarElongation)
				{
					return EventJD.CompareTo(((OccultationElements)other).EventJD);
				}
				return ((OccultationElements)other).SolarElongation.CompareTo(SolarElongation);
			case 7:
			{
				int num = string.Compare(StarCatName, ((OccultationElements)other).StarCatName);
				if (num == 0)
				{
					return EventJD.CompareTo(((OccultationElements)other).EventJD);
				}
				return num;
			}
			case 8:
			{
				int.TryParse(ObjectNumber, out var result);
				int.TryParse(((OccultationElements)other).ObjectNumber, out var result2);
				if (result > 0 && result2 > 0)
				{
					if (result == result2)
					{
						return EventJD.CompareTo(((OccultationElements)other).EventJD);
					}
					return result.CompareTo(result2);
				}
				if (result > 0 && result2 == 0)
				{
					return -1;
				}
				if (result == 0 && result2 > 0)
				{
					return 1;
				}
				if (ObjectNumber == ((OccultationElements)other).ObjectNumber)
				{
					int num = string.Compare(ObjectName, ((OccultationElements)other).ObjectName);
					if (num == 0)
					{
						return EventJD.CompareTo(((OccultationElements)other).EventJD);
					}
					return num;
				}
				return ObjectNumber.CompareTo(((OccultationElements)other).ObjectNumber);
			}
			case 9:
			{
				int num = string.Compare(ObjectName, ((OccultationElements)other).ObjectName);
				if (num == 0)
				{
					return EventJD.CompareTo(((OccultationElements)other).EventJD);
				}
				return num;
			}
			case 10:
				if (probability == ((OccultationElements)other).probability)
				{
					return EventJD.CompareTo(((OccultationElements)other).EventJD);
				}
				return ((OccultationElements)other).probability.CompareTo(probability);
			case 11:
				if (TestSiteDistanceKM)
				{
					if (minDistKM == ((OccultationElements)other).minDistKM)
					{
						return EventJD.CompareTo(((OccultationElements)other).EventJD);
					}
					return minDistKM.CompareTo(((OccultationElements)other).minDistKM);
				}
				if (minDArcSec == ((OccultationElements)other).minDArcSec)
				{
					return EventJD.CompareTo(((OccultationElements)other).EventJD);
				}
				return minDArcSec.CompareTo(((OccultationElements)other).minDArcSec);
			case 12:
				if ((MagDropR > 50.0) | (((OccultationElements)other).MagDropR > 50.0))
				{
					if (MagDropR == ((OccultationElements)other).MagDropR)
					{
						return EventJD.CompareTo(((OccultationElements)other).EventJD);
					}
					return ((OccultationElements)other).MagDropR.CompareTo(MagDropR);
				}
				if (MagDropR == ((OccultationElements)other).MagDropR)
				{
					return EventJD.CompareTo(((OccultationElements)other).EventJD);
				}
				return ((OccultationElements)other).MagDropR.CompareTo(MagDropR);
			case 13:
				if (CombinedMagnitudeV == ((OccultationElements)other).CombinedMagnitudeV)
				{
					return EventJD.CompareTo(((OccultationElements)other).EventJD);
				}
				return CombinedMagnitudeV.CompareTo(((OccultationElements)other).CombinedMagnitudeV);
			case 14:
				if (AsteroidKmSec == ((OccultationElements)other).AsteroidKmSec)
				{
					return EventJD.CompareTo(((OccultationElements)other).EventJD);
				}
				return AsteroidKmSec.CompareTo(((OccultationElements)other).AsteroidKmSec);
			default:
				return 0;
			}
		}

		public void SetSiteDependantFields()
		{
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			double num4 = 0.0;
			int num5 = 0;
			float x = 1f;
			float y = 1f;
			float z = 1f;
			float x2;
			float y2;
			float z2;
			if (TestSite)
			{
				if (SiteNeedsUpdating)
				{
					tClosest = 0.0;
					for (int i = 0; i <= 5; i++)
					{
						Maps.SetGlobeOrientation((SubstellarLongitude_OfDate_deg - TClosest * 15.0) / (180.0 / Math.PI), SubstellarLatitude_OfDate_deg / (180.0 / Math.PI));
						Maps.GlobeCoords(SiteLongitudeTest, SiteLatitudeTest, out x, out y, out z, Mirrored: false);
						num2 = XatMin - (double)x + dX * TClosest + d2X * TClosest * TClosest + d3X * TClosest * TClosest * TClosest;
						num3 = YatMin - (double)y + dY * TClosest + d2Y * TClosest * TClosest + d3Y * TClosest * TClosest * TClosest;
						num = (0.0 - (num2 * (dX + d2X * TClosest * 2.0 + d3X * d3x * TClosest * TClosest * 3.0) + num3 * (dY + d2Y * TClosest * 2.0 + d3Y * d3Y * TClosest * TClosest * 3.0))) / n / n;
						tClosest += num;
						if (Math.Abs(num) < 0.0001 || Math.Abs(num) > 1000.0)
						{
							break;
						}
					}
					if (tClosest == double.NaN)
					{
						tClosest = 0.0;
					}
					if (Math.Abs(tClosest) > 36.0)
					{
						tClosest = 0.0;
					}
					jD_EventClosest = EventJD + (MidTime_Hrs + tClosest) / 24.0;
					minD = Math.Abs(num2 * (dY + d2Y * TClosest * 2.0 + d3Y * d3Y * TClosest * TClosest * 3.0) - num3 * (dX + d2X * TClosest * 2.0 + d3X * d3x * TClosest * TClosest * 3.0)) / n;
					minDArcSec = MinD * 8.794143836182533 / DistAsteroid;
					minDistKM = Math.Abs(minD) / VerticalPathScale;
					double num6 = 1.0 / Math.Sqrt(Math.Cos(SiteLatitudeTest) * Math.Cos(SiteLatitudeTest) + 0.993305615000412 * Math.Sin(SiteLatitudeTest) * Math.Sin(SiteLatitudeTest));
					double num7 = 0.993305615000412 * num6 * Math.Sin(SiteLatitudeTest);
					double num8 = num6 * Math.Cos(SiteLatitudeTest);
					double d = SiteLongitudeTest - (SubstellarLongitude_OfDate_deg - TClosest * 15.0) / (180.0 / Math.PI);
					double num9 = (0.0 - Math.Cos(decStarApparent)) * Math.Sin(d);
					double num10 = Math.Sin(decStarApparent) * num8 - Math.Cos(decStarApparent) * Math.Cos(d) * num7;
					double num11 = Math.Sin(decStarApparent) * num7 + Math.Cos(decStarApparent) * Math.Cos(d) * num8;
					altitudeAtEvent = 180.0 / Math.PI * Math.Atan(num11 / Math.Sqrt(num9 * num9 + num10 * num10));
					azimuthAtEvent = 180.0 / Math.PI * Math.Atan2(num9, num10);
					if (azimuthAtEvent < 0.0)
					{
						azimuthAtEvent += 360.0;
					}
					Maps.SetGlobeOrientation((SubSolarLongitude_OfDate_deg - TClosest * 15.0) / (180.0 / Math.PI), SubSolarLatitude_OfDate_deg / (180.0 / Math.PI));
					Maps.GlobeCoords(SiteLongitudeTest, SiteLatitudeTest, out x2, out y2, out z2, Mirrored: false);
					altitudeSunAtEvent = Math.Asin(z2) * (180.0 / Math.PI);
					Utilities.TopocentricMoon(jD_EventClosest, SiteLongitudeTest, SiteLatitudeTest, 0.0, Altitude_is_MSL: true, 0, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out altitudeMoonAtEvent);
				}
				Probability = DisplayMPOccultations.Probability(AsteroidDiameter / 2.0 / DistAsteroid / 6378.137 * 8.794143836182533, ErrorAcrossPath, MinDArcSec);
			}
			if (TestInRegion && RegionNeedsUpdating)
			{
				inRegion = false;
				double num12 = AsteroidDiameter / 2.0 / 6378.137;
				for (int j = 0; j < 4; j++)
				{
					num2 = (tClosestRegional = (num3 = 0.0));
					for (int k = 0; k <= 2; k++)
					{
						Maps.SetGlobeOrientation((SubstellarLongitude_OfDate_deg - TClosest * 15.0) / (180.0 / Math.PI), SubstellarLatitude_OfDate_deg / (180.0 / Math.PI));
						Maps.GlobeCoords(RegionLongitude[j], RegionLatitude[j], out x, out y, out z, Mirrored: false);
						num2 = XatMin - (double)x + dX * tClosestRegional;
						num3 = YatMin - (double)y + dY * tClosestRegional;
						tClosestRegional += (0.0 - (num2 * dX + num3 * dY)) / n / n;
					}
					if (double.IsNaN(num2) | double.IsNaN(num3))
					{
						inRegion = false;
						break;
					}
					num4 = (num2 * dY - num3 * dX) / n;
					if (Math.Abs(num4) < num12)
					{
						inRegion = true;
						break;
					}
					try
					{
						if (j == 0)
						{
							num5 = Math.Sign(num4);
						}
						else if (Math.Sign(num4) != num5)
						{
							inRegion = true;
							break;
						}
					}
					catch
					{
						using StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\ErrorReport.txt", append: true);
						streamWriter.WriteLine("Date" + UTDate);
						streamWriter.WriteLine("Asteroid " + ObjectNumber + " " + ObjectName);
						streamWriter.WriteLine("X0,Y0,dX,dY,n " + num2 + "," + num3 + "," + dX + "," + dY + "," + n);
						streamWriter.WriteLine("tClosestRegional " + tClosestRegional);
						streamWriter.WriteLine("D " + num4);
						streamWriter.WriteLine();
					}
				}
				double num13 = 0.0;
				double num14 = 0.0;
				for (int m = 0; m < 4; m++)
				{
					num13 += RegionLongitude[m];
					num14 += RegionLatitude[m];
				}
				num13 /= 4.0;
				num14 /= 4.0;
				Maps.SetGlobeOrientation((SubstellarLongitude_OfDate_deg - TClosest * 15.0) / (180.0 / Math.PI), SubstellarLatitude_OfDate_deg / (180.0 / Math.PI));
				Maps.GlobeCoords(num13, num14, out x, out y, out z, Mirrored: false);
				Maps.SetGlobeOrientation((SubSolarLongitude_OfDate_deg - TClosest * 15.0) / (180.0 / Math.PI), SubSolarLatitude_OfDate_deg / (180.0 / Math.PI));
				Maps.GlobeCoords(num13, num14, out x2, out y2, out z2, Mirrored: false);
				aboveHorizonInRegion = z > 0f && (double)z2 < 0.1;
			}
			string PlanetOut = "";
			if (IsPlanet & !IsPlanetaryMoon)
			{
				Get_Planet_Local_Event_Times(SiteLongitudeTest, SiteLatitudeTest, MultiSite: false, out PlanetOut);
			}
			Planet_Local_Event_Times = PlanetOut;
		}

		public static void SetLocalHorizon()
		{
			string[] array = Settings.Default.LocalHorizon.Split(new char[1] { ',' });
			for (int i = 0; i < 24; i++)
			{
				LocalHorizon[i] = int.Parse(array[i]);
			}
		}

		public bool Get_Planet_Local_Event_Times(double LonObs, double LatObs, bool MultiSite, out string PlanetOut)
		{
			Satellites.SetPlanetPlusMoonPlotParameters(Planet);
			double[] array = new double[2];
			double[] array2 = new double[2];
			double[] array3 = new double[2];
			bool[] array4 = new bool[2];
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			double num4 = 0.0;
			bool flag = false;
			PlanetOut = "";
			for (int i = 0; i < 2; i++)
			{
				array4[i] = false;
			}
			for (int j = 0; j <= 1; j++)
			{
				double num5 = (double)(Satellites.f / 2f) + (double)(Satellites.f / 2f) * Math.Cos(2.0 * PlanetocentricEarthDec / (180.0 / Math.PI));
				double num6 = 0.0;
				double num7 = 0.0;
				double num8 = 0.0;
				double d = Math.Atan(Utilities.sqrt_1LessEarthEllipticitySqrd * Math.Tan(LatObs));
				num = Math.Cos(d);
				num2 = Utilities.sqrt_1LessEarthEllipticitySqrd * Math.Sin(d);
				double num9;
				double num16;
				bool flag2;
				do
				{
					num9 = num6;
					double d2 = (0.0 - SubstellarLongitude_OfDate_deg) / (180.0 / Math.PI) + 0.2625161707907961 * num6 + LonObs;
					double num10 = XatMin + dX * num6 + d2X * num6 * num6 + d3X * num6 * num6 * num6 - num * Math.Sin(d2);
					double num11 = YatMin + dY * num6 + d2Y * num6 * num6 + d3Y * num6 * num6 * num6 - num2 * Math.Cos(SubstellarLatitude_OfDate_deg / (180.0 / Math.PI)) + num * Math.Cos(d2) * Math.Sin(SubstellarLatitude_OfDate_deg / (180.0 / Math.PI));
					double num12 = dX + 2.0 * d2X * num6 + 3.0 * d3X * num6 * num6 - 0.2625161707907961 * num * Math.Cos(d2);
					double num13 = dY + 2.0 * d2Y * num6 + 3.0 * d3Y * num6 * num6 - 0.2625161707907961 * num * Math.Sin(d2) * Math.Sin(SubstellarLatitude_OfDate_deg / (180.0 / Math.PI));
					double num14 = num12 * num12 + num13 * num13;
					num3 = Math.Sqrt(num14);
					double num15 = num10 * num12 + num11 * num13;
					num16 = Math.Atan2(num10, num11) * (180.0 / Math.PI);
					double num17;
					for (num17 = num16 - PAPlanetPole - 90.0; num17 > 180.0; num17 -= 360.0)
					{
					}
					for (; num17 < -180.0; num17 += 360.0)
					{
					}
					num17 = Math.Abs(num17);
					if (num17 > 90.0)
					{
						num17 = 180.0 - num17;
					}
					num6 = num9 - num15 / num14;
					double num18 = (num10 * num13 - num11 * num12) / num3;
					double num19 = (double)(Satellites.PlanetRadius / 8.79414f) * (1.0 - num5 / 2.0 + num5 / 2.0 * Math.Cos(2.0 * num17 / (180.0 / Math.PI)));
					if (Math.Abs(num18) > num19)
					{
						num7 += 1.0;
						flag2 = false;
						if (num7 > 15.0)
						{
							break;
						}
					}
					else
					{
						double num20 = Math.Sqrt(num19 * num19 - num18 * num18) / num3;
						num6 = ((j != 0) ? (num6 + num20) : (num6 - num20));
						flag2 = true;
						flag = true;
					}
					if (num4 > 0.4)
					{
						break;
					}
					num8 += 1.0;
				}
				while (Math.Abs(num9 - num6) > 0.001 && num8 < 20.0);
				array[j] = MidTime_Hrs + num6;
				array4[j] = flag2;
				array3[j] = num16 - 180.0;
				if (array3[j] < 0.0)
				{
					array3[j] += 360.0;
				}
			}
			for (int k = 0; k <= 1; k++)
			{
				if (array4[k])
				{
					double d2 = (0.0 - SubstellarLongitude_OfDate_deg) / (180.0 / Math.PI) + 0.2625161707907961 * (array[k] - MidTime_Hrs) + LonObs;
					double num21 = num2 * Math.Sin(SubstellarLatitude_J2000_deg / (180.0 / Math.PI)) + num * Math.Cos(d2) * Math.Cos(SubstellarLatitude_J2000_deg / (180.0 / Math.PI));
					array2[k] = Math.Asin(num21) * (180.0 / Math.PI);
					double num22 = num2 * Math.Sin(SubSolarLatitude_OfDate_deg / (180.0 / Math.PI)) + num * Math.Cos(d2 + (SubstellarLongitude_OfDate_deg - SubSolarLongitude_OfDate_deg) / (180.0 / Math.PI)) * Math.Cos(SubSolarLatitude_OfDate_deg / (180.0 / Math.PI));
					if (num21 < 0.0)
					{
						array4[k] = false;
					}
					if ((num22 > 0.0) & (mV > 5.0))
					{
						array4[k] = false;
					}
				}
				flag |= array4[k];
			}
			if (flag)
			{
				StringBuilder stringBuilder = new StringBuilder();
				if (array4[0])
				{
					double num23 = array[0];
					if (num23 < 0.0)
					{
						num23 += 24.0;
					}
					stringBuilder.AppendFormat(" D at " + DisplayMPOccultations.HourMinute(num23, secs: true));
					stringBuilder.AppendFormat(" PA={0,1:F0}° Alt={1,1:F0}°", array3[0], array2[0]);
				}
				if (array4[1])
				{
					if (array4[0])
					{
						stringBuilder.Append(", ");
					}
					double num23 = array[1];
					if (num23 < 0.0)
					{
						num23 += 24.0;
					}
					stringBuilder.Append(" R at " + DisplayMPOccultations.HourMinute(num23, secs: true));
					stringBuilder.AppendFormat(" PA={0,1:F0}° Alt={1,1:F0}°", array3[1], array2[1]);
				}
				PlanetOut = stringBuilder.ToString();
			}
			return flag;
		}

		public bool Read540FormatElements(StreamReader File540, string OrbSource)
		{
			//IL_00b4: Unknown result type (might be due to invalid IL or missing references)
			string text = File540.ReadLine();
			string text2 = File540.ReadLine();
			string text3 = File540.ReadLine();
			string text4 = File540.ReadLine();
			string text5 = File540.ReadLine();
			string text6 = File540.ReadLine();
			string text7 = File540.ReadLine();
			string text8 = File540.ReadLine();
			string text9 = File540.ReadLine();
			string text10 = File540.ReadLine();
			if (text.Length + text2.Length + text3.Length + text4.Length + text5.Length + text6.Length + text7.Length + text8.Length + text9.Length + text10.Length + 20 != 540)
			{
				MessageBox.Show("The selected file is not a file of asteroid predicton elements", "Incorrect file", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return false;
			}
			int num3 = (Planet = (PlanetaryMoon = 0));
			if (text2.Substring(1, 1) == "P")
			{
				Planet = int.Parse(text2.Substring(2, 1));
			}
			if (text2.Substring(3, 1) == "M")
			{
				PlanetaryMoon = int.Parse(text2.Substring(4, 2));
			}
			IsAsteroid = Planet == 0;
			IsPlanetaryMoon = PlanetaryMoon > 0;
			IsPlanet = (Planet > 0) & (PlanetaryMoon < 1);
			if (text8.PadRight(1).Substring(0, 1) == "P")
			{
				IsPlanet = true;
				Planet = int.Parse(text8.Substring(1, 1));
				IsPlanetaryMoon = text8.Substring(2, 1) == "M";
				IsAsteroid = false;
			}
			eventYear = int.Parse(text.Substring(1, 4));
			eventMonth = "JanFebMarAprMayJunJulAugSepOctNovDec".IndexOf(text.Substring(6, 3)) / 3 + 1;
			eventDay = int.Parse(text.Substring(9, 3));
			EventJD = Utilities.JD_from_Date(eventYear, eventMonth, eventDay);
			ObjectName = text2.Substring(7, 15).Trim();
			ObjectNumber = text2.Substring(0, 7).Trim();
			asteroidNumber = 0;
			if (IsAsteroid)
			{
				int.TryParse(ObjectNumber, out var result);
				asteroidNumber = result;
				HasShapeModel = DisplayMPOccultations.IsInDAMIT(result);
				if (!HasShapeModel)
				{
					HasShapeModel = DisplayMPOccultations.IsInISAM(result);
				}
			}
			if (!double.TryParse(text4.Substring(0, 9), out var result2))
			{
				result2 = 25.0;
			}
			mV = result2;
			if (!double.TryParse(text4.Substring(53, 6), out result2))
			{
				result2 = 25.0;
			}
			mB = result2;
			if (!double.TryParse(text5.Substring(0, 4), out result2))
			{
				result2 = 25.0;
			}
			MagAsteroid = result2;
			if (mB > 20.0)
			{
				mb = 25.0;
				if (mr == 0.0)
				{
					mr = 25.0;
				}
				magDropR = 0.0;
			}
			else if (mB != 0.0)
			{
				mr = 1.54 * mV - 0.54 * mB;
				magDropR = MagAsteroid - 0.45 - Utilities.CombinedMagnitude(mR, MagAsteroid - 0.45);
			}
			else
			{
				magDropR = 0.0;
			}
			magDropV = MagAsteroid - Utilities.CombinedMagnitude(mV, MagAsteroid);
			CombinedMagnitudeV = Utilities.CombinedMagnitude(mV, MagAsteroid);
			if (!double.TryParse(text10.Substring(69, 2), out var result3))
			{
				result3 = 0.0;
			}
			if (!double.TryParse(text10.Substring(71, 2), out var result4))
			{
				result4 = 0.0;
			}
			RA_Star2000 = (Utilities.DMStoDeg("  " + text4.Substring(9, 12)) + result3 / 36000000.0) * 15.0 / (180.0 / Math.PI);
			Dec_Star2000 = (Utilities.DMStoDeg(" " + text4.Substring(21, 12)) + result4 / 3600000.0) / (180.0 / Math.PI);
			Utilities.PrecessFromJ2000(EventJD, use2006values_Not1976: false, RA_Star2000, Dec_Star2000, out var RA_date, out var Dec_date);
			RA_Star_Apparent = RA_date;
			Dec_Star_Apparent = Dec_date;
			StarCatName = text4.Substring(33, 20).Trim();
			if (!double.TryParse(text5.Substring(4, 7), out asteroidShadowDiameter_Penumbral))
			{
				asteroidShadowDiameter_Penumbral = 1.0;
			}
			asteroidShadowDiameter_Umbral = asteroidShadowDiameter_Penumbral;
			MaxDurn = double.Parse(text5.Substring(11, 7));
			DistAsteroid = double.Parse(text5.Substring(18, 8));
			asteroidDiameterArcSec = asteroidShadowDiameter_Penumbral / 725.3 / DistAsteroid;
			hourly_dRA = double.Parse(text5.Substring(26, 10));
			hourly_dDec = double.Parse(text5.Substring(36, 10));
			SubstellarLongitude_J2000_deg = double.Parse(text6.Substring(0, 9));
			SubstellarLongitude_OfDate_deg = SubstellarLongitude_J2000_deg + (RA_Star_Apparent - RA_Star2000) * (180.0 / Math.PI);
			SubstellarLatitude_J2000_deg = double.Parse(text6.Substring(9, 8));
			SubstellarLatitude_OfDate_deg = SubstellarLatitude_J2000_deg + (Dec_Star_Apparent - Dec_Star2000) * (180.0 / Math.PI);
			UsesApparentStar = false;
			SubSolarLongitude_OfDate_deg = double.Parse(text6.Substring(17, 9));
			SubSolarLatitude_OfDate_deg = double.Parse(text6.Substring(26, 8));
			solarElongation = Math.Acos(Math.Sin(SubSolarLatitude_OfDate_deg / (180.0 / Math.PI)) * Math.Sin(SubstellarLatitude_J2000_deg / (180.0 / Math.PI)) + Math.Cos(SubSolarLatitude_OfDate_deg / (180.0 / Math.PI)) * Math.Cos(SubstellarLatitude_J2000_deg / (180.0 / Math.PI)) * Math.Cos((SubstellarLongitude_J2000_deg - SubSolarLongitude_OfDate_deg) / (180.0 / Math.PI))) * (180.0 / Math.PI);
			ForJWST = false;
			magLimit = 30.0;
			if (SolarElongation < 10.0)
			{
				magLimit = 5.0;
			}
			else if (SolarElongation < 12.0)
			{
				magLimit = 7.0;
			}
			else if (SolarElongation < 15.0)
			{
				magLimit = 8.0;
			}
			else if (SolarElongation < 20.0)
			{
				magLimit = 9.0;
			}
			else if (SolarElongation < 30.0)
			{
				magLimit = 11.0;
			}
			MidTime_Hrs = double.Parse(text7.Substring(0, 8));
			XatMin = double.Parse(text7.Substring(8, 9));
			YatMin = double.Parse(text7.Substring(17, 9));
			dX = double.Parse(text7.Substring(26, 9));
			if (dX == 0.0)
			{
				dX = 1E-07;
			}
			dY = double.Parse(text7.Substring(35, 9));
			d2X = double.Parse(text7.Substring(44, 9));
			d2Y = double.Parse(text7.Substring(53, 9));
			double num6 = (d3X = (d3Y = 0.0));
			if (IsAsteroid)
			{
				Equinox = int.Parse(text8.Substring(0, 8));
				MeanAnomaly = double.Parse(text8.Substring(8, 10)) / (180.0 / Math.PI);
				EpochYear = int.Parse(text8.Substring(18, 4));
				EpochMonth = int.Parse(text8.Substring(22, 2));
				EpochDay = double.Parse(text8.Substring(24, 8));
				Perihelion = double.Parse(text8.Substring(32, 10)) / (180.0 / Math.PI);
				Node = double.Parse(text8.Substring(42, 10)) / (180.0 / Math.PI);
				i = double.Parse(text8.Substring(52, 10)) / (180.0 / Math.PI);
				e = double.Parse(text8.Substring(62, 10));
				a = double.Parse(text8.Substring(72, 10));
				q = double.Parse(text8.Substring(82, 10));
				MagConst_H0 = double.Parse(text8.Substring(92, 4));
				MagCoeff_logR = double.Parse(text8.Substring(96, 4));
				MagSlopeConst_G = double.Parse(text8.Substring(100, 4));
				if (MagSlopeConst_G == 0.0)
				{
					MagSlopeConst_G = 0.0001;
				}
				delta_MeanAnomaly = double.Parse(text9.Substring(4, 10)) / 10.0 / (180.0 / Math.PI);
				delta_Perihelion = double.Parse(text9.Substring(14, 9)) / 10.0 / (180.0 / Math.PI);
				delta_Node = double.Parse(text9.Substring(23, 9)) / 10.0 / (180.0 / Math.PI);
				delta_i = double.Parse(text9.Substring(32, 9)) / 10.0 / (180.0 / Math.PI);
				delta_e = double.Parse(text9.Substring(41, 10)) / 10.0;
				delta_a = double.Parse(text9.Substring(51, 10)) / 10.0;
				delta_q = double.Parse(text9.Substring(61, 10)) / 10.0;
			}
			else if (IsPlanet & !IsPlanetaryMoon)
			{
				PABrightLimb = double.Parse(text8.Substring(2, 7));
				PlanetPhaseAngle = double.Parse(text8.Substring(9, 7));
				PlanetocentricEarthDec = double.Parse(text8.Substring(16, 7));
				PAPlanetPole = double.Parse(text8.Substring(23, 7));
			}
			OrbitSource = OrbSource;
			ErrorBasis = text10.Substring(0, 5).Trim();
			double.TryParse(text10.Substring(5, 6), out errorEllipseMajor);
			double.TryParse(text10.Substring(11, 6), out errorEllipseMinor);
			double.TryParse(text10.Substring(17, 6), out errorEllipsePA);
			double.TryParse(text10.Substring(23, 6), out error_AsIncreaseInPathWidths);
			double.TryParse(text10.Substring(29, 6), out knownSigma_StarAsterPosns);
			if (KnownSigma_StarAsterPosns == 0.0)
			{
				KnownSigma_StarAsterPosns = (double)Settings.Default.AsteroidSearchDefaultUncertainty;
			}
			if (!int.TryParse(text10.Substring(35, 1), out numAsteroidRings))
			{
				numAsteroidRings = 0;
			}
			if (!int.TryParse(text10.Substring(36, 1), out numAsteroidMoons))
			{
				numAsteroidMoons = 0;
			}
			if (numAsteroidMoons > 0)
			{
				double.TryParse(text10.Substring(37, 4), out var result5);
				double.TryParse(text10.Substring(41, 6), out var result6);
				BinaryAsteroidElements binaryAsteroidElements = new BinaryAsteroidElements();
				binaryAsteroidElements.IDMoonName = "";
				binaryAsteroidElements.Diameter = result5;
				binaryAsteroidElements.A = result6;
				AsteroidMoons.Add(binaryAsteroidElements);
				double.TryParse(text10.Substring(51, 4), out result5);
				double.TryParse(text10.Substring(55, 6), out result6);
				if (result5 > 0.0)
				{
					binaryAsteroidElements.IDMoonName = "";
					binaryAsteroidElements.Diameter = result5;
					binaryAsteroidElements.A = result6;
					AsteroidMoons.Add(binaryAsteroidElements);
				}
			}
			RUWE_540 = text10.Substring(62, 1);
			MagDopV_540 = text10.Substring(63, 1);
			MagDopR_540 = text10.Substring(64, 1);
			NearbyStars540 = text10.Substring(65, 1);
			IsKepler2Star = text10.Substring(66, 1) == "K";
			if (!double.TryParse(text10.Substring(67, 2), out starDiamMAS))
			{
				starDiamMAS = 0.0;
			}
			int.TryParse(text10.Substring(74, 1), out doubleStarCode);
			if (!double.TryParse(text10.Substring(75, 8), out predictionMJD))
			{
				predictionMJD = 0.0;
			}
			eventID = text10.Substring(83, 15);
			AsteroidClass = "";
			AsteroidDiameter = asteroidShadowDiameter_Penumbral - starDiamMAS / 3600000.0 / (180.0 / Math.PI) * DistAsteroid * 149600000.0;
			return true;
		}

		public string XML_Elements()
		{
			string text = "\r\n";
			string[] array = new string[Tags.Length];
			array[0] = "  " + Tags[0] + text;
			array[1] = "    " + Tags[1] + OrbitSource + "," + string.Format("{0,1:f2},", MaxDurn) + EventYear + "," + EventMonth + "," + EventDay + "," + string.Format("{0,1:f7},", MidTime_Hrs) + string.Format("{0,1:f7},", XatMin) + string.Format("{0,1:f7},", YatMin) + string.Format("{0,1:f7},", dX) + string.Format("{0,1:f7},", dY) + string.Format("{0,1:f7},", d2X) + string.Format("{0,1:f7},", d2Y) + string.Format("{0,1:f7},", d3X) + string.Format("{0,1:f7}", d3Y) + EndTags[1] + text;
			string[] obj = new string[9]
			{
				"    ",
				Tags[2],
				string.Format("{0,1:f4},", SubstellarLongitude_OfDate_deg),
				string.Format("{0,1:f4},", SubstellarLatitude_OfDate_deg),
				string.Format("{0,1:f2},", SubSolarLongitude_OfDate_deg),
				string.Format("{0,1:f2},", SubSolarLatitude_OfDate_deg),
				null,
				null,
				null
			};
			bool flag = forJWST;
			obj[6] = flag.ToString();
			obj[7] = EndTags[2];
			obj[8] = text;
			array[2] = string.Concat(obj);
			array[3] = "    " + Tags[3] + StarCatName + "," + string.Format("{0,1:f8},", RA_Star2000 * (180.0 / Math.PI) / 15.0) + string.Format("{0,1:f7},", Dec_Star2000 * (180.0 / Math.PI)) + string.Format("{0,1:f2},", mB) + string.Format("{0,1:f2},", mV) + string.Format("{0,1:f2},", mR) + string.Format("{0,1:f1},", StarDiamMAS) + DoubleStarCode + ",";
			if (IsKepler2Star)
			{
				array[3] += "K";
			}
			else
			{
				ref string reference = ref array[3];
				reference = reference ?? "";
			}
			ref string reference2 = ref array[3];
			reference2 = reference2 + $",{RA_Star_Apparent * (180.0 / Math.PI) / 15.0:f8}," + $"{Dec_Star_Apparent * (180.0 / Math.PI):f7}";
			array[3] += string.Format(",{0:f2},{1:f2},{2,1},{3,1},{4,1}", MagDropV, MagDropR, Convert.ToInt32(NearbyStars_MagAdjusted), NearbyStars_Bright, NearbyStars_All);
			ref string reference3 = ref array[3];
			reference3 = reference3 + EndTags[3] + text;
			array[4] = "    " + Tags[4] + ObjectNumber + "," + ObjectName + "," + string.Format("{0,1:f2},", MagAsteroid) + string.Format("{0,1:f3},", AsteroidShadowDiameter_Penumbral) + string.Format("{0,1:f4},", DistAsteroid) + NumAsteroidRings + "," + NumAsteroidMoons + "," + string.Format("{0,1:f3},", hourly_dRA) + string.Format("{0,1:f2},", hourly_dDec) + AsteroidClass.ToString() + "," + string.Format("{0,1:f1}", AsteroidDiameterUncertainty) + "," + Convert.ToInt32(PlanetMoonInPlanetShadow) + string.Format(",{0,1:f2},", MagVAsteroid) + string.Format("{0,1:f2},", MagRAsteroid) + EndTags[4] + text;
			array[5] = "    " + Tags[5] + text;
			for (int i = 0; i < AsteroidMoons.Count; i++)
			{
				ref string reference4 = ref array[5];
				reference4 = reference4 + "      <Moon>" + AsteroidMoons[i].IDMoonName;
				if (AsteroidMoons[i].Diameter != 0.0)
				{
					if (AsteroidMoons[i].Diameter % 1.0 != 0.0)
					{
						array[5] += string.Format(",{0,1:F1},", AsteroidMoons[i].Diameter);
					}
					else
					{
						array[5] += string.Format(",{0,1:f0},", AsteroidMoons[i].Diameter);
					}
				}
				else
				{
					array[5] += ",,";
				}
				if (AsteroidMoons[i].A != 0.0)
				{
					if (AsteroidMoons[i].A % 1.0 != 0.0)
					{
						array[5] += string.Format("{0,1:F3},", AsteroidMoons[i].A);
					}
					else
					{
						array[5] += string.Format("{0,1:f0},", AsteroidMoons[i].A);
					}
				}
				else
				{
					array[5] += ",";
				}
				if (AsteroidMoons[i].PeriodDays != 0.0)
				{
					array[5] += string.Format("{0,1:F6},", AsteroidMoons[i].PeriodDays);
				}
				else
				{
					array[5] += ",";
				}
				if (AsteroidMoons[i].E != 0.0)
				{
					array[5] += string.Format("{0,1:F4},", AsteroidMoons[i].E);
				}
				else
				{
					array[5] += ",";
				}
				if (AsteroidMoons[i].I != 0.0)
				{
					array[5] += string.Format("{0,1:F2},", AsteroidMoons[i].I);
				}
				else
				{
					array[5] += ",";
				}
				if (AsteroidMoons[i].Year > 1999)
				{
					array[5] += string.Format("{0,1:f2},", Utilities.JD_from_Date(AsteroidMoons[i].Year, AsteroidMoons[i].Month, AsteroidMoons[i].Day));
				}
				else
				{
					array[5] += ",";
				}
				if (AsteroidMoons[i].Meananomaly != 0.0)
				{
					array[5] += string.Format("{0,1:F3},", AsteroidMoons[i].Meananomaly);
				}
				else
				{
					array[5] += ",";
				}
				if (AsteroidMoons[i].Perihelion != 0.0)
				{
					array[5] += string.Format("{0,1:F2},", AsteroidMoons[i].Perihelion);
				}
				else
				{
					array[5] += ",";
				}
				if (AsteroidMoons[i].Node != 0.0)
				{
					array[5] += string.Format("{0,1:F2},", AsteroidMoons[i].Node);
				}
				else
				{
					array[5] += " ";
				}
				ref string reference5 = ref array[5];
				reference5 = reference5 + "</Moon>" + text;
			}
			ref string reference6 = ref array[5];
			reference6 = reference6 + "    " + EndTags[5] + text;
			if (IsAsteroid)
			{
				array[6] = "    " + Tags[6] + (int)Equinox + "," + string.Format("{0,1:f4},", MeanAnomaly * (180.0 / Math.PI)) + EpochYear + "," + EpochMonth + ",";
				if (EpochDay % 1.0 == 0.0)
				{
					ref string reference7 = ref array[6];
					reference7 = reference7 + EpochDay + ",";
				}
				else
				{
					array[6] += string.Format("{0,1:f5},", EpochDay);
				}
				ref string reference8 = ref array[6];
				reference8 = reference8 + string.Format("{0,1:f4},", Perihelion * (180.0 / Math.PI)) + string.Format("{0,1:f4},", Node * (180.0 / Math.PI)) + string.Format("{0,1:f4},", this.i * (180.0 / Math.PI)) + string.Format("{0,1:f5},", e) + string.Format("{0,1:f5},", a) + string.Format("{0,1:f5},", q) + string.Format("{0,1:f2},", MagConst_H0) + string.Format("{0,1:f1},", MagCoeff_logR) + string.Format("{0,1:f2}", magCoeff_G) + EndTags[6] + text;
				if (ObjectName == "50000")
				{
					array[7] = "    " + Tags[7] + string.Format("{0,1:f0},", PlanetIlluminationPercent) + string.Format("{0,1:f0},", PABrightLimb) + string.Format("{0,1:f0},", PlanetPhaseAngle) + string.Format("{0,1:f1},", PlanetocentricEarthDec) + string.Format("{0,1:f1}", PAPlanetPole) + EndTags[7] + text;
				}
			}
			if (IsPlanet)
			{
				array[7] = "    " + Tags[7] + string.Format("{0,1:f0},", PlanetIlluminationPercent) + string.Format("{0,1:f0},", PABrightLimb) + string.Format("{0,1:f0},", PlanetPhaseAngle) + string.Format("{0,1:f1},", PlanetocentricEarthDec) + string.Format("{0,1:f1}", PAPlanetPole) + EndTags[7] + text;
			}
			array[8] = "    " + Tags[8] + string.Format("{0,1:f3},", Error_AsIncreaseInPathWidths) + string.Format("{0,1:f4},", ErrorEllipseMajorAxis) + string.Format("{0,1:f4},", ErrorEllipseMinorAxis) + string.Format("{0,1:f0},", ErrorEllipsePA) + string.Format("{0,1:f4},", KnownSigma_StarAsterPosns) + ErrorBasis + string.Format(",{0,1:f2},", StarReliability) + DuplicateSource + "," + NoGaiaPM + "," + GaiaUCAC4PM + EndTags[8] + text;
			array[9] = "    " + Tags[9] + eventID + string.Format(",{0,1:f4}", PredictionMJD) + EndTags[9] + text;
			string text2 = array[0] + array[1] + array[2] + array[3] + array[4];
			if (AsteroidMoons.Count > 0)
			{
				text2 += array[5];
			}
			if (array[6] != null && array[6].Trim().Length > Tags[6].Length + EndTags[6].Length)
			{
				text2 += array[6];
			}
			if (array[7] != null && array[7].Trim().Length > Tags[7].Length + EndTags[7].Length)
			{
				text2 += array[7];
			}
			return text2 + array[8] + array[9] + "  " + EndTags[0] + text;
		}

		public void Read_XMLElements(string[] Lines, int LineCount)
		{
			for (int i = 0; i < LineCount; i++)
			{
				if (Lines[i].Contains(Tags[1]))
				{
					Parse_Elements(Lines[i]);
				}
				else if (Lines[i].Contains(Tags[2]))
				{
					Parse_Earth(Lines[i]);
				}
				else if (Lines[i].Contains(Tags[3]))
				{
					Parse_Star(Lines[i]);
				}
				else if (Lines[i].Contains(Tags[4]))
				{
					Parse_Object(Lines[i]);
				}
				else if (Lines[i].Contains(Tags[5]))
				{
					AsteroidMoons.Clear();
					do
					{
						i++;
						if (Lines[i].Contains(EndTags[5]))
						{
							break;
						}
						Parse_Moons(Lines[i]);
					}
					while (i < LineCount);
				}
				else if (Lines[i].Contains(Tags[6]))
				{
					Parse_Orbit(Lines[i]);
				}
				else if (Lines[i].Contains(Tags[7]))
				{
					Parse_PlanetPhysical(Lines[i]);
				}
				else if (Lines[i].Contains(Tags[8]))
				{
					Parse_Errors(Lines[i]);
				}
				else if (Lines[i].Contains(Tags[9]))
				{
					Parse_ID(Lines[i]);
				}
			}
			SubstellarLongitude_J2000_deg = SubstellarLongitude_OfDate_deg - (RA_Star_Apparent - RA_Star2000) * (180.0 / Math.PI);
			SubstellarLatitude_J2000_deg = SubstellarLatitude_OfDate_deg - (decStarApparent - decStar2000) * (180.0 / Math.PI);
			EventJD = Utilities.JD_from_Date(eventYear, eventMonth, eventDay);
			double PA_atOrigin;
			if (IsAsteroid)
			{
				int.TryParse(ObjectNumber, out var result);
				AsteroidNumber = result;
				HasShapeModel = DisplayMPOccultations.IsInDAMIT(AsteroidNumber);
				if (!HasShapeModel)
				{
					HasShapeModel = DisplayMPOccultations.IsInISAM(AsteroidNumber);
				}
				if (((MagVAsteroid < 0.0) & (AsteroidNumber > 0)) && MagValues.AsteroidMagnitudeFileExists)
				{
					Utilities.QuickSolarElongation_PhaseAngle(EventJD, rAStar2000, decStar2000, DistAsteroid, out var Elongation, out var PhaseAngle, out var SolarDistObject);
					int colorMagnitudeRecord = MagValues.GetColorMagnitudeRecord(AsteroidNumber);
					if (colorMagnitudeRecord < 0)
					{
						PA_atOrigin = (MagVAsteroid = (MagRAsteroid = -5.0));
					}
					else
					{
						MagValues.ColorMagnitudeData[colorMagnitudeRecord].GetColorMagnitudes(rAStar2000, decStar2000, SolarDistObject, DistAsteroid, PhaseAngle / (180.0 / Math.PI), out magVAsteroid, out magRAsteroid, out Elongation, out PA_atOrigin);
					}
				}
			}
			AsteroidDiameter = asteroidShadowDiameter_Penumbral - starDiamMAS / 3600000.0 / (180.0 / Math.PI) * DistAsteroid * 149600000.0;
			AsteroidDiameterArcSec = AsteroidDiameter / 725.3 / DistAsteroid;
			AsteroidShadowDiameter_Umbral = AsteroidShadowDiameter_Penumbral - 2.0 * starDiamMAS / 3600000.0 / (180.0 / Math.PI) * DistAsteroid * 149600000.0;
			if (MagVAsteroid == -5.0)
			{
				if ((MagDropV < 0.0) | (MagDropR < 0.0))
				{
					if (mB > 20.0)
					{
						mb = 25.0;
						if (mr == 0.0)
						{
							mr = 25.0;
						}
						magDropR = 0.0;
					}
					else if (mB != 0.0)
					{
						mr = 1.54 * mV - 0.54 * mB;
						magDropR = MagAsteroid - 0.45 - Utilities.CombinedMagnitude(mR, MagAsteroid - 0.45);
					}
					else
					{
						magDropR = 0.0;
					}
					magDropV = MagAsteroid - Utilities.CombinedMagnitude(mV, MagAsteroid);
				}
				CombinedMagnitudeV = Utilities.CombinedMagnitude(mV, MagAsteroid);
			}
			else
			{
				magDropV = MagVAsteroid - Utilities.CombinedMagnitude(mV, MagVAsteroid);
				magDropR = MagRAsteroid - Utilities.CombinedMagnitude(mR, MagRAsteroid);
			}
			SolarElongation = Math.Acos(Math.Sin(SubSolarLatitude_OfDate_deg / (180.0 / Math.PI)) * Math.Sin(SubstellarLatitude_OfDate_deg / (180.0 / Math.PI)) + Math.Cos(SubSolarLatitude_OfDate_deg / (180.0 / Math.PI)) * Math.Cos(SubstellarLatitude_OfDate_deg / (180.0 / Math.PI)) * Math.Cos((SubstellarLongitude_OfDate_deg - SubSolarLongitude_OfDate_deg) / (180.0 / Math.PI))) * (180.0 / Math.PI);
			double RA = 0.0;
			double Dec = 0.0;
			double GeocentricDistance = 0.0;
			Utilities.QuickMoon(EventJD + MidTime_Hrs / 24.0, out var RA2, out var Dec2, out var _, out var _, out var _);
			Utilities.QuickPlanet(EventJD + MidTime_Hrs / 24.0, 3, EquinoxOfDate: true, out RA, out Dec, out GeocentricDistance);
			Utilities.Distance(RA2, Dec2, RA, Dec, out var Distance, out PA_atOrigin);
			Utilities.Distance(RA2, Dec2, RA_Star2000, Dec_Star2000, out var Distance2, out PA_atOrigin);
			StarMoonElongation_deg = Distance2 * (180.0 / Math.PI);
			MoonPhase_percent = 50.0 * (1.0 - Math.Cos(Distance));
			magLimit = 30.0;
			if (SolarElongation < 10.0)
			{
				magLimit = 5.0;
			}
			else if (SolarElongation < 12.0)
			{
				magLimit = 7.0;
			}
			else if (SolarElongation < 15.0)
			{
				magLimit = 8.0;
			}
			else if (SolarElongation < 20.0)
			{
				magLimit = 9.0;
			}
			else if (SolarElongation < 30.0)
			{
				magLimit = 11.0;
			}
			if (KnownSigma_StarAsterPosns == 0.0)
			{
				KnownSigma_StarAsterPosns = (double)Settings.Default.AsteroidSearchDefaultUncertainty;
			}
		}

		private void Parse_Elements(string Line)
		{
			string[] array = Line.Replace(Tags[1], "").Replace(EndTags[1], "").Split(new char[1] { ',' });
			if (array.Length != 0)
			{
				OrbitSource = array[0];
			}
			if (array.Length > 1)
			{
				MaxDurn = double.Parse(array[1]);
			}
			if (array.Length > 2)
			{
				EventYear = int.Parse(array[2]);
			}
			if (array.Length > 3)
			{
				EventMonth = int.Parse(array[3]);
			}
			if (array.Length > 4)
			{
				EventDay = int.Parse(array[4]);
			}
			if (array.Length > 5)
			{
				MidTime_Hrs = double.Parse(array[5]);
			}
			if (array.Length > 6)
			{
				XatMin = double.Parse(array[6]);
			}
			if (array.Length > 7)
			{
				YatMin = double.Parse(array[7]);
			}
			if (array.Length > 8)
			{
				dX = double.Parse(array[8]);
			}
			if (array.Length > 9)
			{
				dY = double.Parse(array[9]);
			}
			if (array.Length > 10)
			{
				d2X = double.Parse(array[10]);
			}
			if (array.Length > 11)
			{
				d2Y = double.Parse(array[11]);
			}
			if (array.Length > 12)
			{
				d3X = double.Parse(array[12]);
			}
			if (array.Length > 13)
			{
				d3Y = double.Parse(array[13]);
			}
		}

		private void Parse_Earth(string Line)
		{
			string[] array = Line.Replace(Tags[2], "").Replace(EndTags[2], "").Split(new char[1] { ',' });
			if (array.Length != 0)
			{
				SubstellarLongitude_OfDate_deg = double.Parse(array[0]);
			}
			if (array.Length > 1)
			{
				SubstellarLatitude_OfDate_deg = double.Parse(array[1]);
			}
			if (array.Length > 2)
			{
				SubSolarLongitude_OfDate_deg = double.Parse(array[2]);
			}
			if (array.Length > 3)
			{
				SubSolarLatitude_OfDate_deg = double.Parse(array[3]);
			}
			if (array.Length > 4)
			{
				ForJWST = array[4].Trim() == "1";
			}
			else
			{
				ForJWST = false;
			}
		}

		private void Parse_Star(string Line)
		{
			string[] array = Line.Replace(Tags[3], "").Replace(EndTags[3], "").Split(new char[1] { ',' });
			if (array.Length != 0)
			{
				StarCatName = array[0];
			}
			if (array.Length > 1)
			{
				RA_Star2000 = double.Parse(array[1]) * 15.0 / (180.0 / Math.PI);
			}
			if (array.Length > 2)
			{
				Dec_Star2000 = double.Parse(array[2]) / (180.0 / Math.PI);
			}
			double result;
			if (array.Length > 3)
			{
				if (!double.TryParse(array[3], out result))
				{
					result = 25.0;
				}
				mB = result;
			}
			if (array.Length > 4)
			{
				if (!double.TryParse(array[4], out result))
				{
					result = 25.0;
				}
				mV = result;
			}
			if (array.Length > 5)
			{
				if (!double.TryParse(array[5], out result))
				{
					result = 25.0;
				}
				mR = result;
			}
			if (array.Length > 6)
			{
				StarDiamMAS = double.Parse(array[6]);
			}
			if (array.Length > 7)
			{
				DoubleStarCode = int.Parse(array[7]);
			}
			if (array.Length > 8)
			{
				IsKepler2Star = array[8] == "K";
			}
			if (array.Length >= 10)
			{
				UsesApparentStar = true;
				RA_Star_Apparent = double.Parse(array[9]) * 15.0 / (180.0 / Math.PI);
				Dec_Star_Apparent = double.Parse(array[10]) / (180.0 / Math.PI);
				NearbyStars_MagAdjusted = false;
				int num3 = (NearbyStars_Bright = (NearbyStars_All = -1));
				double num6 = (MagDropV = (MagDropR = -1.0));
				if (array.Length > 15)
				{
					MagDropV = double.Parse(array[11]);
					MagDropR = double.Parse(array[12]);
					NearbyStars_MagAdjusted = array[13] == "1";
					NearbyStars_Bright = int.Parse(array[14]);
					NearbyStars_All = int.Parse(array[15]);
				}
			}
		}

		private void Parse_Object(string Line)
		{
			string[] array = Line.Replace(Tags[4], "").Replace(EndTags[4], "").Split(new char[1] { ',' });
			if (array.Length >= 0)
			{
				ObjectNumber = array[0].Trim();
			}
			int num3 = (Planet = (PlanetaryMoon = 0));
			if (ObjectNumber.Contains("P"))
			{
				Planet = int.Parse(ObjectNumber.Substring(1, 1));
			}
			if (ObjectNumber.Contains("M"))
			{
				PlanetaryMoon = int.Parse(ObjectNumber.Substring(3, 2));
			}
			IsAsteroid = Planet == 0;
			IsPlanetaryMoon = PlanetaryMoon > 0;
			IsPlanet = (Planet > 0) & (PlanetaryMoon < 1);
			if (array.Length > 1)
			{
				ObjectName = array[1];
			}
			if (array.Length > 2)
			{
				MagAsteroid = double.Parse(array[2]);
			}
			if (array.Length > 3)
			{
				AsteroidShadowDiameter_Penumbral = double.Parse(array[3]);
			}
			if (array.Length > 4)
			{
				DistAsteroid = double.Parse(array[4]);
			}
			if (array.Length > 5)
			{
				NumAsteroidRings = int.Parse(array[5]);
			}
			if (array.Length > 6)
			{
				NumAsteroidMoons = int.Parse(array[6]);
			}
			if (array.Length > 7)
			{
				hourly_dRA = double.Parse(array[7]);
			}
			if (array.Length > 8)
			{
				hourly_dDec = double.Parse(array[8]);
			}
			if (array.Length > 9)
			{
				AsteroidClass = array[9].Trim();
			}
			if (array.Length > 10)
			{
				AsteroidDiameterUncertainty = double.Parse(array[10]);
			}
			else
			{
				AsteroidDiameterUncertainty = AsteroidShadowDiameter_Penumbral / 10.0;
			}
			if (array.Length > 11)
			{
				PlanetMoonInPlanetShadow = array[11] == "1";
			}
			if (array.Length <= 13)
			{
				double num6 = (MagVAsteroid = (MagRAsteroid = -10.0));
			}
			else
			{
				MagVAsteroid = double.Parse(array[12]);
				MagRAsteroid = double.Parse(array[13]);
			}
			AsteroidShadowDiameter_Umbral = AsteroidShadowDiameter_Penumbral;
		}

		private void Parse_Moons(string Line)
		{
			string[] array = Line.Replace("      <Moon>".Trim(), "").Replace("</Moon>", "").Split(new char[1] { ',' });
			BinaryAsteroidElements binaryAsteroidElements = new BinaryAsteroidElements();
			binaryAsteroidElements.IDMoonName = array[0].Trim();
			if (array[1].Trim() == "")
			{
				binaryAsteroidElements.Diameter = 0.1;
			}
			else
			{
				binaryAsteroidElements.Diameter = double.Parse(array[1]);
			}
			if (array.Length > 2)
			{
				if (array[2].Trim() == "")
				{
					binaryAsteroidElements.A = 0.0;
				}
				else
				{
					binaryAsteroidElements.A = double.Parse(array[2]);
				}
			}
			if (array.Length > 3)
			{
				if (array[3].Trim() == "")
				{
					binaryAsteroidElements.A = 0.0;
				}
				else
				{
					binaryAsteroidElements.PeriodDays = double.Parse(array[3]);
				}
			}
			if (array.Length > 4)
			{
				if (array[4].Trim() == "")
				{
					binaryAsteroidElements.E = 0.0;
				}
				else
				{
					binaryAsteroidElements.E = double.Parse(array[4]);
				}
			}
			if (array.Length > 5)
			{
				if (array[5].Trim() == "")
				{
					binaryAsteroidElements.I = 0.0;
				}
				else
				{
					binaryAsteroidElements.I = double.Parse(array[5]);
				}
			}
			if (array.Length > 6)
			{
				if (array[6].Trim() == "")
				{
					binaryAsteroidElements.Epoch = 0.0;
				}
				else
				{
					binaryAsteroidElements.Epoch = double.Parse(array[6]);
				}
			}
			if (array.Length > 7)
			{
				if (array[7].Trim() == "")
				{
					binaryAsteroidElements.Meananomaly = 0.0;
				}
				else
				{
					binaryAsteroidElements.Meananomaly = double.Parse(array[7]);
				}
			}
			if (array.Length > 8)
			{
				if (array[8].Trim() == "")
				{
					binaryAsteroidElements.Perihelion = 0.0;
				}
				else
				{
					binaryAsteroidElements.Perihelion = double.Parse(array[8]);
				}
			}
			if (array.Length > 9)
			{
				if (array[9].Trim() == "")
				{
					binaryAsteroidElements.Node = 0.0;
				}
				else
				{
					binaryAsteroidElements.Node = double.Parse(array[9]);
				}
			}
			AsteroidMoons.Add(binaryAsteroidElements);
		}

		private void Parse_Orbit(string Line)
		{
			string[] array = Line.Replace(Tags[6], "").Replace(EndTags[6], "").Split(new char[1] { ',' });
			if (array.Length != 0)
			{
				Equinox = double.Parse(array[0]);
			}
			if (array.Length > 1)
			{
				MeanAnomaly = double.Parse(array[1]) / (180.0 / Math.PI);
			}
			if (array.Length > 2)
			{
				EpochYear = int.Parse(array[2]);
			}
			if (array.Length > 3)
			{
				EpochMonth = int.Parse(array[3]);
			}
			if (array.Length > 4)
			{
				EpochDay = double.Parse(array[4]);
			}
			if (array.Length > 5)
			{
				Perihelion = double.Parse(array[5]) / (180.0 / Math.PI);
			}
			if (array.Length > 6)
			{
				Node = double.Parse(array[6]) / (180.0 / Math.PI);
			}
			if (array.Length > 7)
			{
				i = double.Parse(array[7]) / (180.0 / Math.PI);
			}
			if (array.Length > 8)
			{
				e = double.Parse(array[8]);
			}
			if (array.Length > 9)
			{
				a = double.Parse(array[9]);
			}
			if (array.Length > 10)
			{
				q = double.Parse(array[10]);
			}
			if (array.Length > 11)
			{
				MagConst_H0 = double.Parse(array[11]);
			}
			if (array.Length > 12)
			{
				MagCoeff_logR = double.Parse(array[12]);
			}
			if (array.Length > 13)
			{
				magCoeff_G = double.Parse(array[13]);
			}
		}

		private void Parse_PlanetPhysical(string Line)
		{
			string[] array = Line.Replace(Tags[7], "").Replace(EndTags[7], "").Split(new char[1] { ',' });
			if (array.Length != 0)
			{
				PlanetIlluminationPercent = double.Parse(array[0]);
			}
			if (array.Length > 1)
			{
				PABrightLimb = double.Parse(array[1]);
			}
			if (array.Length > 2)
			{
				PlanetPhaseAngle = double.Parse(array[2]);
			}
			if (array.Length > 3)
			{
				PlanetocentricEarthDec = double.Parse(array[3]);
			}
			if (array.Length > 4)
			{
				PAPlanetPole = double.Parse(array[4]);
			}
		}

		private void Parse_Errors(string Line)
		{
			StarReliability = -1.0;
			DuplicateSource = -1;
			NoGaiaPM = -1;
			GaiaUCAC4PM = -1;
			string[] array = Line.Replace(Tags[8], "").Replace(EndTags[8], "").Split(new char[1] { ',' });
			if (array.Length != 0)
			{
				Error_AsIncreaseInPathWidths = double.Parse(array[0]);
			}
			if (array.Length > 1)
			{
				ErrorEllipseMajorAxis = double.Parse(array[1]);
			}
			if (array.Length > 2)
			{
				ErrorEllipseMinorAxis = double.Parse(array[2]);
			}
			if (array.Length > 3)
			{
				ErrorEllipsePA = double.Parse(array[3]);
			}
			if (array.Length > 4)
			{
				KnownSigma_StarAsterPosns = double.Parse(array[4]);
			}
			if (array.Length > 5)
			{
				ErrorBasis = array[5].Trim();
			}
			if (array.Length > 6)
			{
				StarReliability = double.Parse(array[6]);
			}
			else
			{
				StarReliability = -1.0;
			}
			if (array.Length > 7)
			{
				DuplicateSource = int.Parse(array[7]);
			}
			else
			{
				DuplicateSource = -1;
			}
			if (array.Length > 8)
			{
				NoGaiaPM = int.Parse(array[8]);
			}
			else
			{
				NoGaiaPM = -1;
			}
			if (array.Length > 9)
			{
				GaiaUCAC4PM = int.Parse(array[9]);
			}
			else
			{
				GaiaUCAC4PM = -1;
			}
		}

		private void Parse_ID(string Line)
		{
			string[] array = Line.Replace(Tags[9], "").Replace(EndTags[9], "").Split(new char[1] { ',' });
			if (array.Length != 0)
			{
				eventID = array[0];
			}
			if (array.Length > 1)
			{
				PredictionMJD = double.Parse(array[1]);
			}
		}
	}
}
