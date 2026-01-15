using System;
using System.Collections;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Imaging;
using System.Drawing.Printing;
using System.IO;
using System.Reflection;
using System.Text;
using System.Windows.Forms;
using GaiaDoubles;
using Occult.Asteroid_Observations;
using Occult.Asteroid_Predictions;
using Occult.Asteroids;
using Occult.Mapping;
using Occult.Properties;
using Occult.Star_Catalogues;
using Shapes;

namespace Occult
{
	public class DisplayMPOccultations
	{
		internal static string AppPath;

		private const double Radian = 180.0 / Math.PI;

		public static bool HasBeenCalledFromOccult = false;

		internal static bool Earth2014Present;

		internal const int ElementRecordLength = 540;

		internal const double ShowUmbralPath_Ratio = 0.8;

		internal static bool ElementsFrom540Format = false;

		public static bool FileInUse = false;

		internal static bool MultipathFileFromListAndDisplayIsCurrent = false;

		private static StreamWriter AutogenerateOWStreamWriter;

		internal static string AutogenerateOWpath = "";

		internal static double StarChart_EphemTimeStep = 1.0;

		internal static ListAndDisplay ListAndDisplay;

		internal static AsteroidPlotPath Plot;

		internal static AsteroidSearch SearchForm;

		internal static AsteroidPath PathCoordsForm;

		internal static StarChart Chart;

		private static PlanetViewer PlanetConfiguration;

		private static AsteroidPlotMultipath PlotMultipath;

		private static AsteroidMultiLocations MultiLocations;

		private static AsteroidSummary SummaryForm;

		private static AsteroidPlanetContactTimes ContactTimes;

		private static AsteroidPrePointStars PrePoint;

		private static BinaryAsteroids_Edit BinaryAsteroids;

		private static AsteroidRings AsteroidRings;

		private static SiteEditor Site_Editor;

		internal static StarDia_Fresnel StarDiameter;

		private static Star_DisplayRange Display_StarRange;

		private static DisplayData Non_Single_Gaia;

		internal static PredictionLightCurve PredictionLightCurves;

		internal static Relativity Relativity;

		internal static bool ISAMandDAMIT_IDs_areLoaded = false;

		internal static List<string> FenceLabels = new List<string>();

		internal static double[] RegionLongitude = new double[2];

		internal static double[] RegionLatitude = new double[2];

		internal static bool IncludePathAltitudes = false;

		internal static double EOPx = 0.0;

		internal static double EOPy = 0.0;

		internal static double dUT1 = 0.0;

		internal static double JD_for_EOP = 0.0;

		internal static string Line1;

		internal static string Line2;

		internal static string Line3;

		internal static string Line4;

		internal static string Line5;

		internal static string Line6;

		internal static string Line7;

		internal static string Line8;

		internal static string Line9;

		internal static string Line10;

		internal static bool IsPlanet = false;

		internal static bool IsMoon = false;

		internal static bool InISAM = false;

		internal static bool InDAMIT = false;

		internal static string UTDate;

		internal static string UTDate_Numeric;

		internal static string UTTime;

		internal static string StarNo;

		internal static string RA;

		internal static string Dec;

		internal static string OldName = "";

		internal static string OldStar = "";

		internal static string AsteroidName;

		internal static string ObjectID = "";

		internal static string ObjectName = "";

		internal static string EventHeader;

		internal static string EventFileBase;

		internal static string PredictionDate;

		internal static string OrbitSourceDate;

		internal static string ErrorBasis;

		internal static double EventJD;

		internal static double OsculatingJD;

		internal static double MaxDurn;

		internal static double Mv;

		internal static double Mp;

		internal static double Mr;

		internal static double MAsteroid;

		internal static double Mdrop = 0.0;

		internal static double MdropRed = 0.0;

		internal static double Reliability = -1.0;

		internal static double MvAsteroid;

		internal static double MrAsteroid;

		internal static double MvAsteroidUncert;

		internal static double MrAsteroidUncert;

		internal static int DuplicateSource = -1;

		internal static int NonGaiaPM = -1;

		internal static int GaiaUCAC4PM = -1;

		internal static double AsteroidDiameter = 1.0;

		internal static double DiameterUncertainty = 1.0;

		internal static double AsteroidShadowDiameter_Penumbral = 1.0;

		internal static double AsteroidShadowDiameter_Umbral = 1.0;

		internal static double AsteroidAngularDiameterMAS = 1.0;

		internal static double deltaRA;

		internal static double deltaDec;

		internal static double StellarDiameter_mas = 0.0;

		internal static double MidTime;

		internal static double Xatmin;

		internal static double YatMin;

		internal static double DeltaX;

		internal static double DeltaY;

		internal static double Delta2X;

		internal static double Delta2Y;

		internal static double Delta3X;

		internal static double Delta3Y;

		internal static double StarRA_2000;

		internal static double StarDec_2000;

		internal static double StarRA_Apparent;

		internal static double StarDec_Apparent;

		internal static double FPlaneRA;

		internal static double FPlaneDec;

		internal static double SubstellarLongitude;

		internal static double SubstellarLatitude_J2000;

		internal static double SubSolarLongitude_OfDate;

		internal static double SubSolarLatitude_OfDate;

		internal static double CosElong;

		internal static double SunElong;

		internal static double StarMoonElongation_deg;

		internal static double MoonPhase_percent;

		internal static double DistanceToAsteroid;

		internal static double EventDay;

		internal static double MagLimit;

		internal static double X0;

		internal static double Y0;

		internal static double n;

		internal static double MinimumSepn;

		internal static double MinDArcSec;

		internal static double TClosest;

		internal static double MinGeocentricD;

		internal static double MinGeocentricDMax;

		internal static double VerticalPathScale;

		internal static double EarthSemiDuration;

		internal static double ErrorMajor;

		internal static double ErrorMinor;

		internal static double ErrorPA;

		internal static double ErrorInTime;

		internal static double PathAugmentation_1sigma;

		internal static double Ratio_1Sigma_to_Limit_Distances;

		internal static double Sigma;

		internal static double PredictionMJD;

		internal static double PathAugmentation_Rings = 0.0;

		internal static float xSiteLocation;

		internal static float ySiteLocation;

		internal static float zSiteLocation;

		internal static float Xsun;

		internal static float Ysun;

		internal static float Zsun;

		internal static double[] PlotRAAsteroid = new double[12];

		internal static double[] PlotDecAsteroid = new double[12];

		internal static bool NearbyStarsMagAdjusted = false;

		internal static int NearbyStarsAll = 0;

		internal static int NearbyStarsBright = 0;

		internal static int PlanetNumber;

		internal static int MoonNumber;

		internal static int AsteroidNumber;

		internal static int EpochYear;

		internal static int EpochMonth;

		internal static int EventYear;

		internal static int EventMonth;

		internal static int Equin;

		internal static double MAnom;

		internal static double Perih;

		internal static double node;

		internal static double i;

		internal static double e;

		internal static double A;

		internal static double Q;

		internal static double MagConst_H0;

		internal static double MagCoeff_logR;

		internal static double MagSlopeConst_G;

		internal static double DeltaMAnom;

		internal static double DeltaPerih;

		internal static double DeltaNode;

		internal static double DeltaI;

		internal static double DeltaE;

		internal static double DeltaA;

		internal static double DeltaQ;

		internal static double NowDate;

		internal static double PartDate;

		internal static double EpochDay;

		internal static double RAsteroid;

		internal static double DistAsteroid;

		internal static double magAsteroid;

		internal static double PhaseAngle;

		internal static double VerticalScaleFactor;

		internal static double MoonDiameter = 1.0;

		internal static int DoubleStarCode;

		internal static bool IsKepler2Star = false;

		internal static bool ForJWST = false;

		internal static bool AllowSitePredictions = false;

		internal static bool AllowAsteroidRings = false;

		internal static double PlanetPhaseAngle;

		internal static double PlanetocentricEarthDec;

		internal static double PAPlanetPole;

		internal static bool PlanetMoonInPlanetShadow = false;

		internal static int NumOfMoons;

		internal static int Diameter1;

		internal static int Diameter2;

		internal static int Distance1;

		internal static int Distance2;

		internal static int NumOfRings;

		internal static string MoonName1 = "";

		internal static string MoonName2 = "";

		internal static string AsteroidClass = "";

		internal static bool SatelliteOrbitAvailable = false;

		internal static bool ShowAsteroidRings = true;

		internal static double RApole_deg;

		internal static double DecPole_deg;

		internal static double PlanetocentricRingLatitude;

		internal static double PARingPole;

		internal static double[] RingRadii = new double[5];

		public static bool FilterOnDuration = false;

		public static bool FilterOnMagDrop = false;

		public static bool FilterOnDiameter = false;

		public static bool FilterOnSolarElongation = false;

		public static bool FilterOnTransNeptune = false;

		public static bool FilterOnName = false;

		public static bool FilterOnNumber = false;

		public static bool FilterOnDate = false;

		public static bool FilterOnSite = false;

		public static bool FilterOnKM = false;

		public static bool FilterOnArcSec = false;

		public static bool FilterOnPlanet = false;

		public static bool FilterOnBinary = false;

		public static bool FilterOnShapeModel = false;

		public static bool FilterOnLocalAltitude = false;

		public static double FilterDuration;

		public static double FilterMagDrop;

		public static double FilterDiameter;

		public static double SiteLongitude_Rad;

		public static double SiteLatitude_Rad;

		public static double FilterKM;

		public static double FilterSolarElongation;

		public static double FilterArcSec;

		public static double FirstFilterDate;

		public static double LastFilterDate;

		public static double FilterLocalAltitude = 0.0;

		public static string FilterName;

		public static string SiteLongitudeText;

		public static string SiteLatitudeText;

		public static int FilterAsteroidNumber;

		public static int FilterPlanetNumber;

		internal static string FileNameIn;

		internal static FileStream ElementsStream;

		public static BinaryReader Elements;

		public static StreamWriter DWDFile;

		internal static bool RIO_TNO_event = false;

		internal static long ElementFileLength;

		internal static long CurrentElementFileRecord;

		public static long RecordsInFile;

		internal static List<AsteroidMultiPredictLine> MultiEvent;

		public static string MultiSiteHeader = "";

		public static List<OccultationElements> OccElements = new List<OccultationElements>();

		internal static int Current_OccElements_Record = -1;

		internal static List<AsteroidPrepoint_Stars> PrePointList;

		internal static List<AsteroidPathCoords> PathCoordinates;

		internal static List<AsteroidSummaryLine> EventOut;

		internal static List<PlanetContactTimes> MultiPlanetEvent;

		internal static List<Future_dat> Future = new List<Future_dat>();

		internal static int[] ISAMasteroids = new int[1];

		internal static byte[] ISAMmodels = new byte[1];

		internal static int ISAMmax = 0;

		internal static int[] DAMITasteroids = new int[1];

		internal static byte[] DAMITmodels = new byte[1];

		internal static int DAMITmax = 0;

		public static double PlotMagnification_Initial = 1.0;

		internal static double PlotMagnification = 1.0;

		internal static float AsteroidEarthDia;

		internal static float XerrorSet;

		internal static float YerrorSet;

		internal static bool BWFlag = false;

		internal static bool DrawErrorEllipse = true;

		internal static bool UseSetErrorLocation = false;

		internal static bool UseRedrawSites = false;

		internal static bool IncludeStarChart = true;

		internal static double RedrawLongitude;

		internal static double RedrawLatitude;

		private static double PlotLongitude;

		private static double PlotLatitude;

		internal static bool CenteredUsingTimeOnPath = false;

		public static int AsteroidDateRange => (int)Settings.Default.DateRangeInAsteroidDisplaySelectionCriterion;

		public static void Show_StarDiameter()
		{
			try
			{
				((Control)StarDiameter).Show();
			}
			catch
			{
				StarDiameter = new StarDia_Fresnel();
				((Control)StarDiameter).Show();
				((Form)StarDiameter).set_Location(Settings.Default.LocationShowStarDiameter);
			}
		}

		public static void ShowSearch()
		{
			try
			{
				((Control)SearchForm).Show();
			}
			catch
			{
				SearchForm = new AsteroidSearch();
				((Control)SearchForm).Show();
			}
			((Control)SearchForm).Focus();
		}

		public static void ShowListAndDisplay()
		{
			try
			{
				((Control)ListAndDisplay).Show();
			}
			catch
			{
				ListAndDisplay = new ListAndDisplay();
				((Control)ListAndDisplay).Show();
			}
			((Control)ListAndDisplay).Focus();
		}

		public static void Show_Relativity()
		{
			try
			{
				((Control)Relativity).Show();
			}
			catch
			{
				Relativity = new Relativity();
				((Control)Relativity).Show();
			}
			((Control)Relativity).Focus();
		}

		public static void Show_PredictionLightCurves(bool ForAnEvent)
		{
			try
			{
				((Control)PredictionLightCurves).Show();
			}
			catch
			{
				PredictionLightCurves = new PredictionLightCurve();
				((Control)PredictionLightCurves).Show();
			}
			((Control)PredictionLightCurves).Focus();
			((Control)PredictionLightCurves.updn_masPERsec).set_Enabled(false);
			((Control)PredictionLightCurves.updnParallax).set_Enabled(false);
			((Control)PredictionLightCurves.chkTopoDistances).set_Enabled(true);
			if (!ForAnEvent)
			{
				PredictionLightCurves.updnAsteroidDiameter_mas.set_Value(20m);
				PredictionLightCurves.updnAsteroidDia_km.set_Value(30m);
				PredictionLightCurves.updnParallax.set_Value(4m);
				PredictionLightCurves.updnAsteroidMag.set_Value(14m);
				PredictionLightCurves.updnStarDiameter.set_Value(1m);
				PredictionLightCurves.updnStarMag.set_Value(10m);
				PredictionLightCurves.updn_masPERsec.set_Value(1m);
				((Control)PredictionLightCurves.updn_masPERsec).set_Enabled(true);
				((Control)PredictionLightCurves.updnParallax).set_Enabled(true);
				((Control)PredictionLightCurves.chkTopoDistances).set_Enabled(false);
				PredictionLightCurves.TopoKmRatio = 1f;
				((Control)PredictionLightCurves.lblEvent).set_Text("Non-specific event lightcurve");
				PredictionLightCurves.ObjectID = " ? ";
				PredictionLightCurves.ReSize();
				PredictionLightCurves.Plot(GenerateModel: true);
			}
		}

		internal static double ErrorAcrossPath(double dx, double dy, double errorEllipseMajor, double errorEllipseMinor, double errorEllipsePA_deg)
		{
			if (errorEllipseMajor == errorEllipseMinor)
			{
				return errorEllipseMajor;
			}
			double num = 90.0 - 180.0 / Math.PI * Math.Atan(dy / dx);
			double num2;
			for (num2 = errorEllipsePA_deg - num; num2 > 90.0; num2 -= 180.0)
			{
			}
			for (; num2 < -90.0; num2 += 180.0)
			{
			}
			num2 = Math.Abs(num2) / (180.0 / Math.PI);
			double num3 = 0.0;
			double num4 = 0.0;
			for (int i = 0; i <= 90; i += 2)
			{
				num4 = errorEllipseMajor * errorEllipseMinor / Math.Sqrt(Math.Pow(errorEllipseMajor * Math.Cos((double)i / (180.0 / Math.PI)), 2.0) + Math.Pow(errorEllipseMinor * Math.Sin((double)i / (180.0 / Math.PI)), 2.0)) * Math.Sin((double)i / (180.0 / Math.PI) - num2 + Math.PI / 2.0);
				if (num4 > num3)
				{
					num3 = num4;
				}
			}
			return num3;
		}

		internal static void LoadISAM_and_DAMIT_ids(bool UpdateISAM)
		{
			if (ISAMandDAMIT_IDs_areLoaded)
			{
				return;
			}
			ISAMmax = 0;
			_ = HasBeenCalledFromOccult;
			if (File.Exists(Utilities.AppPath + "\\ShapeModels\\ISAM_AvailableAsteroids.csv"))
			{
				bool flag = true;
				int num = 5;
				using (StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\ShapeModels\\ISAM_AvailableAsteroids.csv"))
				{
					string text = streamReader.ReadLine();
					flag = text.Contains("Date") | text.Contains("Ast#");
					num++;
					do
					{
						streamReader.ReadLine();
						num++;
					}
					while (!streamReader.EndOfStream);
				}
				ISAMasteroids = new int[num];
				ISAMmodels = new byte[num];
				using StreamReader streamReader2 = new StreamReader(Utilities.AppPath + "\\ShapeModels\\ISAM_AvailableAsteroids.csv");
				ISAMmax = -1;
				if (flag)
				{
					streamReader2.ReadLine();
				}
				do
				{
					string[] array = streamReader2.ReadLine()!.Split(new char[1] { ',' });
					if (int.TryParse(array[0], out var result))
					{
						ISAMmax++;
						ISAMasteroids[ISAMmax] = result;
						ISAMmodels[ISAMmax] = byte.Parse(array[1]);
					}
				}
				while (!streamReader2.EndOfStream);
			}
			GetShapeModelData.InitialiseDAMITShapeModels();
			int count = GetShapeModelData.DAMITModelsByAsteroidNumber.Count;
			DAMITasteroids = new int[count];
			DAMITmodels = new byte[count];
			DAMITasteroids[0] = GetShapeModelData.DAMITModelsByAsteroidNumber[0].AsteroidNumber;
			DAMITmodels[0] = 1;
			DAMITmax = 0;
			for (int i = 1; i < GetShapeModelData.DAMITModelsByAsteroidNumber.Count; i++)
			{
				if (GetShapeModelData.DAMITModelsByAsteroidNumber[i].AsteroidNumber == DAMITasteroids[DAMITmax])
				{
					DAMITmodels[DAMITmax]++;
					continue;
				}
				DAMITmax++;
				DAMITasteroids[DAMITmax] = GetShapeModelData.DAMITModelsByAsteroidNumber[i].AsteroidNumber;
				DAMITmodels[DAMITmax] = 1;
			}
			ISAMandDAMIT_IDs_areLoaded = true;
		}

		internal static bool IsInISAM(int AsteroidNumber)
		{
			int NumModels;
			return IsInISAM(AsteroidNumber, out NumModels);
		}

		internal static bool IsInISAM(int AsteroidNumber, out int NumModels)
		{
			bool result = false;
			if (!ISAMandDAMIT_IDs_areLoaded)
			{
				LoadISAM_and_DAMIT_ids(UpdateISAM: true);
			}
			NumModels = 0;
			int num = 0;
			int num2 = ISAMmax;
			do
			{
				int num3 = (num + num2) / 2;
				if (AsteroidNumber == ISAMasteroids[num3])
				{
					result = true;
					NumModels = ISAMmodels[num3];
					break;
				}
				if (AsteroidNumber < ISAMasteroids[num3])
				{
					num2 = num3 - 1;
				}
				else
				{
					num = num3 + 1;
				}
			}
			while (num2 >= num);
			return result;
		}

		internal static bool IsInDAMIT(int AsteroidNumber)
		{
			int NumModels;
			return IsInDAMIT(AsteroidNumber, out NumModels);
		}

		internal static bool IsInDAMIT(int AsteroidNumber, out int NumModels)
		{
			bool result = false;
			NumModels = 0;
			int num = 0;
			int num2 = DAMITmax;
			do
			{
				int num3 = (num + num2) / 2;
				if (AsteroidNumber == DAMITasteroids[num3])
				{
					result = true;
					NumModels = DAMITmodels[num3];
					break;
				}
				if (AsteroidNumber < DAMITasteroids[num3])
				{
					num2 = num3 - 1;
				}
				else
				{
					num = num3 + 1;
				}
			}
			while (num2 >= num);
			return result;
		}

		internal static void CreateListOfFutureFileEvents()
		{
			string text = "";
			string[] array = new string[20];
			int num = 0;
			Future.Clear();
			for (int i = 0; i <= 1; i++)
			{
				string path = ((i != 0) ? (Utilities.AppPath + "\\Generated Files\\FutureAll.xml") : (Utilities.AppPath + "\\Generated Files\\Future.xml"));
				if (!File.Exists(path))
				{
					continue;
				}
				using StreamReader streamReader = new StreamReader(path);
				streamReader.ReadLine();
				do
				{
					text = streamReader.ReadLine()!.Trim();
					if (text == "</Occultations>" || text != OccultationElements.Tags[0])
					{
						break;
					}
					OccultationElements occultationElements = new OccultationElements();
					num = 0;
					for (int j = 0; j < 20; j++)
					{
						array[j] = "";
					}
					do
					{
						array[num] = streamReader.ReadLine()!.Trim();
						if (array[num] == OccultationElements.EndTags[0])
						{
							break;
						}
						num++;
					}
					while (!streamReader.EndOfStream);
					occultationElements.Read_XMLElements(array, num);
					Future_dat future_dat = new Future_dat();
					future_dat.Date = occultationElements.UTDate.Replace(" 0", "  ").Trim();
					future_dat.AsteroidNumber = occultationElements.AsteroidNumber;
					future_dat.Star = occultationElements.StarCatName.Trim().Replace("UCAC2", "2UCAC").Replace("4UC", "4U")
						.Replace("UCAC4-", "4U ");
					if (future_dat.Star.Contains("TYC"))
					{
						future_dat.Star = future_dat.Star.Replace("-0000", "-").Replace("-000", "-").Replace("-00", "-")
							.Replace("-0", "-");
						future_dat.Star = future_dat.Star.Replace("TYC 000", "TYC ").Replace("TYC 00", "TYC ").Replace("TYC 0", "TYC ");
					}
					Future.Add(future_dat);
				}
				while (!streamReader.EndOfStream);
			}
			Future.Sort();
			for (int num2 = Future.Count - 1; num2 >= 1; num2--)
			{
				if (Future[num2].AsteroidNumber == Future[num2 - 1].AsteroidNumber && ((Future[num2].Star == Future[num2 - 1].Star) & (Future[num2].Date == Future[num2 - 1].Date)))
				{
					Future.RemoveAt(num2);
				}
			}
		}

		internal static bool IsInFuture_XML(string AsterNum, string Date, string Star)
		{
			if (Future.Count < 1)
			{
				return false;
			}
			if (!int.TryParse(AsterNum, out var result))
			{
				return false;
			}
			int num = 0;
			int num2 = Future.Count - 1;
			int num3 = result - 1;
			int num4;
			if (num3 < 1)
			{
				num4 = 0;
			}
			else
			{
				do
				{
					num4 = (num + num2) / 2;
					if (num3 == Future[num4].AsteroidNumber)
					{
						break;
					}
					if (num3 > Future[num4].AsteroidNumber)
					{
						num = num4 + 1;
					}
					else
					{
						num2 = num4 - 1;
					}
				}
				while (num2 >= num);
			}
			Star = Star.Replace("UCAC2", "2UCAC").Replace("4UC", "4U").Replace("UCAC4-", "4U ");
			if (Star.Contains("TYC"))
			{
				Star = Star.Replace("-0000", "-").Replace("-000", "-").Replace("-00", "-")
					.Replace("-0", "-");
				Star = Star.Replace("TYC 000", "TYC ").Replace("TYC 00", "TYC ").Replace("TYC 0", "TYC ");
			}
			do
			{
				if (result == Future[num4].AsteroidNumber)
				{
					if ((Date == Future[num4].Date) & (Star == Future[num4].Star))
					{
						return true;
					}
				}
				else if (result < Future[num4].AsteroidNumber)
				{
					return false;
				}
				num4++;
			}
			while (num4 < Future.Count);
			return false;
		}

		internal static string ReadOccElementFile(bool Merge)
		{
			//IL_0004: Unknown result type (might be due to invalid IL or missing references)
			//IL_000a: Expected O, but got Unknown
			//IL_008c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0092: Invalid comparison between Unknown and I4
			//IL_017a: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ca: Unknown result type (might be due to invalid IL or missing references)
			//IL_027d: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ac: Unknown result type (might be due to invalid IL or missing references)
			bool flag = false;
			bool flag2 = false;
			OpenFileDialog val = new OpenFileDialog();
			((FileDialog)val).set_Title("Specify file to READ asteroidal occultation elements from.");
			((FileDialog)val).set_Filter("XML files (*.xml)|*.xml|Steve Preston's Future files |future*.xml|All files (*.*)|*.*");
			((FileDialog)val).set_FilterIndex(Settings.Default.OccelmntLastIndex);
			((FileDialog)val).set_InitialDirectory(Utilities.AppPath + "\\Generated Files");
			if (File.Exists(Settings.Default.OccelmntLastFileName))
			{
				((FileDialog)val).set_InitialDirectory(Path.GetDirectoryName(Settings.Default.OccelmntLastFileName));
			}
			try
			{
				((Form)PredictionLightCurves).Close();
			}
			catch
			{
			}
			try
			{
				((Form)AsteroidRings).Close();
			}
			catch
			{
			}
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				FileNameIn = ((FileDialog)val).get_FileName();
				if (new FileInfo(FileNameIn).Length > 100)
				{
					Settings.Default.OccelmntLastFileName = FileNameIn;
					Settings.Default.OccelmntLastIndex = ((FileDialog)val).get_FilterIndex();
					if (!Merge)
					{
						OccElements.Clear();
						Current_OccElements_Record = -1;
					}
					using (StreamReader streamReader = new StreamReader(FileNameIn))
					{
						string text = streamReader.ReadLine()!.Trim();
						flag = text == "<Occultations>";
						flag2 = text == OccultationElements.Tags[0];
					}
					Cursor.set_Current(Cursors.get_WaitCursor());
					if (flag || flag2)
					{
						string text2 = "";
						string[] array = new string[20];
						int num = 0;
						using StreamReader streamReader2 = new StreamReader(FileNameIn);
						if (flag && streamReader2.ReadLine() != "<Occultations>")
						{
							MessageBox.Show("The file is incorrectly formatted and can't be read", "Incorrect format", (MessageBoxButtons)0, (MessageBoxIcon)16);
							return "";
						}
						do
						{
							text2 = streamReader2.ReadLine()!.Trim();
							if (text2 == "</Occultations>")
							{
								break;
							}
							if (text2 != OccultationElements.Tags[0])
							{
								MessageBox.Show("The file is incorrectly formatted and can't be read", "Incorrect format", (MessageBoxButtons)0, (MessageBoxIcon)16);
								return "";
							}
							OccultationElements occultationElements = new OccultationElements();
							num = 0;
							for (int i = 0; i < 20; i++)
							{
								array[i] = "";
							}
							do
							{
								array[num] = streamReader2.ReadLine()!.Trim();
								if (array[num] == OccultationElements.EndTags[0])
								{
									break;
								}
								num++;
							}
							while (!streamReader2.EndOfStream);
							occultationElements.Read_XMLElements(array, num);
							OccElements.Add(occultationElements);
							if (OccElements.Count >= MinorPlanetOccultationElements.MaximumNumEvents)
							{
								MessageBox.Show("You are trying to load more than " + MinorPlanetOccultationElements.MaximumNumEvents + " events, which is the maximum number currently allowed.\r\n\r\nThis restriction has been imposed to ensure the program does\r\nnot run out of memory, and has good performance.\r\n\r\nTo display more events, you can change the limit at\r\nMaintenance   User Settings  11. Asteroid occultations...\r\nHowever you will run the risk of a fatal error due to lack of memory.\r\n\r\nPress OK to continue with displaying the currently found events", "Too many events", (MessageBoxButtons)0, (MessageBoxIcon)48);
								break;
							}
						}
						while (!streamReader2.EndOfStream);
					}
					else
					{
						string text3 = "";
						if (FileNameIn.Contains("LuckyStar"))
						{
							text3 = "LuckyStar";
						}
						else if (FileNameIn.Contains("_RIO_"))
						{
							text3 = "RIO_";
						}
						else if (FileNameIn.ToUpper().Contains("FUTURE"))
						{
							text3 = "SPreston";
						}
						DateTime lastWriteTime = new FileInfo(FileNameIn).LastWriteTime;
						text3 = text3 + lastWriteTime.Year + Utilities.ShortMonths[lastWriteTime.Month] + lastWriteTime.Day.ToString().PadLeft(2, '0');
						using StreamReader streamReader3 = new StreamReader(FileNameIn);
						do
						{
							OccultationElements occultationElements2 = new OccultationElements();
							if (!occultationElements2.Read540FormatElements(streamReader3, text3))
							{
								break;
							}
							OccElements.Add(occultationElements2);
							if (OccElements.Count >= MinorPlanetOccultationElements.MaximumNumEvents)
							{
								MessageBox.Show("You are trying to load more than " + MinorPlanetOccultationElements.MaximumNumEvents + " events, which is the maximum number currently allowed.\r\n\r\nThis restriction has been imposed to ensure the program does\r\nnot run out of memory, and has good performance.\r\n\r\nTo display more events, you can change the limit at\r\nMaintenance   User Settings  11. Asteroid occultations...\r\nHowever you will run the risk of a fatal error due to lack of memory.\r\n\r\nPress OK to continue with displaying the currently found events", "Too many events", (MessageBoxButtons)0, (MessageBoxIcon)48);
								break;
							}
						}
						while (!streamReader3.EndOfStream);
					}
					Cursor.set_Current(Cursors.get_Default());
					return FileNameIn;
				}
			}
			return "";
		}

		public static int PasteOccultationElements()
		{
			string[] array = new string[50];
			int num = 0;
			int num2 = 0;
			string[] array2 = Clipboard.GetText().Replace("\r", "").Split(new char[1] { Convert.ToChar("\n") });
			for (int i = 0; i < array2.Length; i++)
			{
				if (array2[i].Trim() != OccultationElements.Tags[0])
				{
					continue;
				}
				array = new string[50];
				num = 0;
				array[num] = array2[i].Trim();
				for (int j = i; j < array2.Length; j++)
				{
					i = j;
					num++;
					array[num] = array2[i].Trim();
					if (array2[i].Trim() == OccultationElements.EndTags[0])
					{
						OccultationElements occultationElements = new OccultationElements();
						occultationElements.Read_XMLElements(array, num);
						OccElements.Add(occultationElements);
						num2++;
						break;
					}
				}
			}
			return num2;
		}

		public static bool OpenOccultationElementFile(bool DWD, string NameOfFile)
		{
			//IL_0003: Unknown result type (might be due to invalid IL or missing references)
			//IL_0009: Expected O, but got Unknown
			//IL_0043: Unknown result type (might be due to invalid IL or missing references)
			//IL_0049: Invalid comparison between Unknown and I4
			//IL_0070: Unknown result type (might be due to invalid IL or missing references)
			//IL_0076: Expected O, but got Unknown
			//IL_00c7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00cd: Invalid comparison between Unknown and I4
			//IL_01e0: Unknown result type (might be due to invalid IL or missing references)
			//IL_01e6: Invalid comparison between Unknown and I4
			if (DWD)
			{
				SaveFileDialog val = new SaveFileDialog();
				((FileDialog)val).set_Title("Specify file to Write DWD elements");
				((FileDialog)val).set_Filter("DWD file (*.dwd)|*.dwd|All files (*.*)|*.*");
				((FileDialog)val).set_FilterIndex(1);
				val.set_OverwritePrompt(false);
				((FileDialog)val).set_InitialDirectory(AppPath + "\\Predictions\\");
				if ((int)((CommonDialog)val).ShowDialog() != 1)
				{
					return false;
				}
				DWDFile = new StreamWriter(((FileDialog)val).get_FileName(), append: true);
			}
			if (NameOfFile == "")
			{
				OpenFileDialog val2 = new OpenFileDialog();
				((FileDialog)val2).set_Title("Specify file to READ asteroidal occultation elements from.");
				((FileDialog)val2).set_Filter("OCCELMNT file (OCCELMNT*.*)|occelmnt*.*|Planet Files (*.PLA)|*.pla|Steve Preston's  Future.dat |future.dat|All files (*.*)|*.*");
				((FileDialog)val2).set_FilterIndex(Settings.Default.OccelmntLastIndex);
				((FileDialog)val2).set_FileName(Settings.Default.OccelmntLastFileName);
				try
				{
					((FileDialog)val2).set_InitialDirectory(Path.GetDirectoryName(Settings.Default.OccelmntLastFileName));
				}
				catch
				{
				}
				if ((int)((CommonDialog)val2).ShowDialog() != 1)
				{
					if (DWD)
					{
						DWDFile.Close();
					}
					return false;
				}
				FileNameIn = ((FileDialog)val2).get_FileName();
				Settings.Default.OccelmntLastFileName = FileNameIn;
				Settings.Default.OccelmntLastIndex = ((FileDialog)val2).get_FilterIndex();
			}
			else
			{
				FileNameIn = NameOfFile;
			}
			if (FileNameIn.ToUpper().Contains("_RIO"))
			{
				Settings.Default.RIO_TNOcount++;
				if (Settings.Default.RIO_TNOcount % 20 == 0)
				{
					Settings.Default.RIO_TNOcount = 0;
					if (!MinorPlanetConvertRIO.ShowRioMessage(Reminder: true))
					{
						if (DWD)
						{
							DWDFile.Close();
						}
						return false;
					}
				}
				RIO_TNO_event = true;
			}
			else
			{
				RIO_TNO_event = false;
			}
			ElementsStream = new FileStream(FileNameIn, FileMode.Open, FileAccess.Read);
			Elements = new BinaryReader(ElementsStream);
			ElementFileLength = ElementsStream.Length;
			if (ElementFileLength % 540 != 0L)
			{
				ElementsStream.Close();
				if (DWD)
				{
					DWDFile.Close();
				}
				if ((int)MessageBox.Show("The selected Element file is not in the current format.\r\n\r\nDo you want to create an error report for this file?", "Incorrect Element File", (MessageBoxButtons)4, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6)
				{
					CheckOCCELMNTFile(FileNameIn);
				}
				return false;
			}
			RecordsInFile = ElementFileLength / 540;
			OldName = "";
			OldStar = "";
			return true;
		}

		public static void Get_Data_From_540Record(int RecordNumber, out bool MeetsCriteria, bool GeneratingSummary, bool AutoPlot)
		{
			_ = new char[540];
			MeetsCriteria = false;
			if ((RecordNumber + 1) * 540 > ElementFileLength)
			{
				return;
			}
			Application.DoEvents();
			if (!ElementsStream.CanRead)
			{
				return;
			}
			try
			{
				((Form)PredictionLightCurves).Close();
			}
			catch
			{
			}
			try
			{
				((Form)AsteroidRings).Close();
			}
			catch
			{
			}
			ElementsStream.Seek(RecordNumber * 540, SeekOrigin.Begin);
			ElementsFrom540Format = true;
			char[] value = Elements.ReadChars(540);
			Line1 = new string(value, 0, 12);
			Line2 = new string(value, 14, 22);
			Line3 = new string(value, 38, 12);
			Line4 = new string(value, 52, 59);
			Line5 = new string(value, 113, 46);
			Line6 = new string(value, 161, 34);
			Line7 = new string(value, 197, 62);
			Line8 = new string(value, 261, 104);
			Line9 = new string(value, 367, 71);
			Line10 = new string(value, 440, 98);
			IsPlanet = false;
			IsMoon = false;
			PlanetNumber = (MoonNumber = 0);
			if ((Line8.Substring(0, 1) == "P") & (Line8.Substring(2, 1) == " "))
			{
				IsPlanet = true;
			}
			if (Line8.Substring(2, 1) == "M")
			{
				IsMoon = true;
				MoonNumber = int.Parse(Line8.Substring(3, 2));
				MoonDiameter = Satellites.SatelliteDiameter(PlanetNumber, MoonNumber, out var _);
			}
			AllowSitePredictions = false;
			if ((PlanetNumber > 0) & ((MoonNumber == 0) | (MoonDiameter > 1000.0)))
			{
				AllowSitePredictions = true;
			}
			AllowAsteroidRings = false;
			int.TryParse(Line8.Substring(1, 1), out PlanetNumber);
			if (FilterOnPlanet && ((!IsPlanet & !IsMoon) || PlanetNumber != FilterPlanetNumber))
			{
				return;
			}
			double.TryParse(Line5.Substring(11, 7), out MaxDurn);
			if (FilterOnDuration && MaxDurn < FilterDuration)
			{
				return;
			}
			Mv = (Mp = (Mr = (MAsteroid = (AsteroidDiameter = (AsteroidShadowDiameter_Penumbral = 0.0)))));
			double.TryParse(Line5.Substring(0, 4), out MAsteroid);
			MvAsteroid = (MrAsteroid = -10.0);
			double.TryParse(Line5.Substring(4, 7), out AsteroidShadowDiameter_Penumbral);
			AsteroidShadowDiameter_Umbral = AsteroidShadowDiameter_Penumbral;
			double.TryParse(Line4.Substring(0, 9), out Mv);
			double.TryParse(Line4.Substring(53, 6), out Mr);
			Mdrop = MAsteroid + 2.5 * Math.Log10(Math.Pow(10.0, Mv / -2.5) + Math.Pow(10.0, MAsteroid / -2.5));
			MdropRed = MAsteroid - 0.45 + 2.5 * Math.Log10(Math.Pow(10.0, Mr / -2.5) + Math.Pow(10.0, (MAsteroid - 0.45) / -2.5));
			if ((Mp == 0.0) | (Mp > 20.0))
			{
				Mp = 90.0;
			}
			if (((FilterOnMagDrop & (AsteroidDiameter < 2000.0)) && Mdrop < FilterMagDrop) || (FilterOnDiameter && AsteroidShadowDiameter_Penumbral < FilterDiameter))
			{
				return;
			}
			double.TryParse(Line8.Substring(72, 10), out A);
			if (FilterOnTransNeptune && A < 6.0)
			{
				return;
			}
			AsteroidName = Line2;
			int.TryParse(Line2.Substring(0, 7), out AsteroidNumber);
			AsteroidName = AsteroidName.Replace(" /", " ");
			InISAM = (SatelliteOrbitAvailable = false);
			if (AsteroidNumber > 0)
			{
				InISAM = IsInISAM(AsteroidNumber);
				InDAMIT = IsInDAMIT(AsteroidNumber);
				if (FilterOnShapeModel && (!InDAMIT & !InISAM))
				{
					return;
				}
				SatelliteOrbitAvailable = Settings.Default.MiriadeAsteroids.Contains(" " + Line2.Substring(0, 7).Trim() + " ");
			}
			StarNo = Line4.Substring(33, 20).Trim();
			if ((OldName == AsteroidName) & (OldStar == StarNo))
			{
				return;
			}
			IsKepler2Star = Line10.Substring(66, 1) == "K";
			int.TryParse(Line10.Substring(74, 1), out DoubleStarCode);
			if ((FilterOnNumber && AsteroidNumber != FilterAsteroidNumber) || (FilterOnName && AsteroidName.IndexOf(FilterName, StringComparison.CurrentCultureIgnoreCase) < 0))
			{
				return;
			}
			UTDate = Line1.TrimStart(Array.Empty<char>()) + " ";
			EventYear = int.Parse(Line1.Substring(1, 4));
			EventMonth = "JanFebMarAprMayJunJulAugSepOctNovDec".IndexOf(Line1.Substring(6, 3)) / 3 + 1;
			EventDay = int.Parse(Line1.Substring(9, 3));
			UTDate_Numeric = EventYear + "_" + EventMonth.ToString().PadLeft(2, '0') + "_" + EventDay.ToString().PadLeft(2, '0');
			EventJD = Utilities.JD_from_Date(EventYear, EventMonth, EventDay);
			UTTime = Line7;
			if (FilterOnDate && ((EventJD < FirstFilterDate) | (EventJD > LastFilterDate)))
			{
				return;
			}
			string text = Line4.Substring(9, 12);
			string text2 = Line4.Substring(21, 12);
			if (!double.TryParse(Line10.Substring(69, 2), out var result))
			{
				result = 0.0;
			}
			if (!double.TryParse(Line10.Substring(71, 2), out var result2))
			{
				result2 = 0.0;
			}
			RA = Utilities.DEGtoDMS(Utilities.DMStoDeg("  " + text) + result / 36000000.0, 2, 4, MinutesOnly: false);
			Dec = Utilities.DEGtoDMS(Utilities.DMStoDeg(" " + text2) + result2 / 3600000.0, 3, 3, MinutesOnly: false);
			DistanceToAsteroid = double.Parse(Line5.Substring(18, 8));
			deltaRA = double.Parse(Line5.Substring(26, 10));
			deltaDec = double.Parse(Line5.Substring(36, 10));
			SubstellarLongitude = double.Parse(Line6.Substring(0, 9));
			SubstellarLatitude_J2000 = double.Parse(Line6.Substring(9, 8));
			SubSolarLongitude_OfDate = double.Parse(Line6.Substring(17, 9));
			SubSolarLatitude_OfDate = double.Parse(Line6.Substring(26, 8));
			CosElong = Math.Sin(SubSolarLatitude_OfDate / (180.0 / Math.PI)) * Math.Sin(SubstellarLatitude_J2000 / (180.0 / Math.PI)) + Math.Cos(SubSolarLatitude_OfDate / (180.0 / Math.PI)) * Math.Cos(SubstellarLatitude_J2000 / (180.0 / Math.PI)) * Math.Cos((SubstellarLongitude - SubSolarLongitude_OfDate) / (180.0 / Math.PI));
			SunElong = Math.Acos(CosElong) * (180.0 / Math.PI);
			if (FilterOnSolarElongation && SunElong < FilterSolarElongation)
			{
				return;
			}
			MagLimit = 30.0;
			if (SunElong < 30.0)
			{
				MagLimit = 11.0;
			}
			if (SunElong < 20.0)
			{
				MagLimit = 9.0;
			}
			if (SunElong < 15.0)
			{
				MagLimit = 8.0;
			}
			if (SunElong < 12.0)
			{
				MagLimit = 7.0;
			}
			if (SunElong < 10.0)
			{
				MagLimit = 5.0;
			}
			MidTime = double.Parse(Line7.Substring(0, 8));
			Xatmin = double.Parse(Line7.Substring(8, 9));
			YatMin = double.Parse(Line7.Substring(17, 9));
			DeltaX = double.Parse(Line7.Substring(26, 9));
			if (DeltaX == 0.0)
			{
				DeltaX = 1E-07;
			}
			DeltaY = double.Parse(Line7.Substring(35, 9));
			Delta2X = double.Parse(Line7.Substring(44, 9));
			Delta2Y = double.Parse(Line7.Substring(53, 9));
			Delta3X = (Delta3Y = 0.0);
			MinimumSepn = Math.Sqrt(Xatmin * Xatmin + YatMin * YatMin);
			if (MinimumSepn < 0.9988)
			{
				VerticalScaleFactor = Math.Sqrt(1.0 - MinimumSepn * MinimumSepn);
			}
			else
			{
				VerticalScaleFactor = 0.05;
			}
			if (!double.TryParse(Line10.Substring(67, 2), out StellarDiameter_mas))
			{
				StellarDiameter_mas = -1E-05;
			}
			AsteroidDiameter = AsteroidShadowDiameter_Penumbral - StellarDiameter_mas / 3600000.0 / (180.0 / Math.PI) * DistanceToAsteroid * 149600000.0;
			AsteroidShadowDiameter_Umbral = AsteroidShadowDiameter_Penumbral - 2.0 * StellarDiameter_mas / 3600000.0 / (180.0 / Math.PI) * DistanceToAsteroid * 149600000.0;
			AsteroidAngularDiameterMAS = AsteroidDiameter / 725.3 / DistanceToAsteroid * 1000.0;
			n = Math.Sqrt(DeltaX * DeltaX + DeltaY * DeltaY);
			double num = 1.0 + AsteroidShadowDiameter_Penumbral / 12756.0;
			if (DistanceToAsteroid > 10.0)
			{
				EarthSemiDuration = num / n;
			}
			else if (MinimumSepn < num)
			{
				EarthSemiDuration = Math.Sqrt(num * num - MinimumSepn * MinimumSepn) / n;
			}
			else
			{
				EarthSemiDuration = 0.0;
			}
			if (MidTime - EarthSemiDuration < 0.0)
			{
				MidTime += 24.0;
				EventJD -= 1.0;
				EventDay -= 1.0;
				UTDate = Utilities.Date_from_JD(EventJD, 0) + " ";
			}
			if ((Line8.Substring(0, 1) != "P") & (A > 0.0))
			{
				if (!GeneratingSummary)
				{
					Equin = int.Parse(Line8.Substring(0, 8));
					MAnom = double.Parse(Line8.Substring(8, 10)) / (180.0 / Math.PI);
					EpochYear = int.Parse(Line8.Substring(18, 4));
					EpochMonth = int.Parse(Line8.Substring(22, 2));
					EpochDay = double.Parse(Line8.Substring(24, 8));
					OsculatingJD = Utilities.JD_from_Date(EpochYear, EpochMonth, EpochDay);
					Perih = double.Parse(Line8.Substring(32, 10)) / (180.0 / Math.PI);
					node = double.Parse(Line8.Substring(42, 10)) / (180.0 / Math.PI);
					DisplayMPOccultations.i = double.Parse(Line8.Substring(52, 10)) / (180.0 / Math.PI);
					e = double.Parse(Line8.Substring(62, 10));
					Q = double.Parse(Line8.Substring(82, 10));
					MagConst_H0 = double.Parse(Line8.Substring(92, 4));
					MagCoeff_logR = double.Parse(Line8.Substring(96, 4));
					MagSlopeConst_G = double.Parse(Line8.Substring(100, 4));
					if (MagSlopeConst_G == 0.0)
					{
						MagSlopeConst_G = 0.0001;
					}
					DeltaMAnom = double.Parse(Line9.Substring(4, 10)) / (180.0 / Math.PI);
					DeltaPerih = double.Parse(Line9.Substring(14, 9)) / (180.0 / Math.PI);
					DeltaNode = double.Parse(Line9.Substring(23, 9)) / (180.0 / Math.PI);
					DeltaI = double.Parse(Line9.Substring(32, 9)) / (180.0 / Math.PI);
					DeltaE = double.Parse(Line9.Substring(41, 10));
					DeltaA = double.Parse(Line9.Substring(51, 10));
					DeltaQ = double.Parse(Line9.Substring(61, 10));
				}
			}
			else if (IsPlanet)
			{
				PlanetPhaseAngle = 180.0 - double.Parse(Line8.Substring(9, 7));
				PlanetocentricEarthDec = double.Parse(Line8.Substring(16, 7));
				PAPlanetPole = double.Parse(Line8.Substring(23, 7));
			}
			OrbitSourceDate = "";
			ErrorBasis = "";
			StarRA_2000 = (double.Parse(RA.Substring(0, 2)) + double.Parse(RA.Substring(3, 2)) / 60.0 + double.Parse(RA.Substring(6)) / 3600.0) * 15.0 / (180.0 / Math.PI);
			StarDec_2000 = (double.Parse(Dec.Substring(1, 2)) + double.Parse(Dec.Substring(4, 2)) / 60.0 + double.Parse(Dec.Substring(7)) / 3600.0) / (180.0 / Math.PI);
			if (Dec.Substring(0, 1) == "-")
			{
				StarDec_2000 = 0.0 - StarDec_2000;
			}
			FPlaneRA = StarRA_2000;
			FPlaneDec = StarDec_2000;
			Utilities.ApparentStarPosition(ref FPlaneRA, ref FPlaneDec, 0.0, 0.0, 2000, EventJD, use2006values_Not1976: false);
			SubstellarLongitude += 180.0 / Math.PI * (FPlaneRA - StarRA_2000);
			if (SubstellarLongitude > 360.0)
			{
				SubstellarLongitude -= 360.0;
			}
			if (SubstellarLongitude < 0.0)
			{
				SubstellarLongitude += 360.0;
			}
			n = Math.Sqrt(DeltaX * DeltaX + DeltaY * DeltaY);
			MinGeocentricD = Math.Abs((Xatmin * DeltaY - YatMin * DeltaX) / n);
			MinGeocentricDMax = MinGeocentricD;
			if (MinGeocentricDMax > 0.9)
			{
				MinGeocentricDMax = 0.9;
			}
			VerticalPathScale = Math.Sqrt(1.0 - MinGeocentricDMax * MinGeocentricDMax) / 6378.137;
			Maps.SetGlobeOrientation(SubstellarLongitude / (180.0 / Math.PI), FPlaneDec);
			Maps.GlobeCoords(SiteLongitude_Rad, SiteLatitude_Rad, out xSiteLocation, out ySiteLocation, out zSiteLocation, Mirrored: false);
			X0 = Xatmin - (double)xSiteLocation;
			Y0 = YatMin - (double)ySiteLocation;
			TClosest = (0.0 - (X0 * DeltaX + Y0 * DeltaY)) / n / n;
			double num2 = 0.0;
			double num3 = 0.0;
			for (int i = 0; i <= 5; i++)
			{
				Maps.SetGlobeOrientation((SubstellarLongitude - TClosest * 15.0) / (180.0 / Math.PI), FPlaneDec);
				Maps.GlobeCoords(SiteLongitude_Rad, SiteLatitude_Rad, out xSiteLocation, out ySiteLocation, out zSiteLocation, Mirrored: false);
				X0 = Xatmin - (double)xSiteLocation + DeltaX * TClosest + Delta2X * TClosest * TClosest + Delta3X * TClosest * TClosest * TClosest;
				Y0 = YatMin - (double)ySiteLocation + DeltaY * TClosest + Delta2Y * TClosest * TClosest + Delta3Y * TClosest * TClosest * TClosest;
				num2 = DeltaX + 2.0 * Delta2X * TClosest + 3.0 * Delta3X * TClosest * TClosest;
				num3 = DeltaY + 2.0 * Delta2Y * TClosest + 3.0 * Delta3Y * TClosest * TClosest;
				n = Math.Sqrt(num2 * num2 + num3 * num3);
				double num4 = (0.0 - (X0 * num2 + Y0 * num3)) / n / n;
				TClosest += num4;
				if (Math.Abs(num4) < 0.001)
				{
					break;
				}
			}
			if (Math.Abs(TClosest) > 24.0)
			{
				return;
			}
			MinimumSepn = Math.Abs(X0 * num3 - Y0 * num2) / n;
			MinDArcSec = MinimumSepn * 8.794143836182533 / DistanceToAsteroid;
			if (FilterOnSite)
			{
				if ((double)zSiteLocation < 0.02 || (FilterOnLocalAltitude && (double)zSiteLocation < FilterLocalAltitude) || ((FilterOnKM & !IsPlanet) && MinimumSepn > FilterKM * VerticalPathScale) || ((FilterOnArcSec & !IsPlanet) && MinDArcSec > FilterArcSec))
				{
					return;
				}
				Maps.SetGlobeOrientation((SubSolarLongitude_OfDate - TClosest) / (180.0 / Math.PI), SubSolarLatitude_OfDate / (180.0 / Math.PI));
				Maps.GlobeCoords(SiteLongitude_Rad, SiteLatitude_Rad, out Xsun, out Ysun, out Zsun, Mirrored: false);
				if (Mv > 4.0 && ((double)Zsun > -0.02 || (((double)Zsun > -0.16) & ((double)zSiteLocation < 0.09)) || ((double)Zsun > -0.17 && Mv > 4.0 - 75.0 * ((double)Zsun + 0.017))))
				{
					return;
				}
			}
			ErrorMajor = (ErrorMinor = (ErrorPA = (PathAugmentation_1sigma = (Ratio_1Sigma_to_Limit_Distances = (Sigma = 0.0)))));
			double.TryParse(Line10.Substring(29, 6), out Sigma);
			if (Sigma == 0.0)
			{
				Sigma = (double)Settings.Default.AsteroidSearchDefaultUncertainty;
			}
			double.TryParse(Line10.Substring(5, 6), out ErrorMajor);
			double.TryParse(Line10.Substring(11, 6), out ErrorMinor);
			double.TryParse(Line10.Substring(17, 6), out ErrorPA);
			double.TryParse(Line10.Substring(23, 6), out PathAugmentation_1sigma);
			Ratio_1Sigma_to_Limit_Distances = 2.0 * PathAugmentation_1sigma - 1.0;
			double num5 = ErrorMajor / 8.794143836182533 * DistanceToAsteroid;
			_ = ErrorMinor / 8.794143836182533;
			_ = DistanceToAsteroid;
			ErrorInTime = num5 / n * 3600.0;
			if (!int.TryParse(Line10.Substring(35, 1), out NumOfRings))
			{
				NumOfRings = 0;
			}
			if (NumOfRings == 0)
			{
				for (int j = 0; j < 5; j++)
				{
					RingRadii[j] = 0.0;
				}
				RApole_deg = (DecPole_deg = (PlanetocentricRingLatitude = (PARingPole = 0.0)));
			}
			else
			{
				AsteroidRings_All.GetRingDetails(AsteroidNumber, out RApole_deg, out DecPole_deg, ref RingRadii, out var _);
				Utilities.PoleOrientation(RApole_deg, DecPole_deg, StarRA_2000, StarDec_2000, out PlanetocentricRingLatitude, out PARingPole);
				PlanetocentricRingLatitude /= 180.0 / Math.PI;
				PARingPole /= 180.0 / Math.PI;
			}
			if (!int.TryParse(Line10.Substring(36, 1), out NumOfMoons))
			{
				NumOfMoons = 0;
			}
			if (FilterOnBinary & (NumOfMoons == 0))
			{
				return;
			}
			if (NumOfMoons > 0)
			{
				if (!int.TryParse(Line10.Substring(37, 4), out Diameter1))
				{
					Diameter1 = 0;
				}
				if (!int.TryParse(Line10.Substring(41, 6), out Distance1))
				{
					Distance1 = 0;
				}
			}
			if (NumOfMoons == 2)
			{
				if (!int.TryParse(Line10.Substring(51, 4), out Diameter2))
				{
					Diameter2 = 0;
				}
				if (!int.TryParse(Line10.Substring(55, 6), out Distance2))
				{
					Distance2 = 0;
				}
			}
			if (Line10.Substring(63, 1) != " ")
			{
				double num6 = ((double)(int)Line10.Substring(63, 1).ToCharArray()[0] - 48.0) / 10.0;
				if (num6 > 0.0)
				{
					Mdrop = num6;
				}
			}
			if (Line10.Substring(64, 1) != " ")
			{
				double num7 = ((double)(int)Line10.Substring(64, 1).ToCharArray()[0] - 48.0) / 10.0;
				if (num7 > 0.0)
				{
					MdropRed = num7;
				}
			}
			int num8 = Convert.ToInt32(Convert.ToByte(Line10.Substring(65, 1).ToCharArray()[0])) - 48;
			if (num8 >= 0)
			{
				NearbyStarsMagAdjusted = num8 % 2 == 1;
				NearbyStarsBright = num8 / 2 % 4;
				NearbyStarsAll = num8 / 8 % 8;
			}
			else
			{
				NearbyStarsMagAdjusted = false;
				NearbyStarsBright = 0;
				NearbyStarsAll = 0;
			}
			if (!GeneratingSummary)
			{
				GetEphemerisForStarChart();
			}
			MeetsCriteria = true;
			EventHeader = "Occultation of " + StarNo.Trim() + " by " + AsteroidName.Trim() + " on " + UTDate;
			OldName = AsteroidName;
			OldStar = StarNo;
			EventFileBase = Line10.Substring(83, 15).Trim().Replace("*", "p");
			if (EventFileBase.Length < 1)
			{
				EventFileBase = UTDate.Substring(0, 4) + UTDate.Substring(5, 3) + UTDate.Substring(8, 2) + Line2.Substring(7, 5);
			}
			if (!double.TryParse(Line10.Substring(75, 8), out PredictionMJD))
			{
				PredictionMJD = 0.0;
			}
			PredictionDate = "";
			if (PredictionMJD == -1.0)
			{
				PredictionDate = "'Original' RIO_TNO prediction";
			}
			else if (PredictionMJD > 0.0)
			{
				PredictionDate = "Prediction of " + Utilities.Date_from_JD(2400000.5 + PredictionMJD, 1);
			}
		}

		internal static void MoonParameters(out double StarMoonElongation, out double MoonPhase)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double GeocentricDistance = 0.0;
			Utilities.QuickMoon(EventJD + MidTime / 24.0, out var RA2, out var Dec2, out var _, out var _, out var _);
			Utilities.QuickPlanet(EventJD + MidTime / 24.0, 3, EquinoxOfDate: true, out RA, out Dec, out GeocentricDistance);
			Utilities.Distance(RA2, Dec2, RA, Dec, out var Distance, out var PA_atOrigin);
			if (StarDec_Apparent != 0.0)
			{
				Utilities.Distance(RA2, Dec2, StarRA_Apparent, StarDec_Apparent, out StarMoonElongation, out PA_atOrigin);
			}
			else
			{
				Utilities.Distance(RA2, Dec2, StarRA_2000, StarDec_2000, out StarMoonElongation, out PA_atOrigin);
			}
			StarMoonElongation *= 180.0 / Math.PI;
			MoonPhase = 50.0 * (1.0 - Math.Cos(Distance));
		}

		internal static void Set_Data_From_OccultationElements_Record(int RecordNumber, bool PlotEvent)
		{
			if (RecordNumber < 0)
			{
				return;
			}
			try
			{
				((Form)PredictionLightCurves).Close();
			}
			catch
			{
			}
			try
			{
				((Form)AsteroidRings).Close();
			}
			catch
			{
			}
			ElementsFrom540Format = false;
			Current_OccElements_Record = RecordNumber;
			IsPlanet = OccElements[RecordNumber].IsPlanet;
			IsMoon = OccElements[RecordNumber].IsPlanetaryMoon;
			PlanetNumber = OccElements[RecordNumber].Planet;
			MoonNumber = OccElements[RecordNumber].PlanetaryMoon;
			IsPlanet = (PlanetNumber > 0) & (MoonNumber == 0);
			IsMoon = MoonNumber > 0;
			double MoonDiaUncert;
			if (IsMoon)
			{
				MoonDiameter = Satellites.SatelliteDiameter(PlanetNumber, MoonNumber, out MoonDiaUncert);
			}
			AllowSitePredictions = false;
			if ((PlanetNumber > 0) & ((MoonNumber == 0) | (MoonDiameter > 1000.0)))
			{
				AllowSitePredictions = true;
			}
			MaxDurn = OccElements[RecordNumber].MaxDurn;
			PlanetMoonInPlanetShadow = OccElements[RecordNumber].PlanetMoonInPlanetShadow;
			MvAsteroid = (MrAsteroid = (MvAsteroidUncert = (MrAsteroidUncert = 0.0)));
			Mv = OccElements[RecordNumber].mV;
			Mp = OccElements[RecordNumber].mB;
			if ((Mp == 0.0) | (Mp > 20.0))
			{
				Mp = 90.0;
			}
			Mr = OccElements[RecordNumber].mR;
			MAsteroid = OccElements[RecordNumber].MagAsteroid;
			MvAsteroid = OccElements[RecordNumber].MagVAsteroid;
			MrAsteroid = OccElements[RecordNumber].MagRAsteroid;
			Mdrop = OccElements[RecordNumber].MagDropV;
			MdropRed = OccElements[RecordNumber].MagDropR;
			NearbyStarsBright = OccElements[RecordNumber].NearbyStars_Bright;
			NearbyStarsAll = OccElements[RecordNumber].NearbyStars_All;
			NearbyStarsMagAdjusted = OccElements[RecordNumber].NearbyStars_MagAdjusted;
			Reliability = OccElements[RecordNumber].StarReliability;
			DuplicateSource = OccElements[RecordNumber].DuplicateSource;
			NonGaiaPM = OccElements[RecordNumber].NoGaiaPM;
			SatelliteOrbitAvailable = Settings.Default.MiriadeAsteroids.Contains(" " + OccElements[RecordNumber].ObjectNumber.Trim() + " ");
			UTTime = "not needed?";
			EventJD = OccElements[RecordNumber].EventJD;
			EventYear = OccElements[RecordNumber].EventYear;
			EventMonth = OccElements[RecordNumber].EventMonth;
			EventDay = OccElements[RecordNumber].EventDay;
			UTDate = EventYear + " " + Utilities.ShortMonths[EventMonth] + " " + EventDay;
			UTDate_Numeric = EventYear + "_" + EventMonth.ToString().PadLeft(2, '0') + "_" + EventDay.ToString().PadLeft(2, '0');
			StarNo = OccElements[RecordNumber].StarCatName + OccElements[RecordNumber].StarIdentifier;
			RA = Utilities.DEGtoDMS(OccElements[RecordNumber].RA_Star2000 * (180.0 / Math.PI) / 15.0, 2, 4, MinutesOnly: false);
			Dec = Utilities.DEGtoDMS(OccElements[RecordNumber].Dec_Star2000 * (180.0 / Math.PI), 3, 3, MinutesOnly: false);
			DoubleStarCode = OccElements[RecordNumber].DoubleStarCode;
			IsKepler2Star = OccElements[RecordNumber].IsKepler2Star;
			ObjectID = OccElements[RecordNumber].ObjectNumber;
			ObjectName = OccElements[RecordNumber].ObjectName;
			AsteroidName = OccElements[RecordNumber].ObjectNumber + " " + OccElements[RecordNumber].ObjectName;
			if (!int.TryParse(OccElements[RecordNumber].ObjectNumber, out AsteroidNumber))
			{
				AsteroidNumber = 0;
			}
			StellarDiameter_mas = OccElements[RecordNumber].StarDiamMAS;
			AsteroidShadowDiameter_Penumbral = OccElements[RecordNumber].AsteroidShadowDiameter_Penumbral;
			AsteroidShadowDiameter_Umbral = AsteroidShadowDiameter_Penumbral - 2.0 * StellarDiameter_mas / 3600000.0 / (180.0 / Math.PI) * OccElements[RecordNumber].DistAsteroid * 149600000.0;
			AsteroidDiameter = AsteroidShadowDiameter_Penumbral - StellarDiameter_mas / 3600000.0 / (180.0 / Math.PI) * OccElements[RecordNumber].DistAsteroid * 149600000.0;
			DiameterUncertainty = OccElements[RecordNumber].AsteroidDiameterUncertainty;
			AsteroidAngularDiameterMAS = OccElements[RecordNumber].AsteroidDiameterArcSec * 1000.0;
			deltaRA = OccElements[RecordNumber].hourly_dRA;
			deltaDec = OccElements[RecordNumber].hourly_dDec;
			MidTime = OccElements[RecordNumber].MidTime_Hrs;
			Xatmin = OccElements[RecordNumber].XatMin;
			YatMin = OccElements[RecordNumber].YatMin;
			DeltaX = OccElements[RecordNumber].dX;
			DeltaY = OccElements[RecordNumber].dY;
			Delta2X = OccElements[RecordNumber].d2X;
			Delta2Y = OccElements[RecordNumber].d2Y;
			Delta3X = OccElements[RecordNumber].d3X;
			Delta3Y = OccElements[RecordNumber].d3Y;
			MinimumSepn = Math.Sqrt(Xatmin * Xatmin + YatMin * YatMin);
			if (MinimumSepn < 0.9988)
			{
				VerticalScaleFactor = Math.Sqrt(1.0 - MinimumSepn * MinimumSepn);
			}
			else
			{
				VerticalScaleFactor = 0.05;
			}
			n = Math.Sqrt(DeltaX * DeltaX + DeltaY * DeltaY);
			double num = 1.0 + AsteroidShadowDiameter_Penumbral / 12756.0;
			if (DistanceToAsteroid > 10.0)
			{
				EarthSemiDuration = num / n;
			}
			else if (MinimumSepn < num)
			{
				EarthSemiDuration = Math.Sqrt(num * num - MinimumSepn * MinimumSepn) / n;
			}
			else
			{
				EarthSemiDuration = 0.0;
			}
			StarRA_2000 = OccElements[RecordNumber].RA_Star2000;
			StarDec_2000 = OccElements[RecordNumber].Dec_Star2000;
			StarRA_Apparent = OccElements[RecordNumber].RA_Star_Apparent;
			StarDec_Apparent = OccElements[RecordNumber].Dec_Star_Apparent;
			if (OccElements[RecordNumber].UsesApparentStar)
			{
				FPlaneRA = StarRA_Apparent;
				FPlaneDec = StarDec_Apparent;
				SubstellarLongitude = OccElements[RecordNumber].SubstellarLongitude_OfDate_deg;
			}
			else
			{
				FPlaneRA = StarRA_2000;
				FPlaneDec = StarDec_2000;
				Utilities.ApparentStarPosition(ref FPlaneRA, ref FPlaneDec, 0.0, 0.0, 2000, EventJD, use2006values_Not1976: false);
				StarRA_Apparent = FPlaneRA;
				StarDec_Apparent = FPlaneDec;
				SubstellarLongitude = OccElements[RecordNumber].SubstellarLongitude_J2000_deg;
				SubstellarLongitude += 180.0 / Math.PI * (StarRA_Apparent - StarRA_2000);
				if (SubstellarLongitude > 360.0)
				{
					SubstellarLongitude -= 360.0;
				}
				if (SubstellarLongitude < 0.0)
				{
					SubstellarLongitude += 360.0;
				}
			}
			MinGeocentricD = Math.Abs((Xatmin * DeltaY - YatMin * DeltaX) / n);
			MinGeocentricDMax = MinGeocentricD;
			if (MinGeocentricDMax > 0.9)
			{
				MinGeocentricDMax = 0.9;
			}
			VerticalPathScale = Math.Sqrt(1.0 - MinGeocentricDMax * MinGeocentricDMax) / 6378.137;
			Maps.SetGlobeOrientation(SubstellarLongitude / (180.0 / Math.PI), FPlaneDec);
			Maps.GlobeCoords(SiteLongitude_Rad, SiteLatitude_Rad, out xSiteLocation, out ySiteLocation, out zSiteLocation, Mirrored: false);
			X0 = Xatmin - (double)xSiteLocation;
			Y0 = YatMin - (double)ySiteLocation;
			TClosest = (0.0 - (X0 * DeltaX + Y0 * DeltaY)) / n / n;
			double num2 = 0.0;
			double num3 = 0.0;
			for (int i = 0; i <= 5; i++)
			{
				Maps.SetGlobeOrientation((SubstellarLongitude - TClosest * 15.0) / (180.0 / Math.PI), FPlaneDec);
				Maps.GlobeCoords(SiteLongitude_Rad, SiteLatitude_Rad, out xSiteLocation, out ySiteLocation, out zSiteLocation, Mirrored: false);
				X0 = Xatmin - (double)xSiteLocation + DeltaX * TClosest + Delta2X * TClosest * TClosest + Delta3X * TClosest * TClosest * TClosest;
				Y0 = YatMin - (double)ySiteLocation + DeltaY * TClosest + Delta2Y * TClosest * TClosest + Delta3Y * TClosest * TClosest * TClosest;
				num2 = DeltaX + 2.0 * Delta2X * TClosest + 3.0 * Delta3X * TClosest * TClosest;
				num3 = DeltaY + 2.0 * Delta2Y * TClosest + 3.0 * Delta3Y * TClosest * TClosest;
				n = Math.Sqrt(num2 * num2 + num3 * num3);
				double num4 = (0.0 - (X0 * num2 + Y0 * num3)) / n / n;
				TClosest += num4;
				if (Math.Abs(num4) < 0.001)
				{
					break;
				}
			}
			if (Math.Abs(TClosest) > 24.0)
			{
				return;
			}
			MinimumSepn = Math.Abs(X0 * num3 - Y0 * num2) / n;
			MinDArcSec = MinimumSepn * 8.794143836182533 / DistanceToAsteroid;
			SubstellarLatitude_J2000 = OccElements[RecordNumber].Dec_Star2000 * (180.0 / Math.PI);
			SubSolarLongitude_OfDate = OccElements[RecordNumber].SubSolarLongitude_OfDate_deg;
			SubSolarLatitude_OfDate = OccElements[RecordNumber].SubSolarLatitude_OfDate_deg;
			SunElong = OccElements[RecordNumber].SolarElongation;
			StarMoonElongation_deg = OccElements[RecordNumber].StarMoonElongation_deg;
			MoonPhase_percent = OccElements[RecordNumber].MoonPhase_percent;
			DistanceToAsteroid = OccElements[RecordNumber].DistAsteroid;
			Equin = (int)OccElements[RecordNumber].Equinox;
			MAnom = OccElements[RecordNumber].MeanAnomaly;
			EpochYear = OccElements[RecordNumber].EpochYear;
			EpochMonth = OccElements[RecordNumber].EpochMonth;
			EpochDay = OccElements[RecordNumber].EpochDay;
			OsculatingJD = Utilities.JD_from_Date(EpochYear, EpochMonth, EpochDay);
			Perih = OccElements[RecordNumber].Perihelion;
			node = OccElements[RecordNumber].Node;
			DisplayMPOccultations.i = OccElements[RecordNumber].i;
			e = OccElements[RecordNumber].e;
			A = OccElements[RecordNumber].a;
			Q = OccElements[RecordNumber].q;
			MagConst_H0 = OccElements[RecordNumber].MagConst_H0;
			MagCoeff_logR = OccElements[RecordNumber].MagCoeff_logR;
			MagSlopeConst_G = OccElements[RecordNumber].MagSlopeConst_G;
			if (MagSlopeConst_G == 0.0)
			{
				MagSlopeConst_G = 0.0001;
			}
			if (((MvAsteroid < -10.0) | (MvAsteroid == 0.0)) && AsteroidNumber > 0 && MagValues.AsteroidMagnitudeFileExists)
			{
				Utilities.QuickSolarElongation_PhaseAngle(EventJD, StarRA_2000, StarDec_2000, DistanceToAsteroid, out MoonDiaUncert, out PhaseAngle, out var SolarDistObject);
				int colorMagnitudeRecord = MagValues.GetColorMagnitudeRecord(AsteroidNumber);
				if (colorMagnitudeRecord >= 0)
				{
					MagValues.ColorMagnitudeData[colorMagnitudeRecord].GetColorMagnitudes(StarRA_2000, StarDec_2000, SolarDistObject, DistanceToAsteroid, PhaseAngle / (180.0 / Math.PI), out MvAsteroid, out MrAsteroid, out MvAsteroidUncert, out MrAsteroidUncert);
				}
				else
				{
					MvAsteroid = (MrAsteroid = -5.0);
				}
			}
			DeltaMAnom = OccElements[RecordNumber].delta_MeanAnomaly;
			DeltaPerih = OccElements[RecordNumber].delta_Perihelion;
			DeltaNode = OccElements[RecordNumber].delta_Node;
			DeltaI = OccElements[RecordNumber].delta_i;
			DeltaE = OccElements[RecordNumber].delta_e;
			DeltaA = OccElements[RecordNumber].delta_a;
			DeltaQ = OccElements[RecordNumber].delta_q;
			ErrorMajor = OccElements[RecordNumber].ErrorEllipseMajorAxis;
			ErrorMinor = OccElements[RecordNumber].ErrorEllipseMinorAxis;
			ErrorPA = OccElements[RecordNumber].ErrorEllipsePA;
			double num5 = ErrorMajor / 8.794143836182533 * DistanceToAsteroid;
			_ = ErrorMinor / 8.794143836182533;
			_ = DistanceToAsteroid;
			ErrorInTime = num5 / n * 3600.0;
			PathAugmentation_1sigma = OccElements[RecordNumber].Error_AsIncreaseInPathWidths;
			Ratio_1Sigma_to_Limit_Distances = 2.0 * PathAugmentation_1sigma - 1.0;
			Sigma = OccElements[RecordNumber].KnownSigma_StarAsterPosns;
			AsteroidClass = OccElements[RecordNumber].AsteroidClass;
			NumOfMoons = OccElements[RecordNumber].NumAsteroidMoons;
			if (OccElements[RecordNumber].AsteroidMoons.Count > 0)
			{
				MoonName1 = OccElements[RecordNumber].AsteroidMoons[0].IDMoonName;
				Diameter1 = (int)OccElements[RecordNumber].AsteroidMoons[0].Diameter;
				Distance1 = (int)OccElements[RecordNumber].AsteroidMoons[0].A;
			}
			if (OccElements[RecordNumber].AsteroidMoons.Count > 1)
			{
				MoonName2 = OccElements[RecordNumber].AsteroidMoons[1].IDMoonName;
				Diameter2 = (int)OccElements[RecordNumber].AsteroidMoons[1].Diameter;
				Distance2 = (int)OccElements[RecordNumber].AsteroidMoons[1].A;
			}
			if (IsPlanet & !IsMoon)
			{
				PlanetPhaseAngle = OccElements[RecordNumber].PlanetPhaseAngle;
				PlanetocentricEarthDec = OccElements[RecordNumber].PlanetocentricEarthDec;
				PAPlanetPole = OccElements[RecordNumber].PAPlanetPole;
			}
			NumOfRings = OccElements[RecordNumber].NumAsteroidRings;
			if (NumOfRings == 0)
			{
				for (int j = 0; j < 5; j++)
				{
					RingRadii[j] = 0.0;
				}
				RApole_deg = (DecPole_deg = (PlanetocentricRingLatitude = (PARingPole = 0.0)));
				PathAugmentation_Rings = 0.0;
				AllowAsteroidRings = false;
			}
			else
			{
				AsteroidRings_All.GetRingDetails(AsteroidNumber, out RApole_deg, out DecPole_deg, ref RingRadii, out var NumberOfRings);
				Utilities.PoleOrientation(RApole_deg, DecPole_deg, StarRA_2000, StarDec_2000, out PlanetocentricRingLatitude, out PARingPole);
				PlanetocentricRingLatitude /= 180.0 / Math.PI;
				double num6 = RingRadii[0];
				for (int k = 1; k < NumberOfRings; k++)
				{
					if (RingRadii[k] > num6)
					{
						num6 = RingRadii[k];
					}
				}
				PathAugmentation_Rings = ErrorAcrossPath(DeltaX, DeltaY, num6, Math.Abs(num6 * Math.Sin(PlanetocentricRingLatitude)), PARingPole + 90.0) / (OccElements[RecordNumber].AsteroidDiameter / 2.0);
				PARingPole /= 180.0 / Math.PI;
				AllowAsteroidRings = true;
			}
			OrbitSourceDate = OccElements[RecordNumber].OrbitSource;
			int num7 = OrbitSourceDate.IndexOf("@");
			if (num7 > 0)
			{
				OrbitSourceDate = OrbitSourceDate.Substring(0, num7);
			}
			RIO_TNO_event = OrbitSourceDate.Contains("RIO") | OrbitSourceDate.Contains("Lucky");
			ErrorBasis = OccElements[RecordNumber].ErrorBasis;
			if (AsteroidNumber > 0)
			{
				InISAM = IsInISAM(AsteroidNumber);
				InDAMIT = IsInDAMIT(AsteroidNumber);
			}
			GetEphemerisForStarChart();
			PredictionMJD = OccElements[RecordNumber].PredictionMJD;
			PredictionDate = "";
			if (PredictionMJD == -1.0)
			{
				PredictionDate = "'Original' RIO_TNO prediction";
			}
			else if (PredictionMJD > 0.0)
			{
				PredictionDate = "Prediction of " + Utilities.Date_from_JD(2400000.5 + PredictionMJD, 1);
			}
			Utilities.Date_from_MJD(PredictionMJD, out var Year, out var Month, out var day);
			string text = Year + "-" + Month.ToString().PadLeft(2, '0') + "-" + ((int)day).ToString().PadLeft(2, '0');
			if (OrbitSourceDate.Contains("Lucky"))
			{
				if (OrbitSourceDate.ToUpper().Contains("NIMA"))
				{
					OrbitSourceDate = "LS-NIMA:" + text;
				}
				else
				{
					OrbitSourceDate = "LuckyStar:" + text;
				}
			}
			else if (OrbitSourceDate.Contains("RIO"))
			{
				int num8 = OrbitSourceDate.LastIndexOf("_");
				string text2 = "";
				if (num8 > 0)
				{
					text2 = OrbitSourceDate.Substring(num8 + 1).Trim();
				}
				num8 = text2.IndexOf("/");
				if (num8 > 1)
				{
					text2 = text2.Substring(0, num8);
				}
				OrbitSourceDate = "RIO-" + text2 + ":" + text;
			}
			EventHeader = "Occultation of " + StarNo.Trim() + " by " + AsteroidName.Trim() + " on " + UTDate;
			EventFileBase = OccElements[RecordNumber].EventID.Trim();
			PredictionDate = "Prediction of " + Utilities.Date_from_JD(2400000.5 + PredictionMJD, 1);
			if (OccElements[RecordNumber].ForJWST || !PlotEvent)
			{
				return;
			}
			try
			{
				((Control)Plot).Show();
			}
			catch
			{
				Plot = new AsteroidPlotPath();
				((Control)Plot).Show();
			}
			Plot.colourToolStripMenuItem.set_Checked(!BWFlag);
			((ToolStripItem)Plot.plotToolStripMenuItem).set_Enabled(true);
			AsteroidPlotPath.SetScaleCheckmarks((int)(2.0 + Math.Log10(PlotMagnification_Initial) / 0.3));
			Plot.ReSetExportButtons();
			ToolStripMenuItem planetContactTimesForMultipleLocationsToolStripMenuItem = Plot.planetContactTimesForMultipleLocationsToolStripMenuItem;
			bool allowSitePredictions;
			((ToolStripItem)Plot.contactTimesForMultipleLocationsToolStripMenuItem).set_Enabled(allowSitePredictions = AllowSitePredictions);
			((ToolStripItem)planetContactTimesForMultipleLocationsToolStripMenuItem).set_Enabled(allowSitePredictions);
			ToolStripMenuItem asteroidRingsContactTimesForMultipleLocationsToolStripMenuItem = Plot.asteroidRingsContactTimesForMultipleLocationsToolStripMenuItem;
			((ToolStripItem)Plot.asteroidRingsContactTimesForMultipleLocdationsToolStripMenuItem).set_Enabled(allowSitePredictions = AllowAsteroidRings);
			((ToolStripItem)asteroidRingsContactTimesForMultipleLocationsToolStripMenuItem).set_Enabled(allowSitePredictions);
			UseRedrawSites = true;
			UseSetErrorLocation = false;
			Plot.updnLongitude.set_Value((decimal)(OccultationElements.SiteLongitudeTest * (180.0 / Math.PI)));
			Plot.updnLatitude.set_Value((decimal)(OccultationElements.SiteLatitudeTest * (180.0 / Math.PI)));
			RedrawLongitude = OccultationElements.SiteLongitudeTest * (180.0 / Math.PI);
			RedrawLatitude = OccultationElements.SiteLatitudeTest * (180.0 / Math.PI);
			PlotPath();
			if (global::GaiaDoubles.GaiaDoubles.GaiaDouble_Match(StarRA_2000 * (180.0 / Math.PI), StarDec_2000 * (180.0 / Math.PI), GaiaCatIDOnly: false, DetailsOnly: false, out var FullDetails))
			{
				try
				{
					((Control)Non_Single_Gaia).Show();
				}
				catch
				{
					Non_Single_Gaia = new DisplayData();
					((Control)Non_Single_Gaia).set_Width((int)((double)FullDetails.Length * 2.5 + 20.0));
					((Control)Non_Single_Gaia).set_Height(150);
					((Control)Non_Single_Gaia).set_BackColor(Color.Aquamarine);
					((Control)Non_Single_Gaia).Show();
				}
				((Control)Non_Single_Gaia.txtBox).set_Text(FullDetails);
				((Control)Non_Single_Gaia).set_Text("Gaia Non-Single-Star entry found for : " + EventDetails.StarCat + " " + EventDetails.StarNumber);
				((Control)Non_Single_Gaia).set_ForeColor(Color.DarkRed);
				return;
			}
			try
			{
				((Form)Non_Single_Gaia).Close();
			}
			catch
			{
			}
		}

		internal static void CopyCurrentOccultationElements()
		{
			if (Current_OccElements_Record < 0)
			{
				Clipboard.SetText("Occultation elements are not available for events from OccultWatcher");
				return;
			}
			try
			{
				Clipboard.SetText(OccElements[Current_OccElements_Record].XML_Elements());
			}
			catch
			{
			}
		}

		internal static void GetEphemerisForStarChart()
		{
			StarChart_EphemTimeStep = 1.0;
			if (Mv > 13.0)
			{
				StarChart_EphemTimeStep = 0.04166667;
			}
			else if (Mv > 12.0)
			{
				StarChart_EphemTimeStep = 0.125;
			}
			else if (Mv > 11.0)
			{
				StarChart_EphemTimeStep = 0.25;
			}
			if (!IsPlanet & !IsMoon)
			{
				if (A > 0.0)
				{
					for (double num = -9.0; num <= 2.0; num += 1.0)
					{
						NowDate = EventJD + MidTime / 24.0 + num * StarChart_EphemTimeStep - StarChart_EphemTimeStep / 2.0;
						Utilities.PositionfromElements(NowDate, 0.0, 0.0, 0.0, 0.0, OsculatingJD, MAnom, Q, e, Perih, node, i, MagConst_H0, MagSlopeConst_G, MagCoeff_logR, 1E-05, out PlotRAAsteroid[(int)num + 9], out PlotDecAsteroid[(int)num + 9], out RAsteroid, out DistAsteroid, out magAsteroid, out var _, out PhaseAngle);
					}
				}
				return;
			}
			string MoonName = "";
			float MoonDiaKm = 0f;
			float Mag = 0f;
			for (double num2 = -9.0; num2 <= 2.0; num2 += 1.0)
			{
				NowDate = EventJD + num2;
				if (IsMoon)
				{
					Satellites.SatelliteCoordinates(NowDate, PlanetNumber, MoonNumber, ref MoonName, ref MoonDiaKm, ref PlotRAAsteroid[(int)(num2 + 9.0)], ref PlotDecAsteroid[(int)(num2 + 9.0)], ref Mag);
				}
				else
				{
					Utilities.PlanetGeocentric(NowDate, PlanetNumber, 0.0, 2, out PlotRAAsteroid[(int)(num2 + 9.0)], out PlotDecAsteroid[(int)(num2 + 9.0)], out var _);
				}
			}
		}

		private static string CreateEventFileBase(int RecordNum)
		{
			string text = OccElements[RecordNum].EventID.Replace("*", "p");
			if (text.Length < 1)
			{
				text = OccElements[RecordNum].EventYear + OccElements[RecordNum].EventMonth.ToString().PadLeft(2, '0') + OccElements[RecordNum].EventDay.ToString().PadLeft(2, '0') + OccElements[RecordNum].ObjectName.PadRight(5).Substring(0, 5);
			}
			return text;
		}

		private static string CreateEventFileBase(string StarCatName, int year, int month, int day)
		{
			string text = StarCatName.Trim().PadLeft(6).Replace(' ', '_');
			return year + month.ToString().PadLeft(2, '0') + day.ToString().PadLeft(2, '0') + "_" + text.Substring(text.Length - 7, 7);
		}

		public static void CreateSummaryOfEvents(bool Global, bool PlanetTimes, bool SaveElements)
		{
			string path = AppPath + "\\Generated Files\\OccelmntSubsetForOW.dat";
			if (!OpenOccultationElementFile(DWD: false, ""))
			{
				return;
			}
			if (!Global)
			{
				new StreamWriter(AppPath + "\\Generated Files\\multipath.tmp", append: false).Close();
			}
			EventOut = new List<AsteroidSummaryLine>();
			try
			{
				((Control)SummaryForm).Show();
			}
			catch
			{
				SummaryForm = new AsteroidSummary();
				((Control)SummaryForm).Show();
			}
			SummaryForm.PBarSummary.set_Minimum(0);
			SummaryForm.PBarSummary.set_Value(0);
			SummaryForm.PBarSummary.set_Maximum((int)RecordsInFile);
			((Control)SummaryForm.PBarSummary).set_Visible(true);
			((Control)SummaryForm).Focus();
			if (SaveElements && File.Exists(path))
			{
				File.Delete(path);
			}
			for (int i = 0; i < RecordsInFile; i++)
			{
				Get_Data_From_540Record(i, out var MeetsCriteria, GeneratingSummary: true, AutoPlot: false);
				SummaryForm.PBarSummary.set_Value(i);
				if (!MeetsCriteria)
				{
					continue;
				}
				AsteroidSummaryLine asteroidSummaryLine = new AsteroidSummaryLine();
				asteroidSummaryLine.IsPlanetaryMoon = IsMoon;
				asteroidSummaryLine.IsPlanet = IsPlanet;
				asteroidSummaryLine.PlanetTimes = PlanetTimes;
				asteroidSummaryLine.Global = Global;
				double num = MidTime + TClosest;
				double num2 = Math.Floor(num);
				double eventMin = 60.0 * (num - num2);
				asteroidSummaryLine.JDEventDate = EventJD + num / 24.0;
				asteroidSummaryLine.UTDate = UTDate;
				asteroidSummaryLine.EventHour = num2;
				asteroidSummaryLine.EventMin = eventMin;
				asteroidSummaryLine.AsteroidDiameter = AsteroidDiameter;
				asteroidSummaryLine.DiameterUncertainty = DiameterUncertainty;
				asteroidSummaryLine.AsteroidDiameterArcSec = AsteroidDiameter / 725.3 / DistanceToAsteroid;
				asteroidSummaryLine.ErrorEarthRadii = ErrorMajor / 8.794143836182533 * DistanceToAsteroid;
				asteroidSummaryLine.MinEarthSep = Math.Sqrt(Xatmin * Xatmin + YatMin * YatMin);
				asteroidSummaryLine.Mv = Mv;
				asteroidSummaryLine.MagCombined = Utilities.CombinedMagnitude(Mv, MAsteroid);
				asteroidSummaryLine.SunElong = SunElong;
				asteroidSummaryLine.MoonElongation_deg = StarMoonElongation_deg;
				asteroidSummaryLine.MoonIllumination_percent = MoonPhase_percent;
				asteroidSummaryLine.MaxDurn = MaxDurn;
				asteroidSummaryLine.Mdrop = Mdrop;
				if (MdropRed < 20.0)
				{
					asteroidSummaryLine.MdropRed = MdropRed;
				}
				else
				{
					asteroidSummaryLine.MdropRed = Mdrop;
				}
				asteroidSummaryLine.PlanetIllum = 50.0 * (1.0 - Math.Cos(PlanetPhaseAngle / (180.0 / Math.PI)));
				asteroidSummaryLine.StarNo = StarNo;
				asteroidSummaryLine.DoubleStarFlag = DoubleStarCode;
				if (IsKepler2Star)
				{
					asteroidSummaryLine.DoubleStarFlag = 16;
				}
				if (StellarDiameter_mas > 0.0)
				{
					asteroidSummaryLine.DoubleStarFlag = DoubleStarCode + 17;
				}
				string text = AsteroidName.Substring(0, 6);
				if (text.Trim().Length > 0)
				{
					asteroidSummaryLine.AsteroidNumber = text;
				}
				else
				{
					asteroidSummaryLine.AsteroidNumber = "";
				}
				asteroidSummaryLine.AsteroidName = AsteroidName.Substring(7);
				asteroidSummaryLine.Rings = NumOfRings;
				asteroidSummaryLine.Moons = NumOfMoons;
				asteroidSummaryLine.AsteroidClass = AsteroidClass;
				asteroidSummaryLine.HasShapeModel = IsInDAMIT(AsteroidNumber) | IsInISAM(AsteroidNumber);
				asteroidSummaryLine.RADec = "  " + RA.Substring(0, 12) + " " + Dec.Substring(0, 12);
				asteroidSummaryLine.RecordNumber = i;
				asteroidSummaryLine.ForJWST = ForJWST;
				if (PlanetTimes)
				{
					if ((IsPlanet & !IsMoon) && Planet_Local_Event_Times(SiteLongitude_Rad, SiteLatitude_Rad, MultiSite: false, out var PlanetOut))
					{
						asteroidSummaryLine.PlanetEventTimes = PlanetOut;
						EventOut.Add(asteroidSummaryLine);
					}
				}
				else
				{
					if (!Global)
					{
						asteroidSummaryLine.Alt = Math.Asin(zSiteLocation) * (180.0 / Math.PI);
						if (FilterOnArcSec)
						{
							asteroidSummaryLine.MinD = MinimumSepn * 8.794143836182533 / DistanceToAsteroid;
						}
						else
						{
							asteroidSummaryLine.Dkm = Math.Abs(MinimumSepn) / VerticalPathScale;
						}
						asteroidSummaryLine.SunAlt = Math.Asin(Zsun) * (180.0 / Math.PI);
						double sigma = 6378.137 * Sigma / 8.794143836182533 * DistanceToAsteroid / VerticalScaleFactor;
						asteroidSummaryLine.Probability = Probability(AsteroidDiameter / 2.0 / VerticalScaleFactor, sigma, Math.Abs(MinimumSepn) / VerticalPathScale);
						AsteroidPath_For_Plots(1.0, Multipath: true);
					}
					else if (IsPlanet)
					{
						double num3 = SubstellarLongitude - SubSolarLongitude_OfDate;
						if (num3 < -180.0)
						{
							num3 += 360.0;
						}
						if (num3 > 180.0)
						{
							num3 -= 360.0;
						}
						if (num3 > 0.0)
						{
							asteroidSummaryLine.SunL90 = Utilities.NormaliseDegrees(SubSolarLongitude_OfDate + 90.0);
							asteroidSummaryLine.L90 = Utilities.NormaliseDegrees(SubstellarLongitude + 90.0);
						}
						else
						{
							asteroidSummaryLine.SunL90 = Utilities.NormaliseDegrees(SubstellarLongitude + 90.0);
							asteroidSummaryLine.L90 = Utilities.NormaliseDegrees(SubSolarLongitude_OfDate + 90.0);
						}
					}
					EventOut.Add(asteroidSummaryLine);
				}
				if (SaveElements)
				{
					StreamWriter streamWriter;
					using (streamWriter = new StreamWriter(path, append: true))
					{
						streamWriter.Write(OccElements[i].ElementsIn540Format);
					}
				}
			}
			((Control)SummaryForm.PBarSummary).set_Visible(false);
			Application.DoEvents();
			Elements.Close();
			if (!PlanetTimes)
			{
				AsteroidSummary.SummaryHeader = "Event Summary for Longitude " + SiteLongitudeText + "   Latitude " + SiteLatitudeText;
				AsteroidSummary.Global = Global;
				AsteroidSummary.DistInArcSec = FilterOnArcSec;
				((ToolStripItem)SummaryForm.byDistanceToolStripMenuItem).set_Enabled(!Global);
				((ToolStripItem)SummaryForm.byProbabilityToolStripMenuItem).set_Enabled(!Global);
				((ToolStripItem)SummaryForm.abbreviatedOutputToolStripMenuItem).set_Enabled(true);
				((ToolStripItem)SummaryForm.plotPathsOnAMapToolStripMenuItem).set_Enabled(true);
				((ToolStripItem)SummaryForm.createGoogleKMLFileOfEventsToolStripMenuItem).set_Enabled(true);
			}
			else
			{
				AsteroidSummary.SummaryHeader = "Planets: local times summary for Longitude " + SiteLongitudeText + "   Latitude " + SiteLatitudeText;
				((ToolStripItem)SummaryForm.abbreviatedOutputToolStripMenuItem).set_Enabled(false);
				((ToolStripItem)SummaryForm.plotPathsOnAMapToolStripMenuItem).set_Enabled(false);
				((ToolStripItem)SummaryForm.createGoogleKMLFileOfEventsToolStripMenuItem).set_Enabled(false);
			}
			SummaryForm.PlanetTimes = PlanetTimes;
			SummaryForm.DefaultSort();
		}

		public static void DisplayEventFromSummary(int RecNum)
		{
			if (MultiLocations != null)
			{
				((Form)MultiLocations).Close();
			}
			if (ContactTimes != null)
			{
				((Form)ContactTimes).Close();
			}
			if (PathCoordsForm != null)
			{
				((Form)PathCoordsForm).Close();
			}
			if (Chart != null)
			{
				((Form)Chart).Close();
			}
			if (PlanetConfiguration != null)
			{
				((Form)PlanetConfiguration).Close();
			}
			if (PrePoint != null)
			{
				((Form)PrePoint).Close();
			}
			if (BinaryAsteroids != null)
			{
				((Form)BinaryAsteroids).Close();
			}
			if (AsteroidRings != null)
			{
				((Form)AsteroidRings).Close();
			}
			if (StarDiameter != null)
			{
				((Form)StarDiameter).Close();
			}
			if (Relativity != null)
			{
				((Form)Relativity).Close();
			}
			OldName = "";
			OldStar = "";
			ElementsStream = new FileStream(FileNameIn, FileMode.Open, FileAccess.Read);
			Elements = new BinaryReader(ElementsStream);
			ElementFileLength = ElementsStream.Length;
			RecordsInFile = ElementFileLength / 540;
			Get_Data_From_540Record(RecNum, out var _, GeneratingSummary: false, AutoPlot: false);
			Elements.Close();
			try
			{
				((Control)Plot).Show();
			}
			catch
			{
				Plot = new AsteroidPlotPath();
				((Control)Plot).Show();
			}
			Plot.colourToolStripMenuItem.set_Checked(!BWFlag);
			((ToolStripItem)Plot.plotToolStripMenuItem).set_Enabled(true);
			AsteroidPlotPath.SetScaleCheckmarks((int)(2.0 + Math.Log10(PlotMagnification_Initial) / 0.3));
			Plot.ReSetExportButtons();
			PlotMagnification = PlotMagnification_Initial;
			ToolStripMenuItem planetContactTimesForMultipleLocationsToolStripMenuItem = Plot.planetContactTimesForMultipleLocationsToolStripMenuItem;
			bool allowSitePredictions;
			((ToolStripItem)Plot.contactTimesForMultipleLocationsToolStripMenuItem).set_Enabled(allowSitePredictions = AllowSitePredictions);
			((ToolStripItem)planetContactTimesForMultipleLocationsToolStripMenuItem).set_Enabled(allowSitePredictions);
			UseRedrawSites = false;
			UseSetErrorLocation = false;
			PlotPath();
		}

		internal static double Probability(double HalfPathWidth, double Sigma, double Distance)
		{
			if (Sigma == 0.0)
			{
				Sigma = 1E-09;
			}
			return Math.Abs(100.0 * dblNormalPDF((Distance + HalfPathWidth) / Sigma) - 100.0 * dblNormalPDF((Distance - HalfPathWidth) / Sigma));
		}

		private static double dblNormalPDF(double X)
		{
			double[] array = new double[6] { 0.0, 0.31938153, -0.356563782, 1.781477937, -1.821255978, 1.330274429 };
			if (X < -7.0)
			{
				return dblNormalDF(X) / Math.Sqrt(1.0 + X * X);
			}
			if (X > 7.0)
			{
				return 1.0 - dblNormalPDF(0.0 - X);
			}
			double num = 0.2316419;
			num = 1.0 / (1.0 + num * Math.Abs(X));
			num = 1.0 - dblNormalDF(X) * (array[1] * num + array[2] * num * num + array[3] * num * num * num + array[4] * num * num * num * num + array[5] * num * num * num * num * num);
			if (X <= 0.0)
			{
				num = 1.0 - num;
			}
			return num;
		}

		private static double dblNormalDF(double X)
		{
			return 0.398942280401433 * Math.Exp((0.0 - X) * X * 0.5);
		}

		internal static void Intersection(float X, float Y, float OldX, float OldY, float Side, out float IntersectX, out float IntersectY)
		{
			float num = Side * (float)Math.Sign(OldX);
			float num2 = Side * (float)Math.Sign(OldY);
			float num3 = X - OldX;
			float num4 = Y - OldY;
			if (num3 == 0f)
			{
				num3 = 1E-06f;
			}
			if (num4 == 0f)
			{
				num4 = 1E-06f;
			}
			float num5 = (num - OldX) / num3;
			float num6 = (num2 - OldY) / num4;
			float num7;
			if (num5 > 1f || num5 < 0f)
			{
				num7 = num6;
			}
			else if (num6 > 1f || num6 < 0f)
			{
				num7 = num5;
			}
			else
			{
				IntersectY = OldY + num5 * num4;
				num7 = ((!(Math.Abs(IntersectY) < Side)) ? num6 : num5);
			}
			IntersectX = OldX + num7 * num3;
			IntersectY = OldY + num7 * num4;
		}

		internal static string HourMinute(double T, bool secs)
		{
			string text = "";
			if (!secs)
			{
				T += 1.0 / 120.0;
			}
			if (T < 0.0)
			{
				T += 24.0;
				text = "-";
			}
			double num = Math.Floor(T);
			double num2 = Math.Floor(60.0 * (T - num));
			if (!secs)
			{
				return text + string.Format("{0,2:F0}h {1,2:F0}m", num, num2);
			}
			double num3 = Math.Floor(3600.0 * (T - num - num2 / 60.0));
			return text + string.Format("{0,1:F0}h {1,1:F0}m {2,1:F0}s", num, num2, num3);
		}

		public static void MakeDWD_OutputBlock(double Interval)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double geocentricDist = 0.0;
			double GeocentricDist = 0.0;
			double num = (int)(MidTime / 24.0);
			if (Interval == 0.1)
			{
				num = Math.Floor(MidTime / 2.4) / 10.0;
			}
			else if (Interval == 0.1)
			{
				num = Math.Floor(MidTime / 0.24) / 100.0;
			}
			MoonParameters(out var StarMoonElongation, out var MoonPhase);
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(AsteroidName.PadRight(21).Substring(0, 21));
			stringBuilder.AppendFormat("{0,6:F2}", MagConst_H0);
			stringBuilder.AppendFormat("{0,5:F2}", MagSlopeConst_G);
			stringBuilder.AppendFormat("{0,4:F0}", AsteroidDiameter);
			stringBuilder.Append("".PadRight(11));
			DWDFile.WriteLine(stringBuilder.ToString());
			stringBuilder = new StringBuilder();
			stringBuilder.Append(StarNo + "     " + DisplayMPOccultations.RA.Substring(0, 12) + " " + DisplayMPOccultations.Dec.Substring(0, 12));
			stringBuilder.AppendFormat("{0,6:F2}", Mv);
			stringBuilder.AppendFormat("{0,6:F2}", Mp);
			stringBuilder.Append(" " + StarNo.Substring(0, 3) + "  ");
			DWDFile.WriteLine(stringBuilder.ToString());
			stringBuilder = new StringBuilder();
			stringBuilder.AppendFormat("{0,4:F0}", EventYear);
			stringBuilder.AppendFormat("{0,3:F0}", EventMonth);
			stringBuilder.AppendFormat("{0,3:F0}", EventDay);
			stringBuilder.AppendFormat("{0,3:F0}", Math.Floor(MidTime));
			stringBuilder.AppendFormat("{0,6:F2}", MidTime % 1.0 * 60.0);
			stringBuilder.AppendFormat("{0,6:F2}", Math.Sqrt(Xatmin * Xatmin + YatMin * YatMin));
			stringBuilder.AppendFormat("{0,6:F1}", MaxDurn);
			stringBuilder.AppendFormat("{0,5:F1}", Mdrop);
			stringBuilder.AppendFormat("{0,4:F0}", SunElong);
			stringBuilder.AppendFormat("{0,4:F0}", StarMoonElongation);
			stringBuilder.AppendFormat("{0,4:F0}", MoonPhase);
			DWDFile.WriteLine(stringBuilder.ToString());
			float MoonDiaKm = 0f;
			float Mag = 0f;
			string MoonName = "";
			string EW = "";
			double num2 = -2.0 * Interval + num;
			double num3 = 3.1 * Interval + num;
			for (double num4 = num2; num4 <= num3; num4 += Interval)
			{
				double num5;
				double Magnitude;
				double Diameter_arcsec;
				double PhaseAngle_deg;
				double illumination;
				double PAlimb_deg;
				double Planetocentric_Latitude_deg;
				double PAPole_deg;
				double Elongation;
				if (IsMoon)
				{
					num5 = EventJD + num4;
					Utilities.PlanetGeocentric(num5, PlanetNumber, 1E-05, 4, out RA, out Dec, out GeocentricDist);
					Utilities.PlanetGeocentric(num5, PlanetNumber, out geocentricDist, out Magnitude, out Diameter_arcsec, out PhaseAngle_deg, out illumination, out PAlimb_deg, out Planetocentric_Latitude_deg, out PAPole_deg);
					Satellites.SatelliteCoordinates(num5, PlanetNumber, MoonNumber, ref MoonName, ref MoonDiaKm, ref RA, ref Dec, ref Mag);
					Magnitude = Mag;
				}
				else if (IsPlanet)
				{
					num5 = EventJD + num4;
					Utilities.PlanetGeocentric(num5, PlanetNumber, 1E-05, 4, out RA, out Dec, out GeocentricDist);
					Utilities.PlanetGeocentric(num5, PlanetNumber, 0.0, 2, physicalFlag: true, out var _, out var _, out var _, out RA, out Dec, out geocentricDist, out Diameter_arcsec, out Magnitude, out Elongation, out EW, out PhaseAngle_deg, out illumination, out PAlimb_deg, out Planetocentric_Latitude_deg, out PAPole_deg, out var _);
				}
				else
				{
					num5 = EventJD + num4;
					double num6 = Math.Floor((num5 - OsculatingJD) * 20.0) / 200.0;
					Utilities.PositionfromElements(num5, 0.0, 0.0, 0.0, 0.0, OsculatingJD + 10.0 * num6, MAnom + DeltaMAnom * num6, Q + DeltaQ * num6, e + DeltaE * num6, Perih + DeltaPerih * num6, node + DeltaNode * num6, i + DeltaI * num6, MagConst_H0, MagSlopeConst_G, MagCoeff_logR, 0.0, out RA, out Dec, out GeocentricDist, out geocentricDist, out Magnitude, out Elongation, out PhaseAngle_deg);
					PhaseAngle_deg *= 180.0 / Math.PI;
				}
				stringBuilder = new StringBuilder();
				stringBuilder.AppendFormat("{0,10:F2} ", num5);
				stringBuilder.Append(Utilities.DEGtoDMS(RA * (180.0 / Math.PI) / 15.0, 2, 3, MinutesOnly: false) + " ");
				stringBuilder.Append(Utilities.DEGtoDMS(Dec * (180.0 / Math.PI), 3, 2, MinutesOnly: false));
				stringBuilder.AppendFormat("{0,11:F7}", GeocentricDist);
				stringBuilder.AppendFormat("{0,11:F7}", geocentricDist);
				stringBuilder.AppendFormat("{0,6:F2}", Magnitude);
				stringBuilder.AppendFormat("{0,6:F2}", PhaseAngle_deg);
				DWDFile.WriteLine(stringBuilder.ToString());
			}
		}

		public static void InitialisePlot()
		{
			if (OpenOccultationElementFile(DWD: false, ""))
			{
				FileInUse = true;
				try
				{
					((Control)Plot).Show();
				}
				catch
				{
					Plot = new AsteroidPlotPath();
					((Control)Plot).Show();
				}
				Plot.ReSetExportButtons();
				Plot.colourToolStripMenuItem.set_Checked(!BWFlag);
				((ToolStripItem)Plot.plotToolStripMenuItem).set_Enabled(true);
				ToolStripMenuItem planetContactTimesForMultipleLocationsToolStripMenuItem = Plot.planetContactTimesForMultipleLocationsToolStripMenuItem;
				bool allowSitePredictions;
				((ToolStripItem)Plot.contactTimesForMultipleLocationsToolStripMenuItem).set_Enabled(allowSitePredictions = AllowSitePredictions);
				((ToolStripItem)planetContactTimesForMultipleLocationsToolStripMenuItem).set_Enabled(allowSitePredictions);
				CurrentElementFileRecord = 0L;
				Button cmdPrevious = Plot.cmdPrevious;
				((Control)Plot.cmdNext).set_Enabled(allowSitePredictions = true);
				((Control)cmdPrevious).set_Enabled(allowSitePredictions);
				UseSetErrorLocation = false;
				Plot.pBarPlot.set_Value(0);
				Plot.pBarPlot.set_Maximum((int)RecordsInFile);
				((Control)Plot.pBarPlot).set_Visible(true);
				PlotNextEvent();
			}
		}

		internal static void PlotNextEvent()
		{
			if (ListAndDisplay != null)
			{
				if (((ListControl)ListAndDisplay.lstSummary).get_SelectedIndex() < 0)
				{
					if (ListAndDisplay.lstSummary.get_Items().get_Count() > 4)
					{
						((ListControl)ListAndDisplay.lstSummary).set_SelectedIndex(ListAndDisplay.lstSummary.get_Items().get_Count() - 1);
					}
					else
					{
						((ListControl)ListAndDisplay.lstSummary).set_SelectedIndex(0);
					}
					return;
				}
				if (((ListControl)ListAndDisplay.lstSummary).get_SelectedIndex() < ListAndDisplay.lstSummary.get_Items().get_Count() - 1)
				{
					ListBox lstSummary = ListAndDisplay.lstSummary;
					int selectedIndex = ((ListControl)lstSummary).get_SelectedIndex();
					((ListControl)lstSummary).set_SelectedIndex(selectedIndex + 1);
				}
				if ((((ListControl)ListAndDisplay.lstSummary).get_SelectedIndex() < ListAndDisplay.lstSummary.get_Items().get_Count() - 1) & (ListAndDisplay.lstSummary.get_Items().get_Item(((ListControl)ListAndDisplay.lstSummary).get_SelectedIndex()).ToString() == ""))
				{
					ListBox lstSummary2 = ListAndDisplay.lstSummary;
					int selectedIndex = ((ListControl)lstSummary2).get_SelectedIndex();
					((ListControl)lstSummary2).set_SelectedIndex(selectedIndex + 1);
				}
				return;
			}
			bool MeetsCriteria;
			do
			{
				if (CurrentElementFileRecord <= RecordsInFile)
				{
					Get_Data_From_540Record((int)CurrentElementFileRecord, out MeetsCriteria, GeneratingSummary: false, AutoPlot: false);
					try
					{
						Plot.pBarPlot.set_Value((int)CurrentElementFileRecord);
					}
					catch
					{
					}
					CurrentElementFileRecord++;
					continue;
				}
				End_PlotRoutine();
				return;
			}
			while (!MeetsCriteria);
			AsteroidPlotPath.SetScaleCheckmarks((int)(2.0 + Math.Log10(PlotMagnification_Initial) / 0.3));
			Plot.ReSetExportButtons();
			PlotMagnification = PlotMagnification_Initial;
			ToolStripMenuItem planetContactTimesForMultipleLocationsToolStripMenuItem = Plot.planetContactTimesForMultipleLocationsToolStripMenuItem;
			bool allowSitePredictions;
			((ToolStripItem)Plot.contactTimesForMultipleLocationsToolStripMenuItem).set_Enabled(allowSitePredictions = AllowSitePredictions);
			((ToolStripItem)planetContactTimesForMultipleLocationsToolStripMenuItem).set_Enabled(allowSitePredictions);
			UseRedrawSites = false;
			UseSetErrorLocation = false;
			PlotPath();
		}

		internal static void PlotPreviousEvent()
		{
			if (ListAndDisplay != null)
			{
				if (((ListControl)ListAndDisplay.lstSummary).get_SelectedIndex() < 0)
				{
					if (ListAndDisplay.lstSummary.get_Items().get_Count() > 4)
					{
						((ListControl)ListAndDisplay.lstSummary).set_SelectedIndex(4);
					}
					else
					{
						((ListControl)ListAndDisplay.lstSummary).set_SelectedIndex(0);
					}
					return;
				}
				if (((ListControl)ListAndDisplay.lstSummary).get_SelectedIndex() > 4)
				{
					ListBox lstSummary = ListAndDisplay.lstSummary;
					int selectedIndex = ((ListControl)lstSummary).get_SelectedIndex();
					((ListControl)lstSummary).set_SelectedIndex(selectedIndex - 1);
				}
				if ((((ListControl)ListAndDisplay.lstSummary).get_SelectedIndex() > 4) & (ListAndDisplay.lstSummary.get_Items().get_Item(((ListControl)ListAndDisplay.lstSummary).get_SelectedIndex()).ToString() == ""))
				{
					ListBox lstSummary2 = ListAndDisplay.lstSummary;
					int selectedIndex = ((ListControl)lstSummary2).get_SelectedIndex();
					((ListControl)lstSummary2).set_SelectedIndex(selectedIndex - 1);
				}
				return;
			}
			bool MeetsCriteria;
			do
			{
				CurrentElementFileRecord--;
				if (CurrentElementFileRecord >= 0)
				{
					Get_Data_From_540Record((int)CurrentElementFileRecord, out MeetsCriteria, GeneratingSummary: false, AutoPlot: false);
					try
					{
						Plot.pBarPlot.set_Value((int)CurrentElementFileRecord);
					}
					catch
					{
					}
					continue;
				}
				End_PlotRoutine();
				return;
			}
			while (!MeetsCriteria);
			AsteroidPlotPath.SetScaleCheckmarks((int)(Math.Log10(PlotMagnification_Initial) / 0.3));
			Plot.ReSetExportButtons();
			PlotMagnification = PlotMagnification_Initial;
			ToolStripMenuItem planetContactTimesForMultipleLocationsToolStripMenuItem = Plot.planetContactTimesForMultipleLocationsToolStripMenuItem;
			bool allowSitePredictions;
			((ToolStripItem)Plot.contactTimesForMultipleLocationsToolStripMenuItem).set_Enabled(allowSitePredictions = AllowSitePredictions);
			((ToolStripItem)planetContactTimesForMultipleLocationsToolStripMenuItem).set_Enabled(allowSitePredictions);
			UseRedrawSites = false;
			UseSetErrorLocation = false;
			PlotPath();
		}

		internal static void End_PlotRoutine()
		{
			try
			{
				Elements.Close();
			}
			catch
			{
			}
			((Control)Plot.pBarPlot).set_Visible(false);
			((Form)Plot).Close();
			FileInUse = false;
		}

		internal static void PlotPath()
		{
			Application.DoEvents();
			int num = ((Control)Plot.picAsteroidPlot).get_Width();
			if (num < 50)
			{
				num = 150;
			}
			int num2 = ((Control)Plot.picAsteroidPlot).get_Height();
			if (num2 < 50)
			{
				num2 = 150;
			}
			IncludeStarChart = Settings.Default.IncludeStarChart_Prediction;
			Bitmap image = new Bitmap(num, num2);
			using (Graphics graphics = Graphics.FromImage(image))
			{
				if (Settings.Default.GraphicsSmoothed)
				{
					graphics.SmoothingMode = SmoothingMode.AntiAlias;
				}
				graphics.Clear(Color.White);
				DrawPath(graphics, num, num2, ((Control)Plot.cmbSiteFiles).get_Text().Trim(), ((ListControl)Plot.cmbNames).get_SelectedIndex(), BWFlag, Printer: false, Auto: false, AutoScaleMissEvents: false);
				Plot.picAsteroidPlot.set_Image((Image)image);
				graphics.Dispose();
			}
			GC.Collect();
			((Control)Plot).Focus();
		}

		internal static void PrintPathGraphic()
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Unknown result type (might be due to invalid IL or missing references)
			//IL_001e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0025: Unknown result type (might be due to invalid IL or missing references)
			//IL_002b: Invalid comparison between Unknown and I4
			PrintDocument printDocument = new PrintDocument();
			PrintDialog val = new PrintDialog();
			val.set_UseEXDialog(true);
			printDocument.DefaultPageSettings.Landscape = true;
			val.set_Document(printDocument);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				printDocument.PrintPage += PrintPath;
				printDocument.Print();
				PlotPath();
			}
		}

		internal static void PrintPreviewPathGraphic()
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			//IL_000c: Expected O, but got Unknown
			//IL_000c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_0018: Unknown result type (might be due to invalid IL or missing references)
			//IL_0032: Unknown result type (might be due to invalid IL or missing references)
			//IL_0039: Unknown result type (might be due to invalid IL or missing references)
			//IL_003f: Invalid comparison between Unknown and I4
			//IL_0054: Unknown result type (might be due to invalid IL or missing references)
			PrintDocument printDocument = new PrintDocument();
			PrintPreviewDialog val = new PrintPreviewDialog();
			PrintDialog val2 = new PrintDialog();
			val2.set_UseEXDialog(true);
			val2.set_UseEXDialog(true);
			printDocument.DefaultPageSettings.Landscape = true;
			val.set_Document(printDocument);
			val2.set_Document(printDocument);
			if ((int)((CommonDialog)val2).ShowDialog() == 1)
			{
				printDocument.PrintPage += PrintPath;
				((Form)val).ShowDialog();
				PlotPath();
			}
		}

		internal static void PrintPath(object sender, PrintPageEventArgs e)
		{
			Graphics graphics = e.Graphics;
			int chartHeight = (int)(0.92 * (double)e.PageBounds.Height);
			int chartWidth = (int)(0.92 * (double)e.PageBounds.Width);
			DrawPath(graphics, chartWidth, chartHeight, ((Control)Plot.cmbSiteFiles).get_Text().Trim(), ((ListControl)Plot.cmbNames).get_SelectedIndex(), InBW: false, Printer: true, Auto: false, AutoScaleMissEvents: false);
		}

		internal static void DrawPath(Graphics formGraphics, int ChartWidth, int ChartHeight, string SiteFile, int NameFlag, bool InBW, bool Printer, bool Auto, bool AutoScaleMissEvents)
		{
			float num = 0f;
			float num2 = 0f;
			int num3 = 95;
			string text = "";
			string text2 = "";
			string text3 = "";
			if (!Auto)
			{
				ToolStripMenuItem showPlanetConfigurationToolStripMenuItem = Plot.showPlanetConfigurationToolStripMenuItem;
				ToolStripMenuItem planetAppearanceToolStripMenuItem = Plot.planetAppearanceToolStripMenuItem;
				bool flag;
				((ToolStripItem)Plot.planetConfigToolStripMenuItem).set_Enabled(flag = IsPlanet | IsMoon);
				bool enabled;
				((ToolStripItem)planetAppearanceToolStripMenuItem).set_Enabled(enabled = flag);
				((ToolStripItem)showPlanetConfigurationToolStripMenuItem).set_Enabled(enabled);
			}
			if (Settings.Default.GraphicsSmoothed)
			{
				formGraphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			Font font = new Font("Courier New", 8f);
			Font font2 = new Font("Courier New", 12f, FontStyle.Bold);
			Font font3 = new Font("Courier New", 8f, FontStyle.Italic);
			Pen pen;
			Pen pen2;
			Pen pen3;
			Pen pen4;
			Pen pen5;
			Pen pen6;
			Pen pen7;
			Pen pen8;
			Pen pen9;
			Pen pen10;
			Pen pen11;
			Brush brush;
			Brush brush2;
			Brush brush3;
			Brush brush4;
			Brush brush5;
			Brush brush6;
			Color color;
			if (InBW || Printer)
			{
				pen = new Pen(Brushes.Black, 2f);
				pen2 = new Pen(Brushes.Black, 1f);
				pen3 = new Pen(Brushes.Black, 1f);
				pen4 = new Pen(Brushes.Black, 1f);
				pen5 = new Pen(Brushes.Black, 1f);
				pen6 = new Pen(Brushes.Black, 1f);
				pen7 = new Pen(Brushes.Black, 2f);
				pen8 = new Pen(Brushes.Black, 1f);
				pen9 = new Pen(Brushes.Black, 1f);
				pen10 = new Pen(Brushes.Black, 1f);
				pen11 = new Pen(Brushes.Black, 1f);
				brush = Brushes.Black;
				brush2 = Brushes.Black;
				brush3 = Brushes.Black;
				brush4 = Brushes.White;
				brush5 = Brushes.White;
				brush6 = Brushes.Black;
				color = Color.Black;
			}
			else
			{
				pen = new Pen(Brushes.Gray, 2f);
				pen2 = new Pen(Brushes.Gray, 1f);
				pen3 = new Pen(Brushes.Cyan, 1f);
				pen4 = new Pen(Brushes.Magenta, 1f);
				pen5 = new Pen(Brushes.Indigo, 1f);
				pen6 = new Pen(Brushes.AliceBlue, 1f);
				pen7 = new Pen(Brushes.WhiteSmoke, 2f);
				pen8 = new Pen(Brushes.WhiteSmoke, 1f);
				pen9 = new Pen(Brushes.Red, 1f);
				pen10 = new Pen(Brushes.Orange, 1f);
				pen11 = new Pen(Brushes.GreenYellow, 1f);
				brush = Brushes.White;
				brush2 = Brushes.Yellow;
				brush3 = Brushes.LightGreen;
				brush4 = Brushes.Black;
				brush5 = new SolidBrush(Color.FromArgb(255, 9, 9, 42));
				brush6 = Brushes.LightGray;
				color = Color.FromArgb(55, 167, 104);
			}
			pen5.DashStyle = DashStyle.Dash;
			pen6.DashPattern = new float[2] { 2f, 8f };
			if (Settings.Default.AsteroidPlot_SolidErrorLines)
			{
				pen9.DashStyle = DashStyle.Dash;
			}
			else
			{
				pen9.DashStyle = DashStyle.Solid;
			}
			pen3.DashStyle = DashStyle.Solid;
			pen3.DashPattern = new float[2] { 5f, 15f };
			pen4.DashStyle = DashStyle.DashDotDot;
			if (InBW || Printer)
			{
				formGraphics.Clear(Color.White);
			}
			else
			{
				formGraphics.Clear(Color.Black);
			}
			double num4 = YatMin - Xatmin / DeltaX * DeltaY;
			int num5;
			if (Math.Sign(DeltaX) == Math.Sign(DeltaY))
			{
				num5 = 1;
				if (num4 > 0.0)
				{
					num5 = 2;
				}
			}
			else
			{
				num5 = 3;
				if (num4 > 0.0)
				{
					num5 = 0;
				}
			}
			if (!IsPlanet && (((PlotMagnification == 1.0) & (PlotMagnification_Initial == 1.0)) || AutoScaleMissEvents) && MinGeocentricD > 1.1)
			{
				PlotMagnification = 0.9 / MinGeocentricD;
			}
			float num6 = (float)(0.3 * (double)ChartHeight * PlotMagnification);
			float num7 = (float)(0.3 * (double)ChartHeight);
			float xCenter;
			float num8;
			float num9;
			if (num5 < 2)
			{
				xCenter = (float)(0.6 * (double)ChartWidth);
				num8 = (float)(0.02 * (double)ChartWidth);
				num9 = num8 + 0.7f * num7;
			}
			else
			{
				xCenter = (float)(0.35 * (double)ChartWidth);
				num8 = (float)(0.98 * (double)ChartWidth - (double)num7);
				num9 = num8 + 0.3f * num7;
			}
			float num10 = (float)(0.6 * (double)ChartHeight);
			float num11;
			float num12;
			if (num5 % 2 == 0)
			{
				num11 = (float)(ChartHeight - 25) - num7;
				num12 = (float)(0.75 * (double)num10);
			}
			else
			{
				num11 = num3 + 18;
				num12 = (float)(1.25 * (double)num10);
			}
			if (UseSetErrorLocation)
			{
				num9 = XerrorSet;
				num12 = YerrorSet;
			}
			text3 = ((!(EarthSemiDuration > 0.0)) ? (" on " + Utilities.Date_from_JD(EventJD, 0) + " at " + HourMinute(MidTime, secs: false) + " UT") : (" on " + Utilities.Date_from_JD(EventJD, 0) + " from " + HourMinute(MidTime - EarthSemiDuration, secs: false) + " to " + HourMinute(MidTime + EarthSemiDuration, secs: false) + " UT"));
			if (!UseRedrawSites)
			{
				PlotLongitude = SiteLongitude_Rad * (180.0 / Math.PI);
				PlotLatitude = SiteLatitude_Rad * (180.0 / Math.PI);
				if (!Auto)
				{
					Plot.updnLongitude.set_Value((decimal)(Math.Floor(PlotLongitude * 100.0) / 100.0));
					Plot.updnLatitude.set_Value((decimal)(Math.Floor(PlotLatitude * 100.0) / 100.0));
				}
			}
			else
			{
				PlotLongitude = RedrawLongitude;
				PlotLatitude = RedrawLatitude;
			}
			AsteroidPath_For_Plots(0.25, Multipath: false);
			List<TimeDistance> list = new List<TimeDistance>();
			bool flag2 = !IsPlanet & !IsMoon;
			double Elongation;
			if (((EarthSemiDuration > 1.0) | CenteredUsingTimeOnPath) && flag2)
			{
				using (StreamReader streamReader = new StreamReader(AppPath + "\\Generated Files\\path.tmp"))
				{
					do
					{
						string[] array = streamReader.ReadLine()!.Split(new char[1] { ',' });
						double.TryParse(array[0], out var result);
						double.TryParse(array[1], out var result2);
						double.TryParse(array[2], out var result3);
						if (!(result == 0.0 || result3 == 100.0))
						{
							Utilities.Distance(PlotLongitude / (180.0 / Math.PI), PlotLatitude / (180.0 / Math.PI), result2 / (180.0 / Math.PI), result3 / (180.0 / Math.PI), out var Distance, out Elongation);
							if (!(Distance > Math.PI / 2.0))
							{
								TimeDistance timeDistance = new TimeDistance();
								timeDistance.Distance = Distance * (180.0 / Math.PI);
								timeDistance.T = result;
								list.Add(timeDistance);
							}
						}
					}
					while (!streamReader.EndOfStream);
				}
				list.Sort();
				if (list.Count > 9)
				{
					TClosest = list[0].T - MidTime;
					TClosest *= 15.0;
					MinDArcSec = 0.0;
					Maps.EarthGlobe(formGraphics, num6, xCenter, num10, PlotLongitude, SubstellarLatitude_J2000, SubSolarLongitude_OfDate - TClosest / 0.99726956633, SubSolarLatitude_OfDate, PlotLongitude, PlotLatitude, ShowSunLit: true, (FilterOnSite | UseRedrawSites) & (PlotMagnification > 1.0), PlotCities: false, PlotMagnification > 1.0, InBW || Printer, Mirrored: false);
				}
			}
			if ((((EarthSemiDuration <= 1.0) & !CenteredUsingTimeOnPath) || !flag2) | (list.Count < 10))
			{
				double num13 = 0.25;
				for (double num14 = 0.0 - EarthSemiDuration; num14 < EarthSemiDuration + 0.0001; num14 += num13)
				{
					TClosest = 0.0;
					double num15 = num14;
					Maps.SetGlobeOrientation((SubstellarLongitude - num14 * 15.0) / (180.0 / Math.PI), SubstellarLatitude_J2000 / (180.0 / Math.PI));
					Maps.GlobeCoords(PlotLongitude / (180.0 / Math.PI), PlotLatitude / (180.0 / Math.PI), out xSiteLocation, out ySiteLocation, out zSiteLocation, Mirrored: false);
					X0 = Xatmin - (double)xSiteLocation;
					Y0 = YatMin - (double)ySiteLocation;
					double num16 = DeltaX + (num14 + TClosest) * 2.0 * Delta2X + 3.0 * Delta3X * (num14 + TClosest) * (num14 + TClosest);
					double num17 = DeltaY + (num14 + TClosest) * 2.0 * Delta2Y + 3.0 * Delta3Y * (num14 + TClosest) * (num14 + TClosest);
					DisplayMPOccultations.n = Math.Sqrt(num16 * num16 + num17 * num17);
					num15 = (0.0 - (X0 * num16 + Y0 * num17)) / DisplayMPOccultations.n / DisplayMPOccultations.n;
					MinimumSepn = (X0 * num17 - Y0 * num16) / DisplayMPOccultations.n;
					double num18 = Xatmin + (num14 + TClosest) * DeltaX + (num14 + TClosest) * (num14 + TClosest) * Delta2X + Delta3X * (num14 + TClosest) * (num14 + TClosest) * (num14 + TClosest) - (double)xSiteLocation;
					double num19 = YatMin + (num14 + TClosest) * DeltaY + (num14 + TClosest) * (num14 + TClosest) * Delta2Y + Delta3Y * (num14 + TClosest) * (num14 + TClosest) * (num14 + TClosest) - (double)ySiteLocation;
					if ((Math.Sqrt(num18 * num18 + num19 * num19) < 0.4 + AsteroidShadowDiameter_Penumbral / 2.0 / 6378.137) & (zSiteLocation > 0f))
					{
						TClosest = num14 + num15;
						break;
					}
				}
				TClosest *= 15.0;
				MinDArcSec = Math.Abs(MinimumSepn) * 8.794143836182533 / DistanceToAsteroid;
				Maps.EarthGlobe(formGraphics, num6, xCenter, num10, SubstellarLongitude - TClosest / 0.99726956633, SubstellarLatitude_J2000, SubSolarLongitude_OfDate - TClosest / 0.99726956633, SubSolarLatitude_OfDate, PlotLongitude, PlotLatitude, ShowSunLit: true, (FilterOnSite | UseRedrawSites) & (PlotMagnification > 1.0), PlotCities: false, PlotMagnification > 1.0, InBW || Printer, Mirrored: false);
			}
			float globeMapCentreX = Maps.GlobeMapCentreX;
			float globeMapCentreY = Maps.GlobeMapCentreY;
			double num20 = Math.Atan2(DeltaY, DeltaX);
			AsteroidEarthDia = (float)(AsteroidShadowDiameter_Penumbral / 6378.137);
			float num21 = (float)((double)(num6 * AsteroidEarthDia / 2f) * Math.Cos(num20 - Math.PI / 2.0));
			float num22 = (float)((double)((0f - num6) * AsteroidEarthDia / 2f) * Math.Sin(num20 - Math.PI / 2.0));
			float num23 = AsteroidEarthDia;
			if ((double)AsteroidEarthDia < 0.02)
			{
				num23 = 0.02f;
			}
			float num24 = (float)((double)(num6 * num23 / 2f) * Math.Cos(num20 - Math.PI / 2.0));
			float num25 = (float)((double)((0f - num6) * num23 / 2f) * Math.Sin(num20 - Math.PI / 2.0));
			double num26 = 60.0 / DisplayMPOccultations.n;
			double num27 = Math.Floor(MidTime);
			double num28 = 60.0 * (MidTime - num27);
			double num29 = num28 - Math.Floor(num28);
			_ = DisplayMPOccultations.n / 60.0;
			float num30 = (float)((double)globeMapCentreX + (double)num6 * (Xatmin - DeltaX * num26 / 10.0));
			float num31 = (float)((double)globeMapCentreY - (double)num6 * (YatMin - DeltaY * num26 / 10.0));
			double num32 = 2.0;
			int num33 = (int)(-4.0 * num26);
			int num34 = (int)(4.0 * num26);
			for (int i = num33; i <= num34; i++)
			{
				Time_GetLatitudeLongitude(((double)i - num29) / 60.0, UseLocalTopography: false, 0.0, out var Valid, out var LatitudeDeg, out var LongitudeDeg, out Elongation);
				float x = 0f;
				float y = 0f;
				float z = 0f;
				if (Valid)
				{
					Maps.GlobeProjection(LongitudeDeg / (180.0 / Math.PI), LatitudeDeg / (180.0 / Math.PI), out x, out y, out z, Mirrored: false);
					num = globeMapCentreX + x;
					num2 = globeMapCentreY - y;
				}
				else
				{
					num = (float)((double)globeMapCentreX + (double)num6 * (Xatmin + DeltaX * ((double)i - num29) / 60.0 + Delta2X * ((double)i - num29) * ((double)i - num29) / 3600.0 + Delta3X * ((double)i - num29) * ((double)i - num29) * ((double)i - num29) / 216000.0));
					num2 = (float)((double)globeMapCentreY - (double)num6 * (YatMin + DeltaY * ((double)i - num29) / 60.0 + Delta2Y * ((double)i - num29) * ((double)i - num29) / 3600.0 + Delta3Y * ((double)i - num29) * ((double)i - num29) * ((double)i - num29) / 216000.0));
				}
				double num35 = Math.Sqrt((num - globeMapCentreX) * (num - globeMapCentreX) + (num2 - globeMapCentreY) * (num2 - globeMapCentreY));
				if ((double)z > -0.1)
				{
					float num36 = 1f;
					float num37 = 0f;
					float num38 = 0f;
					float num39 = 0f;
					float num40 = 0f;
					if ((Math.Floor(num28) + (double)i) % 60.0 == 0.0)
					{
						num36 = 1f + 35f / num6 / num23;
						num37 = num + num36 * num24;
						num38 = num2 + num36 * num25;
						num39 = num - num36 * num24;
						num40 = num2 - num36 * num25;
						formGraphics.DrawLine(pen, num37, num38, num39, num40);
					}
					else if ((Math.Floor(num28) + (double)i) % 10.0 == 0.0)
					{
						num36 = 1f + 25f / num6 / num23;
						num37 = num + num36 * num24;
						num38 = num2 + num36 * num25;
						num39 = num - num36 * num24;
						num40 = num2 - num36 * num25;
						formGraphics.DrawLine(pen, num37, num38, num39, num40);
					}
					else if ((Math.Floor(num28) + (double)i) % 5.0 == 0.0)
					{
						num36 = 1f + 15f / num6 / num23;
						num37 = num + num36 * num24;
						num38 = num2 + num36 * num25;
						num39 = num - num36 * num24;
						num40 = num2 - num36 * num25;
						formGraphics.DrawLine(pen2, num37, num38, num39, num40);
					}
					else if (EarthSemiDuration < 1.0)
					{
						num37 = num + num24;
						num38 = num2 + num25;
						num39 = num - num24;
						num40 = num2 - num25;
						formGraphics.DrawLine(pen2, num37, num38, num39, num40);
					}
					if ((((num26 <= 10.0) & (PlotMagnification > 0.2)) | ((num26 < 20.0) & (PlotMagnification > 1.0)) | ((Math.Floor(num28 + (double)i) % 5.0 == 0.0) & ((num26 > 10.0) | ((double)AsteroidEarthDia > 1.5))) | (Math.Floor(num28 + (double)i) % 10.0 == 0.0)) && ((num35 > 1.02 * (double)num6) | (PlotMagnification > 1.0)))
					{
						double num41 = Math.Floor(num28) + (double)i;
						_ = 2.0;
						string s;
						if (num41 > 60.0)
						{
							Elongation = num41 - 60.0;
							s = Elongation + "+";
						}
						else if (num41 < 0.0)
						{
							Elongation = num41 + 60.0;
							s = Elongation + "-";
						}
						else
						{
							s = num41.ToString();
						}
						float x2 = num37;
						float num42 = num38;
						bool flag3 = num38 < num40;
						if (num39 > num37)
						{
							x2 = num39;
							num42 = num40;
							flag3 = !flag3;
						}
						if ((double)AsteroidEarthDia < 1.5)
						{
							formGraphics.DrawString(s, font, brush6, x2, num42 - 12f * Convert.ToSingle(flag3));
						}
						else
						{
							formGraphics.DrawString(s, font, brush6, (float)((double)num - 2.0 * MinimumSepn / (double)AsteroidEarthDia * (double)num24), (float)((double)num2 - 2.0 * MinimumSepn / (double)AsteroidEarthDia * (double)num25));
						}
					}
					float num43 = num;
					float num44 = num2;
					if (num35 > (double)num6 && num32 > (double)num6)
					{
						formGraphics.DrawLine(pen5, num30 + num21, num31 + num22, num43 + num21, num44 + num22);
						formGraphics.DrawLine(pen5, num30 - num21, num31 - num22, num43 - num21, num44 - num22);
					}
					if (MinGeocentricD > 1.0)
					{
						formGraphics.DrawLine(pen6, (float)((double)num30 + (double)num21 * Ratio_1Sigma_to_Limit_Distances), (float)((double)num31 + (double)num22 * Ratio_1Sigma_to_Limit_Distances), (float)((double)num43 + (double)num21 * Ratio_1Sigma_to_Limit_Distances), (float)((double)num44 + (double)num22 * Ratio_1Sigma_to_Limit_Distances));
						formGraphics.DrawLine(pen6, (float)((double)num30 - (double)num21 * Ratio_1Sigma_to_Limit_Distances), (float)((double)num31 - (double)num22 * Ratio_1Sigma_to_Limit_Distances), (float)((double)num43 - (double)num21 * Ratio_1Sigma_to_Limit_Distances), (float)((double)num44 - (double)num22 * Ratio_1Sigma_to_Limit_Distances));
					}
					num30 = num43;
					num31 = num44;
				}
				num32 = num35;
			}
			double num45 = 0.0;
			double num46 = 0.0;
			double num47 = 0.0;
			double num48 = 0.0;
			double num49 = 0.0;
			double num50 = 1.0;
			double num51 = 1.0;
			double num52 = 1.0;
			double num53 = 1.0;
			float x3 = 0f;
			float y2 = 0f;
			float num54 = 0f;
			float num55 = 0f;
			if (Settings.Default.AsteroidPlot_SolidErrorLines)
			{
				pen8.DashStyle = DashStyle.Solid;
			}
			else
			{
				pen8.DashPattern = new float[2] { 2f, 2f };
			}
			num47 = ErrorInTime / 240.0;
			float z2;
			if (new FileInfo(AppPath + "\\Generated Files\\path.tmp").Length > 100)
			{
				for (int j = 0; j <= 7; j++)
				{
					if ((j == 4 || j == 5) && AsteroidShadowDiameter_Umbral > 0.8 * AsteroidShadowDiameter_Penumbral)
					{
						continue;
					}
					if (!AllowAsteroidRings && j > 5)
					{
						break;
					}
					bool flag4 = false;
					double num56 = -500.0;
					double num57 = -500.0;
					if (j < 2)
					{
						double num58 = 0.0;
					}
					else
					{
						double num58 = num47;
					}
					using StreamReader streamReader2 = new StreamReader(AppPath + "\\Generated Files\\path.tmp");
					do
					{
						string[] array2 = streamReader2.ReadLine()!.Split(new char[1] { ',' });
						num45 = double.Parse(array2[3 + 2 * j]);
						num46 = double.Parse(array2[4 + 2 * j]);
						bool num59 = num46 < 100.0;
						if (j > 1)
						{
							num48 = double.Parse(array2[1]);
							num49 = double.Parse(array2[2 + 2 * j]);
							double num58;
							if (num46 < 100.0 && num49 < 100.0 && num56 < 100.0)
							{
								if (num46 == num49)
								{
									num50 = num45 - num48;
									if (num50 < -180.0)
									{
										num50 += 360.0;
									}
									if (num50 > 180.0)
									{
										num50 -= 360.0;
									}
									num58 = num47 * (double)Math.Sign(num50);
								}
								else if (num45 == num48)
								{
									num51 = Math.Sign(num49 - num46);
									num52 = Math.Sign(num46 - num56);
									num53 = Math.Sign(num45 - num57);
									num58 = num47 * num51 * num52 * num53;
								}
								else
								{
									num50 = num45 - num48;
									if (num50 < -180.0)
									{
										num50 += 360.0;
									}
									if (num50 > 180.0)
									{
										num50 -= 360.0;
									}
									num58 = num47 * (double)Math.Sign(num50);
								}
							}
							else
							{
								num58 = 0.0;
							}
							num57 = num45;
							num45 += num58;
						}
						Maps.GlobeProjection(num45 / (180.0 / Math.PI), num46 / (180.0 / Math.PI), out x3, out y2, out z2, Mirrored: false);
						if (num59 && (double)z2 > -0.1 && flag4)
						{
							if (j < 2)
							{
								formGraphics.DrawLine(pen7, globeMapCentreX + num54, globeMapCentreY - num55, globeMapCentreX + x3, globeMapCentreY - y2);
							}
							else if (j < 4)
							{
								formGraphics.DrawLine(pen8, globeMapCentreX + num54, globeMapCentreY - num55, globeMapCentreX + x3, globeMapCentreY - y2);
							}
							else if (j < 6)
							{
								formGraphics.DrawLine(pen4, globeMapCentreX + num54, globeMapCentreY - num55, globeMapCentreX + x3, globeMapCentreY - y2);
							}
							else
							{
								formGraphics.DrawLine(pen3, globeMapCentreX + num54, globeMapCentreY - num55, globeMapCentreX + x3, globeMapCentreY - y2);
							}
						}
						num54 = x3;
						num55 = y2;
						flag4 = num59;
						num56 = num46;
					}
					while (!streamReader2.EndOfStream);
				}
			}
			string path = AppPath + "\\Sites\\" + SiteFile;
			if (File.Exists(path))
			{
				Sites sites = new Sites();
				float num60 = 1f;
				int num61 = 3;
				if ((double)num6 > 0.5 * (double)ChartHeight)
				{
					num61 = 2;
				}
				if (num6 > (float)(2 * ChartHeight))
				{
					num61 = 1;
				}
				if (num6 > (float)(4 * ChartHeight))
				{
					num61 = 0;
				}
				StreamReader streamReader3 = new StreamReader(path);
				do
				{
					sites.Read_SiteFile(streamReader3.ReadLine());
					if (sites.PlotOnMap <= num61)
					{
						continue;
					}
					Maps.GlobeProjection(sites.Longitude / (180.0 / Math.PI), sites.Latitude / (180.0 / Math.PI), out x3, out y2, out z2, Mirrored: false);
					if (z2 > 0f)
					{
						if (sites.PlotOnMap <= 3)
						{
							num60 = sites.PlotOnMap switch
							{
								1 => 1f, 
								2 => 2f, 
								3 => 4f, 
								_ => 1f, 
							};
							formGraphics.DrawEllipse(pen10, globeMapCentreX + x3 - num60, globeMapCentreY - y2 - num60, 2f * num60, 2f * num60);
						}
						else if (num61 == 0)
						{
							num60 = 1.5f;
							formGraphics.DrawRectangle(pen11, globeMapCentreX + x3 - num60, globeMapCentreY - y2 - num60, 2f * num60, 2f * num60);
						}
						if ((sites.PlotOnMap >= NameFlag || NameFlag == 1) && NameFlag > 0)
						{
							formGraphics.DrawString(sites.ShortName, font, brush6, globeMapCentreX + x3 + num60, globeMapCentreY - y2 - 5f);
						}
					}
				}
				while (!streamReader3.EndOfStream);
				streamReader3.Close();
			}
			if (DrawErrorEllipse)
			{
				double num62 = ErrorMajor * (double)num6 / 8.794143836182533 * DistanceToAsteroid;
				double num63 = ErrorMinor * (double)num6 / 8.794143836182533 * DistanceToAsteroid;
				double num64 = Math.Sin(ErrorPA / (180.0 / Math.PI));
				double num65 = Math.Cos(ErrorPA / (180.0 / Math.PI));
				double num66 = 0.0;
				double num67 = 0.0;
				for (double num68 = 0.0; num68 < 361.0; num68 += 8.0)
				{
					double num69 = num63 * Math.Sin(num68 / (180.0 / Math.PI));
					double num70 = num62 * Math.Cos(num68 / (180.0 / Math.PI));
					double num71 = (double)num9 + num69 * num65 - num70 * num64;
					double num72 = (double)num12 + num69 * num64 + num70 * num65;
					if (num68 > 0.0)
					{
						formGraphics.DrawLine(pen9, (float)num66, (float)num67, (float)num71, (float)num72);
					}
					num66 = num71;
					num67 = num72;
				}
			}
			int num73 = 96;
			int num74 = 12;
			string text4 = " ";
			int num75 = 0;
			string text5 = " ";
			int num76 = 0;
			string text6 = " ";
			int num77 = 0;
			string text7 = " ";
			int num78 = 0;
			string text8 = " ";
			int num79 = 0;
			string text9 = " ";
			int num80 = 0;
			string text10 = " ";
			int num81 = 0;
			string text11 = " ";
			int num82 = 0;
			double num83 = Utilities.FresnelLength_m(DistanceToAsteroid, 600.0, 150.0, out Elongation);
			if (!IsPlanet)
			{
				int gaiaBinary = GaiaBinaries.GetGaiaBinary(AsteroidNumber);
				if (gaiaBinary >= 0)
				{
					text5 = "Gaia binary asteroid " + GaiaBinaries.GaiaBinaryData[gaiaBinary].GaiaBinaryDiaDurn(AsteroidDiameter, MaxDurn);
					num76 = num73;
					num73 += num74;
				}
			}
			if (!IsPlanet & (NumOfMoons > 0))
			{
				string text12 = "";
				if (NumOfMoons > 1)
				{
					text12 = "s";
				}
				text4 = NumOfMoons + " moon" + text12 + ".";
				if (Current_OccElements_Record >= 0)
				{
					for (int k = 0; k < OccElements[Current_OccElements_Record].AsteroidMoons.Count; k++)
					{
						if (k > 0)
						{
							text4 += ",";
						}
						text4 = ((OccElements[Current_OccElements_Record].AsteroidMoons[k].IDMoonName.Trim().Length <= 0) ? (text4 + " {?} ") : (text4 + " {" + OccElements[Current_OccElements_Record].AsteroidMoons[k].IDMoonName.Replace(" ", "") + "} "));
						text4 = ((!(OccElements[Current_OccElements_Record].AsteroidMoons[k].Diameter > 0.0)) ? (text4 + "?km") : (text4 + string.Format("{0,1:f0}km", OccElements[Current_OccElements_Record].AsteroidMoons[k].Diameter)));
						text4 = ((!(OccElements[Current_OccElements_Record].AsteroidMoons[k].A > 0.0)) ? (text4 + " at ?km") : (text4 + string.Format(" at {0,1:f0}km", OccElements[Current_OccElements_Record].AsteroidMoons[k].A)));
						if (OccElements[Current_OccElements_Record].AsteroidMoons[k].PeriodDays > 0.0)
						{
							text4 += string.Format(", Period {0,1:f3}days", OccElements[Current_OccElements_Record].AsteroidMoons[k].PeriodDays);
						}
					}
				}
				else
				{
					text4 = text4 + "{" + MoonName1.Trim() + "} " + Diameter1 + "km at " + Distance1 + "km";
					if (NumOfMoons == 2)
					{
						text4 = text4 + " and {" + MoonName2.Trim() + "} " + Diameter2 + "km at " + Distance2 + "km";
					}
				}
				if (SatelliteOrbitAvailable)
				{
					text4 += "  Orbit@Miriade";
				}
				if (text4.Length > 1)
				{
					num75 = num73;
					num73 += num74;
				}
			}
			if (RIO_TNO_event)
			{
				string text13 = "LESIA [Paris Obs]";
				string text14 = "LuckyStar";
				if (OrbitSourceDate.Contains("RIO"))
				{
					text13 = "the RIO group";
					text14 = "RIO_TNO";
				}
				text8 = "Prediction is from the " + text14 + " feed. Report any observations to " + text13;
				if (text8.Length > 1)
				{
					num79 = num73;
					num73 += num74;
				}
			}
			if ((5.0 * num83 / 1000.0 / (AsteroidDiameter / 2.0) > 1.0) | (MaxDurn / AsteroidDiameter * num83 / 1000.0 > 0.1))
			{
				text7 = string.Format("Diffraction: Fresnel unit = {0,1:f0} m", num83);
				num78 = num73;
				num73 += num74;
			}
			if (StellarDiameter_mas > 0.1)
			{
				double num84 = StellarDiameter_mas / AsteroidAngularDiameterMAS;
				double num85 = num84 * MaxDurn / (1.0 + num84);
				double num86 = Math.Pow(num84, 2.0);
				if (num85 > 0.01)
				{
					string text15 = "0";
					if (num85 < 2.0)
					{
						text15 = "1";
					}
					if (num85 < 0.2)
					{
						text15 = "2";
					}
					text6 = string.Format("Expect fades >{0,1:f" + text15 + "} secs (star dia)", num85 * 0.7);
					if (num84 > 1.0)
					{
						text6 = string.Format("Mag {0,1:f2} Annular Occn.  ", num86) + text6;
					}
					if (text6.Length > 1)
					{
						num77 = num73;
						num73 += num74;
					}
				}
			}
			if (PlanetMoonInPlanetShadow)
			{
				if (ObjectID.Trim().PadRight(4).Substring(0, 3) == "P5M")
				{
					text11 = ObjectName + " is in Jupiter's shadow, but not occulted by Jupiter";
					num82 = num73;
					num73 += num74;
				}
				else if (ObjectID.Trim().PadRight(4).Substring(0, 3) == "P6M")
				{
					text11 = ObjectName + " is in Saturn's shadow, but not occulted by Saturn";
					num82 = num73;
					num73 += num74;
				}
			}
			if ((DoubleStarCode & 3) == 3)
			{
				text9 = "Double star";
			}
			else if ((DoubleStarCode & 3) == 2)
			{
				text9 = "Double, not in WDS";
			}
			else if ((DoubleStarCode & 3) == 1)
			{
				text9 = "Double, in WDS";
			}
			if ((DoubleStarCode & 4) == 4)
			{
				text9 = ((text9.Length <= 2) ? "Variable star" : (text9.Trim() + ";  Variable star"));
			}
			if (text9.Length > 1)
			{
				num80 = num73;
				num73 += num74;
			}
			if (!IsPlanet & !IsMoon)
			{
				if (Current_OccElements_Record >= 0)
				{
					text10 = text10 + OccElements[Current_OccElements_Record].AsteroidClass + "  ";
				}
				if (NumOfRings > 0)
				{
					string text16 = "";
					if (NumOfRings > 1)
					{
						text16 = "s";
					}
					text10 = "Asteroid has " + NumOfRings + " ring" + text16 + ". ";
				}
				num81 = num73;
				num73 += num74;
			}
			if (text5.Length > 1)
			{
				float width = formGraphics.MeasureString(text5, font).Width;
				formGraphics.FillRectangle(brush4, 1f, num76, width + 6f, 14f);
				formGraphics.DrawString(text5, font, brush2, 5f, num76);
			}
			if (text4.Length > 1)
			{
				float width2 = formGraphics.MeasureString(text4, font).Width;
				formGraphics.FillRectangle(brush4, 1f, num75, width2 + 6f, 14f);
				formGraphics.DrawString(text4, font, brush2, 5f, num75);
			}
			if (text8.Length > 1)
			{
				float width3 = formGraphics.MeasureString(text8, font).Width;
				formGraphics.FillRectangle(brush4, 1f, num79, width3 + 6f, 14f);
				formGraphics.DrawString(text8, font, brush2, 5f, num79);
			}
			if (text7.Length > 1)
			{
				float width4 = formGraphics.MeasureString(text7, font).Width;
				formGraphics.FillRectangle(brush4, 1f, num78, width4 + 6f, 14f);
				formGraphics.DrawString(text7, font, brush2, 5f, num78);
			}
			if (text6.Length > 1)
			{
				float width5 = formGraphics.MeasureString(text6, font).Width;
				formGraphics.FillRectangle(brush4, 1f, num77, width5 + 6f, 14f);
				formGraphics.DrawString(text6, font, brush2, 5f, num77);
			}
			if (text9.Length > 1)
			{
				float width6 = formGraphics.MeasureString(text9, font).Width;
				formGraphics.FillRectangle(brush4, 1f, num80, width6 + 6f, 14f);
				formGraphics.DrawString(text9, font, brush2, 5f, num80);
			}
			if (text10.Length > 1)
			{
				float width7 = formGraphics.MeasureString(text10, font).Width;
				formGraphics.FillRectangle(brush4, 1f, num81, width7 + 6f, 14f);
				formGraphics.DrawString(text10, font, brush2, 5f, num81);
			}
			if (text11.Length > 1)
			{
				float width8 = formGraphics.MeasureString(text11, font).Width;
				formGraphics.FillRectangle(brush4, 1f, num82, width8 + 6f, 14f);
				formGraphics.DrawString(text11, font, brush2, 5f, num82);
			}
			if (num5 == 1)
			{
				num11 += (float)(num73 - 100);
			}
			if (IncludeStarChart)
			{
				float num87 = (float)(Mv + 1.0);
				if (num87 < 10f)
				{
					num87 = 10f;
				}
				if (num87 > 16f)
				{
					num87 = 15.99f;
				}
				float num88 = 0.2f;
				string text17 = "1";
				if (Mv < 13.0)
				{
					num88 = 0.5f;
					text17 = "3";
				}
				if (Mv < 12.0)
				{
					num88 = 1f;
					text17 = "6";
				}
				if (Mv < 11.0)
				{
					num88 = 2f;
					text17 = "24";
				}
				text17 = "Motion in " + text17 + "hr steps";
				string text18 = $"{num88 * 60f:f0} arcmin square, to mag {num87:f1}";
				if ((double)num88 > 0.9)
				{
					text18 = $"{num88:f0}° square, to mag {num87:f1}";
				}
				Maps.StarMap(formGraphics, StarRA_2000, StarDec_2000, num8, num11, num7, num88, num87, VisualMag: true, GridLines: false, 0.0, FlipHorizontal: false, FlipVertical: false, use_Gaia: true, use_UCAC4: false, use_NOMAD: false, use_PPMXL: false, InBW || Printer, ForAsteroidWorldMap: true, 0.0, 0.0);
				float width9 = formGraphics.MeasureString(text18, font).Width;
				formGraphics.FillRectangle(brush4, num8 + (num7 - width9) / 2f - 3f, num11 - 12f, width9 + 6f, 11f);
				formGraphics.DrawString(text18, font, brush2, num8 + (num7 - width9) / 2f, num11 - 13f);
				width9 = formGraphics.MeasureString(text17, font).Width;
				formGraphics.FillRectangle(brush4, num8 + (num7 - width9) / 2f - 3f, num11 + num7, width9 + 6f, 11f);
				formGraphics.DrawString(text17, font, brush2, num8 + (num7 - width9) / 2f, num11 + num7 - 2f);
				pen8.DashPattern = new float[2] { 100f, 1f };
				pen8.Color = color;
				if (pen8.Color != Color.Black)
				{
					pen8.Width = 2f;
				}
				for (int l = 0; l < 11; l += 2)
				{
					if (l > 7)
					{
						if (pen8.Color == Color.Black)
						{
							pen8.Color = Color.FromArgb(70, 70, 70);
						}
						else
						{
							pen8.Color = Color.FromArgb(41, 125, 78);
						}
					}
					float num89 = num7 / 2f;
					Maps.StarMapOrthoProjection(PlotRAAsteroid[l], PlotDecAsteroid[l], out var x4, out var y3, out var xFromCentre, out var yFromCentre, 0.0, FlipHorizontal: false, FlipVertical: false);
					bool flag5 = (Math.Abs(xFromCentre) < num89) & (Math.Abs(yFromCentre) < num89);
					Maps.StarMapOrthoProjection(PlotRAAsteroid[l + 1], PlotDecAsteroid[l + 1], out var x5, out var y4, out var xFromCentre2, out var yFromCentre2, 0.0, FlipHorizontal: false, FlipVertical: false);
					bool flag6 = (Math.Abs(xFromCentre2) < num89) & (Math.Abs(yFromCentre2) < num89);
					float IntersectX;
					float IntersectY;
					if (flag5 && flag6)
					{
						formGraphics.DrawLine(pen8, x4, y3, x5, y4);
					}
					else if (!flag5 && flag6)
					{
						Intersection(xFromCentre2, yFromCentre2, xFromCentre, yFromCentre, num89, out IntersectX, out IntersectY);
						formGraphics.DrawLine(pen8, num8 + IntersectX + num89, num11 + IntersectY + num89, x5, y4);
					}
					else if (flag5 && !flag6)
					{
						Intersection(xFromCentre2, yFromCentre2, xFromCentre, yFromCentre, num89, out IntersectX, out IntersectY);
						formGraphics.DrawLine(pen8, x5, y4, num8 + IntersectX + num89, num11 + IntersectY + num89);
					}
				}
				pen8.Width = 1f;
			}
			float num90 = -1f;
			string text19 = "";
			if (ShowAsteroidRings & (NumOfRings > 0))
			{
				float num91 = num7 * 0.5f;
				float num92 = num8 + 1.5f * num91;
				if (num92 < (float)(ChartWidth / 2))
				{
					num92 -= num91;
				}
				float num93 = (float)ChartHeight - num11 - 0.3f * num91;
				if (num93 > (float)(ChartHeight / 2))
				{
					num93 = (float)ChartHeight - 0.5f * num91 - 8f;
				}
				formGraphics.FillRectangle(brush5, num92 - num91 / 2f, num93 - num91 / 2f, num91, num91);
				formGraphics.DrawRectangle(pen7, num92 - num91 / 2f, num93 - num91 / 2f, num91, num91);
				text19 = "Rings N";
				SizeF sizeF = formGraphics.MeasureString(text19, font);
				formGraphics.FillRectangle(brush4, num92 - sizeF.Width / 2f - 1f, num93 - num91 / 2f - sizeF.Height / 2f, sizeF.Width + 2f, sizeF.Height);
				formGraphics.DrawString(text19, font, brush, num92 - sizeF.Width / 2f, num93 - num91 / 2f - sizeF.Height / 2f - 2f);
				float num94 = 0f;
				for (int m = 0; m < 5; m++)
				{
					if (RingRadii[m] > 0.0)
					{
						if (num94 == 0f)
						{
							num94 = 2.3f * (float)RingRadii[m] / num91;
						}
						else if (2.3 * RingRadii[m] / (double)num91 > (double)num94)
						{
							num94 = 2.3f * (float)RingRadii[m] / num91;
						}
					}
				}
				float num95 = (float)(AsteroidDiameter / (double)num94 / 2.0);
				formGraphics.FillEllipse(brush2, num92 - num95, num93 - num95, 2f * num95, 2f * num95);
				string text20 = string.Format("{0,1:f0}km", Utilities.Integer_SignificantDigits(num91 * num94, 2));
				SizeF sizeF2 = formGraphics.MeasureString(text20, font);
				formGraphics.FillRectangle(brush4, num92 - sizeF2.Width / 2f - 1f, num93 + num91 / 2f - sizeF2.Height / 2f, sizeF2.Width + 2f, sizeF2.Height);
				formGraphics.DrawString(text20, font, brush, num92 - sizeF2.Width / 2f, num93 + num91 / 2f - sizeF2.Height / 2f + 1f);
				text20 = "W";
				sizeF2 = formGraphics.MeasureString(text20, font);
				formGraphics.FillRectangle(brush4, num92 - num91 / 2f - sizeF2.Width / 2f - 1f, num93 - sizeF2.Height / 2f - 1f, sizeF2.Width + 2f, sizeF2.Height + 2f);
				formGraphics.DrawString(text20, font, brush, num92 - num91 / 2f - sizeF2.Width / 2f, num93 - sizeF2.Height / 2f);
				text20 = "E";
				sizeF2 = formGraphics.MeasureString(text20, font);
				formGraphics.FillRectangle(brush4, num92 + num91 / 2f - sizeF2.Width / 2f - 1f, num93 - sizeF2.Height / 2f - 1f, sizeF2.Width + 2f, sizeF2.Height + 2f);
				formGraphics.DrawString(text20, font, brush, num92 + num91 / 2f - sizeF2.Width / 2f, num93 - sizeF2.Height / 2f);
				ArrayList arrayList = new ArrayList();
				float num96 = 0f;
				float num97 = 0f;
				float num98 = 0f;
				float num99 = 0f;
				for (int n = 0; n < 5; n++)
				{
					if (RingRadii[n] < 1.0)
					{
						continue;
					}
					arrayList.Clear();
					for (int num100 = 0; num100 <= 364; num100 += 4)
					{
						float num101 = (float)RingRadii[n] / num94;
						float num102 = num90 * (float)((double)num101 * Math.Cos((double)num100 / (180.0 / Math.PI)));
						float num103 = (float)((double)num101 * Math.Sin((double)num100 / (180.0 / Math.PI)) * Math.Sin(PlanetocentricRingLatitude));
						num96 = (float)Math.Sqrt(num102 * num102 + num103 * num103);
						float num104 = (float)((double)num92 + (double)num90 * ((double)(0f - num102) * Math.Cos(PARingPole) - (double)num103 * Math.Sin(PARingPole)));
						float num105 = (float)((double)num93 - (double)num103 * Math.Cos(PARingPole) + (double)num102 * Math.Sin(PARingPole));
						if ((num96 > num95 && num97 > num95) || num100 > 180)
						{
							arrayList.Add(new PointF(num104, num105));
						}
						else
						{
							if (num96 > num95 && num97 <= num95 && num100 > 0)
							{
								float num106 = (num95 - num97) / (num96 - num97);
								arrayList.Add(new PointF(num98 + num106 * (num104 - num98), num99 + num106 * (num105 - num99)));
								arrayList.Add(new PointF(num104, num105));
							}
							if (num96 <= num95 && num97 > num95)
							{
								float num106 = (num95 - num97) / (num96 - num97);
								arrayList.Add(new PointF(num98 + num106 * (num104 - num98), num99 + num106 * (num105 - num99)));
								if (arrayList.Count > 1)
								{
									PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
									formGraphics.DrawLines(pen10, points);
								}
								arrayList.Clear();
							}
						}
						num98 = num104;
						num99 = num105;
						num97 = num96;
					}
					if (arrayList.Count > 1)
					{
						PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
						formGraphics.DrawLines(pen10, points);
					}
				}
			}
			bool flag7 = false;
			if (Current_OccElements_Record >= 0)
			{
				for (int num107 = 0; num107 < OccElements[Current_OccElements_Record].AsteroidMoons.Count; num107++)
				{
					if ((OccElements[Current_OccElements_Record].AsteroidMoons[num107].I != 0.0) & (OccElements[Current_OccElements_Record].AsteroidMoons[num107].Perihelion != 0.0))
					{
						flag7 = true;
					}
				}
				if ((ShowAsteroidRings && flag7) & (NumOfMoons > 0))
				{
					float num108 = num7 * 0.5f;
					float num109 = num8 + 1.5f * num108;
					if (num109 < (float)(ChartWidth / 2))
					{
						num109 -= num108;
					}
					float num110 = (float)ChartHeight - num11 + 0.9f * num108;
					if (num110 > (float)(ChartHeight / 2))
					{
						num110 = (float)ChartHeight - 1.5f * num108 - 22f;
					}
					formGraphics.FillRectangle(brush5, num109 - num108 / 2f, num110 - num108 / 2f, num108, num108);
					formGraphics.DrawRectangle(pen7, num109 - num108 / 2f, num110 - num108 / 2f, num108, num108);
					text19 = "Moons N";
					SizeF sizeF3 = formGraphics.MeasureString(text19, font);
					formGraphics.FillRectangle(brush4, num109 - sizeF3.Width / 2f - 1f, num110 - num108 / 2f - sizeF3.Height / 2f, sizeF3.Width + 2f, sizeF3.Height);
					formGraphics.DrawString(text19, font, brush, num109 - sizeF3.Width / 2f, num110 - num108 / 2f - sizeF3.Height / 2f - 2f);
					float num111 = 0f;
					for (int num112 = 0; num112 < NumOfMoons; num112++)
					{
						if (OccElements[Current_OccElements_Record].AsteroidMoons[num112].A > 0.0)
						{
							if (num111 == 0f)
							{
								num111 = 2.3f * (float)OccElements[Current_OccElements_Record].AsteroidMoons[num112].A / num108;
							}
							else if (2.3 * OccElements[Current_OccElements_Record].AsteroidMoons[num112].A / (double)num108 > (double)num111)
							{
								num111 = 2.3f * (float)OccElements[Current_OccElements_Record].AsteroidMoons[num112].A / num108;
							}
						}
					}
					float num113 = (float)(AsteroidDiameter / (double)num111 / 2.0);
					formGraphics.FillEllipse(brush2, num109 - num113, num110 - num113, 2f * num113, 2f * num113);
					string text21 = string.Format("{0,1:f0}km", Utilities.Integer_SignificantDigits(num108 * num111, 2));
					SizeF sizeF4 = formGraphics.MeasureString(text21, font);
					formGraphics.FillRectangle(brush4, num109 - sizeF4.Width / 2f - 1f, num110 + num108 / 2f - sizeF4.Height / 2f, sizeF4.Width + 2f, sizeF4.Height);
					formGraphics.DrawString(text21, font, brush, num109 - sizeF4.Width / 2f, num110 + num108 / 2f - sizeF4.Height / 2f + 1f);
					text21 = "W";
					sizeF4 = formGraphics.MeasureString(text21, font);
					formGraphics.FillRectangle(brush4, num109 - num108 / 2f - sizeF4.Width / 2f - 1f, num110 - sizeF4.Height / 2f - 1f, sizeF4.Width + 2f, sizeF4.Height + 2f);
					formGraphics.DrawString(text21, font, brush, num109 - num108 / 2f - sizeF4.Width / 2f, num110 - sizeF4.Height / 2f);
					text21 = "E";
					sizeF4 = formGraphics.MeasureString(text21, font);
					formGraphics.FillRectangle(brush4, num109 + num108 / 2f - sizeF4.Width / 2f - 1f, num110 - sizeF4.Height / 2f - 1f, sizeF4.Width + 2f, sizeF4.Height + 2f);
					formGraphics.DrawString(text21, font, brush, num109 + num108 / 2f - sizeF4.Width / 2f, num110 - sizeF4.Height / 2f);
					ArrayList arrayList2 = new ArrayList();
					pen10.DashPattern = new float[2] { 4f, 1f };
					float num114 = 0f;
					float num115 = 0f;
					float num116 = 0f;
					float num117 = 0f;
					for (int num118 = 0; num118 < NumOfMoons; num118++)
					{
						if (OccElements[Current_OccElements_Record].AsteroidMoons[num118].A < 1.0 || ((OccElements[Current_OccElements_Record].AsteroidMoons[num118].I == 0.0) & (OccElements[Current_OccElements_Record].AsteroidMoons[num118].Node == 0.0)))
						{
							continue;
						}
						double Planetocentric_Latitude_deg;
						double PAPole_deg;
						if (OccElements[Current_OccElements_Record].AsteroidMoons[num118].ReferenceFrame == 1)
						{
							Utilities.PoleOrientationEcliptic(OccElements[Current_OccElements_Record].AsteroidMoons[num118].I, OccElements[Current_OccElements_Record].AsteroidMoons[num118].Node, StarRA_2000, StarDec_2000, out Planetocentric_Latitude_deg, out PAPole_deg);
						}
						else
						{
							Utilities.PoleOrientation(90.0 - OccElements[Current_OccElements_Record].AsteroidMoons[num118].I, OccElements[Current_OccElements_Record].AsteroidMoons[num118].Node - 90.0, StarRA_2000, StarDec_2000, out Planetocentric_Latitude_deg, out PAPole_deg);
						}
						Planetocentric_Latitude_deg /= 180.0 / Math.PI;
						PAPole_deg /= 180.0 / Math.PI;
						arrayList2.Clear();
						for (int num119 = 0; num119 <= 364; num119 += 4)
						{
							float num120 = (float)OccElements[Current_OccElements_Record].AsteroidMoons[num118].A / num111;
							float num121 = num90 * (float)((double)num120 * Math.Cos((double)num119 / (180.0 / Math.PI)));
							float num122 = (float)((double)num120 * Math.Sin((double)num119 / (180.0 / Math.PI)) * Math.Sin(Planetocentric_Latitude_deg));
							num114 = (float)Math.Sqrt(num121 * num121 + num122 * num122);
							float num123 = (float)((double)num109 + (double)num90 * ((double)(0f - num121) * Math.Cos(PAPole_deg) - (double)num122 * Math.Sin(PAPole_deg)));
							float num124 = (float)((double)num110 - (double)num122 * Math.Cos(PAPole_deg) + (double)num121 * Math.Sin(PAPole_deg));
							if ((num114 > num113 && num115 > num113) || num119 > 180)
							{
								arrayList2.Add(new PointF(num123, num124));
							}
							else
							{
								if (num114 > num113 && num115 <= num113 && num119 > 0)
								{
									float num125 = (num113 - num115) / (num114 - num115);
									arrayList2.Add(new PointF(num116 + num125 * (num123 - num116), num117 + num125 * (num124 - num117)));
									arrayList2.Add(new PointF(num123, num124));
								}
								if (num114 <= num113 && num115 > num113)
								{
									float num125 = (num113 - num115) / (num114 - num115);
									arrayList2.Add(new PointF(num116 + num125 * (num123 - num116), num117 + num125 * (num124 - num117)));
									if (arrayList2.Count > 1)
									{
										PointF[] points2 = (PointF[])arrayList2.ToArray(arrayList2[0]!.GetType());
										formGraphics.DrawLines(pen10, points2);
									}
									arrayList2.Clear();
								}
							}
							num116 = num123;
							num117 = num124;
							num115 = num114;
						}
						if (arrayList2.Count > 1)
						{
							PointF[] points2 = (PointF[])arrayList2.ToArray(arrayList2[0]!.GetType());
							formGraphics.DrawLines(pen10, points2);
						}
					}
				}
			}
			pen10.DashStyle = DashStyle.Solid;
			if (StellarDiameter_mas >= 0.20000000298023224 * AsteroidAngularDiameterMAS)
			{
				SizeF sizeF5 = default(SizeF);
				float num126 = 0.85f;
				float[] OccBrightness = new float[201];
				GetBrightness(StellarDiameter_mas, AsteroidAngularDiameterMAS, Mv, MAsteroid, ref OccBrightness);
				float num127 = num7 * 0.5f;
				float num128 = num8 + 1.5f * num127;
				if (num128 < (float)(ChartWidth / 2))
				{
					num128 -= num127;
				}
				float num129 = (float)ChartHeight - num11 - 0.3f * num127;
				if (num129 > (float)(ChartHeight / 2))
				{
					num129 = (float)ChartHeight - 2.5f * num127 - 32f;
				}
				float num130 = num127 / 201f;
				float num131 = num126 * num127;
				formGraphics.FillRectangle(brush5, num128 - num127 / 2f, num129 - num127 / 2f, num127, num127);
				for (float num132 = 0.8f; num132 >= 0f; num132 -= 0.2f)
				{
					float num133 = num129 - num126 * num127 / 2f + (1f - num132) * num131;
					text19 = $"{num132:.0}";
					formGraphics.DrawString(y: num133 - formGraphics.MeasureString(text19, font).Height / 2f - 2f, s: text19, font: font, brush: brush, x: num128 - num127 / 2f);
				}
				for (float num134 = 1f; num134 <= 3f; num134 += 1f)
				{
					float num133 = num129 - num126 * num127 / 2f + (float)(1.0 - Math.Pow(10.0, (double)(0f - num134) / 2.5)) * num131;
					text19 = $"{num134:0}";
					sizeF5 = formGraphics.MeasureString(text19, font);
					formGraphics.DrawString(text19, font, brush, num128 + num127 / 2f - sizeF5.Width, num133 - sizeF5.Height / 2f - 2f);
				}
				formGraphics.DrawRectangle(pen7, num128 - num127 / 2f, num129 - num127 / 2f, num127, num127);
				formGraphics.DrawLine(pen8, num128 - num127 / 2f, num129 - num126 * num127 / 2f, num128 + num127 / 2f, num129 - num126 * num127 / 2f);
				formGraphics.DrawLine(pen8, num128 - num127 / 2f, num129 + num126 * num127 / 2f, num128 + num127 / 2f, num129 + num126 * num127 / 2f);
				text19 = "Light Curve";
				sizeF5 = formGraphics.MeasureString(text19, font);
				formGraphics.FillRectangle(brush4, num128 - sizeF5.Width / 2f - 1f, num129 - num127 / 2f - sizeF5.Height / 2f, sizeF5.Width + 2f, sizeF5.Height);
				formGraphics.DrawString(text19, font, brush, num128 - sizeF5.Width / 2f, num129 - num127 / 2f - sizeF5.Height / 2f - 2f);
				ArrayList arrayList3 = new ArrayList();
				for (int num135 = 0; num135 <= 200; num135++)
				{
					float x6 = num128 + (float)(num135 - 100) * num130;
					float num133 = num129 - num126 * num127 / 2f + (1f - OccBrightness[num135]) * num131;
					arrayList3.Add(new PointF(x6, num133));
				}
				PointF[] points3 = (PointF[])arrayList3.ToArray(arrayList3[0]!.GetType());
				try
				{
					formGraphics.DrawLines(pen10, points3);
				}
				catch
				{
				}
			}
			MoonParameters(out StarMoonElongation_deg, out MoonPhase_percent);
			if (ElementsFrom540Format && AsteroidNumber > 0 && MagValues.AsteroidMagnitudeFileExists)
			{
				Utilities.QuickSolarElongation_PhaseAngle(EventJD, StarRA_2000, StarDec_2000, DistanceToAsteroid, out Elongation, out PhaseAngle, out var SolarDistObject);
				int colorMagnitudeRecord = MagValues.GetColorMagnitudeRecord(AsteroidNumber);
				if (colorMagnitudeRecord >= 0)
				{
					if (MagValues.ColorMagnitudeData[colorMagnitudeRecord].GetColorMagnitudes(StarRA_2000, StarDec_2000, SolarDistObject, DistanceToAsteroid, PhaseAngle / (180.0 / Math.PI), out MvAsteroid, out MrAsteroid, out MvAsteroidUncert, out MrAsteroidUncert) && !NearbyStarsMagAdjusted)
					{
						Mdrop = MvAsteroid - Utilities.CombinedMagnitude(Mv, MvAsteroid);
						MdropRed = MrAsteroid - Utilities.CombinedMagnitude(Mr, MrAsteroid);
					}
				}
				else
				{
					MvAsteroid = (MrAsteroid = -5.0);
				}
			}
			if (MvAsteroid == -5.0)
			{
				Mdrop = MAsteroid - Utilities.CombinedMagnitude(Mv, MAsteroid);
				MdropRed = MAsteroid - 0.45 - Utilities.CombinedMagnitude(Mr, MAsteroid - 0.45);
			}
			string text22 = "";
			if (Mv != 0.0)
			{
				text22 += string.Format(" Mv {0,3:F1}", Mv);
			}
			if ((Mr != 0.0) & (Mr < 20.0))
			{
				text22 += string.Format("; Mr {0,3:F1}", Mr);
			}
			if ((Mp != 0.0) & (Mp < 20.0))
			{
				text22 += string.Format("; [Mb {0,3:F1}]", Mp);
			}
			string text23 = "";
			if (NearbyStarsAll > 0)
			{
				text23 = string.Format(" [+{0,1} near", NearbyStarsAll);
				if (NearbyStarsBright > 0)
				{
					text23 += string.Format(",{0,1} bright", NearbyStarsBright);
				}
				text23 += "]";
			}
			text22 += text23;
			string s2 = " RA = " + RA + " (astrometric)";
			string s3 = "Dec = " + Dec + "   ...";
			string s4 = "[of Date: " + Utilities.DEGtoDMS(FPlaneRA * (180.0 / Math.PI) / 15.0, 2, 0, MinutesOnly: false) + ", " + Utilities.DEGtoDMS(FPlaneDec * (180.0 / Math.PI), 3, 0, MinutesOnly: false) + "]";
			text = "";
			if (Reliability == -2.0)
			{
				text += "Not reliable (from UBSC)";
			}
			else if (Reliability == -3.0)
			{
				text += "Reliable - position from UBSC";
			}
			else if (Reliability == -4.0)
			{
				text += "Hip2 posn. Check for a duplicate star";
			}
			else if (Reliability == -1.0)
			{
				text += "Reliable not available";
			}
			else if (Reliability == 0.0)
			{
				text += "Reliable not set in Gaia (problem)";
			}
			else
			{
				text += string.Format("Reliable {0,1:f1}", Reliability);
				text = (((Reliability > 0.0) & (Reliability < 1.41)) ? (text + " (good), ") : ((!((Reliability > 0.0) & (Reliability < 6.0))) ? (text + " (problem), ") : (text + " (beware), ")));
			}
			if (DuplicateSource == 1)
			{
				text += "DupSrc, ";
			}
			if (GaiaUCAC4PM == 1)
			{
				text += "Proper motion used UCAC4";
			}
			else if (NonGaiaPM == 1)
			{
				text += "Bad proper motion";
			}
			text2 = ((!(MaxDurn > 2.0)) ? string.Format("Durations: Max = {0,1:F2} secs", MaxDurn) : string.Format("Durations: Max = {0,1:F1} secs", MaxDurn));
			double num136 = (1.0 - Math.Pow(10.0, Mdrop / -2.5)) * 100.0;
			double num137 = (1.0 - Math.Pow(10.0, MdropRed / -2.5)) * 100.0;
			string text24 = "";
			text24 = (((MdropRed < 50.0) & (MdropRed > 0.0) & (MrAsteroid > 0.0)) ? ((!(Mdrop > 0.5)) ? string.Format("Mag Drop: {0,3:F2} [{1,2:F0}%]v, {2,3:F2} [{3,2:F0}%]r", Mdrop, num136, MdropRed, num137) : string.Format("Mag Drop: {0,3:F1} [{1,2:F0}%]v, {2,3:F1} [{3,2:F0}%]r", Mdrop, num136, MdropRed, num137)) : ((!(Mdrop > 0.5)) ? string.Format("Mag Drop: {0,3:F2} [{1,2:F0}%]v", Mdrop, num136) : string.Format("Mag Drop: {0,3:F1} [{1,2:F0}%]v", Mdrop, num136)));
			if (NearbyStarsMagAdjusted)
			{
				text24 += " [* nearby]";
			}
			string s5 = string.Format("Sun : Dist ={0,4:F0}°", SunElong);
			string s6 = string.Format("Moon: Dist ={0,4:F0}°, illum ={1,3:F0}%", StarMoonElongation_deg, MoonPhase_percent);
			string s7 = $"1σ Err: ±({ErrorMajor * 1000.0:F1} x {ErrorMinor * 1000.0:F1}) mas in PA {ErrorPA:F0}°";
			string text25 = (1 + Convert.ToInt32(MaxDurn / AsteroidDiameter < 1.0) + Convert.ToInt32(MaxDurn / AsteroidDiameter < 0.1)).ToString();
			string text26 = (1 + Convert.ToInt32(MaxDurn / AsteroidAngularDiameterMAS < 1.0) + Convert.ToInt32(MaxDurn / AsteroidAngularDiameterMAS < 0.1)).ToString();
			string s8 = string.Format("1km = {0,1:f" + text25 + "} secs, 1mas = {1,1:f" + text26 + "} secs", MaxDurn / AsteroidDiameter, MaxDurn / (AsteroidAngularDiameterMAS + StellarDiameter_mas));
			string s9 = ((!(MvAsteroid <= 0.0)) ? string.Format("        Mv = {0,3:F1}; Mr = {1,3:F1}", MvAsteroid, MrAsteroid) : string.Format("       Mag ={0,5:F1}", MAsteroid));
			string s10 = ((AsteroidDiameter < 4.0) ? string.Format("       Dia = {0,1:F2} ±{1,1:F2}km, {2,1:F1} mas", AsteroidDiameter, DiameterUncertainty, AsteroidAngularDiameterMAS) : ((!(AsteroidDiameter < 9.95)) ? string.Format("       Dia = {0,1:F0} ±{1,1:F0}km, {2,1:F0} mas", AsteroidDiameter, DiameterUncertainty, AsteroidAngularDiameterMAS) : string.Format("       Dia = {0,1:F1} ±{1,1:F1}km, {2,1:F1} mas", AsteroidDiameter, DiameterUncertainty, AsteroidAngularDiameterMAS)));
			string s11 = string.Format("  Parallax ={0,6:F3}\"", 8.794143836182533 / DistanceToAsteroid);
			string s12 = string.Format("Hourly dRA ={0,6:F3}s", deltaRA);
			string s13 = string.Format("      dDec ={0,6:F2}\"", deltaDec);
			string text27 = OrbitSourceDate;
			if (ErrorBasis.Length > 0)
			{
				text27 = text27 + ", " + ErrorBasis;
			}
			formGraphics.FillRectangle(brush4, 0, 0, ChartWidth, num3);
			formGraphics.DrawLine(pen7, 0, num3, ChartWidth, num3);
			formGraphics.DrawString(AsteroidName.Trim() + " occults " + StarNo.Trim() + text3, font2, brush, 5f, 5f);
			string text28 = "Star:";
			text28 = ((!(StellarDiameter_mas >= 0.1)) ? (text28 + "    (Dia < 0.1 mas)") : (text28 + "    (Dia = " + string.Format("{0,1:f1} mas)", StellarDiameter_mas)));
			formGraphics.DrawString(text28, font, brush, 5f, 20f);
			formGraphics.DrawString(text22, font, brush, 5f, 30f);
			formGraphics.DrawString(s2, font, brush, 5f, 40f);
			formGraphics.DrawString(s3, font, brush, 5f, 50f);
			formGraphics.DrawString(s4, font3, brush3, 5f, 60f);
			formGraphics.DrawString(PredictionDate, font, brush, 5f, 70f);
			formGraphics.DrawString(text, font, brush, 5f, 80f);
			float x7 = ChartWidth / 2 - 110;
			formGraphics.DrawString(text2, font, brush, x7, 20f);
			formGraphics.DrawString(s8, font, brush, x7, 30f);
			formGraphics.DrawString(text24, font, brush, x7, 40f);
			formGraphics.DrawString(s5, font, brush, x7, 50f);
			formGraphics.DrawString(s6, font, brush, x7, 60f);
			formGraphics.DrawString(s7, font, brush, x7, 70f);
			x7 = ChartWidth - 230;
			if (InISAM & InDAMIT)
			{
				formGraphics.DrawString(" Asteroid: (in DAMIT, ISAM)", font, brush2, x7, 20f);
			}
			else if (InDAMIT)
			{
				formGraphics.DrawString("     Asteroid:  (in DAMIT)", font, brush2, x7, 20f);
			}
			else if (InISAM)
			{
				formGraphics.DrawString("     Asteroid:  (in ISAM)", font, brush2, x7, 20f);
			}
			else
			{
				formGraphics.DrawString("     Asteroid:     ", font, brush, x7, 20f);
			}
			formGraphics.DrawString(s9, font, brush, x7, 30f);
			formGraphics.DrawString(s10, font, brush, x7, 40f);
			formGraphics.DrawString(s11, font, brush, x7, 50f);
			formGraphics.DrawString(s12, font, brush, x7, 60f);
			formGraphics.DrawString(s13, font, brush, x7, 70f);
			formGraphics.DrawString(text27, font, brush, (float)ChartWidth - formGraphics.MeasureString(text27, font).Width - 5f, 80f);
			string text29 = "Occult " + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version;
			float width10 = formGraphics.MeasureString(text29, font).Width;
			formGraphics.FillRectangle(brush4, 1f, ChartHeight - 15, width10 + 6f, 14f);
			formGraphics.DrawString(text29, font, brush, 3f, ChartHeight - 12);
		}

		internal static void AsteroidPath_For_Plots(double PathStep, bool Multipath)
		{
			AsteroidPath(PathStep, UseLocalTopography: false, 0.0, 0.0);
			int count = PathCoordinates.Count;
			if (!Multipath)
			{
				StreamWriter streamWriter = new StreamWriter(AppPath + "\\Generated Files\\path.tmp", append: false);
				for (int i = 0; i < count; i++)
				{
					streamWriter.WriteLine(PathCoordinates[i].ToString_PathForPlot());
				}
				streamWriter.Close();
			}
			else if (count >= 1)
			{
				StreamWriter streamWriter2 = new StreamWriter(AppPath + "\\Generated Files\\multipath.tmp", append: true);
				for (int j = 0; j < count; j++)
				{
					streamWriter2.WriteLine(PathCoordinates[j].ToString_PathForPlot());
				}
				if (AsteroidNumber > 0)
				{
					streamWriter2.WriteLine("*****" + UTDate + " (" + AsteroidNumber + ")");
				}
				else if ((AsteroidName.Substring(0, 1) == "P") & (AsteroidName.Substring(2, 1) == "M"))
				{
					streamWriter2.WriteLine("*****" + UTDate + " (" + AsteroidName.Substring(0, 5) + ")");
				}
				else
				{
					streamWriter2.WriteLine("*****" + UTDate + " (" + AsteroidName.Trim() + ")");
				}
				streamWriter2.Close();
			}
		}

		internal static void MultipathKMZfile(bool OnlyWhereSunIsDown_StarIsUp, bool Show)
		{
			//IL_0063: Unknown result type (might be due to invalid IL or missing references)
			//IL_0069: Invalid comparison between Unknown and I4
			//IL_009e: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a4: Invalid comparison between Unknown and I4
			int num = ListAndDisplay.lstSummary.get_Items().get_Count() - 4;
			num -= num / 6;
			string text = "You have requested a GoogleEarth plot of " + num + " occultations paths.\r\n\r\nThis is a large number of paths, and they may appear too crowded on GoogleEarth to be of any use.";
			if ((num > 60) & !OccultationElements.TestInRegion)
			{
				text += "\r\n\r\nAre you sure you want to continue ?";
				if ((int)MessageBox.Show(text, "Too many paths?", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
				{
					return;
				}
			}
			else if ((num > 15) & (OccultationElements.TestInRegion | OccultationElements.TestSite))
			{
				text += "\r\n\r\nThis is particularly the case if the paths all come close to a specified location.\r\n\r\nAre you sure you want to continue ?";
				if ((int)MessageBox.Show(text, "Too many paths?", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
				{
					return;
				}
			}
			ArrayList arrayList = new ArrayList();
			string text2 = "";
			int num2 = 0;
			double num3 = 0.0;
			double num4 = 0.0;
			double longitude = 0.0;
			double num5 = 100.0;
			double longitude2 = 0.0;
			double num6 = 100.0;
			double longitude3 = 0.0;
			double latitude = 100.0;
			string[] array = new string[4] { "Left", "Right", "LeftErr", "RightErr" };
			bool flag = false;
			bool flag2 = false;
			bool flag3 = false;
			int num7 = 0;
			int num8 = 3;
			if (!GoogleEarth.Create_New_GoogleEarthKMZ_File(AppPath + "\\Generated Files\\Multipath", "Multipath", Show, out var CreatedFile))
			{
				return;
			}
			CreateMultiPathFile(OnlyWhereSunIsDown_StarIsUp);
			int num9 = 0;
			if (new FileInfo(AppPath + "\\Generated Files\\multipath.tmp").Length <= 150)
			{
				return;
			}
			using (StreamReader streamReader = new StreamReader(AppPath + "\\Generated Files\\multipath.tmp"))
			{
				do
				{
					string text3 = streamReader.ReadLine()!.PadRight(4);
					if (text3.Substring(0, 3) == "+++")
					{
						arrayList.Clear();
						num2 = 0;
						num5 = (num6 = (latitude = 100.0));
						flag2 = false;
						flag3 = false;
						text2 = text3.Substring(5);
					}
					else if (text3.Substring(0, 3) == "***")
					{
						if (num2 <= 0)
						{
							continue;
						}
						for (int i = num7; i <= num8; i++)
						{
							GoogleEarth.Write_TagsAndLabel_For_New_GoogleEarthKMZ_Path(num9 % 16, text2 + " " + array[i], i < 2);
							for (int j = 0; j < num2; j++)
							{
								string[] array2 = arrayList[j]!.ToString()!.Split(new char[1] { ',' });
								if (array2.Length <= 10)
								{
									continue;
								}
								num3 = double.Parse(array2[3 + 2 * i]);
								num4 = double.Parse(array2[4 + 2 * i]);
								flag = num4 < 100.0;
								if (flag)
								{
									GoogleEarth.Write_Path_Coordinate_GoogleEarthKMZ(num3, num4, 0.0);
								}
								if (num5 > 90.0 && j == num2 / 2 && flag)
								{
									longitude = num3;
									num5 = num4;
								}
								if (num6 > 90.0 && flag)
								{
									longitude2 = num3;
									num6 = num4;
								}
								if (!flag2)
								{
									if (flag)
									{
										longitude3 = num3;
										latitude = num4;
									}
									if (flag3 && !flag)
									{
										flag2 = true;
									}
									flag3 = flag;
								}
							}
							GoogleEarth.Write_Tags_At_End_of_Path_GoogleEarthKMZ();
						}
						GoogleEarth.Write_PlaceMark_PinPlusName_GoogleEarthKML(text2, longitude, num5);
						GoogleEarth.Write_PlaceMark_PinPlusName_GoogleEarthKML(text2, longitude2, num6);
						GoogleEarth.Write_PlaceMark_PinPlusName_GoogleEarthKML(text2, longitude3, latitude);
						num9++;
					}
					else
					{
						arrayList.Add(text3);
						num2++;
					}
				}
				while (!streamReader.EndOfStream);
			}
			CreatedFile = GoogleEarth.Close_GoogleEarthKMZ_File(CreatedFile, ConvertToKMZ: true);
			if (Show)
			{
				GoogleEarth.DisplayGoogleMap(CreatedFile);
			}
		}

		internal static void DisplayAsteroidCoordinatesForm()
		{
			try
			{
				((Control)PathCoordsForm).Show();
			}
			catch
			{
				PathCoordsForm = new AsteroidPath();
				((Control)PathCoordsForm).Show();
			}
			PathCoordsForm.DisplayPathCoords();
		}

		internal static void TopographyEnabledInPathCoords(bool Enabled)
		{
			((Control)PathCoordsForm.pnlTopography).set_Enabled(Enabled);
			((Control)PathCoordsForm.panelWarn).set_Visible(!Enabled);
			((Control)PathCoordsForm.cmdHelp).set_Visible(!Enabled);
			if (Enabled)
			{
				((Control)PathCoordsForm.cmdComputeTopo).set_BackColor(Color.FromArgb(128, 255, 128));
				((Control)PathCoordsForm.cmdComputeTopo).set_Text("Compute using\r\nlocal topography\r\nat 0.75 arcmin steps");
			}
			else
			{
				((Control)PathCoordsForm.cmdComputeTopo).set_BackColor(Color.LightGray);
				((Control)PathCoordsForm.cmdComputeTopo).set_Text("Topographic plot\r\nnot available");
			}
		}

		internal static void AsteroidPath(double PathStep, bool UseLocalTopography, double OffsetX, double OffsetY)
		{
			AsteroidPath(PathStep, UseLocalTopography, 0, 0, 0, IterateByTimeOnly: false);
		}

		internal static void AsteroidPath(double PathStep, bool UseLocalTopography, int NumberOfFences_Left, int NumberOfFences_Right, int FenceSpacing_meters, bool IterateByTimeOnly)
		{
			double num = 0.0;
			double num2 = 1.0;
			bool flag = false;
			double xatmin = Xatmin;
			double yatMin = YatMin;
			int num3 = -1;
			if (UseLocalTopography)
			{
				PathStep = 0.0125;
			}
			PathCoordinates = new List<AsteroidPathCoords>();
			AsteroidPathCoords CalculatedPath = new AsteroidPathCoords();
			double num4 = Math.Floor(SubstellarLongitude);
			double num5 = PathStep;
			bool flag2 = false;
			double num6 = 10.0 * Math.Floor((num4 - 180.0) / 10.0);
			double num7 = num4 + 180.0;
			double num8 = 0.0;
			double num9 = YatMin - Xatmin / DeltaX * DeltaY;
			double num10 = 0.9966 * Math.Cos(FPlaneDec) * (double)Math.Sign(FPlaneDec);
			double value = Xatmin + (num10 - YatMin) / DeltaY * DeltaX;
			num2 = 0.017 * PathStep / n / 3.0;
			double num11 = AsteroidShadowDiameter_Penumbral / 2.0 / 6378.137;
			bool flag3 = false;
			if (((Delta2X / DeltaX < -0.02) & (Math.Abs(DeltaX / DeltaY) > 10.0)) | ((Delta2Y / DeltaY < -0.02) & (Math.Abs(DeltaY / DeltaX) > 10.0)))
			{
				flag3 = true;
			}
			double num12 = Math.Abs((Xatmin * DeltaY - YatMin * DeltaX) / n);
			double num13 = 1.0 + num11;
			double num14 = 1.3 * (Math.Sqrt(num13 * num13 - num12 * num12) / n);
			if (num14 < EarthSemiDuration * 2.6)
			{
				num14 = 2.6 * EarthSemiDuration;
			}
			double num15 = 1.0;
			if (Math.Abs(num12) < 1.4)
			{
				num15 = Math.Sqrt(1.0 - num12 * num12 / 2.0);
			}
			double num16 = 1000.0 * num15;
			if (UseLocalTopography)
			{
				num16 = 10000.0;
			}
			if (flag3)
			{
				num14 *= 3.0;
				num16 *= 3.0;
			}
			num2 = num14 / num16;
			bool ValidPath;
			if (((IterateByTimeOnly | (EarthSemiDuration > 1.0)) || flag3) | (((Math.Sign(num9) == Math.Sign(num10)) & (Math.Abs(num9) > 0.8 * Math.Abs(num10))) | (Math.Abs(value) < 0.35)) | (Math.Abs(StarDec_2000) > 0.5) | (num12 + Ratio_1Sigma_to_Limit_Distances * num11 > 0.95))
			{
				if (num12 < num13)
				{
					double num17 = 0.0;
					for (int i = 0; i < 10; i++)
					{
						double num18 = num17;
						double num19 = DeltaX + Delta2X * num17 + Delta3X * num17 * num17;
						double num20 = DeltaY + Delta2Y * num17 + Delta3Y * num17 * num17;
						n = Math.Sqrt(num19 * num19 + num20 * num20);
						num17 = (0.0 - (Xatmin * num19 + YatMin * DeltaY)) / n / n;
						if (Math.Abs(num18 - num17) < 0.05)
						{
							break;
						}
					}
					for (double num21 = 0.0 - num14; num21 <= num14; num21 += num2)
					{
						PathByTime(num21 + num17, num11, UseLocalTopography, NumberOfFences_Left, NumberOfFences_Right, FenceSpacing_meters, ref CalculatedPath, out ValidPath);
						if (ValidPath)
						{
							PathCoordinates.Add(CalculatedPath);
						}
					}
				}
			}
			else
			{
				if (Math.Abs(num9) > 0.9966 && (((Math.Abs(DeltaX) * Math.Cos(FPlaneDec) < 1.5 * Math.Abs(DeltaY)) | (Math.Abs(num9 - Math.Abs(num10) * (double)Math.Sign(num9)) < 0.15 - num11)) & ((Math.Sign(num9) != Math.Sign(num10)) | (Math.Abs(num9) > Math.Abs(num10)) | ((Math.Sign(num9) == Math.Sign(num10)) & (Math.Abs(num9) < Math.Abs(num10))))))
				{
					flag2 = false;
					bool flag4 = false;
					double num22 = 0.0 - (90.0 - num5);
					double num23 = 90.0 + num5;
					double num24 = num5;
					for (double num25 = num22; Math.Sign(num23 - num25) == Math.Sign(num23); num25 += num24)
					{
						num8 = num25 * (double)Math.Sign(FPlaneDec);
						PathByLatitude(num8, SubstellarLongitude, UseLocalTopography, NumberOfFences_Left, NumberOfFences_Right, FenceSpacing_meters, num11, ref CalculatedPath, out ValidPath);
						if (ValidPath)
						{
							PathCoordinates.Add(CalculatedPath);
						}
						if (ValidPath && !flag)
						{
							flag = true;
						}
						if (!ValidPath && flag)
						{
							break;
						}
						if (flag4)
						{
							double num26;
							for (num26 = CalculatedPath.LongitudeCentre - num; num26 > 180.0; num26 -= 360.0)
							{
							}
							for (; num26 < -180.0; num26 += 360.0)
							{
							}
							if (Math.Abs(num26 * Math.Cos(num8 / (180.0 / Math.PI))) > 1.3 * num24)
							{
								if (num3 < 0)
								{
									num3 = PathCoordinates.Count - 1;
								}
								if (num26 != 0.0)
								{
									num5 = Math.Abs(num5) * (double)Math.Sign(num26);
								}
								num6 = ((!(num5 < 0.0)) ? Math.Ceiling(CalculatedPath.LongitudeCentre) : Math.Floor(CalculatedPath.LongitudeCentre));
								num7 = num6 + (double)(270 * Math.Sign(num5));
							}
						}
						if (ValidPath & (CalculatedPath.LatitudeCentre < 100.0))
						{
							num6 = CalculatedPath.LongitudeCentre;
						}
						if (CalculatedPath.CentreLineValid)
						{
							num = CalculatedPath.LongitudeCentre;
							flag4 = true;
						}
					}
				}
				if (!flag2)
				{
					double StartLatitude = num8;
					flag = false;
					if (PathCoordinates.Count > 10)
					{
						int num27 = PathCoordinates.Count - 10;
						if (num3 > 0)
						{
							while ((PathCoordinates.Count > num3) & (PathCoordinates.Count > num27))
							{
								PathCoordinates.RemoveAt(PathCoordinates.Count - 1);
							}
						}
						double num28 = PathCoordinates[PathCoordinates.Count - 1].LongitudeCentre - PathCoordinates[PathCoordinates.Count - 2].LongitudeCentre;
						num6 = ((PathCoordinates[PathCoordinates.Count - 1].LongitudeCentre < 0.0) ? ((!(num28 < 0.0)) ? Math.Ceiling(PathCoordinates[PathCoordinates.Count - 1].LongitudeCentre) : Math.Floor(PathCoordinates[PathCoordinates.Count - 1].LongitudeCentre)) : ((!(num28 < 0.0)) ? Math.Floor(PathCoordinates[PathCoordinates.Count - 1].LongitudeCentre) : Math.Ceiling(PathCoordinates[PathCoordinates.Count - 1].LongitudeCentre)));
						if (num28 != 0.0)
						{
							num5 = Math.Abs(num5) * (double)Math.Sign(num28);
						}
					}
					if (num5 > 0.0)
					{
						for (double num29 = num6; num29 <= num7; num29 += num5)
						{
							IterateOnLongitude(num11, ref StartLatitude, UseLocalTopography, NumberOfFences_Left, NumberOfFences_Right, FenceSpacing_meters, ref CalculatedPath, num29, out ValidPath);
							if (ValidPath)
							{
								PathCoordinates.Add(CalculatedPath);
							}
							if (ValidPath && !flag)
							{
								flag = true;
							}
							if (!ValidPath && flag)
							{
								flag2 = true;
								break;
							}
						}
					}
					else
					{
						for (double num30 = num6; num30 >= num7; num30 += num5)
						{
							IterateOnLongitude(num11, ref StartLatitude, UseLocalTopography, NumberOfFences_Left, NumberOfFences_Right, FenceSpacing_meters, ref CalculatedPath, num30, out ValidPath);
							if (ValidPath)
							{
								PathCoordinates.Add(CalculatedPath);
							}
							if (ValidPath && !flag)
							{
								flag = true;
							}
							if (!ValidPath && flag)
							{
								flag2 = true;
								break;
							}
						}
					}
				}
				if (!flag2)
				{
					PathCoordinates.Clear();
					if (num12 < num13)
					{
						for (double num21 = 0.0 - num14; num21 <= num14; num21 += num2)
						{
							PathByTime(num21, num11, UseLocalTopography, NumberOfFences_Left, NumberOfFences_Right, FenceSpacing_meters, ref CalculatedPath, out ValidPath);
							if (ValidPath)
							{
								PathCoordinates.Add(CalculatedPath);
							}
						}
					}
				}
			}
			if (!UseLocalTopography)
			{
				for (int j = 0; j < PathCoordinates.Count - 1; j++)
				{
					if (PathCoordinates[j].CentreLineValid & PathCoordinates[j + 1].CentreLineValid)
					{
						PathCoordinates[j + 1].TanZ = TanZshift(PathCoordinates[j + 1].LatitudeCentre / (180.0 / Math.PI), PathCoordinates[j + 1].LongitudeCentre / (180.0 / Math.PI), PathCoordinates[j].LatitudeCentre / (180.0 / Math.PI), PathCoordinates[j].LongitudeCentre / (180.0 / Math.PI), Math.Tan((90.0 - PathCoordinates[j + 1].StarAlt) / (180.0 / Math.PI)), PathCoordinates[j + 1].StarAz / (180.0 / Math.PI));
					}
				}
			}
			Xatmin = xatmin;
			YatMin = yatMin;
		}

		private static void IterateOnLongitude(double ShadowRadius, ref double StartLatitude, bool UseLocalTopography, int NumberOfFences_Left, int NumberOfFences_Right, int FenceSpacing_meters, ref AsteroidPathCoords CalculatedPath, double PathLongitude_Deg, out bool ValidPath)
		{
			PathByLongitude(PathLongitude_Deg, StartLatitude, UseLocalTopography, NumberOfFences_Left, NumberOfFences_Right, FenceSpacing_meters, ShadowRadius, ref CalculatedPath, out ValidPath);
			if (ValidPath & (CalculatedPath.StarAlt > 5.0))
			{
				StartLatitude = CalculatedPath.LatitudeCentre;
			}
		}

		internal static void PathByLongitude(double PathLongitude_deg, double StartLatitude_deg, bool UseLocalTopography, double ShadowRadius, ref AsteroidPathCoords CalculatedPath, out bool ValidPath)
		{
			PathByLongitude(PathLongitude_deg, StartLatitude_deg, UseLocalTopography, 0, 0, 0, ShadowRadius, ref CalculatedPath, out ValidPath);
		}

		internal static void PathByLongitude(double PathLongitude_deg, double StartLatitude_deg, bool UseLocalTopography, int NumberOfFences_Left, int NumberOfFences_Right, int FenceSpacing_meters, double ShadowRadius, ref AsteroidPathCoords CalculatedPath, out bool ValidPath)
		{
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			double num4 = 0.0;
			double num5 = 0.0;
			double num6 = 0.0;
			FenceLabels = new List<string>();
			string text = "";
			List<double> list = new List<double>
			{
				0.0,
				-1.0,
				1.0,
				0.0 - Ratio_1Sigma_to_Limit_Distances,
				Ratio_1Sigma_to_Limit_Distances,
				(0.0 - AsteroidShadowDiameter_Umbral) / AsteroidShadowDiameter_Penumbral,
				AsteroidShadowDiameter_Umbral / AsteroidShadowDiameter_Penumbral,
				0.0 - PathAugmentation_Rings,
				PathAugmentation_Rings
			};
			if (Math.Abs(JD_for_EOP - EventJD) > 10.0)
			{
				Utilities.EarthOrientationParameters(EventJD, out EOPx, out EOPy, out dUT1);
				JD_for_EOP = EventJD;
			}
			if (UseLocalTopography)
			{
				for (int i = -NumberOfFences_Right; i <= NumberOfFences_Left; i++)
				{
					if (i == 0)
					{
						continue;
					}
					double num7 = (double)(i * 2 * FenceSpacing_meters) / AsteroidShadowDiameter_Penumbral / 1000.0;
					if (!(Math.Abs(list[1] - num7) < 0.02) && !(Math.Abs(list[2] - num7) < 0.02) && !(Math.Abs(list[3] - num7) < 0.02) && !(Math.Abs(list[4] - num7) < 0.02))
					{
						list.Add((double)(i * 2 * FenceSpacing_meters) / AsteroidShadowDiameter_Penumbral / 1000.0);
						text = string.Format("Fence @ {0,1:f0} m", Math.Abs(i * FenceSpacing_meters));
						if (num7 < 0.0)
						{
							FenceLabels.Add(text + " right");
						}
						else
						{
							FenceLabels.Add(text + " left");
						}
					}
				}
			}
			ValidPath = false;
			CalculatedPath = new AsteroidPathCoords();
			CalculatedPath.Fences.Clear();
			CalculatedPath.PathHasRings = PathAugmentation_Rings > 0.0;
			CalculatedPath.ShowUmbralPath = AsteroidShadowDiameter_Umbral < 0.8 * AsteroidShadowDiameter_Penumbral;
			for (int j = 0; j < list.Count - 9; j++)
			{
				Fences item = new Fences();
				CalculatedPath.Fences.Add(item);
			}
			CalculatedPath.IterateOnT = false;
			CalculatedPath.IterateOnLatitude = false;
			if (PathLongitude_deg < -180.0)
			{
				PathLongitude_deg += 360.0;
			}
			if (PathLongitude_deg > 180.0)
			{
				PathLongitude_deg -= 360.0;
			}
			double num8 = dUT1 / 240.0;
			num6 = PathLongitude_deg + num8;
			CalculatedPath.PathIsTopographic = UseLocalTopography;
			Earth2014Present = File.Exists(EarthTopography.FileName);
			if (Earth2014Present)
			{
				EarthTopography.OpenEarthFile();
			}
			for (int k = 0; k < list.Count; k++)
			{
				if (((k == 5 || k == 6) && !CalculatedPath.ShowUmbralPath) || ((k == 7 || k == 8) && !AllowAsteroidRings))
				{
					continue;
				}
				ShadowRadius = AsteroidShadowDiameter_Penumbral / 2.0 / 6378.137 * list[k];
				if (Math.Abs(MinGeocentricD + ShadowRadius * (double)Math.Sign(DeltaX * Y0)) > 1.01)
				{
					continue;
				}
				double Ta = 0.0;
				int num9 = 0;
				double Lat_Deg = StartLatitude_deg;
				double num10 = 0.0;
				double pCos;
				double pSin;
				double H;
				double MinDistance;
				double Z;
				double ZSun;
				do
				{
					if (Math.Abs(Lat_Deg) == 90.0)
					{
						Lat_Deg -= 0.01 * (double)Math.Sign(Lat_Deg);
					}
					while (Math.Abs(Lat_Deg) > 90.0)
					{
						Lat_Deg = (double)(180 * Math.Sign(Lat_Deg)) - Lat_Deg;
					}
					Utilities.Get_pSin_pCos(num6 / (180.0 / Math.PI), Lat_Deg / (180.0 / Math.PI), num10, out pCos, out pSin);
					GetTforMinimumDistance(num6, pCos, pSin, ShadowRadius, ref Ta, out H, out MinDistance, PathDistance: false, out var _, out TClosest, out Z, out ZSun, out var _);
					if (num9 == 0)
					{
						num9++;
						num = MinDistance;
						num2 = Lat_Deg;
						num4 = 120.0;
						Lat_Deg += 1.0;
					}
					else
					{
						num9++;
					}
					if (num9 > 1)
					{
						if (MinDistance == num)
						{
							num9 = 20;
						}
						if (num9 < 20)
						{
							num3 = (Lat_Deg - num2) / (num - MinDistance) * MinDistance;
							if (Math.Abs(num3) > num4)
							{
								break;
							}
							num2 = Lat_Deg;
							num = MinDistance;
							Lat_Deg += num3;
							num4 = Math.Abs(num3);
						}
					}
					if (num9 > 19)
					{
						break;
					}
					if (UseLocalTopography)
					{
						num10 = EarthTopography.Elevation_Above_MSL(PathLongitude_deg, Lat_Deg);
					}
				}
				while (Math.Abs(num3) > 5E-05 || num9 < 2);
				Z = pSin * Math.Sin(FPlaneDec) + pCos * Math.Cos(H) * Math.Cos(FPlaneDec);
				double num11 = Xatmin + DeltaX * Ta + Delta2X * Ta * Ta + Delta3X * Ta * Ta * Ta;
				double num12 = YatMin + DeltaY * Ta + Delta2Y * Ta * Ta + Delta3Y * Ta * Ta * Ta;
				if (Math.Sqrt(num11 * num11 + num12 * num12) > 1.0 + Math.Abs(ShadowRadius))
				{
					Z = -1.0;
				}
				ZSun = pSin * Math.Sin(SubSolarLatitude_OfDate / (180.0 / Math.PI)) + pCos * Math.Cos(H + (SubstellarLongitude - SubSolarLongitude_OfDate) / (180.0 / Math.PI)) * Math.Cos(SubSolarLatitude_OfDate / (180.0 / Math.PI));
				if (!((((Z > 0.0) & (Math.Abs(num3) <= 0.0005)) && num9 < 20) & (Math.Abs(MinDistance) < 0.01)))
				{
					continue;
				}
				ValidPath = true;
				num5 = PathLongitude_deg;
				Utilities.ConvertLatLong_forEOP(ref num5, ref Lat_Deg, EOPx, EOPy);
				switch (k)
				{
				case 0:
				{
					CalculatedPath.LongitudeCentre = num5;
					CalculatedPath.LatitudeCentre = Lat_Deg;
					CalculatedPath.AltitudeCentre = num10;
					CalculatedPath.TCentre = MidTime + Ta;
					CalculatedPath.CentreLineValid = true;
					double y = (0.0 - Math.Cos(FPlaneDec)) * Math.Sin(H);
					double x = pCos * Math.Sin(FPlaneDec) - pSin * Math.Cos(H) * Math.Cos(FPlaneDec);
					double num13 = 180.0 / Math.PI * Math.Atan2(y, x);
					if (num13 < 0.0)
					{
						num13 += 360.0;
					}
					CalculatedPath.StarAz = num13;
					CalculatedPath.StarAlt = Math.Asin(Z) * (180.0 / Math.PI);
					CalculatedPath.SunAlt = Math.Asin(ZSun) * (180.0 / Math.PI);
					break;
				}
				case 1:
					CalculatedPath.LongitudeRight = num5;
					CalculatedPath.LatitudeRight = Lat_Deg;
					CalculatedPath.AltitudeRight = num10;
					break;
				case 2:
					CalculatedPath.LongitudeLeft = num5;
					CalculatedPath.LatitudeLeft = Lat_Deg;
					CalculatedPath.AltitudeLeft = num10;
					break;
				case 3:
					CalculatedPath.LongitudeRightSigma = num5;
					CalculatedPath.LatitudeRightSigma = Lat_Deg;
					CalculatedPath.AltitudeRightSigma = num10;
					break;
				case 4:
					CalculatedPath.LongitudeLeftSigma = num5;
					CalculatedPath.LatitudeLeftSigma = Lat_Deg;
					CalculatedPath.AltitudeLeftSigma = num10;
					break;
				case 5:
					CalculatedPath.LongitudeRightUmbra = num5;
					CalculatedPath.LatitudeRightUmbra = Lat_Deg;
					CalculatedPath.AltitudeRightUmbra = num10;
					break;
				case 6:
					CalculatedPath.LongitudeLeftUmbra = num5;
					CalculatedPath.LatitudeLeftUmbra = Lat_Deg;
					CalculatedPath.AltitudeLeftUmbra = num10;
					break;
				case 7:
					CalculatedPath.LongitudeRightRings = num5;
					CalculatedPath.LatitudeRightRings = Lat_Deg;
					CalculatedPath.AltitudeRightRings = num10;
					break;
				case 8:
					CalculatedPath.LongitudeLeftRings = num5;
					CalculatedPath.LatitudeLeftRings = Lat_Deg;
					CalculatedPath.AltitudeLeftRings = num10;
					break;
				default:
					CalculatedPath.Fences[k - 9].Longitude = num5;
					CalculatedPath.Fences[k - 9].Latitude = Lat_Deg;
					CalculatedPath.Fences[k - 9].Altitude = num10;
					break;
				}
			}
			if (Earth2014Present)
			{
				EarthTopography.CloseEarthFile();
			}
		}

		internal static void PathByLatitude(double PathLatitude_deg, double StartLongitude_deg, bool UseLocalTopography, double ShadowRadius, ref AsteroidPathCoords CalculatedPath, out bool ValidPath)
		{
			PathByLatitude(PathLatitude_deg, StartLongitude_deg, UseLocalTopography, 0, 0, 0, ShadowRadius, ref CalculatedPath, out ValidPath);
		}

		internal static void PathByLatitude(double PathLatitude_deg, double StartLongitude_deg, bool UseLocalTopography, int NumberOfFences_Left, int NumberOfFences_Right, int FenceSpacing_meters, double ShadowRadius, ref AsteroidPathCoords CalculatedPath, out bool ValidPath)
		{
			double num = 0.0;
			double num2 = 0.0;
			double value = 90.0;
			double num3 = 0.0;
			FenceLabels = new List<string>();
			string text = "";
			List<double> list = new List<double>
			{
				0.0,
				-1.0,
				1.0,
				0.0 - Ratio_1Sigma_to_Limit_Distances,
				Ratio_1Sigma_to_Limit_Distances,
				(0.0 - AsteroidShadowDiameter_Umbral) / AsteroidShadowDiameter_Penumbral,
				AsteroidShadowDiameter_Umbral / AsteroidShadowDiameter_Penumbral,
				0.0 - PathAugmentation_Rings,
				PathAugmentation_Rings
			};
			if (Math.Abs(JD_for_EOP - EventJD) > 10.0)
			{
				Utilities.EarthOrientationParameters(EventJD, out EOPx, out EOPy, out dUT1);
				JD_for_EOP = EventJD;
			}
			if (UseLocalTopography)
			{
				for (int i = -NumberOfFences_Right; i <= NumberOfFences_Left; i++)
				{
					if (i == 0)
					{
						continue;
					}
					double num4 = (double)(i * 2 * FenceSpacing_meters) / AsteroidShadowDiameter_Penumbral / 1000.0;
					if (!(Math.Abs(list[1] - num4) < 0.02) && !(Math.Abs(list[2] - num4) < 0.02) && !(Math.Abs(list[3] - num4) < 0.02) && !(Math.Abs(list[4] - num4) < 0.02))
					{
						list.Add((double)(i * 2 * FenceSpacing_meters) / AsteroidShadowDiameter_Penumbral / 1000.0);
						text = string.Format("Fence @ {0,1:f0} m", Math.Abs(i * FenceSpacing_meters));
						if (num4 < 0.0)
						{
							FenceLabels.Add(text + " right");
						}
						else
						{
							FenceLabels.Add(text + " left");
						}
					}
				}
			}
			ValidPath = false;
			CalculatedPath = new AsteroidPathCoords();
			CalculatedPath.PathHasRings = PathAugmentation_Rings > 0.0;
			CalculatedPath.ShowUmbralPath = AsteroidShadowDiameter_Umbral < 0.8 * AsteroidShadowDiameter_Penumbral;
			CalculatedPath.Fences.Clear();
			for (int j = 0; j < list.Count - 9; j++)
			{
				Fences item = new Fences();
				CalculatedPath.Fences.Add(item);
			}
			CalculatedPath.IterateOnT = false;
			CalculatedPath.IterateOnLatitude = true;
			CalculatedPath.LatitudeCentre = PathLatitude_deg;
			CalculatedPath.PathIsTopographic = UseLocalTopography;
			Earth2014Present = File.Exists(EarthTopography.FileName);
			if (Earth2014Present)
			{
				EarthTopography.OpenEarthFile();
			}
			for (int k = 0; k < list.Count; k++)
			{
				if (((k == 5 || k == 6) && !CalculatedPath.ShowUmbralPath) || ((k == 7 || k == 8) && !AllowAsteroidRings))
				{
					continue;
				}
				ShadowRadius = AsteroidShadowDiameter_Penumbral / 2.0 / 6378.137 * list[k];
				if (Math.Abs(MinGeocentricD + ShadowRadius * (double)Math.Sign(DeltaX * Y0)) > 1.01)
				{
					continue;
				}
				double num5 = 0.0;
				double Ta = 0.0;
				int num6 = 0;
				double Long_Deg = StartLongitude_deg;
				double pCos;
				double pSin;
				double H;
				double MinDistance;
				do
				{
					Utilities.Get_pSin_pCos(Long_Deg / (180.0 / Math.PI), PathLatitude_deg / (180.0 / Math.PI), num5, out pCos, out pSin);
					GetTforMinimumDistance(Long_Deg, pCos, pSin, ShadowRadius, ref Ta, out H, out MinDistance, PathDistance: false, out var _, out TClosest, out var _, out var _, out var _);
					if (num6 == 0)
					{
						num6++;
						num = MinDistance;
						num3 = Long_Deg;
						value = 120.0;
						Long_Deg += 1.0;
					}
					else
					{
						num6++;
					}
					if (num6 > 1)
					{
						if (MinDistance == num)
						{
							num6 = 20;
						}
						if (num6 < 20)
						{
							num2 = (Long_Deg - num3) / (num - MinDistance) * MinDistance;
							if (Math.Abs(num2) > 30.0)
							{
								num2 = 30 * Math.Sign(num2);
							}
							if (Math.Abs(num2) > Math.Abs(value))
							{
								break;
							}
							num3 = Long_Deg;
							num = MinDistance;
							Long_Deg += num2;
							value = num2;
						}
					}
					if (num6 > 19)
					{
						break;
					}
					if (UseLocalTopography)
					{
						num5 = EarthTopography.Elevation_Above_MSL(Long_Deg, PathLatitude_deg);
					}
				}
				while (Math.Abs(num2) > 5E-05 || num6 < 2);
				if (Long_Deg < -180.0)
				{
					Long_Deg += 360.0;
				}
				if (Long_Deg > 180.0)
				{
					Long_Deg -= 360.0;
				}
				double num7 = pSin * Math.Sin(FPlaneDec) + pCos * Math.Cos(H) * Math.Cos(FPlaneDec);
				double num8 = Xatmin + DeltaX * Ta + Delta2X * Ta * Ta + Delta3X * Ta * Ta * Ta;
				double num9 = YatMin + DeltaY * Ta + Delta2Y * Ta * Ta + Delta3Y * Ta * Ta * Ta;
				if (Math.Sqrt(num8 * num8 + num9 * num9) > 1.0 + Math.Abs(ShadowRadius))
				{
					num7 = -1.0;
				}
				double d = pSin * Math.Sin(SubSolarLatitude_OfDate / (180.0 / Math.PI)) + pCos * Math.Cos(H + (SubstellarLongitude - SubSolarLongitude_OfDate) / (180.0 / Math.PI)) * Math.Cos(SubSolarLatitude_OfDate / (180.0 / Math.PI));
				if (!(num7 > 0.0) || !(Math.Abs(num2) <= 0.0005) || num6 >= 20 || !(Math.Abs(MinDistance) < 0.01))
				{
					continue;
				}
				ValidPath = true;
				double Lat_Deg = PathLatitude_deg;
				Utilities.ConvertLatLong_forEOP(ref Long_Deg, ref Lat_Deg, EOPx, EOPy);
				Long_Deg -= dUT1 / 240.0;
				switch (k)
				{
				case 0:
				{
					CalculatedPath.LongitudeCentre = Long_Deg;
					CalculatedPath.LatitudeCentre = Lat_Deg;
					CalculatedPath.AltitudeCentre = num5;
					CalculatedPath.TCentre = MidTime + Ta;
					CalculatedPath.CentreLineValid = true;
					double y = (0.0 - Math.Cos(FPlaneDec)) * Math.Sin(H);
					double x = pCos * Math.Sin(FPlaneDec) - pSin * Math.Cos(H) * Math.Cos(FPlaneDec);
					double num10 = 180.0 / Math.PI * Math.Atan2(y, x);
					if (num10 < 0.0)
					{
						num10 += 360.0;
					}
					CalculatedPath.StarAz = num10;
					CalculatedPath.StarAlt = Math.Asin(num7) * (180.0 / Math.PI);
					CalculatedPath.SunAlt = Math.Asin(d) * (180.0 / Math.PI);
					break;
				}
				case 1:
					CalculatedPath.LongitudeRight = Long_Deg;
					CalculatedPath.LatitudeRight = Lat_Deg;
					CalculatedPath.AltitudeRight = num5;
					break;
				case 2:
					CalculatedPath.LongitudeLeft = Long_Deg;
					CalculatedPath.LatitudeLeft = Lat_Deg;
					CalculatedPath.AltitudeLeft = num5;
					break;
				case 3:
					CalculatedPath.LongitudeRightSigma = Long_Deg;
					CalculatedPath.LatitudeRightSigma = Lat_Deg;
					CalculatedPath.AltitudeRightSigma = num5;
					break;
				case 4:
					CalculatedPath.LongitudeLeftSigma = Long_Deg;
					CalculatedPath.LatitudeLeftSigma = Lat_Deg;
					CalculatedPath.AltitudeLeftSigma = num5;
					break;
				case 5:
					CalculatedPath.LongitudeRightUmbra = Long_Deg;
					CalculatedPath.LatitudeRightUmbra = Lat_Deg;
					CalculatedPath.AltitudeRightUmbra = num5;
					break;
				case 6:
					CalculatedPath.LongitudeLeftUmbra = Long_Deg;
					CalculatedPath.LatitudeLeftUmbra = Lat_Deg;
					CalculatedPath.AltitudeLeftUmbra = num5;
					break;
				case 7:
					CalculatedPath.LongitudeRightRings = Long_Deg;
					CalculatedPath.LatitudeRightRings = Lat_Deg;
					CalculatedPath.AltitudeRightRings = num5;
					break;
				case 8:
					CalculatedPath.LongitudeLeftRings = Long_Deg;
					CalculatedPath.LatitudeLeftRings = Lat_Deg;
					CalculatedPath.AltitudeLeftRings = num5;
					break;
				default:
					CalculatedPath.Fences[k - 9].Longitude = Long_Deg;
					CalculatedPath.Fences[k - 9].Latitude = Lat_Deg;
					CalculatedPath.Fences[k - 9].Altitude = num5;
					break;
				}
			}
			if (Earth2014Present)
			{
				EarthTopography.CloseEarthFile();
			}
		}

		internal static void PathByTime(double Ta, double ShadowRadius, bool UseLocalTopography, int NumberOfFences_Left, int NumberOfFences_Right, int FenceSpacing_meters, ref AsteroidPathCoords CalculatedPath, out bool ValidPath)
		{
			List<double> list = new List<double>
			{
				0.0,
				-1.0,
				1.0,
				0.0 - Ratio_1Sigma_to_Limit_Distances,
				Ratio_1Sigma_to_Limit_Distances,
				(0.0 - AsteroidShadowDiameter_Umbral) / AsteroidShadowDiameter_Penumbral,
				AsteroidShadowDiameter_Umbral / AsteroidShadowDiameter_Penumbral,
				0.0 - PathAugmentation_Rings,
				PathAugmentation_Rings
			};
			if (Math.Abs(JD_for_EOP - EventJD) > 10.0)
			{
				Utilities.EarthOrientationParameters(EventJD, out EOPx, out EOPy, out dUT1);
				JD_for_EOP = EventJD;
			}
			FenceLabels = new List<string>();
			string text = "";
			if (UseLocalTopography)
			{
				for (int i = -NumberOfFences_Right; i <= NumberOfFences_Left; i++)
				{
					if (i == 0)
					{
						continue;
					}
					double num = (double)(i * 2 * FenceSpacing_meters) / AsteroidShadowDiameter_Penumbral / 1000.0;
					if (!(Math.Abs(list[1] - num) < 0.02) && !(Math.Abs(list[2] - num) < 0.02) && !(Math.Abs(list[3] - num) < 0.02) && !(Math.Abs(list[4] - num) < 0.02))
					{
						list.Add(num);
						text = string.Format("Fence @ {0,1:f0} m", Math.Abs(i * FenceSpacing_meters));
						if (num < 0.0)
						{
							FenceLabels.Add(text + " right");
						}
						else
						{
							FenceLabels.Add(text + " left");
						}
					}
				}
			}
			ValidPath = false;
			CalculatedPath = new AsteroidPathCoords();
			CalculatedPath.PathHasRings = PathAugmentation_Rings > 0.0;
			CalculatedPath.ShowUmbralPath = AsteroidShadowDiameter_Umbral < 0.8 * AsteroidShadowDiameter_Penumbral;
			CalculatedPath.Fences.Clear();
			for (int j = 0; j < list.Count - 9; j++)
			{
				Fences item = new Fences();
				CalculatedPath.Fences.Add(item);
			}
			CalculatedPath.IterateOnT = true;
			CalculatedPath.IterateOnLatitude = false;
			CalculatedPath.PathIsTopographic = UseLocalTopography;
			Earth2014Present = File.Exists(EarthTopography.FileName);
			if (Earth2014Present)
			{
				EarthTopography.OpenEarthFile();
			}
			for (int k = 0; k < list.Count; k++)
			{
				if (((k == 5 || k == 6) && !CalculatedPath.ShowUmbralPath) || ((k == 7 || k == 8) && !AllowAsteroidRings))
				{
					continue;
				}
				ShadowRadius = AsteroidShadowDiameter_Penumbral / 2.0 / 6378.137 * list[k];
				if (Math.Abs(MinGeocentricD + ShadowRadius * (double)Math.Sign(DeltaX * Y0)) > 1.01)
				{
					continue;
				}
				Time_GetLatitudeLongitude(Ta, UseLocalTopography, list[k], out var Valid, out var LatitudeDeg, out var LongitudeDeg, out var SiteAltitude_Meters_Above_MSL);
				if (!Valid)
				{
					continue;
				}
				ValidPath = true;
				Utilities.ConvertLatLong_forEOP(ref LongitudeDeg, ref LatitudeDeg, EOPx, EOPy);
				LongitudeDeg -= dUT1 / 240.0;
				switch (k)
				{
				case 0:
				{
					CalculatedPath.LongitudeCentre = LongitudeDeg;
					CalculatedPath.LatitudeCentre = LatitudeDeg;
					CalculatedPath.AltitudeCentre = SiteAltitude_Meters_Above_MSL;
					CalculatedPath.TCentre = MidTime + Ta;
					CalculatedPath.CentreLineValid = true;
					Utilities.Get_pSin_pCos(LongitudeDeg / (180.0 / Math.PI), LatitudeDeg / (180.0 / Math.PI), 0.0, out var pCos, out var pSin);
					double num2 = (0.0 - SubstellarLongitude) / (180.0 / Math.PI) + 0.2625161707907961 * Ta + LongitudeDeg / (180.0 / Math.PI);
					double y = (0.0 - Math.Cos(FPlaneDec)) * Math.Sin(num2);
					double x = pCos * Math.Sin(FPlaneDec) - pSin * Math.Cos(num2) * Math.Cos(FPlaneDec);
					double num3 = 180.0 / Math.PI * Math.Atan2(y, x);
					if (num3 < 0.0)
					{
						num3 += 360.0;
					}
					CalculatedPath.StarAz = num3;
					CalculatedPath.StarAlt = 180.0 / Math.PI * Math.Asin(pSin * Math.Sin(FPlaneDec) + pCos * Math.Cos(num2) * Math.Cos(FPlaneDec));
					CalculatedPath.SunAlt = 180.0 / Math.PI * Math.Asin(pSin * Math.Sin(SubSolarLatitude_OfDate / (180.0 / Math.PI)) + pCos * Math.Cos(num2 + (SubstellarLongitude - SubSolarLongitude_OfDate) / (180.0 / Math.PI)) * Math.Cos(SubSolarLatitude_OfDate / (180.0 / Math.PI)));
					break;
				}
				case 1:
					CalculatedPath.LongitudeRight = LongitudeDeg;
					CalculatedPath.LatitudeRight = LatitudeDeg;
					CalculatedPath.AltitudeRight = SiteAltitude_Meters_Above_MSL;
					break;
				case 2:
					CalculatedPath.LongitudeLeft = LongitudeDeg;
					CalculatedPath.LatitudeLeft = LatitudeDeg;
					CalculatedPath.AltitudeLeft = SiteAltitude_Meters_Above_MSL;
					break;
				case 3:
					CalculatedPath.LongitudeRightSigma = LongitudeDeg;
					CalculatedPath.LatitudeRightSigma = LatitudeDeg;
					CalculatedPath.AltitudeRightSigma = SiteAltitude_Meters_Above_MSL;
					break;
				case 4:
					CalculatedPath.LongitudeLeftSigma = LongitudeDeg;
					CalculatedPath.LatitudeLeftSigma = LatitudeDeg;
					CalculatedPath.AltitudeLeftSigma = SiteAltitude_Meters_Above_MSL;
					break;
				case 5:
					CalculatedPath.LongitudeRightUmbra = LongitudeDeg;
					CalculatedPath.LatitudeRightUmbra = LatitudeDeg;
					CalculatedPath.AltitudeRightUmbra = SiteAltitude_Meters_Above_MSL;
					break;
				case 6:
					CalculatedPath.LongitudeLeftUmbra = LongitudeDeg;
					CalculatedPath.LatitudeLeftUmbra = LatitudeDeg;
					CalculatedPath.AltitudeLeftUmbra = SiteAltitude_Meters_Above_MSL;
					break;
				case 7:
					CalculatedPath.LongitudeRightRings = LongitudeDeg;
					CalculatedPath.LatitudeRightRings = LatitudeDeg;
					CalculatedPath.AltitudeRightRings = SiteAltitude_Meters_Above_MSL;
					break;
				case 8:
					CalculatedPath.LongitudeLeftRings = LongitudeDeg;
					CalculatedPath.LatitudeLeftRings = LatitudeDeg;
					CalculatedPath.AltitudeLeftRings = SiteAltitude_Meters_Above_MSL;
					break;
				default:
					CalculatedPath.Fences[k - 9].Longitude = LongitudeDeg;
					CalculatedPath.Fences[k - 9].Latitude = LatitudeDeg;
					CalculatedPath.Fences[k - 9].Altitude = SiteAltitude_Meters_Above_MSL;
					break;
				}
			}
			if (Earth2014Present)
			{
				EarthTopography.CloseEarthFile();
			}
		}

		internal static void GetTforMinimumDistance(double LongitudeDeg, double pCos, double pSin, double PathDistance_ShadowRadii, ref double Ta, out double H1, out double MinDistance, bool PathDistance, out double Separation, out double TClosest, out double Z, out double ZSun, out string Direction)
		{
			int num = 0;
			Separation = (MinDistance = (Z = (ZSun = 0.0)));
			TClosest = 0.0;
			Direction = "-";
			double num2;
			double num3;
			double num4;
			double num5;
			double num7;
			double num8;
			do
			{
				H1 = (0.0 - SubstellarLongitude) / (180.0 / Math.PI) + 0.2625161707907961 * Ta + LongitudeDeg / (180.0 / Math.PI);
				num2 = Xatmin + DeltaX * Ta + Delta2X * Ta * Ta + Delta3X * Ta * Ta * Ta - pCos * Math.Sin(H1);
				num3 = YatMin + DeltaY * Ta + Delta2Y * Ta * Ta + Delta3Y * Ta * Ta * Ta - pSin * Math.Cos(FPlaneDec) + pCos * Math.Cos(H1) * Math.Sin(FPlaneDec);
				num4 = DeltaX + 2.0 * Delta2X * Ta + 3.0 * Delta3X * Ta * Ta - 0.2625161707907961 * pCos * Math.Cos(H1);
				num5 = DeltaY + 2.0 * Delta2Y * Ta + 3.0 * Delta3Y * Ta * Ta - 0.2625161707907961 * pCos * Math.Sin(H1) * Math.Sin(FPlaneDec);
				double num6 = num4 * num4 + num5 * num5;
				num7 = Math.Sqrt(num6);
				num8 = (num2 * num4 + num3 * num5) / num6;
				if (Math.Abs(num8) > 1.0)
				{
					num8 = 0.1 * (double)Math.Sign(num8);
				}
				Ta -= num8;
				num++;
			}
			while (Math.Abs(num8) > 1E-05 && num < 30);
			MinDistance = (num2 * num5 - num3 * num4) / num7 - PathDistance_ShadowRadii;
			if (!PathDistance)
			{
				return;
			}
			MinDistance = 0.0 - MinDistance;
			if (Math.Abs(num5 / num4) > 2.0)
			{
				if (MinDistance / num5 < 0.0)
				{
					Direction = "W";
				}
				else
				{
					Direction = "E";
				}
			}
			else if (MinDistance / num4 < 0.0)
			{
				Direction = "N";
			}
			else
			{
				Direction = "S";
			}
			Separation = (0.0 - MinDistance) * 8.794143836182533 / DistanceToAsteroid;
			MinDistance = MinDistance * 6378.137 / VerticalScaleFactor;
			TClosest = MidTime + Ta;
			Z = pSin * Math.Sin(FPlaneDec) + pCos * Math.Cos(H1) * Math.Cos(FPlaneDec);
			ZSun = pSin * Math.Sin(SubSolarLatitude_OfDate / (180.0 / Math.PI)) + pCos * Math.Cos(H1 + (SubstellarLongitude - SubSolarLongitude_OfDate) / (180.0 / Math.PI)) * Math.Cos(SubSolarLatitude_OfDate / (180.0 / Math.PI));
		}

		internal static void Time_GetLatitudeLongitude(double Ta, bool UseLocalTopography, double PathDistance_ShadowRadii, out bool Valid, out double LatitudeDeg, out double LongitudeDeg, out double SiteAltitude_Meters_Above_MSL)
		{
			double num = 1.0;
			double num2 = 0.0;
			double num3 = 2E-06;
			double num4 = 3E-05;
			LatitudeDeg = (LongitudeDeg = (SiteAltitude_Meters_Above_MSL = 0.0));
			double num5 = DeltaX + 2.0 * Delta2X * Ta + 3.0 * Delta3X * Ta * Ta;
			double num6 = DeltaY + 2.0 * Delta2Y * Ta + 3.0 * Delta3Y * Ta * Ta;
			double num7 = Math.Atan2(0.0 - num6, num5);
			double num8 = AsteroidShadowDiameter_Penumbral / 2.0 / 6378.137;
			double num9 = Xatmin + DeltaX * Ta + Delta2X * Ta * Ta + Delta3X * Ta * Ta * Ta;
			double num10 = YatMin + DeltaY * Ta + Delta2Y * Ta * Ta + Delta3Y * Ta * Ta * Ta;
			double num11 = Math.Sin(FPlaneDec);
			double num12 = 0.9966471893352525 * Math.Cos(FPlaneDec);
			double num13 = Math.Sqrt(num11 * num11 + num12 * num12);
			double num14 = num11 / num13;
			double num15 = num12 / num13;
			double num16 = (0.0 - SubstellarLongitude) / (180.0 / Math.PI) + 0.2625161707907961 * Ta;
			Valid = false;
			double num17 = num9;
			double num18 = num10;
			double num19 = num9;
			double num20 = num10;
			for (int i = 0; i < 25; i++)
			{
				double num21 = num20 / num13;
				double num22 = 1.0 - num19 * num19 - num21 * num21;
				if (num22 > 0.0)
				{
					Valid = true;
					double num23 = Math.Sqrt(num22);
					if (i < 3)
					{
						double num25;
						double num24;
						if (num8 == 0.0)
						{
							num25 = (num24 = 0.0);
						}
						else
						{
							double y = 0.0 - num6 + 0.2625161707907961 * num9 * Math.Sin(FPlaneDec);
							double x = num5 + 0.2625161707907961 * num10 * Math.Sin(FPlaneDec) - num23 * 0.2625161707907961 * Math.Cos(FPlaneDec);
							num7 = Math.Atan2(y, x);
							num25 = num8 * Math.Sin(num7) * PathDistance_ShadowRadii;
							num24 = num8 * Math.Cos(num7) * PathDistance_ShadowRadii;
						}
						num17 = Xatmin + DeltaX * Ta + Delta2X * Ta * Ta + Delta3X * Ta * Ta * Ta + num25;
						num18 = YatMin + DeltaY * Ta + Delta2Y * Ta * Ta + Delta3Y * Ta * Ta * Ta + num24;
					}
					double num26 = num19;
					double num27 = (0.0 - num21) * num14 + num23 * num15;
					double num28 = num21 * num15 + num23 * num14;
					LatitudeDeg = 180.0 / Math.PI * Math.Atan(num28 / Math.Sqrt(num26 * num26 + num27 * num27) / 0.9966471893352525);
					for (LongitudeDeg = 180.0 / Math.PI * (Math.Atan2(num26, num27) - num16); LongitudeDeg < -180.0; LongitudeDeg += 360.0)
					{
					}
					while (LongitudeDeg > 180.0)
					{
						LongitudeDeg -= 360.0;
					}
					if (!UseLocalTopography)
					{
						if (i >= 3)
						{
							break;
						}
					}
					else
					{
						SiteAltitude_Meters_Above_MSL = EarthTopography.Elevation_Above_MSL(LongitudeDeg, LatitudeDeg);
					}
					Utilities.Get_pSin_pCos(LongitudeDeg / (180.0 / Math.PI), LatitudeDeg / (180.0 / Math.PI), SiteAltitude_Meters_Above_MSL, out var pCos, out var pSin);
					num = Math.Sqrt(pCos * pCos + pSin * pSin);
					double num29 = pCos * Math.Sin(num16 + LongitudeDeg / (180.0 / Math.PI));
					double num30 = pSin * Math.Cos(FPlaneDec) - pCos * Math.Cos(num16 + LongitudeDeg / (180.0 / Math.PI)) * Math.Sin(FPlaneDec);
					double num31 = num17 - num29;
					double num32 = num18 - num30;
					num2 = Math.Sqrt(num31 * num31 + num32 * num32) / num;
					num4 = ((num2 < 0.96) ? 0.0001 : ((num2 < 0.97) ? 3E-05 : ((num2 < 0.978) ? 1E-05 : ((!(num2 < 0.982)) ? 1E-06 : 3E-06))));
					double num33 = Utilities.QuadratureAddition(num31, num32);
					if (num33 < num3)
					{
						break;
					}
					_ = LongitudeDeg;
					_ = -67.0;
					if (num33 > num4 && UseLocalTopography && i >= 2)
					{
						num31 = num31 / num33 * num4;
						num32 = num32 / num33 * num4;
					}
					num19 += num31;
					num20 += num32;
					continue;
				}
				Valid = false;
				break;
			}
		}

		internal static double TanZshift(double Latitude, double Longitude, double OldLatitude, double OldLongitude, double TanZ, double StarAZ)
		{
			double num = Longitude - OldLongitude;
			double y = Math.Cos(OldLatitude) * Math.Sin(num);
			double x = Math.Sin(OldLatitude) * Math.Cos(Latitude) - Math.Cos(OldLatitude) * Math.Sin(Latitude) * Math.Cos(num);
			double num2;
			for (num2 = 180.0 - 180.0 / Math.PI * Math.Atan2(y, x); num2 < 0.0; num2 += 180.0)
			{
			}
			while (num2 > 180.0)
			{
				num2 -= 180.0;
			}
			return Math.Sin(num2 / (180.0 / Math.PI) - StarAZ) * TanZ;
		}

		internal static bool GeographicCoordsAtMinD(out double Longitude, out double Latitude)
		{
			Longitude = (Latitude = 0.0);
			double num = Math.Sqrt(Math.Sin(FPlaneDec) * Math.Sin(FPlaneDec) + Math.Cos(FPlaneDec) * Math.Cos(FPlaneDec) * 0.9933052);
			double num2 = YatMin / num;
			double num3 = Math.Sin(FPlaneDec) / num;
			double num4 = Math.Cos(FPlaneDec) * Utilities.sqrt_1LessEarthEllipticitySqrd / num;
			double num5 = 1.0 - Xatmin * Xatmin - num2 * num2;
			if (num5 > 0.0)
			{
				double num6 = Math.Sqrt(num5);
				double xatmin = Xatmin;
				double num7 = (0.0 - num2) * num3 + num6 * num4;
				double num8 = num2 * num4 + num6 * num3;
				Longitude = SubstellarLongitude + Math.Atan2(xatmin, num7) * (180.0 / Math.PI);
				if (Longitude < -180.0)
				{
					Longitude += 360.0;
				}
				if (Longitude > 180.0)
				{
					Longitude -= 360.0;
				}
				Latitude = Math.Atan(Utilities.sqrt_1LessEarthEllipticitySqrd * num8 / Math.Sqrt(xatmin * xatmin + num7 * num7)) * (180.0 / Math.PI);
				return true;
			}
			return false;
		}

		internal static void SetPathRangeFilter()
		{
			RegionLongitude[0] = (double)PathCoordsForm.updnLongW.get_Value();
			if (RegionLongitude[0] < -180.0)
			{
				RegionLongitude[0] += 360.0;
			}
			RegionLongitude[1] = (double)PathCoordsForm.updnLongE.get_Value();
			if (RegionLongitude[1] > 180.0)
			{
				RegionLongitude[1] -= 360.0;
			}
			if ((RegionLongitude[0] > 0.0) & (RegionLongitude[1] < 0.0))
			{
				RegionLongitude[1] += 360.0;
			}
			RegionLatitude[0] = (double)PathCoordsForm.updnLatN.get_Value();
			RegionLatitude[1] = (double)PathCoordsForm.updnLatS.get_Value();
		}

		internal static bool IsWithinPathRegion(double Longitude, double Latitude, bool ApplyLimitTest)
		{
			if (!ApplyLimitTest)
			{
				return true;
			}
			if (Longitude > -180.0 && RegionLongitude[1] > 180.0)
			{
				Longitude += 360.0;
			}
			if (Longitude >= RegionLongitude[0] && Longitude <= RegionLongitude[1] && Latitude <= RegionLatitude[0] && Latitude >= RegionLatitude[1])
			{
				return true;
			}
			return false;
		}

		internal static void CreateGoogleEarthKMLFile(string OutFile_withPath, string Label, bool Auto, bool View, string SiteFile)
		{
			string[] array = new string[9] { "Center line", "Path limit left", "Path limit right", "1-sigma left", "1-sigma right", "Umbral limit left", "Umbral limit right", "Ring limit left", "Ring limit right" };
			if (PathCoordinates[0].ShowUmbralPath)
			{
				array[1] = "Penumbral limit left";
				array[2] = "Penumbral limit right";
			}
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			int[] array2 = new int[9] { 2, 3, 3, 1, 1, 38, 38, 7, 7 };
			bool flag = false;
			bool flag2 = false;
			string text = "";
			int num4 = 2;
			_ = ErrorInTime / 240.0;
			if (PathCoordinates.Count < 1)
			{
				return;
			}
			int num5 = 0;
			if (AllowAsteroidRings)
			{
				num5 = 2;
			}
			int num6 = 7 + num5;
			int num7;
			bool applyLimitTest;
			try
			{
				SetPathRangeFilter();
				num7 = num6 + PathCoordinates[0].Fences.Count;
				applyLimitTest = PathCoordsForm.chkLimit.get_Checked();
			}
			catch
			{
				num7 = num6;
				applyLimitTest = false;
			}
			try
			{
				IncludePathAltitudes = PathCoordsForm.chkHeights.get_Checked() & ((Control)PathCoordsForm.chkHeights).get_Enabled();
			}
			catch
			{
				IncludePathAltitudes = false;
			}
			if (!GoogleEarth.Create_New_GoogleEarthKMZ_File(OutFile_withPath, Label, Auto, out var CreatedFile))
			{
				return;
			}
			for (int i = 0; i < num7; i++)
			{
				if ((i == 5 || i == 6) && !PathCoordinates[0].ShowUmbralPath)
				{
					continue;
				}
				if (i < num6)
				{
					text = array[i];
					num4 = array2[i];
				}
				else
				{
					text = FenceLabels[i - num6];
					num4 = 16;
					flag2 = true;
					if (i == num6)
					{
						GoogleEarth.OpenFolder("Fences");
					}
				}
				GoogleEarth.Write_TagsAndLabel_For_New_GoogleEarthKMZ_Path(num4, text, i < 5 + num5);
				for (int j = 0; j < PathCoordinates.Count; j++)
				{
					if (i < num6)
					{
						switch (i)
						{
						case 0:
							num = PathCoordinates[j].LongitudeCentre;
							num2 = PathCoordinates[j].LatitudeCentre;
							if (!IsWithinPathRegion(num, num2, applyLimitTest))
							{
								continue;
							}
							num3 = PathCoordinates[j].AltitudeCentre;
							flag = PathCoordinates[j].CentreLineValid;
							break;
						case 1:
							num = PathCoordinates[j].LongitudeLeft;
							num2 = PathCoordinates[j].LatitudeLeft;
							if (!IsWithinPathRegion(num, num2, applyLimitTest))
							{
								continue;
							}
							num3 = PathCoordinates[j].AltitudeLeft;
							flag = PathCoordinates[j].LatitudeLeft < 100.0;
							break;
						case 2:
							num = PathCoordinates[j].LongitudeRight;
							num2 = PathCoordinates[j].LatitudeRight;
							if (!IsWithinPathRegion(num, num2, applyLimitTest))
							{
								continue;
							}
							num3 = PathCoordinates[j].AltitudeRight;
							flag = PathCoordinates[j].LatitudeRight < 100.0;
							break;
						case 3:
							num = PathCoordinates[j].LongitudeLeftSigma;
							num2 = PathCoordinates[j].LatitudeLeftSigma;
							if (!IsWithinPathRegion(num, num2, applyLimitTest))
							{
								continue;
							}
							num3 = PathCoordinates[j].AltitudeLeftSigma;
							flag = PathCoordinates[j].LatitudeLeftSigma < 100.0;
							break;
						case 4:
							num = PathCoordinates[j].LongitudeRightSigma;
							num2 = PathCoordinates[j].LatitudeRightSigma;
							if (!IsWithinPathRegion(num, num2, applyLimitTest))
							{
								continue;
							}
							num3 = PathCoordinates[j].AltitudeRightSigma;
							flag = PathCoordinates[j].LatitudeRightSigma < 100.0;
							break;
						case 5:
							num = PathCoordinates[j].LongitudeLeftUmbra;
							num2 = PathCoordinates[j].LatitudeLeftUmbra;
							if (!IsWithinPathRegion(num, num2, applyLimitTest))
							{
								continue;
							}
							num3 = PathCoordinates[j].AltitudeLeftUmbra;
							flag = PathCoordinates[j].LatitudeLeftUmbra < 100.0;
							break;
						case 6:
							num = PathCoordinates[j].LongitudeRightUmbra;
							num2 = PathCoordinates[j].LatitudeRightUmbra;
							if (!IsWithinPathRegion(num, num2, applyLimitTest))
							{
								continue;
							}
							num3 = PathCoordinates[j].AltitudeRightUmbra;
							flag = PathCoordinates[j].LatitudeRightUmbra < 100.0;
							break;
						case 7:
							num = PathCoordinates[j].LongitudeLeftRings;
							num2 = PathCoordinates[j].LatitudeLeftRings;
							if (!IsWithinPathRegion(num, num2, applyLimitTest))
							{
								continue;
							}
							num3 = PathCoordinates[j].AltitudeLeftRings;
							flag = PathCoordinates[j].LatitudeLeftRings < 100.0;
							break;
						case 8:
							num = PathCoordinates[j].LongitudeRightRings;
							num2 = PathCoordinates[j].LatitudeRightRings;
							if (!IsWithinPathRegion(num, num2, applyLimitTest))
							{
								continue;
							}
							num3 = PathCoordinates[j].AltitudeRightRings;
							flag = PathCoordinates[j].LatitudeRightRings < 100.0;
							break;
						default:
							num = PathCoordinates[j].Fences[i - 7 - num5].Longitude;
							num2 = PathCoordinates[j].Fences[i - 7 - num5].Latitude;
							if (!IsWithinPathRegion(num, num2, applyLimitTest))
							{
								continue;
							}
							num3 = PathCoordinates[j].Fences[i - 7 - num5].Altitude;
							flag = PathCoordinates[j].Fences[i - 7 - num5].Latitude < 100.0;
							break;
						}
					}
					else
					{
						num = PathCoordinates[j].Fences[i - 7 - num5].Longitude;
						num2 = PathCoordinates[j].Fences[i - 7 - num5].Latitude;
						if (!IsWithinPathRegion(num, num2, applyLimitTest))
						{
							continue;
						}
						num3 = PathCoordinates[j].Fences[i - 7 - num5].Altitude;
						flag = PathCoordinates[j].Fences[i - 7 - num5].Latitude < 100.0;
					}
					if (flag)
					{
						GoogleEarth.Write_Path_Coordinate_GoogleEarthKMZ(num, num2, num3);
					}
				}
				GoogleEarth.Write_Tags_At_End_of_Path_GoogleEarthKMZ();
			}
			if (flag2)
			{
				GoogleEarth.CloseFolder();
			}
			GoogleEarth.OpenFolder("UTC times");
			int num8 = 0;
			int num9 = 0;
			double num10 = 0.0;
			double num11 = 0.0;
			double num12 = 0.0;
			double num13 = 0.0;
			int num14 = 0;
			int num15 = 0;
			string text2 = "";
			for (int k = 0; k < PathCoordinates.Count; k++)
			{
				if (!PathCoordinates[k].CentreLineValid)
				{
					continue;
				}
				num = PathCoordinates[k].LongitudeCentre;
				num2 = PathCoordinates[k].LatitudeCentre;
				if (!IsWithinPathRegion(num, num2, applyLimitTest))
				{
					continue;
				}
				int num16 = (int)(PathCoordinates[k].TCentre * 360.0);
				if (num16 != 0 && num8 == 0)
				{
					num8 = num16;
					num14 = k;
				}
				else if (num16 != num8)
				{
					if (num16 > num8)
					{
						num9 = num16;
						num13 = (PathCoordinates[k].TCentre * 360.0 - (double)num16) / ((PathCoordinates[k].TCentre - PathCoordinates[k - 1].TCentre) * 360.0);
						num10 = PathCoordinates[k].LongitudeCentre - num13 * (PathCoordinates[k].LongitudeCentre - PathCoordinates[k - 1].LongitudeCentre);
						num11 = PathCoordinates[k].LatitudeCentre - num13 * (PathCoordinates[k].LatitudeCentre - PathCoordinates[k - 1].LatitudeCentre);
						num12 = PathCoordinates[k].AltitudeCentre - num13 * (PathCoordinates[k].AltitudeCentre - PathCoordinates[k - 1].AltitudeCentre);
					}
					else
					{
						num9 = num8;
						num13 = (PathCoordinates[k - 1].TCentre * 360.0 - (double)num8) / ((PathCoordinates[k - 1].TCentre - PathCoordinates[k].TCentre) * 360.0);
						num10 = PathCoordinates[k - 1].LongitudeCentre + num13 * (PathCoordinates[k].LongitudeCentre - PathCoordinates[k - 1].LongitudeCentre);
						num11 = PathCoordinates[k - 1].LatitudeCentre + num13 * (PathCoordinates[k].LatitudeCentre - PathCoordinates[k - 1].LatitudeCentre);
						num12 = PathCoordinates[k - 1].AltitudeCentre + num13 * (PathCoordinates[k].AltitudeCentre - PathCoordinates[k - 1].AltitudeCentre);
					}
					text2 = "<![CDATA[";
					text2 = ((!PathCoordinates[k].PathIsTopographic) ? (text2 + "<b>Path at Mean Sea Level</b>") : (text2 + $"<b>Topographic path</b><br />Site Elevation = <b>{num12:###0} meters;</b><br />(from Earth2014 SUR 1-arcmin)"));
					text2 += $"<br /><br />Star Altitude = <b>{PathCoordinates[k].StarAlt:+##;-##}&deg;</b><br />Star Azimuth = <b>{PathCoordinates[k].StarAz:###}&deg;</b><br /><br />Sun Altitude = <b>{PathCoordinates[k].SunAlt:+##;-##}&deg;</b><br /><br />";
					string text3 = OrbitSourceDate;
					if (text3.Length > 2)
					{
						text3 = OrbitSourceDate.Substring(0, 1) + OrbitSourceDate.Substring(1).Replace(":", "</b> dated <b>");
					}
					text2 = text2 + "Orbit<br /><b>" + text3 + "</b><br />Date of prediction: <b>" + PredictionDate.Substring(14) + "</b><br /><br />by <b><i>Occult</i></b>";
					text2 += "]]>";
					GoogleEarth.Write_PlaceMark_BullseyePlusDescriptionBox_GoogleEarthKML(Utilities.DEGtoDMS((double)num9 / 360.0, 2, 0, MinutesOnly: false, IncludeLeadingZeros: false, IncludePlusSymbol: false, IncludeDMS: false, IncludeHMS: true) + " UTC", text2, num10, num11);
					num8 = num16;
					num15 = k;
				}
			}
			GoogleEarth.CloseFolder();
			if (IncludePathAltitudes)
			{
				GoogleEarth.OpenFolder("Elevations");
				for (int l = 0; l < PathCoordinates.Count; l++)
				{
					if (PathCoordinates[l].CentreLineValid)
					{
						num = PathCoordinates[l].LongitudeCentre;
						num2 = PathCoordinates[l].LatitudeCentre;
						if (IsWithinPathRegion(num, num2, applyLimitTest))
						{
							GoogleEarth.Write_PlaceMark_DiamondPlusHeight_GoogleEarthKML(PathCoordinates[l].AltitudeCentre + "m", num, num2);
							num = PathCoordinates[l].LongitudeLeft;
							num2 = PathCoordinates[l].LatitudeLeft;
							GoogleEarth.Write_PlaceMark_DiamondPlusHeight_GoogleEarthKML(PathCoordinates[l].AltitudeLeft + "m", num, num2);
							num = PathCoordinates[l].LongitudeRight;
							num2 = PathCoordinates[l].LatitudeRight;
							GoogleEarth.Write_PlaceMark_DiamondPlusHeight_GoogleEarthKML(PathCoordinates[l].AltitudeRight + "m", num, num2);
						}
					}
				}
				GoogleEarth.CloseFolder();
			}
			GoogleEarth.Set_OpenLocation(PathCoordinates[(num14 + num15) / 2].LongitudeCentre, PathCoordinates[(num14 + num15) / 2].LatitudeCentre);
			if (SiteFile.Length > 5 && File.Exists(AppPath + "\\Sites\\" + SiteFile))
			{
				Sites sites = new Sites();
				StreamReader streamReader = new StreamReader(AppPath + "\\Sites\\" + SiteFile);
				do
				{
					sites.Read_SiteFile(streamReader.ReadLine());
					if (sites.PlotInGoogle > 0)
					{
						if (sites.PlotInGoogle == 2)
						{
							GoogleEarth.Write_PlaceMark_PinPlusName_GoogleEarthKML(sites.ShortName, sites.Longitude, sites.Latitude);
						}
						else
						{
							GoogleEarth.Write_PlaceMark_PinPlusName_GoogleEarthKML(sites.ShortName + "#", sites.LongitudeApprox, sites.LatitudeApprox);
						}
					}
				}
				while (!streamReader.EndOfStream);
				streamReader.Close();
			}
			CreatedFile = GoogleEarth.Close_GoogleEarthKMZ_File(CreatedFile, ConvertToKMZ: true);
			if (View)
			{
				GoogleEarth.DisplayGoogleMap(CreatedFile);
			}
		}

		internal static void CreateGoogleMapHTMFile(string OutFile, string PageName, bool Auto)
		{
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			double num4 = 0.0;
			bool flag = false;
			bool positiveTimeIncrement = true;
			int num5 = 1;
			double num6 = 0.0;
			double num7 = 0.0;
			double num8 = 0.0;
			double num9 = 0.0;
			double num10 = 100.0;
			double num11 = ErrorInTime / 240.0;
			int num12 = PathCoordinates.Count - 1;
			if (num12 < 0)
			{
				return;
			}
			int num13 = num12 / 2 - 1;
			if (num13 > 0)
			{
				positiveTimeIncrement = PathCoordinates[num13].TCentre < PathCoordinates[num13 + 1].TCentre;
			}
			if (!GoogleEarth.Create_New_GoogleMap_File(OutFile, PageName, positiveTimeIncrement, IncludeDirectionArrow: true, Auto))
			{
				return;
			}
			for (num5 = -1; num5 <= 3; num5++)
			{
				GoogleEarth.Write_Tags_For_New_GoogleMap_Polyline_Path();
				for (int i = 0; i <= num12; i++)
				{
					switch (num5)
					{
					case -1:
						num = PathCoordinates[i].LongitudeCentre;
						if ((i == 0) & (Math.Abs(num) == 180.0))
						{
							num = 179.99 * (double)Math.Sign(PathCoordinates[1].LongitudeCentre);
						}
						num2 = PathCoordinates[i].LatitudeCentre;
						flag = PathCoordinates[i].CentreLineValid;
						break;
					case 0:
						num = PathCoordinates[i].LongitudeLeft;
						if ((i == 0) & (Math.Abs(num) == 180.0))
						{
							num = 179.99 * (double)Math.Sign(PathCoordinates[1].LongitudeLeft);
						}
						num2 = PathCoordinates[i].LatitudeLeft;
						flag = PathCoordinates[i].LatitudeLeft < 100.0;
						break;
					case 1:
						num = PathCoordinates[i].LongitudeRight;
						if ((i == 0) & (Math.Abs(num) == 180.0))
						{
							num = 179.99 * (double)Math.Sign(PathCoordinates[1].LongitudeRight);
						}
						num2 = PathCoordinates[i].LatitudeRight;
						flag = PathCoordinates[i].LatitudeRight < 100.0;
						break;
					case 2:
						num = PathCoordinates[i].LongitudeLeftSigma;
						if ((i == 0) & (Math.Abs(num) == 180.0))
						{
							num = 179.99 * (double)Math.Sign(PathCoordinates[1].LongitudeLeftSigma);
						}
						num2 = PathCoordinates[i].LatitudeLeftSigma;
						flag = PathCoordinates[i].LatitudeLeftSigma < 100.0;
						break;
					case 3:
						num = PathCoordinates[i].LongitudeRightSigma;
						if ((i == 0) & (Math.Abs(num) == 180.0))
						{
							num = 179.99 * (double)Math.Sign(PathCoordinates[1].LongitudeRightSigma);
						}
						num2 = PathCoordinates[i].LatitudeRightSigma;
						flag = PathCoordinates[i].LatitudeRightSigma < 100.0;
						break;
					}
					double Distance = -1.0;
					if (flag)
					{
						Utilities.Distance(SubSolarLongitude_OfDate / (180.0 / Math.PI), SubSolarLatitude_OfDate / (180.0 / Math.PI), num / (180.0 / Math.PI), num2 / (180.0 / Math.PI), out Distance, out var _);
						flag &= (Distance > 1.67) | (Mv < 5.0);
					}
					if (!flag)
					{
						continue;
					}
					if (num5 > 1)
					{
						num6 = PathCoordinates[i].LongitudeCentre;
						num7 = PathCoordinates[i].LatitudeCentre;
						double num14;
						if ((PathCoordinates[i].CentreLineValid & (PathCoordinates[i].LatitudeLeftSigma < 100.0)) && num10 < 100.0)
						{
							if (num2 == num7)
							{
								num8 = num - num6;
								if (num8 < -180.0)
								{
									num8 += 360.0;
								}
								if (num8 > 180.0)
								{
									num8 -= 360.0;
								}
								num14 = num11 * (double)Math.Sign(num8);
							}
							else if (num == num6)
							{
								double num15 = Math.Sign(num7 - num2);
								double num16 = Math.Sign(num2 - num10);
								double num17 = Math.Sign(num - num9);
								num14 = num11 * num15 * num16 * num17;
							}
							else
							{
								num8 = num - num6;
								if (num8 < -180.0)
								{
									num8 += 360.0;
								}
								if (num8 > 180.0)
								{
									num8 -= 360.0;
								}
								num14 = num11 * (double)Math.Sign(num8);
							}
						}
						else
						{
							num14 = 0.0;
						}
						num += num14;
					}
					GoogleEarth.Write_PolyLine_Path_Coordinate_GoogleMap(num, num2, num5);
					num10 = num2;
					num9 = num;
				}
				GoogleEarth.Write_Tags_At_End_of_Polyline_Path_GoogleMap(num5);
			}
			GoogleEarth.Write_Tags_For_New_GoogleMap_PolyPoint_Path();
			for (int j = 0; j < PathCoordinates.Count; j++)
			{
				num = PathCoordinates[j].LongitudeCentre;
				if ((j == 0) & (Math.Abs(num) == 180.0))
				{
					num = 179.99 * (double)Math.Sign(PathCoordinates[1].LongitudeCentre);
				}
				num2 = PathCoordinates[j].LatitudeCentre;
				flag = PathCoordinates[j].CentreLineValid;
				if (flag & ((PathCoordinates[j].SunAlt < -0.1) | (Mv < 5.0)))
				{
					GoogleEarth.Write_PolyPoint_Path_Coordinate_GoogleMap(num, num2);
				}
			}
			GoogleEarth.Write_Tags_At_End_of_Polypoint_Path_GoogleMap(num5);
			int index = PathCoordinates.Count / 2;
			if (PathCoordinates[index].CentreLineValid)
			{
				num3 = PathCoordinates[index].LongitudeCentre;
				num4 = PathCoordinates[index].LatitudeCentre;
			}
			else if (PathCoordinates[index].LatitudeLeft < 100.0)
			{
				num3 = PathCoordinates[index].LongitudeLeft;
				num4 = PathCoordinates[index].LatitudeLeft;
			}
			else if (PathCoordinates[index].LatitudeRight < 100.0)
			{
				num3 = PathCoordinates[index].LongitudeRight;
				num4 = PathCoordinates[index].LatitudeRight;
			}
			else
			{
				num3 = (num4 = 0.0);
			}
			GoogleEarth.Close_GoogleMap_File(num3, num4);
		}

		public static void LargeStarChart()
		{
			try
			{
				((Control)Chart).Show();
			}
			catch
			{
				Chart = new StarChart(SetCoords: false);
				((Control)Chart).Show();
			}
			Chart.CentreRA = StarRA_2000;
			Chart.CentreDec = StarDec_2000;
			Chart.Use_Gaia = true;
			Chart.ShowObjectPath = true;
			((Control)Chart).Focus();
			Chart.PlotChart();
		}

		public static void DrawAsteroidStarChart(Graphics formGraphics, bool Printer, float TopLeftX, float TopLeftY, float ChartWidth, float ChartHeight, double CentreRA, double CentreDec, float ChartHeightDegrees, float MagLimit, bool VisualMag, bool GridLines, double RotateAngle, bool FlipHorizontal, bool FlipVertical, bool DrawInBW, bool Use_Gaia, bool use_UCAC4, bool Use_NOMAD, bool Use_PPMXL)
		{
			DrawAsteroidStarChart(formGraphics, Printer, TopLeftX, TopLeftY, ChartWidth, ChartHeight, CentreRA, CentreDec, ChartHeightDegrees, MagLimit, VisualMag, GridLines, RotateAngle, FlipHorizontal, FlipVertical, DrawInBW, Use_Gaia, use_UCAC4, Use_NOMAD, Use_PPMXL, 0.0, 100.0);
		}

		public static void DrawAsteroidStarChart(Graphics formGraphics, bool Printer, float TopLeftX, float TopLeftY, float ChartWidth, float ChartHeight, double CentreRA, double CentreDec, float ChartHeightDegrees, float MagLimit, bool VisualMag, bool GridLines, double RotateAngle, bool FlipHorizontal, bool FlipVertical, bool DrawInBW, bool Use_Gaia, bool use_UCAC4, bool Use_NOMAD, bool Use_PPMXL, double CompStarRA, double CompStarDec)
		{
			float num = ChartWidth / 2f;
			new Font("Courier New", 8f);
			Font font = new Font("Arial", 10f);
			new Font("Courier New", 12f, FontStyle.Bold);
			Pen pen;
			Pen pen2;
			Brush brush;
			Brush brush2;
			Color color;
			if (DrawInBW || Printer)
			{
				new Pen(Brushes.Black, 2f);
				pen = new Pen(Brushes.Black, 1f);
				new Pen(Brushes.Black, 1f);
				new Pen(Brushes.Black, 2f);
				pen2 = new Pen(Brushes.Black, 1f);
				brush = Brushes.Black;
				brush2 = Brushes.White;
				_ = Brushes.Black;
				color = Color.Black;
			}
			else
			{
				new Pen(Brushes.Gray, 2f);
				pen = new Pen(Brushes.Gray, 1f);
				new Pen(Brushes.Indigo, 1f);
				new Pen(Brushes.WhiteSmoke, 2f);
				pen2 = new Pen(Brushes.WhiteSmoke, 1f);
				brush = Brushes.White;
				brush2 = Brushes.Black;
				_ = Brushes.LightGray;
				color = Color.Green;
			}
			Application.set_UseWaitCursor(true);
			Maps.StarMap(formGraphics, CentreRA, CentreDec, TopLeftX, TopLeftY, ChartWidth, ChartHeightDegrees, MagLimit, VisualMag, GridLines, RotateAngle, FlipHorizontal, FlipVertical, Use_Gaia, use_UCAC4, Use_NOMAD, Use_PPMXL, DrawInBW || Printer, ForAsteroidWorldMap: false, CompStarRA, CompStarDec);
			Application.set_UseWaitCursor(false);
			pen2.Color = Color.Gray;
			formGraphics.DrawEllipse(pen2, TopLeftX + num - 10f, TopLeftY + num - 10f, 20f, 20f);
			pen2.DashPattern = new float[2] { 100f, 1f };
			pen2.Color = color;
			for (int i = 0; i < 11; i += 2)
			{
				Maps.StarMapOrthoProjection(PlotRAAsteroid[i], PlotDecAsteroid[i], out var x, out var y, out var xFromCentre, out var yFromCentre, RotateAngle, FlipHorizontal, FlipVertical);
				bool flag = (Math.Abs(xFromCentre) < num) & (Math.Abs(yFromCentre) < num);
				Maps.StarMapOrthoProjection(PlotRAAsteroid[i + 1], PlotDecAsteroid[i + 1], out var x2, out var y2, out var xFromCentre2, out var yFromCentre2, RotateAngle, FlipHorizontal, FlipVertical);
				bool flag2 = (Math.Abs(xFromCentre2) < num) & (Math.Abs(yFromCentre2) < num);
				float IntersectX;
				float IntersectY;
				if (flag && flag2)
				{
					formGraphics.DrawLine(pen2, x, y, x2, y2);
				}
				else if (!flag && flag2)
				{
					Intersection(xFromCentre2, yFromCentre2, xFromCentre, yFromCentre, num, out IntersectX, out IntersectY);
					formGraphics.DrawLine(pen2, IntersectX + num + TopLeftX, IntersectY + num + TopLeftY, x2, y2);
				}
				else if (flag && !flag2)
				{
					Intersection(xFromCentre2, yFromCentre2, xFromCentre, yFromCentre, num, out IntersectX, out IntersectY);
					formGraphics.DrawLine(pen2, x2, y2, IntersectX + num + TopLeftX, IntersectY + num + TopLeftY);
				}
			}
			try
			{
				if (UTDate == null)
				{
					UTDate = "Not set";
				}
				if (AsteroidName == null)
				{
					AsteroidName = "Not set";
				}
				string text = UTDate.Trim() + ", " + AsteroidName.Trim() + string.Format(" @ {0,1:f0}hr steps", StarChart_EphemTimeStep * 24.0);
				text = ((!(ChartHeightDegrees >= 1f)) ? (text + string.Format("    Plot {0,1:F0}' x {0,1:F0}', to Mag ", ChartHeightDegrees * 60f)) : (text + string.Format("    Plot {0,1:F0}° x {0,1:F0}°, to Mag ", ChartHeightDegrees)));
				text += string.Format("{0,3:F1}", MagLimit);
				formGraphics.FillRectangle(brush2, 4f, 4f, formGraphics.MeasureString(text, font).Width, formGraphics.MeasureString(text, font).Height);
				formGraphics.DrawRectangle(pen, 4f, 4f, formGraphics.MeasureString(text, font).Width, formGraphics.MeasureString(text, font).Height);
				formGraphics.DrawString(text, font, brush, 4f, 4f);
			}
			catch
			{
			}
		}

		internal static void ShowPlanetConfiguration()
		{
			try
			{
				((Control)PlanetConfiguration).Show();
			}
			catch
			{
				PlanetConfiguration = new PlanetViewer();
				((Control)PlanetConfiguration).Show();
			}
			PlanetConfiguration.SetPlanet(PlanetNumber);
			PlanetConfiguration.EnablePlanetSelection(Enable: false);
			PlanetConfiguration.updnYear.set_Value((decimal)EventYear);
			PlanetConfiguration.updnMonth.set_Value((decimal)EventMonth);
			PlanetConfiguration.updnDay.set_Value((decimal)(int)EventDay);
			PlanetConfiguration.updnHour.set_Value((decimal)MidTime);
			PlanetConfiguration.chkMagnify.set_Checked(false);
			PlanetConfiguration.chkMagnify2.set_Checked((PlanetNumber >= 5) & (PlanetNumber <= 8) & !IsMoon);
			PlanetConfiguration.ShowStarPath = true;
			Application.DoEvents();
			((Control)PlanetConfiguration).Focus();
			PlanetConfiguration.DrawPlanetsAndMoons();
		}

		public static void Plot_MultiPaths(bool FromListAndDisplay)
		{
			Plot_MultiPaths(FromListAndDisplay, OnlyWhereSunIsDown: false);
		}

		public static void Plot_MultiPaths(bool FromListAndDisplay, bool OnlyWhereSunIsDown)
		{
			try
			{
				((Control)PlotMultipath).Show();
			}
			catch
			{
				PlotMultipath = new AsteroidPlotMultipath();
				((Control)PlotMultipath).Show();
			}
			if (FromListAndDisplay)
			{
				CreateMultiPathFile(OnlyWhereSunIsDown);
			}
			PlotMultipath.PlotMultiPath();
		}

		private static void CreateMultiPathFile()
		{
			CreateMultiPathFile(OnlyWhereSunIsDown_StarIsUp: false);
		}

		private static void CreateMultiPathFile(bool OnlyWhereSunIsDown_StarIsUp)
		{
			string text = "";
			int result = -1;
			int num = 100;
			int num2 = 0;
			if (!(!File.Exists(AppPath + "\\Generated Files\\multipath.tmp") | !MultipathFileFromListAndDisplayIsCurrent))
			{
				return;
			}
			using StreamWriter streamWriter = new StreamWriter(AppPath + "\\Generated Files\\multipath.tmp");
			for (int i = 3; i < ListAndDisplay.lstSummary.get_Items().get_Count(); i++)
			{
				string text2 = ListAndDisplay.lstSummary.get_Items().get_Item(i).ToString()!.Trim();
				if (text2.Length <= 50)
				{
					continue;
				}
				int startIndex = text2.LastIndexOf(" ");
				if (!int.TryParse(text2.Substring(startIndex), out result))
				{
					result = -1;
				}
				if (result < 0)
				{
					continue;
				}
				Set_Data_From_OccultationElements_Record(result, PlotEvent: false);
				num = 100;
				num2 = 0;
				if (OnlyWhereSunIsDown_StarIsUp)
				{
					if (Mv >= 3.0)
					{
						num = (int)(0.0 - Mv);
					}
					if (num < -12)
					{
						num = -12;
					}
					num2 = ((!(Mv < 10.0)) ? 10 : ((int)Mv));
				}
				text = ((AsteroidNumber > 0) ? (UTDate + " (" + AsteroidNumber + ")") : ((!((AsteroidName.Substring(0, 1) == "P") & (AsteroidName.Substring(2, 1) == "M"))) ? (UTDate + " (" + AsteroidName.Trim() + ")") : (UTDate + " (" + AsteroidName.Substring(0, 5) + ")")));
				streamWriter.WriteLine("+++  " + text);
				AsteroidPath(2.0, UseLocalTopography: false, 0.0, 0.0);
				int count = PathCoordinates.Count;
				for (int j = 0; j < count; j++)
				{
					if ((PathCoordinates[j].SunAlt < (double)num) & (PathCoordinates[j].StarAlt > (double)num2))
					{
						streamWriter.WriteLine(PathCoordinates[j].ToString_PathForPlot());
					}
				}
				streamWriter.WriteLine("*****");
			}
			MultipathFileFromListAndDisplayIsCurrent = true;
		}

		public static void Show_Multilocations()
		{
			try
			{
				((Control)MultiLocations).Show();
			}
			catch
			{
				MultiLocations = new AsteroidMultiLocations();
				((Control)MultiLocations).Show();
			}
			MultiLocations.ComputePrediction();
		}

		public static void MultiLocationComputation(string MultiSiteFileName, int LimitFlag, bool Miles, bool use3Sigma)
		{
			double num = 0.0;
			double Ta = 0.0;
			double num2 = 0.0;
			double MinDistance = 0.0;
			Sites sites = new Sites();
			if (!File.Exists(AppPath + "\\Sites\\" + MultiSiteFileName))
			{
				return;
			}
			using (StreamReader streamReader = new StreamReader(AppPath + "\\Sites\\" + MultiSiteFileName))
			{
				double num3 = 1.0;
				if (Miles)
				{
					num3 = 1.609;
				}
				double num4 = AsteroidShadowDiameter_Penumbral / 2.0 / VerticalScaleFactor;
				double num5 = ErrorAcrossPath(DeltaX, DeltaY, ErrorMajor, ErrorMinor, ErrorPA);
				if (use3Sigma)
				{
					num5 *= 3.0;
				}
				double num6 = 6378.137 * num5 / 8.794143836182533 * DistanceToAsteroid / VerticalScaleFactor;
				double num7 = (double)LimitFlag * num6 + num4;
				if (LimitFlag == 0)
				{
					num7 = 20000.0;
				}
				MultiEvent = new List<AsteroidMultiPredictLine>();
				MultiEvent.Clear();
				AsteroidMultiPredictLine asteroidMultiPredictLine = new AsteroidMultiPredictLine();
				double num8 = num6 + num4;
				asteroidMultiPredictLine.Distance = num8 / num3;
				asteroidMultiPredictLine.Probability = Probability(num4, num6, num8);
				asteroidMultiPredictLine.SiteName = "** Path limit plus 1-Sigma **";
				MultiEvent.Add(asteroidMultiPredictLine);
				asteroidMultiPredictLine = new AsteroidMultiPredictLine();
				num8 = 0.0 - num6 - num4;
				asteroidMultiPredictLine.Distance = num8 / num3;
				asteroidMultiPredictLine.Probability = Probability(num4, num6, num8);
				asteroidMultiPredictLine.SiteName = "** Path limit plus 1-Sigma **";
				MultiEvent.Add(asteroidMultiPredictLine);
				asteroidMultiPredictLine = new AsteroidMultiPredictLine();
				num8 = 2.0 * num6 + num4;
				asteroidMultiPredictLine.Distance = num8 / num3;
				asteroidMultiPredictLine.Probability = Probability(num4, num6, num8);
				asteroidMultiPredictLine.SiteName = "** Path limit plus 2-Sigma **";
				MultiEvent.Add(asteroidMultiPredictLine);
				asteroidMultiPredictLine = new AsteroidMultiPredictLine();
				num8 = -2.0 * num6 - num4;
				asteroidMultiPredictLine.Distance = num8 / num3;
				asteroidMultiPredictLine.Probability = Probability(num4, num6, num8);
				asteroidMultiPredictLine.SiteName = "** Path limit plus 2-Sigma **";
				MultiEvent.Add(asteroidMultiPredictLine);
				asteroidMultiPredictLine = new AsteroidMultiPredictLine();
				num8 = 3.0 * num6 + num4;
				asteroidMultiPredictLine.Distance = num8 / num3;
				asteroidMultiPredictLine.Probability = Probability(num4, num6, num8);
				asteroidMultiPredictLine.SiteName = "** Path limit plus 3-Sigma **";
				MultiEvent.Add(asteroidMultiPredictLine);
				asteroidMultiPredictLine = new AsteroidMultiPredictLine();
				num8 = -3.0 * num6 - num4;
				asteroidMultiPredictLine.Distance = num8 / num3;
				asteroidMultiPredictLine.Probability = Probability(num4, num6, num8);
				asteroidMultiPredictLine.SiteName = "** Path limit plus 3-Sigma **";
				MultiEvent.Add(asteroidMultiPredictLine);
				do
				{
					sites.Read_SiteFile(streamReader.ReadLine());
					num = sites.Longitude;
					double pCos = sites.pCos;
					double pSin = sites.pSin;
					num2 = ErrorInTime / 240.0;
					GetTforMinimumDistance(num + num2, pCos, pSin, 0.0, ref Ta, out var H, out MinDistance, PathDistance: true, out var Separation, out var TClosest, out var Z, out var ZSun, out var Direction);
					GetTforMinimumDistance(num, pCos, pSin, 0.0, ref Ta, out H, out num8, PathDistance: true, out Separation, out TClosest, out Z, out ZSun, out Direction);
					double num9 = Math.Abs(MinDistance - num8);
					if (Z > -0.01)
					{
						double num10 = Math.Asin(Z);
						double num11 = Utilities.Refraction_deg(num10 * (180.0 / Math.PI), 1016.0, 0.0) / (180.0 / Math.PI);
						if (Z < 0.2)
						{
							double num12 = 1.000278 * (Math.Cos(num11) - Math.Sin(num11) * Math.Tan(num10));
							GetTforMinimumDistance(num, num12 * pCos, num12 * pSin, 0.0, ref Ta, out H, out num8, PathDistance: true, out Separation, out TClosest, out Z, out ZSun, out Direction);
						}
					}
					if (Math.Abs(num8) < num7 && ((Z > 0.0) & ((ZSun < -0.02) | (Mv < 5.0))))
					{
						asteroidMultiPredictLine = new AsteroidMultiPredictLine();
						asteroidMultiPredictLine.Distance = num8 / num3;
						if (VerticalScaleFactor == 0.05)
						{
							asteroidMultiPredictLine.Distance = num8 / num3 * VerticalScaleFactor;
						}
						asteroidMultiPredictLine.Direction = Direction;
						asteroidMultiPredictLine.Probability = Probability(num4, num6 + num9, num8);
						asteroidMultiPredictLine.SiteName = sites.Name.PadRight(34);
						asteroidMultiPredictLine.Longitude = sites.Longitude;
						asteroidMultiPredictLine.Latitude = sites.Latitude;
						asteroidMultiPredictLine.SiteAlt = sites.Altitude;
						asteroidMultiPredictLine.UT = TClosest;
						asteroidMultiPredictLine.Separation = Separation;
						asteroidMultiPredictLine.StarAlt = 180.0 / Math.PI * Math.Asin(Z);
						asteroidMultiPredictLine.SunAlt = 180.0 / Math.PI * Math.Asin(ZSun);
						MultiEvent.Add(asteroidMultiPredictLine);
					}
				}
				while (!streamReader.EndOfStream);
				asteroidMultiPredictLine = new AsteroidMultiPredictLine();
				num8 = 0.0;
				asteroidMultiPredictLine.Distance = num8 / num3;
				asteroidMultiPredictLine.Probability = Probability(num4, num6, num8);
				asteroidMultiPredictLine.SiteName = "**** Centre Line    ****";
				MultiEvent.Add(asteroidMultiPredictLine);
				asteroidMultiPredictLine = new AsteroidMultiPredictLine();
				num8 = num4;
				asteroidMultiPredictLine.Distance = num8 / num3;
				asteroidMultiPredictLine.Probability = Probability(num4, num6, num8);
				asteroidMultiPredictLine.SiteName = "***  Path limit  ***";
				MultiEvent.Add(asteroidMultiPredictLine);
				asteroidMultiPredictLine = new AsteroidMultiPredictLine();
				num8 = 0.0 - num4;
				asteroidMultiPredictLine.Distance = num8 / num3;
				asteroidMultiPredictLine.Probability = Probability(num4, num6, num8);
				asteroidMultiPredictLine.SiteName = "***  Path limit  ***";
				MultiEvent.Add(asteroidMultiPredictLine);
			}
			MultiEvent.Sort();
		}

		public static void Show_PlanetContactTimes()
		{
			try
			{
				((Control)ContactTimes).Show();
			}
			catch
			{
				ContactTimes = new AsteroidPlanetContactTimes();
				((Control)ContactTimes).Show();
			}
			((Control)ContactTimes).Focus();
			ContactTimes.IsAsteroid = !IsPlanet;
			ContactTimes.AsteroidHasSatellites = NumOfMoons > 0;
			MultiLocation_PlanetTimes(ContactTimes.cmbSites.get_Items().get_Item(ContactTimes.cmbSites.get_SelectedIndex()).ToString());
			ContactTimes.Display();
		}

		public static bool Planet_Local_Event_Times(double LonObs, double LatObs, bool MultiSite, out string PlanetOut)
		{
			Satellites.SetPlanetPlusMoonPlotParameters(PlanetNumber, MoonNumber);
			double[] array = new double[24];
			double[] array2 = new double[24];
			double[] array3 = new double[24];
			bool[] array4 = new bool[24];
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			double num4 = 0.0;
			bool flag = false;
			bool flag2 = false;
			PlanetOut = "";
			if (MoonNumber > 0 && Satellites.PlanetDiameterKM < 1000f)
			{
				return false;
			}
			for (int i = 0; i < 24; i++)
			{
				array4[i] = false;
			}
			for (int j = 0; j <= 1; j++)
			{
				double num5 = (double)(Satellites.f / 2f) + (double)(Satellites.f / 2f) * Math.Cos(2.0 * PlanetocentricEarthDec / (180.0 / Math.PI));
				double num6 = 0.0;
				double num7 = 0.0;
				double num8 = 0.0;
				double num9 = Math.Atan(Utilities.sqrt_1LessEarthEllipticitySqrd * Math.Tan(LatObs));
				num = Math.Cos(num9);
				num2 = Utilities.sqrt_1LessEarthEllipticitySqrd * Math.Sin(num9);
				double num10;
				double num18;
				bool flag3;
				do
				{
					num10 = num6;
					double num11 = (0.0 - SubstellarLongitude) / (180.0 / Math.PI) + 0.2625161707907961 * num6 + LonObs;
					double num12 = Xatmin + DeltaX * num6 + Delta2X * num6 * num6 + Delta3X * num6 * num6 * num6 - num * Math.Sin(num11);
					double num13 = YatMin + DeltaY * num6 + Delta2Y * num6 * num6 + Delta3Y * num6 * num6 * num6 - num2 * Math.Cos(FPlaneDec) + num * Math.Cos(num11) * Math.Sin(FPlaneDec);
					double num14 = DeltaX + 2.0 * Delta2X * num6 + 3.0 * Delta3X * num6 * num6 - 0.2625161707907961 * num * Math.Cos(num11);
					double num15 = DeltaY + 2.0 * Delta2Y * num6 + 3.0 * Delta3Y * num6 * num6 - 0.2625161707907961 * num * Math.Sin(num11) * Math.Sin(FPlaneDec);
					double num16 = num14 * num14 + num15 * num15;
					num3 = Math.Sqrt(num16);
					double num17 = num12 * num14 + num13 * num15;
					num18 = Math.Atan2(num12, num13) * (180.0 / Math.PI);
					double num19;
					for (num19 = num18 - PAPlanetPole - 90.0; num19 > 180.0; num19 -= 360.0)
					{
					}
					for (; num19 < -180.0; num19 += 360.0)
					{
					}
					num19 = Math.Abs(num19);
					if (num19 > 90.0)
					{
						num19 = 180.0 - num19;
					}
					num6 = num10 - num17 / num16;
					double num20 = (num12 * num15 - num13 * num14) / num3;
					double num21 = (double)(Satellites.PlanetRadius / 8.79414f) * (1.0 - num5 / 2.0 + num5 / 2.0 * Math.Cos(2.0 * num19 / (180.0 / Math.PI)));
					if (Math.Abs(num20) > num21)
					{
						num7 += 1.0;
						flag3 = false;
						if (num7 > 15.0)
						{
							break;
						}
					}
					else
					{
						double num22 = Math.Sqrt(num21 * num21 - num20 * num20) / num3;
						num6 = ((j != 0) ? (num6 + num22) : (num6 - num22));
						flag3 = true;
						flag2 = true;
					}
					if (num4 > 0.4)
					{
						break;
					}
					num8 += 1.0;
				}
				while (Math.Abs(num10 - num6) > 0.001 && num8 < 20.0);
				array[j] = MidTime + num6;
				array4[j] = flag3;
				array3[j] = num18 - 180.0;
				if (array3[j] < 0.0)
				{
					array3[j] += 360.0;
				}
			}
			int num23 = Satellites.NumberOfRings;
			if (!MultiSite)
			{
				num23 = 0;
			}
			if (num23 > 0)
			{
				double num24 = Math.Cos(PAPlanetPole / (180.0 / Math.PI));
				double num25 = Math.Sin(PAPlanetPole / (180.0 / Math.PI));
				double num26 = 5000.0;
				double num27 = 0.01;
				for (int num28 = num23; num28 >= 1; num28--)
				{
					double num29 = Satellites.RingRadius[num28] * Satellites.PlanetRadius / 8.79414f;
					double num30 = num29 * Math.Sin(PlanetocentricEarthDec / (180.0 / Math.PI));
					if (num30 == 0.0)
					{
						num30 = 1E-06;
					}
					double num31 = num29 / num3 + 0.2;
					flag = false;
					for (double num32 = 0.0 - num31; num32 <= num31; num32 += num27)
					{
						double num11 = (0.0 - SubstellarLongitude) / (180.0 / Math.PI) + 0.2625161707907961 * num32 + LonObs;
						double num33 = Xatmin + DeltaX * num32 + Delta2X * num32 * num32 + Delta3X * num32 * num32 * num32 - num * Math.Sin(num11);
						double num34 = YatMin + DeltaY * num32 + Delta2Y * num32 * num32 + Delta3Y * num32 * num32 * num32 - num2 * Math.Cos(FPlaneDec) + num * Math.Cos(num11) * Math.Sin(FPlaneDec);
						double num35 = (num33 * num24 - num34 * num25) / num29;
						double num36 = (num34 * num24 + num33 * num25) / num30;
						double num37 = Math.Sqrt(num35 * num35 + num36 * num36) - 1.0;
						if (num32 > 0.0 - num31)
						{
							if (num26 > 0.0 && num37 < 0.0)
							{
								array[2 * num28] = MidTime + num32 + num27 * num37 / (num26 - num37);
								array4[2 * num28] = true;
								flag = true;
							}
							if (num26 < 0.0 && num37 > 0.0)
							{
								array[2 * num28 + 1] = MidTime + num32 + num27 * num37 / (num26 - num37);
								array4[2 * num28 + 1] = true;
								flag = true;
								flag2 = true;
								break;
							}
							if (num26 > 0.0 && num37 > num26)
							{
								break;
							}
						}
						num26 = num37;
					}
					if (!flag)
					{
						break;
					}
				}
			}
			for (int k = 0; k <= 2 * num23 + 1; k++)
			{
				if (array4[k])
				{
					double num11 = (0.0 - SubstellarLongitude) / (180.0 / Math.PI) + 0.2625161707907961 * (array[k] - MidTime) + LonObs;
					double num38 = num2 * Math.Sin(FPlaneDec) + num * Math.Cos(num11) * Math.Cos(FPlaneDec);
					array2[k] = Math.Asin(num38) * (180.0 / Math.PI);
					double num39 = num2 * Math.Sin(SubSolarLatitude_OfDate / (180.0 / Math.PI)) + num * Math.Cos(num11 + (SubstellarLongitude - SubSolarLongitude_OfDate) / (180.0 / Math.PI)) * Math.Cos(SubSolarLatitude_OfDate / (180.0 / Math.PI));
					if (num38 < 0.0)
					{
						array4[k] = false;
					}
					if ((num39 > 0.0) & (Mv > 5.0))
					{
						array4[k] = false;
					}
				}
				flag2 |= array4[k];
			}
			if (flag2)
			{
				StringBuilder stringBuilder = new StringBuilder();
				for (int num40 = 2 * num23; num40 >= 0; num40 -= 2)
				{
					if (array4[num40] & !((array[num40] > array[0]) & (array[num40] < array[1])))
					{
						double num41 = array[num40];
						if (num41 < 0.0)
						{
							num41 += 24.0;
						}
						stringBuilder.AppendFormat(Utilities.DEGtoDMS(num41, 2, 1, MinutesOnly: true) + "  ");
						if (!MultiSite && num40 == 0)
						{
							stringBuilder.AppendFormat("{0,3:F0}", array3[0]);
							stringBuilder.AppendFormat("{0,4:F0}   ", array2[0]);
						}
					}
					else
					{
						stringBuilder.AppendFormat(".. ....  ");
						if (!MultiSite && num40 == 0)
						{
							stringBuilder.AppendFormat("".PadRight(10));
						}
					}
				}
				for (int l = 1; l <= 2 * num23 + 1; l += 2)
				{
					if (array4[l] & !((array[l] > array[0]) & (array[l] < array[1])))
					{
						double num41 = array[l];
						if (num41 < 0.0)
						{
							num41 += 24.0;
						}
						stringBuilder.AppendFormat(Utilities.DEGtoDMS(num41, 2, 1, MinutesOnly: true) + "  ");
						if (!MultiSite && l == 1)
						{
							stringBuilder.AppendFormat("{0,3:F0}", array3[1]);
							stringBuilder.AppendFormat("{0,4:F0}", array2[1]);
						}
					}
					else
					{
						stringBuilder.AppendFormat(".. ....  ");
						if (!MultiSite && l == 1)
						{
							stringBuilder.AppendFormat("".PadRight(7));
						}
					}
				}
				PlanetOut = stringBuilder.ToString();
			}
			return flag2;
		}

		public static bool AsteroidRings_Local_Event_Times(int AsteroidNumber, double LonObs, double LatObs, bool MultiSite, out string PlanetOut)
		{
			double[] array = new double[24];
			double[] array2 = new double[24];
			_ = new double[24];
			bool[] array3 = new bool[24];
			double[] RingRadius = new double[5];
			double num = 0.0;
			bool flag = false;
			PlanetOut = "";
			if (AsteroidRings_All.AsteroidRings.Count < 1)
			{
				AsteroidRings_All.Fill_AllAsteroidRings();
			}
			if (AsteroidRings_All.AsteroidRings.Count < 1)
			{
				return false;
			}
			for (int i = 0; i < 24; i++)
			{
				array3[i] = false;
			}
			AsteroidRings_All.GetRingDetails(AsteroidNumber, out var _, out var _, ref RingRadius, out var NumberOfRings);
			if (NumberOfRings < 1)
			{
				return false;
			}
			for (int j = 0; j < NumberOfRings - 1; j++)
			{
				for (int k = 0; k < NumberOfRings - j - 1; k++)
				{
					if (RingRadius[k] > RingRadius[k + 1])
					{
						Utilities.Swap(ref RingRadius[k], ref RingRadius[k + 1]);
					}
				}
			}
			if (!MultiSite)
			{
				NumberOfRings = 0;
			}
			double num2 = Math.Cos(PARingPole);
			double num3 = Math.Sin(PARingPole);
			double num4 = Math.Atan(Utilities.sqrt_1LessEarthEllipticitySqrd * Math.Tan(LatObs));
			double num5 = Math.Cos(num4);
			double num6 = Utilities.sqrt_1LessEarthEllipticitySqrd * Math.Sin(num4);
			double num7;
			double num14;
			do
			{
				num7 = num;
				double num8 = (0.0 - SubstellarLongitude) / (180.0 / Math.PI) + 0.2625161707907961 * num + LonObs;
				double num9 = Xatmin + DeltaX * num + Delta2X * num * num + Delta3X * num * num * num - num5 * Math.Sin(num8);
				double num10 = YatMin + DeltaY * num + Delta2Y * num * num + Delta3Y * num * num * num - num6 * Math.Cos(FPlaneDec) + num5 * Math.Cos(num8) * Math.Sin(FPlaneDec);
				double num11 = DeltaX + 2.0 * Delta2X * num + 3.0 * Delta3X * num * num - 0.2625161707907961 * num5 * Math.Cos(num8);
				double num12 = DeltaY + 2.0 * Delta2Y * num + 3.0 * Delta3Y * num * num - 0.2625161707907961 * num5 * Math.Sin(num8) * Math.Sin(FPlaneDec);
				double num13 = num11 * num11 + num12 * num12;
				num14 = Math.Sqrt(num13);
				double num15 = num9 * num11 + num10 * num12;
				num = num7 - num15 / num13;
			}
			while (Math.Abs(num7 - num) > 0.001);
			array[0] = MidTime + num;
			array3[0] = true;
			double num16 = 5000.0;
			double num17 = 0.0027778;
			for (int num18 = NumberOfRings; num18 >= 1; num18--)
			{
				double num19 = RingRadius[num18 - 1] / 6378.137;
				double num20 = num19 * Math.Sin(PlanetocentricRingLatitude);
				if (num20 == 0.0)
				{
					num20 = 1E-06;
				}
				double num21 = (num19 + 1.05) / num14;
				bool flag2 = false;
				for (double num22 = 0.0 - num21; num22 <= num21; num22 += num17)
				{
					double num8 = (0.0 - SubstellarLongitude) / (180.0 / Math.PI) + 0.2625161707907961 * num22 + LonObs;
					double num23 = Xatmin + DeltaX * num22 + Delta2X * num22 * num22 + Delta3X * num22 * num22 * num22 - num5 * Math.Sin(num8);
					double num24 = YatMin + DeltaY * num22 + Delta2Y * num22 * num22 + Delta3Y * num22 * num22 * num22 - num6 * Math.Cos(FPlaneDec) + num5 * Math.Cos(num8) * Math.Sin(FPlaneDec);
					double num25 = (num23 * num2 - num24 * num3) / num19;
					double num26 = (num24 * num2 + num23 * num3) / num20;
					double num27 = Math.Sqrt(num25 * num25 + num26 * num26) - 1.0;
					if (num22 > 0.0 - num21)
					{
						if (num16 > 0.0 && num27 < 0.0)
						{
							array[2 * num18] = MidTime + num22 + num17 * num27 / (num16 - num27);
							array3[2 * num18] = true;
							flag2 = true;
						}
						if (num16 < 0.0 && num27 > 0.0)
						{
							array[2 * num18 + 1] = MidTime + num22 + num17 * num27 / (num16 - num27);
							array3[2 * num18 + 1] = true;
							flag2 = true;
							flag = true;
							break;
						}
						if (num16 > 0.0 && num27 > num16)
						{
							break;
						}
					}
					num16 = num27;
				}
				if (!flag2)
				{
					break;
				}
			}
			for (int l = 0; l <= 2 * NumberOfRings + 1; l++)
			{
				if (array3[l])
				{
					double num8 = (0.0 - SubstellarLongitude) / (180.0 / Math.PI) + 0.2625161707907961 * (array[l] - MidTime) + LonObs;
					double num28 = num6 * Math.Sin(FPlaneDec) + num5 * Math.Cos(num8) * Math.Cos(FPlaneDec);
					array2[l] = Math.Asin(num28) * (180.0 / Math.PI);
					double num29 = num6 * Math.Sin(SubSolarLatitude_OfDate / (180.0 / Math.PI)) + num5 * Math.Cos(num8 + (SubstellarLongitude - SubSolarLongitude_OfDate) / (180.0 / Math.PI)) * Math.Cos(SubSolarLatitude_OfDate / (180.0 / Math.PI));
					if (num28 < 0.0)
					{
						array3[l] = false;
					}
					if ((num29 > 0.0) & (Mv > 5.0))
					{
						array3[l] = false;
					}
				}
				flag |= array3[l];
			}
			if (flag & (array2[0] > 0.0))
			{
				StringBuilder stringBuilder = new StringBuilder();
				double num30 = array[0];
				if (num30 < 0.0)
				{
					num30 += 24.0;
				}
				stringBuilder.AppendFormat(" " + Utilities.DEGtoDMS(num30, 2, 1, MinutesOnly: true) + " ");
				stringBuilder.AppendFormat(" {0,4:F0}°   ", array2[0]);
				for (int num31 = 2 * NumberOfRings; num31 >= 2; num31 -= 2)
				{
					if (array3[num31])
					{
						num30 = array[num31];
						if (num30 < 0.0)
						{
							num30 += 24.0;
						}
						stringBuilder.AppendFormat(Utilities.DEGtoDMS(num30, 2, 1, MinutesOnly: true) + "  ");
					}
					else
					{
						stringBuilder.AppendFormat(".. ....  ");
						if (!MultiSite && num31 == 0)
						{
							stringBuilder.AppendFormat("".PadRight(10));
						}
					}
				}
				for (int m = 3; m <= 2 * NumberOfRings + 1; m += 2)
				{
					if (array3[m])
					{
						num30 = array[m];
						if (num30 < 0.0)
						{
							num30 += 24.0;
						}
						stringBuilder.AppendFormat(Utilities.DEGtoDMS(num30, 2, 1, MinutesOnly: true) + "  ");
					}
					else
					{
						stringBuilder.AppendFormat(".. ....  ");
						if (!MultiSite && m == 1)
						{
							stringBuilder.AppendFormat("".PadRight(7));
						}
					}
				}
				PlanetOut = stringBuilder.ToString();
			}
			return flag;
		}

		public static void MultiLocation_PlanetTimes(string ActiveFile)
		{
			Sites sites = new Sites();
			StreamReader streamReader = new StreamReader(AppPath + "\\Sites\\" + ActiveFile);
			MultiPlanetEvent = new List<PlanetContactTimes>();
			MultiPlanetEvent.Clear();
			do
			{
				sites.Read_SiteFile(streamReader.ReadLine());
				string PlanetOut;
				if (IsPlanet | IsMoon)
				{
					if (Planet_Local_Event_Times(sites.Longitude / (180.0 / Math.PI), sites.Latitude / (180.0 / Math.PI), MultiSite: true, out PlanetOut))
					{
						PlanetContactTimes planetContactTimes = new PlanetContactTimes();
						planetContactTimes.SiteName = sites.Name;
						planetContactTimes.Longitude = sites.Longitude;
						planetContactTimes.Latitude = sites.Latitude;
						planetContactTimes.Times = PlanetOut;
						MultiPlanetEvent.Add(planetContactTimes);
					}
				}
				else if (AsteroidRings_Local_Event_Times(AsteroidNumber, sites.Longitude / (180.0 / Math.PI), sites.Latitude / (180.0 / Math.PI), MultiSite: true, out PlanetOut))
				{
					PlanetContactTimes planetContactTimes = new PlanetContactTimes();
					planetContactTimes.SiteName = sites.Name;
					planetContactTimes.Longitude = sites.Longitude;
					planetContactTimes.Latitude = sites.Latitude;
					planetContactTimes.Times = PlanetOut;
					MultiPlanetEvent.Add(planetContactTimes);
				}
			}
			while (!streamReader.EndOfStream);
			streamReader.Close();
		}

		public static void Show_PrePointStars()
		{
			try
			{
				((Control)PrePoint).Show();
			}
			catch
			{
				PrePoint = new AsteroidPrePointStars();
				((Control)PrePoint).Show();
			}
			Application.DoEvents();
			PrePoint.ComputeList(AddSAO: false);
		}

		public static void ComputePrePointStars(double ZoneWidthArcMin, bool GetSAO)
		{
			//IL_011a: Unknown result type (might be due to invalid IL or missing references)
			double num = 0.0;
			int[] array = new int[6];
			bool ServerDown = false;
			bool flag = File.Exists(Utilities.AppPath + "\\Resource files\\SAO1950.bin");
			double num2 = (double)Settings.Default.PrePointFaintStar;
			double num3 = (double)Settings.Default.PrePointFaintWidth / 60.0 / (180.0 / Math.PI);
			double num4 = (double)Settings.Default.PrePointBrightStar;
			double num5 = (double)Settings.Default.PrePointBrightWidth / 60.0 / (180.0 / Math.PI);
			double num6 = (double)Settings.Default.PrePointLeadTime;
			if (num6 == 24.0)
			{
				num6 = 23.93;
			}
			num6 = num6 * 15.0 / (180.0 / Math.PI);
			bool flag2 = Utilities.InternetIsAvailable();
			if (flag)
			{
				SAOids.InitialiseSAO();
			}
			BinaryReader binaryReader = null;
			BinaryReader binaryReader2 = null;
			FileStream fileStream = null;
			FileStream fileStream2 = null;
			Gaia.GetAvailableGaiaCatalogues();
			if (Gaia.GaiaPrimaryFiles.Count == 0)
			{
				MessageBox.Show("No standard Gaia catalogues available to create the list of Pre-Point stars", "No Gaia Cats", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return;
			}
			string text = "";
			text = ((Gaia.GaiaPrimaryFiles.Count == 1) ? Gaia.GaiaPrimaryFiles[0] : ((num2 < 9.1) ? Gaia.GaiaPrimaryFiles[Gaia.GaiaPrimaryFiles.Count - 1] : ((!Gaia.GaiaPrimaryFiles[Gaia.GaiaPrimaryFiles.Count - 1].Contains("9")) ? Gaia.GaiaPrimaryFiles[Gaia.GaiaPrimaryFiles.Count - 1] : Gaia.GaiaPrimaryFiles[Gaia.GaiaPrimaryFiles.Count - 2])));
			fileStream2 = new FileStream(Utilities.AppPath + "\\Resource Files\\Gaia\\" + text + ".bin", FileMode.Open, FileAccess.Read);
			fileStream = new FileStream(Utilities.AppPath + "\\Resource Files\\Gaia\\" + text + ".inx", FileMode.Open, FileAccess.Read);
			binaryReader = new BinaryReader(fileStream);
			binaryReader2 = new BinaryReader(fileStream2);
			if (text.Contains("DR3"))
			{
				Gaia.CurrentRecordLength = 58L;
			}
			else
			{
				Gaia.CurrentRecordLength = 48L;
			}
			double RA = StarRA_2000;
			double Dec = StarDec_2000;
			Cursor.set_Current(Cursors.get_WaitCursor());
			PrePointList = new List<AsteroidPrepoint_Stars>();
			AsteroidPrepoint_Stars asteroidPrepoint_Stars = new AsteroidPrepoint_Stars();
			asteroidPrepoint_Stars.TOffset = 0.0;
			asteroidPrepoint_Stars.Mv = Mv;
			asteroidPrepoint_Stars.RA = Utilities.DEGtoDMS(RA * (180.0 / Math.PI) / 15.0, 2, 1, MinutesOnly: true);
			asteroidPrepoint_Stars.Dec = Utilities.DEGtoDMS(Dec * (180.0 / Math.PI), 3, 0, MinutesOnly: true);
			asteroidPrepoint_Stars.DecDiff = "   star";
			PrePointList.Add(asteroidPrepoint_Stars);
			Utilities.PrecessStartToEnd(2451540.5, EventJD, use2006values_Not1976: false, ref RA, ref Dec);
			double num7 = RA * (180.0 / Math.PI);
			double num8 = num7 - num6 * (180.0 / Math.PI);
			if (num8 < 0.0)
			{
				num8 += 360.0;
			}
			if (ZoneWidthArcMin < 1.0)
			{
				ZoneWidthArcMin = 1.0;
			}
			double Arg = Dec * (180.0 / Math.PI) - ZoneWidthArcMin / 60.0 - 0.2;
			double Arg2 = Dec * (180.0 / Math.PI) + ZoneWidthArcMin / 60.0 + 0.2;
			if (Arg > Arg2)
			{
				Swap(ref Arg, ref Arg2);
			}
			int num9 = 179 - (int)(2.0 * Arg2);
			if (Arg2 < 0.0)
			{
				num9++;
			}
			if (num9 < 0)
			{
				num9 = 0;
			}
			int num10 = 179 - (int)(2.0 * Arg);
			if (Arg < 0.0)
			{
				num10++;
			}
			if (num10 < 0)
			{
				num10 = 0;
			}
			for (int i = num9; i <= num10; i++)
			{
				int num11 = 361 * i;
				int num12 = (int)Math.Floor(num8 - 0.02);
				if (num12 < 0)
				{
					num12 += 360;
				}
				if (num12 > 360)
				{
					num12 -= 360;
				}
				int num13 = (int)Math.Ceiling(num7 + 0.02);
				if (num13 > 360)
				{
					num13 -= 360;
				}
				if (num13 < 0)
				{
					num13 += 360;
				}
				fileStream.Seek(num11 * 4, SeekOrigin.Begin);
				array[0] = binaryReader.ReadInt32() - 27;
				if (array[0] < 0)
				{
					array[0] = 0;
				}
				fileStream.Seek((num11 + num12) * 4, SeekOrigin.Begin);
				array[1] = binaryReader.ReadInt32() - 1;
				if (array[1] < 0)
				{
					array[1] = 0;
				}
				fileStream.Seek((num11 + num13) * 4, SeekOrigin.Begin);
				array[2] = binaryReader.ReadInt32();
				fileStream.Seek((num11 + 360) * 4, SeekOrigin.Begin);
				array[3] = binaryReader.ReadInt32();
				int num14 = array[1];
				int num15 = array[2];
				double num16 = 0.0;
				if (num14 >= num15)
				{
					num14 = array[0];
					num15 = array[2];
					num16 = Math.PI * 2.0;
				}
				double num17 = num7;
				double num18 = num8 - num16;
				while (true)
				{
					if (!Gaia.ReadNext(fileStream2, binaryReader2, num14))
					{
						num14++;
					}
					else
					{
						double magGreen = Gaia.MagGreen;
						double RA2 = Gaia.RA_rad;
						num = Gaia.Dec_rad;
						if (magGreen <= num2 && Math.Abs(num - Dec) < num5)
						{
							Utilities.PrecessStartToEnd(2451540.5, EventJD, use2006values_Not1976: false, ref RA2, ref num);
							if ((Math.Abs(num - Dec) < num3 && magGreen <= num2) | ((magGreen <= num4) & (Math.Abs(num - Dec) < num5)))
							{
								double num19 = RA - RA2;
								if (num19 < 0.0)
								{
									num19 += Math.PI * 2.0;
								}
								if (num19 < num6)
								{
									asteroidPrepoint_Stars = new AsteroidPrepoint_Stars();
									asteroidPrepoint_Stars.TOffset = 0.9972696 * num19 * (180.0 / Math.PI) / 15.0;
									asteroidPrepoint_Stars.Mv = magGreen;
									asteroidPrepoint_Stars.RA = Utilities.DEGtoDMS(Gaia.RA_deg / 15.0, 2, 1, MinutesOnly: true);
									asteroidPrepoint_Stars.Dec = Utilities.DEGtoDMS(Gaia.Dec_deg, 3, 0, MinutesOnly: true);
									asteroidPrepoint_Stars.DecDiff = string.Format("{0,7:F1}  ", (Dec - num) * (180.0 / Math.PI) * 60.0);
									if (flag)
									{
										asteroidPrepoint_Stars.SAO = SAOids.SAOnumber(Gaia.RA_deg, Gaia.PMRA_deg * 3600.0, Gaia.Dec_deg, Gaia.PMDec_deg * 3600.0).ToString();
									}
									else if (flag2 && GetSAO && !ServerDown && magGreen < 8.1)
									{
										asteroidPrepoint_Stars.SAO = Vizier_Sesame.GetSAOnum(Gaia.RA_deg, Gaia.Dec_deg, out ServerDown);
									}
									PrePointList.Add(asteroidPrepoint_Stars);
								}
							}
						}
						num14++;
					}
					if (num14 > num15)
					{
						if (num16 == 0.0)
						{
							break;
						}
						num16 = 0.0;
						num14 = array[1];
						num15 = array[3];
						num17 += Math.PI * 2.0;
						num18 += Math.PI * 2.0;
						if (!(num16 >= 0.0))
						{
							break;
						}
					}
				}
			}
			if (flag)
			{
				SAOids.CloseSAO();
			}
			Cursor.set_Current(Cursors.get_Default());
			PrePointList.Sort();
		}

		private static void Swap(ref double Arg1, ref double Arg2)
		{
			double num = Arg1;
			Arg1 = Arg2;
			Arg2 = num;
		}

		internal static void GeneratePredictionOutputFilesFromSummaryPage(bool FromListAndDisplay, string ElementsFileName, int RecNum, string MultiSiteFileName, double WorldMapPlotScale, bool UpdatedMultiSiteFile, StreamWriter OutIndexFile)
		{
			double result = 0.0;
			if (FromListAndDisplay)
			{
				Set_Data_From_OccultationElements_Record(RecNum, PlotEvent: false);
			}
			else
			{
				AutoPredictReadElements(ElementsFileName, RecNum);
			}
			if (FromListAndDisplay)
			{
				PlotMagnification = (PlotMagnification_Initial = WorldMapPlotScale);
			}
			else
			{
				PlotMagnification = PlotMagnification_Initial;
			}
			OutIndexFile.WriteLine(EventHeader.Substring(15) + "," + EventFileBase);
			string path = AppPath + "\\AutoGenerated Asteroids\\MJD";
			if (!Directory.Exists(path))
			{
				Directory.CreateDirectory(path);
			}
			string path2 = AppPath + "\\AutoGenerated Asteroids\\MJD\\" + EventFileBase + "_Date.txt";
			if (File.Exists(path2))
			{
				StreamReader streamReader = new StreamReader(path2);
				string s = streamReader.ReadLine();
				streamReader.Close();
				if (!double.TryParse(s, out result))
				{
					result = 0.0;
				}
				if (result == PredictionMJD && !UpdatedMultiSiteFile)
				{
					return;
				}
			}
			StreamWriter streamWriter = new StreamWriter(path2, append: false);
			streamWriter.WriteLine(PredictionMJD);
			streamWriter.Close();
			if (Settings.Default.AutoAsteroidMultisite)
			{
				AutoPredictMultiSite(MultiSiteFileName, AppPath + "\\AutoGenerated Asteroids\\" + EventFileBase + "_Sites.txt");
			}
			if (!(result == PredictionMJD && UpdatedMultiSiteFile))
			{
				if (Settings.Default.AutoAsteroidWorld)
				{
					AutoPredictWorldGraphic(AppPath + "\\AutoGenerated Asteroids\\" + EventFileBase, Settings.Default.AutoAsteroidColour, Settings.Default.SiteFileForAutoMap, AutoScaleMissEvents: true);
				}
				if (Settings.Default.AutoAsteroidStarChart)
				{
					AutoPredictStarChart(AppPath + "\\AutoGenerated Asteroids\\" + EventFileBase + "_Chart");
				}
				if (Settings.Default.AutoAsteroidPathCoords & (PathCoordinates.Count > 0))
				{
					AutoPredictPathCoords(AppPath + "\\AutoGenerated Asteroids\\" + EventFileBase + "_path.txt", IncludeSunUp: false);
				}
				if (Settings.Default.AutoAsteroidKML)
				{
					CreateGoogleEarthKMLFile(AppPath + "\\AutoGenerated Asteroids\\" + EventFileBase + ".KMZ", EventHeader, Auto: true, View: false, "");
				}
				if (Settings.Default.AutoAsteroidHTM)
				{
					CreateGoogleMapHTMFile(AppPath + "\\AutoGenerated Asteroids\\" + EventFileBase + ".HTM", EventHeader, Auto: true);
				}
				if (Settings.Default.AutoAsteroidPrePoint)
				{
					AutoPredictPrePointStars(AppPath + "\\AutoGenerated Asteroids\\" + EventFileBase + "_Stars.txt");
				}
				if (Settings.Default.AutoAsteroidBessel)
				{
					AutoPredictBessel(AppPath + "\\AutoGenerated Asteroids\\" + EventFileBase + "_Elements.txt", FromListAndDisplay, RecNum);
				}
				if (Settings.Default.AutoAsteroidDoubleInfo)
				{
					AutoPredictDoubleInfo(AppPath + "\\AutoGenerated Asteroids\\" + EventFileBase + "_DoubleInfo.txt");
				}
			}
		}

		internal static int GetNumberOfAutogeneratedFiles()
		{
			int num = 1;
			if (Settings.Default.AutoAsteroidMultisite)
			{
				num++;
			}
			if (Settings.Default.AutoAsteroidWorld)
			{
				num++;
			}
			if (Settings.Default.AutoAsteroidStarChart)
			{
				num++;
			}
			if (Settings.Default.AutoAsteroidPathCoords)
			{
				num++;
			}
			if (Settings.Default.AutoAsteroidKML)
			{
				num++;
			}
			if (Settings.Default.AutoAsteroidHTM)
			{
				num++;
			}
			if (Settings.Default.AutoAsteroidPrePoint)
			{
				num++;
			}
			if (Settings.Default.AutoAsteroidBessel)
			{
				num++;
			}
			if (Settings.Default.AutoAsteroidDoubleInfo)
			{
				num++;
			}
			return num;
		}

		internal static void AutoPredictReadElements(string ElementsFileName, int RecNum)
		{
			LoadISAM_and_DAMIT_ids(UpdateISAM: false);
			OldName = "";
			OldStar = "";
			ElementsStream = new FileStream(ElementsFileName, FileMode.Open, FileAccess.Read);
			Elements = new BinaryReader(ElementsStream);
			ElementFileLength = ElementsStream.Length;
			RecordsInFile = ElementFileLength / 540;
			Get_Data_From_540Record(RecNum, out var _, GeneratingSummary: false, AutoPlot: false);
			Elements.Close();
		}

		internal static void AutoPredictWorldGraphic(string OutputFile, bool PlotInColour, string SiteFile, bool AutoScaleMissEvents)
		{
			Bitmap bitmap = new Bitmap(1024, 768);
			Graphics graphics = Graphics.FromImage(bitmap);
			if (Settings.Default.GraphicsSmoothed)
			{
				graphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			graphics.Clear(Color.White);
			DrawPath(graphics, 1024, 768, SiteFile, 0, !PlotInColour, Printer: false, Auto: true, AutoScaleMissEvents);
			Application.DoEvents();
			switch (Settings.Default.GraphicsAutoSaveFileType)
			{
			case 0:
				bitmap.Save(OutputFile + ".jpg", ImageFormat.Jpeg);
				break;
			case 1:
				bitmap.Save(OutputFile + ".bmp", ImageFormat.Bmp);
				break;
			case 2:
				bitmap.Save(OutputFile + ".gif", ImageFormat.Gif);
				break;
			case 3:
				bitmap.Save(OutputFile + ".png", ImageFormat.Png);
				break;
			case 4:
				bitmap.Save(OutputFile + ".tif", ImageFormat.Tiff);
				break;
			}
			graphics.Dispose();
		}

		internal static void AutoPredictStarChart(string OutputFile)
		{
			bool flag = Settings.Default.AutoGenerateUseTychoGaia | Settings.Default.AutoGenerateUseGaia16 | Settings.Default.AutoGenerateUseGaia14;
			bool flag2 = Settings.Default.AutoGenerateUseUCAC4;
			bool use_NOMAD = Settings.Default.AutoGenerateUseNomad;
			bool flag3 = Settings.Default.AutoGenerateUsePPMXL;
			if (flag)
			{
				flag2 = (flag3 = (use_NOMAD = false));
			}
			else if (flag2)
			{
				flag3 = (use_NOMAD = false);
			}
			else if (flag3)
			{
				use_NOMAD = false;
			}
			int num = Settings.Default.AutoGenerateStarWidthPixels switch
			{
				0 => 480, 
				1 => 768, 
				_ => 1024, 
			};
			float chartHeightDegrees = Settings.Default.AutoGenerateStarWidthDeg switch
			{
				0 => 0.2f, 
				1 => 0.4f, 
				2 => 0.7f, 
				3 => 1f, 
				4 => 2f, 
				5 => 4f, 
				6 => 5f, 
				7 => 6f, 
				8 => 8f, 
				9 => 10f, 
				10 => 12f, 
				11 => 15f, 
				12 => 20f, 
				_ => 4f, 
			};
			float magLimit = Settings.Default.AutoGenerateStarMag switch
			{
				0 => 20f, 
				1 => 19f, 
				2 => 18f, 
				3 => 17f, 
				4 => 16f, 
				5 => 15f, 
				6 => 14f, 
				7 => 13f, 
				8 => 12f, 
				9 => 11f, 
				10 => 10f, 
				11 => 9f, 
				12 => 8f, 
				13 => 7f, 
				14 => 6f, 
				_ => 13f, 
			};
			Bitmap bitmap = new Bitmap(num, num);
			Graphics graphics = Graphics.FromImage(bitmap);
			if (Settings.Default.GraphicsSmoothed)
			{
				graphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			graphics.Clear(Color.White);
			DrawAsteroidStarChart(graphics, Printer: false, 0f, 0f, num, num, StarRA_2000, StarDec_2000, chartHeightDegrees, magLimit, VisualMag: true, GridLines: true, 0.0, FlipHorizontal: false, FlipVertical: false, !Settings.Default.AutoAsteroidColour, flag, flag2, use_NOMAD, flag3);
			switch (Settings.Default.GraphicsAutoSaveFileType)
			{
			case 0:
				bitmap.Save(OutputFile + ".jpg", ImageFormat.Jpeg);
				break;
			case 1:
				bitmap.Save(OutputFile + ".bmp", ImageFormat.Bmp);
				break;
			case 2:
				bitmap.Save(OutputFile + ".gif", ImageFormat.Gif);
				break;
			case 3:
				bitmap.Save(OutputFile + ".png", ImageFormat.Png);
				break;
			case 4:
				bitmap.Save(OutputFile + ".tif", ImageFormat.Tiff);
				break;
			}
			graphics.Dispose();
		}

		internal static void AutoPredictDoubleInfo(string OutputFile)
		{
			if (DoubleStarCode == 0)
			{
				return;
			}
			using StreamWriter streamWriter = new StreamWriter(OutputFile, append: false);
			streamWriter.Write(Interferometric_Plus_WDS.Find_WDS_IF_Matches(StarRA_2000 * (180.0 / Math.PI) / 15.0, StarDec_2000 * (180.0 / Math.PI), HighPrecision: false, ShowResults: false));
		}

		internal static void AutoPredictPathCoords(string OutputFile, bool IncludeSunUp)
		{
			AsteroidPath(1.0, UseLocalTopography: false, 0.0, 0.0);
			if (PathCoordinates.Count <= 0)
			{
				return;
			}
			StreamWriter streamWriter = new StreamWriter(OutputFile, append: false);
			int num = 0;
			bool flag = false;
			if (PathCoordinates.Count < 1)
			{
				return;
			}
			streamWriter.WriteLine(EventHeader);
			if (PathCoordinates[0].IterateOnT)
			{
				if (!AllowAsteroidRings)
				{
					streamWriter.WriteLine("                   Centre              Star  Star   Sun     Path Limit1           Path Limit2           Error Limit1          Error Limit2       Alt");
					streamWriter.WriteLine("  E. Longitude    Latitude      U.T.    Alt   Az    Alt" + "".PadRight(90) + "Crn");
					streamWriter.WriteLine("      o  '  \"     o  '  \"    h  m  s     o     o     o    o  '  \"    o  '  \"    o  '  \"    o  '  \"    o  '  \"    o  '  \"    o  '  \"    o  '  \"");
					streamWriter.WriteLine("".PadRight(57) + "Longitude  Latitude   Longitude  Latitude   Longitude  Latitude   Longitude  Latitude");
				}
				else
				{
					streamWriter.WriteLine("                   Centre              Star  Star   Sun     Path Limit1           Path Limit2           Error Limit1          Error Limit2            Ring Limit1           Ring Limit2       Alt");
					streamWriter.WriteLine("  E. Longitude    Latitude      U.T.    Alt   Az    Alt" + "".PadRight(135) + "Crn");
					streamWriter.WriteLine("      o  '  \"     o  '  \"    h  m  s     o     o     o    o  '  \"    o  '  \"    o  '  \"    o  '  \"    o  '  \"    o  '  \"    o  '  \"    o  '  \"    o  '  \"    o  '  \"");
					streamWriter.WriteLine("".PadRight(57) + "Longitude  Latitude   Longitude  Latitude   Longitude  Latitude   Longitude  Latitude   Longitude  Latitude");
				}
			}
			else
			{
				if (!AllowAsteroidRings)
				{
					streamWriter.WriteLine("                  Centre              Star  Star   Sun      Path Limits           Error Limits       Alt");
					streamWriter.WriteLine("  E. Longitude   Latitude      U.T.    Alt    Az   Alt    Limit 1    Limit 2    Limit 3    Limit 4   Crn");
					streamWriter.WriteLine("      o  '  \"     o  '  \"    h  m  s     o     o     o    o  '  \"    o  '  \"    o  '  \"    o  '  \"");
				}
				else
				{
					streamWriter.WriteLine("                  Centre              Star  Star   Sun      Path Limits           Error Limits          Ring Limits        Alt");
					streamWriter.WriteLine("  E. Longitude   Latitude      U.T.    Alt    Az   Alt    Limit 1    Limit 2    Limit 3    Limit 4    Limit 5    Limit 6   Crn");
					streamWriter.WriteLine("      o  '  \"     o  '  \"    h  m  s     o     o     o    o  '  \"    o  '  \"    o  '  \"    o  '  \"");
				}
				flag = PathCoordinates[0].IterateOnLatitude;
				if (flag)
				{
					if (!AllowAsteroidRings)
					{
						streamWriter.WriteLine("".PadRight(56) + "Longitude  Longitude  Longitude  Longitude");
					}
					else
					{
						streamWriter.WriteLine("".PadRight(56) + "Longitude  Longitude  Longitude  Longitude  Longitude  Longitude");
					}
				}
				else if (!AllowAsteroidRings)
				{
					streamWriter.WriteLine("".PadRight(57) + "Latitude   Latitude   Latitude   Latitude");
				}
				else
				{
					streamWriter.WriteLine("".PadRight(57) + "Latitude   Latitude   Latitude   Latitude   Latitude   Latitude");
				}
			}
			for (int i = 0; i < PathCoordinates.Count; i++)
			{
				if (!PathCoordinates[i].IterateOnLatitude && flag && (PathCoordinates[i].SunAlt < 0.0 || IncludeSunUp))
				{
					if (!AllowAsteroidRings)
					{
						streamWriter.WriteLine("".PadRight(57) + "Latitude   Latitude   Latitude   Latitude");
					}
					else
					{
						streamWriter.WriteLine("".PadRight(57) + "Latitude   Latitude   Latitude   Latitude   Latitude   Latitude");
					}
					flag = false;
				}
				if (PathCoordinates[i].SunAlt < 0.0 || IncludeSunUp)
				{
					streamWriter.WriteLine(PathCoordinates[i].ToString());
					num++;
					if ((num % 5 == 0) & Settings.Default.Skip5thLine)
					{
						streamWriter.WriteLine("");
					}
				}
			}
			streamWriter.WriteLine("");
			streamWriter.WriteLine("Uncertainty in time = +/- " + string.Format("{0,1:F0} secs", ErrorInTime));
			streamWriter.WriteLine("");
			streamWriter.WriteLine(PredictionDate);
			streamWriter.Close();
		}

		internal static void AutoPredictPrePointStars(string OutputFile)
		{
			ComputePrePointStars((double)Settings.Default.PrePointBrightWidth, Settings.Default.DefaultPrepointAddSAO);
			int num = PrePointList.Count - 1;
			if (PrePointList.Count <= 0)
			{
				return;
			}
			StreamWriter streamWriter = new StreamWriter(OutputFile, append: false);
			string text = "Occultation of " + StarNo.Trim() + " by ";
			int totalWidth = 20 + text.Length / 2;
			streamWriter.WriteLine(text.PadLeft(totalWidth));
			text = AsteroidName.Trim() + " on " + UTDate;
			totalWidth = 20 + text.Length / 2;
			streamWriter.WriteLine(text.PadLeft(totalWidth));
			streamWriter.WriteLine("           Pre-point stars");
			streamWriter.WriteLine("");
			streamWriter.WriteLine("     " + PredictionDate);
			streamWriter.WriteLine("");
			streamWriter.WriteLine("  Time                J2000         Dec");
			streamWriter.WriteLine(" Offset    Star    RA       Dec   Offset   SAO");
			streamWriter.WriteLine(" h  m  s    mag   h   m     o  '  ArcMin");
			streamWriter.WriteLine("");
			for (int i = 0; i <= num; i++)
			{
				streamWriter.WriteLine(PrePointList[i].ToString());
				if (i < num && Math.Floor(PrePointList[i].TOffset) != Math.Floor(PrePointList[i + 1].TOffset))
				{
					streamWriter.WriteLine("");
				}
			}
			streamWriter.Close();
		}

		internal static void AutoPredictMultiSite(string MultiSiteFileName, string OutputFile)
		{
			if (!File.Exists(AppPath + "\\Sites\\" + MultiSiteFileName))
			{
				return;
			}
			MultiLocationComputation(MultiSiteFileName, 3, Miles: false, use3Sigma: false);
			using StreamWriter streamWriter = new StreamWriter(OutputFile, append: false);
			streamWriter.WriteLine(EventHeader);
			streamWriter.WriteLine("");
			streamWriter.WriteLine("Distance from center of occultation path - in km");
			streamWriter.WriteLine("Distances are positive to the right, referenced to the direction of motion along the path");
			streamWriter.WriteLine("");
			streamWriter.WriteLine("Uncertainty in time = +/- " + string.Format("{0,1:F0} secs", ErrorInTime));
			streamWriter.WriteLine("          d Proba-  Location                           Longitude   Latitude    alt      U.T.     Sepn   Alt  Sun");
			streamWriter.WriteLine("       km   bility                                       o  '  \"    o  '  \"      m    h  m  s     \"       o    o");
			streamWriter.WriteLine("");
			for (int i = 0; i < MultiEvent.Count; i++)
			{
				streamWriter.WriteLine(MultiEvent[i].ToString(Abbreviated: true));
				if (((i + 1) % 5 == 0) & Settings.Default.Skip5thLine)
				{
					streamWriter.WriteLine("");
				}
			}
			streamWriter.WriteLine("");
			streamWriter.WriteLine(PredictionDate);
		}

		internal static void AutoPredictBessel(string OutputFile, bool FromListAndDisplay, int RecNum)
		{
			if (FromListAndDisplay)
			{
				using (StreamWriter streamWriter = new StreamWriter(OutputFile, append: false))
				{
					streamWriter.Write(OccElements[RecNum].ElementsIn540Format);
				}
				using StreamWriter streamWriter2 = new StreamWriter(OutputFile.Replace(".txt", ".xml"), append: false);
				streamWriter2.WriteLine("<Occultations>");
				streamWriter2.Write(OccElements[RecNum].XML_Elements());
				streamWriter2.WriteLine("</Occultations>");
				return;
			}
			using StreamWriter streamWriter3 = new StreamWriter(OutputFile, append: false);
			streamWriter3.WriteLine(Line1);
			streamWriter3.WriteLine(Line2);
			streamWriter3.WriteLine(Line3);
			streamWriter3.WriteLine(Line4);
			streamWriter3.WriteLine(Line5);
			streamWriter3.WriteLine(Line6);
			streamWriter3.WriteLine(Line7);
			streamWriter3.WriteLine(Line8);
			streamWriter3.WriteLine(Line9);
			streamWriter3.WriteLine(Line10);
		}

		internal static void OWFeedGen_Open(string FeedName)
		{
			AutogenerateOWpath = Utilities.AppPath + "\\OWfeeds\\" + FeedName.Replace(" ", "_");
			if (!Directory.Exists(AutogenerateOWpath))
			{
				Directory.CreateDirectory(AutogenerateOWpath);
			}
			string[] files = Directory.GetFiles(AutogenerateOWpath);
			for (int i = 0; i < files.Length; i++)
			{
				File.Delete(files[i]);
			}
			AutogenerateOWStreamWriter = new StreamWriter(AutogenerateOWpath + "\\index.html");
			string value = "\r\n            <html>\r\n            <body>\r\n            OccultWatcher Feed:&nbsp;<b>" + FeedName + "</b><br>\r\n            <br>\r\n            <table id='feed' cellspacing='2' border='1'>\r\n            <tr align='center'>\r\n            <th>\r\n            Event\r\n            </th>\r\n            <th>\r\n            AstNo\r\n            </th>\r\n            <th>\r\n            AstName\r\n            </th>\r\n            <th>\r\n            StarName\r\n            </th>\r\n            <th>\r\n            StarMag\r\n            </th>\r\n            <th>\r\n            Dur\r\n            </th>\r\n            <th>\r\n            Drop\r\n            </th>\r\n            <th>\r\n            DTFrom\r\n            </th>\r\n            <th>\r\n            DTTo\r\n            </th>\r\n            <th>\r\n            SunDist\r\n            </th>\r\n            <th>\r\n            MoonDist\r\n            </th>\r\n            <th>\r\n            MoonIll\r\n            </th>\r\n            <th>\r\n            StarRA\r\n            </th>\r\n            <th>\r\n            StarDE\r\n            </th>\r\n            <th>\r\n            &nbsp;\r\n            </th>\r\n            <th>\r\n            &nbsp;\r\n            </th>\r\n            </tr>";
			AutogenerateOWStreamWriter.WriteLine(value);
		}

		internal static void OWFeedGen_AddEvent(int RecNum, bool FromListAndDisplay, double WorldMapPlotScale)
		{
			string[] array = new string[5] { ".jpg", ".bmp", ".gif", ".png", ".tif" };
			Set_Data_From_OccultationElements_Record(RecNum, PlotEvent: false);
			using (StreamWriter streamWriter = new StreamWriter(AutogenerateOWpath + "\\" + EventFileBase + "_Elements.txt"))
			{
				streamWriter.Write(OccElements[RecNum].ElementsIn540Format.ToString());
			}
			if (FromListAndDisplay)
			{
				PlotMagnification = (PlotMagnification_Initial = WorldMapPlotScale);
			}
			else
			{
				PlotMagnification = PlotMagnification_Initial;
			}
			AutoPredictWorldGraphic(AutogenerateOWpath + "\\" + EventFileBase, Settings.Default.AutoAsteroidColour, Settings.Default.SiteFileForAutoMap, AutoScaleMissEvents: true);
			AutoPredictPathCoords(AutogenerateOWpath + "\\" + EventFileBase + "_path.txt", IncludeSunUp: false);
			using (StreamWriter streamWriter2 = new StreamWriter(AutogenerateOWpath + "\\" + EventFileBase + "_summary.html"))
			{
				string text = OccElements[RecNum].EventYear.ToString().Substring(2) + OccElements[RecNum].EventMonth.ToString().PadLeft(2, '0') + OccElements[RecNum].EventDay.ToString().PadLeft(2, '0') + " : ";
				text = text + " (" + OccElements[RecNum].ObjectNumber + ") " + OccElements[RecNum].ObjectName + " occults " + OccElements[RecNum].StarCatName;
				string text2 = "<html>\r\n                <body>\r\n\t                <table>\r\n\t\t                <tr>\r\n\t\t\t                <td>\r\n\t\t\t\t                <center>\r\n\t\t\t\t\t                <h3>" + text + "</h3>\r\n                                </center>\r\n\t\t\t                </td>\r\n\t\t                </tr>\r\n\t\t                <tr>\r\n\t\t\t                <td>&nbsp;</td>\r\n\t\t                </tr>\r\n\t\t\t\t        <tr>\r\n\t\t\t                <td>\r\n\t\t\t\t                <center>";
				text2 = ((!File.Exists(AutogenerateOWpath + "\\" + EventFileBase + "_path.txt")) ? (text2 + "</center></td>") : (text2 + "<a href='" + EventFileBase + "_path.txt'>View path coordinates</a></center></td>"));
				text2 = text2 + "<br/>\r\n\t\t\t\t\t    <br/>\r\n\t\t\t                </td>\r\n\t\t                </tr>\r\n\t\t                <tr>\r\n\t\t\t                <td>\r\n\t\t\t\t                <br/>\r\n\t\t\t\t                <center><img src='" + EventFileBase + array[Settings.Default.GraphicsAutoSaveFileType] + "'/></center>\r\n\t\t\t                </td>\r\n\t\t                </tr>\r\n\t                </table>\r\n                </body>\r\n                </html>\r\n                ";
				streamWriter2.Write(text2);
			}
			string text3 = "\r\n            <tr>\r\n            <td align='right'>" + OccElements[RecNum].EventYear.ToString().Substring(2) + OccElements[RecNum].EventMonth.ToString().PadLeft(2, '0') + OccElements[RecNum].EventDay.ToString().PadLeft(2, '0') + "</td>\r\n            <td align='right'>" + OccElements[RecNum].ObjectNumber + "</td>\r\n            <td align='right'>" + OccElements[RecNum].ObjectName + "</td>\r\n            <td align='right'>" + OccElements[RecNum].StarCatName + "</td>\r\n            <td align='right'>" + string.Format("{0,1:f1}", OccElements[RecNum].mV) + "</td>\r\n            <td align='right'>" + string.Format("{0,1:f1}", OccElements[RecNum].MaxDurn) + "</td>\r\n            <td align='right'>" + string.Format("{0,1:f1}", OccElements[RecNum].MagDropV) + "</td>\r\n            <td align='right'>" + HourMinute(MidTime - EarthSemiDuration, secs: false) + "</td>\r\n            <td align='right'>" + HourMinute(MidTime + EarthSemiDuration, secs: false) + "</td>\r\n            <td align='right'>" + string.Format("{0,1:f0}", OccElements[RecNum].SolarElongation) + "</td>\r\n            <td align='right'>" + string.Format("{0,1:f0}", OccElements[RecNum].StarMoonElongation_deg) + "</td>\r\n            <td align='right'>" + string.Format("{0,1:f0}", OccElements[RecNum].MoonPhase_percent) + "</td>\r\n            <td align='right'>" + Utilities.DEGtoDMS(OccElements[RecNum].RA_Star2000 * (180.0 / Math.PI) / 15.0, 2, 2, MinutesOnly: false) + "</td>\r\n            <td align='right'>" + Utilities.DEGtoDMS(OccElements[RecNum].Dec_Star2000 * (180.0 / Math.PI), 3, 1, MinutesOnly: false) + "</td>";
			text3 = ((!File.Exists(AutogenerateOWpath + "\\" + EventFileBase + "_path.txt")) ? (text3 + "<td align='right'></td>") : (text3 + "<td align='right'><a href='" + EventFileBase + "_path.txt'>PATH</a></td>"));
			text3 = text3 + "<td align='right'><a href='" + EventFileBase + "_summary.html'>DETAILS</a></td>\r\n            </tr>";
			AutogenerateOWStreamWriter.WriteLine(text3);
		}

		internal static void OWFeedGen_Close()
		{
			string value = "</ table >\r\n        </ body >\r\n        </ html > ";
			AutogenerateOWStreamWriter.WriteLine(value);
			AutogenerateOWStreamWriter.Close();
		}

		public static void CheckOCCELMNTFile(string FileNameIn)
		{
			//IL_0035: Unknown result type (might be due to invalid IL or missing references)
			//IL_003b: Invalid comparison between Unknown and I4
			//IL_004b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0052: Expected O, but got Unknown
			//IL_00a9: Unknown result type (might be due to invalid IL or missing references)
			//IL_00af: Invalid comparison between Unknown and I4
			//IL_0302: Unknown result type (might be due to invalid IL or missing references)
			//IL_032c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0340: Unknown result type (might be due to invalid IL or missing references)
			string[] array = new string[10];
			bool[] array2 = new bool[10];
			bool flag = false;
			int[] array3 = new int[10] { 12, 22, 12, 59, 46, 34, 62, 104, 71, 98 };
			int num = 0;
			if ((int)MessageBox.Show("This routine is to be used when a selected file of occultation elements\r\nis reported to be 'not in the current format'. It will create a file in \r\nthe same location as the selected file, but with the additional extension\r\nof '.error'. The report will identify all invalid records, and the relevant\r\nline(s) within that record\r\n\r\nIf the cause of the problem is not obvious, this report file should be sent\r\nto the author of OCCULT so that the cause of the problem can be identified\r\n\r\nDo you want to continue?", "Error report - Occelmnt file", (MessageBoxButtons)1, (MessageBoxIcon)64) == 2)
			{
				return;
			}
			if (FileNameIn == "")
			{
				OpenFileDialog val = new OpenFileDialog();
				((FileDialog)val).set_Title("Specify OCCELMNT file to locate format error.");
				((FileDialog)val).set_Filter("OCCELMNT file (OCCELMNT*.*)|occelmnt*.*|Planet Files (*.PLA)|*.pla|Steve Preston's  Future.dat |future.dat|All files (*.*)|*.*");
				((FileDialog)val).set_FilterIndex(Settings.Default.OccelmntLastIndex);
				((FileDialog)val).set_FileName(Settings.Default.OccelmntLastFileName);
				try
				{
					((FileDialog)val).set_InitialDirectory(Path.GetDirectoryName(Settings.Default.OccelmntLastFileName));
				}
				catch
				{
				}
				if ((int)((CommonDialog)val).ShowDialog() == 1)
				{
					FileNameIn = ((FileDialog)val).get_FileName();
				}
			}
			int num2 = 0;
			using (StreamReader streamReader = new StreamReader(FileNameIn))
			{
				using StreamWriter streamWriter = new StreamWriter(FileNameIn + ".Errors");
				streamWriter.WriteLine("Error report for file " + FileNameIn);
				streamWriter.WriteLine("UT Date of report: " + DateTime.Now.ToUniversalTime().ToLongDateString() + " at " + DateTime.Now.ToShortTimeString());
				streamWriter.WriteLine("");
				do
				{
					try
					{
						for (int i = 0; i < 10; i++)
						{
							array[i] = streamReader.ReadLine();
						}
					}
					catch
					{
						streamWriter.WriteLine("Record #" + num2 + " is incomplete");
						for (int j = 0; j < 10; j++)
						{
							streamWriter.WriteLine(array[j]);
						}
						streamWriter.WriteLine("");
					}
					for (int k = 0; k < 10; k++)
					{
						array2[k] = array[k].Length == array3[k];
					}
					if (!(array2[0] & array2[1] & array2[2] & array2[3] & array2[4] & array2[5] & array2[6] & array2[7] & array2[8] & array2[9]))
					{
						streamWriter.WriteLine("Record #" + num2 + " has one or more lines of invalid length, as indicated");
						streamWriter.WriteLine("Line");
						for (int l = 0; l < 10; l++)
						{
							if (!array2[l])
							{
								streamWriter.WriteLine("   ***  The next line has " + array[l].ToString().Length + " characters, instead of " + array3[l] + " ***");
							}
							streamWriter.WriteLine((l + 1).ToString().PadLeft(2) + ":" + array[l]);
						}
						streamWriter.WriteLine("");
						num++;
					}
					num2++;
					if (num > 50)
					{
						flag = true;
						break;
					}
				}
				while (!streamReader.EndOfStream);
			}
			if (flag)
			{
				MessageBox.Show("Error check terminated. More than 50 record(s) have errors", "Error check", (MessageBoxButtons)0, (MessageBoxIcon)64);
			}
			else if (num > 0)
			{
				MessageBox.Show("Error check completed.   " + num + " record(s) have errors", "Error check", (MessageBoxButtons)0, (MessageBoxIcon)64);
			}
			else
			{
				MessageBox.Show("Error check completed, with no errors found", "Error check", (MessageBoxButtons)0, (MessageBoxIcon)64);
			}
		}

		public static void ShowSiteEditor()
		{
			//IL_0005: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Unknown result type (might be due to invalid IL or missing references)
			try
			{
				((Form)Site_Editor).ShowDialog();
			}
			catch
			{
				Site_Editor = new SiteEditor();
				((Form)Site_Editor).ShowDialog();
			}
			((Control)Site_Editor).Focus();
		}

		public static void ShowBinaryAsteroidEditor()
		{
			//IL_0005: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Unknown result type (might be due to invalid IL or missing references)
			try
			{
				((Form)BinaryAsteroids).ShowDialog();
			}
			catch
			{
				BinaryAsteroids = new BinaryAsteroids_Edit();
				((Form)BinaryAsteroids).ShowDialog();
			}
			((Control)BinaryAsteroids).Focus();
		}

		public static void ShowAsteroidRingsEditor()
		{
			//IL_000f: Unknown result type (might be due to invalid IL or missing references)
			AsteroidRings = new AsteroidRings();
			((Form)AsteroidRings).ShowDialog();
			((Control)AsteroidRings).Focus();
		}

		public static void Show_DisplayStarsFromCatalogue()
		{
			//IL_0005: Unknown result type (might be due to invalid IL or missing references)
			try
			{
				((Form)Display_StarRange).ShowDialog();
			}
			catch
			{
				Display_StarRange = new Star_DisplayRange();
				((Control)Display_StarRange).Show();
			}
			((Control)Display_StarRange).Focus();
		}

		internal static void ListRelativisticOffset()
		{
			Show_Relativity();
			((Control)Relativity.txtRH1).set_Text(RA.Substring(0, 2));
			((Control)Relativity.txtRM1).set_Text(RA.Substring(3, 2));
			((Control)Relativity.txtRS1).set_Text(RA.Substring(6));
			((Control)Relativity.txtDD1).set_Text(Dec.Substring(0, 3));
			((Control)Relativity.txtDM1).set_Text(Dec.Substring(4, 2));
			((Control)Relativity.txtDS1).set_Text(Dec.Substring(7));
			Relativity.updnYear.set_Value((decimal)EventYear);
			Relativity.updnMonth.set_Value((decimal)EventMonth);
			Relativity.updnDay.set_Value((decimal)(EventDay + MidTime / 24.0));
			((Control)Relativity.txtDistance).set_Text($"{DistanceToAsteroid:f5}");
			Relativity.Compute();
		}

		internal static void GetBrightness(double StarRadius_mas, double AsteroidRadius_mas, double Star_Mag, double Asteroid_Mag, ref float[] OccBrightness)
		{
			double num = 1.0;
			double num2 = Math.PI * AsteroidRadius_mas * AsteroidRadius_mas;
			double num3 = Math.PI * StarRadius_mas * StarRadius_mas;
			double num4 = Math.Pow(10.0, (0.0 - (Star_Mag - Asteroid_Mag)) / 2.5) * num2 / num3;
			double num5 = num2 + num3 * num4;
			double num6 = 1.2 * (StarRadius_mas + AsteroidRadius_mas) / 100.0;
			OccBrightness = new float[201];
			for (int i = 0; i <= 100; i++)
			{
				double num7 = EclipseArea(StarRadius_mas, AsteroidRadius_mas, Math.Abs((double)i * num6));
				num = ((!((double)i * num6 > AsteroidRadius_mas + StarRadius_mas)) ? ((!((double)i * num6 <= AsteroidRadius_mas - StarRadius_mas)) ? ((!((double)i * num6 <= StarRadius_mas - AsteroidRadius_mas)) ? ((num2 + (Math.PI * StarRadius_mas * StarRadius_mas - num7) * num4) / num5) : ((num2 + (num3 - num2) * num4) / num5)) : (num2 / num5)) : ((num2 + num3 * num4) / num5));
				OccBrightness[100 + i] = (OccBrightness[100 - i] = (float)num);
			}
			using StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "/Generated Files/FluxTest.dat");
			for (int j = -100; j <= 100; j++)
			{
				streamWriter.WriteLine(string.Format("{0,1:f3}", OccBrightness[100 + j]));
			}
		}

		private static double EclipseArea(double StarRadius_mas, double AsteroidRadius_mas, double Delta_mas)
		{
			if (AsteroidRadius_mas + Delta_mas <= StarRadius_mas)
			{
				return AsteroidRadius_mas * AsteroidRadius_mas * Math.PI;
			}
			if (AsteroidRadius_mas >= Delta_mas + StarRadius_mas)
			{
				return StarRadius_mas * StarRadius_mas * Math.PI;
			}
			if (AsteroidRadius_mas + StarRadius_mas <= Delta_mas)
			{
				return 0.0;
			}
			double num = Math.Acos((AsteroidRadius_mas * AsteroidRadius_mas + Delta_mas * Delta_mas - StarRadius_mas * StarRadius_mas) / 2.0 / Math.Abs(AsteroidRadius_mas) / Delta_mas);
			if (double.IsNaN(num))
			{
				return 0.0;
			}
			double num2 = Math.Acos((StarRadius_mas * StarRadius_mas + Delta_mas * Delta_mas - AsteroidRadius_mas * AsteroidRadius_mas) / 2.0 / StarRadius_mas / Delta_mas);
			if (double.IsNaN(num2))
			{
				return 0.0;
			}
			return AsteroidRadius_mas * AsteroidRadius_mas * (num - Math.Sin(2.0 * num) / 2.0) + StarRadius_mas * StarRadius_mas * (num2 - Math.Sin(2.0 * num2) / 2.0);
		}
	}
}
