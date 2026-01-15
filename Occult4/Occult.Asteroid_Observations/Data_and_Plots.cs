using System;
using System.Collections;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Printing;
using System.IO;
using System.Reflection;
using System.Text;
using System.Windows.Forms;
using Occult.Asteroids;
using Occult.File_Actions;
using Occult.MPC_PDS;
using Occult.Properties;

namespace Occult.Asteroid_Observations
{
	public class Data_and_Plots
	{
		public static ObservationsEditor Observations_Editor;

		internal static PlotObservations PlotForm;

		internal static TimeBaseOffsets TimeBase_Offset;

		internal static PredictionOffsets Prediction_Offset;

		internal static MissTimes Miss_Times;

		internal static ReductionPlots ReductionPlot;

		internal static DisplayChordLengths ChordLengths;

		internal static DisplayObserverVelocities ObserverVelocities;

		public static EmailObservations EmailObservations;

		internal static DiameterFromChords DiameterFromChords;

		internal static PathRelativeDistances PathDistances;

		internal static SetDoubleStarOffsets SetOffsets;

		internal static ExportComments Export_Comments;

		internal static MergeMainWithUpdatedObsFile MergeFiles;

		internal static Event_coordinates Event_Coords;

		internal static MPCDetails mPCDetails;

		public static bool IsUsingNewFormat = false;

		internal static AllEvents Historical_AllEvents = new AllEvents();

		internal static AllEvents SingleEvent = new AllEvents();

		internal static AllEvents EurasterEvent = new AllEvents();

		internal static AllEvents JapanEvent = new AllEvents();

		internal static List<Asteroid_OBS_Tags> Tags = new List<Asteroid_OBS_Tags>();

		internal static int Year;

		internal static int Month;

		internal static int Day;

		internal static double RAStar_Apparent = 0.0;

		internal static double DecStar_Apparent = 0.0;

		internal static double StarRA = 0.0;

		internal static double StarDec = 0.0;

		internal static double TZero_forMotions;

		internal static double Parallax;

		internal static double dParallax = 0.0;

		internal static double d2Parallax = 0.0;

		internal static double d3Parallax = 0.0;

		internal static double Diameter = 1.0;

		internal static double DiameterUncertainty = 0.1;

		internal static float LineThicknessOther = 1f;

		internal static float LineThicknessPath = 1f;

		internal static float StarDia_mas = 0f;

		internal static double X_RefPoint_atMidTforMotions = 0.0;

		internal static double Y_RefPoint_atMidTforMotions = 0.0;

		internal static double Xoffset = 0.0;

		internal static double Yoffset = 0.0;

		internal static double XoffsetSat = 0.0;

		internal static double YoffsetSat = 0.0;

		internal static int EventLineToHighlight = -1;

		internal static float LengthOfDiameterByVolLine = 0f;

		internal static float LengthOfDiameterBySurfLine = 0f;

		private const int Numcolors = 100;

		private static Pen[] PenChords = new Pen[100];

		private const double Radian = 180.0 / Math.PI;

		private const double TimeForMotions = 10.0;

		private const double ErrorFlag = 10000.0;

		private const int MaxObservations = 900;

		internal static double[,] X = new double[900, 4];

		internal static double[,] Y = new double[900, 4];

		internal static double[,] T = new double[900, 4];

		internal static double[,] LightTime_Geo_Secs = new double[900, 2];

		private static float[,] ErrorX_motion = new float[900, 2];

		private static float[,] ErrorY_Motion = new float[900, 2];

		private static double[,] Variations = new double[1800, 8];

		private static double MinimumDistance_Min = 0.0;

		private static double MinimumDistance_Max = 0.0;

		internal static bool ChordsTooCloseForAutoSolve = true;

		internal static bool HighPrecisionSolution = false;

		internal static int DecimalPlacesInSolution = 1;

		internal static bool PrimaryStarEventFound = false;

		private static bool[,] CloudyObs = new bool[900, 2];

		private static bool[,] PredictedObs = new bool[900, 2];

		private static string[] LabelTag = new string[900];

		private static double JD;

		internal static double X_RefPoint;

		internal static double Y_RefPoint;

		internal static double dX;

		internal static double d2X;

		internal static double d3X;

		internal static double dY;

		internal static double d2Y;

		internal static double d3Y;

		internal static double d2T0;

		private static double xi;

		private static double eta;

		private static double XMin;

		private static double YMin;

		private static double XMax;

		private static double YMax;

		internal static double X_CenterOfPoints;

		internal static double Y_CenterOfPoints;

		internal static double XRange;

		internal static double YRange;

		internal static double Range;

		private static double PlotGeocentricX;

		private static double PlotGeocentricY;

		private static double PlotGeocentricT;

		internal static string PlotLabel;

		private static double CosPA;

		private static double SinPA;

		private static double X1;

		private static double Y1;

		private static float X2;

		private static float Y2;

		private static float xAxis0;

		private static float yAxis0;

		private static float xAxis90;

		private static float yAxis90;

		private static float xAxis180;

		private static float yAxis180;

		private static float xAxis270;

		private static float yAxis270;

		private static float xOld = 0f;

		private static float yOld = 0f;

		private static float xOffset;

		private static float yOffset;

		private static float CurrentX;

		private static float CurrentY;

		internal static float CenterOfEllipse_X;

		internal static float CenterOfEllipse_Y;

		internal static float CenterOf2ndEllipse_X = 0f;

		internal static float CenterOf2ndEllipse_Y = 0f;

		internal static float Offset_X_2ndFromPrimary = 0f;

		internal static float Offset_Y_2ndFromPrimary = 0f;

		private static float EventPredictionOffsetX = 0f;

		private static float EventPredictionOffsetY = 0f;

		private static double GridAngle = 0.0;

		private static bool ShowMarkers;

		private static bool ShowIDs;

		private static bool ShowVideoOnly;

		private static bool ShowErrors;

		private static bool ShowErrorsForChordLength;

		private static bool ShowOcculted;

		private static bool ShowMiss;

		private static bool ShowMissAsDashed;

		private static bool ShowZeroWeightedObservations;

		private static bool ShowPredicted;

		private static bool ShowCloud;

		private static bool ShowEllipse;

		private static bool ShowFresnel;

		private static bool ShowAstrometry;

		private static bool ShowAxes;

		private static bool ShowSolution;

		private static bool ShowTitle;

		private static bool ShowTitleExtra;

		private static bool IncludeTimeShift;

		private static bool ShowScaleGrid;

		private static bool ShowScaleKM;

		private static bool ShowScale_mas;

		private static bool ShowMeanDiameter;

		private static bool ShowRings;

		private static bool ShowStar;

		private static bool ShowSatelliteOrbits;

		internal static bool WhiteBackground;

		internal static bool GrayBackground;

		internal static bool BlackBackground;

		internal static bool ShowPathsIncolour;

		internal static bool ShowEllipseInColour;

		internal static bool ShowEventsInColour;

		internal static bool ShowVisible;

		internal static bool ShowAlign;

		internal static bool ShowWatermark;

		internal static bool ShowProvisionalMark;

		internal static bool CenterStarPlotOnPlot = false;

		private static bool Secondary;

		internal static List<int> ObserverListIndex = new List<int>();

		internal static List<int> KnownBinaryAsteroidElements_RecNums = new List<int>();

		internal static int SelectedSatellite = 0;

		internal static bool PDSPlots = false;

		internal static bool FirstTimePlot = false;

		public static bool ShapeModelsDisabled = false;

		internal static double[] DoublesOffset_X = new double[6];

		internal static double[] DoublesOffset_Y = new double[6];

		internal static List<SubmissionID> MPCdetailsList;

		internal static string MPCDate;

		internal static string MPCNumber;

		internal static string MPC_SubmissionID = "";

		internal static bool CancelDetails = true;

		public static void ShowChordLengths()
		{
			try
			{
				((Control)ChordLengths).Show();
			}
			catch
			{
				ChordLengths = new DisplayChordLengths();
				((Control)ChordLengths).Show();
				((Form)ChordLengths).set_Location(Settings.Default.LocationChordLengths);
			}
		}

		public static void ShowDiameterFromChords()
		{
			try
			{
				((Control)DiameterFromChords).Show();
			}
			catch
			{
				DiameterFromChords = new DiameterFromChords();
				((Control)DiameterFromChords).Show();
			}
		}

		public static void ShowPathDistances()
		{
			try
			{
				((Control)PathDistances).Show();
			}
			catch
			{
				PathDistances = new PathRelativeDistances();
				((Control)PathDistances).Show();
				((Form)PathDistances).set_Location(Settings.Default.LocationRelativePathDistances);
			}
		}

		public static void ShowObserverVelocities()
		{
			try
			{
				((Control)ObserverVelocities).Show();
			}
			catch
			{
				ObserverVelocities = new DisplayObserverVelocities();
				((Control)ObserverVelocities).Show();
				((Form)ObserverVelocities).set_Location(Settings.Default.LocationVelocities);
			}
		}

		public static void ShowEditor()
		{
			ShowPlotForm();
			try
			{
				((Control)Observations_Editor).Show();
			}
			catch
			{
				Observations_Editor = new ObservationsEditor();
				((Control)Observations_Editor).Show();
			}
			((Control)Observations_Editor).Focus();
			PDSPlots = false;
		}

		public static void ShowExportComments(bool WhenExporting)
		{
			try
			{
				((Control)Export_Comments).Show();
			}
			catch
			{
				Export_Comments = new ExportComments(WhenExporting);
				((Control)Export_Comments).Show();
			}
		}

		public static void ShowPlotForm()
		{
			try
			{
				((Control)PlotForm).Show();
			}
			catch
			{
				PlotForm = new PlotObservations();
				((Control)PlotForm).Show();
			}
		}

		public static void CreatePlotForm()
		{
			if (PlotForm == null)
			{
				PlotForm = new PlotObservations();
			}
		}

		public static void ShowTimeBaseForm()
		{
			try
			{
				((Control)TimeBase_Offset).Show();
			}
			catch
			{
				TimeBase_Offset = new TimeBaseOffsets();
				((Control)TimeBase_Offset).Show();
				((Form)TimeBase_Offset).set_Location(Settings.Default.LocationTimeBaseOffset);
			}
		}

		public static void ShowPredictOffsetForm()
		{
			try
			{
				((Control)Prediction_Offset).Show();
			}
			catch
			{
				Prediction_Offset = new PredictionOffsets();
				((Control)Prediction_Offset).Show();
				((Form)Prediction_Offset).set_Location(Settings.Default.LocationPredictionOffset);
			}
		}

		public static void ShowMissTimesForm()
		{
			try
			{
				((Control)Miss_Times).Show();
			}
			catch
			{
				Miss_Times = new MissTimes();
				((Control)Miss_Times).Show();
			}
		}

		public static void ShowReductionPlotForm(int Kind)
		{
			ReductionPlot = new ReductionPlots(Kind);
			((Control)ReductionPlot).Show();
			PDSPlots = true;
		}

		public static void ShowEmailObservations()
		{
			try
			{
				((Control)EmailObservations).Show();
			}
			catch
			{
				((Control)new EmailObservations()).Show();
			}
		}

		public static void ShowMergeObservations()
		{
			try
			{
				((Control)MergeFiles).Show();
			}
			catch
			{
				((Control)new MergeMainWithUpdatedObsFile()).Show();
			}
		}

		public static void ShowEventCoords()
		{
			try
			{
				((Control)Event_Coords).Show();
			}
			catch
			{
				Event_Coords = new Event_coordinates();
				((Control)Event_Coords).Show();
			}
		}

		internal static void GetPlotParametersFromForm()
		{
			LineThicknessOther = 1f;
			LineThicknessPath = 0.5f;
			GridAngle = ((ListControl)PlotForm.dropAngles).get_SelectedIndex();
			if (PlotForm.mnuThickLines_Paths.get_Checked())
			{
				LineThicknessPath = (float)Settings.Default.AsterPlot_LineThickness_Path;
			}
			if (PlotForm.mnuThickLines_EllipseEvents.get_Checked())
			{
				LineThicknessOther = (float)Settings.Default.AsterPlot_LineThickness_Path;
			}
			if (!PDSPlots)
			{
				WhiteBackground = (GrayBackground = false);
				if (PlotForm.mnuGrayBackground.get_Checked())
				{
					GrayBackground = true;
				}
				else
				{
					WhiteBackground = !PlotForm.mnuBlackBackground.get_Checked();
				}
				ShowMarkers = PlotForm.mnuShowMarkers.get_Checked();
				ShowIDs = PlotForm.mnuShowID.get_Checked();
				ShowVideoOnly = PlotForm.mnuShowVideo.get_Checked();
				ShowErrors = PlotForm.mnuShowErrors.get_Checked();
				ShowErrorsForChordLength = PlotForm.ErrorBarsForFittingToShapeModels_menu.get_Checked();
				ShowOcculted = PlotForm.mnuShowPathsOcculted.get_Checked();
				ShowVisible = PlotForm.mnuShowPathsVisible.get_Checked();
				ShowMiss = PlotForm.mnuShowPathsMiss.get_Checked();
				ShowMissAsDashed = PlotForm.showMISSEventsAsDashedLinesToolStripMenuItem.get_Checked();
				ShowZeroWeightedObservations = PlotForm.mnuShowZeroWeighted.get_Checked();
				ShowPredicted = PlotForm.mnuShowPredicted.get_Checked();
				ShowCloud = PlotForm.mnuShowCloud.get_Checked();
				ShowEllipse = PlotForm.mnuShowEllipse.get_Checked();
				ShowFresnel = PlotForm.showFresnelDiffractionPeakToolStripMenuItem.get_Checked();
				ShowAxes = PlotForm.mnuShowAxes.get_Checked();
				ShowAstrometry = PlotForm.showASTROMETRYLocation.get_Checked();
				ShowSatelliteOrbits = PlotForm.showSatelliteOrbitsifAnyToolStripMenuItem.get_Checked();
				ShowAlign = PlotForm.mnuAlign.get_Checked();
				ShowSolution = PlotForm.mnuShowSolution.get_Checked();
				ShowTitle = PlotForm.mnuShowTitle.get_Checked();
				ShowTitleExtra = PlotForm.addThisTextToTheTitleToolStripMenuItem.get_Checked();
				ShowRings = PlotForm.showRINGEventPathsToolStripMenuItem.get_Checked();
				IncludeTimeShift = PlotForm.mnuIncludeTimeShift.get_Checked();
				ShowScaleGrid = PlotForm.mnuShowScaleGrid.get_Checked();
				ShowScaleKM = PlotForm.mnuShowPlotScalekm.get_Checked();
				ShowScale_mas = PlotForm.showPLOTSCALEmasToolStripMenuItem.get_Checked();
				ShowPathsIncolour = PlotForm.mnuPathsInColour.get_Checked();
				ShowEllipseInColour = PlotForm.showAsteroidEllipseInColorToolStripMenuItem.get_Checked();
				ShowEventsInColour = PlotForm.showEventInColorToolStripMenuItem.get_Checked();
				ShowMeanDiameter = PlotForm.mnuShowMeanDiameterDAMIT.get_Checked();
				ShowWatermark = PlotForm.applyWatermark.get_Checked();
				ShowProvisionalMark = PlotForm.applyProvisionalMarkToolStripMenuItem.get_Checked();
				ShowStar = PlotForm.showStarWithADiameterOfToolStripMenuItem.get_Checked();
				float.TryParse(Settings.Default.Asterplot_StarDia, out StarDia_mas);
			}
			else
			{
				WhiteBackground = !PlotForm.mnuBlackBackground.get_Checked();
				GrayBackground = PlotForm.mnuGrayBackground.get_Checked();
				ShowMarkers = true;
				ShowIDs = false;
				ShowVideoOnly = false;
				ShowErrors = false;
				ShowOcculted = EventDetails.Quality < 3;
				ShowMiss = true;
				ShowMissAsDashed = true;
				ShowZeroWeightedObservations = false;
				ShowPredicted = false;
				ShowCloud = false;
				ShowEllipse = EventDetails.Quality < 4;
				ShowFresnel = false;
				ShowAstrometry = true;
				ShowAxes = false;
				ShowAstrometry = false;
				ShowAlign = true;
				ShowSolution = true;
				ShowTitle = false;
				ShowTitleExtra = false;
				ShowRings = true;
				IncludeTimeShift = true;
				ShowScaleGrid = false;
				ShowScaleKM = false;
				ShowScale_mas = false;
				ShowMeanDiameter = false;
				ShowWatermark = false;
				ShowProvisionalMark = false;
				ShowStar = false;
			}
		}

		private static void SetMulticolorPens()
		{
			PenChords[0] = new Pen(Color.Indigo, LineThicknessPath);
			PenChords[1] = new Pen(Color.Red, LineThicknessPath);
			PenChords[2] = new Pen(Color.Blue, LineThicknessPath);
			PenChords[3] = new Pen(Color.BlueViolet, LineThicknessPath);
			PenChords[4] = new Pen(Color.Brown, LineThicknessPath);
			PenChords[5] = new Pen(Color.CadetBlue, LineThicknessPath);
			PenChords[6] = new Pen(Color.Crimson, LineThicknessPath);
			PenChords[7] = new Pen(Color.Chartreuse, LineThicknessPath);
			PenChords[8] = new Pen(Color.Coral, LineThicknessPath);
			PenChords[9] = new Pen(Color.CornflowerBlue, LineThicknessPath);
			PenChords[10] = new Pen(Color.Fuchsia, LineThicknessPath);
			PenChords[11] = new Pen(Color.Goldenrod, LineThicknessPath);
			PenChords[12] = new Pen(Color.Green, LineThicknessPath);
			PenChords[13] = new Pen(Color.Gold, LineThicknessPath);
			PenChords[14] = new Pen(Color.HotPink, LineThicknessPath);
			PenChords[15] = new Pen(Color.IndianRed, LineThicknessPath);
			PenChords[16] = new Pen(Color.Khaki, LineThicknessPath);
			PenChords[17] = new Pen(Color.LightCoral, LineThicknessPath);
			PenChords[18] = new Pen(Color.RoyalBlue, LineThicknessPath);
			PenChords[19] = new Pen(Color.Purple, LineThicknessPath);
			PenChords[20] = new Pen(Color.MediumVioletRed, LineThicknessPath);
			PenChords[21] = new Pen(Color.MidnightBlue, LineThicknessPath);
			PenChords[22] = new Pen(Color.Orange, LineThicknessPath);
			PenChords[23] = new Pen(Color.OrangeRed, LineThicknessPath);
			PenChords[24] = new Pen(Color.DarkBlue, LineThicknessPath);
			PenChords[25] = new Pen(Color.SlateBlue, LineThicknessPath);
			PenChords[26] = new Pen(Color.Teal, LineThicknessPath);
			PenChords[27] = new Pen(Color.Yellow, LineThicknessPath);
			PenChords[28] = new Pen(Color.YellowGreen, LineThicknessPath);
			PenChords[29] = new Pen(Color.Olive, LineThicknessPath);
			PenChords[30] = new Pen(Color.Indigo, LineThicknessPath);
			PenChords[31] = new Pen(Color.Red, LineThicknessPath);
			PenChords[32] = new Pen(Color.Aquamarine, LineThicknessPath);
			PenChords[33] = new Pen(Color.BlueViolet, LineThicknessPath);
			PenChords[34] = new Pen(Color.Brown, LineThicknessPath);
			PenChords[35] = new Pen(Color.CadetBlue, LineThicknessPath);
			PenChords[36] = new Pen(Color.Crimson, LineThicknessPath);
			PenChords[37] = new Pen(Color.Chartreuse, LineThicknessPath);
			PenChords[38] = new Pen(Color.Coral, LineThicknessPath);
			PenChords[39] = new Pen(Color.CornflowerBlue, LineThicknessPath);
			PenChords[40] = new Pen(Color.Fuchsia, LineThicknessPath);
			PenChords[41] = new Pen(Color.Goldenrod, LineThicknessPath);
			PenChords[42] = new Pen(Color.Green, LineThicknessPath);
			PenChords[43] = new Pen(Color.Gold, LineThicknessPath);
			PenChords[44] = new Pen(Color.HotPink, LineThicknessPath);
			PenChords[45] = new Pen(Color.IndianRed, LineThicknessPath);
			PenChords[46] = new Pen(Color.Khaki, LineThicknessPath);
			PenChords[47] = new Pen(Color.LightCoral, LineThicknessPath);
			PenChords[48] = new Pen(Color.RoyalBlue, LineThicknessPath);
			PenChords[49] = new Pen(Color.Purple, LineThicknessPath);
			PenChords[50] = new Pen(Color.MediumVioletRed, LineThicknessPath);
			PenChords[51] = new Pen(Color.MidnightBlue, LineThicknessPath);
			PenChords[52] = new Pen(Color.Orange, LineThicknessPath);
			PenChords[53] = new Pen(Color.OrangeRed, LineThicknessPath);
			PenChords[54] = new Pen(Color.DarkBlue, LineThicknessPath);
			PenChords[55] = new Pen(Color.SlateBlue, LineThicknessPath);
			PenChords[56] = new Pen(Color.Teal, LineThicknessPath);
			PenChords[57] = new Pen(Color.Yellow, LineThicknessPath);
			PenChords[58] = new Pen(Color.YellowGreen, LineThicknessPath);
			PenChords[59] = new Pen(Color.Olive, LineThicknessPath);
			PenChords[60] = new Pen(Color.Indigo, LineThicknessPath);
			PenChords[61] = new Pen(Color.Red, LineThicknessPath);
			PenChords[62] = new Pen(Color.Aquamarine, LineThicknessPath);
			PenChords[63] = new Pen(Color.BlueViolet, LineThicknessPath);
			PenChords[64] = new Pen(Color.Brown, LineThicknessPath);
			PenChords[65] = new Pen(Color.CadetBlue, LineThicknessPath);
			PenChords[66] = new Pen(Color.Crimson, LineThicknessPath);
			PenChords[67] = new Pen(Color.Chartreuse, LineThicknessPath);
			PenChords[68] = new Pen(Color.Coral, LineThicknessPath);
			PenChords[69] = new Pen(Color.CornflowerBlue, LineThicknessPath);
			PenChords[70] = new Pen(Color.Fuchsia, LineThicknessPath);
			PenChords[71] = new Pen(Color.Goldenrod, LineThicknessPath);
			PenChords[72] = new Pen(Color.Green, LineThicknessPath);
			PenChords[73] = new Pen(Color.Gold, LineThicknessPath);
			PenChords[74] = new Pen(Color.HotPink, LineThicknessPath);
			PenChords[75] = new Pen(Color.IndianRed, LineThicknessPath);
			PenChords[76] = new Pen(Color.Khaki, LineThicknessPath);
			PenChords[77] = new Pen(Color.LightCoral, LineThicknessPath);
			PenChords[78] = new Pen(Color.RoyalBlue, LineThicknessPath);
			PenChords[79] = new Pen(Color.Purple, LineThicknessPath);
			PenChords[80] = new Pen(Color.MediumVioletRed, LineThicknessPath);
			PenChords[81] = new Pen(Color.MidnightBlue, LineThicknessPath);
			PenChords[82] = new Pen(Color.Orange, LineThicknessPath);
			PenChords[83] = new Pen(Color.OrangeRed, LineThicknessPath);
			PenChords[84] = new Pen(Color.DarkBlue, LineThicknessPath);
			PenChords[85] = new Pen(Color.SlateBlue, LineThicknessPath);
			PenChords[86] = new Pen(Color.Teal, LineThicknessPath);
			PenChords[87] = new Pen(Color.Yellow, LineThicknessPath);
			PenChords[88] = new Pen(Color.YellowGreen, LineThicknessPath);
			PenChords[89] = new Pen(Color.Olive, LineThicknessPath);
			PenChords[90] = new Pen(Color.Indigo, LineThicknessPath);
			PenChords[91] = new Pen(Color.Red, LineThicknessPath);
			PenChords[92] = new Pen(Color.Aquamarine, LineThicknessPath);
			PenChords[93] = new Pen(Color.BlueViolet, LineThicknessPath);
			PenChords[94] = new Pen(Color.Brown, LineThicknessPath);
			PenChords[95] = new Pen(Color.CadetBlue, LineThicknessPath);
			PenChords[96] = new Pen(Color.Crimson, LineThicknessPath);
			PenChords[97] = new Pen(Color.Chartreuse, LineThicknessPath);
			PenChords[98] = new Pen(Color.Coral, LineThicknessPath);
			PenChords[99] = new Pen(Color.CornflowerBlue, LineThicknessPath);
		}

		internal static void Observations_InitialisePlot_Using_EventDetails()
		{
			if (EventDetails.Observers.Count < 1)
			{
				return;
			}
			double num = 0.0;
			double ObserverLongitude = 0.0;
			int num2 = 10;
			bool[] array = new bool[2];
			string text = "    ";
			ArrayList arrayList = new ArrayList();
			Font font = new Font("Courier New", 10f);
			Pen pen = (WhiteBackground ? new Pen(Color.Black, LineThicknessOther) : ((!GrayBackground) ? new Pen(Color.HotPink, LineThicknessOther) : new Pen(Color.FromArgb(255, 64, 64, 64), LineThicknessOther)));
			PlotForm.Updating = true;
			Utilities.EarthOrientationParameters(JD, out var x, out var y, out var dUT);
			RAStar_Apparent = EventDetails.RA_Star_Apparent;
			DecStar_Apparent = EventDetails.Dec_Star_Apparent;
			JD = Utilities.JD_from_Date(EventDetails.Year, EventDetails.Month, EventDetails.Day);
			double num3 = Utilities.SiderealTime_deg(JD + dUT / 86400.0, Apparent: true) / (180.0 / Math.PI);
			try
			{
				((ToolStripItem)PlotForm.toolStripStarDia_mas).set_Text(string.Format("{0,3:f1}", EventDetails.StarDia_mas));
			}
			catch
			{
			}
			PlotLabel = "";
			if (!EventDetails.AsteroidNumber.Contains("P"))
			{
				PlotLabel = "(" + EventDetails.AsteroidNumber.Trim() + ") " + EventDetails.AsteroidID.Trim();
			}
			else
			{
				PlotLabel = EventDetails.AsteroidID.Trim();
			}
			string[] obj2 = new string[8]
			{
				PlotLabel,
				"  ",
				EventDetails.Year.ToString(),
				" ",
				Utilities.ShortMonths[EventDetails.Month],
				" ",
				null,
				null
			};
			int TotalEvents = EventDetails.Day;
			obj2[6] = TotalEvents.ToString();
			obj2[7] = "  ";
			PlotLabel = string.Concat(obj2);
			dX = EventDetails.dX;
			d2X = EventDetails.d2X;
			d3X = EventDetails.d3X;
			dY = EventDetails.dY;
			d2Y = EventDetails.d2Y;
			d3Y = EventDetails.d3Y;
			TZero_forMotions = EventDetails.MidT_forMotions;
			PlotForm.lstObservers.get_Items().Clear();
			Panel panelDouble = PlotForm.panelDouble;
			bool visible;
			((Control)PlotForm.PanelSatellites).set_Visible(visible = false);
			((Control)panelDouble).set_Visible(visible);
			PlotForm.updnBrightRatio.set_Value((decimal)EventDetails.BrightnessRatio_SolnStar_to_Companion);
			PlotForm.updnBrightnessUncertPerCent.set_Value((decimal)EventDetails.BrightnessRatio_UncertaintyPercent);
			((Control)PlotForm.txtMainDrop).set_Text("100");
			((Control)PlotForm.txt2ndDrop).set_Text(string.Format("{0,1:f0}", 100.0 / EventDetails.BrightnessRatio_SolnStar_to_Companion));
			PlotForm.SetDoubleMagnitudes();
			PlotForm.chkEditJDSO.set_Checked(false);
			((Control)PlotForm.txtDoublePairID).set_Text(EventDetails.KnownPair_ID);
			TextBox txtJDSOSubmitDate = PlotForm.txtJDSOSubmitDate;
			((TextBoxBase)PlotForm.txtJDSO_Vol_Num_Pg).set_ReadOnly(visible = true);
			((TextBoxBase)txtJDSOSubmitDate).set_ReadOnly(visible);
			((Control)PlotForm.txtJDSOSubmitDate).set_Text(EventDetails.JDSO_SubmitDate);
			((Control)PlotForm.txtJDSO_Vol_Num_Pg).set_Text(EventDetails.JDSO_Vol_Num_Pg);
			for (int i = 0; i < 4; i++)
			{
				NumericUpDown obj3 = PlotForm.updn_DoublePA[i];
				((Control)PlotForm.updn_DoubleSep[i]).set_Visible(visible = i == PlotForm.CurrentlySelectedComponent);
				((Control)obj3).set_Visible(visible);
				PlotForm.updn_DoubleSep[i].set_DecimalPlaces(DecimalPlacesInSolution);
				PlotForm.updn_DoubleSep[i].set_Increment((decimal)Math.Pow(10.0, -DecimalPlacesInSolution));
			}
			int num4 = 0;
			if (IncludeTimeShift)
			{
				num4 = 1;
			}
			double num5 = Math.Atan2(EventDetails.dX, EventDetails.dY) * (180.0 / Math.PI);
			double num6 = 0.0;
			EventDetails.RefHour_forAnalysis = 0.0;
			EventDetails.Number_Chords = 0;
			EventDetails.Number_SatelliteChords = 0;
			double pCos;
			double pSin;
			for (int j = 0; j < EventDetails.Observers.Count; j++)
			{
				double num7 = EventDetails.Observers[j].Longitude / (180.0 / Math.PI);
				double latitude = EventDetails.Observers[j].Latitude / (180.0 / Math.PI);
				double altitude_metres = EventDetails.Observers[j].Altitude;
				Utilities.GetGeodetic(latitude, altitude_metres, Altitude_is_MSL: true, out pCos, out pSin, 0, ref ObserverLongitude);
				if ((EventDetails.Observers[j].T_Disappear == 0.0) | (EventDetails.Observers[j].PlotCode == "z"))
				{
					double[,] lightTime_Geo_Secs = LightTime_Geo_Secs;
					int num8 = j;
					ObserverData observerData = EventDetails.Observers[j];
					double num10 = (EventDetails.Observers[j].ObserverMotion_AcrossPath_D = 0.0);
					double num12 = (lightTime_Geo_Secs[num8, 0] = (observerData.ObserverMotion_AlongPath_D = num10));
				}
				else
				{
					double num13 = num3 + num7 - RAStar_Apparent + 1.002738 * (EventDetails.Observers[j].T_Disappear + (double)num4 * EventDetails.Observers[j].TimeAdjustment / 3600.0) * 15.0 / (180.0 / Math.PI);
					num = Math.Asin(pSin * Math.Sin(DecStar_Apparent) + pCos * Math.Cos(DecStar_Apparent) * Math.Cos(num13));
					LightTime_Geo_Secs[j, 0] = 0.02127517497454856 * Math.Sin(num);
					double num14 = 0.2625161 * pCos * Math.Cos(num13);
					double num15 = 0.2625161 * pCos * Math.Sin(num13) * Math.Sin(DecStar_Apparent);
					num6 = Math.Atan2(num14, num15) * (180.0 / Math.PI);
					Utilities.RotateXY(num14, num15, num5 - num6, out var x2, out var y2);
					EventDetails.Observers[j].ObserverMotion_AlongPath_D = x2;
					EventDetails.Observers[j].ObserverMotion_AcrossPath_D = y2;
				}
				if ((EventDetails.Observers[j].T_Reappear == 0.0) | (EventDetails.Observers[j].PlotCode == "y"))
				{
					double[,] lightTime_Geo_Secs2 = LightTime_Geo_Secs;
					int num16 = j;
					ObserverData observerData2 = EventDetails.Observers[j];
					double num10 = (EventDetails.Observers[j].ObserverMotion_AcrossPath_R = 0.0);
					double num12 = (lightTime_Geo_Secs2[num16, 1] = (observerData2.ObserverMotion_AlongPath_R = num10));
				}
				else
				{
					double num13 = num3 + num7 - RAStar_Apparent + 1.002738 * (EventDetails.Observers[j].T_Reappear + (double)num4 * EventDetails.Observers[j].TimeAdjustment / 3600.0) * 15.0 / (180.0 / Math.PI);
					num = Math.Asin(pSin * Math.Sin(DecStar_Apparent) + pCos * Math.Cos(DecStar_Apparent) * Math.Cos(num13));
					LightTime_Geo_Secs[j, 1] = 0.02127517497454856 * Math.Sin(num);
					double num19 = 0.2625161 * pCos * Math.Cos(num13);
					double num15 = 0.2625161 * pCos * Math.Sin(num13) * Math.Sin(DecStar_Apparent);
					num6 = Math.Atan2(num19, num15) * (180.0 / Math.PI);
					Utilities.RotateXY(num19, num15, num5 - num6, out var x3, out var y3);
					EventDetails.Observers[j].ObserverMotion_AlongPath_R = x3;
					EventDetails.Observers[j].ObserverMotion_AcrossPath_R = y3;
				}
			}
			double num20 = 0.0;
			double num21 = 0.0;
			int num22 = 0;
			int num23 = 0;
			for (int j = 0; j < EventDetails.Observers.Count; j++)
			{
				if (EventDetails.Observers[j].PlotCode != "x")
				{
					if ("DE".Contains(EventDetails.Observers[j].Event_D.ToUpper()) & (EventDetails.Observers[j].T_Disappear > 0.0) & ((EventDetails.Observers[j].Weight_D != 0) | !EventDetails.Observers[j].Weight_D_Set) & (EventDetails.Observers[j].PlotCode != "z"))
					{
						num20 += EventDetails.Observers[j].T_Disappear + ((double)num4 * EventDetails.Observers[j].TimeAdjustment + LightTime_Geo_Secs[j, 0]) / 3600.0;
						num22++;
					}
					if ("RF".Contains(EventDetails.Observers[j].Event_R.ToUpper()) & (EventDetails.Observers[j].T_Reappear > 0.0) & ((EventDetails.Observers[j].Weight_R != 0) | !EventDetails.Observers[j].Weight_R_Set) & (EventDetails.Observers[j].PlotCode != "y"))
					{
						num20 += EventDetails.Observers[j].T_Reappear + ((double)num4 * EventDetails.Observers[j].TimeAdjustment + LightTime_Geo_Secs[j, 1]) / 3600.0;
						num22++;
					}
					if ((EventDetails.Observers[j].Event_D.ToUpper() == "G") & (EventDetails.Observers[j].T_Disappear > 0.0) & ((EventDetails.Observers[j].Weight_D != 0) | !EventDetails.Observers[j].Weight_D_Set) & (EventDetails.Observers[j].PlotCode != "z"))
					{
						num21 += EventDetails.Observers[j].T_Disappear + ((double)num4 * EventDetails.Observers[j].TimeAdjustment + LightTime_Geo_Secs[j, 0]) / 3600.0;
						num23++;
					}
					if ((EventDetails.Observers[j].Event_R.ToUpper() == "B") & (EventDetails.Observers[j].T_Reappear > 0.0) & ((EventDetails.Observers[j].Weight_R != 0) | !EventDetails.Observers[j].Weight_R_Set) & (EventDetails.Observers[j].PlotCode != "y"))
					{
						num21 += EventDetails.Observers[j].T_Reappear + ((double)num4 * EventDetails.Observers[j].TimeAdjustment + LightTime_Geo_Secs[j, 1]) / 3600.0;
						num23++;
					}
				}
			}
			EventDetails.RefHour_forAnalysis_Uncert_secs = GetEventTimeUncertainty_secs(num23 > 0, out TotalEvents, out var AcrossPathUncertainty_mas, out var NoUncert, out var NTP_Phone_Offset);
			EventDetails.RefHour_forAnalysis = num20 / (double)num22 + NTP_Phone_Offset;
			EventDetails.RefHour_forAnalysis_AcrossPathUncertainty_mas = AcrossPathUncertainty_mas;
			if (num23 > 0)
			{
				EventDetails.RefHour_forAnalysis = num21 / (double)num23 + NTP_Phone_Offset;
			}
			EventDetails.Number_Chords = num22 / 2;
			EventDetails.Number_SatelliteChords = num23 / 2;
			bool flag = true;
			ObserverListIndex.Clear();
			for (int j = 0; j < EventDetails.Observers.Count; j++)
			{
				LabelTag[j] = EventDetails.Observers[j].SeqNumber.ToString();
				if (EventDetails.Observers[j].PlotCode != "x")
				{
					text = "    ";
					if (EventDetails.Observers[j].Event_D == "M")
					{
						text = "(M) ";
					}
					if (EventDetails.Observers[j].Event_D == "m")
					{
						text = "(m) ";
					}
					if (EventDetails.Observers[j].Event_D == "C")
					{
						text = "(C) ";
					}
					if (EventDetails.Observers[j].Event_D == "P")
					{
						text = "(P) ";
					}
					if (EventDetails.Observers[j].Event_D == "N")
					{
						text = "(R) ";
					}
					if (EventDetails.Observers[j].Event_D == "n")
					{
						text = "(Rm)";
					}
					string text2 = LabelTag[j].PadLeft(3) + text + EventDetails.Observers[j].ObserversAll;
					if (EventDetails.Observers[j].NearTo.Trim() != "")
					{
						text2 = text2 + ", near " + EventDetails.Observers[j].NearTo;
					}
					PlotForm.lstObservers.get_Items().Add((object)text2);
					ObserverListIndex.Add(j);
					if (text2.Length > num2)
					{
						num2 = text2.Length;
					}
					arrayList.Add(j % 100);
				}
				double num7 = EventDetails.Observers[j].Longitude / (180.0 / Math.PI);
				double latitude = EventDetails.Observers[j].Latitude / (180.0 / Math.PI);
				double altitude_metres = EventDetails.Observers[j].Altitude;
				Utilities.ConvertLatLong_forEOP(num7, latitude, x, y, out var EOPLong, out var EOPLat);
				num7 = EOPLong;
				latitude = EOPLat;
				ObserverLongitude = 0.0;
				int datumNumber = 0;
				int num24 = " NETG*".IndexOf(EventDetails.Observers[j].Datum);
				if (num24 < 0)
				{
					num24 = 0;
				}
				switch (num24)
				{
				case 1:
					datumNumber = 27;
					break;
				case 2:
					datumNumber = 16;
					break;
				case 3:
					datumNumber = 46;
					break;
				case 4:
					datumNumber = 34;
					break;
				}
				Utilities.GetGeodetic(latitude, altitude_metres, Altitude_is_MSL: true, out pCos, out pSin, datumNumber, ref ObserverLongitude);
				if ((EventDetails.Observers[j].T_Disappear == 0.0) | (EventDetails.Observers[j].PlotCode == "z"))
				{
					array[0] = false;
					T[j, 0] = 0.0;
					T[j, 2] = 0.0;
				}
				else
				{
					array[0] = true;
					T[j, 0] = EventDetails.Observers[j].T_Disappear + (double)num4 * EventDetails.Observers[j].TimeAdjustment / 3600.0;
					T[j, 2] = T[j, 0] - 1.0 / 6.0;
				}
				if ((EventDetails.Observers[j].T_Reappear == 0.0) | (EventDetails.Observers[j].PlotCode == "y"))
				{
					array[1] = false;
					T[j, 1] = 0.0;
					T[j, 3] = 0.0;
				}
				else
				{
					array[1] = true;
					T[j, 1] = EventDetails.Observers[j].T_Reappear + (double)num4 * EventDetails.Observers[j].TimeAdjustment / 3600.0;
					T[j, 3] = T[j, 1] + 1.0 / 6.0;
				}
				for (int k = 0; k <= 1; k++)
				{
					if (array[k])
					{
						double num13 = num3 + num7 - RAStar_Apparent + 1.002738 * T[j, k] * 15.0 / (180.0 / Math.PI);
						double num25 = 1.0;
						num = Math.Asin(pSin * Math.Sin(DecStar_Apparent) + pCos * Math.Cos(DecStar_Apparent) * Math.Cos(num13));
						if (num < 0.5)
						{
							double num26 = Utilities.Refraction_deg(num * (180.0 / Math.PI), 1016.0, 0.0) / (180.0 / Math.PI);
							num25 = 1.000292 * Math.Cos(num + num26) / Math.Cos(num);
						}
						xi = num25 * pCos * Math.Sin(num13);
						eta = num25 * pSin * Math.Cos(DecStar_Apparent) - num25 * pCos * Math.Sin(DecStar_Apparent) * Math.Cos(num13);
						if (flag && ((k == 0) | ((k == 1) & !array[0])))
						{
							X_RefPoint = 0.0 - xi;
							Y_RefPoint = eta;
							GetDisplacementOnFundamentalPlane(EventDetails.RefHour_forAnalysis, out Xoffset, out Yoffset);
							X_RefPoint_atMidTforMotions = X_RefPoint + Xoffset;
							Y_RefPoint_atMidTforMotions = Y_RefPoint - Yoffset;
							flag = false;
						}
						GetDisplacementOnFundamentalPlane(T[j, k] + LightTime_Geo_Secs[j, k] / 86400.0, out Xoffset, out Yoffset);
						XoffsetSat = (YoffsetSat = 0.0);
						if ((EventDetails.Observers[j].Event_D == "G") | (EventDetails.Observers[j].Event_R == "B"))
						{
							GetExtraSatelliteDisplacementOnFundamentalPlane(T[j, k] + LightTime_Geo_Secs[j, k] / 86400.0, out XoffsetSat, out YoffsetSat);
						}
						X[j, k] = (X_RefPoint_atMidTforMotions - Xoffset - XoffsetSat + xi) * 6378.137;
						Y[j, k] = (Y_RefPoint_atMidTforMotions + Yoffset + YoffsetSat - eta) * 6378.137;
						num13 = num3 + num7 - RAStar_Apparent + 1.002738 * T[j, k + 2] * 15.0 / (180.0 / Math.PI);
						xi = pCos * Math.Sin(num13);
						eta = pSin * Math.Cos(DecStar_Apparent) - pCos * Math.Sin(DecStar_Apparent) * Math.Cos(num13);
						GetDisplacementOnFundamentalPlane(T[j, k + 2] + LightTime_Geo_Secs[j, k] / 86400.0, out Xoffset, out Yoffset);
						X[j, k + 2] = (X_RefPoint_atMidTforMotions - Xoffset + xi) * 6378.137;
						Y[j, k + 2] = (Y_RefPoint_atMidTforMotions + Yoffset - eta) * 6378.137;
						ErrorX_motion[j, k] = (float)((X[j, k] - X[j, k + 2]) / 60.0 / (10.0 + T[j, k + 2] - T[j, k]));
						ErrorY_Motion[j, k] = (float)((Y[j, k] - Y[j, k + 2]) / 60.0 / (10.0 + T[j, k + 2] - T[j, k]));
					}
					else
					{
						X[j, k] = 10000.0;
						Y[j, k] = 10000.0;
						X[j, k + 2] = 10000.0;
						Y[j, k + 2] = 10000.0;
						ErrorX_motion[j, k] = 0f;
						ErrorY_Motion[j, k] = 0f;
					}
					CloudyObs[j, k] = text == "(C) ";
					PredictedObs[j, k] = text == "(P) ";
				}
			}
			for (int l = 0; l < 6; l++)
			{
				DoublesOffset_X[l] = (DoublesOffset_Y[l] = 0.0);
			}
			((Control)PlotForm.panelDouble).set_Visible(false);
			((Control)PlotForm.PanelSatellites).set_Visible(false);
			if (!EventDetails.StarIsDouble)
			{
				PlotForm.optBoth.set_Checked(true);
				((Control)PlotForm.pnlDouble).set_Enabled(false);
			}
			else
			{
				((Control)PlotForm.pnlDouble).set_Enabled(true);
				((Control)PlotForm.panelDouble).set_Visible(true);
				((Control)PlotForm.lblDoubleStar).set_Visible(true);
				((Control)PlotForm.pnlDoubleStarSolution).set_Visible(true);
				((Control)PlotForm.lblSatelliteQuality).set_Visible(false);
				PlotForm.mnuAlign.set_Checked(true);
				PlotForm.chkEditJDSO.set_Checked(false);
				for (int m = 0; m < EventDetails.NumberOfDoubleSolutions; m++)
				{
					DoublesOffset_X[EventDetails.Doubles[m].SolutionID] = EventDetails.Doubles[m].Offset_X;
					DoublesOffset_Y[EventDetails.Doubles[m].SolutionID] = EventDetails.Doubles[m].Offset_Y;
				}
				PlotForm.SetLabelsForDoubleOffsets();
			}
			if (EventDetails.AsteroidHasSatellite)
			{
				((Control)PlotForm.PanelSatellites).set_Visible(true);
				PlotForm.updnSatNum.set_Value(1m);
				PlotForm.SetSatelliteControls();
				PlotForm.SetSatelliteControls_Visible(SelectedSatellite);
				((Control)PlotForm.lblSatelliteQuality).set_Visible(true);
				((Control)PlotForm.PanelSatellites).set_Height(313);
				PlotForm.MiriadeAsteroids = Settings.Default.MiriadeAsteroids.Split(new char[1] { ' ' });
				for (int n = 0; n < PlotForm.MiriadeAsteroids.Length; n++)
				{
					if (PlotForm.MiriadeAsteroids[n] == EventDetails.AsteroidNumber.Trim())
					{
						((Control)PlotForm.PanelSatellites).set_Height(392);
					}
				}
				Label lblSat = PlotForm.lblSat1;
				Label lblSat2 = PlotForm.lblSat2;
				Label lblSat3 = PlotForm.lblSat3;
				string text3;
				((Control)PlotForm.lblSat4).set_Text(text3 = "");
				string text4;
				((Control)lblSat3).set_Text(text4 = text3);
				string text5;
				((Control)lblSat2).set_Text(text5 = text4);
				((Control)lblSat).set_Text(text5);
				string path = Utilities.AppPath + "\\Resource Files\\BinaryAsteroid_SatelliteNames.txt";
				if (File.Exists(path))
				{
					using StreamReader streamReader = new StreamReader(path);
					int num27 = 0;
					string value = "(" + EventDetails.AsteroidNumber + ")";
					do
					{
						string text6 = streamReader.ReadLine();
						if (text6.Contains(value))
						{
							if (text6.Contains("S/"))
							{
								text6 = text6.Substring(text6.IndexOf("S"));
							}
							string text7 = text6.Trim();
							switch (num27)
							{
							case 0:
								((Control)PlotForm.lblSat1).set_Text(text7);
								break;
							case 1:
								((Control)PlotForm.lblSat2).set_Text(text7);
								break;
							case 2:
								((Control)PlotForm.lblSat3).set_Text(text7);
								break;
							case 3:
								((Control)PlotForm.lblSat4).set_Text(text7);
								break;
							}
							num27++;
						}
					}
					while (num27 <= 3 && !streamReader.EndOfStream);
				}
				PlotForm.mnuAlign.set_Checked(false);
			}
			GetPlotParametersFromForm();
			XMin = (YMin = 20000.0);
			XMax = (YMax = -20000.0);
			double num28 = 20000.0;
			double num29 = 20000.0;
			double num30 = -20000.0;
			double num31 = -20000.0;
			PrimaryStarEventFound = false;
			for (int j = 0; j < EventDetails.Observers.Count; j++)
			{
				if (!(!CloudyObs[j, 0] & !PredictedObs[j, 0] & (EventDetails.Observers[j].PlotCode != "x")))
				{
					continue;
				}
				for (int k = 0; k <= 1; k++)
				{
					bool flag2 = "De".Contains(EventDetails.Observers[j].Event_D) & ((EventDetails.Observers[j].Weight_D != 0) | !EventDetails.Observers[j].Weight_D_Set);
					bool flag3 = "d".Contains(EventDetails.Observers[j].Event_D) & ((EventDetails.Observers[j].Weight_D != 0) | !EventDetails.Observers[j].Weight_D_Set);
					if (k == 1)
					{
						flag2 = "Rf".Contains(EventDetails.Observers[j].Event_R) & ((EventDetails.Observers[j].Weight_R != 0) | !EventDetails.Observers[j].Weight_R_Set);
						flag3 = "r".Contains(EventDetails.Observers[j].Event_R) & ((EventDetails.Observers[j].Weight_R != 0) | !EventDetails.Observers[j].Weight_R_Set);
					}
					if (flag2)
					{
						PrimaryStarEventFound = true;
						if (X[j, k] != 10000.0)
						{
							if (XMin > X[j, k])
							{
								XMin = X[j, k];
							}
							if (XMax < X[j, k])
							{
								XMax = X[j, k];
							}
							if (YMin > Y[j, k])
							{
								YMin = Y[j, k];
							}
							if (YMax < Y[j, k])
							{
								YMax = Y[j, k];
							}
						}
					}
					if (!PrimaryStarEventFound && flag3 && X[j, k] != 10000.0)
					{
						if (num28 > X[j, k])
						{
							num28 = X[j, k];
						}
						if (num30 < X[j, k])
						{
							num30 = X[j, k];
						}
						if (num29 > Y[j, k])
						{
							num29 = Y[j, k];
						}
						if (num31 < Y[j, k])
						{
							num31 = Y[j, k];
						}
					}
				}
			}
			if (!PrimaryStarEventFound)
			{
				XMin = num28;
				XMax = num30;
				YMin = num29;
				YMax = num31;
			}
			X_CenterOfPoints = (XMin + XMax) / 2.0;
			Y_CenterOfPoints = (YMin + YMax) / 2.0;
			XRange = Math.Abs(XMax - XMin);
			YRange = Math.Abs(YMax - YMin);
			Range = XRange;
			if (Range < YRange)
			{
				Range = YRange;
			}
			if (Range < EventDetails.AsteroidNominalDiameter)
			{
				Range = EventDetails.AsteroidNominalDiameter;
			}
			bool flag4 = false;
			for (int j = 0; j < EventDetails.Observers.Count; j++)
			{
				if ((X[j, 0] != 10000.0) & (X[j, 1] != 10000.0) & (X[j, 0] != X[j, 1]))
				{
					if (Math.Abs(X[j, 1] - X[j, 0]) < Math.Abs(Y[j, 1] - Y[j, 0]))
					{
						flag4 = true;
					}
					break;
				}
			}
			for (int j = 0; j < EventDetails.Observers.Count; j++)
			{
				if (!flag4)
				{
					if ((X[j, 0] != 10000.0) & (X[j, 1] != 10000.0) & (X[j, 0] != X[j, 1]))
					{
						EventDetails.Observers[j].MinimumDistance = Y[j, 0] - Y_CenterOfPoints + (Y[j, 1] - Y[j, 0]) / (X[j, 1] - X[j, 0]) * (X_CenterOfPoints - X[j, 0]);
					}
					else if (Y[j, 0] != 10000.0)
					{
						EventDetails.Observers[j].MinimumDistance = Y[j, 0] - Y_CenterOfPoints + (Y[j, 2] - Y[j, 0]) / (X[j, 2] - X[j, 0]) * (X_CenterOfPoints - X[j, 0]);
					}
					else if (Y[j, 1] != 10000.0)
					{
						EventDetails.Observers[j].MinimumDistance = Y[j, 1] - Y_CenterOfPoints + (Y[j, 3] - Y[j, 1]) / (X[j, 3] - X[j, 1]) * (X_CenterOfPoints - X[j, 1]);
					}
				}
				else if ((X[j, 0] != 10000.0) & (X[j, 1] != 10000.0) & (X[j, 0] != X[j, 1]))
				{
					EventDetails.Observers[j].MinimumDistance = X[j, 0] - X_CenterOfPoints + (X[j, 1] - X[j, 0]) / (Y[j, 1] - Y[j, 0]) * (Y_CenterOfPoints - Y[j, 0]);
				}
				else if (Y[j, 0] != 10000.0)
				{
					EventDetails.Observers[j].MinimumDistance = X[j, 0] - X_CenterOfPoints + (X[j, 2] - X[j, 0]) / (Y[j, 2] - Y[j, 0]) * (Y_CenterOfPoints - Y[j, 0]);
				}
				else if (Y[j, 1] != 10000.0)
				{
					EventDetails.Observers[j].MinimumDistance = X[j, 1] - X_CenterOfPoints + (X[j, 3] - X[j, 1]) / (Y[j, 3] - Y[j, 1]) * (Y_CenterOfPoints - Y[j, 1]);
				}
			}
			PlotGeocentricX = (0.0 - X_RefPoint) * 6378.137 + X_CenterOfPoints;
			PlotGeocentricY = Y_RefPoint * 6378.137 - Y_CenterOfPoints;
			PlotGeocentricT = EventDetails.RefHour_forAnalysis;
			SetPlotFormParametersFromFile();
			int num32 = (int)Math.Floor((double)PlotForm.lstObservers.get_Items().get_Count() / 5.0);
			int height = 12 * PlotForm.lstObservers.get_Items().get_Count() + 4 * num32 + 5;
			((Control)PlotForm.picLegend).set_Height(height);
			((Control)PlotForm.picLegend).set_Width(NoUncert = (int)(8.5 * (double)num2 + 75.0));
			Bitmap image = new Bitmap(NoUncert, height);
			Graphics graphics = Graphics.FromImage(image);
			if (PenChords[0] == null)
			{
				SetMulticolorPens();
			}
			if (Settings.Default.GraphicsSmoothed)
			{
				graphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			graphics.Clear(Color.White);
			for (int num33 = 0; num33 < PlotForm.lstObservers.get_Items().get_Count(); num33++)
			{
				graphics.DrawString(PlotForm.lstObservers.get_Items().get_Item(num33).ToString(), font, Brushes.Black, 75f, (float)((double)(12 * num33) + 4.0 * Math.Floor((double)num33 / 5.0)));
				if (PlotForm.lstObservers.get_Items().get_Item(num33).ToString()!.Contains("Predicted"))
				{
					for (int num34 = 0; num34 < 5; num34++)
					{
						graphics.DrawEllipse(pen, 6 + 15 * num34, 6 + num33 * 12 + 4 * (int)Math.Floor((double)num33 / 5.0), 2, 2);
					}
				}
				else
				{
					graphics.DrawLine(PenChords[Convert.ToInt16(arrayList[num33]!.ToString())], 5, 7 + num33 * 12 + 4 * (int)Math.Floor((double)num33 / 5.0), 70, 7 + num33 * 12 + 4 * (int)Math.Floor((double)num33 / 5.0));
					graphics.DrawLine(PenChords[Convert.ToInt16(arrayList[num33]!.ToString())], 5, 6 + num33 * 12 + 4 * (int)Math.Floor((double)num33 / 5.0), 70, 6 + num33 * 12 + 4 * (int)Math.Floor((double)num33 / 5.0));
				}
			}
			PlotForm.picLegend.set_Image((Image)image);
			graphics.Dispose();
			PlotForm.Updating = false;
		}

		internal static void SetPlotFormParametersFromFile()
		{
			PlotForm.chkUseAssumedDiameter.set_Checked(EventDetails.UsedAssumedDiameter);
			PlotForm.chkCircle.set_Checked(EventDetails.Solve_Circular | PlotForm.chkUseAssumedDiameter.get_Checked());
			NumericUpDown updnX = PlotForm.updnX;
			NumericUpDown updnY = PlotForm.updnY;
			NumericUpDown updnA = PlotForm.updnA;
			int num;
			PlotForm.updnB.set_DecimalPlaces(num = 3);
			int num2;
			updnA.set_DecimalPlaces(num2 = num);
			int decimalPlaces;
			updnY.set_DecimalPlaces(decimalPlaces = num2);
			updnX.set_DecimalPlaces(decimalPlaces);
			PlotObservations.SetDecimalPlaces = false;
			PlotForm.updnX.set_Value((decimal)EventDetails.X);
			PlotForm.updnY.set_Value((decimal)EventDetails.Y);
			PlotForm.updnA.set_Value((decimal)EventDetails.X_Dia);
			PlotForm.updnB.set_Value((decimal)EventDetails.Y_Dia);
			PlotForm.updnPA.set_Value((decimal)EventDetails.PA_Ellipse);
			PlotObservations.SetDecimalPlaces = true;
			PlotForm.SetDiameterDecimalPlaces();
			PlotForm.updnCenterOfMass_X.set_Value(-(decimal)EventDetails.CentreOfMass_Offset_X);
			PlotForm.updnCenterOfMass_Y.set_Value((decimal)EventDetails.CentreOfMass_Offset_Y);
			PlotForm.chkShapeModelCentered.set_Checked(EventDetails.AstrometryShapeModelCentered);
			if ((EventDetails.CentreOfMass_Offset_X != 0.0) | (EventDetails.CentreOfMass_Offset_Y != 0.0))
			{
				PlotForm.chkShapeModelCentered.set_Checked(false);
			}
			((Control)PlotForm.chkShapeModelCentered).set_Enabled(PlotForm.chkShapeModelCentered.get_Checked());
			PlotForm.chkCompanionPA.set_Checked(EventDetails.Solve_CompanionPA);
			PlotForm.chkCompanionSep.set_Checked(EventDetails.Solve_CompanionSep);
			try
			{
				for (int i = 0; i < 4; i++)
				{
					double num3 = EventDetails.Doubles[i].Sep_Companion;
					if (num3 < 0.0 && num3 > -0.3)
					{
						num3 = -0.3;
					}
					PlotForm.updn_DoubleSep[i].set_Value((decimal)Math.Abs(num3));
					PlotForm.updn_DoublePA[i].set_Value((decimal)EventDetails.Doubles[i].PA_Companion);
					if (PlotForm.updn_DoublePA[i].get_Value() < 0m)
					{
						NumericUpDown obj = PlotForm.updn_DoublePA[i];
						obj.set_Value(obj.get_Value() + 360m);
					}
				}
				if (!PrimaryStarEventFound)
				{
					double num4 = 0.0;
					double num5 = 0.05;
					for (int j = 0; j < EventDetails.Observers.Count; j++)
					{
						if (EventDetails.Observers[j].PlotCode == " ")
						{
							dX = X[j, 0] - X[j, 1];
							dY = Y[j, 0] - Y[j, 1];
							num4 = Math.Sqrt(dX * dX + dY * dY);
							if (num4 > num5)
							{
								num5 = num4;
							}
						}
					}
					if (num5 > Diameter)
					{
						num5 = Diameter;
					}
					double num6 = (Diameter - Math.Sqrt(Diameter * Diameter - num5 * num5)) / Diameter / 2.0;
					double num7 = 1000.0 * Diameter / 6378.137 * Parallax * num6;
					if (num7 > 0.0 && num7 < 0.3)
					{
						num7 = 0.3;
					}
					PlotForm.updn_DoubleSep[0].set_Value((decimal)num7);
					PlotForm.updn_DoublePA[0].set_Value((decimal)(90.0 + Math.Atan(EventDetails.dX / EventDetails.dY) * (180.0 / Math.PI)));
					bool showAlign;
					PlotForm.mnuAlign.set_Checked(showAlign = false);
					ShowAlign = showAlign;
				}
				PlotForm.SetDoubleSolutionType(EventDetails.NumberOfDoubleSolutions, Initialising: true);
			}
			catch
			{
			}
			if (EventDetails.AsteroidHasSatellite)
			{
				PlotForm.SetSatelliteControls();
			}
			PlotForm.chkX.set_Checked(EventDetails.Solve_X);
			PlotForm.chkY.set_Checked(EventDetails.Solve_Y);
			PlotForm.chkA.set_Checked(EventDetails.Solve_Major);
			PlotForm.chkB.set_Checked(EventDetails.Solve_Minor);
			PlotForm.chkPA.set_Checked(EventDetails.Solve_PA);
			if (PlotForm.chkUseAssumedDiameter.get_Checked())
			{
				NumericUpDown updnA2 = PlotForm.updnA;
				decimal value;
				PlotForm.updnB.set_Value(value = (decimal)GetAssumedDiameter());
				updnA2.set_Value(value);
				CheckBox chkA = PlotForm.chkA;
				CheckBox chkB = PlotForm.chkB;
				bool flag;
				((Control)PlotForm.chkPA).set_Enabled(flag = false);
				bool showAlign;
				((Control)chkB).set_Enabled(showAlign = flag);
				((Control)chkA).set_Enabled(showAlign);
				NumericUpDown updnA3 = PlotForm.updnA;
				NumericUpDown updnB = PlotForm.updnB;
				((Control)PlotForm.updnPA).set_Enabled(flag = false);
				((Control)updnB).set_Enabled(showAlign = flag);
				((Control)updnA3).set_Enabled(showAlign);
				PlotForm.updnPA.set_Value(0m);
			}
			else if (PlotForm.chkCircle.get_Checked())
			{
				CheckBox chkB2 = PlotForm.chkB;
				bool showAlign;
				((Control)PlotForm.chkPA).set_Enabled(showAlign = false);
				((Control)chkB2).set_Enabled(showAlign);
				((Control)PlotForm.updnA).set_Enabled(true);
				NumericUpDown updnB2 = PlotForm.updnB;
				((Control)PlotForm.updnPA).set_Enabled(showAlign = false);
				((Control)updnB2).set_Enabled(showAlign);
				PlotForm.updnPA.set_Value(0m);
			}
			PlotForm.chkMiss.set_Checked(EventDetails.Inc_Miss);
			((ListControl)PlotForm.cmbQuality).set_SelectedIndex(EventDetails.Quality_To_cmbQuality);
			PlotForm.Populate_ReviewList(EventDetails.FlagForReview);
		}

		internal static double GetAssumedDiameter()
		{
			double num = 0.0;
			int.TryParse(EventDetails.AsteroidNumber, out var result);
			if (result < 550000 && result > 0)
			{
				using FileStream fileStream = new FileStream(Utilities.AppPath + "\\Resource Files\\AsteroidDiameters.bin", FileMode.Open, FileAccess.Read, FileShare.ReadWrite);
				using BinaryReader binaryReader = new BinaryReader(fileStream);
				fileStream.Seek(2 * (result - 1), SeekOrigin.Begin);
				num = (double)binaryReader.ReadInt16() / 10.0;
				if (num <= 0.0)
				{
					num = 0.0 - num;
				}
			}
			if (num == 0.0)
			{
				num = EventDetails.AsteroidNominalDiameter;
			}
			return num;
		}

		internal static void getMiriadeSatelliteMotions()
		{
			//IL_00f3: Unknown result type (might be due to invalid IL or missing references)
			int num = (int)PlotForm.updnSatNum.get_Value();
			List<BinaryAsteroidOffsets> BinaryOffsets = new List<BinaryAsteroidOffsets>();
			XMLprocess.GetMiraideEphemeris(int.Parse(EventDetails.AsteroidNumber), EventDetails.JD_EventDate + EventDetails.MidT_forMotions / 24.0, ref BinaryOffsets, SaveMiriadeResponse: true);
			SelectSatellite selectSatellite = new SelectSatellite();
			for (int i = 0; i < BinaryOffsets.Count; i++)
			{
				selectSatellite.lstSatellites.get_Items().Add((object)(BinaryOffsets[i].ComponentSeqNum + "   " + BinaryOffsets[i].SolutionID + "   " + BinaryOffsets[i].ComponentName + " (" + BinaryOffsets[i].SolutionType + ")"));
			}
			((Form)selectSatellite).ShowDialog();
			PlotForm.updnSatNum.set_Value((decimal)num);
			int selectedIndex = ((ListControl)selectSatellite.lstSatellites).get_SelectedIndex();
			if (selectedIndex >= 0)
			{
				int num2 = (int)PlotForm.updnSatNum.get_Value() - 1;
				Utilities.FitQuadraticTo3Points(BinaryOffsets[selectedIndex].Offset_RA_mas[0], BinaryOffsets[selectedIndex].Offset_RA_mas[1], BinaryOffsets[selectedIndex].Offset_RA_mas[2], out var Xat, out var num3, out var num4);
				EventDetails.Satellites[num2].Sat_dRA_mas = num3;
				EventDetails.Satellites[num2].Sat_d2RA_mas = num4;
				Utilities.FitQuadraticTo3Points(BinaryOffsets[selectedIndex].Offset_Dec_mas[0], BinaryOffsets[selectedIndex].Offset_Dec_mas[1], BinaryOffsets[selectedIndex].Offset_Dec_mas[2], out Xat, out num3, out num4);
				EventDetails.Satellites[num2].Sat_dDec_mas = num3;
				EventDetails.Satellites[num2].Sat_d2Dec_mas = num4;
				((Control)PlotForm.txtSat_dRA[num2]).set_Text(string.Format("{0,1:f1}", EventDetails.Satellites[num2].Sat_dRA_mas));
				((Control)PlotForm.txtSat_dDec[num2]).set_Text(string.Format("{0,1:f1}", EventDetails.Satellites[num2].Sat_dDec_mas));
				((Control)PlotForm.txtSat_d2RA[num2]).set_Text(string.Format("{0,1:f1}", EventDetails.Satellites[num2].Sat_d2RA_mas));
				((Control)PlotForm.txtSat_d2Dec[num2]).set_Text(string.Format("{0,1:f1}", EventDetails.Satellites[num2].Sat_d2Dec_mas));
			}
		}

		private static void GetDisplacementOnFundamentalPlane(double T_Event, out double X, out double Y)
		{
			double num = T_Event - TZero_forMotions;
			X = EventDetails.dX * num + EventDetails.d2X * num * num + EventDetails.d3X * num * num * num;
			Y = EventDetails.dY * num + EventDetails.d2Y * num * num + EventDetails.d3Y * num * num * num;
		}

		private static void GetExtraSatelliteDisplacementOnFundamentalPlane(double T_Event, out double X, out double Y)
		{
			double num = T_Event - EventDetails.RefHour_forAnalysis;
			X = (EventDetails.Satellites[SelectedSatellite].Sat_dRA_mas * num + EventDetails.Satellites[SelectedSatellite].Sat_d2RA_mas * num * num) / 1000.0 / EventDetails.Parallax;
			Y = (EventDetails.Satellites[SelectedSatellite].Sat_dDec_mas * num + EventDetails.Satellites[SelectedSatellite].Sat_d2Dec_mas * num * num) / 1000.0 / EventDetails.Parallax;
		}

		internal static void PlotEventOnScreen()
		{
			Application.DoEvents();
			EnableRingEventPaths();
			int width = ((Control)PlotForm.picPlot).get_Width();
			int height = ((Control)PlotForm.picPlot).get_Height();
			Bitmap image = new Bitmap(width, height);
			Graphics graphics = Graphics.FromImage(image);
			if (Settings.Default.GraphicsSmoothed)
			{
				graphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			graphics.Clear(Color.White);
			PlotEvent(graphics, width, height, PrintOnPrinter: false);
			PlotForm.picPlot.set_Image((Image)image);
			graphics.Dispose();
			((Control)PlotForm.picPlot).Focus();
			GetVariations();
			PlotForm.SetRatioAndMag();
		}

		internal static void EnableRingEventPaths()
		{
			bool enabled = false;
			for (int i = 0; i < EventDetails.Observers.Count; i++)
			{
				if ((EventDetails.Observers[i].Event_D.ToUpper() == "N") | (EventDetails.Observers[i].Event_R.ToUpper() == "N"))
				{
					enabled = true;
					break;
				}
			}
			((ToolStripItem)PlotForm.showRINGEventPathsToolStripMenuItem).set_Enabled(enabled);
		}

		internal static void PrintEvent()
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
				printDocument.PrintPage += PrintTheEvent;
				printDocument.Print();
			}
		}

		internal static void PrintEventPreview()
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			//IL_000c: Expected O, but got Unknown
			//IL_000c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_002b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0032: Unknown result type (might be due to invalid IL or missing references)
			//IL_0038: Invalid comparison between Unknown and I4
			//IL_004d: Unknown result type (might be due to invalid IL or missing references)
			PrintDocument printDocument = new PrintDocument();
			PrintPreviewDialog val = new PrintPreviewDialog();
			PrintDialog val2 = new PrintDialog();
			val2.set_UseEXDialog(true);
			printDocument.DefaultPageSettings.Landscape = true;
			val.set_Document(printDocument);
			val2.set_Document(printDocument);
			if ((int)((CommonDialog)val2).ShowDialog() == 1)
			{
				printDocument.PrintPage += PrintTheEvent;
				((Form)val).ShowDialog();
			}
		}

		internal static void PrintTheEvent(object sender, PrintPageEventArgs e)
		{
			Graphics graphics = e.Graphics;
			int num = (int)(0.92 * (double)e.PageBounds.Height);
			int num2 = (int)(0.92 * (double)e.PageBounds.Width);
			if (num < num2)
			{
				num2 = num;
			}
			else
			{
				num = num2;
			}
			PlotEvent(graphics, num2, num, PrintOnPrinter: true);
		}

		internal static void PlotEvent(Graphics formGraphics, int ChartWidth, int ChartHeight, bool PrintOnPrinter)
		{
			//IL_11e4: Unknown result type (might be due to invalid IL or missing references)
			if (PlotForm.updn_PASat == null)
			{
				ShowPlotForm();
				((Control)PlotForm).Focus();
			}
			float num = 0f;
			float num2 = 0f;
			float num3 = 0f;
			float num4 = 0f;
			float num5 = 0f;
			float num6 = 0f;
			float num7 = 0f;
			float num8 = 0f;
			float num9 = 2f;
			float num10 = (float)((double)ChartWidth / Range * 0.6000000238418579);
			float num11 = (float)PlotForm.SliderScale.get_Value() / 150f;
			num10 = ((!(num11 > 0f)) ? (num10 / (1f - num11)) : (num10 * (1f + num11)));
			if (PlotForm.optPlotx2.get_Checked())
			{
				num10 /= 2f;
				HScrollBar hbar = PlotForm.Hbar;
				int maximum;
				((ScrollBar)PlotForm.Vbar).set_Maximum(maximum = 2000);
				((ScrollBar)hbar).set_Maximum(maximum);
				HScrollBar hbar2 = PlotForm.Hbar;
				((ScrollBar)PlotForm.Vbar).set_Minimum(maximum = -2000);
				((ScrollBar)hbar2).set_Minimum(maximum);
			}
			if (PlotForm.optPlotx5.get_Checked())
			{
				num10 /= 5f;
				HScrollBar hbar3 = PlotForm.Hbar;
				int maximum;
				((ScrollBar)PlotForm.Vbar).set_Maximum(maximum = 5000);
				((ScrollBar)hbar3).set_Maximum(maximum);
				HScrollBar hbar4 = PlotForm.Hbar;
				((ScrollBar)PlotForm.Vbar).set_Minimum(maximum = -5000);
				((ScrollBar)hbar4).set_Minimum(maximum);
			}
			else
			{
				HScrollBar hbar5 = PlotForm.Hbar;
				int maximum;
				((ScrollBar)PlotForm.Vbar).set_Maximum(maximum = 1000);
				((ScrollBar)hbar5).set_Maximum(maximum);
				HScrollBar hbar6 = PlotForm.Hbar;
				((ScrollBar)PlotForm.Vbar).set_Minimum(maximum = -1000);
				((ScrollBar)hbar6).set_Minimum(maximum);
			}
			int num12 = 0;
			int num13 = 0;
			if (Range == 0.0)
			{
				return;
			}
			Cursor.set_Current(Cursors.get_WaitCursor());
			Tags.Clear();
			if (Settings.Default.GraphicsSmoothed)
			{
				formGraphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			Font font2;
			Font font3;
			Font font;
			if (PlotForm.mnuLargeLabels.get_Checked())
			{
				font2 = (font = new Font("Courier New", (float)Settings.Default.AsterPlot_LargeFontSize));
				font3 = new Font("Courier New", (float)Settings.Default.AsterPlot_LargeFontSize, FontStyle.Underline);
			}
			else
			{
				font2 = (font = new Font("Courier New", 8f));
				font3 = new Font("Courier New", 8f, FontStyle.Underline);
			}
			Font font4 = new Font("MSSansSerif", 10f, FontStyle.Bold);
			Font font5 = new Font("MSSansSerif", 18f, FontStyle.Bold);
			SetMulticolorPens();
			Brush red = Brushes.Red;
			Brush forestGreen = Brushes.ForestGreen;
			Brush brush;
			Brush brush2;
			Brush brush3;
			Pen pen;
			Pen pen2;
			Pen pen3;
			Brush gold;
			Pen pen4;
			if ((WhiteBackground | GrayBackground) || PrintOnPrinter)
			{
				brush = ((!(GrayBackground && !PrintOnPrinter)) ? Brushes.White : new SolidBrush(Color.FromArgb(255, 230, 245, 245)));
				new SolidBrush(Color.FromArgb(220, 255, 255, 255));
				brush2 = Brushes.Black;
				brush3 = Brushes.DarkGreen;
				pen = new Pen(Color.Black, 1f);
				pen2 = new Pen(Color.Black, 0.7f);
				pen3 = new Pen(Color.Black, 0.7f);
				gold = Brushes.Gold;
				pen4 = new Pen(Color.Gray, 0.7f);
			}
			else
			{
				brush = Brushes.Black;
				new SolidBrush(Color.FromArgb(40, 0, 0, 0));
				brush2 = Brushes.White;
				brush3 = Brushes.LawnGreen;
				pen = new Pen(Color.White, 1f);
				pen2 = new Pen(Color.White, 0.7f);
				pen3 = new Pen(Color.White, 0.7f);
				gold = Brushes.Gold;
				pen4 = new Pen(Color.White, 0.7f);
			}
			Pen pen6;
			Pen pen5;
			Pen pen8;
			Pen pen7;
			Pen pen9;
			Pen pen10;
			Pen pen11;
			Pen pen12;
			Pen pen13;
			Pen pen14;
			Pen pen15;
			Pen pen16;
			Pen pen17;
			Pen pen18;
			Pen pen19;
			Pen pen20;
			Pen pen22;
			Pen pen23;
			Pen pen24;
			Pen pen25;
			Pen pen21;
			Brush brush4;
			if ((WhiteBackground | GrayBackground) || PrintOnPrinter)
			{
				if (ShowEventsInColour)
				{
					pen6 = (pen5 = new Pen(Color.Red, 2f));
					pen8 = (pen7 = new Pen(Color.ForestGreen, 2f));
					pen9 = new Pen(Color.DeepPink, 2f);
					pen10 = new Pen(Color.LimeGreen, 2f);
					pen11 = new Pen(Color.Tomato, 2f);
					pen12 = new Pen(Color.Lime, 2f);
				}
				else
				{
					pen6 = (pen8 = (pen9 = (pen10 = (pen11 = (pen12 = (pen5 = (pen7 = new Pen(Color.Black, 1f))))))));
				}
				if (ShowErrorsForChordLength)
				{
					if (!WhiteBackground)
					{
						pen5 = new Pen(Color.Aqua, 3f);
						pen7 = new Pen(Color.Aqua, 3f);
					}
					else
					{
						pen5 = new Pen(Color.Purple, 2f);
						pen7 = new Pen(Color.Purple, 2f);
					}
				}
				pen13 = new Pen(Color.Purple, LineThicknessOther);
				pen14 = new Pen(Color.HotPink, 1f);
				new Pen(Color.LawnGreen, 1f);
				pen15 = new Pen(Color.Black, 1f);
				pen16 = new Pen(Color.Black, 1f);
				pen17 = new Pen(Color.CadetBlue, LineThicknessOther);
				new Pen(Color.White, 1f);
				if (ShowEllipseInColour)
				{
					pen18 = new Pen(Color.Orange, 1f);
					pen19 = new Pen(Color.DarkGoldenrod, LineThicknessOther);
					pen20 = new Pen(Color.DarkOrange, LineThicknessOther);
					pen21 = new Pen(Color.Tan, 5f);
					pen22 = new Pen(Color.Red, LineThicknessOther);
				}
				else
				{
					pen18 = new Pen(Color.Black, 1f);
					pen19 = new Pen(Color.Black, LineThicknessOther);
					pen20 = new Pen(Color.Black, LineThicknessOther);
					pen22 = new Pen(Color.Black, LineThicknessOther / 2f);
				}
				pen23 = new Pen(Color.Blue, 1.5f);
				pen24 = new Pen(Color.Black, 1f);
				pen25 = new Pen(Color.Black, 1f);
				pen21 = ((!(WhiteBackground || PrintOnPrinter)) ? new Pen(Color.DarkGray, 5f) : new Pen(Color.Gray, 5f));
				brush4 = Brushes.CadetBlue;
			}
			else
			{
				if (ShowEventsInColour)
				{
					pen6 = (pen5 = new Pen(Color.Red, 1f));
					pen8 = (pen7 = new Pen(Color.ForestGreen, 1f));
					pen9 = new Pen(Color.DeepPink, 1f);
					pen10 = new Pen(Color.LimeGreen, 1f);
					pen11 = new Pen(Color.Tomato, 1f);
					pen12 = new Pen(Color.Lime, 1f);
				}
				else
				{
					pen6 = (pen8 = (pen9 = (pen10 = (pen11 = (pen12 = (pen5 = (pen7 = new Pen(Color.White, 1f))))))));
				}
				if (ShowErrorsForChordLength)
				{
					if (!WhiteBackground)
					{
						pen5 = new Pen(Color.Aqua, 3f);
						pen7 = new Pen(Color.Aqua, 3f);
					}
					else
					{
						pen5 = new Pen(Color.Purple, 2f);
						pen7 = new Pen(Color.Purple, 2f);
					}
				}
				pen13 = new Pen(Color.Lavender, 1f);
				pen14 = new Pen(Color.HotPink, 1f);
				new Pen(Color.LawnGreen, 1f);
				pen15 = new Pen(Color.Gray, 1f);
				pen18 = new Pen(Color.Orange, 1f);
				pen16 = new Pen(Color.Blue, 2f);
				pen17 = new Pen(Color.LightBlue, LineThicknessOther);
				new Pen(Color.Black, 1f);
				pen23 = new Pen(Color.Cyan, 1.5f);
				pen24 = ((brush != Brushes.Black) ? new Pen(Color.Black, 1f) : new Pen(Color.White, 1f));
				pen25 = new Pen(Color.Violet, 1f);
				if (ShowEllipseInColour)
				{
					pen19 = new Pen(Color.Yellow, LineThicknessOther);
					pen20 = new Pen(Color.Goldenrod, LineThicknessOther);
					pen21 = new Pen(Color.Tan, 5f);
					pen22 = new Pen(Color.Red, LineThicknessOther);
					if (WhiteBackground && !PrintOnPrinter)
					{
						pen25 = new Pen(Color.DarkViolet, 1f);
					}
				}
				else
				{
					pen18 = new Pen(Color.White, 1f);
					pen19 = new Pen(Color.White, LineThicknessOther);
					pen20 = new Pen(Color.White, LineThicknessOther);
					pen21 = new Pen(Color.White, 5f);
					pen22 = new Pen(Color.White, LineThicknessOther / 2f);
				}
				brush4 = Brushes.Aquamarine;
			}
			if (PlotForm.mnuShowEllipseInOutline.get_Checked())
			{
				pen18.DashPattern = new float[2] { 1f, 10f };
			}
			else
			{
				pen18.DashPattern = new float[2] { 5f, 5f };
			}
			pen15.DashPattern = new float[2] { 20f, 20f };
			pen14.DashPattern = new float[2] { 20f, 5f };
			pen5.DashPattern = new float[2] { 3f, 4f };
			pen7.DashPattern = new float[2] { 3f, 4f };
			pen23.DashPattern = new float[2] { 2f, 15f };
			pen22.DashPattern = new float[2] { 4f, 4f };
			pen25.DashPattern = new float[2] { 5f, 3f };
			if ((WhiteBackground | GrayBackground) || PrintOnPrinter)
			{
				if (GrayBackground && !PrintOnPrinter)
				{
					formGraphics.Clear(Color.FromArgb(255, 230, 245, 245));
				}
				else
				{
					formGraphics.Clear(Color.White);
				}
				formGraphics.DrawRectangle(new Pen(Color.Black, 1f), 0, 0, ChartWidth - 1, ChartHeight - 1);
			}
			else
			{
				formGraphics.Clear(Color.Black);
				formGraphics.DrawRectangle(new Pen(Color.White, 1f), 0, 0, ChartWidth - 1, ChartHeight - 1);
			}
			float num14 = (float)ChartWidth / 2f;
			float num15 = (float)ChartHeight / 2f;
			if (FirstTimePlot)
			{
				num12 = (int)((double)((0f - (float)EventDetails.X) * 500f) / Range);
				if (num12 > ((ScrollBar)PlotForm.Hbar).get_Maximum())
				{
					num12 = ((ScrollBar)PlotForm.Hbar).get_Maximum();
				}
				if (num12 < ((ScrollBar)PlotForm.Hbar).get_Minimum())
				{
					num12 = ((ScrollBar)PlotForm.Hbar).get_Minimum();
				}
				((ScrollBar)PlotForm.Hbar).set_Value(num12);
				num13 = (int)((double)((float)EventDetails.Y * 500f) / Range);
				if (num13 > ((ScrollBar)PlotForm.Vbar).get_Maximum())
				{
					num13 = ((ScrollBar)PlotForm.Vbar).get_Maximum();
				}
				if (num13 < ((ScrollBar)PlotForm.Vbar).get_Minimum())
				{
					num13 = ((ScrollBar)PlotForm.Vbar).get_Minimum();
				}
				((ScrollBar)PlotForm.Vbar).set_Value(num13);
				FirstTimePlot = false;
			}
			float num16 = 500f;
			if (PlotForm.chkScoll.get_Checked())
			{
				num16 = 400f;
			}
			float num17 = (float)((0.0 - Range) * (double)((ScrollBar)PlotForm.Hbar).get_Value() / (double)num16);
			float num18 = (float)((0.0 - Range) * (double)((ScrollBar)PlotForm.Vbar).get_Value() / (double)num16);
			float num19 = (float)(X_CenterOfPoints - (double)num17);
			float num20 = (float)(Y_CenterOfPoints + (double)num18);
			CenterOfEllipse_X = (float)PlotForm.updnX.get_Value() + (float)DoublesOffset_X[PlotForm.CurrentlySelectedComponent + 1];
			CenterOfEllipse_Y = (float)PlotForm.updnY.get_Value() + (float)DoublesOffset_Y[PlotForm.CurrentlySelectedComponent + 1];
			float num21 = (float)PlotForm.updnCenterOfMass_X.get_Value();
			float num22 = (float)PlotForm.updnCenterOfMass_Y.get_Value();
			EventDetails.X_Geo_atEvent = PlotGeocentricX - (double)PlotForm.updnX.get_Value();
			EventDetails.Y_Geo_atEvent = PlotGeocentricY + (double)PlotForm.updnY.get_Value();
			if (PlotForm.addShapeImageFromDAMIT_ISAMToolStripMenuItem.get_Checked() && Asteroid_Observations_Reports.Display_ShapeModels != null)
			{
				try
				{
					float x = num14 - (float)(Asteroid_Observations_Reports.Display_ShapeModels.picDisplay.get_Image().Width / 2);
					float y = num15 - (float)(Asteroid_Observations_Reports.Display_ShapeModels.picDisplay.get_Image().Height / 2);
					using (Bitmap bitmap = new Bitmap(Asteroid_Observations_Reports.Display_ShapeModels.picDisplay.get_Image()))
					{
						if (Asteroid_Observations_Reports.Display_ShapeModels.mnuWhiteBackground.get_Checked())
						{
							bitmap.MakeTransparent(Color.White);
						}
						else if (Asteroid_Observations_Reports.Display_ShapeModels.mnuGrayBackground.get_Checked())
						{
							bitmap.MakeTransparent(DisplayShapeModels.ShapeModelGrayBackColor);
						}
						else
						{
							bitmap.MakeTransparent(Color.Black);
						}
						formGraphics.DrawImage(bitmap, x, y);
					}
					LengthOfDiameterByVolLine = (float)((double)(Asteroid_Observations_Reports.Display_ShapeModels.picDisplay.get_Image().Width * 2) * DisplayShapeModels.ModelParameters[DisplayShapeModels.Currently_Displayed_Model].VolumeEquivalentRadius) / DisplayShapeModels.ImageSize / num10;
					LengthOfDiameterBySurfLine = (float)((double)(Asteroid_Observations_Reports.Display_ShapeModels.picDisplay.get_Image().Width * 2) * DisplayShapeModels.ModelParameters[DisplayShapeModels.Currently_Displayed_Model].SurfaceEquivalentRadius) / DisplayShapeModels.ImageSize / num10;
					int num23 = DisplayShapeModels.Currently_Displayed_Model;
					if (num23 > 5)
					{
						num23 -= 6 - Asteroid_Observations_Reports.Display_ShapeModels.NumDAMITImages;
					}
					LengthOfDiameterByVolLine = (float)((double)Asteroid_Observations_Reports.Display_ShapeModels.picDisplay.get_Image().Width * DisplayShapeModels.VolumeEquivalent_Diameter_Pixels[num23] / (double)DisplayShapeModels.ImageSize / (double)num10);
					LengthOfDiameterBySurfLine = (float)((double)Asteroid_Observations_Reports.Display_ShapeModels.picDisplay.get_Image().Width * DisplayShapeModels.SurfaceEquivalent_Diameter_Pixels[num23] / (double)DisplayShapeModels.ImageSize / (double)num10);
				}
				catch
				{
				}
			}
			if (PlotForm.addImageFromClipboardToolStripMenuItem.get_Checked() && Clipboard.ContainsImage())
			{
				try
				{
					float x2 = num14 - (float)(Clipboard.GetImage().Width / 2);
					float y2 = num15 - (float)(Clipboard.GetImage().Height / 2);
					using Bitmap bitmap2 = new Bitmap(Clipboard.GetImage());
					if (WhiteBackground || PrintOnPrinter)
					{
						bitmap2.MakeTransparent(Color.Black);
					}
					else if (GrayBackground)
					{
						bitmap2.MakeTransparent(Color.FromArgb(255, 230, 230, 230));
					}
					formGraphics.DrawImage(bitmap2, x2, y2);
				}
				catch
				{
				}
			}
			if (ShowWatermark)
			{
				string text = "Occult " + Utilities.OccultVersion;
				float emSize = (float)ChartWidth / 17f;
				Font font6 = new Font("Cascadia Code", emSize, FontStyle.Bold);
				int alpha = 60;
				int num24 = 255;
				if (GrayBackground)
				{
					alpha = 100;
					num24 = 90;
				}
				else if (WhiteBackground || PrintOnPrinter)
				{
					alpha = 80;
					num24 = 0;
				}
				Brush brush5 = new SolidBrush(Color.FromArgb(alpha, num24, num24, num24));
				SizeF sizeF = formGraphics.MeasureString(text, font6);
				formGraphics.DrawString("Occult " + Utilities.OccultVersion, font6, brush5, ((float)ChartWidth - sizeF.Width) / 2f, 3f * ((float)ChartHeight - sizeF.Height) / 4f);
			}
			if (ShowProvisionalMark)
			{
				string text2 = "Provisional";
				float emSize2 = (float)ChartWidth / 30f;
				Font font7 = new Font("Cascadia Code", emSize2, FontStyle.Bold);
				int alpha2 = 180;
				int num25 = 255;
				if (GrayBackground)
				{
					alpha2 = 150;
					num25 = 20;
				}
				else if (WhiteBackground || PrintOnPrinter)
				{
					alpha2 = 150;
					num25 = 0;
				}
				Brush brush6 = new SolidBrush(Color.FromArgb(alpha2, num25, num25, num25));
				SizeF sizeF2 = formGraphics.MeasureString(text2, font7);
				formGraphics.DrawString(text2, font7, brush6, num14 + num10 * (CenterOfEllipse_X - num17) - sizeF2.Width / 2f, num15 + num10 * (0f - CenterOfEllipse_Y - num18) - 1.3f * sizeF2.Height);
			}
			num = (num2 = 0f);
			if (EventDetails.AsteroidHasSatellite)
			{
				if (!(EventDetails.Parallax > 0.0))
				{
					MessageBox.Show("The asteroid details have not been set.\r\n\r\nYou MUST set both the Star and Asteroid details before \r\nplotting the event.", "Data not set", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
					return;
				}
				Offset_X_2ndFromPrimary = (float)((double)PlotForm.updn_SatSep[SelectedSatellite].get_Value() / 1000.0 * Math.Sin((double)PlotForm.updn_SatPA[SelectedSatellite].get_Value() / (180.0 / Math.PI)) / EventDetails.Parallax * 6378.137);
				Offset_Y_2ndFromPrimary = (float)((0.0 - (double)PlotForm.updn_SatSep[SelectedSatellite].get_Value()) / 1000.0 * Math.Cos((double)PlotForm.updn_SatPA[SelectedSatellite].get_Value() / (180.0 / Math.PI)) / EventDetails.Parallax * 6378.137);
				Offset_X_2ndFromPrimary = 0f - Offset_X_2ndFromPrimary;
				Offset_Y_2ndFromPrimary = 0f - Offset_Y_2ndFromPrimary;
				num = Offset_X_2ndFromPrimary / 2f;
				num2 = Offset_Y_2ndFromPrimary / 2f;
				CenterOf2ndEllipse_X = CenterOfEllipse_X + Offset_X_2ndFromPrimary;
				CenterOf2ndEllipse_Y = CenterOfEllipse_Y + Offset_Y_2ndFromPrimary;
				num6 = (float)PlotForm.updn_ASat[SelectedSatellite].get_Value() / 2f;
				num7 = (float)PlotForm.updn_BSat[SelectedSatellite].get_Value() / 2f;
				num8 = (float)PlotForm.updn_PASat[SelectedSatellite].get_Value();
			}
			else if (EventDetails.StarIsDouble && EventDetails.Parallax > 0.0)
			{
				Offset_X_2ndFromPrimary = (float)((double)PlotForm.updn_DoubleSep[PlotForm.CurrentlySelectedComponent].get_Value() / 1000.0 * Math.Sin((double)PlotForm.updn_DoublePA[PlotForm.CurrentlySelectedComponent].get_Value() / (180.0 / Math.PI)) / EventDetails.Parallax * 6378.137);
				Offset_Y_2ndFromPrimary = (float)((0.0 - (double)PlotForm.updn_DoubleSep[PlotForm.CurrentlySelectedComponent].get_Value()) / 1000.0 * Math.Cos((double)PlotForm.updn_DoublePA[PlotForm.CurrentlySelectedComponent].get_Value() / (180.0 / Math.PI)) / EventDetails.Parallax * 6378.137);
				CenterOf2ndEllipse_X = CenterOfEllipse_X + Offset_X_2ndFromPrimary;
				CenterOf2ndEllipse_Y = CenterOfEllipse_Y + Offset_Y_2ndFromPrimary;
			}
			num3 = (float)PlotForm.updnA.get_Value() / 2f;
			if (PlotForm.chkCircle.get_Checked())
			{
				num4 = num3;
				num5 = 0f;
			}
			else
			{
				num4 = (float)PlotForm.updnB.get_Value() / 2f;
				num5 = (float)PlotForm.updnPA.get_Value();
			}
			double num26 = (float)ChartWidth / num10;
			string text3 = ((!(num26 > 10.0)) ? string.Format("Plot width: {0,1:F1} km", num26) : string.Format("Plot width: {0,1:F0} km", num26));
			font = new Font("Times New Roman", 9f, FontStyle.Regular);
			formGraphics.DrawString(text3, font, brush2, (float)ChartWidth - formGraphics.MeasureString(text3, font).Width - 15f, ChartHeight - 18);
			if (ShowScaleGrid)
			{
				float num27 = (float)Settings.Default.Asterplot_GridScale;
				if (num27 < 1f)
				{
					num27 *= 10f;
				}
				else if (num27 >= 10f)
				{
					num27 /= 10f;
				}
				int num28 = 10;
				if (num27 < 4f)
				{
					num28 = 5;
				}
				float num29 = (float)Settings.Default.Asterplot_GridScale * num10;
				int num30 = (int)((float)ChartWidth / num29 / 1.4f);
				double x3 = 0.0;
				double y3 = 0.0;
				double x4 = 0.0;
				double y4 = 0.0;
				for (int i = 0; i < num30 + 1; i++)
				{
					if (i % num28 == 0)
					{
						pen2.DashPattern = new float[2] { 1.8f, 4f };
					}
					else
					{
						pen2.DashPattern = new float[2] { 0.7f, 4f };
					}
					Utilities.RotateXY((float)i * num29, 0.0, GridAngle, out x3, out y3);
					Utilities.RotateXY(ChartWidth, 0.0, GridAngle + 90.0, out x4, out y4);
					formGraphics.DrawLine(pen2, (float)((double)num14 + x3 + x4), (float)((double)num15 + y3 + y4), (float)((double)num14 + x3 - x4), (float)((double)num15 + y3 - y4));
					formGraphics.DrawLine(pen2, (float)((double)num14 - x3 + x4), (float)((double)num15 - y3 + y4), (float)((double)num14 - x3 - x4), (float)((double)num15 - y3 - y4));
					formGraphics.DrawLine(pen2, (float)((double)num14 + y3 + y4), (float)((double)num15 - x3 - x4), (float)((double)num14 + y3 - y4), (float)((double)num15 - x3 + x4));
					formGraphics.DrawLine(pen2, (float)((double)num14 - y3 + y4), (float)((double)num15 + x3 - x4), (float)((double)num14 - y3 - y4), (float)((double)num15 + x3 + x4));
				}
				string text4 = ((!(Settings.Default.Asterplot_GridScale >= 1.0)) ? string.Format("{0,1:F0} m Grid", 1000.0 * Settings.Default.Asterplot_GridScale) : string.Format("{0,1:F0} km Grid", Settings.Default.Asterplot_GridScale));
				font = new Font("Times New Roman", 8f, FontStyle.Regular);
				formGraphics.DrawString(text4, font, brush2, (float)ChartWidth - formGraphics.MeasureString(text4, font).Width - 15f, ChartHeight - 28);
			}
			if (ShowStar && ((StarDia_mas > 0f) & (EventDetails.Parallax > 0.0)))
			{
				float num31 = (float)((double)StarDia_mas / 1000.0 / EventDetails.Parallax * 6378.137);
				float num32 = 50f;
				float num33 = 55f;
				float num34 = num31 * num10;
				if (CenterStarPlotOnPlot)
				{
					num32 = (num33 = num14 - num34 / 2f);
				}
				formGraphics.FillEllipse(gold, num32, num33, num34, num34);
				formGraphics.DrawEllipse(pen4, num32, num33, num34, num34);
				if (StarDia_mas * num10 > 10f)
				{
					formGraphics.FillEllipse(brush2, num32 + num34 / 2f - 1.5f, num33 + num34 / 2f - 1.5f, 3f, 3f);
				}
				string text5 = string.Format("Star {0,1:f1}mas", StarDia_mas);
				formGraphics.DrawString(text5, font4, brush2, num32 + (num34 - formGraphics.MeasureString(text5, font4).Width) / 2f, num33 + 3f + num34);
			}
			if (ShowEllipse)
			{
				float num35 = 1.225f;
				bool @checked = PlotForm.mnuShowEllipseInOutline.get_Checked();
				CosPA = Math.Cos((double)num5 / (180.0 / Math.PI));
				SinPA = Math.Sin((double)num5 / (180.0 / Math.PI));
				double mas;
				if (!((Control)PlotForm.panelDouble).get_Visible() | !PlotForm.optSecondary.get_Checked() | ShowAlign)
				{
					for (int j = 0; j <= 1; j++)
					{
						float num36 = 0f;
						if (j == 1)
						{
							if (!ShowFresnel)
							{
								break;
							}
							if (EventDetails.Parallax > 0.0)
							{
								num36 = num35 * (float)Utilities.FresnelLength_m(8.794143836182533 / EventDetails.Parallax, 600.0, 150.0, out mas) / 1000f;
							}
						}
						for (float num37 = 0f; num37 < 361f; num37 += 1f)
						{
							X1 = (double)(num4 + num36) * Math.Sin((double)num37 / (180.0 / Math.PI));
							Y1 = (double)(num3 + num36) * Math.Cos((double)num37 / (180.0 / Math.PI));
							X2 = num14 + num10 * (float)(X1 * CosPA + Y1 * SinPA + (double)CenterOfEllipse_X - (double)num - (double)num17);
							Y2 = num15 + num10 * (float)((0.0 - X1) * SinPA + Y1 * CosPA - (double)CenterOfEllipse_Y + (double)num2 - (double)num18);
							if (num37 == 0f)
							{
								xAxis0 = X2;
								yAxis0 = Y2;
							}
							else if (num37 == 90f)
							{
								xAxis90 = X2;
								yAxis90 = Y2;
							}
							else if (num37 == 180f)
							{
								xAxis180 = X2;
								yAxis180 = Y2;
							}
							else if (num37 == 270f)
							{
								xAxis270 = X2;
								yAxis270 = Y2;
							}
							if (num37 > 0f)
							{
								if ((j == 0) & (num37 % 8f == 0f || !@checked))
								{
									formGraphics.DrawLine(pen19, xOld, yOld, X2, Y2);
								}
								else if (num37 % 4f == 0f)
								{
									formGraphics.DrawLine(pen22, xOld, yOld, X2, Y2);
								}
							}
							xOld = X2;
							yOld = Y2;
						}
					}
					if (ShowAxes)
					{
						formGraphics.DrawLine(pen18, xAxis0, yAxis0, xAxis180, yAxis180);
						formGraphics.DrawLine(pen18, xAxis90, yAxis90, xAxis270, yAxis270);
					}
				}
				if ((((Control)PlotForm.panelDouble).get_Visible() & !ShowAlign) | EventDetails.AsteroidHasSatellite)
				{
					for (int k = 0; k <= 1; k++)
					{
						float num36 = 0f;
						if (k == 1)
						{
							if (!ShowFresnel)
							{
								break;
							}
							if (EventDetails.Parallax > 0.0)
							{
								num36 = num35 * (float)Utilities.FresnelLength_m(8.794143836182533 / EventDetails.Parallax, 600.0, 150.0, out mas) / 1000f;
							}
						}
						if (PlotForm.optPrimary.get_Checked())
						{
							continue;
						}
						float num38 = num3;
						float num39 = num4;
						double num40 = SinPA;
						double num41 = CosPA;
						if (EventDetails.AsteroidHasSatellite)
						{
							num38 = num6;
							num39 = num7;
							double num42 = num8;
							num40 = (float)Math.Sin(num42 / (180.0 / Math.PI));
							num41 = (float)Math.Cos(num42 / (180.0 / Math.PI));
						}
						for (float num43 = 0f; num43 < 361f; num43 += 1f)
						{
							X1 = (double)(num39 + num36) * Math.Sin((double)num43 / (180.0 / Math.PI));
							Y1 = (double)(num38 + num36) * Math.Cos((double)num43 / (180.0 / Math.PI));
							X2 = num14 + num10 * (float)(X1 * num41 + Y1 * num40 + (double)CenterOf2ndEllipse_X - (double)num - (double)num17);
							Y2 = num15 + num10 * (float)((0.0 - X1) * num40 + Y1 * num41 - (double)CenterOf2ndEllipse_Y + (double)num2 - (double)num18);
							if (num43 == 0f)
							{
								xAxis0 = X2;
								yAxis0 = Y2;
							}
							else if (num43 == 90f)
							{
								xAxis90 = X2;
								yAxis90 = Y2;
							}
							else if (num43 == 180f)
							{
								xAxis180 = X2;
								yAxis180 = Y2;
							}
							else if (num43 == 270f)
							{
								xAxis270 = X2;
								yAxis270 = Y2;
							}
							if (num43 > 0f)
							{
								if ((k == 0) & (num43 % 8f == 0f || !@checked))
								{
									formGraphics.DrawLine(pen20, xOld, yOld, X2, Y2);
								}
								else if (num43 % 4f == 0f)
								{
									formGraphics.DrawLine(pen22, xOld, yOld, X2, Y2);
								}
							}
							xOld = X2;
							yOld = Y2;
						}
						if (ShowAxes)
						{
							formGraphics.DrawLine(pen18, xAxis0, yAxis0, xAxis180, yAxis180);
							formGraphics.DrawLine(pen18, xAxis90, yAxis90, xAxis270, yAxis270);
						}
					}
				}
			}
			if (ShowAstrometry)
			{
				X2 = num14 + num10 * (CenterOfEllipse_X - num - num17 + num21);
				Y2 = num15 + num10 * (0f - CenterOfEllipse_Y + num2 - num18 - num22);
				formGraphics.DrawEllipse(pen16, X2 - 2f, Y2 - 2f, 4f, 4f);
				if (EventDetails.AsteroidHasSatellite)
				{
					float num44 = num14 + num10 * (CenterOfEllipse_X - num - num17);
					float num45 = num15 + num10 * (0f - CenterOfEllipse_Y + num2 - num18);
					formGraphics.DrawLine(pen23, X2, Y2, num44, num45);
					float num46 = num14 + num10 * (CenterOf2ndEllipse_X - num - num17);
					float num47 = num15 + num10 * (0f - CenterOf2ndEllipse_Y + num2 - num18);
					formGraphics.DrawLine(pen23, X2, Y2, num46, num47);
					double num48 = num3 * num4 * (num3 + num4) / 2f;
					double num49 = num6 * num7 * (num6 + num7) / 2f;
					double num50 = num49 / (num48 + num49);
					double num51 = Math.Sqrt(Math.Pow(num46 - num44, 2.0) + Math.Pow(num47 - num45, 2.0));
					double num52 = Math.Sqrt(Math.Pow(X2 - num44, 2.0) + Math.Pow(Y2 - num45, 2.0));
					double num53 = Math.Sqrt(Math.Pow(num46 - X2, 2.0) + Math.Pow(num47 - Y2, 2.0));
					double num54 = (num51 + num52 + num53) / 2.0;
					double num55 = 2.0 / num51 / num52 * Math.Sqrt(num54 * (num54 - num51) * (num54 - num52) * (num54 - num53));
					if (double.IsNaN(num55))
					{
						num55 = 1E-09;
					}
					double num56 = Math.Sqrt(Math.Pow(num44 - num46, 2.0) + Math.Pow(num45 - num47, 2.0)) * num50;
					double num57 = num55 * num52;
					double num58 = num56 - num52 * Math.Sqrt(1.0 - num55 * num55);
					string s = string.Format("{0,1:f1},{1,1:f1}\r\n{2,1:f3},{3,1:f3}", num58 / (double)num10, num57 / (double)num10, num52 / num51, num50);
					formGraphics.DrawString(s, font, brush2, X2 + 5f, Y2 + 5f);
				}
			}
			for (int l = 0; l < EventDetails.Observers.Count; l++)
			{
				if (EventDetails.Observers[l].PlotCode == "x")
				{
					continue;
				}
				xOffset = 0f;
				yOffset = 0f;
				Secondary = false;
				if ("dmGg".Contains(EventDetails.Observers[l].Event_D) | "rmBb".Contains(EventDetails.Observers[l].Event_R))
				{
					Secondary = true;
					if (ShowAlign)
					{
						xOffset = Offset_X_2ndFromPrimary;
						yOffset = Offset_Y_2ndFromPrimary;
					}
				}
				bool flag = true;
				if (PlotForm.optPrimary.get_Checked())
				{
					flag = !Secondary;
				}
				else if (PlotForm.optSecondary.get_Checked())
				{
					flag = Secondary;
				}
				bool flag2 = false;
				bool flag3 = false;
				bool flag4 = false;
				bool flag5 = false;
				if ((EventDetails.Observers[l].Event_D.ToUpper() == "M") | (EventDetails.Observers[l].Event_D == "n"))
				{
					flag2 = true;
				}
				if ((EventDetails.Observers[l].Event_D.ToUpper() == "N") | (EventDetails.Observers[l].Event_R.ToUpper() == "N"))
				{
					flag5 = true;
				}
				if (EventDetails.Observers[l].Event_D == "C")
				{
					flag2 = true;
					flag3 = true;
				}
				if (EventDetails.Observers[l].Event_D == "P")
				{
					flag2 = true;
					flag4 = true;
				}
				bool flag6 = "abcde".Contains(EventDetails.Observers[l].Method);
				bool flag7 = "ab".Contains(EventDetails.Observers[l].Method);
				int Weight = 1;
				UncertAndWeight_NoUncertaintySet(l, out var TimeUncertainty, out Weight);
				float num59;
				float num60 = (num59 = (float)TimeUncertainty);
				if (EventDetails.Observers[l].Accuracy_D_Set)
				{
					num59 = (float)EventDetails.Observers[l].Accuracy_D;
				}
				if (EventDetails.Observers[l].Accuracy_R_Set)
				{
					num60 = (float)EventDetails.Observers[l].Accuracy_R;
				}
				num59 *= num10;
				num60 *= num10;
				if (ShowErrorsForChordLength)
				{
					float num61 = (float)Math.Sqrt(num59 * num59 + num60 * num60);
					num59 = num61 / (1f + num60 / num59);
					num60 = num61 / (1f + num59 / num60);
				}
				bool flag8 = true;
				if (EventDetails.Observers[l].Weight_D_Set & (EventDetails.Observers[l].Weight_D == 0))
				{
					flag8 = false;
				}
				bool flag9 = true;
				if (EventDetails.Observers[l].Weight_R_Set & (EventDetails.Observers[l].Weight_R == 0))
				{
					flag9 = false;
				}
				if (!((flag6 | !ShowVideoOnly) || flag4))
				{
					continue;
				}
				if ((X[l, 0] != 10000.0 && flag) & (flag8 | ShowZeroWeightedObservations))
				{
					CurrentX = num14 - num10 * (float)(X[l, 0] - (double)num19 + (double)num + (double)xOffset);
					CurrentY = num15 + num10 * (float)(Y[l, 0] - (double)num20 + (double)num2 + (double)yOffset);
					if (ShowMarkers | "mM".Contains(EventDetails.Observers[l].Event_D))
					{
						if (flag4)
						{
							if (ShowPredicted)
							{
								formGraphics.DrawLine(pen13, CurrentX - 2f * num9, CurrentY, CurrentX + 2f * num9, CurrentY);
								formGraphics.DrawLine(pen13, CurrentX, CurrentY - 2f * num9, CurrentX, CurrentY + 2f * num9);
								formGraphics.DrawLine(pen13, CurrentX - 2f * num9, CurrentY - 2f * num9, CurrentX + 2f * num9, CurrentY + 2f * num9);
								formGraphics.DrawLine(pen13, CurrentX + 2f * num9, CurrentY - 2f * num9, CurrentX - 2f * num9, CurrentY + 2f * num9);
							}
						}
						else if (!flag2)
						{
							if (flag5 & ShowRings)
							{
								formGraphics.DrawEllipse(pen9, CurrentX - 1.5f * num9, CurrentY - 1.5f * num9, 3f * num9, 3f * num9);
								formGraphics.DrawLine(pen9, CurrentX - 3f * num9, CurrentY, CurrentX + 3f * num9, CurrentY);
								formGraphics.DrawLine(pen9, CurrentX, CurrentY - 3f * num9, CurrentX, CurrentY + 3f * num9);
							}
							else if (flag7)
							{
								formGraphics.DrawRectangle(pen11, CurrentX - num9, CurrentY - num9, 2f * num9, 2f * num9);
							}
							else if (flag6)
							{
								formGraphics.DrawEllipse(pen11, CurrentX - num9, CurrentY - num9, 2f * num9, 2f * num9);
							}
							else
							{
								formGraphics.DrawLine(pen6, CurrentX - 1.5f * num9, CurrentY, CurrentX + 1.5f * num9, CurrentY);
								formGraphics.DrawLine(pen6, CurrentX, CurrentY - 1.5f * num9, CurrentX, CurrentY + 1.5f * num9);
							}
						}
						else if (flag2 & ShowMiss)
						{
							formGraphics.DrawLine(pen13, CurrentX - num9, CurrentY - num9, CurrentX + num9, CurrentY + num9);
							formGraphics.DrawLine(pen13, CurrentX - num9, CurrentY + num9, CurrentX + num9, CurrentY - num9);
						}
						else if (flag3 & ShowCloud)
						{
							formGraphics.DrawLine(pen13, CurrentX - num9, CurrentY - num9, CurrentX + num9, CurrentY + num9);
							formGraphics.DrawLine(pen13, CurrentX - num9, CurrentY + num9, CurrentX + num9, CurrentY - num9);
						}
					}
					Asteroid_OBS_Tags asteroid_OBS_Tags = new Asteroid_OBS_Tags();
					asteroid_OBS_Tags.X = CurrentX;
					asteroid_OBS_Tags.Y = CurrentY;
					if (T[l, 2] == 0.0)
					{
						asteroid_OBS_Tags.dX = (float)((X[l, 3] - X[l, 1]) / 10.0 / 60.0);
						asteroid_OBS_Tags.dY = (float)((Y[l, 3] - Y[l, 1]) / 10.0 / 60.0);
					}
					else
					{
						asteroid_OBS_Tags.dX = (float)((X[l, 0] - X[l, 2]) / 10.0 / 60.0);
						asteroid_OBS_Tags.dY = (float)((Y[l, 0] - Y[l, 2]) / 10.0 / 60.0);
					}
					asteroid_OBS_Tags.RateMilliArcSec = (float)(Math.Sqrt(asteroid_OBS_Tags.dX * asteroid_OBS_Tags.dX + asteroid_OBS_Tags.dY * asteroid_OBS_Tags.dY) / 6378.137 * EventDetails.Parallax * 1000.0);
					asteroid_OBS_Tags.EventType = 1;
					if (flag4)
					{
						asteroid_OBS_Tags.Tag = "Predicted : ";
					}
					else
					{
						asteroid_OBS_Tags.Tag = EventDetails.Observers[l].SeqNumber.ToString().PadLeft(2, '_') + " " + EventDetails.Observers[l].ObserversAll + " " + EventDetails.Observers[l].Formatted_T_Disappear;
					}
					Tags.Add(asteroid_OBS_Tags);
					if (ShowIDs)
					{
						if (flag8)
						{
							if (!(flag3 & !ShowCloud) & !(flag4 & !ShowPredicted))
							{
								formGraphics.DrawString(LabelTag[l], font2, brush2, CurrentX, CurrentY);
							}
						}
						else if (!(flag3 & !ShowCloud) & !(flag4 & !ShowPredicted))
						{
							formGraphics.DrawString(LabelTag[l], font3, brush2, CurrentX, CurrentY);
						}
					}
				}
				if ((X[l, 1] != 10000.0 && flag) & (flag9 | ShowZeroWeightedObservations))
				{
					CurrentX = num14 - num10 * (float)(X[l, 1] - (double)num19 + (double)num + (double)xOffset);
					CurrentY = num15 + num10 * (float)(Y[l, 1] - (double)num20 + (double)num2 + (double)yOffset);
					if (ShowMarkers && !flag2)
					{
						if (flag5 & ShowRings)
						{
							formGraphics.DrawEllipse(pen10, CurrentX - 1.5f * num9, CurrentY - 1.5f * num9, 3f * num9, 3f * num9);
							formGraphics.DrawLine(pen10, CurrentX - 2.5f * num9, CurrentY, CurrentX + 2.5f * num9, CurrentY);
							formGraphics.DrawLine(pen10, CurrentX, CurrentY - 2.5f * num9, CurrentX, CurrentY + 2.5f * num9);
						}
						else if (flag7)
						{
							formGraphics.DrawRectangle(pen12, CurrentX - num9, CurrentY - num9, 2f * num9, 2f * num9);
						}
						else if (flag6)
						{
							formGraphics.DrawEllipse(pen12, CurrentX - num9, CurrentY - num9, 2f * num9, 2f * num9);
						}
						else
						{
							formGraphics.DrawLine(pen8, CurrentX - 1.5f * num9, CurrentY, CurrentX + 1.5f * num9, CurrentY);
							formGraphics.DrawLine(pen8, CurrentX, CurrentY - 1.5f * num9, CurrentX, CurrentY + 1.5f * num9);
						}
					}
					Asteroid_OBS_Tags asteroid_OBS_Tags = new Asteroid_OBS_Tags();
					asteroid_OBS_Tags.X = CurrentX;
					asteroid_OBS_Tags.Y = CurrentY;
					if (T[l, 2] == 0.0)
					{
						asteroid_OBS_Tags.dX = (float)((X[l, 3] - X[l, 1]) / 10.0 / 60.0);
						asteroid_OBS_Tags.dY = (float)((Y[l, 3] - Y[l, 1]) / 10.0 / 60.0);
					}
					else
					{
						asteroid_OBS_Tags.dX = (float)((X[l, 0] - X[l, 2]) / 10.0 / 60.0);
						asteroid_OBS_Tags.dY = (float)((Y[l, 0] - Y[l, 2]) / 10.0 / 60.0);
					}
					asteroid_OBS_Tags.RateMilliArcSec = (float)(Math.Sqrt(asteroid_OBS_Tags.dX * asteroid_OBS_Tags.dX + asteroid_OBS_Tags.dY * asteroid_OBS_Tags.dY) / 6378.137 * EventDetails.Parallax * 1000.0);
					asteroid_OBS_Tags.EventType = 2;
					if (flag4)
					{
						asteroid_OBS_Tags.Tag = "Predicted : ";
					}
					else
					{
						asteroid_OBS_Tags.Tag = EventDetails.Observers[l].SeqNumber.ToString().PadLeft(2, '_') + " " + EventDetails.Observers[l].ObserversAll + " " + EventDetails.Observers[l].Formatted_T_Reappear;
					}
					Tags.Add(asteroid_OBS_Tags);
					if (ShowIDs)
					{
						if (flag9)
						{
							if (!(flag3 & !ShowCloud) & !(flag4 & !ShowPredicted))
							{
								formGraphics.DrawString(LabelTag[l], font2, brush2, CurrentX, CurrentY);
							}
						}
						else if (!(flag3 & !ShowCloud) & !(flag4 & !ShowPredicted))
						{
							formGraphics.DrawString(LabelTag[l], font3, brush2, CurrentX, CurrentY);
						}
					}
				}
				float num62 = num14 - num10 * (float)(X[l, 0] - (double)num19 + (double)xOffset + (double)num);
				float num63 = num15 + num10 * (float)(Y[l, 0] - (double)num20 + (double)yOffset + (double)num2);
				float num64 = num14 - num10 * (float)(X[l, 1] - (double)num19 + (double)xOffset + (double)num);
				float num65 = num15 + num10 * (float)(Y[l, 1] - (double)num20 + (double)yOffset + (double)num2);
				float OuterX = num14 - num10 * (float)(X[l, 2] - (double)num19 + (double)xOffset + (double)num);
				float OuterY = num15 + num10 * (float)(Y[l, 2] - (double)num20 + (double)yOffset + (double)num2);
				float OuterX2 = num14 - num10 * (float)(X[l, 3] - (double)num19 + (double)xOffset + (double)num);
				float OuterY2 = num15 + num10 * (float)(Y[l, 3] - (double)num20 + (double)yOffset + (double)num2);
				_ = 51;
				if (((ShowOcculted | ShowVisible | ShowRings) & (!flag5 | ((flag5 & ShowRings) && !flag2))) && !flag4 && !flag2)
				{
					Pen pen26 = pen3;
					if (ShowPathsIncolour)
					{
						pen26 = PenChords[l % 100];
					}
					pen26.Width = LineThicknessPath;
					if (EventLineToHighlight == l)
					{
						pen26 = pen24;
						pen26.Width = LineThicknessPath + 1f;
					}
					if (flag5)
					{
						pen26.DashPattern = new float[2] { 2f, 4f };
						if (ShowVisible)
						{
							pen26.Width = LineThicknessPath + 1.5f;
						}
						else
						{
							pen26.Width = LineThicknessOther + 1f;
						}
					}
					if (!ShowVisible && !flag5 && !flag2)
					{
						if (((X[l, 0] != 10000.0) & (X[l, 1] != 10000.0)) && flag && ((flag8 && flag9) | ShowZeroWeightedObservations) && (ShowOcculted | (flag5 & ShowRings)))
						{
							formGraphics.DrawLine(pen26, num62, num63, num64, num65);
						}
					}
					else if (!(flag3 & !ShowCloud) & !(flag4 & !ShowPredicted))
					{
						if ((((X[l, 0] != 10000.0) & (X[l, 1] != 10000.0)) && flag) & ((flag8 || flag9) | ShowZeroWeightedObservations))
						{
							if (!flag5 | ((flag5 & ShowRings) && !flag2))
							{
								if (l > 0 && ((X[l - 1, 1] != 10000.0) & (EventDetails.Observers[l - 1].PlotCode != "x") & (EventDetails.Observers[l - 1].Event_D != "P")) && ((EventDetails.Observers[l].Longitude == EventDetails.Observers[l - 1].Longitude) & (EventDetails.Observers[l].Latitude == EventDetails.Observers[l - 1].Latitude) & (EventDetails.Observers[l].Observer1 == EventDetails.Observers[l - 1].Observer1) & !ShowAlign))
								{
									if (EventDetails.Observers[l].T_Disappear > EventDetails.Observers[l - 1].T_Reappear)
									{
										OuterX = num14 - num10 * (float)(X[l - 1, 1] - (double)num19 + (double)xOffset + (double)num);
										OuterY = num15 + num10 * (float)(Y[l - 1, 1] - (double)num20 + (double)yOffset + (double)num2);
									}
									else if (EventDetails.Observers[l].T_Reappear < EventDetails.Observers[l - 1].T_Disappear)
									{
										OuterX2 = num14 - num10 * (float)(X[l - 1, 0] - (double)num19 + (double)xOffset + (double)EventPredictionOffsetX + (double)num);
										OuterY2 = num15 + num10 * (float)(Y[l - 1, 0] - (double)num20 + (double)yOffset + (double)EventPredictionOffsetY + (double)num2);
									}
								}
								if (l < EventDetails.Observers.Count - 1 && ((X[l + 1, 0] != 10000.0) & (EventDetails.Observers[l + 1].PlotCode != "x") & (EventDetails.Observers[l + 1].Event_D != "P")) && ((EventDetails.Observers[l].Longitude == EventDetails.Observers[l + 1].Longitude) & (EventDetails.Observers[l].Latitude == EventDetails.Observers[l + 1].Latitude) & (EventDetails.Observers[l].Observer1 == EventDetails.Observers[l + 1].Observer1) & !ShowAlign))
								{
									if (EventDetails.Observers[l].T_Reappear < EventDetails.Observers[l + 1].T_Disappear)
									{
										OuterX2 = num14 - num10 * (float)(X[l + 1, 0] - (double)num19 + (double)xOffset + (double)EventPredictionOffsetX + (double)num);
										OuterY2 = num15 + num10 * (float)(Y[l + 1, 0] - (double)num20 + (double)yOffset + (double)EventPredictionOffsetY + (double)num2);
									}
									else if (EventDetails.Observers[l].T_Disappear > EventDetails.Observers[l + 1].T_Reappear)
									{
										OuterX = num14 - num10 * (float)(X[l + 1, 1] - (double)num19 + (double)xOffset + (double)num);
										OuterY = num15 + num10 * (float)(Y[l + 1, 1] - (double)num20 + (double)yOffset + (double)num2);
									}
								}
								NormaliseXYto1000000(num62, num63, ref OuterX, ref OuterY);
								NormaliseXYto1000000(num64, num65, ref OuterX2, ref OuterY2);
								if (flag3)
								{
									formGraphics.DrawLine(pen15, OuterX, OuterY, num62, num63);
									formGraphics.DrawLine(pen15, num64, num65, OuterX2, OuterY2);
								}
								else
								{
									if (flag8 | ShowZeroWeightedObservations)
									{
										formGraphics.DrawLine(pen26, OuterX, OuterY, num62, num63);
									}
									if (flag9 | ShowZeroWeightedObservations)
									{
										formGraphics.DrawLine(pen26, num64, num65, OuterX2, OuterY2);
									}
								}
							}
						}
						else if ((X[l, 0] != 10000.0 && flag) & (flag8 | ShowZeroWeightedObservations))
						{
							if (flag3)
							{
								formGraphics.DrawLine(pen15, num62, num63, OuterX, OuterY);
							}
							else
							{
								if (l < EventDetails.Observers.Count - 1 && ((X[l + 1, 0] != 10000.0) & (EventDetails.Observers[l + 1].PlotCode != "x")) && ((EventDetails.Observers[l].Longitude == EventDetails.Observers[l + 1].Longitude) & (EventDetails.Observers[l].Latitude == EventDetails.Observers[l + 1].Latitude) & !ShowAlign))
								{
									if (EventDetails.Observers[l].T_Reappear < EventDetails.Observers[l + 1].T_Disappear)
									{
										OuterX2 = num14 - num10 * (float)(X[l + 1, 0] - (double)num19 + (double)xOffset + (double)EventPredictionOffsetX);
										OuterY2 = num15 + num10 * (float)(Y[l + 1, 0] - (double)num20 + (double)yOffset + (double)EventPredictionOffsetY);
									}
									else if (EventDetails.Observers[l].T_Disappear > EventDetails.Observers[l + 1].T_Reappear)
									{
										OuterX = num14 - num10 * (float)(X[l + 1, 1] - (double)num19 + (double)xOffset);
										OuterY = num15 + num10 * (float)(Y[l + 1, 1] - (double)num20 + (double)yOffset);
									}
								}
								NormaliseXYto1000000(num62, num63, ref OuterX, ref OuterY);
								formGraphics.DrawLine(pen26, num62, num63, OuterX, OuterY);
							}
						}
						else if ((X[l, 1] != 10000.0 && flag) & (flag9 | ShowZeroWeightedObservations))
						{
							if (flag3)
							{
								formGraphics.DrawLine(pen15, num64, num65, OuterX2, OuterY2);
							}
							else
							{
								if (l < EventDetails.Observers.Count - 1 && ((X[l + 1, 0] != 10000.0) & (EventDetails.Observers[l + 1].PlotCode != "x")) && ((EventDetails.Observers[l].Longitude == EventDetails.Observers[l + 1].Longitude) & (EventDetails.Observers[l].Latitude == EventDetails.Observers[l + 1].Latitude) & !ShowAlign))
								{
									if (EventDetails.Observers[l].T_Reappear < EventDetails.Observers[l + 1].T_Disappear)
									{
										OuterX2 = num14 - num10 * (float)(X[l + 1, 0] - (double)num19 + (double)xOffset + (double)EventPredictionOffsetX);
										OuterY2 = num15 + num10 * (float)(Y[l + 1, 0] - (double)num20 + (double)yOffset + (double)EventPredictionOffsetY);
									}
									else if (EventDetails.Observers[l].T_Disappear > EventDetails.Observers[l + 1].T_Reappear)
									{
										OuterX = num14 - num10 * (float)(X[l + 1, 1] - (double)num19 + (double)xOffset);
										OuterY = num15 + num10 * (float)(Y[l + 1, 1] - (double)num20 + (double)yOffset);
									}
								}
								NormaliseXYto1000000(num64, num65, ref OuterX2, ref OuterY2);
								formGraphics.DrawLine(pen26, num64, num65, OuterX2, OuterY2);
							}
						}
					}
				}
				if (ShowPredicted && flag4)
				{
					int num66 = 600;
					int num67 = 1;
					if (EventLineToHighlight == l)
					{
						num67 = 3;
					}
					for (int m = 0; m < num66; m++)
					{
						float num68 = num62 + (OuterX - num62) / (float)num66 * (float)m;
						float num69 = num63 + (OuterY - num63) / (float)num66 * (float)m;
						formGraphics.DrawEllipse(pen14, num68 - (float)num67, num69 - (float)num67, 2 * num67, 2 * num67);
						num68 = num64 + (OuterX2 - num64) / (float)num66 * (float)m;
						num69 = num65 + (OuterY2 - num65) / (float)num66 * (float)m;
						formGraphics.DrawEllipse(pen14, num68 - (float)num67, num69 - (float)num67, 2 * num67, 2 * num67);
					}
				}
				if ((flag2 & ShowMiss) && !flag4)
				{
					Pen pen26 = pen17;
					if (ShowPathsIncolour)
					{
						pen26 = PenChords[l % 100];
					}
					if (EventLineToHighlight == l)
					{
						pen26 = pen24;
						pen26.Width = LineThicknessOther + 1f;
					}
					if (flag5 & ShowRings)
					{
						pen26.Width = LineThicknessOther;
						pen26.DashPattern = new float[2] { 2f, 4f };
						NormaliseXYto1000000(num62, num63, ref OuterX, ref OuterY);
						NormaliseXYto1000000(num64, num65, ref OuterX2, ref OuterY2);
						if ((X[l, 0] != 10000.0 && flag) & (flag8 | ShowZeroWeightedObservations))
						{
							formGraphics.DrawLine(pen26, OuterX, OuterY, num62, num63);
						}
						if ((X[l, 1] != 10000.0 && flag) & (flag9 | ShowZeroWeightedObservations))
						{
							formGraphics.DrawLine(pen26, num64, num65, OuterX2, OuterY2);
						}
					}
					else if (ShowVisible)
					{
						pen26.Width = LineThicknessPath;
						if (ShowMissAsDashed)
						{
							pen26.DashPattern = new float[2] { 30f, 3f };
						}
						NormaliseXYto1000000(num62, num63, ref OuterX, ref OuterY);
						NormaliseXYto1000000(num64, num65, ref OuterX2, ref OuterY2);
						if ((X[l, 0] != 10000.0 && flag) & (flag8 | ShowZeroWeightedObservations))
						{
							formGraphics.DrawLine(pen26, OuterX, OuterY, num62, num63);
						}
						if ((X[l, 1] != 10000.0 && flag) & (flag9 | ShowZeroWeightedObservations))
						{
							formGraphics.DrawLine(pen26, num64, num65, OuterX2, OuterY2);
						}
					}
				}
				if (!ShowErrors || flag2)
				{
					continue;
				}
				double num70 = 0.0;
				float num71 = 0f;
				float num72 = 0f;
				float num75 = (pen6.Width = (pen8.Width = 2f));
				if (((X[l, 0] != 10000.0 && flag) & (flag8 | ShowZeroWeightedObservations)) && !flag5)
				{
					formGraphics.DrawLine(pen5, num62 - num59 * ErrorX_motion[l, 0], num63 + num59 * ErrorY_Motion[l, 0], num62 + num59 * ErrorX_motion[l, 0], num63 - num59 * ErrorY_Motion[l, 0]);
					num70 = Math.Atan2(ErrorY_Motion[l, 0], ErrorX_motion[l, 0]) + Math.PI / 2.0;
					num72 = (float)(3.0 * Math.Sin(num70));
					num71 = (float)(3.0 * Math.Cos(num70));
					if (ShowErrorsForChordLength)
					{
						formGraphics.DrawLine(pen5, num62 - num59 * ErrorX_motion[l, 0] + num71, num63 + num59 * ErrorY_Motion[l, 0] - num72, num62 - num59 * ErrorX_motion[l, 0] - num71, num63 + num59 * ErrorY_Motion[l, 0] + num72);
						formGraphics.DrawLine(pen5, num62 + num59 * ErrorX_motion[l, 0] + num71, num63 - num59 * ErrorY_Motion[l, 0] - num72, num62 + num59 * ErrorX_motion[l, 0] - num71, num63 - num59 * ErrorY_Motion[l, 0] + num72);
					}
					else
					{
						formGraphics.DrawLine(pen6, num62 - num59 * ErrorX_motion[l, 0] + num71, num63 + num59 * ErrorY_Motion[l, 0] - num72, num62 - num59 * ErrorX_motion[l, 0] - num71, num63 + num59 * ErrorY_Motion[l, 0] + num72);
						formGraphics.DrawLine(pen6, num62 + num59 * ErrorX_motion[l, 0] + num71, num63 - num59 * ErrorY_Motion[l, 0] - num72, num62 + num59 * ErrorX_motion[l, 0] - num71, num63 - num59 * ErrorY_Motion[l, 0] + num72);
					}
				}
				if (((X[l, 1] != 10000.0 && flag) & (flag9 | ShowZeroWeightedObservations)) && !flag5)
				{
					formGraphics.DrawLine(pen7, num64 - num60 * ErrorX_motion[l, 1], num65 + num60 * ErrorY_Motion[l, 1], num64 + num60 * ErrorX_motion[l, 1], num65 - num60 * ErrorY_Motion[l, 1]);
					num70 = Math.Atan2(ErrorY_Motion[l, 1], ErrorX_motion[l, 1]);
					num70 = Math.Atan2(ErrorY_Motion[l, 1], ErrorX_motion[l, 1]) + Math.PI / 2.0;
					num72 = (float)(3.0 * Math.Sin(num70));
					num71 = (float)(3.0 * Math.Cos(num70));
					if (ShowErrorsForChordLength)
					{
						formGraphics.DrawLine(pen7, num64 - num60 * ErrorX_motion[l, 1] + num71, num65 + num60 * ErrorY_Motion[l, 1] - num72, num64 - num60 * ErrorX_motion[l, 1] - num71, num65 + num60 * ErrorY_Motion[l, 1] + num72);
						formGraphics.DrawLine(pen7, num64 + num60 * ErrorX_motion[l, 1] + num71, num65 - num60 * ErrorY_Motion[l, 1] - num72, num64 + num60 * ErrorX_motion[l, 1] - num71, num65 - num60 * ErrorY_Motion[l, 1] + num72);
					}
					else
					{
						formGraphics.DrawLine(pen8, num64 - num60 * ErrorX_motion[l, 1] + num71, num65 + num60 * ErrorY_Motion[l, 1] - num72, num64 - num60 * ErrorX_motion[l, 1] - num71, num65 + num60 * ErrorY_Motion[l, 1] + num72);
						formGraphics.DrawLine(pen8, num64 + num60 * ErrorX_motion[l, 1] + num71, num65 - num60 * ErrorY_Motion[l, 1] - num72, num64 + num60 * ErrorX_motion[l, 1] - num71, num65 - num60 * ErrorY_Motion[l, 1] + num72);
					}
				}
				num75 = (pen6.Width = (pen8.Width = 1f));
			}
			Tags.Sort();
			if (((Control)PlotForm.panelLimbFit).get_Visible())
			{
				ArrayList arrayList = new ArrayList();
				for (int l = 0; l < EventDetails.Observers.Count; l++)
				{
					bool flag = true;
					if (PlotForm.optPrimary.get_Checked())
					{
						flag = !Secondary;
					}
					else if (PlotForm.optSecondary.get_Checked())
					{
						flag = Secondary;
					}
					bool flag8 = true;
					if (EventDetails.Observers[l].Weight_D_Set & (EventDetails.Observers[l].Weight_D == 0))
					{
						flag8 = false;
					}
					if (((EventDetails.Observers[l].Event_D.ToUpper() == "D") & (X[l, 0] != 10000.0)) && flag && flag8)
					{
						CurrentX = num14 - num10 * (float)(X[l, 0] - (double)num19 + (double)xOffset);
						CurrentY = num15 + num10 * (float)(Y[l, 0] - (double)num20 + (double)yOffset);
						arrayList.Add(new PointF(CurrentX, CurrentY));
					}
				}
				for (int l = EventDetails.Observers.Count - 1; l >= 0; l--)
				{
					bool flag = true;
					if (PlotForm.optPrimary.get_Checked())
					{
						flag = !Secondary;
					}
					else if (PlotForm.optSecondary.get_Checked())
					{
						flag = Secondary;
					}
					bool flag9 = true;
					if (EventDetails.Observers[l].Weight_R_Set & (EventDetails.Observers[l].Weight_R == 0))
					{
						flag9 = false;
					}
					if (((EventDetails.Observers[l].Event_R.ToUpper() == "R") & (X[l, 1] != 10000.0)) && flag && flag9)
					{
						CurrentX = num14 - num10 * (float)(X[l, 1] - (double)num19 + (double)xOffset);
						CurrentY = num15 + num10 * (float)(Y[l, 1] - (double)num20 + (double)yOffset);
						arrayList.Add(new PointF(CurrentX, CurrentY));
					}
				}
				PointF[] array = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
				if (array.GetUpperBound(0) > 2)
				{
					formGraphics.DrawClosedCurve(pen21, array, (float)PlotForm.trackLimbFit.get_Value() / 100f, FillMode.Winding);
				}
			}
			if (ShowAlign & EventDetails.StarIsDouble)
			{
				float num78 = num14 + num10 * (CenterOfEllipse_X - num17);
				float num79 = num15 + num10 * (0f - CenterOfEllipse_Y - num18);
				formGraphics.FillEllipse(brush4, num78 - 8f, num79 - 8f, 16f, 16f);
				formGraphics.FillEllipse(brush, num78 - num10 * Offset_X_2ndFromPrimary - 5f, num79 + num10 * Offset_Y_2ndFromPrimary - 5f, 10f, 10f);
				formGraphics.FillEllipse(brush4, num78 - num10 * Offset_X_2ndFromPrimary - 4f, num79 + num10 * Offset_Y_2ndFromPrimary - 4f, 8f, 8f);
				formGraphics.DrawLine(pen17, num78, num79, num78 - num10 * Offset_X_2ndFromPrimary, num79 + num10 * Offset_Y_2ndFromPrimary);
			}
			if (ShowSatelliteOrbits && KnownBinaryAsteroidElements_RecNums.Count > 0)
			{
				ArrayList arrayList2 = new ArrayList();
				for (int n = 0; n < KnownBinaryAsteroidElements_RecNums.Count; n++)
				{
					if (!(BinaryAsteroids.BinElements[KnownBinaryAsteroidElements_RecNums[n]].A < 1.0) && !((BinaryAsteroids.BinElements[KnownBinaryAsteroidElements_RecNums[n]].I == 0.0) & (BinaryAsteroids.BinElements[KnownBinaryAsteroidElements_RecNums[n]].Node == 0.0)))
					{
						double Planetocentric_Latitude_deg;
						double PAPole_deg;
						if (BinaryAsteroids.BinElements[KnownBinaryAsteroidElements_RecNums[n]].ReferenceFrame == 1)
						{
							Utilities.PoleOrientationEcliptic(BinaryAsteroids.BinElements[KnownBinaryAsteroidElements_RecNums[n]].I, BinaryAsteroids.BinElements[KnownBinaryAsteroidElements_RecNums[n]].Node, EventDetails.RA_Star_2000, EventDetails.Dec_Star_2000, out Planetocentric_Latitude_deg, out PAPole_deg);
						}
						else
						{
							Utilities.PoleOrientation(90.0 - BinaryAsteroids.BinElements[KnownBinaryAsteroidElements_RecNums[n]].I, BinaryAsteroids.BinElements[KnownBinaryAsteroidElements_RecNums[n]].Node - 90.0, EventDetails.RA_Star_2000, EventDetails.Dec_Star_2000, out Planetocentric_Latitude_deg, out PAPole_deg);
						}
						Planetocentric_Latitude_deg /= 180.0 / Math.PI;
						PAPole_deg /= 180.0 / Math.PI;
						arrayList2.Clear();
						for (int num80 = 0; num80 <= 364; num80 += 4)
						{
							float num81 = (float)BinaryAsteroids.BinElements[KnownBinaryAsteroidElements_RecNums[n]].A;
							float num82 = (float)((double)num81 * Math.Cos((double)num80 / (180.0 / Math.PI)));
							float num83 = (float)((double)num81 * Math.Sin((double)num80 / (180.0 / Math.PI)) * Math.Sin(Planetocentric_Latitude_deg));
							Math.Sqrt(num82 * num82 + num83 * num83);
							float num84 = (float)((double)num82 * Math.Cos(PAPole_deg) + (double)num83 * Math.Sin(PAPole_deg));
							float num85 = (float)((double)(0f - num83) * Math.Cos(PAPole_deg) + (double)num82 * Math.Sin(PAPole_deg));
							CurrentX = num14 - num10 * (num84 - CenterOfEllipse_X + num + num17);
							CurrentY = num15 + num10 * (num85 - CenterOfEllipse_Y + num2 - num18);
							arrayList2.Add(new PointF(CurrentX, CurrentY));
						}
						if (arrayList2.Count > 1)
						{
							PointF[] points = (PointF[])arrayList2.ToArray(arrayList2[0]!.GetType());
							formGraphics.DrawLines(pen25, points);
						}
					}
				}
			}
			formGraphics.FillRectangle(brush, 7f, num15 - 7f, 18f, 22f);
			formGraphics.DrawString("E", font5, brush2, 5f, num15 - 9f);
			if (!ShowTitle)
			{
				formGraphics.FillRectangle(brush, num14 - 9f, 12f, 18f, 22f);
				formGraphics.DrawString("N", font5, brush2, num14 - 9f, 12f);
			}
			else
			{
				formGraphics.FillRectangle(brush, num14 - 5f, 37f, 18f, 22f);
				formGraphics.DrawString("N", font5, brush2, num14 - 9f, 34f);
			}
			if (ShowSolution)
			{
				string text6 = AllEvents.DecimalPlaces_String_InSolution_Output(EventDetails.Y_Dia);
				StringBuilder stringBuilder = new StringBuilder();
				stringBuilder.Append(PlotLabel);
				stringBuilder.AppendFormat(" {0:F" + text6 + "} ", EventDetails.X_Dia);
				if (EventDetails.Sdev_Major_Set)
				{
					stringBuilder.AppendFormat("±{0:F" + text6 + "} ", EventDetails.Sdev_Major);
				}
				if (!EventDetails.Solve_Circular)
				{
					stringBuilder.AppendFormat("x {0:F" + text6 + "} ", EventDetails.Y_Dia);
					if (EventDetails.Sdev_Minor_Set)
					{
						stringBuilder.AppendFormat("±{0:F" + text6 + "} ", EventDetails.Sdev_Minor);
					}
				}
				stringBuilder.AppendFormat("km");
				if (!EventDetails.Solve_Circular)
				{
					stringBuilder.AppendFormat(", PA {0:F1}° ", EventDetails.PA_Ellipse);
					if (EventDetails.Sdev_PA_Ellipse_Set)
					{
						stringBuilder.AppendFormat("±{0:F1}° ", EventDetails.Sdev_PA_Ellipse);
					}
				}
				formGraphics.DrawString(stringBuilder.ToString(), font4, brush2, 2f, 2f);
				StringBuilder stringBuilder2 = new StringBuilder();
				stringBuilder2.Append("Geocentric  X ");
				stringBuilder2.AppendFormat(" {0:F" + text6 + "} ", EventDetails.X_Geo_atEvent);
				if (EventDetails.Sdev_X_Set)
				{
					stringBuilder2.AppendFormat("±{0:F" + text6 + "} ", EventDetails.Sdev_X);
				}
				stringBuilder2.AppendFormat(" Y {0:F" + text6 + "} ", EventDetails.Y_Geo_atEvent);
				if (EventDetails.Sdev_Y_Set)
				{
					stringBuilder2.AppendFormat("±{0:F" + text6 + "} ", EventDetails.Sdev_Y);
				}
				stringBuilder2.Append("km");
				formGraphics.DrawString(stringBuilder2.ToString(), font4, brush2, 2f, 18f);
				int currentlySelectedComponent = PlotForm.CurrentlySelectedComponent;
				if (EventDetails.StarIsDouble | EventDetails.AsteroidHasSatellite)
				{
					StringBuilder stringBuilder3 = new StringBuilder();
					if (EventDetails.AsteroidHasSatellite)
					{
						stringBuilder3.Append("Sat: " + EventDetails.Satellites[SelectedSatellite].CompanionIAUname + " ");
						stringBuilder3.AppendFormat("{0:F1} x {1:f1}km, PA {2:f1}°", EventDetails.Satellites[SelectedSatellite].MajorAxisSatellite, EventDetails.Satellites[SelectedSatellite].MinorAxisSatellite, EventDetails.Satellites[SelectedSatellite].PAAxisSatellite);
						stringBuilder3.AppendFormat("; Sep {0:F4}", EventDetails.Satellites[SelectedSatellite].SatelliteSeparation / 1000.0);
						stringBuilder3.AppendFormat("\" at PA {0:F1}° ", EventDetails.Satellites[SelectedSatellite].SatellitePA_Apparent);
						formGraphics.DrawString(stringBuilder3.ToString(), font4, brush2, 2f, 34f);
					}
					else
					{
						stringBuilder3.Append("Double : Sep ");
						try
						{
							stringBuilder3.AppendFormat(" {0:F4}", EventDetails.Doubles[currentlySelectedComponent].Sep_Companion / 1000.0);
							if (EventDetails.Sdev_Sep_Set)
							{
								stringBuilder3.AppendFormat(" ±{0:F4}", EventDetails.Doubles[currentlySelectedComponent].Sdev_Sep_Companion / 1000.0);
							}
							stringBuilder3.AppendFormat("\", PA {0:F1}° ", EventDetails.Doubles[currentlySelectedComponent].PA_Companion);
							if (EventDetails.Sdev_PA_Star_Set)
							{
								stringBuilder3.AppendFormat("±{0:F1}°", EventDetails.Doubles[currentlySelectedComponent].Sdev_PA_Companion);
							}
						}
						catch
						{
						}
						formGraphics.DrawString(stringBuilder3.ToString(), font4, brush2, 2f, 34f);
					}
				}
			}
			else if (ShowTitle)
			{
				StringBuilder stringBuilder4 = new StringBuilder();
				stringBuilder4.Append(string.Format("({0,1}) {1}", EventDetails.AsteroidNo, EventDetails.AsteroidID) + ",   ");
				stringBuilder4.Append(EventDetails.Year + " " + Utilities.ShortMonths[EventDetails.Month] + " " + EventDetails.Day);
				if (ShowTitleExtra)
				{
					stringBuilder4.Append(",   " + ((ToolStripItem)PlotForm.toolStrip_txtExtra).get_Text());
				}
				float width = formGraphics.MeasureString(stringBuilder4.ToString(), font5).Width;
				formGraphics.FillRectangle(brush, ((float)ChartWidth - width) / 2f, 4f, width + 4f, 28f);
				formGraphics.DrawString(stringBuilder4.ToString(), font5, brush3, ((float)ChartWidth - width) / 2f, 2f);
			}
			if (ShowScaleKM)
			{
				float num86 = 1f;
				float num87 = num3;
				if (num4 > num3)
				{
					num87 = num4;
				}
				double num88 = 0.0;
				double num89 = 0.0;
				double num90 = 1.0;
				num86 = ((num87 > 1000f) ? ((float)(1000 * (int)(num87 / 1000f))) : ((num87 > 100f) ? ((float)(100 * (int)(num87 / 100f))) : ((num87 > 10f) ? ((float)(10 * (int)(num87 / 10f))) : ((!(num87 > 1f)) ? 0.25f : ((float)(int)num87)))));
				X1 = num14 - num10 * num86;
				X2 = num14 + num10 * num86;
				Y1 = ChartHeight - 20;
				pen2.DashPattern = new float[2] { 4f, 2f };
				formGraphics.FillRectangle(brush, (float)X1 - 2f, (float)Y1 - 3f, (float)((double)X2 - X1 + 4.0), 6f);
				formGraphics.FillRectangle(brush, (float)X1 - 3f, (float)Y1 - 12f, 6f, 12f);
				formGraphics.FillRectangle(brush, X2 - 3f, (float)Y1 - 12f, 6f, 12f);
				formGraphics.DrawLine(pen2, (float)X1, (float)Y1, X2, (float)Y1);
				formGraphics.DrawLine(pen2, (float)X1, (float)Y1, (float)X1, (float)Y1 - 10f);
				formGraphics.DrawLine(pen2, X2, (float)Y1, X2, (float)Y1 - 10f);
				num88 = (num89 = (num90 = 0.0));
				for (int num91 = 0; num91 < EventDetails.Observers.Count; num91++)
				{
					if (X[num91, 0] != X[num91, 2])
					{
						num88 += Math.Abs((X[num91, 0] - X[num91, 2]) / 10.0 / 60.0);
						num89 += Math.Abs((Y[num91, 0] - Y[num91, 2]) / 10.0 / 60.0);
						num90 += 1.0;
					}
				}
				if (num90 > 0.0)
				{
					num88 /= num90;
					num89 /= num90;
					((Control)PlotForm.lblMotion).set_Text(string.Format("Motion: {0,1:F2} km/s", Math.Sqrt(num88 * num88 + num89 * num89)));
				}
				else
				{
					((Control)PlotForm.lblMotion).set_Text("");
				}
				string text7 = ((num86 > 10f) ? string.Format(" {0,1:F0} km ", 2f * num86) : ((num86 > 5f) ? string.Format(" {0,1:F1} km ", 2f * num86) : ((!(num86 > 1f)) ? string.Format(" {0,1:F3} km ", 2f * num86) : string.Format(" {0,1:F2} km ", 2f * num86))));
				font = new Font("Times New Roman", 9f, FontStyle.Italic);
				SizeF sizeF3 = formGraphics.MeasureString(text7, font);
				formGraphics.FillRectangle(brush, ((float)ChartWidth - sizeF3.Width) / 2f, (float)ChartHeight - sizeF3.Height - 3f, sizeF3.Width, sizeF3.Height);
				formGraphics.DrawString(text7, font, brush2, num14 - formGraphics.MeasureString(text7, font).Width / 2f, (float)ChartHeight - sizeF3.Height - 3f);
			}
			if (ShowScale_mas)
			{
				float num92 = (float)(6.378392524383191 / EventDetails.Parallax);
				if (EventDetails.Parallax <= 0.0)
				{
					num92 = 1f;
				}
				float num93 = num3 * 2f / num92;
				float num94 = 1f;
				if (num93 > 20000f)
				{
					num94 = 20000f;
				}
				else if (num93 > 10000f)
				{
					num94 = 10000f;
				}
				else if (num93 > 5000f)
				{
					num94 = 5000f;
				}
				else if (num93 > 2000f)
				{
					num94 = 2000f;
				}
				else if (num93 > 1000f)
				{
					num94 = 1000f;
				}
				else if (num93 > 500f)
				{
					num94 = 500f;
				}
				else if (num93 > 200f)
				{
					num94 = 200f;
				}
				else if (num93 > 100f)
				{
					num94 = 100f;
				}
				else if (num93 > 50f)
				{
					num94 = 50f;
				}
				else if (num93 > 20f)
				{
					num94 = 20f;
				}
				else if (num93 > 10f)
				{
					num94 = 10f;
				}
				else if (num93 > 5f)
				{
					num94 = 5f;
				}
				else if (num93 > 2f)
				{
					num94 = 2f;
				}
				Y1 = num14 - num10 * num92 * num94 / 2f;
				Y2 = num14 + num10 * num92 * num94 / 2f;
				X1 = ChartWidth - 8;
				pen2.DashPattern = new float[2] { 4f, 2f };
				formGraphics.FillRectangle(brush, (float)X1 - 3f, (float)Y1 - 3f, 6f, (float)((double)Y2 - Y1 + 6.0));
				formGraphics.FillRectangle(brush, (float)X1 - 13f, (float)Y1 - 3f, 12f, 6f);
				formGraphics.FillRectangle(brush, (float)X1 - 13f, Y2 - 3f, 12f, 6f);
				formGraphics.DrawLine(pen2, (float)X1, (float)Y1, (float)X1, Y2);
				formGraphics.DrawLine(pen2, (float)X1, (float)Y1, (float)X1 - 10f, (float)Y1);
				formGraphics.DrawLine(pen2, (float)X1, Y2, (float)X1 - 10f, Y2);
				string text8;
				if (num94 > 5000f)
				{
					text8 = string.Format("{0,1:F0}\"", num94 / 1000f);
				}
				text8 = ((!(num94 > 500f)) ? string.Format("{0,1:F0} mas", num94) : string.Format("{0,1:F1}\"", num94 / 1000f));
				font = new Font("Times New Roman", 11f);
				SizeF sizeF4 = formGraphics.MeasureString(text8, font);
				formGraphics.FillRectangle(brush, (float)ChartWidth - sizeF4.Height - 11f, num15 - sizeF4.Height - 3f, sizeF4.Height + 2f, sizeF4.Width + 4f);
				Utilities.DrawRotatedTextAt(formGraphics, 90f, text8, ChartWidth - 10, (int)(num15 - sizeF4.Height), xy_isMiddleOfString: false, font, brush2, brush);
			}
			if (ShowEventsInColour)
			{
				string text9 = "Disappear";
				formGraphics.FillEllipse(red, 10, ChartHeight - 49, 10, 10);
				SizeF sizeF5 = formGraphics.MeasureString(text9, font);
				formGraphics.FillRectangle(brush, 25f, ChartHeight - 51, sizeF5.Width, sizeF5.Height + 1f);
				formGraphics.DrawString(text9, font, brush2, 25f, ChartHeight - 51);
				text9 = "Reappear";
				formGraphics.FillEllipse(forestGreen, 10, ChartHeight - 35, 10, 10);
				sizeF5 = formGraphics.MeasureString(text9, font);
				formGraphics.FillRectangle(brush, 25f, ChartHeight - 37, sizeF5.Width, sizeF5.Height + 1f);
				formGraphics.DrawString(text9, font, brush2, 25f, ChartHeight - 37);
			}
			if ((ShowMeanDiameter & PlotForm.addShapeImageFromDAMIT_ISAMToolStripMenuItem.get_Checked()) && LengthOfDiameterByVolLine > 0f)
			{
				string text10 = ((LengthOfDiameterByVolLine > 100f) ? "0" : ((LengthOfDiameterByVolLine > 20f) ? "1" : ((!(LengthOfDiameterByVolLine > 2f)) ? "3" : "2")));
				font = new Font("Times New Roman", 8f, FontStyle.Regular);
				float num95 = LengthOfDiameterBySurfLine * num10 / 2f;
				X1 = num14 - num95;
				X2 = num14 + num95;
				Y1 = ChartHeight - 40;
				formGraphics.FillRectangle(brush, (float)X1 - 2f, (float)Y1 - 3f, (float)((double)X2 - X1 + 4.0), 6f);
				formGraphics.FillRectangle(brush, (float)X1 - 3f, (float)Y1 - 3f, 6f, 14f);
				formGraphics.FillRectangle(brush, X2 - 3f, (float)Y1 - 3f, 6f, 14f);
				formGraphics.DrawLine(pen, (float)X1, (float)Y1, X2, (float)Y1);
				formGraphics.DrawLine(pen, (float)X1, (float)Y1, (float)X1, (float)Y1 + 10f);
				formGraphics.DrawLine(pen, X2, (float)Y1, X2, (float)Y1 + 10f);
				string text11 = string.Format("(Surface) Mean dia = {0:F" + text10 + "} km", LengthOfDiameterBySurfLine);
				float width2 = formGraphics.MeasureString(text11, font).Width;
				float x5 = num14 - width2;
				if (width2 > num95 - 5f)
				{
					x5 = num14 - num95 + 5f;
				}
				formGraphics.FillRectangle(brush, x5, ChartHeight - 39, width2, 12f);
				formGraphics.DrawString(text11, font, brush2, x5, ChartHeight - 40);
				num95 = LengthOfDiameterByVolLine * num10 / 2f;
				X1 = num14 - num95;
				X2 = num14 + num95;
				Y1 = ChartHeight - 40;
				formGraphics.FillRectangle(brush, (float)X1 - 3f, (float)Y1 - 12f, 6f, 12f);
				formGraphics.FillRectangle(brush, X2 - 3f, (float)Y1 - 12f, 6f, 12f);
				formGraphics.DrawLine(pen, (float)X1, (float)Y1, (float)X1, (float)Y1 - 10f);
				formGraphics.DrawLine(pen, X2, (float)Y1, X2, (float)Y1 - 10f);
				text11 = string.Format("(Volume) Mean dia = {0:F" + text10 + "} km", LengthOfDiameterByVolLine);
				width2 = formGraphics.MeasureString(text11, font).Width;
				x5 = num14;
				if (width2 > num95 - 5f)
				{
					x5 = X2 - width2 - 5f;
				}
				formGraphics.FillRectangle(brush, x5, ChartHeight - 53, width2, 12f);
				formGraphics.DrawString(text11, font, brush2, x5, ChartHeight - 53);
			}
			font = new Font("Times New Roman", 7f, FontStyle.Regular);
			formGraphics.DrawString("Occult " + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version, font, brush2, 6f, ChartHeight - 18);
			if (PlotForm.addShapeImageFromDAMIT_ISAMToolStripMenuItem.get_Checked() && Asteroid_Observations_Reports.Display_ShapeModels != null)
			{
				formGraphics.DrawString("Phase offset " + ((Control)Asteroid_Observations_Reports.Display_ShapeModels.txtPhaseOffset).get_Text(), font, brush2, 110f, ChartHeight - 18);
			}
			Cursor.set_Current(Cursors.get_Default());
		}

		private static void NormaliseXYto1000000(float InnerX, float InnerY, ref float OuterX, ref float OuterY)
		{
			float num = 1f;
			if (Math.Abs(OuterX) > 30000f)
			{
				num = Math.Abs(OuterX) / 30000f;
			}
			if ((Math.Abs(OuterY) > 30000f) & (Math.Abs(OuterY) > Math.Abs(OuterX)))
			{
				num = Math.Abs(OuterY) / 30000f;
			}
			OuterX = InnerX + (OuterX - InnerX) / num;
			OuterY = InnerY + (OuterY - InnerY) / num;
		}

		internal static void GetVariations()
		{
			double[,] array = new double[8, 8];
			double[,] array2 = new double[8, 8];
			double[] array3 = new double[1800];
			double num = 10.0;
			double num2 = 0.0;
			int num3 = 0;
			double num4 = 0.0 - (double)PlotForm.updnX.get_Value();
			double num5 = (double)PlotForm.updnY.get_Value();
			if (num4 == 0.0)
			{
				num4 += 0.001;
			}
			if (num5 == 0.0)
			{
				num5 += 0.001;
			}
			double num6 = (double)PlotForm.updnA.get_Value() / 2.0;
			double num7;
			double num8;
			if (PlotForm.chkCircle.get_Checked())
			{
				num7 = num6;
				num8 = 90.0;
			}
			else
			{
				num7 = (double)PlotForm.updnB.get_Value() / 2.0;
				num8 = 90.0 - (double)PlotForm.updnPA.get_Value();
			}
			if (num7 < 0.01)
			{
				return;
			}
			double num9 = EventDetails.AsteroidNominalDiameter / 1000.0;
			double num10 = EventDetails.AsteroidNominalDiameter / 1000.0;
			double num11 = 0.01;
			int num12 = 0;
			num2 = (num3 = 0);
			for (int i = 0; i < EventDetails.Observers.Count; i++)
			{
				if (((EventDetails.Observers[i].Event_D == "C") | (EventDetails.Observers[i].Event_R == "C")) || ((EventDetails.Observers[i].Event_D == "P") | (EventDetails.Observers[i].Event_R == "P")) || ((EventDetails.Observers[i].Event_D.ToUpper() == "N") | (EventDetails.Observers[i].Event_R.ToUpper() == "N")) || EventDetails.Observers[i].PlotCode == "x" || (!"abc34".Contains(EventDetails.Observers[i].Method) & ShowVideoOnly))
				{
					continue;
				}
				bool flag = true;
				double num13 = 0.0;
				double num14 = 0.0;
				double num15 = 0.0;
				double num16 = 0.0;
				double num17 = 0.0;
				double num18 = 0.0;
				if ("GBgb".Contains(EventDetails.Observers[i].Event_D) | "GBgb".Contains(EventDetails.Observers[i].Event_R))
				{
					flag = false;
				}
				if (EventDetails.NumberOfSatellites > 0)
				{
					bool flag2 = false;
					if ((EventDetails.Observers[i].Event_D == "G") | (EventDetails.Observers[i].Event_R == "B") | (EventDetails.Observers[i].Event_D == "m") | (EventDetails.Observers[i].Event_R == "m"))
					{
						flag2 = true;
					}
					if (PlotForm.optPrimary.get_Checked() & ((EventDetails.Observers[i].Event_D == "G") | (EventDetails.Observers[i].Event_R == "B") | (EventDetails.Observers[i].Event_D == "m") | (EventDetails.Observers[i].Event_R == "m")))
					{
						flag = false;
					}
					if (PlotForm.optSecondary.get_Checked() & ((EventDetails.Observers[i].Event_D == "D") | (EventDetails.Observers[i].Event_R == "R") | (EventDetails.Observers[i].Event_D == "M") | (EventDetails.Observers[i].Event_R == "M")))
					{
						flag = false;
					}
					if (flag2)
					{
						num13 = (double)CenterOf2ndEllipse_X + num4;
						num14 = (double)CenterOf2ndEllipse_Y - num5;
						num15 = num9 * Math.Sin((double)PlotForm.updn_SatPA[SelectedSatellite].get_Value() / (180.0 / Math.PI)) / EventDetails.Parallax * 6378.137 / 1000.0;
						num16 = (0.0 - num9) * Math.Cos((double)PlotForm.updn_SatPA[SelectedSatellite].get_Value() / (180.0 / Math.PI)) / EventDetails.Parallax * 6378.137 / 1000.0;
						num17 = (double)PlotForm.updn_SatSep[SelectedSatellite].get_Value() * Math.Sin(((double)PlotForm.updn_SatPA[SelectedSatellite].get_Value() + num11) / (180.0 / Math.PI)) / EventDetails.Parallax * 6378.137 / 1000.0 - (double)Offset_X_2ndFromPrimary;
						num18 = (0.0 - (double)PlotForm.updn_SatSep[SelectedSatellite].get_Value()) * Math.Cos(((double)PlotForm.updn_SatPA[SelectedSatellite].get_Value() + num11) / (180.0 / Math.PI)) / EventDetails.Parallax * 6378.137 / 1000.0 - (double)Offset_Y_2ndFromPrimary;
					}
				}
				else if (EventDetails.StarIsDouble)
				{
					bool flag3 = false;
					if ((EventDetails.Observers[i].Event_D == "d") | (EventDetails.Observers[i].Event_R == "r") | (EventDetails.Observers[i].Event_D == "m") | (EventDetails.Observers[i].Event_R == "m"))
					{
						flag3 = true;
					}
					if (PlotForm.optPrimary.get_Checked() & ((EventDetails.Observers[i].Event_D == "d") | (EventDetails.Observers[i].Event_R == "r") | (EventDetails.Observers[i].Event_D == "m") | (EventDetails.Observers[i].Event_R == "m")))
					{
						flag = false;
					}
					if (PlotForm.optSecondary.get_Checked() & ((EventDetails.Observers[i].Event_D == "D") | (EventDetails.Observers[i].Event_R == "R") | (EventDetails.Observers[i].Event_D == "M") | (EventDetails.Observers[i].Event_R == "M")))
					{
						flag = false;
					}
					if (flag3)
					{
						num13 = (double)CenterOf2ndEllipse_X + num4;
						num14 = (double)CenterOf2ndEllipse_Y - num5;
						num15 = num9 * Math.Sin((double)PlotForm.updn_DoublePA[PlotForm.CurrentlySelectedComponent].get_Value() / (180.0 / Math.PI)) / EventDetails.Parallax * 6378.137 / 1000.0;
						num16 = (0.0 - num9) * Math.Cos((double)PlotForm.updn_DoublePA[PlotForm.CurrentlySelectedComponent].get_Value() / (180.0 / Math.PI)) / EventDetails.Parallax * 6378.137 / 1000.0;
						num17 = (double)PlotForm.updn_DoubleSep[PlotForm.CurrentlySelectedComponent].get_Value() * Math.Sin(((double)PlotForm.updn_DoublePA[PlotForm.CurrentlySelectedComponent].get_Value() + num11) / (180.0 / Math.PI)) / EventDetails.Parallax * 6378.137 / 1000.0 - (double)Offset_X_2ndFromPrimary;
						num18 = (0.0 - (double)PlotForm.updn_DoubleSep[PlotForm.CurrentlySelectedComponent].get_Value()) * Math.Cos(((double)PlotForm.updn_DoublePA[PlotForm.CurrentlySelectedComponent].get_Value() + num11) / (180.0 / Math.PI)) / EventDetails.Parallax * 6378.137 / 1000.0 - (double)Offset_Y_2ndFromPrimary;
					}
				}
				if (!flag)
				{
					continue;
				}
				for (int j = 0; j <= 1; j++)
				{
					if (!((ErrorX_motion[i, j] != 0f) & !((EventDetails.Observers[i].PlotCode == "y" && j == 0) & (EventDetails.Observers[i].PlotCode == "z" && j == 1))))
					{
						continue;
					}
					double num19 = X[i, j] - X_CenterOfPoints;
					double num20 = 0.0 - Y[i, j] + Y_CenterOfPoints;
					bool flag4 = false;
					bool missFlag = false;
					if (EventDetails.Observers[i].Event_D.ToUpper() == "M")
					{
						missFlag = true;
						flag4 = true;
						if (PlotForm.chkMiss.get_Checked())
						{
							double num21 = num19 - num4 + num13;
							double num22 = num20 - num5 - num14;
							double num23 = X[i, j] - X[i, j + 2];
							double num24 = 0.0 - Y[i, j] + Y[i, j + 2];
							double num25 = Math.Sqrt(num23 * num23 + num24 * num24);
							if (Math.Abs((num21 * num24 - num22 * num23) / num25) < num6)
							{
								double num26 = (0.0 - (num21 * num23 + num22 * num24)) / num25 / num25;
								double num27 = num6 / num25;
								double num28 = 10000.0;
								for (double num29 = 0.0 - num27; num29 <= num27; num29 += num27 / 20.0)
								{
									double num30 = num19 + (num29 + num26) * num23;
									double num31 = num20 + (num29 + num26) * num24;
									double num32 = RadiusResidual(num4, num5, num6, num7, num8, num30, num31, num13, num14, missFlag);
									if (num32 < num28)
									{
										num28 = num32;
										continue;
									}
									if (num28 < 0.0)
									{
										num19 = num30 - num27 / 40.0 * num23;
										num20 = num31 - num27 / 40.0 * num24;
										flag4 = false;
									}
									break;
								}
							}
							j++;
						}
					}
					if (flag4)
					{
						continue;
					}
					Variations[num12, 7] = RadiusResidual(num4, num5, num6, num7, num8, num19, num20, num13, num14, missFlag);
					if (PlotForm.chkX.get_Checked())
					{
						Variations[num12, 0] = RadiusResidual(num4 - num9, num5, num6, num7, num8, num19, num20, num13, num14, missFlag) - Variations[num12, 7];
					}
					else
					{
						Variations[num12, 0] = 0.0;
					}
					if (PlotForm.chkY.get_Checked())
					{
						Variations[num12, 1] = RadiusResidual(num4, num5 + num9, num6, num7, num8, num19, num20, num13, num14, missFlag) - Variations[num12, 7];
					}
					else
					{
						Variations[num12, 1] = 0.0;
					}
					if (PlotForm.chkA.get_Checked())
					{
						Variations[num12, 2] = RadiusResidual(num4, num5, num6 + num10 / 2.0, num7, num8, num19, num20, num13, num14, missFlag) - Variations[num12, 7];
					}
					else
					{
						Variations[num12, 2] = 0.0;
					}
					if (PlotForm.chkB.get_Checked())
					{
						Variations[num12, 3] = RadiusResidual(num4, num5, num6, num7 + num10 / 2.0, num8, num19, num20, num13, num14, missFlag) - Variations[num12, 7];
					}
					else
					{
						Variations[num12, 3] = 0.0;
					}
					if (PlotForm.chkPA.get_Checked())
					{
						Variations[num12, 4] = RadiusResidual(num4, num5, num6, num7, num8 + num11, num19, num20, num13, num14, missFlag) - Variations[num12, 7];
					}
					else
					{
						Variations[num12, 4] = 0.0;
					}
					if (PlotForm.chkCompanionSep.get_Checked())
					{
						Variations[num12, 5] = RadiusResidual(num4, num5, num6, num7, num8, num19, num20, num13 + num15, num14 + num16, missFlag) - Variations[num12, 7];
					}
					else
					{
						Variations[num12, 5] = 0.0;
					}
					if (PlotForm.chkCompanionPA.get_Checked())
					{
						Variations[num12, 6] = RadiusResidual(num4, num5, num6, num7, num8, num19, num20, num13 + num17, num14 + num18, missFlag) - Variations[num12, 7];
					}
					else
					{
						Variations[num12, 6] = 0.0;
					}
					if (PlotForm.chkShapeModelCentered.get_Checked() & (PlotForm.chkCompanionSep.get_Checked() | PlotForm.chkCompanionPA.get_Checked()))
					{
						Variations[num12, 0] = (Variations[num12, 1] = 0.0);
					}
					array3[num12] = 1.0;
					if ("abc".Contains(EventDetails.Observers[i].Method))
					{
						if ("a".Contains(EventDetails.Observers[i].TimeSource))
						{
							array3[num12] = 5.0;
						}
						else
						{
							array3[num12] = 4.0;
						}
					}
					else if ("de".Contains(EventDetails.Observers[i].Method))
					{
						array3[num12] = 3.0;
					}
					if ((EventDetails.Observers[i].Event_D.ToUpper() == "M") | (EventDetails.Observers[i].Event_R.ToUpper() == "M"))
					{
						array3[num12] = 5.0;
					}
					if ((EventDetails.Observers[i].Event_D.ToUpper() == "C") | (EventDetails.Observers[i].Event_R.ToUpper() == "C"))
					{
						array3[num12] = 0.0;
					}
					if (j == 0)
					{
						if (EventDetails.Observers[i].Weight_D_Set)
						{
							array3[num12] = EventDetails.Observers[i].Weight_D;
						}
					}
					else if (EventDetails.Observers[i].Weight_R_Set)
					{
						array3[num12] = EventDetails.Observers[i].Weight_R;
					}
					if (array3[num12] != 0.0)
					{
						if (array3[num12] < num)
						{
							num2 += array3[num12];
							num3++;
						}
						num12++;
					}
				}
			}
			if (num12 < 1)
			{
				return;
			}
			double num33 = 0.0;
			for (int i = 0; i < num12; i++)
			{
				num33 += Variations[i, 7];
			}
			num33 /= (double)num12;
			double num34 = 0.0;
			for (int i = 0; i < num12; i++)
			{
				num34 += Math.Pow(Variations[i, 7] - num33, 2.0);
			}
			double num35 = Math.Sqrt(num34 / (double)num12);
			((Control)PlotForm.lblRMS).set_Text($"RMS fit {num33:F1} ±{num35:F1} km");
			num = ((num3 <= 0) ? 1.0 : (num2 / (double)num3));
			for (int i = 0; i < num12; i++)
			{
				for (int j = 0; j < 7; j++)
				{
					for (int k = j; k <= 7; k++)
					{
						array[j, k] += Variations[i, j] * Variations[i, k] * array3[i] / num;
					}
				}
			}
			for (int j = 1; j < 7; j++)
			{
				for (int k = 0; k < j; k++)
				{
					array[j, k] = array[k, j];
				}
			}
			for (int j = 0; j <= 6; j++)
			{
				for (int k = 0; k <= 6; k++)
				{
					if (j == k)
					{
						array2[j, k] = 1.0;
					}
					else
					{
						array2[j, k] = 0.0;
					}
				}
			}
			for (int l = 0; l <= 6; l++)
			{
				if (array[l, l] == 0.0)
				{
					continue;
				}
				double num36 = array[l, l];
				for (int k = 0; k <= 6; k++)
				{
					array[l, k] /= num36;
					array2[l, k] /= num36;
				}
				for (int j = 0; j <= 6; j++)
				{
					num36 = array[j, l];
					if (j != l)
					{
						for (int k = 0; k <= 6; k++)
						{
							array[j, k] -= array[l, k] * num36;
							array2[j, k] -= array2[l, k] * num36;
						}
					}
				}
			}
			double num37 = array2[0, 0] * array[0, 7] + array2[0, 1] * array[1, 7] + array2[0, 2] * array[2, 7] + array2[0, 3] * array[3, 7] + array2[0, 4] * array[4, 7] + array2[0, 5] * array[5, 7] + array2[0, 6] * array[6, 7];
			double num38 = array2[1, 0] * array[0, 7] + array2[1, 1] * array[1, 7] + array2[1, 2] * array[2, 7] + array2[1, 3] * array[3, 7] + array2[1, 4] * array[4, 7] + array2[1, 5] * array[5, 7] + array2[1, 6] * array[6, 7];
			double num39 = array2[2, 0] * array[0, 7] + array2[2, 1] * array[1, 7] + array2[2, 2] * array[2, 7] + array2[2, 3] * array[3, 7] + array2[2, 4] * array[4, 7] + array2[2, 5] * array[5, 7] + array2[2, 6] * array[6, 7];
			double num40 = array2[3, 0] * array[0, 7] + array2[3, 1] * array[1, 7] + array2[3, 2] * array[2, 7] + array2[3, 3] * array[3, 7] + array2[3, 4] * array[4, 7] + array2[3, 5] * array[5, 7] + array2[3, 6] * array[6, 7];
			double num41 = array2[4, 0] * array[0, 7] + array2[4, 1] * array[1, 7] + array2[4, 2] * array[2, 7] + array2[4, 3] * array[3, 7] + array2[4, 4] * array[4, 7] + array2[4, 5] * array[5, 7] + array2[4, 6] * array[6, 7];
			double num42 = array2[5, 0] * array[0, 7] + array2[5, 1] * array[1, 7] + array2[5, 2] * array[2, 7] + array2[5, 3] * array[3, 7] + array2[5, 4] * array[4, 7] + array2[5, 5] * array[5, 7] + array2[5, 6] * array[6, 7];
			double num43 = array2[6, 0] * array[0, 7] + array2[6, 1] * array[1, 7] + array2[6, 2] * array[2, 7] + array2[6, 3] * array[3, 7] + array2[6, 4] * array[4, 7] + array2[6, 5] * array[5, 7] + array2[6, 6] * array[6, 7];
			if ((Math.Abs(array2[0, 0]) < 10000000000.0) & PlotForm.chkX.get_Checked())
			{
				EventDetails.Sdev_X_Set = true;
				EventDetails.Sdev_X = num35 * Math.Sqrt(Math.Abs(array2[0, 0])) * num9;
			}
			else
			{
				EventDetails.Sdev_X_Set = false;
				EventDetails.Sdev_X = EventDetails.AsteroidNominalDiameter / 2.0;
			}
			if ((Math.Abs(array2[1, 1]) < 10000000000.0) & PlotForm.chkY.get_Checked())
			{
				EventDetails.Sdev_Y_Set = true;
				EventDetails.Sdev_Y = num35 * Math.Sqrt(Math.Abs(array2[1, 1])) * num9;
			}
			else
			{
				EventDetails.Sdev_Y_Set = false;
				EventDetails.Sdev_Y = EventDetails.AsteroidNominalDiameter / 2.0;
			}
			if ((Math.Abs(array2[2, 2]) < 10000000000.0) & PlotForm.chkA.get_Checked())
			{
				EventDetails.Sdev_Major_Set = true;
				EventDetails.Sdev_Major = num35 * Math.Sqrt(Math.Abs(array2[2, 2])) * num10;
			}
			else
			{
				EventDetails.Sdev_Major_Set = false;
			}
			if ((Math.Abs(array2[3, 3]) < 10000000000.0) & PlotForm.chkB.get_Checked())
			{
				EventDetails.Sdev_Minor_Set = true;
				EventDetails.Sdev_Minor = num35 * Math.Sqrt(Math.Abs(array2[3, 3])) * num10;
			}
			else
			{
				EventDetails.Sdev_Minor_Set = false;
			}
			if ((Math.Abs(array2[4, 4]) < 10000000000.0) & PlotForm.chkPA.get_Checked())
			{
				EventDetails.Sdev_PA_Ellipse_Set = true;
				EventDetails.Sdev_PA_Ellipse = num35 * Math.Sqrt(Math.Abs(array2[4, 4])) * num11;
			}
			else
			{
				EventDetails.Sdev_PA_Ellipse_Set = false;
			}
			EventDetails.Sdev_Sep_Set = (EventDetails.Sdev_PA_Star_Set = false);
			if (EventDetails.StarIsDouble && PlotForm.chkCompanionSep.get_Checked())
			{
				if (Math.Abs(array2[5, 5]) < 10000000000.0)
				{
					EventDetails.Sdev_Sep_Set = true;
					EventDetails.Doubles[PlotForm.CurrentlySelectedComponent].Sdev_Sep_Companion = num35 * Math.Sqrt(Math.Abs(array2[5, 5])) * num9;
				}
				if (Math.Abs(array2[6, 6]) < 10000000000.0)
				{
					EventDetails.Sdev_PA_Star_Set = true;
					EventDetails.Doubles[PlotForm.CurrentlySelectedComponent].Sdev_PA_Companion = num35 * Math.Sqrt(Math.Abs(array2[6, 6])) * num11;
				}
			}
			string text = PlotForm.updnB.get_DecimalPlaces().ToString();
			double num44 = (0.0 - num37) * num9;
			if (Math.Abs(num44) > GetAssumedDiameter() * 0.6)
			{
				((Control)PlotForm.chkX).set_Text("---");
			}
			else
			{
				((Control)PlotForm.chkX).set_Text(string.Format("{0:F" + text.ToString() + "}", num44));
			}
			double num45 = (0.0 - num38) * num9;
			if (Math.Abs(num45) > GetAssumedDiameter() * 0.6)
			{
				((Control)PlotForm.chkY).set_Text("---");
			}
			else
			{
				((Control)PlotForm.chkY).set_Text(string.Format("{0:F" + text.ToString() + "}", num45));
			}
			try
			{
				if (Math.Abs((0.0 - num39) * num10) < 100.0)
				{
					((Control)PlotForm.chkA).set_Text(string.Format("{0:F" + text.ToString() + "}", (0.0 - num39) * num10));
				}
				else
				{
					((Control)PlotForm.chkA).set_Text((-Math.Sign(num39) * 100).ToString());
				}
				if (Math.Abs((0.0 - num40) * num10) < 100.0)
				{
					((Control)PlotForm.chkB).set_Text(string.Format("{0:F" + text.ToString() + "}", (0.0 - num40) * num10));
				}
				else
				{
					((Control)PlotForm.chkB).set_Text((-Math.Sign(num40) * 100).ToString());
				}
			}
			catch
			{
			}
			if (Math.Abs(num41 * num11) < 180.0)
			{
				((Control)PlotForm.chkPA).set_Text($"{num41 * num11:F1}");
			}
			else
			{
				((Control)PlotForm.chkPA).set_Text("-0");
			}
			((Control)PlotForm.chkCompanionSep).set_Text($"{(0.0 - num42) * num9:F1}");
			if (Math.Abs((0.0 - num43) * num11) < 180.0)
			{
				((Control)PlotForm.chkCompanionPA).set_Text($"{(0.0 - num43) * num11:F1}");
			}
			else
			{
				((Control)PlotForm.chkCompanionPA).set_Text("-0");
			}
			if (Math.Abs((0.0 - num37) * num9) > 100.0)
			{
				PlotForm.updnX.set_Increment(10m);
			}
			else if (Math.Abs((0.0 - num37) * num9) > 20.0)
			{
				PlotForm.updnX.set_Increment(5m);
			}
			else
			{
				PlotForm.updnX.set_Increment((decimal)Math.Pow(10.0, -PlotForm.updnX.get_DecimalPlaces()));
			}
			if (Math.Abs((0.0 - num38) * num9) > 100.0)
			{
				PlotForm.updnY.set_Increment(10m);
			}
			else if (Math.Abs((0.0 - num38) * num9) > 20.0)
			{
				PlotForm.updnY.set_Increment(5m);
			}
			else
			{
				PlotForm.updnY.set_Increment((decimal)Math.Pow(10.0, -PlotForm.updnY.get_DecimalPlaces()));
			}
			if (Math.Abs((0.0 - num39) * num10) > 30.0)
			{
				PlotForm.updnA.set_Increment(5m);
			}
			else if (Math.Abs((0.0 - num39) * num10) > 10.0)
			{
				PlotForm.updnA.set_Increment(2m);
			}
			else
			{
				PlotForm.updnA.set_Increment((decimal)Math.Pow(10.0, -PlotForm.updnA.get_DecimalPlaces()));
			}
			PlotForm.updnB.set_Increment(PlotForm.updnA.get_Increment());
			if (Math.Abs((0.0 - num40) * num10) > 30.0)
			{
				PlotForm.updnB.set_Increment(5m);
			}
			else if (Math.Abs((0.0 - num40) * num10) > 10.0)
			{
				PlotForm.updnB.set_Increment(2m);
			}
			else
			{
				PlotForm.updnB.set_Increment((decimal)Math.Pow(10.0, -PlotForm.updnB.get_DecimalPlaces()));
			}
		}

		private static double RadiusResidual(double Xc, double Yc, double Major, double Minor, double PA, double Xp, double Yp, double Xs, double Ys, bool MissFlag)
		{
			if (Major < 0.01)
			{
				Major = 0.01;
			}
			else if (Major < 0.1)
			{
				Major = 0.1;
			}
			if (Minor == 0.0)
			{
				Minor = Major / 10000.0;
			}
			double num = Xp - Xc + Xs;
			double num2 = Yp - Yc - Ys;
			double num3 = PA / (180.0 / Math.PI);
			double num4 = num * Math.Cos(num3) + num2 * Math.Sin(num3);
			double num5 = 0.0 - (num * Math.Sin(num3) - num2 * Math.Cos(num3));
			return Math.Sqrt(num4 * num4 + num5 * num5) - EllipseRadius(Major, Minor, num4, num5) - 0.5 * Convert.ToDouble(MissFlag);
		}

		private static double EllipseRadius(double Major, double Minor, double X, double Y)
		{
			double num = Math.Atan(Y / X);
			return Major * Minor / Math.Sqrt(Math.Pow(Major * Math.Sin(num), 2.0) + Math.Pow(Minor * Math.Cos(num), 2.0));
		}

		internal static void ComputeSolution_and_UpDate_EventDetails()
		{
			if (PlotForm.Updating)
			{
				return;
			}
			ShowPlotForm();
			((Control)PlotForm).Focus();
			CheckQualitySetting();
			EventDetails.NumberOfDoubleSolutions = 0;
			EventDetails.RA_Offset_DoubleStar_mas = (EventDetails.Dec_Offset_DoubleStar_mas = (EventDetails.RA_Offset_DoubleStar_sDev_mas = (EventDetails.Dec_Offset_DoubleStar_sDev_mas = 0.0)));
			EventDetails.BrightnessRatio_SolnStar_to_Companion = 1.2;
			if (EventDetails.AsteroidHasSatellite)
			{
				EventDetails.Satellites[SelectedSatellite].SatelliteQuality = ((ListControl)PlotForm.cmb_SatFitQuality[SelectedSatellite]).get_SelectedIndex();
				EventDetails.Satellites[SelectedSatellite].CompanionIAUname = ((Control)PlotForm.txtSatID[SelectedSatellite]).get_Text();
				double.TryParse(((Control)PlotForm.txtSat_dRA[SelectedSatellite]).get_Text(), out var result);
				EventDetails.Satellites[SelectedSatellite].Sat_dRA_mas = result;
				double.TryParse(((Control)PlotForm.txtSat_dDec[SelectedSatellite]).get_Text(), out result);
				EventDetails.Satellites[SelectedSatellite].Sat_dDec_mas = result;
				double.TryParse(((Control)PlotForm.txtSat_d2RA[SelectedSatellite]).get_Text(), out result);
				EventDetails.Satellites[SelectedSatellite].Sat_d2RA_mas = result;
				double.TryParse(((Control)PlotForm.txtSat_d2Dec[SelectedSatellite]).get_Text(), out result);
				EventDetails.Satellites[SelectedSatellite].Sat_d2Dec_mas = result;
				EventDetails.Satellites[SelectedSatellite].CBET_Discovery = ((Control)PlotForm.txtCBET[SelectedSatellite]).get_Text();
			}
			else if (EventDetails.StarIsDouble)
			{
				EventDetails.BrightnessRatio_SolnStar_to_Companion = (double)PlotForm.updnBrightRatio.get_Value();
				EventDetails.BrightnessRatio_UncertaintyPercent = (int)PlotForm.updnBrightnessUncertPerCent.get_Value();
				EventDetails.KnownPair_ID = ((Control)PlotForm.txtDoublePairID).get_Text().Trim();
				EventDetails.JDSO_SubmitDate = ((Control)PlotForm.txtJDSOSubmitDate).get_Text();
				EventDetails.JDSO_Vol_Num_Pg = ((Control)PlotForm.txtJDSO_Vol_Num_Pg).get_Text();
				try
				{
					for (int i = 0; i < 4; i++)
					{
						EventDetails.Doubles[i].Sep_Companion = (double)PlotForm.updn_DoubleSep[i].get_Value();
						if (!PrimaryStarEventFound && i == 0)
						{
							EventDetails.Doubles[i].Sep_Companion = 0.0 - EventDetails.Doubles[i].Sep_Companion;
						}
						if (EventDetails.Doubles[i].Sep_Companion != 0.0)
						{
							EventDetails.NumberOfDoubleSolutions++;
						}
						EventDetails.Doubles[i].PA_Companion = (double)PlotForm.updn_DoublePA[i].get_Value();
						EventDetails.Doubles[i].Companion_Set = ((Control)PlotForm.panelDouble).get_Visible();
						EventDetails.Doubles[i].Offset_X = DoublesOffset_X[i + 1];
						EventDetails.Doubles[i].Offset_Y = DoublesOffset_Y[i + 1];
						EventDetails.Doubles[i].SolutionID = i + 1;
					}
				}
				catch
				{
				}
				List<double> list = new List<double>();
				List<double> list2 = new List<double>();
				for (int j = 0; j < EventDetails.NumberOfDoubleSolutions; j++)
				{
					if ((EventDetails.BrightnessRatio_SolnStar_to_Companion < 50.0) & (EventDetails.BrightnessRatio_SolnStar_to_Companion > -0.02))
					{
						list.Add((0.0 - EventDetails.Doubles[j].Sep_Companion) * Math.Sin(EventDetails.Doubles[j].PA_Companion / (180.0 / Math.PI)) / (1.0 + EventDetails.BrightnessRatio_SolnStar_to_Companion));
						list2.Add((0.0 - EventDetails.Doubles[j].Sep_Companion) * Math.Cos(EventDetails.Doubles[j].PA_Companion / (180.0 / Math.PI)) / (1.0 + EventDetails.BrightnessRatio_SolnStar_to_Companion));
					}
					else
					{
						list.Add(0.0);
						list2.Add(0.0);
					}
				}
				Utilities.Mean_Sdev(list, out var Mean, out var SDev);
				EventDetails.RA_Offset_DoubleStar_mas = Mean;
				EventDetails.RA_Offset_DoubleStar_sDev_mas = Utilities.QuadratureAddition(SDev, Mean * (double)EventDetails.BrightnessRatio_UncertaintyPercent / 100.0);
				Utilities.Mean_Sdev(list2, out var Mean2, out var SDev2);
				EventDetails.Dec_Offset_DoubleStar_mas = Mean2;
				EventDetails.Dec_Offset_DoubleStar_sDev_mas = Utilities.QuadratureAddition(SDev2, Mean2 * (double)EventDetails.BrightnessRatio_UncertaintyPercent / 100.0);
			}
			EventDetails.X = (double)PlotForm.updnX.get_Value();
			EventDetails.Y = (double)PlotForm.updnY.get_Value();
			EventDetails.X_Dia = (double)PlotForm.updnA.get_Value();
			EventDetails.Y_Dia = (double)PlotForm.updnB.get_Value();
			EventDetails.PA_Ellipse = (double)PlotForm.updnPA.get_Value();
			EventDetails.CentreOfMass_Offset_X = 0.0 - (double)PlotForm.updnCenterOfMass_X.get_Value();
			EventDetails.CentreOfMass_Offset_Y = (double)PlotForm.updnCenterOfMass_Y.get_Value();
			EventDetails.Solve_X = PlotForm.chkX.get_Checked();
			EventDetails.Solve_Y = PlotForm.chkY.get_Checked();
			EventDetails.Solve_Major = PlotForm.chkA.get_Checked();
			EventDetails.Solve_Minor = PlotForm.chkB.get_Checked();
			EventDetails.Solve_PA = PlotForm.chkPA.get_Checked();
			EventDetails.Solve_CompanionSep = PlotForm.chkCompanionSep.get_Checked();
			EventDetails.Solve_CompanionPA = PlotForm.chkCompanionPA.get_Checked();
			EventDetails.Solve_Circular = PlotForm.chkCircle.get_Checked();
			EventDetails.Inc_Miss = PlotForm.chkMiss.get_Checked();
			EventDetails.AstrometryShapeModelCentered = PlotForm.chkShapeModelCentered.get_Checked();
			double num4 = EventDetails.RefHour_forAnalysis - EventDetails.MidT_forMotions;
			if (num4 > 12.0)
			{
				num4 -= 24.0;
			}
			if (num4 < -12.0)
			{
				num4 += 24.0;
			}
			double num5 = EventDetails.Parallax + num4 * EventDetails.dParallax;
			Diameter = EventDetails.AsteroidNominalDiameter;
			if (((ListControl)PlotForm.cmbQuality).get_SelectedIndex() == 0)
			{
				EventDetails.dRACosDec_atEvent = 0.0;
				EventDetails.dDec_atEvent = 0.0;
				EventDetails.Sdev_dRACosDec_atEvent = 0.0;
				EventDetails.Sdev_dDec_atEvent = 0.0;
			}
			else
			{
				BessellianElementsInReverse(EventDetails.RA_Star_Apparent, EventDetails.Dec_Star_Apparent, num5, EventDetails.X_Geo_CenterOfMass_atEvent / 6378.137, EventDetails.Y_Geo_CenterOfMass_atEvent / 6378.137, out var dRA_Asteroid_asec, out var dDec_Asteroid_asec);
				Utilities.ApparentOffsetToJ2000Offset(EventDetails.JD_EventDate, EventDetails.RA_Star_2000, EventDetails.Dec_Star_2000, dRA_Asteroid_asec, dDec_Asteroid_asec, out var J2000OffsetRA_asec, out var J2000OffsetDec_asec);
				EventDetails.dRACosDec_atEvent = J2000OffsetRA_asec;
				EventDetails.dDec_atEvent = J2000OffsetDec_asec;
				EventDetails.Sdev_dRACosDec_atEvent = (EventDetails.Sdev_dDec_atEvent = 0.0);
				if (EventDetails.AstrometryShapeModelCentered)
				{
					EventDetails.Sdev_dRACosDec_atEvent = (EventDetails.Sdev_dDec_atEvent = 0.02 * Diameter / 6378.137 * num5);
				}
				else if ((EventDetails.Quality > 1) & EventDetails.Solve_X & EventDetails.Solve_Y)
				{
					EventDetails.Sdev_dRACosDec_atEvent = EventDetails.Sdev_X / 6378.137 * num5;
					EventDetails.Sdev_dDec_atEvent = EventDetails.Sdev_Y / 6378.137 * num5;
				}
				else
				{
					EventDetails.Sdev_dRACosDec_atEvent = (EventDetails.Sdev_dDec_atEvent = 0.4 * Diameter / 6378.137 * num5);
				}
			}
			if (EventDetails.AsteroidHasSatellite)
			{
				for (int k = 0; k < EventDetails.NumberOfSatellites; k++)
				{
					EventDetails.Satellites[k].SatelliteSeparation = (double)PlotForm.updn_SatSep[k].get_Value();
					double num9 = EventDetails.Satellites[k].SatelliteSeparation / 1000.0 / num5 * 6378.137;
					double num10 = (double)PlotForm.updn_SatPA[k].get_Value();
					EventDetails.Satellites[k].SatellitePA_Apparent = num10;
					EventDetails.Satellites[k].XSat_Geo_atEvent = EventDetails.X_Geo_atEvent + num9 * Math.Sin(num10 / (180.0 / Math.PI));
					EventDetails.Satellites[k].YSat_Geo_atEvent = EventDetails.Y_Geo_atEvent + num9 * Math.Cos(num10 / (180.0 / Math.PI));
					double num11 = Utilities.PrecessionalRotationToJ2000_deg(EventDetails.JD_EventDate, EventDetails.RA_Star_Apparent, EventDetails.Dec_Star_Apparent);
					EventDetails.Satellites[k].SatellitePA_2000 = num10 + num11;
					if (EventDetails.Satellites[k].SatellitePA_2000 < 0.0)
					{
						EventDetails.Satellites[k].SatellitePA_2000 += 360.0;
					}
					if (EventDetails.Satellites[k].SatellitePA_2000 > 360.0)
					{
						EventDetails.Satellites[k].SatellitePA_2000 -= 360.0;
					}
					BessellianElementsInReverse(EventDetails.RA_Star_Apparent, EventDetails.Dec_Star_Apparent, num5, EventDetails.Satellites[k].XSat_Geo_atEvent / 6378.137, EventDetails.Satellites[k].YSat_Geo_atEvent / 6378.137, out var dRA_Asteroid_asec2, out var dDec_Asteroid_asec2);
					Utilities.ApparentOffsetToJ2000Offset(EventDetails.JD_EventDate, EventDetails.RA_Star_2000, EventDetails.Dec_Star_2000, dRA_Asteroid_asec2, dDec_Asteroid_asec2, out var J2000OffsetRA_asec2, out var J2000OffsetDec_asec2);
					EventDetails.Satellites[k].dRACosDecSat_atEvent = J2000OffsetRA_asec2;
					EventDetails.Satellites[k].ddecSat_atEvent = J2000OffsetDec_asec2;
					double num12 = Math.Abs(EventDetails.Satellites[k].SatelliteSeparation * Math.Sin((EventDetails.Satellites[k].SatellitePA_2000 + EventDetails.Satellites[k].Sat_PA_Uncertainty) / (180.0 / Math.PI)) - EventDetails.Satellites[k].SatelliteSeparation * Math.Sin(EventDetails.Satellites[k].SatellitePA_2000 / (180.0 / Math.PI)));
					double num13 = Math.Abs(EventDetails.Satellites[k].SatelliteSeparation * Math.Cos((EventDetails.Satellites[k].SatellitePA_2000 + EventDetails.Satellites[k].Sat_PA_Uncertainty) / (180.0 / Math.PI)) - EventDetails.Satellites[k].SatelliteSeparation * Math.Cos(EventDetails.Satellites[k].SatellitePA_2000 / (180.0 / Math.PI)));
					EventDetails.Satellites[k].Sdev_dRACosDecSat_atEvent = Math.Sqrt(Math.Pow(EventDetails.Satellites[k].Sat_Sep_Uncertainty * Math.Sin(EventDetails.Satellites[k].SatellitePA_2000 / (180.0 / Math.PI)), 2.0) + num12 * num12) / 1000.0;
					EventDetails.Satellites[k].Sdev_dDecSat_atEvent = Math.Sqrt(Math.Pow(EventDetails.Satellites[k].Sat_Sep_Uncertainty * Math.Cos(EventDetails.Satellites[k].SatellitePA_2000 / (180.0 / Math.PI)), 2.0) + num13 * num13) / 1000.0;
				}
			}
			GetChordLimits(out var MaxHitPlus, out var MaxHitMinus, out var MinMissPlus, out var MinMissMinus);
			EventDetails.MaxHitPlus = MaxHitPlus;
			EventDetails.MaxHitMinus = MaxHitMinus;
			EventDetails.MinMissPlus = MinMissPlus;
			EventDetails.MinMissMinus = MinMissMinus;
			bool flag = false;
			double num14 = 1.0;
			double num15 = 1.0;
			if (Math.Abs(MaxHitPlus - MaxHitMinus) < 0.3)
			{
				double num16 = Math.Abs((MaxHitPlus + MaxHitMinus) / 2.0);
				if (num16 > 0.8)
				{
					num16 = 0.8;
				}
				if (num16 < 0.2)
				{
					num16 = 0.2;
				}
				num14 /= Math.Sqrt(1.0 - num16 * num16);
				num15 /= num16;
			}
			bool flag2 = false;
			bool flag3 = false;
			bool flag4 = false;
			bool flag5 = false;
			try
			{
				if (EventDetails.AsteroidNumber.PadRight(1).Substring(0, 1) == "P")
				{
					flag5 = true;
					int num17 = int.Parse(EventDetails.AsteroidNumber.Substring(1, 1));
					int num18 = int.Parse(EventDetails.AsteroidNumber.Substring(3, 2));
					if (num18 == 0)
					{
						flag4 = true;
					}
					else if (num17 == 5 && num18 < 5)
					{
						flag4 = true;
					}
					else if (num17 == 6 && num18 > 2 && (num18 < 7 || num18 == 9))
					{
						flag4 = true;
					}
					else if (num17 == 7 && num18 < 5)
					{
						flag4 = true;
					}
					else if (num17 == 8 && num18 == 1)
					{
						flag4 = true;
					}
					else if (num17 == 9 && num18 == 1)
					{
						flag4 = true;
					}
					_ = (num17 == 2 || num17 > 4) && num18 == 0;
					_ = num17 == 6 && num18 == 6;
				}
			}
			catch
			{
			}
			if (flag4)
			{
				if (EventDetails.Number_Chords > 1)
				{
					if (MaxHitPlus >= 0.3 && MaxHitMinus <= -0.3 && MaxHitPlus < 0.9 && MaxHitMinus > -0.9)
					{
						flag2 = true;
					}
					else if ((MaxHitPlus > 0.6 && MaxHitMinus < -0.1) || (MaxHitPlus > 0.1 && MaxHitMinus < -0.6))
					{
						flag2 = true;
					}
					else if ((MaxHitPlus < 0.95 && MaxHitMinus > -0.95) & (Math.Sign(MaxHitPlus) == Math.Sign(MaxHitMinus)))
					{
						if (MaxHitPlus > 0.1 && MaxHitMinus < -0.3)
						{
							flag2 = true;
						}
						if (MaxHitPlus > 0.3 && MaxHitMinus < -0.1)
						{
							flag2 = true;
						}
						if (Math.Abs(MaxHitPlus - MaxHitMinus) > 0.5)
						{
							flag2 = true;
						}
					}
					if (!flag2 && MaxHitPlus < 0.99 && MaxHitMinus > -0.99)
					{
						if (Math.Abs(MaxHitPlus - MaxHitMinus) > 0.3)
						{
							if (MaxHitPlus > 0.4 || MaxHitMinus < -0.4)
							{
								flag2 = true;
							}
							else if ((MaxHitPlus > 0.3 || MaxHitMinus < -0.3) & (EventDetails.Number_Chords > 2))
							{
								flag3 = true;
							}
						}
						else if (Math.Abs(MaxHitPlus - MaxHitMinus) > 0.2)
						{
							if (MaxHitPlus > 0.5 || MaxHitMinus < -0.5)
							{
								flag2 = true;
							}
							else if ((MaxHitPlus > 0.3 || MaxHitMinus < -0.3) & (EventDetails.Number_Chords > 2))
							{
								flag3 = true;
							}
						}
					}
				}
				flag = MinMissPlus - MaxHitPlus < 0.9 || MaxHitMinus - MinMissMinus < 0.9;
			}
			else
			{
				if (MaxHitPlus >= 0.3 && MaxHitMinus <= -0.3)
				{
					flag2 = true;
				}
				else if ((MaxHitPlus > 0.6 && MaxHitMinus < -0.1) || (MaxHitPlus > 0.1 && MaxHitMinus < -0.6))
				{
					flag2 = true;
				}
				else if ((MaxHitPlus < 0.3 || MaxHitMinus > -0.3) && MaxHitPlus - MaxHitMinus > 0.5)
				{
					flag3 = true;
				}
				flag = MinMissPlus < 1.3 || MinMissMinus > -1.3 || MinMissPlus - MaxHitPlus < 0.6 || MaxHitMinus - MinMissMinus < 0.6;
			}
			if (((ListControl)PlotForm.cmbQuality).get_SelectedIndex() == 0)
			{
				EventDetails.X_Geo_atConj = 0.0;
				EventDetails.Y_Geo_atConj = 0.0;
				EventDetails.Sep_km_atConj = 0.0;
				EventDetails.Year_Conj = 0;
				EventDetails.Month_Conj = 0;
				EventDetails.Day_Conj = 0.0;
				EventDetails.Sdev_T_Conj = 0.0;
				EventDetails.Sdev_AlongTrack = 0.0;
				EventDetails.Sep_Conj = 0.0;
				EventDetails.Sdev_Sep_Conj = 0.0;
				EventDetails.PA_Conj_2000 = 0.0;
			}
			else
			{
				double num19 = dX * 6378.137;
				double num20 = dY * 6378.137;
				double num21 = d2X * 6378.137;
				double num22 = d2Y * 6378.137;
				double num23 = d3X * 6378.137;
				double num24 = d3Y * 6378.137;
				double num25 = Math.Atan(dX / dY) * (180.0 / Math.PI);
				double num26 = EventDetails.RefHour_forAnalysis - EventDetails.MidT_forMotions;
				double num27 = EventDetails.X_Geo_atEvent - (num19 * num26 + num21 * (num26 * num26) + num23 * (num26 * num26 * num26));
				double num28 = EventDetails.Y_Geo_atEvent - (num20 * num26 + num22 * (num26 * num26) + num24 * (num26 * num26 * num26));
				double num29 = 0.0;
				int num30 = 0;
				double num32;
				double num33;
				double num34;
				double num38;
				double num39;
				do
				{
					double num31 = num29;
					num32 = EventDetails.Parallax + num31 * EventDetails.dParallax;
					num33 = num27 + num19 * num31 + num21 * (num31 * num31) + num23 * (num31 * num31 * num31);
					num34 = num28 + num20 * num31 + num22 * (num31 * num31) + num24 * (num31 * num31 * num31);
					double num35 = num19 + 2.0 * num21 * num31 + 3.0 * num23 * num31 * num31;
					double num36 = num20 + 2.0 * num22 * num31 + 3.0 * num24 * num31 * num31;
					double num37 = num33 * num35 + num34 * num36;
					num38 = num35 * num35 + num36 * num36;
					if (num38 == 0.0)
					{
						num38 = 1E-06;
					}
					num39 = (0.0 - num37) / num38;
					num29 += num39;
					num30++;
				}
				while (num30 <= 4 && Math.Abs(num39) > 1E-08);
				BessellianElementsInReverse(EventDetails.RA_Star_Apparent, EventDetails.Dec_Star_Apparent, num32, num33 / 6378.137, num34 / 6378.137, out var dRA_Asteroid_asec3, out var dDec_Asteroid_asec3);
				Utilities.ApparentOffsetToJ2000Offset(EventDetails.JD_EventDate, EventDetails.RA_Star_2000, EventDetails.Dec_Star_2000, dRA_Asteroid_asec3, dDec_Asteroid_asec3, out var J2000OffsetRA_asec3, out var J2000OffsetDec_asec3);
				Utilities.Relativistic_Differential_Correction(EventDetails.JD_EventDate, EventDetails.RA_Star_2000, EventDetails.Dec_Star_2000, 8.794143836182533 / num32, out var dRA_ComparedToStar, out var dDec_ComparedToStar);
				double num40 = (0.0 - dRA_ComparedToStar) * (180.0 / Math.PI) * 3600.0 * Math.Cos(EventDetails.Dec_Star_2000);
				double num41 = (0.0 - dDec_ComparedToStar) * (180.0 / Math.PI) * 3600.0;
				J2000OffsetRA_asec3 += num40;
				J2000OffsetDec_asec3 += num41;
				double sdev_X = EventDetails.Sdev_X;
				double sdev_Y = EventDetails.Sdev_Y;
				double num42 = Math.Sqrt(sdev_X * sdev_X * Math.Cos(num25 / (180.0 / Math.PI)) * Math.Cos(num25 / (180.0 / Math.PI)) + sdev_Y * sdev_Y * Math.Sin(num25 / (180.0 / Math.PI)) * Math.Sin(num25 / (180.0 / Math.PI)));
				double num43 = Math.Sqrt(sdev_X * sdev_X * Math.Sin(num25 / (180.0 / Math.PI)) * Math.Sin(num25 / (180.0 / Math.PI)) + sdev_Y * sdev_Y * Math.Cos(num25 / (180.0 / Math.PI)) * Math.Cos(num25 / (180.0 / Math.PI)));
				EventDetails.FitUncertaintyCategory = "a";
				double num44 = num43;
				double num45 = num42;
				int bestShapeModelFit = GetBestShapeModelFit();
				if (flag4)
				{
					double num46 = EventDetails.RefHour_forAnalysis_Uncert_secs;
					if (num46 < 0.3)
					{
						num46 = 1.0;
					}
					double num47 = num46 * Math.Sqrt(num38) / 3600.0;
					if (flag2)
					{
						EventDetails.FitUncertaintyCategory = "d1";
					}
					else if (flag3)
					{
						num43 *= 3.0;
						num42 *= 3.0;
						EventDetails.FitUncertaintyCategory = "d2";
					}
					else if (flag)
					{
						if ((EventDetails.Number_Chords == 1) | (Math.Abs(MaxHitPlus - MaxHitMinus) < 0.1))
						{
							if (num47 > num43)
							{
								num43 = num47 * num14;
								num42 = num47 * num15;
							}
							else
							{
								num43 *= num14;
								num42 *= num15;
							}
						}
						else
						{
							num43 *= num14;
							num42 *= num15;
						}
						EventDetails.FitUncertaintyCategory = "d3";
					}
					else
					{
						if (EventDetails.Number_Chords == 1)
						{
							num43 = ((!(num47 > num43)) ? (num43 * num14) : (num47 * num14));
							double num48 = Diameter / 2.0 / 6378.137 * EventDetails.Parallax;
							num42 = ((!(flag5 & (Math.Abs(MaxHitPlus) > 2.0 / num48))) ? (Diameter / 4.0) : ((!(num47 > num43)) ? (num42 * num15) : (num47 * num15)));
						}
						else
						{
							num43 *= num14;
							num42 = (((Math.Abs(MaxHitPlus + MaxHitMinus) / 2.0 > 0.25) & (Math.Abs(MaxHitPlus - MaxHitMinus) > 0.05)) ? (num42 * num15) : ((!((Math.Abs(MaxHitPlus + MaxHitMinus) / 2.0 > 0.15) & (Math.Abs(MaxHitPlus - MaxHitMinus) > 0.1))) ? (Diameter / 4.0) : (num42 * num15)));
						}
						EventDetails.FitUncertaintyCategory = "d4";
					}
				}
				else if (EventDetails.AstrometryShapeModelCentered | (EventDetails.CentreOfMass_Offset_X != 0.0) | (EventDetails.CentreOfMass_Offset_Y != 0.0))
				{
					num43 = (num42 = Diameter * 0.05);
					EventDetails.FitUncertaintyCategory = "b";
				}
				else if ((bestShapeModelFit >= 4) & (EventDetails.Quality > 1) & (EventDetails.Quality < 5))
				{
					if (EventDetails.Quality > 2)
					{
						num43 = (num42 = Diameter * 0.04);
						EventDetails.FitUncertaintyCategory = "c1";
					}
					else if (EventDetails.Quality == 2)
					{
						num43 = (num42 = Diameter * 0.12);
						EventDetails.FitUncertaintyCategory = "c2";
					}
				}
				else if ((EventDetails.Quality > 1) & (EventDetails.Quality < 5) & ((EventDetails.Solve_Major & EventDetails.Solve_Minor) | EventDetails.Solve_Circular))
				{
					if (EventDetails.Quality > 2)
					{
						double num49;
						if (flag2)
						{
							num49 = Diameter * 0.05;
							EventDetails.FitUncertaintyCategory = "e5";
						}
						else if (flag3)
						{
							num49 = Diameter * 0.08;
							EventDetails.FitUncertaintyCategory = "e6";
						}
						else if (flag)
						{
							num49 = Diameter * 0.12;
							EventDetails.FitUncertaintyCategory = "e7";
						}
						else
						{
							num49 = Diameter * 0.16;
							EventDetails.FitUncertaintyCategory = "e8";
						}
						if (num43 < num49)
						{
							num43 = num49;
						}
						if (num42 < num49)
						{
							num42 = num49;
						}
					}
					else
					{
						double num49;
						if (flag2)
						{
							num49 = Diameter * 0.08;
							EventDetails.FitUncertaintyCategory = "e1";
						}
						else if (flag3)
						{
							num49 = Diameter * 0.12;
							EventDetails.FitUncertaintyCategory = "e2";
						}
						else if (flag)
						{
							num49 = Diameter * 0.16;
							EventDetails.FitUncertaintyCategory = "e3";
						}
						else
						{
							num49 = Diameter * 0.2;
							EventDetails.FitUncertaintyCategory = "e4";
						}
						if (EventDetails.Number_Chords < 6)
						{
							num49 = Diameter * 0.1;
						}
						if (num43 < num49)
						{
							num43 = num49;
						}
						if (num42 < num49)
						{
							num42 = num49;
						}
					}
				}
				else
				{
					num43 = GetTimeUncertainties_AstrometryOnlyEvents();
					if (flag2)
					{
						num42 = EventDetails.DiameterUncertainty;
						EventDetails.FitUncertaintyCategory = "f1";
					}
					else if (flag3)
					{
						num42 = 2.0 * EventDetails.DiameterUncertainty;
						EventDetails.FitUncertaintyCategory = "f2";
					}
					else if (flag)
					{
						num42 = 2.0 * EventDetails.DiameterUncertainty;
						EventDetails.FitUncertaintyCategory = "f3";
					}
					else
					{
						num42 = 0.4 * Diameter;
						EventDetails.FitUncertaintyCategory = "f4";
					}
				}
				EventDetails.AdjustmentTo_sDev_AlongTrack = num43 - num44;
				EventDetails.AdjustmentTo_sDev_AcrossTrack = num42 - num45;
				Utilities.Date_from_JD(Utilities.JD_from_Date(Data_and_Plots.Year, Data_and_Plots.Month, (double)Day + (EventDetails.MidT_forMotions + num29) / 24.0), out var Year, out var Month, out var day);
				EventDetails.Year_Conj = Year;
				EventDetails.Month_Conj = Month;
				EventDetails.Day_Conj = day;
				EventDetails.X_Geo_atConj = num33;
				EventDetails.Y_Geo_atConj = num34;
				EventDetails.Sep_Conj = Math.Sqrt(J2000OffsetRA_asec3 * J2000OffsetRA_asec3 + J2000OffsetDec_asec3 * J2000OffsetDec_asec3);
				EventDetails.PA_Conj_2000 = Math.Atan2(J2000OffsetRA_asec3, J2000OffsetDec_asec3) * (180.0 / Math.PI);
				if (EventDetails.PA_Conj_2000 < 0.0)
				{
					EventDetails.PA_Conj_2000 += 360.0;
				}
				EventDetails.Sdev_T_Conj = num43 / Math.Sqrt(num38) / 24.0;
				EventDetails.Sdev_AlongTrack = num43 / 6378.137 * num32 * 1000.0;
				EventDetails.Sdev_Sep_Conj = num42 / 6378.137 * num32 * 1000.0;
			}
			if (EventDetails.AsteroidHasSatellite)
			{
				for (int l = 0; l < EventDetails.NumberOfSatellites; l++)
				{
					EventDetails.Satellites[SelectedSatellite].SatelliteMotionIncluded = (EventDetails.Satellites[SelectedSatellite].Sat_dRA_mas != 0.0) | (EventDetails.Satellites[SelectedSatellite].Sat_dRA_mas != 0.0);
					double num50 = dX * 6378.137 + EventDetails.Satellites[SelectedSatellite].Sat_dRA_mas / 1000.0 / EventDetails.Parallax;
					double num51 = dY * 6378.137 + EventDetails.Satellites[SelectedSatellite].Sat_dDec_mas / 1000.0 / EventDetails.Parallax;
					double num52 = d2X * 6378.137 + EventDetails.Satellites[SelectedSatellite].Sat_d2RA_mas / 1000.0 / EventDetails.Parallax;
					double num53 = d2Y * 6378.137 + EventDetails.Satellites[SelectedSatellite].Sat_d2Dec_mas / 1000.0 / EventDetails.Parallax;
					double num54 = d3X * 6378.137;
					double num55 = d3Y * 6378.137;
					double num56 = EventDetails.RefHour_forAnalysis - EventDetails.MidT_forMotions;
					double num57 = EventDetails.Satellites[l].XSat_Geo_atEvent - (num50 * num56 + num52 * (num56 * num56) + num54 * (num56 * num56 * num56));
					double num58 = EventDetails.Satellites[l].YSat_Geo_atEvent - (num51 * num56 + num53 * (num56 * num56) + num55 * (num56 * num56 * num56));
					double num59 = 0.0;
					int num60 = 0;
					double num62;
					double num63;
					double num64;
					double num68;
					double num69;
					do
					{
						double num61 = num59;
						num62 = EventDetails.Parallax + num61 * EventDetails.dParallax;
						num63 = num57 + num50 * num61 + num52 * (num61 * num61) + num54 * (num61 * num61 * num61);
						num64 = num58 + num51 * num61 + num53 * (num61 * num61) + num55 * (num61 * num61 * num61);
						double num65 = num50 + 2.0 * num52 * num61 + 3.0 * num54 * num61 * num61;
						double num66 = num51 + 2.0 * num53 * num61 + 3.0 * num55 * num61 * num61;
						double num67 = num63 * num65 + num64 * num66;
						num68 = num65 * num65 + num66 * num66;
						if (num68 == 0.0)
						{
							num68 = 1E-06;
						}
						num69 = (0.0 - num67) / num68;
						num59 += num69;
						num60++;
					}
					while (num60 <= 4 && Math.Abs(num69) > 1E-08);
					EventDetails.Satellites[l].XSat_Geo_atConj = num63;
					EventDetails.Satellites[l].YSat_Geo_atConj = num64;
					BessellianElementsInReverse(EventDetails.RA_Star_Apparent, EventDetails.Dec_Star_Apparent, num62, num63 / 6378.137, num64 / 6378.137, out var dRA_Asteroid_asec4, out var dDec_Asteroid_asec4);
					Utilities.ApparentOffsetToJ2000Offset(EventDetails.JD_EventDate, EventDetails.RA_Star_2000, EventDetails.Dec_Star_2000, dRA_Asteroid_asec4, dDec_Asteroid_asec4, out var J2000OffsetRA_asec4, out var J2000OffsetDec_asec4);
					Utilities.Relativistic_Differential_Correction(EventDetails.JD_EventDate, EventDetails.RA_Star_2000, EventDetails.Dec_Star_2000, 8.794143836182533 / num62, out var dRA_ComparedToStar2, out var dDec_ComparedToStar2);
					double num70 = (0.0 - dRA_ComparedToStar2) * (180.0 / Math.PI) * 3600.0 * Math.Cos(EventDetails.Dec_Star_2000);
					double num71 = (0.0 - dDec_ComparedToStar2) * (180.0 / Math.PI) * 3600.0;
					J2000OffsetRA_asec4 += num70;
					J2000OffsetDec_asec4 += num71;
					EventDetails.Satellites[l].XSat_Geo_atConj = num63;
					EventDetails.Satellites[l].YSat_Geo_atConj = num64;
					if (double.IsNaN(num59))
					{
						num59 = 0.0;
					}
					Utilities.Date_from_JD(Utilities.JD_from_Date(Data_and_Plots.Year, Data_and_Plots.Month, (double)Day + (EventDetails.MidT_forMotions + num59) / 24.0), out var Year2, out var Month2, out var day2);
					EventDetails.Satellites[l].Year_Conj = Year2;
					EventDetails.Satellites[l].Month_Conj = Month2;
					EventDetails.Satellites[l].Day_Conj = day2;
					EventDetails.Satellites[l].Sep_Conj_Star = Math.Sqrt(J2000OffsetRA_asec4 * J2000OffsetRA_asec4 + J2000OffsetDec_asec4 * J2000OffsetDec_asec4);
					EventDetails.Satellites[l].PA_Conj_2000_Star = Math.Atan2(J2000OffsetRA_asec4, J2000OffsetDec_asec4) * (180.0 / Math.PI);
					if (EventDetails.Satellites[l].PA_Conj_2000_Star < 0.0)
					{
						EventDetails.Satellites[l].PA_Conj_2000_Star += 360.0;
					}
					double num72 = (EventDetails.Satellites[l].MajorAxisSatellite + EventDetails.Satellites[l].MinorAxisSatellite) / 20.0;
					double num73 = (EventDetails.Satellites[l].MajorAxisSatellite + EventDetails.Satellites[l].MinorAxisSatellite) / 5.0;
					EventDetails.Satellites[l].Sdev_T_Conj = num72 / Math.Sqrt(num68) / 24.0;
					EventDetails.Satellites[l].Sdev_AlongTrack = num72 / 6378.137 * num62 * 1000.0;
					EventDetails.Satellites[l].Sdev_Sep_Conj = num73 / 6378.137 * num62 * 1000.0;
				}
			}
			CurrentSolutionLabel();
		}

		internal static double GetEventTimeUncertainty_secs(bool UseSatellite, out int TotalEvents, out double AcrossPathUncertainty_mas, out int NoUncert, out double NTP_Phone_Offset)
		{
			List<double> list = new List<double>();
			List<double> list2 = new List<double>();
			double num = 0.0;
			bool flag = false;
			int num2 = 0;
			int num3 = 1;
			double b = 0.0;
			double num4 = 0.0;
			NTP_Phone_Offset = 0.0;
			double num5 = Math.Sqrt(EventDetails.dX * EventDetails.dX + EventDetails.dY * EventDetails.dY);
			TotalEvents = (NoUncert = 0);
			for (int i = 0; i < EventDetails.Observers.Count; i++)
			{
				if (((EventDetails.Observers[i].Event_D == "M") | (EventDetails.Observers[i].Event_D == "P") | (EventDetails.Observers[i].Event_D.ToUpper() == "N")) || !((!EventDetails.Observers[i].Weight_D_Set | (EventDetails.Observers[i].Weight_D > 0)) & (!EventDetails.Observers[i].Weight_R_Set | (EventDetails.Observers[i].Weight_R > 0))) || !((EventDetails.Observers[i].T_Disappear > 0.0) & (EventDetails.Observers[i].T_Reappear > 0.0)))
				{
					continue;
				}
				TotalEvents++;
				if (!((!UseSatellite) ? ((EventDetails.Observers[i].Event_D == "D") | (EventDetails.Observers[i].Event_R == "R")) : ((EventDetails.Observers[i].Event_D == "G") | (EventDetails.Observers[i].Event_R == "B"))))
				{
					continue;
				}
				if (EventDetails.Observers[i].TelescopeType == "8")
				{
					num2++;
				}
				UncertAndWeight_NoUncertaintySet(i, out var TimeUncertainty, out var Weight);
				if (!"xy".Contains(EventDetails.Observers[i].PlotCode))
				{
					num4 = EventDetails.Observers[i].Accuracy_D;
					if (num4 == 0.0)
					{
						num4 = TimeUncertainty;
					}
					num4 *= 1.0 - EventDetails.Observers[i].ObserverMotion_AlongPath_D / num5;
					num3 = ((!EventDetails.Observers[i].Weight_D_Set) ? Weight : EventDetails.Observers[i].Weight_D);
					for (int j = 0; j < num3; j++)
					{
						list.Add(num4);
						list2.Add(num4 * EventDetails.Observers[i].ObserverMotion_AcrossPath_D / 3600.0 * 6378.137);
					}
				}
				if (!"xy".Contains(EventDetails.Observers[i].PlotCode))
				{
					num4 = EventDetails.Observers[i].Accuracy_R;
					if (num4 == 0.0)
					{
						num4 = TimeUncertainty;
					}
					num4 *= 1.0 - EventDetails.Observers[i].ObserverMotion_AlongPath_R / num5;
					num3 = ((!EventDetails.Observers[i].Weight_R_Set) ? Weight : EventDetails.Observers[i].Weight_R);
					for (int k = 0; k < num3; k++)
					{
						list.Add(num4);
						list2.Add(num4 * EventDetails.Observers[i].ObserverMotion_AcrossPath_R / 3600.0 * 6378.137);
					}
				}
			}
			double num6 = 0.0;
			double num7 = 0.0;
			double a = 0.0;
			if (list.Count > 0)
			{
				for (int l = 0; l < list.Count; l++)
				{
					num6 += list[l] * list[l];
					num7 += list2[l] * list2[l];
				}
				a = Math.Sqrt(num6 / (double)list.Count);
				num = Math.Sqrt(num7 / (double)list2.Count);
			}
			AcrossPathUncertainty_mas = num / 6378.137 * EventDetails.Parallax * 1000.0;
			if (num2 > 0)
			{
				double num8 = Math.Sqrt(num2);
				if (num8 / (double)TotalEvents >= 0.99)
				{
					b = 0.4;
				}
				else if (num8 / (double)TotalEvents > 0.49)
				{
					b = 0.2;
				}
				else if (num8 / (double)TotalEvents > 0.3)
				{
					b = 0.1;
				}
			}
			a = Utilities.QuadratureAddition(a, b);
			if (list.Count > 0)
			{
				return a;
			}
			return 0.0;
		}

		private static void UncertAndWeight_NoUncertaintySet(int k, out double TimeUncertainty, out int Weight)
		{
			if ("abc".Contains(EventDetails.Observers[k].Method))
			{
				if ("adef".Contains(EventDetails.Observers[k].TimeSource))
				{
					TimeUncertainty = 0.5;
					if ("ad".Contains(EventDetails.Observers[k].TimeSource))
					{
						Weight = 3;
					}
					if ("f".Contains(EventDetails.Observers[k].TimeSource))
					{
						Weight = 2;
					}
					else
					{
						Weight = 1;
					}
				}
				else
				{
					TimeUncertainty = 0.7;
					Weight = 1;
				}
			}
			else if ("de".Contains(EventDetails.Observers[k].Method))
			{
				TimeUncertainty = 0.5;
				Weight = 2;
			}
			else
			{
				TimeUncertainty = 1.0;
				Weight = 1;
			}
		}

		internal static double GetTimeUncertainties_AstrometryOnlyEvents()
		{
			List<double> list = new List<double>();
			double asteroidNominalDiameter = EventDetails.AsteroidNominalDiameter;
			double num = 6378.137 * Math.Sqrt(EventDetails.dX * EventDetails.dX + EventDetails.dY * EventDetails.dY);
			double num2 = asteroidNominalDiameter / num * 3600.0;
			for (int i = 0; i < EventDetails.Observers.Count; i++)
			{
				if (!((EventDetails.Observers[i].Event_D == "M") | (EventDetails.Observers[i].Event_D == "P") | (EventDetails.Observers[i].Event_D.ToUpper() == "N")) && EventDetails.Observers[i].T_Disappear != EventDetails.Observers[i].T_Reappear && ((!EventDetails.Observers[i].Weight_D_Set | (EventDetails.Observers[i].Weight_D > 0)) & (!EventDetails.Observers[i].Weight_R_Set | (EventDetails.Observers[i].Weight_R > 0))) && ((EventDetails.Observers[i].Event_D == "D") | (EventDetails.Observers[i].Event_R == "R")) && ((EventDetails.Observers[i].T_Disappear > 0.0) & (EventDetails.Observers[i].T_Reappear > 0.0)))
				{
					double num3 = X[i, 1] - X[i, 0];
					double num4 = Y[i, 1] - Y[i, 0];
					double num5 = Math.Sqrt(num3 * num3 + num4 * num4) / asteroidNominalDiameter;
					double item = (((EventDetails.AsteroidNo == 216) | (EventDetails.AsteroidNo == 433) | (EventDetails.AsteroidNo == 25143)) ? (0.2 * num2) : ((num5 > 0.8) ? (0.05 * num2) : ((!(num5 > 0.6)) ? (0.2 * num2) : (0.1 * num2))));
					list.Add(item);
				}
			}
			if (list.Count == 0)
			{
				return 0.0;
			}
			double num6 = 0.0;
			for (int j = 0; j < list.Count; j++)
			{
				num6 += 1.0 / (list[j] * list[j]);
			}
			return 1.0 / Math.Sqrt(num6 / (double)list.Count) / 3600.0 * num;
		}

		internal static int GetBestShapeModelFit()
		{
			int num = -1;
			for (int i = 0; i < EventDetails.ShapeData.Count; i++)
			{
				int fitQuality = EventDetails.ShapeData[i].FitQuality;
				if (fitQuality > num)
				{
					num = fitQuality;
				}
			}
			return num;
		}

		internal static void BessellianElementsInReverse(double RAStarApparent, double DecStarApparent, double AsteroidParallaxAtEvent, double ObservedX, double ObservedY, out double dRA_Asteroid_asec, out double dDec_Asteroid_asec)
		{
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 1E-05;
			double num4 = 1E-05;
			double num5 = 2.42406840554768E-10;
			double num6 = 8.794143836182533 / AsteroidParallaxAtEvent / Math.Sin(4.26352124542639E-05);
			double num7 = num6 * Math.Cos(DecStarApparent) * Math.Sin(num3);
			double num8 = num6 * Math.Sin(num4);
			double num11;
			double num12;
			do
			{
				double num9 = num6 * Math.Cos(DecStarApparent + num2) * Math.Sin(num);
				double num10 = num6 * (Math.Sin(DecStarApparent + num2) * Math.Cos(DecStarApparent) - Math.Cos(DecStarApparent + num2) * Math.Sin(DecStarApparent) * Math.Cos(num));
				num11 = num3 * (ObservedX - num9) / num7;
				num12 = num4 * (ObservedY - num10) / num8;
				num += num11;
				num2 += num12;
			}
			while ((Math.Abs(num11) > num5) | (Math.Abs(num12) > num5));
			dRA_Asteroid_asec = num * (180.0 / Math.PI) * 3600.0 * Math.Cos(DecStarApparent);
			dDec_Asteroid_asec = num2 * (180.0 / Math.PI) * 3600.0;
		}

		internal static void RotateCoords(double Xin, double Yin, double AngleDeg, out double X, out double Y)
		{
			double num = Math.Sin(AngleDeg / (180.0 / Math.PI));
			double num2 = Math.Cos(AngleDeg / (180.0 / Math.PI));
			X = Xin * num2 - Yin * num;
			Y = Xin * num + Yin * num2;
		}

		internal static void CheckQualitySetting()
		{
			//IL_0125: Unknown result type (might be due to invalid IL or missing references)
			if (((ListControl)PlotForm.cmbQuality).get_SelectedIndex() == 0)
			{
				return;
			}
			for (int i = 0; i < EventDetails.Observers.Count; i++)
			{
				if ("DdGg".Contains(EventDetails.Observers[i].Event_D) & (!EventDetails.Observers[i].Weight_D_Set | (EventDetails.Observers[i].Weight_D > 0)) & "RrBb".Contains(EventDetails.Observers[i].Event_R) & (!EventDetails.Observers[i].Weight_R_Set | (EventDetails.Observers[i].Weight_R > 0)) & (EventDetails.Observers[i].PlotCode == " "))
				{
					return;
				}
			}
			if (!Observations_Editor.UpdatingOldFile)
			{
				MessageBox.Show(EventDetails.AsteroidNumber + " " + EventDetails.AsteroidID + " on " + EventDetails.FormattedDate + "\r\n\r\nThere is no observed chord that is definite. As a result\r\nthe Quality setting cannot be greater than 'Not fitted'.\r\n\r\nThe setting will be automatically adjusted to 'Not fitted'.", "Invalid Quality setting", (MessageBoxButtons)0, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
			}
			((ListControl)PlotForm.cmbQuality).set_SelectedIndex(0);
		}

		internal static void CurrentSolutionLabel()
		{
			if (Observations_Editor == null)
			{
				return;
			}
			string[] array = new string[7] { "No reliable position or size", "Astrometry only. No reliable size", "Limits on size, but no shape", "Reliable size. Can fit to shape models", "Resolution better than shape models", "Single frame event. No astrometry", "Only 1 star of a double. No astrometry" };
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append("Current Solution : ");
			stringBuilder.AppendFormat("{0:F1} ", EventDetails.X_Dia);
			stringBuilder.AppendFormat("x {0:F1} ", EventDetails.Y_Dia);
			stringBuilder.AppendFormat("km, PA {0:F1} ", EventDetails.PA_Ellipse);
			try
			{
				if (EventDetails.Doubles[0].Companion_Set)
				{
					if (EventDetails.AsteroidHasSatellite)
					{
						stringBuilder.Append("; Satellite ");
					}
					else
					{
						stringBuilder.Append("; 2nd star ");
					}
					stringBuilder.AppendFormat(" {0:F4}", EventDetails.Doubles[0].Sep_Companion / 1000.0);
					stringBuilder.AppendFormat("\", PA {0:F1} ", EventDetails.Doubles[0].PA_Companion);
				}
			}
			catch
			{
			}
			stringBuilder.Append(": " + array[EventDetails.Quality]);
			int num = NumberOfShapeModels();
			if (num > 0)
			{
				if (EventDetails.ShapeData.Count == num)
				{
					stringBuilder.Append(", all " + num + " models matched");
				}
				else if (EventDetails.ShapeData.Count < num)
				{
					stringBuilder.Append(", " + (num - EventDetails.ShapeData.Count) + "/" + num + " models to be matched");
				}
				else if (EventDetails.ShapeData.Count > num)
				{
					stringBuilder.Append(", " + (EventDetails.ShapeData.Count - num) + " 'old' model(s) to be deleted");
				}
			}
			((Control)Observations_Editor.lblCurrentSolution).set_Text(stringBuilder.ToString());
		}

		internal static int NumberOfShapeModels()
		{
			int num = 0;
			int num2 = 0;
			try
			{
				if (((Control)Observations_Editor.lblInDAMIT).get_Visible())
				{
					num2 = int.Parse(((Control)Observations_Editor.lblInDAMIT).get_Text().Replace("-DAMIT", ""));
				}
				if (((Control)Observations_Editor.lblISAM).get_Visible())
				{
					num = int.Parse(((Control)Observations_Editor.lblISAM).get_Text().Replace("-ISAM", ""));
				}
			}
			catch
			{
			}
			return num2 + num;
		}

		internal static void RelativePathDistances(bool topocentric)
		{
			Observations_InitialisePlot_Using_EventDetails();
			PathDistances.lstDistances.get_Items().Clear();
			if (topocentric)
			{
				PathDistances.lstDistances.get_Items().Add((object)"Relative topocentric path distances {approximate}");
			}
			else
			{
				PathDistances.lstDistances.get_Items().Add((object)"Relative fundamental plane distances");
			}
			PathDistances.lstDistances.get_Items().Add((object)"");
			PathDistances.lstDistances.get_Items().Add((object)"   Distance    Observer");
			double num = Math.Abs(EventDetails.X_Geo_atEvent * dY - EventDetails.Y_Geo_atEvent * dX) / Math.Sqrt(dX * dX + dY * dY) / 6378.137;
			double num2 = 1.0;
			if (topocentric && num < 0.95)
			{
				num2 = 1.0 / Math.Sqrt(1.0 - num * num);
			}
			double asteroidNominalDiameter = EventDetails.AsteroidNominalDiameter;
			string text = ((asteroidNominalDiameter > 100.0) ? "0" : ((asteroidNominalDiameter > 50.0) ? "1" : ((!(asteroidNominalDiameter > 5.0)) ? "3" : "2")));
			double Dist = 0.0;
			for (int i = 0; i < EventDetails.Observers.Count && (!EventDetails.Observers[i].Observer1.ToLower().Contains("predict") || !GetPathDistance(i, out Dist)); i++)
			{
			}
			for (int i = 0; i < EventDetails.Observers.Count; i++)
			{
				if (GetPathDistance(i, out var Dist2))
				{
					StringBuilder stringBuilder = new StringBuilder();
					stringBuilder.AppendFormat("{0,8:F" + text + "} km", num2 * (Dist2 - Dist));
					stringBuilder.AppendFormat("{0,5:F0} ", EventDetails.Observers[i].SeqNumber);
					stringBuilder.Append(EventDetails.Observers[i].ObserversAll);
					PathDistances.lstDistances.get_Items().Add((object)stringBuilder.ToString());
				}
			}
			GetChordLimits(out var MaxHitPlus, out var MaxHitMinus, out var MinMissPlus, out var MinMissMinus);
			PathDistances.lstDistances.get_Items().Add((object)"");
			PathDistances.lstDistances.get_Items().Add((object)("Chord limits: Occn " + string.Format("Max = {0,5:f" + text + "}", MaxHitPlus) + string.Format("  Min = {0,5:F" + text + "}", MaxHitMinus)));
			PathDistances.lstDistances.get_Items().Add((object)(" (in radii)   Miss " + string.Format("Max = {0,5:F" + text + "}", MinMissPlus) + string.Format("  Min = {0,5:F" + text + "}", MinMissMinus)));
			((Form)PathDistances).set_WindowState((FormWindowState)0);
			((Control)PathDistances).Focus();
		}

		private static bool GetPathDistance(int k, out double Dist)
		{
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			Dist = 0.0;
			if (X[k, 0] != X[k, 2])
			{
				num2 = X[k, 0] - X[k, 2];
				num3 = Y[k, 0] - Y[k, 2];
				num = num2 * num2 + num3 * num3;
				Dist = ((X[k, 0] - X_CenterOfPoints + (double)CenterOfEllipse_X) * num3 - (Y[k, 0] - Y_CenterOfPoints + (double)CenterOfEllipse_Y) * num2) / ((double)Math.Sign(num2) * Math.Sqrt(num));
			}
			else
			{
				if (X[k, 1] == X[k, 3])
				{
					return false;
				}
				num2 = X[k, 3] - X[k, 1];
				num3 = Y[k, 3] - Y[k, 1];
				num = num2 * num2 + num3 * num3;
				Dist = ((X[k, 3] - X_CenterOfPoints + (double)CenterOfEllipse_X) * num3 - (Y[k, 3] - Y_CenterOfPoints + (double)CenterOfEllipse_Y) * num2) / ((double)Math.Sign(num2) * Math.Sqrt(num));
			}
			return true;
		}

		private static void GetChordLimits(out double MaxHitPlus, out double MaxHitMinus, out double MinMissPlus, out double MinMissMinus)
		{
			MaxHitPlus = -9.0;
			MaxHitMinus = 9.0;
			MinMissPlus = 9.0;
			MinMissMinus = -9.0;
			bool flag = false;
			if (EventDetails.Quality == 0)
			{
				MaxHitPlus = 9.0;
				MaxHitMinus = -9.0;
				return;
			}
			for (int i = 0; i < EventDetails.Observers.Count; i++)
			{
				if (EventDetails.Observers[i].PlotCode.Trim().Length > 0 || (!"DRM".Contains(EventDetails.Observers[i].Event_D) & !"DRM".Contains(EventDetails.Observers[i].Event_R)) || (((EventDetails.Observers[i].Weight_D == 0) & EventDetails.Observers[i].Weight_D_Set) | ((EventDetails.Observers[i].Weight_R == 0) & EventDetails.Observers[i].Weight_R_Set)) || !GetPathDistance(i, out var Dist))
				{
					continue;
				}
				double num = Dist / EventDetails.AsteroidNominalDiameter * 2.0;
				if (EventDetails.Observers[i].Event_D == "M")
				{
					if ((num >= 0.0) & (num <= MinMissPlus))
					{
						MinMissPlus = num;
					}
					else if ((num <= 0.0) & (num >= MinMissMinus))
					{
						MinMissMinus = num;
					}
					continue;
				}
				if (!flag)
				{
					MaxHitPlus = (MaxHitMinus = num);
				}
				else
				{
					if (num > MaxHitPlus)
					{
						MaxHitPlus = num;
					}
					if (num < MaxHitMinus)
					{
						MaxHitMinus = num;
					}
				}
				flag = true;
			}
		}

		internal static void PredictionOffsets()
		{
			Observations_InitialisePlot_Using_EventDetails();
			Prediction_Offset.lstPredictions.get_Items().Clear();
			Prediction_Offset.lstPredictions.get_Items().Add((object)"O-C's for predictions.   Distances are on the Fundamental Plane.");
			Prediction_Offset.lstPredictions.get_Items().Add((object)"");
			for (int i = 0; i < EventDetails.Observers.Count; i++)
			{
				if ((EventDetails.Observers[i].Event_D == "P") & (EventDetails.Observers[i].PlotCode == " "))
				{
					MinimumDistance(i, out var dT, out var Dist);
					double num = X[i, 0] - X_CenterOfPoints + (double)CenterOfEllipse_X;
					double num2 = Y[i, 0] - Y_CenterOfPoints + (double)CenterOfEllipse_Y;
					double num3 = Math.Sqrt(num * num + num2 * num2);
					StringBuilder stringBuilder = new StringBuilder();
					stringBuilder.AppendFormat("{0,3:F0} ", EventDetails.Observers[i].SeqNumber);
					stringBuilder.Append(EventDetails.Observers[i].ObserversAll);
					stringBuilder.AppendFormat("  dT = {0,1:F1} secs", dT);
					if (EventDetails.AsteroidNominalDiameter > 20.0)
					{
						stringBuilder.AppendFormat(", X = {0,1:f0}, Y = {1,1:f0}, dist = {2,1:F0}, Min sepn = {3,1:F0} km", num, num2, num3, Dist);
					}
					else if (EventDetails.AsteroidNominalDiameter > 8.0)
					{
						stringBuilder.AppendFormat(", X = {0,1:f1}, Y = {1,1:f1}, dist = {2,1:F1}, Min sepn = {3,1:F0} km ", num, num2, num3, Dist);
					}
					else
					{
						stringBuilder.AppendFormat(", X = {0,1:f0}, Y = {1,1:f0}, dist = {2,1:F0}, Min sepn = {3,1:F0} meters ", num * 1000.0, num2 * 1000.0, num3 * 1000.0, Dist * 1000.0);
					}
					Prediction_Offset.lstPredictions.get_Items().Add((object)stringBuilder.ToString());
				}
			}
			((Form)Prediction_Offset).set_WindowState((FormWindowState)0);
			((Control)Prediction_Offset).Focus();
		}

		internal static void MinimumDistance(int Event, out double dT, out double Dist)
		{
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			num = X[Event, 0] - X[Event, 2];
			num2 = Y[Event, 0] - Y[Event, 2];
			num3 = num * num + num2 * num2;
			dT = (0.0 - ((X[Event, 0] - X_CenterOfPoints + (double)CenterOfEllipse_X) * num + (Y[Event, 0] - Y_CenterOfPoints + (double)CenterOfEllipse_Y) * num2)) / num3 * 60.0 * 10.0;
			try
			{
				Dist = (0.0 - ((X[Event, 0] - X_CenterOfPoints + (double)CenterOfEllipse_X) * num2 - (Y[Event, 0] - Y_CenterOfPoints + (double)CenterOfEllipse_Y) * num)) / ((double)Math.Sign(num) * Math.Sqrt(num3));
			}
			catch
			{
				Dist = -999.0;
			}
		}

		internal static void TimeBaseOffsets()
		{
			double[] array = new double[900];
			Observations_InitialisePlot_Using_EventDetails();
			TimeBase_Offset.lstOffsets.get_Items().Clear();
			double num;
			double num2 = (num = 0.0);
			int num3 = 0;
			for (int i = 0; i <= 1; i++)
			{
				for (int j = 0; j < EventDetails.Observers.Count; j++)
				{
					if ((T[j, 0] != 0.0) & (T[j, 1] != 0.0) & (T[j, 0] != T[j, 1]))
					{
						double num4 = (X[j, 0] + X[j, 1]) / 2.0 - X_CenterOfPoints - (double)CenterOfEllipse_X;
						double num5 = (Y[j, 0] + Y[j, 1]) / 2.0 - Y_CenterOfPoints - (double)CenterOfEllipse_Y;
						double num6 = (X[j, 1] - X[j, 0]) / (T[j, 1] - T[j, 0]) / 3600.0;
						double num7 = (Y[j, 1] - Y[j, 0]) / (T[j, 1] - T[j, 0]) / 3600.0;
						array[j] = (0.0 - (num4 * num6 + num5 * num7)) / (num6 * num6 + num7 * num7);
						if (i == 0)
						{
							num2 += array[j];
							num3++;
							continue;
						}
						StringBuilder stringBuilder = new StringBuilder();
						stringBuilder.AppendFormat("{0,3:F0} ", EventDetails.Observers[j].SeqNumber);
						stringBuilder.Append(EventDetails.Observers[j].ObserversAll);
						stringBuilder.AppendFormat(" {0,7:F3}", array[j] - num);
						TimeBase_Offset.lstOffsets.get_Items().Add((object)stringBuilder.ToString());
					}
				}
				num = ((!(i == 0 && num3 > 0)) ? 0.0 : (num2 / (double)num3));
			}
			((Form)TimeBase_Offset).set_WindowState((FormWindowState)0);
			((Control)TimeBase_Offset).Focus();
		}

		internal static void DisplayChordLengths()
		{
			Observations_InitialisePlot_Using_EventDetails();
			if (EventDetails.AsteroidID == null)
			{
				return;
			}
			ChordLengths.lstChords.get_Items().Clear();
			ChordLengths.lstChords.get_Items().Add((object)(EventDetails.AsteroidNumber.Trim() + " " + EventDetails.AsteroidID.Trim() + " on " + EventDetails.FormattedDate));
			ChordLengths.lstChords.get_Items().Add((object)"Observed chord lengths");
			ChordLengths.lstChords.get_Items().Add((object)"");
			ChordLengths.lstChords.get_Items().Add((object)"Length      # Observer");
			for (int i = 0; i < EventDetails.Observers.Count; i++)
			{
				dX = X[i, 0] - X[i, 1];
				dY = Y[i, 0] - Y[i, 1];
				double num = Math.Sqrt(dX * dX + dY * dY);
				if (num > 0.05)
				{
					StringBuilder stringBuilder = new StringBuilder();
					stringBuilder.AppendFormat("{0,6:F1} km", num);
					stringBuilder.AppendFormat(" {0,3:F0} ", EventDetails.Observers[i].SeqNumber);
					stringBuilder.Append(EventDetails.Observers[i].ObserversAll);
					ChordLengths.lstChords.get_Items().Add((object)stringBuilder.ToString());
				}
			}
			((Form)ChordLengths).set_WindowState((FormWindowState)0);
			((Control)ChordLengths).Focus();
		}

		internal static void DisplayObserverVelocities()
		{
			Observations_InitialisePlot_Using_EventDetails();
			if (EventDetails.AsteroidID == null)
			{
				return;
			}
			ObserverVelocities.lstVelocities.get_Items().Clear();
			ObserverVelocities.lstVelocities.get_Items().Add((object)(EventDetails.AsteroidNumber.Trim().PadLeft(8) + " " + EventDetails.AsteroidID.Trim() + " on " + EventDetails.FormattedDate));
			ObserverVelocities.lstVelocities.get_Items().Add((object)"");
			ObserverVelocities.lstVelocities.get_Items().Add((object)"Observer velocities relative to the asteriod - km/sec");
			ObserverVelocities.lstVelocities.get_Items().Add((object)"");
			ObserverVelocities.lstVelocities.get_Items().Add((object)"Chord");
			ObserverVelocities.lstVelocities.get_Items().Add((object)"  #     dX       dY");
			for (int i = 0; i < EventDetails.Observers.Count; i++)
			{
				double num = (T[i, 1] - T[i, 0]) * 3600.0;
				if (num > 0.0)
				{
					double num2 = (X[i, 1] - X[i, 0]) / num;
					double num3 = (0.0 - Y[i, 1] + Y[i, 0]) / num;
					StringBuilder stringBuilder = new StringBuilder();
					stringBuilder.AppendFormat(" {0,2:F0}    {1,6:f3}   {2,6:f3}", EventDetails.Observers[i].SeqNumber, num2, num3);
					ObserverVelocities.lstVelocities.get_Items().Add((object)stringBuilder.ToString());
				}
			}
			((Form)ObserverVelocities).set_WindowState((FormWindowState)0);
			((Control)ObserverVelocities).Focus();
		}

		internal static bool ClosestMissTimes()
		{
			int num = 0;
			int num2 = 0;
			Miss_Times.lstMiss.get_Items().Clear();
			for (int i = 0; i < EventDetails.Observers.Count; i++)
			{
				if (T[i, 0] != 0.0)
				{
					if ((EventDetails.Observers[i].Event_D.ToUpper() == "M") | (EventDetails.Observers[i].Event_D == "n") | (EventDetails.Observers[i].Event_D == "C"))
					{
						double num3 = X[i, 0] - X[i, 2];
						double num4 = Y[i, 0] - Y[i, 2];
						double num5 = num3 * num3 + num4 * num4;
						Miss_Times.Rec[num] = i;
						Miss_Times.Tcorrn[num] = (0.0 - ((X[i, 0] - X_CenterOfPoints + (double)CenterOfEllipse_X) * num3 + (Y[i, 0] - Y_CenterOfPoints + (double)CenterOfEllipse_Y) * num4)) / num5 * 60.0 * 10.0;
						StringBuilder stringBuilder = new StringBuilder();
						stringBuilder.AppendFormat("{0,3:F0} ", EventDetails.Observers[i].SeqNumber);
						stringBuilder.AppendFormat("{0,6:F1}s", Miss_Times.Tcorrn[num]);
						Miss_Times.lstMiss.get_Items().Add((object)stringBuilder.ToString());
						num++;
					}
					else if (((EventDetails.Observers[i].Event_D.ToUpper() == "D") | (EventDetails.Observers[i].Event_D.ToUpper() == "R")) & (EventDetails.Observers[i].PlotCode.Trim() == ""))
					{
						num2++;
					}
				}
			}
			return num2 > 0;
		}

		internal static void CloseClosestMissTimes()
		{
			try
			{
				Miss_Times.MClose();
			}
			catch
			{
			}
		}

		public static void AddMPC_AcceptedData_toHistoricalFile()
		{
			try
			{
				((Control)mPCDetails).Show();
			}
			catch
			{
				mPCDetails = new MPCDetails();
				((Control)mPCDetails).Show();
			}
		}

		public static void AddMPCdata_toHistoricalFile(bool AddID, bool AddMPC, string SubmissionDate, string MPCnum)
		{
			Historical_AllEvents.OccEvents.Clear();
			Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			bool flag = false;
			int num = 0;
			int num2 = 0;
			((Control)mPCDetails.PBar).set_Visible(true);
			mPCDetails.PBar.set_Minimum(0);
			mPCDetails.PBar.set_Maximum(MPCdetailsList.Count + 1);
			for (int i = 0; i < MPCdetailsList.Count; i++)
			{
				int year = MPCdetailsList[i].Year;
				int month = MPCdetailsList[i].Month;
				double day = MPCdetailsList[i].Day;
				string astNum = MPCdetailsList[i].AstNum;
				string provID = MPCdetailsList[i].ProvID;
				string iD = MPCdetailsList[i].ID;
				flag = false;
				mPCDetails.PBar.set_Value(i);
				Application.DoEvents();
				for (num = num2; num < Historical_AllEvents.OccEvents.Count; num++)
				{
					Historical_AllEvents.GetEventDateOfAstrometry(num, out var Year, out var Month, out var Day, out var Hour);
					if (Year < year)
					{
						continue;
					}
					if (Year > year)
					{
						break;
					}
					if (Month < month)
					{
						continue;
					}
					if (Month > month)
					{
						break;
					}
					double num3 = (double)Day + Hour / 24.0;
					if (num3 < day - 0.005)
					{
						continue;
					}
					if (num3 > day + 0.005)
					{
						break;
					}
					int eventQuality = Historical_AllEvents.GetEventQuality(num);
					if (eventQuality == 0 || eventQuality == 5 || eventQuality == 6)
					{
						continue;
					}
					Historical_AllEvents.GetAsteroidID(num, out var AsteroidNumber, out var AsteroidName);
					if (astNum.Trim().Length > 0)
					{
						if (AsteroidNumber.PadLeft(10) != astNum.PadLeft(10))
						{
							continue;
						}
					}
					else if (AsteroidName.PadLeft(10) != provID.PadLeft(10))
					{
						continue;
					}
					flag = true;
					for (int j = 0; j < Historical_AllEvents.OccEvents[num].Lines.Count; j++)
					{
						if (Historical_AllEvents.OccEvents[num].Lines[j].Contains(Occult.Asteroid_Observations.Tags.TagStart[15]))
						{
							string[] array = Historical_AllEvents.OccEvents[num].Lines[j].Trim().Replace(Occult.Asteroid_Observations.Tags.TagStart[15], "").Replace(Occult.Asteroid_Observations.Tags.TagEnd[15], "")
								.Split(new char[1] { '|' });
							if (AddID)
							{
								array[0] = SubmissionDate;
								array[2] = iD;
							}
							else if (AddMPC)
							{
								array[1] = MPCnum;
							}
							string value = "".PadLeft(Occult.Asteroid_Observations.Tags.TagIndent[15]) + Occult.Asteroid_Observations.Tags.TagStart[15] + array[0] + "|" + array[1] + "|" + array[2] + Occult.Asteroid_Observations.Tags.TagEnd[15];
							Historical_AllEvents.OccEvents[num].Lines[j] = value;
						}
					}
					break;
				}
				num2 = num - 30;
				if (num2 < 0)
				{
					num2 = 0;
				}
				if (flag)
				{
					MPCdetailsList[i].StatusValue_None_Good_Bad = 1;
				}
				else
				{
					MPCdetailsList[i].StatusValue_None_Good_Bad = 2;
				}
			}
			((Control)mPCDetails.PBar).set_Visible(false);
			Historical_AllEvents.WriteHistoricalObservationsFile(HistoricalFile: true, "");
		}

		public static void ValidateHistoricalFile()
		{
			//IL_0039: Unknown result type (might be due to invalid IL or missing references)
			//IL_003f: Invalid comparison between Unknown and I4
			//IL_0196: Unknown result type (might be due to invalid IL or missing references)
			if (Historical_AllEvents.OccEvents.Count < 1)
			{
				Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			bool flag = (int)MessageBox.Show("Include telescope aperture in check?", "Inclusions", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 6;
			using (StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Resource Files\\AsteroidObservations.val"))
			{
				streamWriter.WriteLine("Validity check run on " + DateTime.Today.ToUniversalTime().ToShortDateString());
				streamWriter.WriteLine("");
				streamWriter.Write("Tags = Xdia Ydia Long Lat Alt D=R AccD AccR TelAp");
				if (flag)
				{
					streamWriter.WriteLine("TelAp ");
				}
				else
				{
					streamWriter.WriteLine("");
				}
				streamWriter.WriteLine(" Xdia = X diameter of the asteroid <=0");
				streamWriter.WriteLine(" Ydia = Y diameter of the asteroid <=0");
				streamWriter.WriteLine(" Long = longitude value  < -180, > 360, or exactly zero");
				streamWriter.WriteLine(" Lat  = latitude value < -90, > 90, or exactly zero");
				streamWriter.WriteLine(" Alt  = Altitude < -300m or > 5800m");
				streamWriter.WriteLine(" D=R  = the D and R times are identical, and the event is not a miss, nor 'not seen' (eg because of cloud)");
				streamWriter.WriteLine(" AccD = timing uncertainty for D <0 or >10 secs");
				streamWriter.WriteLine(" AccR = timing uncertainty for R <0 or >10 secs");
				if (flag)
				{
					streamWriter.WriteLine(" TelAp= telescope aperture <= 0 cm, or > 110 cm");
				}
				streamWriter.WriteLine("");
				streamWriter.WriteLine("".PadRight(50, '*'));
				for (int i = 0; i < Historical_AllEvents.OccEvents.Count; i++)
				{
					if (Historical_AllEvents.CheckEventForFormatErrors(i, flag, out var ErrorList))
					{
						streamWriter.WriteLine(ErrorList);
					}
				}
			}
			MessageBox.Show("The validation check has completed.\r\n\r\nThe results are in the file\r\n" + Utilities.AppPath + "/Resource Files/AsteroidObservations.val", "Output File", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
		}

		internal static void EventCoordinates(bool IncludeEventUncertainties)
		{
			float num = 0.2f;
			float num2 = 0.2f;
			string value = " (       ,       ) ";
			string value2 = " (        ,        ) ";
			string format = " ({0,8:f3},{1,8:f3}) ";
			string format2 = " ({0,7:f2},{1,7:f2}) ";
			string text = "Site: Observer             Minus uncertainty     Disappear (x,y)   Plus Uncertainty        Minus uncertainty     Reappear (x,y)    Plus Uncertainty     Star motion (km/sec) & PA";
			string text2 = "Site: Observer               Disappear (x,y)           Reappear (x,y)       Star motion (km/sec) & PA";
			Event_Coords.lstCoords.get_Items().Clear();
			Event_Coords.lstCoords.get_Items().Add((object)("Event coordinates for (" + EventDetails.AsteroidNumber + ") " + EventDetails.AsteroidID + " on " + EventDetails.Year + " " + Utilities.ShortMonths[EventDetails.Month] + " " + EventDetails.Day.ToString().PadLeft(2, '0')));
			Event_Coords.lstCoords.get_Items().Add((object)"");
			if (IncludeEventUncertainties)
			{
				Event_Coords.lstCoords.get_Items().Add((object)text);
			}
			else
			{
				Event_Coords.lstCoords.get_Items().Add((object)text2);
			}
			for (int i = 0; i < EventDetails.Observers.Count; i++)
			{
				StringBuilder stringBuilder = new StringBuilder();
				stringBuilder.Append(EventDetails.Observers[i].SeqNumber.ToString().PadLeft(4) + ": " + EventDetails.Observers[i].Observer1.Trim().PadRight(20));
				int Weight = 1;
				UncertAndWeight_NoUncertaintySet(i, out var TimeUncertainty, out Weight);
				num2 = (num = (float)TimeUncertainty);
				if (EventDetails.Observers[i].Accuracy_D_Set)
				{
					num = (float)EventDetails.Observers[i].Accuracy_D;
				}
				if (EventDetails.Observers[i].Accuracy_R_Set)
				{
					num2 = (float)EventDetails.Observers[i].Accuracy_R;
				}
				if (IncludeEventUncertainties)
				{
					if ((EventDetails.Observers[i].Event_D.ToUpper() == "M") | (EventDetails.Observers[i].T_Disappear == 0.0))
					{
						stringBuilder.Append(value);
					}
					else
					{
						stringBuilder.AppendFormat(format2, X[i, 0] - (double)(num * ErrorX_motion[i, 0]) - X_CenterOfPoints, 0.0 - Y[i, 0] + (double)(num * ErrorY_Motion[i, 0]) + Y_CenterOfPoints);
					}
				}
				if (EventDetails.Observers[i].T_Disappear == 0.0)
				{
					stringBuilder.Append(value2);
				}
				else
				{
					stringBuilder.AppendFormat(format, X[i, 0] - X_CenterOfPoints, 0.0 - Y[i, 0] + Y_CenterOfPoints);
				}
				if (IncludeEventUncertainties)
				{
					if ((EventDetails.Observers[i].Event_D.ToUpper() == "M") | (EventDetails.Observers[i].T_Disappear == 0.0))
					{
						stringBuilder.Append(value);
					}
					else
					{
						stringBuilder.AppendFormat(format2, X[i, 0] + (double)(num * ErrorX_motion[i, 0]) - X_CenterOfPoints, 0.0 - Y[i, 0] - (double)(num * ErrorY_Motion[i, 0]) + Y_CenterOfPoints);
					}
					stringBuilder.Append("     ");
					if ((EventDetails.Observers[i].Event_D.ToUpper() == "M") | (EventDetails.Observers[i].T_Reappear == 0.0))
					{
						stringBuilder.Append(value);
					}
					else
					{
						stringBuilder.AppendFormat(format2, X[i, 1] + (double)(num2 * ErrorX_motion[i, 1]) - X_CenterOfPoints, 0.0 - Y[i, 1] - (double)(num2 * ErrorY_Motion[i, 1]) + Y_CenterOfPoints);
					}
				}
				else
				{
					stringBuilder.Append("     ");
				}
				if (EventDetails.Observers[i].T_Reappear == 0.0)
				{
					stringBuilder.Append(value2);
				}
				else
				{
					stringBuilder.AppendFormat(format, X[i, 1] - X_CenterOfPoints, 0.0 - Y[i, 1] + Y_CenterOfPoints);
				}
				if (IncludeEventUncertainties)
				{
					if ((EventDetails.Observers[i].Event_D.ToUpper() == "M") | (EventDetails.Observers[i].T_Reappear == 0.0))
					{
						stringBuilder.Append(value);
					}
					else
					{
						stringBuilder.AppendFormat(format2, X[i, 1] - (double)(num2 * ErrorX_motion[i, 1]) - X_CenterOfPoints, 0.0 - Y[i, 1] + (double)(num2 * ErrorY_Motion[i, 1]) + Y_CenterOfPoints);
					}
				}
				double num3 = 1200.0 + (EventDetails.Observers[i].T_Reappear - EventDetails.Observers[i].T_Disappear) * 3600.0;
				double num4 = (X[i, 3] - X[i, 2]) / num3;
				double num5 = (Y[i, 3] - Y[i, 2]) / num3;
				double num6 = Math.Atan2(num4, 0.0 - num5) * (180.0 / Math.PI);
				if (num6 < 0.0)
				{
					num6 += 360.0;
				}
				stringBuilder.AppendFormat("   dX={0,6:f3}, dY={1,6:f3}, {2,1:f2}°", 0.0 - num4, num5, num6);
				Event_Coords.lstCoords.get_Items().Add((object)stringBuilder);
			}
			Event_Coords.lstCoords.get_Items().Add((object)"");
			Event_Coords.lstCoords.get_Items().Add((object)"                       Center coords (x,y)    Major axis   Minor axis   PA of axis");
			Event_Coords.lstCoords.get_Items().Add((object)string.Format("Fitted ellipse:      ({0,9:f3}, {1,9:f3}), {2,8:f3} km, {3,8:f3} km,  {4,6:f2}°", CenterOfEllipse_X, CenterOfEllipse_Y, (float)PlotForm.updnA.get_Value(), (float)PlotForm.updnB.get_Value(), (double)PlotForm.updnPA.get_Value()));
			if (EventDetails.AsteroidHasSatellite)
			{
				Event_Coords.lstCoords.get_Items().Add((object)"");
				Event_Coords.lstCoords.get_Items().Add((object)"                       Center coords (x,y)    Major axis   Minor axis   PA of axis");
				for (int j = 0; j < EventDetails.NumberOfSatellites; j++)
				{
					float num7 = (float)((double)PlotForm.updn_SatSep[j].get_Value() / 1000.0 * Math.Sin((double)PlotForm.updn_SatPA[j].get_Value() / (180.0 / Math.PI)) / EventDetails.Parallax * 6378.137);
					float num8 = (float)((0.0 - (double)PlotForm.updn_SatSep[j].get_Value()) / 1000.0 * Math.Cos((double)PlotForm.updn_SatPA[j].get_Value() / (180.0 / Math.PI)) / EventDetails.Parallax * 6378.137);
					CenterOf2ndEllipse_X = CenterOfEllipse_X - num7;
					CenterOf2ndEllipse_Y = CenterOfEllipse_Y - num8;
					Event_Coords.lstCoords.get_Items().Add((object)string.Format("Satellite Ellipse {0}: ({1,9:f3}, {2,9:f3}), {3,8:f3} km, {4,8:f3} km,  {5,6:f2}°", j, CenterOf2ndEllipse_X, CenterOf2ndEllipse_Y, (float)PlotForm.updn_ASat[j].get_Value(), (float)PlotForm.updn_BSat[j].get_Value(), (double)PlotForm.updn_PASat[j].get_Value()));
				}
			}
		}
	}
}
