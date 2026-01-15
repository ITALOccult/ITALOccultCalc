using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using GaiaDoubles;
using LightCurves;
using Occult.Asteroid_Predictions;
using Occult.Asteroids;
using Occult.Ephemerides;
using Occult.MPC_PDS;
using Occult.Properties;
using Occult.Star_Catalogues;
using Shapes;

namespace Occult.Asteroid_Observations
{
	public class ObservationsEditor : Form
	{
		private readonly string AppPath;

		private const double Radian = 180.0 / Math.PI;

		private bool Loaded;

		private bool UpdatingData;

		internal bool UpdatingOldFile;

		private bool EditorHasBeenActivated;

		private DisplayData DataBox;

		private DisplayData ReportFileCheck;

		private static Unistellar Unistellar;

		private static DisplayData D_ShapeModels;

		private static DisplayData D_Doubles;

		private static DisplayData D_NearbyStars;

		private static DisplayData D_Satellites;

		private static DisplayData D_Stars;

		private static double PreviousEventMidHour = -1.0;

		private Timer EditTimer;

		private Timer MagDropTimer;

		internal DateTime StartTime;

		internal string TimeDifference = "Not saved";

		private static readonly int ColorCount = 4;

		private static Color[] C = new Color[ColorCount];

		private static int CurrentColor = 0;

		private const string TimeOptions = " abcdefg1234567890";

		private const string MethodOptions = " abcdefg1234567890";

		private string[] GaiaVersions = Gaia.StarSourceCat;

		private const int StartYearIndexByHalves = 2018;

		private int EntryForImportEdit = -1;

		private double StarMag;

		private double MagAsteroid;

		private int CurrentHistoricalRecord;

		private string Current_OBS_File;

		private string[] MultipleOBSFiles;

		private string ToDeleteFileName = "";

		private string Observations_BUPfile = "";

		private string DeletionPath = "";

		internal static string Quality_FileNameBase = "Qchecked_";

		internal static string Quality_FileName = "";

		internal static string QualityFileRoot = Utilities.AppPath + "\\Import_Export\\";

		private int Current_MultipleOBSFiles;

		private bool IsEuraster;

		private bool IsJP;

		private bool ReadingMultipleOBSFiles;

		private bool ReadingMultipleEurasterEntries;

		private bool ReadingMultipleJPEntries;

		private bool EventLoadedFromFile;

		private bool NewEventForShapeModelling;

		private bool AutoSaveSingle;

		private static string[] EurasterEvents;

		private int EurasterCount;

		private int CurrentEuraster;

		private static string[] JPEvents;

		private int JPCount;

		private int CurrentJP;

		private double PMPeriod;

		private double EventDate;

		private double StarRA2000;

		private double StarDec2000;

		private double Mv;

		private double Mp;

		private double RAobject_1_Apparent;

		private double RAobject0_Apparent;

		private double RAobject1_Apparent;

		private double RAobject2_Apparent;

		private double DecObject_1_Apparent;

		private double DecObject0_Apparent;

		private double DecObject1_Apparent;

		private double DecObject2_Apparent;

		private double DeltaAsteroid_1;

		private double DeltaAsteroid0;

		private double DeltaAsteroid1;

		private double DeltaAsteroid2;

		private double RAStar_1_Apparent;

		private double RAStar0_Apparent;

		private double RAStar1_Apparent;

		private double RAStar2_Apparent;

		private double DecStar_1_Apparent;

		private double DecStar0_Apparent;

		private double DecStar1_Apparent;

		private double DecStar2_Apparent;

		private static LightCurveViewer LightCurveViewer;

		private List<int> LightCurveRecordNumbers;

		private List<int> SubmittedRecordNumbers;

		private List<GoogleMapsCoords> GoogleMaps_Coords;

		private static DisplayData AltitudeCheck;

		private static GaiaNonSingleStar Non_Single_Gaia;

		private static PredictionLightCurve PredictionLightCurves;

		private static GaiaMagMeasures GaiaMagMeasures;

		private static Star_Diameter_from_Gaia GaiaStarDiameter;

		private static SetCompileFileNames SetCompileNames;

		private static StarEquivalentList StarEquivalent;

		private static string SesameAddress = "http://cds.unistra.fr/cgi-bin/nph-sesame/-oI?";

		private bool MidTChanging;

		private IContainer components;

		private GroupBox grpDate;

		private TextBox txtYear;

		private GroupBox grpStar;

		private GroupBox grpAsteroid;

		private GroupBox grpObserver;

		private GroupBox grpHistory;

		private GroupBox grpTimes;

		private GroupBox grpConditions;

		private MenuStrip menuStrip1;

		private TextBox txtMonth;

		private TextBox txtMidT;

		private Label label5;

		private Label label4;

		private Label label3;

		private Label label2;

		private Label label1;

		private Label label6;

		private ComboBox cmbCatalogue;

		private Panel panelHip;

		private TextBox txtHip;

		private Panel panelB1;

		private TextBox txtB1number;

		private TextBox txtB1zone;

		private Label label9;

		private Label lblMag;

		private Label lblDec;

		private Label lblRA;

		private Button cmdGetStarPos;

		private Label label10;

		private Panel panelPlanets;

		private ComboBox cmbMoons;

		private ComboBox cmbPlanets;

		private RadioButton optPlanet;

		private RadioButton optAsteroid;

		private Label label12;

		private Label label11;

		private Label label14;

		private Label label13;

		private TextBox txtAsteroidName;

		private TextBox txtAsteroidNumber;

		private Button cmdGetAsteroid;

		private ToolStripMenuItem fileToolStripMenuItem;

		private ToolStripMenuItem newToolStripMenuItem;

		private ToolStripMenuItem openToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem saveAsToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripMenuItem addToHistoricalFileToolStripMenuItem;

		private Label lblMotions;

		private Label lblDiameterKM;

		private Label lblParallax;

		private ComboBox cmbHistorical;

		private ComboBox cmbNumberRange;

		private ComboBox cmbAlpha;

		private ComboBox cmbYearRange;

		private RadioButton optByRecords;

		private RadioButton optByNumber;

		private RadioButton optByName;

		private RadioButton optByDate;

		private ListBox lstObservations;

		private Panel panel1;

		private Label label19;

		private Label label18;

		private Label label17;

		private Label label16;

		private Panel panel2;

		private RadioButton optFeet;

		private Panel panelDMS;

		private Panel panelDDD;

		private Panel panelDMM;

		private TextBox txtD_Min;

		private ComboBox cmbD_Wt;

		private TextBox txtD_Sec;

		private TextBox txtR_Hr;

		private TextBox txtR_Min;

		private TextBox txtR_Sec;

		private TextBox txtD_Uncert;

		private TextBox txtR_Uncert;

		private TextBox txtD_PEq;

		private TextBox txtR_PEq;

		private TextBox txtShift;

		private ComboBox cmbR_Wt;

		private TextBox txtD_Hr;

		private ComboBox cmbTransparency;

		private ComboBox cmbPlotControl;

		private ComboBox cmbStability;

		private TextBox txtFree;

		private CheckBox chkMiss;

		private CheckBox chkPredicted;

		private CheckBox chkSatellite;

		private CheckBox chkNotSeen;

		private CheckBox chkDoubleStar;

		private Label label30;

		private Label label29;

		private Label label28;

		private Label label27;

		private Label label26;

		private Label label25;

		private Label label24;

		private Label label23;

		private Label label22;

		private Label label31;

		private Label label36;

		private Label label35;

		private Label label34;

		private Label label33;

		private Label label32;

		private Label label37;

		private Label lblFreeLeft;

		private Button cmdAdd;

		private Button cmdReplace;

		private Button cmdDelete;

		private Button cmdUp;

		private Button cmdDown;

		private Label label38;

		private Button cmdRenumber;

		private ToolStripMenuItem updateHistoricalFileToolStripMenuItem;

		private ToolStripMenuItem sortObserverLinesToolStripMenuItem;

		private ToolStripMenuItem bySequenceNumberToolStripMenuItem;

		private ToolStripMenuItem byNameFieldToolStripMenuItem;

		private ToolStripMenuItem byLongitudeToolStripMenuItem;

		private ToolStripMenuItem byLatitudeToolStripMenuItem;

		private GroupBox grpManageObservers;

		private Panel panelTycho2;

		private Label label8;

		private Label label7;

		private TextBox txtTycComp;

		private TextBox txtTycSeqNum;

		private TextBox txtTycRegion;

		private Label lblTycho_UCAC;

		private ToolStripMenuItem plotToolStripMenuItem;

		private Label lblNumbers;

		internal Label lblCurrentSolution;

		internal Label lblMagDrop;

		private ToolStripMenuItem byPathDistanceToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private RadioButton optByPlanets;

		private ToolStripMenuItem pasteToolStripMenuItem;

		private ToolStripMenuItem eurasterEventAsNewEventToolStripMenuItem;

		private ToolStripMenuItem eurasterObservationToCurrentEventToolStripMenuItem;

		private ToolStripMenuItem newEventFromJPToolStripMenuItem;

		private ToolStripMenuItem newObservatToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator3;

		private ToolStripMenuItem listOffsetsToolStripMenuItem;

		private ToolStripMenuItem fromPredictionToolStripMenuItem;

		private ToolStripMenuItem possibleTimebaseCorrectionsToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator2;

		private ToolStripMenuItem renumberObservationsToolStripMenuItem;

		private ToolStripMenuItem theseAreMaintenaceFunctionsToolStripMenuItem;

		private ToolStripMenuItem byNameFieldskip2ToolStripMenuItem;

		internal Label lblMPC;

		private ToolStripMenuItem plotSitesInGoogleEarthToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator4;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator5;

		private ToolStripMenuItem copyObserverNamesToolStripMenuItem;

		private ToolStripMenuItem copyvalidObserverNamesToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator6;

		private ToolStripMenuItem newEventOBSFormatToolStripMenuItem;

		private ToolStripMenuItem newObservationLineoneOnlyOBSFormatToolStripMenuItem;

		private Panel panelNOMAD;

		private Label label40;

		private TextBox txtNOMADnumber;

		private TextBox txtNOMADzone;

		private ToolStripMenuItem alternativeStarIdentifiersToolStripMenuItem;

		private ToolStripMenuItem chordLengthsToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator7;

		private ToolStripMenuItem predictionLineToolStripMenuItem;

		private Panel panelUCAC4;

		private Label label41;

		private TextBox txtU4Number;

		private TextBox txtU4Zone;

		private ToolStripMenuItem relativePathDistancesToolStripMenuItem;

		private ToolStripMenuItem openMultipleFilesToolStripMenuItem;

		private ToolStripMenuItem skipToNextFileToolStripMenuItem;

		private ToolStripMenuItem satelliteIRDiametersToolStripMenuItem;

		private ToolStripMenuItem previousOccultationsOfThisStarToolStripMenuItem;

		private CheckBox chkShapeModelsOnly;

		private ToolStripMenuItem copyAllObserverDetailsToolStripMenuItem;

		private RadioButton optByClass;

		private ComboBox cmbAsteroidClasses;

		private ToolStripMenuItem convertMPCorbEtcToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator9;

		private Label lblinGaiaDR2;

		private Button cmdAddToHistorical;

		private NumericUpDown updnSNR;

		private Label label43;

		private Label label47;

		private Label label46;

		private Label label45;

		private Label label44;

		private Label label20;

		private Label label21;

		private Label label49;

		private ToolStripMenuItem transitionToolStripMenuItem;

		private ToolStripMenuItem convertObsToolStripMenuItem;

		private ToolStripMenuItem checkCountryStateToolStripMenuItem;

		private RadioButton optbyChords;

		private GroupBox grpManageHistorical;

		private ToolStripMenuItem ExportImport;

		private ToolStripSeparator toolStripSeparator13;

		private ToolStripMenuItem stellarDiameterFresnelDiffractionToolStripMenuItem;

		private ToolStripMenuItem asteroidLightCurveDataToolStripMenuItem;

		private ToolStripMenuItem solutionsToolStripMenuItem;

		private ToolStripMenuItem doubleStarsToolStripMenuItem;

		private ToolStripMenuItem listCurrentEventSolutionsToolStripMenuItem1;

		private ToolStripMenuItem deleteCurrentEventSolutionsToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator15;

		private ToolStripMenuItem satelliteToolStripMenuItem;

		private ToolStripMenuItem listCurrenteventSatelliteSolutionToolStripMenuItem;

		private ToolStripMenuItem deeteCurrenteventSatelliteSolutionToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator16;

		private ToolStripMenuItem shapeModelsToolStripMenuItem1;

		private ToolStripMenuItem listCurrenteventShapeModelsSolutionsToolStripMenuItem;

		private ToolStripMenuItem deleteCurrenteventShapemodelSolutionsToolStripMenuItem;

		private ToolStripMenuItem downloadShapeModelsToolStripMenuItem;

		private ToolStripMenuItem createNewExportFileToolStripMenuItem;

		private ToolStripMenuItem selectExportFileToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator17;

		private ToolStripMenuItem exportCurrentEventToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator18;

		private ToolStripSeparator toolStripSeparator19;

		private ToolStripMenuItem selectImportFileImportToolStripMenuItem;

		private ToolStripMenuItem toolStripMenuItemExportFile2;

		private ToolStripMenuItem toolStripMenuItemImportFile;

		private ToolStripSeparator toolStripSeparator20;

		private ToolStripMenuItem importSolutionsToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator21;

		private ToolStripMenuItem astrometricSolutionToolStripMenuItem;

		private ToolStripMenuItem astrometricSolutionToolStripMenuItem1;

		private ToolStripMenuItem listEventsInExportFileToolStripMenuItem;

		private ToolStripMenuItem openCommentsFormForCurrentEventToolStripMenuItem;

		private Label lblMagnitude;

		private ToolStripMenuItem helpOnExportImportToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator8;

		private ToolStripMenuItem newObservationObserverGroupXMLFormatToolStripMenuItem;

		private Panel panelGaiaCoords;

		private TextBox txtGaiaCoords;

		private Label label48;

		private ToolStripMenuItem updateStarPositionsToolStripMenuItem;

		private CheckBox chkUnseen;

		private ToolStripMenuItem lineForAsnUnseenBinaryasteroidPrimaryObjectToolStripMenuItem;

		private ToolTip toolTip;

		private ToolStripMenuItem gravitationalLightDeflectionToolStripMenuItem;

		private ToolStripMenuItem addParallaxMotionToolStripMenuItem;

		private Label label50;

		internal Label lblISAM;

		internal Label lblInDAMIT;

		private ToolStripMenuItem updateAsteroidMotionToolStripMenuItem;

		private ToolStripMenuItem eventsWhereDAMITHasBeenUpdatedToolStripMenuItem;

		private ToolStripMenuItem testGaiaCorrnToolStripMenuItem;

		private ToolStripMenuItem testGaiaDownloadToolStripMenuItem;

		private ToolStripMenuItem starDetailsToolStripMenuItem;

		private ToolStripMenuItem updateSolutionsToolStripMenuItem;

		private ToolStripMenuItem getEventsWithAssumedUncertsToolStripMenuItem;

		private ToolStripMenuItem addDiameterUncertaintiesToolStripMenuItem;

		private ToolStripMenuItem displayLightCurveToolStripMenuItem;

		private ToolStripMenuItem updateRUWEFrom2019ToolStripMenuItem;

		private Label label51;

		private ToolStripMenuItem updateAsteroidFitUncertaintiesToolStripMenuItem;

		private Label txtNumberOfLightCurves;

		private Label lblStarDiameter;

		internal Label lblLastUpdate;

		private CheckBox chkAutoOpenSave;

		private Button cmdPlusOne;

		private Label lblCurrentSelected;

		internal Label lblLastSaveTime;

		private ToolStripMenuItem astrometricSolutionIncludingatConjunctionSolutionToolStripMenuItem;

		private CheckBox chkRing;

		private Label lblStarIssues;

		private ToolStripMenuItem nearbyStarsToolStripMenuItem;

		private ToolStripMenuItem matchUnistellarObservationsToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator11;

		private ToolStripMenuItem copyEventRecordToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator10;

		private ToolStripMenuItem sortByDistanceWhenOpeningToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator12;

		private ToolStripMenuItem whenPastingSortByDistanceToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator14;

		private Button cmdIdentifyGstar;

		private ToolStripSeparator toolStripSeparator22;

		private ToolStripMenuItem setListsToAutodisplayToolStripMenuItem;

		private ToolStripMenuItem toolStripMenuItem1;

		private ToolStripMenuItem toolStripMenuItem2;

		private ToolStripMenuItem toolStripMenuItem3;

		private ToolStripMenuItem toolStripMenuItem5;

		private ToolStripMenuItem toolStripMenuItem4;

		private ToolStripMenuItem toolStripMenuItem6;

		private ToolStripMenuItem AutoNearbyStarstoolStripMenuItem7;

		private ToolStripMenuItem toolStripMenuItem8;

		private ToolStripMenuItem AutoStarDiametertoolStripMenuItem;

		private ToolStripMenuItem AutoLightCuveDatatoolStripMenuItem;

		private ToolStripMenuItem shapeModelFitsToolStripMenuItem;

		private ToolStripMenuItem doubleStarSolutionsToolStripMenuItem;

		private ToolStripMenuItem displayWDSInterferometricAndVariableStarDataToolStripMenuItem;

		private Button cmdCloseExpandedFreeText;

		private Label lblStarID;

		private CheckBox ChkUnfitted;

		private Panel panel3;

		private Panel panel4;

		private ToolStripMenuItem validateAllSiteAltitudesToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator23;

		private ToolStripMenuItem altitudeErrorsFindAndSaveToolStripMenuItem;

		private ToolStripMenuItem sortByPathDistanceRenumberToolStripMenuItem;

		private RadioButton optComets;

		private Panel pnlComet;

		private Label label39;

		private TextBox txtCometName;

		private TextBox txtCometNumber;

		private Panel pnlDAMIT_ISAM;

		private Label label42;

		private Label label52;

		private Label label53;

		private Button cmdAltitude;

		private ToolStripMenuItem removeAllMPCReferencesToolStripMenuItem;

		internal Label lblRecordNumber;

		private ToolStripSeparator toolStripSeparator24;

		private ToolStripMenuItem dELETIONModeToolStripMenuItem;

		private Button cmdDeleteEvent;

		private GroupBox grpDeletion;

		private Label lblRecNoDel;

		private Label label15;

		private Label label55;

		private Label lblDeleteName;

		private ToolStripMenuItem starChartToolStripMenuItem;

		private ToolStripMenuItem iSAMUpdateToUniqueOnlyToolStripMenuItem;

		private ToolStripMenuItem eventCoordinatesToolStripMenuItem;

		internal Button cmdUpdateHistorical;

		private ToolStripMenuItem mPCEditModeToolStripMenuItem;

		private TextBox txtMPCyear;

		private TextBox txtMPCmonth;

		private TextBox txtMPCday;

		private TextBox txtMPEC;

		private Label lblMPEC;

		private Panel pnlMPC;

		private ToolStripMenuItem displaySubmittedLightCurvesToolStripMenuItem;

		private ToolStripMenuItem doubleStarFormatErrorsToolStripMenuItem;

		private Button cmdPredict;

		private TextBox txtDay;

		private ToolStripMenuItem asteroidNumberFromDAMITToolStripMenuItem;

		private ToolStripMenuItem updateUncertaintiesToolStripMenuItem;

		private Label lblRUWE;

		private ToolStripMenuItem gaiaDoubleStarsnearbyToolStripMenuItem;

		private ToolStripMenuItem checkSubmittedFilesForOmissionsToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator25;

		private ToolStripSeparator toolStripSeparator26;

		private Label label54;

		private ToolStripSeparator toolStripSeparator28;

		private ToolStripMenuItem observerVelocityRelativeToAsteriodToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator27;

		private ToolStripMenuItem summaryDataToolStripMenuItem;

		private ToolStripMenuItem asteroidLightcurvePhotometryDatabaseToolStripMenuItem;

		private ToolStripMenuItem rotationLightCurvesBehrandToolStripMenuItem;

		private ToolStripMenuItem toolStripMenuItem7;

		private ToolStripMenuItem toolStripMenuItem9;

		private ToolStripMenuItem toolStripMenuItem10;

		private ToolStripMenuItem toolStripMenuItem11;

		private ToolStripMenuItem toolStripMenuItem13;

		private ToolStripMenuItem cometsToolStripMenuItem;

		private ToolStripMenuItem approximateRangeToolStripMenuItem;

		private ToolStripMenuItem informationRelatedToTheStarToolStripMenuItem;

		private ToolStripMenuItem informationRelatedToTheAsteroidToolStripMenuItem;

		private ToolStripMenuItem observerrelatedInformationToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator29;

		private ToolStripSeparator toolStripSeparator30;

		private ToolStripSeparator toolStripSeparator31;

		private ToolStripMenuItem lightCurvesToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator32;

		private ToolStripSeparator toolStripSeparator33;

		private ToolStripSeparator toolStripSeparator34;

		private ToolStripSeparator toolStripSeparator35;

		private ToolStripSeparator toolStripSeparator36;

		private TextBox txtMPC_ID;

		private Label label56;

		private ToolTip toolTipLightCurve;

		private ToolStripMenuItem astrometryHeldByTheMinorPlanetCenterToolStripMenuItem;

		private Panel pnlBinary;

		private Label lblBinary;

		private Label label57;

		private ToolStripMenuItem binaryAsteroidDetailsToolStripMenuItem;

		private ToolStripMenuItem binaryAsteroidsEstimateOrbitSizeOrPeriodToolStripMenuItem;

		private ToolStripMenuItem jPLSmallBodyDatabaseToolStripMenuItem;

		private ToolStripMenuItem fresnelDiffractionLightCurvesToolStripMenuItem;

		private ToolStripMenuItem starDiameterModelLightCurvesToolStripMenuItem;

		private ToolStripMenuItem asteroidmagsFromGAIAToolStripMenuItem;

		private ToolStripMenuItem whenOpeningToolStripMenuItem;

		private ToolStripMenuItem validateFilesWhenEventOrObserversPastedToolStripMenuItem;

		private ToolStripMenuItem cOMPILATIONModeToolStripMenuItem;

		private ToolStripMenuItem fileNameToolStripMenuItem;

		private ToolStripMenuItem setCompilationFileNameToolStripMenuItem;

		internal Button cmdAddToCompiled;

		private ToolStripSeparator toolStripSeparator38;

		private ToolStripSeparator toolStripSeparator37;

		private ToolStripMenuItem starDiameterFromVizieRToolStripMenuItem;

		internal TextBox txtAperture;

		internal TextBox txtLongDeg;

		internal TextBox txtAlt_m;

		internal TextBox txtLatSec;

		internal TextBox txtLatMin;

		internal TextBox txtLatDeg;

		internal TextBox txtLongSec;

		internal TextBox txtLongMin;

		internal ComboBox cmbTelescope;

		internal RadioButton optDMM;

		internal RadioButton optDDD;

		internal RadioButton optDMS;

		internal TextBox txtLongDDD;

		internal TextBox txtLatDDD;

		internal TextBox txtLongDmm;

		internal TextBox txtLatMM;

		internal TextBox txtLatDmm;

		internal TextBox txtLongMM;

		internal TextBox txtAlt_ft;

		internal TextBox txtStateCountry;

		internal TextBox txtLocatedNear;

		internal CheckBox chkEtAl;

		internal TextBox txtObserver2;

		internal TextBox txtObserver1;

		internal Button cmdHelpCountryCodes;

		internal RadioButton optMeters;

		private ToolStripMenuItem loadSaveObserverDetailsToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator39;

		internal ComboBox cmbDatum;

		internal ComboBox cmbMethod;

		internal ComboBox cmbTime;

		private Label cmdAutoCompleteObserver;

		private Label cmdCreateLightCurve;

		private ToolStripMenuItem eventDateStarAsteroidOccelmntPredictionXmlToolStripMenuItem;

		public ObservationsEditor()
		{
			//IL_02d1: Unknown result type (might be due to invalid IL or missing references)
			//IL_02db: Expected O, but got Unknown
			//IL_030e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0318: Expected O, but got Unknown
			InitializeComponent();
			((ToolStripItem)transitionToolStripMenuItem).set_Visible(Utilities.IsInVisualStudio);
			AppPath = Utilities.AppPath;
			((ListControl)cmbCatalogue).set_SelectedIndex(0);
			for (int i = 2000; i <= DateTime.Today.Year; i++)
			{
				if (i < 2018)
				{
					cmbYearRange.get_Items().Insert(0, (object)i.ToString());
					continue;
				}
				cmbYearRange.get_Items().Insert(0, (object)(i + " to June"));
				cmbYearRange.get_Items().Insert(0, (object)(i + "  from July"));
			}
			Button obj = cmdUpdateHistorical;
			Point location;
			((Control)cmdAddToCompiled).set_Location(location = ((Control)cmdAddToHistorical).get_Location());
			((Control)obj).set_Location(location);
			((ListControl)cmbNumberRange).set_SelectedIndex(0);
			((ListControl)cmbAlpha).set_SelectedIndex(0);
			if (DateTime.Today.Month < 2)
			{
				((ListControl)cmbYearRange).set_SelectedIndex(2);
			}
			else if (DateTime.Today.Month < 8)
			{
				((ListControl)cmbYearRange).set_SelectedIndex(1);
			}
			else
			{
				((ListControl)cmbYearRange).set_SelectedIndex(0);
			}
			((ListControl)cmbAsteroidClasses).set_SelectedIndex(0);
			((ListControl)cmbTelescope).set_SelectedIndex(0);
			((ListControl)cmbDatum).set_SelectedIndex(0);
			((Control)txtAlt_ft).set_Top(((Control)txtAlt_m).get_Top());
			Button obj2 = cmdAddToCompiled;
			((Control)cmdAddToHistorical).set_Location(location = ((Control)cmdUpdateHistorical).get_Location());
			((Control)obj2).set_Location(location);
			((Control)panelDDD).set_Top(((Control)panelDMS).get_Top());
			((Control)panelDMM).set_Top(((Control)panelDMS).get_Top());
			((Control)panelDDD).set_Left(((Control)panelDMS).get_Left());
			((Control)panelDMM).set_Left(((Control)panelDMS).get_Left());
			((ToolStripItem)plotSitesInGoogleEarthToolStripMenuItem).set_Enabled(Settings.Default.GoogleEarthInstalled);
			ToolStripMenuItem obj3 = validateFilesWhenEventOrObserversPastedToolStripMenuItem;
			bool gE_Alts_Auto;
			whenOpeningToolStripMenuItem.set_Checked(gE_Alts_Auto = Settings.Default.GE_Alts_Auto);
			obj3.set_Checked(gE_Alts_Auto);
			ReSetForm();
			VisibilityOfControls(HistoricalEnabled: false);
			((Control)txtTycComp).set_Text("1");
			SetMPC_visibility();
			((ToolStripItem)toolStripMenuItemExportFile2).set_Text("(  " + Settings.Default.ExportFile + "  )");
			((ToolStripItem)toolStripMenuItemImportFile).set_Text("(  " + Settings.Default.ImportFile + "  )");
			EditTimer = new Timer();
			EditTimer.set_Interval(1000);
			EditTimer.add_Tick((EventHandler)EditTimer_Tick);
			EditTimer.Start();
			MagDropTimer = new Timer();
			MagDropTimer.set_Interval(300);
			MagDropTimer.add_Tick((EventHandler)MagDropTimer_Tick);
			C[0] = Color.DarkRed;
			C[1] = Color.Red;
			C[2] = Color.DeepPink;
			C[3] = Color.Red;
			Loaded = true;
		}

		private void ObservationsEditor_Load(object sender, EventArgs e)
		{
			//IL_0483: Unknown result type (might be due to invalid IL or missing references)
			//IL_0489: Invalid comparison between Unknown and I4
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			if ((((Control)this).get_Left() + ((Control)this).get_Width() <= Screen.GetWorkingArea((Control)(object)this).Left) | (((Control)this).get_Right() >= Screen.GetWorkingArea((Control)(object)this).Right) | (((Control)this).get_Bottom() <= 0) | (((Control)this).get_Top() >= Screen.GetWorkingArea((Control)(object)this).Bottom))
			{
				((Control)this).set_Left((Screen.GetWorkingArea((Control)(object)this).Width - ((Control)this).get_Width()) / 2);
				((Control)this).set_Top((Screen.GetWorkingArea((Control)(object)this).Height - ((Control)this).get_Height()) / 2);
			}
			else
			{
				((Form)this).set_Location(Settings.Default.LocationAsterObsEditor);
			}
			if ((((Control)this).get_Width() < 100) | (((Control)this).get_Height() < 100))
			{
				((Control)this).set_Width(1093);
				((Control)this).set_Height(676);
				Settings.Default.SizeAsterEditor = (Point)((Form)this).get_Size();
			}
			else
			{
				((Form)this).set_Size((Size)Settings.Default.SizeAsterEditor);
			}
			if (!File.Exists(AppPath + "\\Resource Files\\AsteroidClasses.csv"))
			{
				http.DownloadAsteroidClassFile();
			}
			ReSetForm();
			AltitudeVisibility();
			DMSformatVisibility();
			((ListControl)cmbPlanets).set_SelectedIndex(0);
			((ListControl)cmbMoons).set_SelectedIndex(0);
			((ListControl)cmbD_Wt).set_SelectedIndex(0);
			((ListControl)cmbR_Wt).set_SelectedIndex(0);
			((ListControl)cmbMethod).set_SelectedIndex(0);
			((ListControl)cmbTime).set_SelectedIndex(0);
			((ListControl)cmbStability).set_SelectedIndex(0);
			((ListControl)cmbTransparency).set_SelectedIndex(0);
			((ListControl)cmbPlotControl).set_SelectedIndex(0);
			Button obj = cmdAddToCompiled;
			Button obj2 = cmdAddToHistorical;
			Button obj3 = cmdUpdateHistorical;
			ToolStripMenuItem obj4 = updateHistoricalFileToolStripMenuItem;
			ToolStripMenuItem obj5 = addToHistoricalFileToolStripMenuItem;
			bool administrator;
			((ToolStripItem)checkSubmittedFilesForOmissionsToolStripMenuItem).set_Enabled(administrator = Utilities.Administrator);
			bool flag;
			((ToolStripItem)obj5).set_Enabled(flag = administrator);
			bool flag2;
			((ToolStripItem)obj4).set_Enabled(flag2 = flag);
			bool flag3;
			((Control)obj3).set_Enabled(flag3 = flag2);
			bool enabled;
			((Control)obj2).set_Enabled(enabled = flag3);
			((Control)obj).set_Enabled(enabled);
			MessageForm messageForm = new MessageForm();
			((Control)messageForm.label).set_Text("Checking for new ISAM models");
			((Control)messageForm).Show();
			Application.DoEvents();
			if (Settings.Default.ISAM_DateLastChecked.AddMonths(3).CompareTo(DateTime.Now) < 0)
			{
				DisplayMPOccultations.LoadISAM_and_DAMIT_ids(UpdateISAM: true);
			}
			else
			{
				DisplayMPOccultations.LoadISAM_and_DAMIT_ids(UpdateISAM: false);
			}
			((Control)messageForm.label).set_Text("Reading observations file");
			Application.DoEvents();
			Data_and_Plots.Historical_AllEvents = new AllEvents();
			Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			updnSNR.set_DecimalPlaces(1);
			DisplayIndex(ForCurrentEvent: false);
			((Control)messageForm.label).set_Text("Reading Light curve files");
			Application.DoEvents();
			FillLightCurveInfo();
			((Form)messageForm).Close();
			CheckBox obj6 = chkAutoOpenSave;
			((Control)cmdPlusOne).set_Visible(enabled = Settings.Default.Administrator);
			((Control)obj6).set_Visible(enabled);
			chkAutoOpenSave.set_Checked(false);
			((Control)cmdPlusOne).set_Visible(chkAutoOpenSave.get_Checked());
			sortByDistanceWhenOpeningToolStripMenuItem.set_Checked(Settings.Default.Asteroid_Open_Sort);
			whenPastingSortByDistanceToolStripMenuItem.set_Checked(Settings.Default.Asteroid_Paste_Sort);
			ToolStripMenuItem obj7 = dELETIONModeToolStripMenuItem;
			((ToolStripItem)mPCEditModeToolStripMenuItem).set_Visible(enabled = Settings.Default.AdministratorGlobal);
			((ToolStripItem)obj7).set_Visible(enabled);
			((Control)grpDeletion).set_Visible(false);
			ToolStripMenuItem obj8 = cOMPILATIONModeToolStripMenuItem;
			ToolStripMenuItem obj9 = fileNameToolStripMenuItem;
			((ToolStripItem)setCompilationFileNameToolStripMenuItem).set_Visible(flag3 = Settings.Default.AdministratorGlobal);
			((ToolStripItem)obj9).set_Visible(enabled = flag3);
			((ToolStripItem)obj8).set_Visible(enabled);
			Quality_FileName = Settings.Default.QualityFileName;
			((ToolStripItem)fileNameToolStripMenuItem).set_Text("File name :  Quality_" + Quality_FileName + ".xml");
			DateTime value = new DateTime(1962, 1, 1);
			if (File.Exists(Utilities.AppPath + "\\Resource Files\\EOP_present.dat"))
			{
				value = File.GetLastWriteTime(Utilities.AppPath + "\\Resource Files\\EOP_present.dat");
			}
			DateTime value2 = DateTime.Now.AddDays(-90.0);
			if (((value.CompareTo(value2) < 0) | !File.Exists(Utilities.AppPath + "\\Resource Files\\EOP_2020plus.dat")) && (int)MessageBox.Show("The file of Earth Orientation Parameters is " + DateTime.Now.Subtract(value).Days + " days old\r\n\r\nFor accurate reductions you need to download the file before it is more than 90 days old\r\n\r\nDo you want to download the file now?", "Download EOP & USNO files", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6)
			{
				((Control)this).set_Cursor(Cursors.get_WaitCursor());
				ftp.Download_EOP_current(SupressMessages: true);
				((Control)this).set_Cursor(Cursors.get_Default());
			}
		}

		private void newToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ReSetForm();
			VisibilityOfControls(HistoricalEnabled: false);
			((ToolStripItem)saveToolStripMenuItem).set_Enabled(false);
			Current_OBS_File = "";
			ReadingMultipleOBSFiles = false;
			ReadingMultipleEurasterEntries = false;
			ReadingMultipleJPEntries = false;
			((ToolStripItem)skipToNextFileToolStripMenuItem).set_Visible(false);
			SetPasteEnabled(PasteEnabled: true);
			EventLoadedFromFile = false;
		}

		private void ReSetForm()
		{
			UpdatingData = true;
			EditorHasBeenActivated = false;
			((ToolStripItem)saveToolStripMenuItem).set_Enabled(false);
			CurrentHistoricalRecord = -1;
			Label obj = lblRecNoDel;
			string text;
			((Control)lblRecordNumber).set_Text(text = ".....");
			((Control)obj).set_Text(text);
			((Control)this).set_Text("Asteroid observations editor :");
			((Control)txtYear).set_Text(DateTime.Today.ToUniversalTime().Year.ToString());
			((Control)txtMonth).set_Text(DateTime.Today.ToUniversalTime().Month.ToString());
			((Control)txtDay).set_Text(DateTime.Today.ToUniversalTime().Day.ToString());
			((Control)txtMidT).set_Text("");
			((Control)cmdPredict).set_Enabled(false);
			((ListControl)cmbCatalogue).set_SelectedIndex(0);
			((Control)txtHip).set_Text("");
			TextBox obj2 = txtTycRegion;
			TextBox obj3 = txtTycSeqNum;
			string text2;
			((Control)txtTycComp).set_Text(text2 = "1");
			((Control)obj3).set_Text(text = text2);
			((Control)obj2).set_Text(text);
			TextBox obj4 = txtB1number;
			((Control)txtB1zone).set_Text(text = "");
			((Control)obj4).set_Text(text);
			((Control)lblStarIssues).set_Text("");
			((Control)lblStarID).set_Text("");
			((Control)lblRA).set_Text("RA =");
			((Control)lblDec).set_Text("Dec =");
			Label obj5 = lblMag;
			((Control)lblMagnitude).set_Text(text = "Mv =");
			((Control)obj5).set_Text(text);
			((Control)lblMotions).set_Text("dX = km/s, dY = km/sec");
			((Control)lblParallax).set_Text("= \"");
			((Control)lblDiameterKM).set_Text("Dia = km,  mas");
			((Control)lblMPC).set_Text("");
			((Control)lblMagDrop).set_Text("Mag. drop:-");
			((Control)lblCurrentSolution).set_Text("Current solution:");
			((Control)lblLastUpdate).set_Text("Added :    Updated :");
			optAsteroid.set_Checked(true);
			TextBox obj6 = txtAsteroidNumber;
			((Control)txtAsteroidName).set_Text(text = "");
			((Control)obj6).set_Text(text);
			((ListControl)cmbPlanets).set_SelectedIndex(0);
			((ListControl)cmbMoons).set_SelectedIndex(0);
			((Control)cmbHistorical).set_Text("");
			((ListControl)cmbHistorical).set_SelectedIndex(-1);
			ComboBox obj7 = cmbDatum;
			int selectedIndex;
			((ListControl)cmbTelescope).set_SelectedIndex(selectedIndex = 0);
			((ListControl)obj7).set_SelectedIndex(selectedIndex);
			RadioButton obj8 = optDMS;
			bool @checked;
			optMeters.set_Checked(@checked = true);
			obj8.set_Checked(@checked);
			TextBox obj9 = txtAlt_ft;
			TextBox obj10 = txtAlt_m;
			TextBox obj11 = txtAperture;
			TextBox obj12 = txtObserver1;
			TextBox obj13 = txtObserver2;
			TextBox obj14 = txtLocatedNear;
			string text3;
			((Control)txtStateCountry).set_Text(text3 = "");
			string text4;
			((Control)obj14).set_Text(text4 = text3);
			string text5;
			((Control)obj13).set_Text(text5 = text4);
			string text6;
			((Control)obj12).set_Text(text6 = text5);
			((Control)obj11).set_Text(text2 = text6);
			((Control)obj10).set_Text(text = text2);
			((Control)obj9).set_Text(text);
			chkEtAl.set_Checked(false);
			TextBox obj15 = txtLongDDD;
			TextBox obj16 = txtLongDeg;
			TextBox obj17 = txtLongDmm;
			TextBox obj18 = txtLongMin;
			TextBox obj19 = txtLongMM;
			((Control)txtLongSec).set_Text(text4 = "");
			((Control)obj19).set_Text(text5 = text4);
			((Control)obj18).set_Text(text6 = text5);
			((Control)obj17).set_Text(text2 = text6);
			((Control)obj16).set_Text(text = text2);
			((Control)obj15).set_Text(text);
			TextBox obj20 = txtLatDDD;
			TextBox obj21 = txtLatDeg;
			TextBox obj22 = txtLatDmm;
			TextBox obj23 = txtLatMin;
			TextBox obj24 = txtLatMM;
			((Control)txtLatSec).set_Text(text4 = "");
			((Control)obj24).set_Text(text5 = text4);
			((Control)obj23).set_Text(text6 = text5);
			((Control)obj22).set_Text(text2 = text6);
			((Control)obj21).set_Text(text = text2);
			((Control)obj20).set_Text(text);
			((Control)txtShift).set_Text("");
			((Control)txtFree).set_Text("");
			updnSNR.set_Value(0m);
			TextBox obj25 = txtD_Hr;
			TextBox obj26 = txtD_Min;
			TextBox obj27 = txtD_PEq;
			TextBox obj28 = txtD_Sec;
			((Control)txtD_Uncert).set_Text(text5 = "");
			((Control)obj28).set_Text(text6 = text5);
			((Control)obj27).set_Text(text2 = text6);
			((Control)obj26).set_Text(text = text2);
			((Control)obj25).set_Text(text);
			TextBox obj29 = txtR_Hr;
			TextBox obj30 = txtR_Min;
			TextBox obj31 = txtR_PEq;
			TextBox obj32 = txtR_Sec;
			((Control)txtR_Uncert).set_Text(text5 = "");
			((Control)obj32).set_Text(text6 = text5);
			((Control)obj31).set_Text(text2 = text6);
			((Control)obj30).set_Text(text = text2);
			((Control)obj29).set_Text(text);
			CheckBox obj33 = chkDoubleStar;
			CheckBox obj34 = chkMiss;
			CheckBox obj35 = chkNotSeen;
			CheckBox obj36 = chkPredicted;
			CheckBox obj37 = chkSatellite;
			CheckBox obj38 = chkUnseen;
			bool flag;
			chkRing.set_Checked(flag = false);
			bool flag2;
			obj38.set_Checked(flag2 = flag);
			bool flag3;
			obj37.set_Checked(flag3 = flag2);
			bool flag4;
			obj36.set_Checked(flag4 = flag3);
			bool flag5;
			obj35.set_Checked(flag5 = flag4);
			obj34.set_Checked(@checked = flag5);
			obj33.set_Checked(@checked);
			for (int num = EventDetails.ShapeData.Count - 1; num >= 0; num--)
			{
				EventDetails.ShapeData.Remove(EventDetails.ShapeData[num]);
			}
			EventDetails.ShapeModelFitted = false;
			NewEventForShapeModelling = true;
			Label obj39 = lblInDAMIT;
			Label obj40 = lblISAM;
			((Control)lblBinary).set_Visible(flag5 = false);
			((Control)obj40).set_Visible(@checked = flag5);
			((Control)obj39).set_Visible(@checked);
			((ToolStripItem)binaryAsteroidDetailsToolStripMenuItem).set_Enabled(false);
			for (int num2 = EventDetails.Doubles.Count - 1; num2 >= 0; num2--)
			{
				DoubleData doubleData = EventDetails.Doubles[num2];
				DoubleData doubleData2 = EventDetails.Doubles[num2];
				DoubleData doubleData3 = EventDetails.Doubles[num2];
				double num4 = (EventDetails.Doubles[num2].Sdev_Sep_Companion = 0.0);
				double num6 = (doubleData3.Sdev_PA_Companion = num4);
				double num9 = (doubleData.Sep_Companion = (doubleData2.PA_Companion = num6));
			}
			EventDetails.StarIsDouble = false;
			EventDetails.AsteroidHasSatellite = false;
			EventDetails.NumberOfSatellites = 0;
			lstObservations.get_Items().Clear();
			EventDetails.Observers.Clear();
			EventDetails.Parallax = 1.0;
			EventDetails.X = (EventDetails.Y = (EventDetails.CentreOfMass_Offset_X = (EventDetails.CentreOfMass_Offset_Y = (EventDetails.PA_Ellipse = 0.0))));
			EventDetails.X_Dia = (EventDetails.Y_Dia = 0.0001);
			for (int i = 0; i < 4; i++)
			{
				DoubleData doubleData4 = new DoubleData();
				doubleData4.Companion_Set = false;
				EventDetails.Doubles.Add(doubleData4);
			}
			EventDetails.Kepler2ID = 0L;
			EventDetails.MPCDate = (EventDetails.MPCNumber = (EventDetails.MPCsubmissionID = ""));
			Data_and_Plots.FirstTimePlot = true;
			Data_and_Plots.PlotForm.addShapeImageFromDAMIT_ISAMToolStripMenuItem.set_Checked(false);
			EventDetails.Quality = 0;
			((ListControl)Data_and_Plots.PlotForm.cmbQuality).set_SelectedIndex(0);
			((Control)Data_and_Plots.PlotForm.panelShapeModelControl).set_Visible(false);
			CheckBox chkShapeModelCentered = Data_and_Plots.PlotForm.chkShapeModelCentered;
			((Control)Data_and_Plots.PlotForm.chkShapeModelCentered).set_Enabled(@checked = false);
			chkShapeModelCentered.set_Checked(@checked);
			EventDetails.FlagForReview = 0;
			Data_and_Plots.PlotForm.Populate_ReviewList(0);
			Data_and_Plots.PlotForm.CollapseReviewList();
			Data_and_Plots.PlotForm.drawStarImageAtPlotCenterToolStripMenuItem.set_Checked(@checked = false);
			Data_and_Plots.CenterStarPlotOnPlot = @checked;
			if (LightCurveViewer != null)
			{
				LightCurveViewer.CloseThis();
			}
			UpdatingData = false;
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			VisibilityOfControls(HistoricalEnabled: false);
			if ((Current_OBS_File == null) | !Current_OBS_File.ToUpper().Contains(".XML"))
			{
				saveAsToolStripMenuItem_Click(sender, e);
			}
			else if (Current_OBS_File.Length < 1)
			{
				saveAsToolStripMenuItem_Click(sender, e);
			}
			else
			{
				Save_OBS_file();
			}
		}

		private void saveAsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_0059: Unknown result type (might be due to invalid IL or missing references)
			//IL_005f: Invalid comparison between Unknown and I4
			SaveFileDialog val = new SaveFileDialog();
			((FileDialog)val).set_Title("Specify XML file to Write observations to.");
			((FileDialog)val).set_Filter("OBS-xml file (*.xml)|*.xml");
			((FileDialog)val).set_FilterIndex(Settings.Default.OBS_File_LastIndex);
			val.set_OverwritePrompt(false);
			try
			{
				((FileDialog)val).set_InitialDirectory(Path.GetDirectoryName(Settings.Default.OBS_FileDirectory));
			}
			catch
			{
			}
			((FileDialog)val).set_FileName("");
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				VisibilityOfControls(HistoricalEnabled: false);
				Current_OBS_File = ((FileDialog)val).get_FileName();
				Settings.Default.OBS_File_LastName = Current_OBS_File;
				Settings.Default.OBS_File_LastIndex = ((FileDialog)val).get_FilterIndex();
				Settings.Default.OBS_FileDirectory = ((FileDialog)val).get_FileName();
				((Control)this).set_Text("Asteroid observations editor : " + Path.GetFileName(Current_OBS_File));
				Save_OBS_file();
			}
		}

		private void Save_OBS_file()
		{
			//IL_0057: Unknown result type (might be due to invalid IL or missing references)
			//IL_005d: Invalid comparison between Unknown and I4
			if (!GetAverageTimeOfEvents(out var AverageTime) || !(Math.Abs(AverageTime - EventDetails.MidT_forMotions) >= 0.2) || (int)MessageBox.Show(string.Format("The 'Approx Mid-time' setting should be = {0,1:f1} hrs\r\n - not " + ((Control)txtMidT).get_Text() + " hrs\r\n\r\nDo you want to continue with the save?\r\n\r\nIMPORTANT\r\nWhen you change this setting, you must also\r\nupdate the Asteroid details, by clicking the\r\n'Get Details' button in the Asteroid group.", AverageTime), "Check MidT", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				Data_and_Plots.ComputeSolution_and_UpDate_EventDetails();
				((Control)this).Focus();
				EventDetails.YearAdded = DateTime.Today.ToUniversalTime().Year;
				EventDetails.MonthAdded = DateTime.Today.ToUniversalTime().Month;
				EventDetails.DayAdded = DateTime.Today.ToUniversalTime().Day;
				AllEvents allEvents = new AllEvents();
				allEvents.EncodeAnEvent_inXML(NewEvent: true, 0);
				allEvents.WriteHistoricalObservationsFile(HistoricalFile: false, Current_OBS_File);
				((Control)lblLastSaveTime).set_ForeColor(Color.Green);
				StartTime = DateTime.Now;
			}
		}

		private void copyEventRecordToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Data_and_Plots.ComputeSolution_and_UpDate_EventDetails();
			((Control)this).Focus();
			AllEvents allEvents = new AllEvents();
			allEvents.EncodeAnEvent_inXML(NewEvent: true, 0);
			allEvents.WriteHistoricalObservationsFile(HistoricalFile: false, "");
		}

		private void addToHistoricalFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Add_Event_to_HistoricalFile();
		}

		private void Add_Event_to_HistoricalFile()
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			//IL_0040: Unknown result type (might be due to invalid IL or missing references)
			//IL_006a: Unknown result type (might be due to invalid IL or missing references)
			//IL_008f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0095: Invalid comparison between Unknown and I4
			//IL_00b7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00bd: Invalid comparison between Unknown and I4
			//IL_00df: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e5: Invalid comparison between Unknown and I4
			//IL_0159: Unknown result type (might be due to invalid IL or missing references)
			//IL_015f: Invalid comparison between Unknown and I4
			//IL_018d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0193: Invalid comparison between Unknown and I4
			//IL_01b2: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b8: Invalid comparison between Unknown and I4
			//IL_0397: Unknown result type (might be due to invalid IL or missing references)
			//IL_039d: Invalid comparison between Unknown and I4
			//IL_040a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0438: Unknown result type (might be due to invalid IL or missing references)
			//IL_043e: Invalid comparison between Unknown and I4
			//IL_04a8: Unknown result type (might be due to invalid IL or missing references)
			if ((int)MessageBox.Show("This will add the data for the currently displayed event to\r\nthe list of Historical observations held in memory.\r\n\r\nBefore adding the event, the Sites are plotted in GoogleEarth\r\nto confirm the site locations are correct.\r\n\r\nDo you wish to continue?", "Adding data", (MessageBoxButtons)1, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 2)
			{
				return;
			}
			if (EventDetails.Observers.Count < 1)
			{
				MessageBox.Show("There are no observations to be added to the Historical File", "No observations", (MessageBoxButtons)0, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return;
			}
			if (EventDetails.AsteroidNominalDiameter < 0.05)
			{
				MessageBox.Show("The diameter of the asteroid has not been set\r\n\r\nYou will need to click 'Get details' in the Asteroid box", "No diameter", (MessageBoxButtons)0, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return;
			}
			if (EventDetails.Quality == 0)
			{
				if ((int)MessageBox.Show("The Quality rating is set as 'No reliable position or size'.\r\n\r\nDo you want to continue with this rating?", "Has quality rating been set?", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
				{
					return;
				}
			}
			else if (EventDetails.Quality == 5)
			{
				if ((int)MessageBox.Show("The Quality rating is set as 'Short duration event. No astrometry'.\r\n\r\nDo you want to continue with this rating?", "Has quality rating been correctly set?", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
				{
					return;
				}
			}
			else if (EventDetails.Quality == 6 && (int)MessageBox.Show("The Quality rating is set as 'Only 1 star of a double. No astrometry'.\r\n\r\nDo you want to continue with this rating?", "Has quality rating been correctly set?", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			if ((EventDetails.ShapeData.Count != Data_and_Plots.NumberOfShapeModels() && (int)MessageBox.Show("There are " + (Data_and_Plots.NumberOfShapeModels() - EventDetails.ShapeData.Count) + " / " + Data_and_Plots.NumberOfShapeModels() + " shape models that have not been fitted to this event.\r\n\r\nDo you want to continue without fitting all shape models?", "Shape models have not been fitted", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7) || (CheckIfInHistoricalFile(out var ObserverNames) && (int)MessageBox.Show("An entry for this Asteroid, Star and Date exists,\r\nwith Observers: " + ObserverNames + "\r\n\r\nDo you want to create a new entry?", "Duplicate entry", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7))
			{
				return;
			}
			PlotSitesInGoogleEarth();
			if ((int)MessageBox.Show("The sites are being plotted in GoogleEarth.\r\n\r\nAre the site locations OK?", "GoogleEarth site check", (MessageBoxButtons)1, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 2)
			{
				return;
			}
			Data_and_Plots.ComputeSolution_and_UpDate_EventDetails();
			if (IsEuraster)
			{
				Data_and_Plots.EurasterEvent.OccEvents.Clear();
				Data_and_Plots.EurasterEvent.EncodeAnEvent_inXML(NewEvent: true, 0);
				Data_and_Plots.Historical_AllEvents.OccEvents.Add(Data_and_Plots.EurasterEvent.OccEvents[0]);
			}
			else if (IsJP)
			{
				Data_and_Plots.JapanEvent.OccEvents.Clear();
				Data_and_Plots.JapanEvent.EncodeAnEvent_inXML(NewEvent: true, 0);
				Data_and_Plots.Historical_AllEvents.OccEvents.Add(Data_and_Plots.JapanEvent.OccEvents[0]);
			}
			else
			{
				Data_and_Plots.SingleEvent.OccEvents.Clear();
				Data_and_Plots.SingleEvent.EncodeAnEvent_inXML(NewEvent: true, 0);
				Data_and_Plots.Historical_AllEvents.OccEvents.Add(Data_and_Plots.SingleEvent.OccEvents[0]);
			}
			Data_and_Plots.Historical_AllEvents.OccEvents.Sort();
			Data_and_Plots.Historical_AllEvents.WriteHistoricalObservationsFile(HistoricalFile: true, "");
			DisplayIndex(ForCurrentEvent: false);
			((Control)lblLastUpdate).set_Text("Added : " + EventDetails.YearAdded + " " + Utilities.ShortMonths[EventDetails.MonthAdded] + " " + EventDetails.DayAdded + "     Updated : " + EventDetails.YearEdited + " " + Utilities.ShortMonths[EventDetails.MonthEdited] + " " + EventDetails.DayEdited);
			StartTime = DateTime.Now;
			((Control)lblLastSaveTime).set_ForeColor(Color.Green);
			if (EventLoadedFromFile)
			{
				if ((int)MessageBox.Show("Do you want to move the source file:\r\n\r\n   " + Current_OBS_File + "\r\n\r\nto the 'Processed' subdirectory of the file's location?", "Move source file", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6)
				{
					string text = Path.GetDirectoryName(Current_OBS_File) + "\\Processed";
					string destFileName = text + "\\" + Path.GetFileName(Current_OBS_File);
					try
					{
						if (!Directory.Exists(text))
						{
							Directory.CreateDirectory(text);
						}
						File.Move(Current_OBS_File, destFileName);
					}
					catch
					{
						MessageBox.Show("The file\r\n  " + Current_OBS_File + "has not been moved", "Move error", (MessageBoxButtons)0, (MessageBoxIcon)16);
					}
				}
				else if ((int)MessageBox.Show("Do you want to move the source file:\r\n\r\n   " + Current_OBS_File + "\r\n\r\nto the 'Not added' subdirectory of the file's location?", "Move source file", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6)
				{
					string text = Path.GetDirectoryName(Current_OBS_File) + "\\Not added";
					string destFileName = text + "\\" + Path.GetFileName(Current_OBS_File);
					try
					{
						if (!Directory.Exists(text))
						{
							Directory.CreateDirectory(text);
						}
						File.Move(Current_OBS_File, destFileName);
					}
					catch
					{
						MessageBox.Show("The file\r\n  " + Current_OBS_File + "has not been moved", "Move error", (MessageBoxButtons)0, (MessageBoxIcon)16);
					}
				}
				EventLoadedFromFile = false;
			}
			if (ReadingMultipleOBSFiles)
			{
				ReadNextMultipleFile();
			}
			if (ReadingMultipleEurasterEntries & (CurrentEuraster < EurasterCount))
			{
				ImportEuraster(NewObservationForExistingEvent: false, EurasterEvents[CurrentEuraster]);
			}
			if (ReadingMultipleJPEntries & (CurrentJP < JPCount))
			{
				ImportJP(NewObservationForExistingEvents: false, JPEvents[CurrentJP]);
			}
		}

		private static bool CheckIfInHistoricalFile(out string ObserverNames)
		{
			ObserverNames = "";
			try
			{
				string value = EventDetails.Year + EventDetails.Month.ToString().PadLeft(3) + EventDetails.Day.ToString().PadLeft(3);
				string text = EventDetails.StarCat + " " + EventDetails.StarNumber;
				for (int i = 0; i < Data_and_Plots.Historical_AllEvents.OccEvents.Count; i++)
				{
					if (Data_and_Plots.Historical_AllEvents.OccEvents[i].IndexLine.Contains(value))
					{
						Data_and_Plots.Historical_AllEvents.GetAsteroidID(i, out var AsteroidNumber, out var AsteroidName);
						int.TryParse(AsteroidNumber, out var result);
						if (((EventDetails.AsteroidID.Trim() == AsteroidName) | (EventDetails.AsteroidNo == result)) && text == Data_and_Plots.Historical_AllEvents.GetStarID(i).Trim().Replace("-coords ", ""))
						{
							ObserverNames = Data_and_Plots.Historical_AllEvents.GetObserverNames(i);
							return true;
						}
					}
				}
			}
			catch
			{
			}
			return false;
		}

		private void VisibilityOfControls(bool HistoricalEnabled)
		{
			if (HistoricalEnabled)
			{
				((ToolStripItem)saveToolStripMenuItem).set_Enabled(false);
				ToolStripMenuItem obj = updateHistoricalFileToolStripMenuItem;
				bool visible;
				((Control)cmdUpdateHistorical).set_Visible(visible = true);
				((ToolStripItem)obj).set_Visible(visible);
				ToolStripMenuItem obj2 = newEventOBSFormatToolStripMenuItem;
				ToolStripMenuItem obj3 = addToHistoricalFileToolStripMenuItem;
				bool flag;
				((Control)cmdAddToHistorical).set_Visible(flag = false);
				((ToolStripItem)obj3).set_Visible(visible = flag);
				((ToolStripItem)obj2).set_Visible(visible);
			}
			else
			{
				((ToolStripItem)saveToolStripMenuItem).set_Enabled(true);
				ToolStripMenuItem obj4 = updateHistoricalFileToolStripMenuItem;
				bool visible;
				((Control)cmdUpdateHistorical).set_Visible(visible = false);
				((ToolStripItem)obj4).set_Visible(visible);
				ToolStripMenuItem obj5 = newEventOBSFormatToolStripMenuItem;
				ToolStripMenuItem obj6 = addToHistoricalFileToolStripMenuItem;
				bool flag;
				((Control)cmdAddToHistorical).set_Visible(flag = true);
				((ToolStripItem)obj6).set_Visible(visible = flag);
				((ToolStripItem)obj5).set_Visible(visible);
			}
			((Control)cmdAdd).set_Text("ADD  as new #" + $"{EventDetails.Observers.Count + 1:F0}");
			((Control)cmdReplace).set_Text("REPLACE  record #");
			((Control)cmdDelete).set_Text("DELETE  record #");
		}

		private void FillLightCurveInfo()
		{
			if (LightData.AsteroidIndex == null)
			{
				LightData.ReadMainFile(Index: true, ReRead: false, ShowReadErrorMessages: false);
			}
			if (LightData.AsteroidIndex.Count < 10)
			{
				LightData.ReadMainFile(Index: true, ReRead: false, ShowReadErrorMessages: false);
			}
		}

		private void GetEventDate()
		{
			if (!UpdatingData)
			{
				if (!int.TryParse(((Control)txtYear).get_Text(), out Data_and_Plots.Year))
				{
					Data_and_Plots.Year = 2000;
				}
				if (!int.TryParse(((Control)txtMonth).get_Text(), out Data_and_Plots.Month))
				{
					Data_and_Plots.Month = 1;
				}
				if (!int.TryParse(((Control)txtDay).get_Text(), out Data_and_Plots.Day))
				{
					Data_and_Plots.Day = 1;
				}
				if (!double.TryParse(((Control)txtMidT).get_Text(), out Data_and_Plots.TZero_forMotions))
				{
					Data_and_Plots.TZero_forMotions = 0.0;
				}
				EventDate = Utilities.JD_from_Date(Data_and_Plots.Year, Data_and_Plots.Month, Data_and_Plots.Day) + Data_and_Plots.TZero_forMotions / 24.0;
				PMPeriod = Utilities.BesselianYear(EventDate) - 2000.0;
			}
		}

		private void txtYear_TextChanged(object sender, EventArgs e)
		{
			GetEventDate();
			((Control)txtNumberOfLightCurves).set_Visible(false);
		}

		private void txtYear_Leave(object sender, EventArgs e)
		{
			//IL_0015: Unknown result type (might be due to invalid IL or missing references)
			if (!IsYearValid())
			{
				MessageBox.Show("You must enter a valid Year greater than 1800", "Invalid year", (MessageBoxButtons)0, (MessageBoxIcon)16);
				((Control)txtYear).Focus();
			}
			GetEventDate();
			if (!int.TryParse(((Control)txtYear).get_Text(), out var result))
			{
				result = 2000;
			}
			EventDetails.Year = result;
			((Control)txtNumberOfLightCurves).set_Visible(false);
		}

		private bool IsYearValid()
		{
			int.TryParse(((Control)txtYear).get_Text(), out var result);
			if (result < 1800)
			{
				return false;
			}
			return true;
		}

		private void txtMonth_TextChanged(object sender, EventArgs e)
		{
			GetEventDate();
			if (!int.TryParse(((Control)txtMonth).get_Text(), out var result))
			{
				result = 1;
			}
			EventDetails.Month = result;
			((Control)txtNumberOfLightCurves).set_Visible(false);
		}

		private void txtMonth_Leave(object sender, EventArgs e)
		{
			//IL_0015: Unknown result type (might be due to invalid IL or missing references)
			if (!IsMonthValid())
			{
				MessageBox.Show("You must enter a valid Month", "Invalid month", (MessageBoxButtons)0, (MessageBoxIcon)16);
				((Control)txtMonth).Focus();
				if (!int.TryParse(((Control)txtMonth).get_Text(), out var result))
				{
					result = 1;
				}
				EventDetails.Month = result;
			}
			GetEventDate();
			((Control)txtNumberOfLightCurves).set_Visible(false);
		}

		private bool IsMonthValid()
		{
			int.TryParse(((Control)txtMonth).get_Text(), out var result);
			if (result < 1)
			{
				return false;
			}
			return true;
		}

		private void txtDay_TextChanged(object sender, EventArgs e)
		{
			GetEventDate();
			if (!int.TryParse(((Control)txtDay).get_Text(), out var result))
			{
				result = 1;
			}
			EventDetails.Day = result;
			((Control)txtNumberOfLightCurves).set_Visible(false);
		}

		private void txtDay_Leave(object sender, EventArgs e)
		{
			//IL_0015: Unknown result type (might be due to invalid IL or missing references)
			if (!IsDayValid())
			{
				MessageBox.Show("You must enter a valid Day", "Invalid day", (MessageBoxButtons)0, (MessageBoxIcon)16);
				((Control)txtDay).Focus();
			}
			GetEventDate();
			if (!int.TryParse(((Control)txtDay).get_Text(), out var result))
			{
				result = 1;
			}
			EventDetails.Day = result;
			((Control)txtNumberOfLightCurves).set_Visible(false);
		}

		private bool IsDayValid()
		{
			int.TryParse(((Control)txtDay).get_Text(), out var result);
			if (result < 0)
			{
				return false;
			}
			return true;
		}

		private void txtMidT_TextChanged(object sender, EventArgs e)
		{
			if (MidTChanging)
			{
				MidTChanging = false;
				return;
			}
			GetEventDate();
			if (!double.TryParse(((Control)txtMidT).get_Text(), out var result))
			{
				result = 0.1;
			}
			result = (EventDetails.MidT_forMotions = (double)Convert.ToInt32(result * 10.0) / 10.0);
		}

		private void txtMidT_Leave(object sender, EventArgs e)
		{
			//IL_0015: Unknown result type (might be due to invalid IL or missing references)
			if (!IsHourValid())
			{
				MessageBox.Show("You must enter a valid hour that is greater than 0.0 and less than 24", "Invalid hour", (MessageBoxButtons)0, (MessageBoxIcon)16);
				((Control)txtMidT).Focus();
			}
			if (!double.TryParse(((Control)txtMidT).get_Text(), out var result))
			{
				result = 0.1;
			}
			result = (double)Convert.ToInt32(result * 10.0) / 10.0;
			if (result < 0.1)
			{
				result = 0.1;
			}
			else if (result > 23.9)
			{
				result = 23.9;
			}
			EventDetails.MidT_forMotions = result;
			MidTChanging = true;
			((Control)txtMidT).set_Text(string.Format("{0,3:f1}", result));
		}

		private bool IsHourValid()
		{
			double.TryParse(((Control)txtMidT).get_Text(), out var result);
			if (result < 0.1 || result > 23.9)
			{
				return false;
			}
			return true;
		}

		private void cmbCatalogue_SelectedIndexChanged(object sender, EventArgs e)
		{
			ShowNumberField(((ListControl)cmbCatalogue).get_SelectedIndex());
			((Control)cmdIdentifyGstar).set_Visible(((ListControl)cmbCatalogue).get_SelectedIndex() == 5);
		}

		private void ShowNumberField(int x)
		{
			((Control)panelHip).set_Visible(x == 0);
			((Control)panelTycho2).set_Visible(x == 1);
			((Control)panelUCAC4).set_Visible(x == 2);
			((Control)panelB1).set_Visible(x == 3);
			((Control)panelNOMAD).set_Visible(x == 4);
			Panel obj = panelGaiaCoords;
			bool visible;
			((Control)cmdIdentifyGstar).set_Visible(visible = x == 5);
			((Control)obj).set_Visible(visible);
		}

		private void cmdIdentifyGstar_Click(object sender, EventArgs e)
		{
			//IL_00e0: Unknown result type (might be due to invalid IL or missing references)
			//IL_010b: Unknown result type (might be due to invalid IL or missing references)
			//IL_017c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0182: Invalid comparison between Unknown and I4
			//IL_0331: Unknown result type (might be due to invalid IL or missing references)
			//IL_034b: Unknown result type (might be due to invalid IL or missing references)
			if (Occult.Star_Catalogues.GetStarPosition.GetGaiaPosition(((Control)txtGaiaCoords).get_Text(), double.Parse(((Control)txtYear).get_Text()) - 2000.0, 2.0, 10.0, FilterUsingStarMag: false, LimitUsingStarMag: false, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var TruncatedString))
			{
				if (((Control)txtGaiaCoords).get_Text() != TruncatedString)
				{
					string text = ((Control)txtGaiaCoords).get_Text();
					((Control)txtGaiaCoords).set_Text(TruncatedString);
					Application.DoEvents();
					MessageBox.Show("J coordinates for the star have been corrected from\r\n    " + text + "\r\n        to\r\n    " + TruncatedString + "\r\n\r\nThose coordinates need to be reviewed. The required format is\r\n\r\n hhmmss.ss-ddmmss.s  OR hhmmss.s-ddmmss\r\n\r\nwith '+' instead of '-' as needed", "J Coords corrected", (MessageBoxButtons)0, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				}
				int catID = Gaia.catID;
				uint catNumber = Gaia.catNumber;
				if (catID == 0)
				{
					MessageBox.Show("Star is in the Occult Gaia file as a coordinate-based star. No change to the star identifier can be applied.\r\n\r\n[This should mainly occur when the magnitude of the star is too faint for the Gaia catalogue]", "G-star coords correct", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
					return;
				}
				string text2 = "";
				if (catID == 2)
				{
					text2 = " Hipparcos ";
				}
				else if (catID == 3 || catID == 4 || catID == 5)
				{
					text2 = " Tycho-2 ";
				}
				else if (catID == 6 || catID == 7 || catID == 8)
				{
					text2 = " UCAC4 ";
				}
				if ((int)MessageBox.Show("The star has been identified as a" + text2 + "star \r\nin the Occult Gaia file.\r\n\r\nDo you want to update the star identifier", "G-star identified", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 7)
				{
					return;
				}
				if (catID == 2)
				{
					((ListControl)cmbCatalogue).set_SelectedIndex(0);
					((Control)txtHip).set_Text(catNumber.ToString());
				}
				else if (catID == 3 || catID == 4 || catID == 5)
				{
					((ListControl)cmbCatalogue).set_SelectedIndex(1);
					((Control)txtTycRegion).set_Text((catNumber / 100000u).ToString().PadLeft(3, '0').ToString());
					((Control)txtTycSeqNum).set_Text((catNumber % 100000u).ToString().PadLeft(6, '0').ToString());
					((Control)txtTycComp).set_Text((catID - 2).ToString());
				}
				else if (catID == 6 || catID == 7 || catID == 8)
				{
					((ListControl)cmbCatalogue).set_SelectedIndex(2);
					((Control)txtU4Zone).set_Text((catNumber / 1000000u).ToString().PadLeft(3, '0').ToString());
					((Control)txtU4Number).set_Text((catNumber % 1000000u).ToString().PadLeft(6, '0').ToString());
				}
				ShowNumberField(((ListControl)cmbCatalogue).get_SelectedIndex());
				bool flag = true;
				if (optAsteroid.get_Checked())
				{
					if (((Control)txtAsteroidNumber).get_Text().Trim() == "")
					{
						flag = false;
					}
				}
				else if (optPlanet.get_Checked() && ((ListControl)cmbPlanets).get_SelectedIndex() < 1)
				{
					flag = false;
				}
				if (flag)
				{
					GetAsteroidPosition(UseExistingStarPosition: false);
				}
				else
				{
					GetStarPosition(ForStarDiameter: false, UpdateStarOnly: false);
				}
				MessageBox.Show("The star identifier has been updated\r\n\r\nDon't forget to save the event.", "Updated star and asteroid", (MessageBoxButtons)0, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
			}
			else
			{
				MessageBox.Show("No star found in Occult Gaia files\r\n\r\n Have you provided the correct star ccordinates.\r\n The format is:   hhmmss.s±ddmmss OR  hhmmss.ss±ddmmss.s", "Star not found", (MessageBoxButtons)0, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
			}
		}

		private void cmdGetStarPos_Click(object sender, EventArgs e)
		{
			GetStarDetails();
		}

		private void GetStarDetails()
		{
			//IL_0092: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d5: Unknown result type (might be due to invalid IL or missing references)
			//IL_016a: Unknown result type (might be due to invalid IL or missing references)
			if (((ListControl)cmbCatalogue).get_SelectedIndex() == 5)
			{
				string text = ((Control)txtGaiaCoords).get_Text();
				int num = text.IndexOf("+");
				if (num < 0)
				{
					num = text.IndexOf("-");
				}
				if (num > 0)
				{
					string text2 = text.Substring(0, num);
					int num2 = text2.IndexOf('.');
					string text3 = text.Substring(num + 1);
					if ((text2.Length < 8 || num2 != 6) | (text3.Length < 6))
					{
						MessageBox.Show("The Gaia star identifier\r\n" + text + "\r\ndoes not comply with the required format.\r\n\r\nPlease change the coordinate format to match\r\nhhmmss.s+ddmmss or \r\nhhmmss.ss+ddmmss.s", "Invalid Gaia identifier", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
						return;
					}
				}
			}
			if (!IsYearValid() | !IsMonthValid() | !IsDayValid() | !IsHourValid())
			{
				MessageBox.Show("You must set a valid Year, Month, Day and Hour", "Set a vaild date", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				((Control)txtYear).Focus();
			}
			else
			{
				GetStarPosition(ForStarDiameter: false, UpdateStarOnly: false);
			}
			((Control)lblRUWE).set_Text(string.Format("RUWE = {0,1:f2}", EventDetails.StarReliability));
			if (EventDetails.StarReliability > 1.4)
			{
				((Control)lblRUWE).set_ForeColor(Color.Red);
			}
			else
			{
				((Control)lblRUWE).set_ForeColor(Color.Green);
			}
			if (GetPreviousOccultsOfStar(Data_and_Plots.StarRA, Data_and_Plots.StarDec, DisplayOnlyIfFound: true, out var _))
			{
				MessageBox.Show("A previous occultation of this star exists", "Previous occultation", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
			}
		}

		private void GetStarPosition(bool ForStarDiameter, bool UpdateStarOnly)
		{
			GetStarPosition(ForStarDiameter, UpdateStarOnly, UseExistingStarPosition: false);
		}

		private void GetStarPosition(bool ForStarDiameter, bool UpdateStarOnly, bool UseExistingStarPosition)
		{
			//IL_01c0: Unknown result type (might be due to invalid IL or missing references)
			//IL_0230: Unknown result type (might be due to invalid IL or missing references)
			//IL_02e2: Unknown result type (might be due to invalid IL or missing references)
			//IL_0393: Unknown result type (might be due to invalid IL or missing references)
			//IL_042b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0561: Unknown result type (might be due to invalid IL or missing references)
			//IL_06d5: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a74: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a7a: Invalid comparison between Unknown and I4
			//IL_0b2a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b59: Unknown result type (might be due to invalid IL or missing references)
			bool flag = true;
			double RA = 0.0;
			double pmRA = 0.0;
			double Dec = 0.0;
			double pmDec = 0.0;
			double Parallax_asec = 0.0;
			double num = 0.0;
			double MagV = 0.0;
			double MagB = 0.0;
			double MagR = 0.0;
			double Epoch = 2000.0;
			double RV = 0.0;
			double UncertRA = 0.0;
			double UncertDec = 0.0;
			double UncertPMRA = 0.0;
			double UncertPMDec = 0.0;
			double StarDiameter_mas = 0.0;
			double StarReliability = 0.0;
			int DuplicateSource = -1;
			int NoGaiaPM = -1;
			int GaiaPMfromUCAC = -1;
			int GaiaVersion = 0;
			ulong GaiaSourceID = 0uL;
			bool flag2 = false;
			bool UsedGaia = false;
			string SourceFile = "";
			string CorrectStarIdentifier = "";
			int result3;
			int result5;
			string Basis;
			if (((ListControl)cmbCatalogue).get_SelectedIndex() == 0)
			{
				if (!int.TryParse(((Control)txtHip).get_Text(), out var result))
				{
					return;
				}
				flag2 = Occult.Star_Catalogues.GetStarPosition.GetHipparcosPosition(result, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out Parallax_asec, out Epoch, out UsedGaia, out GaiaVersion, out GaiaSourceID, out SourceFile, out RV, out UncertRA, out UncertDec, out UncertPMRA, out UncertPMDec, out StarDiameter_mas, out StarReliability, out DuplicateSource, out NoGaiaPM, out GaiaPMfromUCAC, out CorrectStarIdentifier);
				num = Parallax_asec / 3600.0 / (180.0 / Math.PI);
				if (!flag2)
				{
					Clipboard.SetText("HIP " + ((Control)txtHip).get_Text() + " = " + CorrectStarIdentifier);
					MessageBox.Show("HIP " + ((Control)txtHip).get_Text() + " is held in the Occult Gaia catalogue as\r\n\r\n" + CorrectStarIdentifier + "\r\n\r\nThe star identifier MUST be set to this star, and\r\nthe 'Get Details' button in BOTH the   STAR   and   ASTEROID\r\nboxes must be clicked.\r\n\r\nThe correct star number has been copied to the Clipboard", "HIP star identifier", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
					return;
				}
			}
			else if (((ListControl)cmbCatalogue).get_SelectedIndex() == 1)
			{
				if (!int.TryParse(((Control)txtTycRegion).get_Text(), out var result2))
				{
					flag = false;
				}
				if (!int.TryParse(((Control)txtTycSeqNum).get_Text(), out result3))
				{
					flag = false;
				}
				if (!int.TryParse(((Control)txtTycComp).get_Text(), out var result4))
				{
					flag = false;
				}
				if (!flag)
				{
					MessageBox.Show("The Tycho2 star number is incorrectly specified", "Invalid Star Number", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
					return;
				}
				flag2 = Occult.Star_Catalogues.GetStarPosition.GetTycho2Position(result2, result3, result4, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out Parallax_asec, out Epoch, out UsedGaia, out GaiaVersion, out GaiaSourceID, out SourceFile, out RV, out UncertRA, out UncertDec, out UncertPMRA, out UncertPMDec, out StarDiameter_mas, out StarReliability, out DuplicateSource, out NoGaiaPM, out GaiaPMfromUCAC);
				num = Parallax_asec / 3600.0 / (180.0 / Math.PI);
			}
			else if (((ListControl)cmbCatalogue).get_SelectedIndex() == 2)
			{
				if (!int.TryParse(((Control)txtU4Zone).get_Text(), out result5))
				{
					flag = false;
				}
				if (!int.TryParse(((Control)txtU4Number).get_Text(), out result3))
				{
					flag = false;
				}
				if (!flag)
				{
					MessageBox.Show("The UCAC4 star number is incorrectly specified", "Invalid Star Number", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
					return;
				}
				flag2 = Occult.Star_Catalogues.GetStarPosition.GetUCAC4Position(result5, result3, Parallax_IfNotInGaia_TryHipparcos: false, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out Parallax_asec, out Epoch, out UsedGaia, out GaiaVersion, out GaiaSourceID, out SourceFile, out RV, out UncertRA, out UncertDec, out UncertPMRA, out UncertPMDec, out StarDiameter_mas, out StarReliability, out DuplicateSource, out NoGaiaPM, out GaiaPMfromUCAC);
				num = Parallax_asec / 3600.0 / (180.0 / Math.PI);
			}
			else if (((ListControl)cmbCatalogue).get_SelectedIndex() == 3)
			{
				if (!int.TryParse(((Control)txtB1zone).get_Text(), out result5))
				{
					flag = false;
				}
				if (!int.TryParse(((Control)txtB1number).get_Text(), out result3))
				{
					flag = false;
				}
				if (!flag)
				{
					MessageBox.Show("The USNO-B1 star number is incorrectly specified", "Invalid Star Number", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
					return;
				}
				flag2 = Occult.Star_Catalogues.GetStarPosition.GetB1Position(result5, result3, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out Parallax_asec, out Epoch, out UsedGaia, out GaiaVersion, out GaiaSourceID, out SourceFile, out RV, out UncertRA, out UncertDec, out UncertPMRA, out UncertPMDec, out StarDiameter_mas, out StarReliability, out DuplicateSource, out NoGaiaPM, out GaiaPMfromUCAC);
			}
			else if (((ListControl)cmbCatalogue).get_SelectedIndex() == 4)
			{
				if (!int.TryParse(((Control)txtNOMADzone).get_Text(), out result5))
				{
					flag = false;
				}
				if (!int.TryParse(((Control)txtNOMADnumber).get_Text(), out result3))
				{
					flag = false;
				}
				if (!flag)
				{
					MessageBox.Show("The NOMAD star number is incorrectly specified", "Invalid Star Number", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
					return;
				}
				flag2 = Occult.Star_Catalogues.GetStarPosition.GetNOMAD_Full_Position(result5, result3, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out Parallax_asec, out Epoch, out UsedGaia, out GaiaVersion, out GaiaSourceID, out SourceFile, out RV, out UncertRA, out UncertDec, out UncertPMRA, out UncertPMDec, out StarDiameter_mas, out StarReliability, out DuplicateSource, out NoGaiaPM, out GaiaPMfromUCAC);
			}
			else if (((ListControl)cmbCatalogue).get_SelectedIndex() == 5)
			{
				flag2 = Occult.Star_Catalogues.GetStarPosition.GetGaiaPosition(((Control)txtGaiaCoords).get_Text(), double.Parse(((Control)txtYear).get_Text()) - 2000.0, 2.0, 10.0, FilterUsingStarMag: false, LimitUsingStarMag: false, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out Parallax_asec, out Epoch, out UsedGaia, out GaiaVersion, out GaiaSourceID, out SourceFile, out RV, out UncertRA, out UncertDec, out UncertPMRA, out UncertPMDec, out StarDiameter_mas, out StarReliability, out DuplicateSource, out NoGaiaPM, out GaiaPMfromUCAC, out Basis, out var TruncatedString);
				if (((Control)txtGaiaCoords).get_Text() != TruncatedString)
				{
					string text = ((Control)txtGaiaCoords).get_Text();
					((Control)txtGaiaCoords).set_Text(TruncatedString);
					Application.DoEvents();
					MessageBox.Show("J coordinates for the star have been corrected from\r\n    '" + text + "'\r\n        to\r\n    '" + TruncatedString + "'\r\n\r\nThey need to be reviewed. The required format is\r\n\r\n hhmmss.ss-ddmmss.s  OR hhmmss.s-ddmmss\r\n\r\nwith '+' instead of '-' as needed", "J Coords corrected", (MessageBoxButtons)0, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				}
				num = Parallax_asec / 3600.0 / (180.0 / Math.PI);
			}
			if (!flag2 | (SourceFile == "NOMAD") | (SourceFile == "UCAC4"))
			{
				List<StarEntry> list = new List<StarEntry>();
				double RA_ID_deg = 0.0;
				double Dec_ID_deg = 0.0;
				if (SourceFile == "NOMAD")
				{
					RA_ID_deg = RA * (180.0 / Math.PI) / 15.0;
					Dec_ID_deg = Dec * (180.0 / Math.PI);
					flag2 = false;
				}
				else if ((SourceFile == "UCAC4") | (SourceFile == "TychoGaia"))
				{
					RA_ID_deg = RA * (180.0 / Math.PI) / 15.0;
					Dec_ID_deg = Dec * (180.0 / Math.PI);
				}
				else if (((ListControl)cmbCatalogue).get_SelectedIndex() == 5)
				{
					if (Occult.Star_Catalogues.GetStarPosition.StarCoords_to_RA_Dec(((Control)txtGaiaCoords).get_Text(), out RA_ID_deg, out Dec_ID_deg, out var TruncatedString2) && TruncatedString2 != ((Control)txtGaiaCoords).get_Text())
					{
						string text2 = ((Control)txtGaiaCoords).get_Text();
						((Control)txtGaiaCoords).set_Text(TruncatedString2);
						Application.DoEvents();
						MessageBox.Show("J coordinates for the star have been corrected from\r\n    '" + text2 + "'\r\n        to\r\n    '" + TruncatedString2 + "'\r\n\r\nThose coordinates need to be reviewed. The required format is\r\n\r\n hhmmss.ss-ddmmss.s  OR hhmmss.s-ddmmss\r\n\r\nwith '+' instead of '-' as needed", "J Coords corrected", (MessageBoxButtons)0, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
					}
					RA_ID_deg /= 15.0;
				}
				else if (UpdatingOldFile)
				{
					RA_ID_deg = EventDetails.RA_Star_2000 * (180.0 / Math.PI) / 15.0;
					Dec_ID_deg = EventDetails.Dec_Star_2000 * (180.0 / Math.PI);
				}
				MessageForm messageForm = new MessageForm();
				((Control)messageForm.label).set_Text("Downloading star data from VizieR");
				((Control)messageForm).Show();
				Application.DoEvents();
				list = CatalogQuery.GetStarsInRegion(RA_ID_deg, Dec_ID_deg, 4, VizierEquinox.J2000, "I/350", VizierMirror.France);
				((Form)messageForm).Close();
				if (list.Count > 0)
				{
					int index = 0;
					for (int i = 1; i < list.Count; i++)
					{
						if (list[index].Mv > list[i].Mv)
						{
							index = i;
						}
					}
					RA = list[index].RACat * 15.0 / (180.0 / Math.PI);
					Dec = list[index].DECat / (180.0 / Math.PI);
					if (!double.IsNaN(list[index].pmRA))
					{
						pmRA = list[index].pmRA * 15.0 * Math.Cos(Dec) / 3600.0 / (180.0 / Math.PI);
					}
					if (!double.IsNaN(list[index].pmDEC))
					{
						pmDec = list[index].pmDEC / 3600.0 / (180.0 / Math.PI);
					}
					MagB = list[index].Mb;
					MagV = list[index].Mv;
					MagR = list[index].Mr;
					num = list[index].Parallax / 3600000.0 / (180.0 / Math.PI);
					if (num < 0.0)
					{
						num = 0.0;
					}
					Epoch = list[index].EpochRA;
					UsedGaia = true;
					GaiaVersion = 3;
					GaiaSourceID = list[index].SourceID;
					SourceFile = "";
					RV = list[index].RV;
					UncertRA = list[index].e_RA * 1000.0;
					UncertDec = list[index].e_DEC * 1000.0;
					if (!double.IsNaN(list[index].e_pmRA))
					{
						UncertPMRA = list[index].e_pmRA * 1000.0;
					}
					if (!double.IsNaN(list[index].e_pmDEC))
					{
						UncertPMDec = list[index].e_pmDEC * 1000.0;
					}
					StarDiameter_mas = Utilities.StarDiameter_CHARM2_CADARS(RA * (180.0 / Math.PI), Dec * (180.0 / Math.PI), MagV, MagB, MagR, out Basis, out var _, out var _);
					StarReliability = list[index].RUWE;
					DuplicateSource = list[index].DuplicateSource;
					NoGaiaPM = Convert.ToInt32(list[index].RUWE == 0.0);
					GaiaPMfromUCAC = 0;
					if ((((ListControl)cmbCatalogue).get_SelectedIndex() == 2) & double.IsNaN(list[index].e_pmRA))
					{
						GaiaPMfromUCAC = 1;
					}
					flag2 = true;
				}
			}
			Application.DoEvents();
			if (!UseExistingStarPosition)
			{
				if (GaiaVersion != 9 && ((Epoch != 2016.0) & !UpdatingOldFile) && (int)MessageBox.Show("The position you have retrieved has not come from Gaia DR3\r\n\r\nThis could be because the star is not in DR3. Or it could \r\nbe because the star is fainter than the version of Gaia DR3\r\nyou have on your computer. \r\n\r\nDo you want to try and retrieve a Gaia DR3 position from VizieR?", "Get DR3 position", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6)
				{
					string text3 = "";
					string text4 = "";
					string label = "";
					string gRA = "";
					string gdec = "";
					if (((ListControl)cmbCatalogue).get_SelectedIndex() == 2)
					{
						text3 = ((Control)txtU4Zone).get_Text();
						text4 = ((Control)txtU4Number).get_Text();
						label = "Get UCAC4 position";
					}
					else if (((ListControl)cmbCatalogue).get_SelectedIndex() == 5)
					{
						gRA = ((Control)txtGaiaCoords).get_Text().Substring(0, 8);
						gdec = ((Control)txtGaiaCoords).get_Text().Substring(8);
					}
					else
					{
						if ((text3 == "") | (text4 == ""))
						{
							MessageBox.Show("You need to specify the star by its J coordinates");
							return;
						}
						text3 = "0";
						text4 = "0";
						label = "Manual entry needed";
					}
					Gaia_EDR3FromVizieR gaia_EDR3FromVizieR = new Gaia_EDR3FromVizieR(text3, text4, label, gRA, gdec);
					try
					{
						((Form)gaia_EDR3FromVizieR).ShowDialog();
						if (gaia_EDR3FromVizieR.SelectedStar >= 0)
						{
							RA = gaia_EDR3FromVizieR.stars[gaia_EDR3FromVizieR.SelectedStar].RACat * 15.0 / (180.0 / Math.PI);
							Dec = gaia_EDR3FromVizieR.stars[gaia_EDR3FromVizieR.SelectedStar].DECat / (180.0 / Math.PI);
							pmRA = gaia_EDR3FromVizieR.stars[gaia_EDR3FromVizieR.SelectedStar].pmRA * 15.0 * Math.Cos(Dec) / 3600.0 / (180.0 / Math.PI);
							pmDec = gaia_EDR3FromVizieR.stars[gaia_EDR3FromVizieR.SelectedStar].pmDEC / 3600.0 / (180.0 / Math.PI);
							MagV = gaia_EDR3FromVizieR.stars[gaia_EDR3FromVizieR.SelectedStar].Mv;
							MagB = gaia_EDR3FromVizieR.stars[gaia_EDR3FromVizieR.SelectedStar].Mb;
							MagR = gaia_EDR3FromVizieR.stars[gaia_EDR3FromVizieR.SelectedStar].Mr;
							num = gaia_EDR3FromVizieR.stars[gaia_EDR3FromVizieR.SelectedStar].Parallax / 3600000.0 / (180.0 / Math.PI);
							GaiaVersion = 3;
							if (num < 0.0)
							{
								num = 0.0;
							}
							RV = gaia_EDR3FromVizieR.stars[gaia_EDR3FromVizieR.SelectedStar].RV;
							Epoch = gaia_EDR3FromVizieR.stars[gaia_EDR3FromVizieR.SelectedStar].EpochRA;
							UsedGaia = true;
							flag2 = true;
						}
					}
					finally
					{
						((IDisposable)gaia_EDR3FromVizieR)?.Dispose();
					}
				}
				if (flag2)
				{
					if (ForStarDiameter)
					{
						StarRA2000 = RA;
						StarDec2000 = Dec;
						Mv = MagV;
						Mp = MagB;
						return;
					}
					GetEventDate();
					Utilities.ProperMotion(RA, Dec, pmRA, pmDec, num, RV, PMPeriod - (Epoch - 2000.0), out var RAatEpoch, out var DecatEpoch);
					Utilities.StellarParallaxCoefficients(EventDetails.JD_EventDate, RAatEpoch, DecatEpoch, out var ParallaxCoeff_RA, out var ParallaxCoeff_Dec);
					RAatEpoch += num * ParallaxCoeff_RA;
					DecatEpoch += num * ParallaxCoeff_Dec;
					switch (((ListControl)cmbCatalogue).get_SelectedIndex())
					{
					case 0:
						EventDetails.StarCat = "HIP";
						EventDetails.StarNumber = ((Control)txtHip).get_Text().Trim();
						break;
					case 1:
						EventDetails.StarCat = "Tycho2";
						EventDetails.StarNumber = ((Control)txtTycRegion).get_Text().Trim().PadLeft(4, '0') + "-" + ((Control)txtTycSeqNum).get_Text().Trim().PadLeft(5, '0') + "-" + ((Control)txtTycComp).get_Text().Trim().PadLeft(1, '1');
						break;
					case 2:
						EventDetails.StarCat = "UCAC4";
						EventDetails.StarNumber = ((Control)txtU4Zone).get_Text().Trim().PadLeft(3, '0')
							.Substring(0, 3) + "-" + ((Control)txtU4Number).get_Text().Trim().PadLeft(6, '0')
							.Substring(0, 6);
						break;
					case 3:
						EventDetails.StarCat = "USNO-B1";
						EventDetails.StarNumber = ((Control)txtB1zone).get_Text().Trim().PadLeft(4, '0') + "-" + ((Control)txtB1number).get_Text().Trim().PadLeft(7, '0');
						break;
					case 4:
						EventDetails.StarCat = "NOMAD";
						EventDetails.StarNumber = ((Control)txtNOMADzone).get_Text().Trim().PadLeft(4, '0') + "-" + ((Control)txtNOMADnumber).get_Text().Trim().PadLeft(7, '0');
						break;
					case 5:
						EventDetails.StarCat = "J-coords";
						EventDetails.StarNumber = ((Control)txtGaiaCoords).get_Text().Trim();
						Epoch = 2016.0;
						break;
					}
					EventDetails.GaiaVersion = GaiaVersion;
					EventDetails.Gaia_ID = GaiaSourceID.ToString();
					EventDetails.RA_Star_2000 = RAatEpoch;
					EventDetails.Dec_Star_2000 = DecatEpoch;
					EventDetails.RA_Star_Uncertainty_mas = Math.Sqrt(Math.Pow(UncertRA, 2.0) + Math.Pow((Utilities.BesselianYear(EventDetails.JD_EventDate) - Epoch) * UncertPMRA, 2.0));
					EventDetails.Dec_Star_Uncertainty_mas = Math.Sqrt(Math.Pow(UncertDec, 2.0) + Math.Pow((Utilities.BesselianYear(EventDetails.JD_EventDate) - Epoch) * UncertPMDec, 2.0));
					EventDetails.StarDia_mas = StarDiameter_mas;
					int num2 = 0;
					if (StarReliability > 1.4 || StarReliability <= 0.0)
					{
						num2++;
					}
					if (DuplicateSource > 0)
					{
						num2 += 2;
					}
					EventDetails.IssuesFlag = num2;
					EventDetails.MbStar = MagB;
					EventDetails.MgStar = MagV;
					EventDetails.MrStar = MagR;
					EventDetails.StarReliability = StarReliability;
					EventDetails.DuplicatedSource = DuplicateSource;
					EventDetails.NoGaiaPM = NoGaiaPM;
					EventDetails.GaiaPMfromUCAC4 = GaiaPMfromUCAC;
				}
			}
			Data_and_Plots.StarRA = EventDetails.RA_Star_2000;
			Data_and_Plots.StarDec = EventDetails.Dec_Star_2000;
			Utilities.ApparentStarPosition(EventDetails.RA_Star_2000, EventDetails.Dec_Star_2000, 0.0, 0.0, 0.0, 2000, IncludeRelativisticBending: false, EventDate - 1.0 / 24.0, use2006values_Not1976: true, out RAStar_1_Apparent, out DecStar_1_Apparent);
			Utilities.ApparentStarPosition(EventDetails.RA_Star_2000, EventDetails.Dec_Star_2000, 0.0, 0.0, 0.0, 2000, IncludeRelativisticBending: false, EventDate, use2006values_Not1976: true, out RAStar0_Apparent, out DecStar0_Apparent);
			EventDetails.RA_Star_Apparent = (Data_and_Plots.RAStar_Apparent = RAStar0_Apparent);
			EventDetails.Dec_Star_Apparent = (Data_and_Plots.DecStar_Apparent = DecStar0_Apparent);
			Utilities.ApparentStarPosition(EventDetails.RA_Star_2000, EventDetails.Dec_Star_2000, 0.0, 0.0, 0.0, 2000, IncludeRelativisticBending: false, EventDate + 1.0 / 24.0, use2006values_Not1976: true, out RAStar1_Apparent, out DecStar1_Apparent);
			Utilities.ApparentStarPosition(EventDetails.RA_Star_2000, EventDetails.Dec_Star_2000, 0.0, 0.0, 0.0, 2000, IncludeRelativisticBending: false, EventDate + 1.0 / 12.0, use2006values_Not1976: true, out RAStar2_Apparent, out DecStar2_Apparent);
			SetStarIssues();
			((Control)lblRA).set_Text("RA = " + Utilities.DEGtoDMS(Data_and_Plots.StarRA / 15.0 * (180.0 / Math.PI), 2, 5, MinutesOnly: false));
			((Control)lblDec).set_Text("Dec = " + Utilities.DEGtoDMS(Data_and_Plots.StarDec * (180.0 / Math.PI), 3, 4, MinutesOnly: false, IncludeLeadingZeros: false, IncludePlusSymbol: true));
			((Control)lblStarIssues).set_Visible(EventDetails.IssuesFlag != 0);
			((Control)lblMag).set_Text(string.Format("Mv = {0,3:F2}", MagV));
			((Control)lblinGaiaDR2).set_Visible(EventDetails.GaiaVersion >= 0);
			if (EventDetails.GaiaVersion >= 0)
			{
				((Control)lblinGaiaDR2).set_Text(Gaia.StarSourceCat[EventDetails.GaiaVersion]);
			}
			if (!UpdateStarOnly)
			{
				Interferometric_Plus_WDS.Find_WDS_IF_Matches(RA * (180.0 / Math.PI) / 15.0, Dec * (180.0 / Math.PI), HighPrecision: false, ShowResults: true, out WDS_Interferometer_Display.VariableID);
			}
			SetBesselianMotions();
			MagDrop();
			SetFadeIndicator();
			SetStarID();
		}

		private void cmdGetAsteroid_Click(object sender, EventArgs e)
		{
			GetAsteroidPosition(UseExistingStarPosition: false);
		}

		private void GetAsteroidPosition()
		{
			GetAsteroidPosition(UseExistingStarPosition: false);
		}

		private void GetAsteroidPosition(bool UseExistingStarPosition)
		{
			//IL_0036: Unknown result type (might be due to invalid IL or missing references)
			//IL_0943: Unknown result type (might be due to invalid IL or missing references)
			//IL_09e5: Unknown result type (might be due to invalid IL or missing references)
			//IL_14a0: Unknown result type (might be due to invalid IL or missing references)
			if (!IsYearValid() | !IsMonthValid() | !IsDayValid() | !IsHourValid())
			{
				MessageBox.Show("You must set a valid Year, Month, Day and Hour", "Set a vaild date", (MessageBoxButtons)0, (MessageBoxIcon)16);
				((Control)txtYear).Focus();
				return;
			}
			int num = 0;
			int num2 = 0;
			int num3 = -1;
			_ = new char[145];
			double num4 = 0.01;
			double num5 = 0.01;
			double num6 = 0.0;
			double PlanetDiameterUncertainty_km = 1.0;
			string text = "";
			double RA = 0.0;
			double RA2 = 0.0;
			double RA3 = 0.0;
			double RA4 = 0.0;
			double Dec = 0.0;
			double Dec2 = 0.0;
			double Dec3 = 0.0;
			double Dec4 = 0.0;
			GetEventDate();
			GetStarPosition(ForStarDiameter: false, UpdateStarOnly: false, UseExistingStarPosition);
			double MagR_Uncert;
			string EphemSource;
			if (optAsteroid.get_Checked())
			{
				bool flag = Utilities.InternetIsAvailable();
				string text2 = ((Control)txtAsteroidNumber).get_Text().Trim();
				if (!int.TryParse(text2, out var result) || result == 0)
				{
					text2 = ((Control)txtAsteroidName).get_Text().Trim();
				}
				AsteroidElements AE = new AsteroidElements();
				string text3 = "";
				if (flag)
				{
					text3 = http.GetHorizonsElements_TDBtime(text2, Data_and_Plots.Year, Data_and_Plots.Month, Data_and_Plots.Day, Data_and_Plots.TZero_forMotions, ref AE);
					if (text3.Length > 50)
					{
						using (StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Resource Files\\HorizonForReductions.csv"))
						{
							streamWriter.WriteLine(AsteroidElements.CSVHeader());
							streamWriter.WriteLine(text3);
						}
						num3 = 0;
					}
					else
					{
						flag = false;
						num3 = ((!int.TryParse(((Control)txtAsteroidNumber).get_Text(), out var result2)) ? Elements.MainAsteroids.GetAsteroidRecord_fromName(text2) : Elements.MainAsteroids.GetAsteroidRecord_fromNumber(result2));
						text = "(" + text2.ToString() + ")";
					}
				}
				if (num3 < 0)
				{
					MessageBox.Show("The asteroid " + text + " is not contained in the current file of asteroid elements\r\n\r\nYou should recreate the file from the MPCORB, AstDyS2 or\r\nASTORB files, specifying this asteroid in the list of \r\n'mandatory' minor planets.\r\n\r\nYou can access the conversion under the menu item\r\n    File... Convert MPCorb etc", "Asteroid not present", (MessageBoxButtons)0, (MessageBoxIcon)48);
					return;
				}
				List<AsteroidElements> astElements;
				double osculatingJD;
				if (flag)
				{
					astElements = Elements.Reductions.AstElements;
					astElements.Clear();
					Elements.Reductions.Fill_AllAsteroids();
					osculatingJD = Elements.Reductions.AstElements[num3].OsculatingJD;
				}
				else
				{
					astElements = Elements.MainAsteroids.AstElements;
					if (astElements.Count < 1)
					{
						Elements.MainAsteroids.Fill_AllAsteroids();
					}
					osculatingJD = Elements.MainAsteroids.AstElements[num3].OsculatingJD;
				}
				double endJD = Math.Floor(EventDate) + 0.5;
				double meanAnomaly;
				double perihelion;
				double node;
				double i;
				double e;
				double a;
				double q;
				double h;
				double g_phaseCoeff;
				double num7;
				double osculatingJD2;
				double num8;
				if (flag)
				{
					meanAnomaly = Elements.Reductions.AstElements[0].Meananomaly / (180.0 / Math.PI);
					perihelion = Elements.Reductions.AstElements[0].Perihelion / (180.0 / Math.PI);
					node = Elements.Reductions.AstElements[0].Node / (180.0 / Math.PI);
					i = Elements.Reductions.AstElements[0].I / (180.0 / Math.PI);
					e = Elements.Reductions.AstElements[0].E;
					a = Elements.Reductions.AstElements[0].A;
					q = a * (1.0 - e);
					h = Elements.Reductions.AstElements[0].H0;
					g_phaseCoeff = Elements.Reductions.AstElements[0].G_phaseCoeff;
					num7 = Elements.Reductions.AstElements[0].LogR_Coeff;
					num6 = Elements.Reductions.AstElements[0].Diameter_Mean;
					PlanetDiameterUncertainty_km = Elements.Reductions.AstElements[0].DiameterUncertainty;
					osculatingJD2 = Elements.Reductions.AstElements[0].OsculatingJD;
					string iDName;
					((Control)txtAsteroidName).set_Text(iDName = Elements.Reductions.AstElements[0].IDName);
					num8 = Elements.Reductions.AstElements[0].IDNumber;
				}
				else
				{
					MinorPlanetOccultationElements.Integrate(astElements, num3, osculatingJD, endJD, AddNotIntegratedFlag: false);
					int index = MinorPlanetOccultationElements.ElementsAt1Day.Count - 1;
					num8 = MinorPlanetOccultationElements.ElementsAt1Day[index].IDNumber;
					((Control)txtAsteroidNumber).set_Text(num8.ToString());
					string iDName;
					((Control)txtAsteroidName).set_Text(iDName = MinorPlanetOccultationElements.ElementsAt1Day[index].IDName);
					meanAnomaly = MinorPlanetOccultationElements.ElementsAt1Day[index].Meananomaly / (180.0 / Math.PI);
					perihelion = MinorPlanetOccultationElements.ElementsAt1Day[index].Perihelion / (180.0 / Math.PI);
					node = MinorPlanetOccultationElements.ElementsAt1Day[index].Node / (180.0 / Math.PI);
					i = MinorPlanetOccultationElements.ElementsAt1Day[index].I / (180.0 / Math.PI);
					e = MinorPlanetOccultationElements.ElementsAt1Day[index].E;
					a = MinorPlanetOccultationElements.ElementsAt1Day[index].A;
					q = a * (1.0 - e);
					h = MinorPlanetOccultationElements.ElementsAt1Day[index].H0;
					g_phaseCoeff = MinorPlanetOccultationElements.ElementsAt1Day[index].G_phaseCoeff;
					num7 = MinorPlanetOccultationElements.ElementsAt1Day[index].LogR_Coeff;
					if (num7 == 5.0 && ((iDName.Substring(1, 1) == "/") | (((iDName.Substring(0, 1) == "P") | (iDName.Substring(0, 1) == "C")) & int.TryParse(iDName.Substring(1, 1), out var _))))
					{
						num7 = 10.0;
					}
					num6 = MinorPlanetOccultationElements.ElementsAt1Day[index].Diameter_Mean;
					PlanetDiameterUncertainty_km = MinorPlanetOccultationElements.ElementsAt1Day[index].DiameterUncertainty;
					osculatingJD2 = MinorPlanetOccultationElements.ElementsAt1Day[index].OsculatingJD;
				}
				if (e > 0.97)
				{
					q = a;
					meanAnomaly = 0.0;
				}
				Utilities.PositionfromElements(EventDate - 1.0 / 24.0, 0.0, 0.0, 0.0, 0.0, osculatingJD2, meanAnomaly, q, e, perihelion, node, i, h, g_phaseCoeff, num7, 0.0, out RA, out Dec, out var RadiusVector, out DeltaAsteroid_1, out MagAsteroid, out var Elongation, out var PhaseAngle);
				Utilities.ApparentStarPosition(RA, Dec, 0.0, 0.0, 0.0, 2000, IncludeRelativisticBending: false, EventDate - 1.0 / 24.0, use2006values_Not1976: true, out RAobject_1_Apparent, out DecObject_1_Apparent);
				Utilities.PositionfromElements(EventDate + 1.0 / 24.0, 0.0, 0.0, 0.0, 0.0, osculatingJD2, meanAnomaly, q, e, perihelion, node, i, h, g_phaseCoeff, num7, 0.0, out RA3, out Dec3, out RadiusVector, out DeltaAsteroid1, out MagAsteroid, out Elongation, out PhaseAngle);
				Utilities.ApparentStarPosition(RA3, Dec3, 0.0, 0.0, 0.0, 2000, IncludeRelativisticBending: false, EventDate + 1.0 / 24.0, use2006values_Not1976: true, out RAobject1_Apparent, out DecObject1_Apparent);
				Utilities.PositionfromElements(EventDate + 1.0 / 12.0, 0.0, 0.0, 0.0, 0.0, osculatingJD2, meanAnomaly, q, e, perihelion, node, i, h, g_phaseCoeff, num7, 0.0, out RA4, out Dec4, out RadiusVector, out DeltaAsteroid2, out MagAsteroid, out Elongation, out PhaseAngle);
				Utilities.ApparentStarPosition(RA4, Dec4, 0.0, 0.0, 0.0, 2000, IncludeRelativisticBending: false, EventDate + 1.0 / 12.0, use2006values_Not1976: true, out RAobject2_Apparent, out DecObject2_Apparent);
				Utilities.PositionfromElements(EventDate, 0.0, 0.0, 0.0, 0.0, osculatingJD2, meanAnomaly, q, e, perihelion, node, i, h, g_phaseCoeff, num7, 0.0, out RA2, out Dec2, out RadiusVector, out DeltaAsteroid0, out MagAsteroid, out Elongation, out PhaseAngle);
				Utilities.ApparentStarPosition(RA2, Dec2, 0.0, 0.0, 0.0, 2000, IncludeRelativisticBending: false, EventDate, use2006values_Not1976: true, out RAobject0_Apparent, out DecObject0_Apparent);
				if (MagValues.AsteroidMagnitudeFileExists)
				{
					int colorMagnitudeRecord = MagValues.GetColorMagnitudeRecord((int)num8);
					if (colorMagnitudeRecord >= 0 && MagValues.ColorMagnitudeData[colorMagnitudeRecord].GetColorMagnitudes(RA2, Dec2, RadiusVector, DeltaAsteroid0, PhaseAngle, out var MagV, out var _, out var _, out MagR_Uncert))
					{
						MagAsteroid = MagV;
					}
				}
				Data_and_Plots.Parallax = 8.794143836182533 / DeltaAsteroid0;
				Data_and_Plots.dParallax = 8.794143836182533 / DeltaAsteroid1 - Data_and_Plots.Parallax;
				Data_and_Plots.Diameter = num6;
				Data_and_Plots.DiameterUncertainty = PlanetDiameterUncertainty_km;
			}
			else if (optPlanet.get_Checked())
			{
				num = ((ListControl)cmbPlanets).get_SelectedIndex();
				if (num > 2)
				{
					num++;
				}
				num2 = ((ListControl)cmbMoons).get_SelectedIndex();
				if (num == 6 && num2 > 9)
				{
					num2 += 2;
				}
				if (num < 1 || num2 < 0)
				{
					MessageBox.Show("Invalid Planet or Moon name. Please select names from drop-down list", "Invalid Planet/Moon", (MessageBoxButtons)0, (MessageBoxIcon)48);
					return;
				}
				if (num2 < 1)
				{
					Utilities.PlanetGeocentric(EventDate - 1.0 / 24.0, num, 1E-07, 2, out RA, out Dec, out DeltaAsteroid_1, out MagAsteroid, out var Diameter);
					Utilities.ApparentStarPosition(RA, Dec, 0.0, 0.0, 0.0, 2000, IncludeRelativisticBending: false, EventDate - 1.0 / 24.0, use2006values_Not1976: true, out RAobject_1_Apparent, out DecObject_1_Apparent);
					Utilities.PlanetGeocentric(EventDate + 1.0 / 24.0, num, 1E-07, 2, out RA3, out Dec3, out DeltaAsteroid1, out MagAsteroid, out Diameter);
					Utilities.ApparentStarPosition(RA3, Dec3, 0.0, 0.0, 0.0, 2000, IncludeRelativisticBending: false, EventDate + 1.0 / 24.0, use2006values_Not1976: true, out RAobject1_Apparent, out DecObject1_Apparent);
					Utilities.PlanetGeocentric(EventDate + 1.0 / 12.0, num, 1E-07, 2, out RA4, out Dec4, out DeltaAsteroid2, out MagAsteroid, out Diameter);
					Utilities.ApparentStarPosition(RA4, Dec4, 0.0, 0.0, 0.0, 2000, IncludeRelativisticBending: false, EventDate + 1.0 / 12.0, use2006values_Not1976: true, out RAobject2_Apparent, out DecObject2_Apparent);
					Utilities.PlanetGeocentric(EventDate, num, 1E-07, 2, out RA2, out Dec2, out DeltaAsteroid0, out MagAsteroid, out Diameter);
					Utilities.ApparentStarPosition(RA2, Dec2, 0.0, 0.0, 0.0, 2000, IncludeRelativisticBending: false, EventDate, use2006values_Not1976: true, out RAobject0_Apparent, out DecObject0_Apparent);
					PlanetOccultationElements.PlanetParameters(num, out var PlanetName, out var _, out var _, out var PlanetRadius, out PlanetDiameterUncertainty_km);
					Data_and_Plots.Diameter = 2.0 * PlanetRadius * 6378.137;
					Data_and_Plots.DiameterUncertainty = PlanetDiameterUncertainty_km;
					EventDetails.AsteroidID = PlanetName.Trim();
					Data_and_Plots.Parallax = 8.794143836182533 / DeltaAsteroid0;
					Data_and_Plots.dParallax = 8.794143836182533 / DeltaAsteroid1 - Data_and_Plots.Parallax;
				}
				else
				{
					string MoonName = "";
					float MoonDiaKm = 0f;
					float Mag = 0f;
					bool includeRelativisticBending = Utilities.InternetIsAvailable();
					Satellites.SatelliteCoordinates(EventDate - 1.0 / 24.0, num, num2, ref MoonName, ref MoonDiaKm, ref RA, ref Dec, ref Mag);
					Utilities.ApparentStarPosition(RA, Dec, 0.0, 0.0, 0.0, 2000, includeRelativisticBending, EventDate - 1.0 / 24.0, use2006values_Not1976: true, out RAobject_1_Apparent, out DecObject_1_Apparent);
					DeltaAsteroid_1 = Satellites.PlanetDistance;
					Satellites.SatelliteCoordinates(EventDate + 1.0 / 24.0, num, num2, ref MoonName, ref MoonDiaKm, ref RA3, ref Dec3, ref Mag);
					Utilities.ApparentStarPosition(RA3, Dec3, 0.0, 0.0, 0.0, 2000, includeRelativisticBending, EventDate + 1.0 / 24.0, use2006values_Not1976: true, out RAobject1_Apparent, out DecObject1_Apparent);
					DeltaAsteroid1 = Satellites.PlanetDistance;
					Satellites.SatelliteCoordinates(EventDate + 1.0 / 12.0, num, num2, ref MoonName, ref MoonDiaKm, ref RA4, ref Dec4, ref Mag);
					Utilities.ApparentStarPosition(RA4, Dec4, 0.0, 0.0, 0.0, 2000, includeRelativisticBending, EventDate + 1.0 / 12.0, use2006values_Not1976: true, out RAobject2_Apparent, out DecObject2_Apparent);
					DeltaAsteroid2 = Satellites.PlanetDistance;
					Satellites.SatelliteCoordinates(EventDate, num, num2, ref MoonName, ref MoonDiaKm, ref RA2, ref Dec2, ref Mag);
					Utilities.ApparentStarPosition(RA2, Dec2, 0.0, 0.0, 0.0, 2000, includeRelativisticBending, EventDate, use2006values_Not1976: true, out RAobject0_Apparent, out DecObject0_Apparent);
					DeltaAsteroid0 = Satellites.PlanetDistance;
					if (Utilities.InternetIsAvailable())
					{
						double jd = EventDate - 1.0 / 24.0;
						http.GetHorizonsSatelliteApparentEphemeris(num, num2, jd, UseTTnotUT: true, out var RA5, out var Dec5, out var Delta, out EphemSource, out MagR_Uncert);
						RAobject_1_Apparent = RA5[0];
						DecObject_1_Apparent = Dec5[0];
						DeltaAsteroid_1 = Delta[0];
						RAobject0_Apparent = RA5[1];
						DecObject0_Apparent = Dec5[1];
						DeltaAsteroid0 = Delta[1];
						RAobject1_Apparent = RA5[2];
						DecObject1_Apparent = Dec5[2];
						DeltaAsteroid1 = Delta[2];
						RAobject2_Apparent = RA5[3];
						DecObject2_Apparent = Dec5[3];
						DeltaAsteroid2 = Delta[3];
					}
					Data_and_Plots.Diameter = Satellites.SatelliteDiameter(num, num2, out PlanetDiameterUncertainty_km);
					Data_and_Plots.DiameterUncertainty = PlanetDiameterUncertainty_km;
					EventDetails.AsteroidID = MoonName.Trim();
					int num9 = EventDetails.AsteroidID.IndexOf("(");
					if (num9 > 0)
					{
						EventDetails.AsteroidID = EventDetails.AsteroidID.Substring(0, num9);
					}
					MagAsteroid = Mag;
					Data_and_Plots.Parallax = 8.794143836182533 / DeltaAsteroid0;
					Data_and_Plots.dParallax = 8.794143836182533 / DeltaAsteroid1 - Data_and_Plots.Parallax;
				}
			}
			else
			{
				double Diameter2 = 1.0;
				if (Utilities.InternetIsAvailable())
				{
					double jd2 = EventDate - 1.0 / 24.0;
					http.GetHorizonsCometApparentEphemeris(((Control)txtCometNumber).get_Text().ToUpper().Trim(), jd2, UseTTnotUT: true, out var RA6, out var Dec6, out var Delta2, out EphemSource, out MagR_Uncert, out Diameter2, out var T_Mag, out var N_Mag);
					RAobject_1_Apparent = RA6[0];
					DecObject_1_Apparent = Dec6[0];
					DeltaAsteroid_1 = Delta2[0];
					RAobject0_Apparent = RA6[1];
					DecObject0_Apparent = Dec6[1];
					DeltaAsteroid0 = Delta2[1];
					RAobject1_Apparent = RA6[2];
					DecObject1_Apparent = Dec6[2];
					DeltaAsteroid1 = Delta2[2];
					RAobject2_Apparent = RA6[3];
					DecObject2_Apparent = Dec6[3];
					DeltaAsteroid2 = Delta2[3];
					if (N_Mag > 0.0 && N_Mag < 30.0)
					{
						MagAsteroid = N_Mag;
					}
					else
					{
						MagAsteroid = T_Mag;
					}
				}
				Data_and_Plots.Diameter = Diameter2;
				if (Diameter2 != 2.0)
				{
					Data_and_Plots.DiameterUncertainty = Diameter2 / 10.0;
				}
				else
				{
					Data_and_Plots.DiameterUncertainty = 2.0;
				}
				Data_and_Plots.Parallax = 8.794143836182533 / DeltaAsteroid0;
				Data_and_Plots.dParallax = 8.794143836182533 / DeltaAsteroid1 - Data_and_Plots.Parallax;
			}
			SetBesselianMotions();
			((Control)lblMotions).set_Text(string.Format("dX= {0,6:F3} km/s  dY= {1,6:F3}km/s", EventDetails.dX * 6378.137 / 3600.0, EventDetails.dY * 6378.137 / 3600.0));
			((Control)lblParallax).set_Text(string.Format("={0,6:F4}\" {1:+0.0;-0.0}mas/hr", Data_and_Plots.Parallax, Data_and_Plots.dParallax * 1000.0));
			((Control)lblMagnitude).set_Text(string.Format("Mv: {0,4:F2}", MagAsteroid));
			double num10 = Data_and_Plots.Diameter / 6378.137 * Data_and_Plots.Parallax;
			if (Data_and_Plots.Diameter < 2.0)
			{
				((Control)lblDiameterKM).set_Text(string.Format("Dia: {0,1:F0}m ±{1,1:f0} m = ", Data_and_Plots.Diameter * 1000.0, Data_and_Plots.DiameterUncertainty * 1000.0));
			}
			else if (Data_and_Plots.Diameter < 5.0)
			{
				((Control)lblDiameterKM).set_Text(string.Format("Dia: {0,1:F2} ±{1,1:f2} km = ", Data_and_Plots.Diameter, Data_and_Plots.DiameterUncertainty));
			}
			else if (Data_and_Plots.Diameter < 10.0)
			{
				((Control)lblDiameterKM).set_Text(string.Format("Dia: {0,1:F1} ±{1,1:f1} km = ", Data_and_Plots.Diameter, Data_and_Plots.DiameterUncertainty));
			}
			else
			{
				((Control)lblDiameterKM).set_Text(string.Format("Dia: {0,1:F0} ±{1,1:f0} km = ", Data_and_Plots.Diameter, Data_and_Plots.DiameterUncertainty));
			}
			if (num10 < 0.05)
			{
				Label obj = lblDiameterKM;
				((Control)obj).set_Text(((Control)obj).get_Text() + string.Format("{0,1:F1} mas", num10 * 1000.0));
			}
			else if (num10 < 1.0)
			{
				Label obj2 = lblDiameterKM;
				((Control)obj2).set_Text(((Control)obj2).get_Text() + string.Format("{0,1:F0} mas", num10 * 1000.0));
			}
			else if (num10 < 10.0)
			{
				Label obj3 = lblDiameterKM;
				((Control)obj3).set_Text(((Control)obj3).get_Text() + string.Format("{0,1:F2}\"", num10));
			}
			else
			{
				Label obj4 = lblDiameterKM;
				((Control)obj4).set_Text(((Control)obj4).get_Text() + string.Format("{0,1:F1}\"", num10));
			}
			int width = ((Control)lblDiameterKM).get_Width();
			((Control)lblDiameterKM).set_Left((((Control)grpAsteroid).get_Width() - width) / 2);
			num4 = (0.001 + Data_and_Plots.Parallax / 3600.0) / Math.Cos(Dec2) / (180.0 / Math.PI);
			num5 = (0.001 + Data_and_Plots.Parallax / 3600.0) / (180.0 / Math.PI);
			if ((Math.Abs(Data_and_Plots.RAStar_Apparent - RAobject0_Apparent) > num4) | (Math.Abs(Data_and_Plots.DecStar_Apparent - DecObject0_Apparent) > num5))
			{
				MessageBox.Show("Star and Asteroid coordinates do not match for the date and nominal mid-time of the event\r\n\r\nComputed J2000 co-ordinates of the Asteroid are: " + Utilities.DEGtoDMS(RA2 * (180.0 / Math.PI) / 15.0, 2, 2, MinutesOnly: false) + "   " + Utilities.DEGtoDMS(Dec2 * (180.0 / Math.PI), 3, 1, MinutesOnly: false) + "\r\n\r\nDifferences [in degrees] are:  dRA " + string.Format("{0,6:F4}", (Data_and_Plots.RAStar_Apparent - RAobject0_Apparent) * (180.0 / Math.PI)) + "   dDec " + string.Format("{0,6:F4}", (Data_and_Plots.DecStar_Apparent - DecObject0_Apparent) * (180.0 / Math.PI)) + "\r\n\r\nCHECK star number, Asteroid number, Date, and Mid-time of the event, for errors", "Position Mismatch", (MessageBoxButtons)0, (MessageBoxIcon)48);
			}
			if (optPlanet.get_Checked())
			{
				int num11 = ((ListControl)cmbPlanets).get_SelectedIndex();
				if (num11 > 2)
				{
					num11++;
				}
				if ((num11 == 6) & (((ListControl)cmbMoons).get_SelectedIndex() > 9))
				{
					EventDetails.AsteroidNumber = "P" + num11 + "M" + (((ListControl)cmbMoons).get_SelectedIndex() + 2).ToString().PadLeft(2, '0');
				}
				else
				{
					EventDetails.AsteroidNumber = "P" + num11 + "M" + ((ListControl)cmbMoons).get_SelectedIndex().ToString().PadLeft(2, '0');
				}
			}
			else if (optAsteroid.get_Checked())
			{
				optAsteroid.set_Checked(true);
				EventDetails.AsteroidNumber = ((Control)txtAsteroidNumber).get_Text().Trim();
				EventDetails.AsteroidID = ((Control)txtAsteroidName).get_Text().Trim();
			}
			else
			{
				EventDetails.AsteroidNumber = ((Control)txtCometNumber).get_Text().Trim();
				EventDetails.AsteroidID = ((Control)txtCometName).get_Text().Trim();
			}
			EventDetails.MvAsteroid = MagAsteroid;
			EventDetails.AsteroidNominalDiameter = Data_and_Plots.Diameter;
			EventDetails.DiameterUncertainty = Data_and_Plots.DiameterUncertainty;
			if (EventDetails.X_Dia == 0.0)
			{
				EventDetails.X_Dia = (EventDetails.Y_Dia = (int)Data_and_Plots.Diameter);
			}
			MagDrop();
			SetFadeIndicator();
			SetStarID();
			if (Settings.Default.AutoStarDiameter)
			{
				Get_StarDia_Fresnel();
			}
			if (Settings.Default.AutoLightCurveData)
			{
				GetLightCurveData();
			}
		}

		private void SetBesselianMotions()
		{
			BessellianElements(RAStar_1_Apparent, DecStar_1_Apparent, RAobject_1_Apparent, DecObject_1_Apparent, DeltaAsteroid_1, out var X, out var Y);
			BessellianElements(RAStar0_Apparent, DecStar0_Apparent, RAobject0_Apparent, DecObject0_Apparent, DeltaAsteroid0, out var X2, out var Y2);
			BessellianElements(RAStar1_Apparent, DecStar1_Apparent, RAobject1_Apparent, DecObject1_Apparent, DeltaAsteroid1, out var X3, out var Y3);
			BessellianElements(RAStar2_Apparent, DecStar2_Apparent, RAobject2_Apparent, DecObject2_Apparent, DeltaAsteroid2, out var X4, out var Y4);
			Utilities.FitCubicTo4_EqualSpaced_Points(X, X2, X3, X4, out var Xat, out Data_and_Plots.dX, out Data_and_Plots.d2X, out Data_and_Plots.d3X);
			Utilities.FitCubicTo4_EqualSpaced_Points(Y, Y2, Y3, Y4, out Xat, out Data_and_Plots.dY, out Data_and_Plots.d2Y, out Data_and_Plots.d3Y);
			EventDetails.dX = Data_and_Plots.dX;
			EventDetails.d2X = Data_and_Plots.d2X;
			EventDetails.d3X = Data_and_Plots.d3X;
			EventDetails.dY = Data_and_Plots.dY;
			EventDetails.d2Y = Data_and_Plots.d2Y;
			EventDetails.d3Y = Data_and_Plots.d3Y;
			try
			{
				Utilities.FitCubicTo4_EqualSpaced_Points(8.794143836182533 / DeltaAsteroid_1, 8.794143836182533 / DeltaAsteroid0, 8.794143836182533 / DeltaAsteroid1, 8.794143836182533 / DeltaAsteroid2, out Data_and_Plots.Parallax, out Data_and_Plots.dParallax, out Data_and_Plots.d2Parallax, out Data_and_Plots.d3Parallax);
				EventDetails.Parallax = Data_and_Plots.Parallax;
				EventDetails.dParallax = Data_and_Plots.dParallax;
			}
			catch
			{
			}
		}

		internal static void BessellianElements(double RAStar, double DecStar, double RAAsteroid, double DecAsteroid, double DistanceAsteroid, out double X, out double Y)
		{
			double num = DistanceAsteroid / Math.Sin(4.26352124542639E-05);
			X = num * Math.Cos(DecAsteroid) * Math.Sin(RAAsteroid - RAStar);
			Y = num * (Math.Sin(DecAsteroid) * Math.Cos(DecStar) - Math.Cos(DecAsteroid) * Math.Sin(DecStar) * Math.Cos(RAAsteroid - RAStar));
		}

		private void cmbPlanets_SelectedIndexChanged(object sender, EventArgs e)
		{
			cmbMoons.get_Items().Clear();
			cmbMoons.get_Items().Add((object)"");
			((ListControl)cmbMoons).set_SelectedIndex(0);
			switch (((ListControl)cmbPlanets).get_SelectedIndex())
			{
			case 3:
				cmbMoons.get_Items().Add((object)"Phobos");
				cmbMoons.get_Items().Add((object)"Deimos");
				break;
			case 4:
				cmbMoons.get_Items().Add((object)"Io");
				cmbMoons.get_Items().Add((object)"Europa");
				cmbMoons.get_Items().Add((object)"Ganymede");
				cmbMoons.get_Items().Add((object)"Callisto");
				cmbMoons.get_Items().Add((object)"Amalthea");
				cmbMoons.get_Items().Add((object)"Himalia");
				cmbMoons.get_Items().Add((object)"Elara");
				cmbMoons.get_Items().Add((object)"Pasiphae");
				cmbMoons.get_Items().Add((object)"Sinope");
				cmbMoons.get_Items().Add((object)"Lysithea");
				cmbMoons.get_Items().Add((object)"Carme");
				cmbMoons.get_Items().Add((object)"Ananke");
				cmbMoons.get_Items().Add((object)"Leda");
				cmbMoons.get_Items().Add((object)"Thebe");
				cmbMoons.get_Items().Add((object)"Adrastea");
				cmbMoons.get_Items().Add((object)"Metis");
				break;
			case 5:
				cmbMoons.get_Items().Add((object)"Mimas");
				cmbMoons.get_Items().Add((object)"Enceladus");
				cmbMoons.get_Items().Add((object)"Tethys");
				cmbMoons.get_Items().Add((object)"Dione");
				cmbMoons.get_Items().Add((object)"Rhea");
				cmbMoons.get_Items().Add((object)"Titan");
				cmbMoons.get_Items().Add((object)"Hyperion");
				cmbMoons.get_Items().Add((object)"Iapetus");
				cmbMoons.get_Items().Add((object)"Phoebe");
				cmbMoons.get_Items().Add((object)"Helene");
				cmbMoons.get_Items().Add((object)"Telesto");
				cmbMoons.get_Items().Add((object)"Calypso");
				break;
			case 6:
				cmbMoons.get_Items().Add((object)"Ariel");
				cmbMoons.get_Items().Add((object)"Umbriel");
				cmbMoons.get_Items().Add((object)"Titania");
				cmbMoons.get_Items().Add((object)"Oberon");
				cmbMoons.get_Items().Add((object)"Miranda");
				cmbMoons.get_Items().Add((object)"Cordelia");
				cmbMoons.get_Items().Add((object)"Orphelia");
				cmbMoons.get_Items().Add((object)"Bianca");
				cmbMoons.get_Items().Add((object)"Cressida");
				cmbMoons.get_Items().Add((object)"Desdemona");
				cmbMoons.get_Items().Add((object)"Juliet");
				cmbMoons.get_Items().Add((object)"Portia");
				cmbMoons.get_Items().Add((object)"Rosalind");
				cmbMoons.get_Items().Add((object)"Belinda");
				cmbMoons.get_Items().Add((object)"Puck");
				break;
			case 7:
				cmbMoons.get_Items().Add((object)"Triton");
				cmbMoons.get_Items().Add((object)"Nereid");
				break;
			case 8:
				cmbMoons.get_Items().Add((object)"Charon");
				cmbMoons.get_Items().Add((object)"Hydra");
				cmbMoons.get_Items().Add((object)"Nix");
				cmbMoons.get_Items().Add((object)"Kerberos");
				cmbMoons.get_Items().Add((object)"Styx");
				break;
			}
		}

		private void optAsteroid_CheckedChanged(object sender, EventArgs e)
		{
			((Control)panelPlanets).set_Visible(optPlanet.get_Checked());
			((Control)pnlComet).set_Visible(optComets.get_Checked());
			Panel obj = pnlDAMIT_ISAM;
			bool @checked;
			((Control)pnlBinary).set_Visible(@checked = optAsteroid.get_Checked());
			((Control)obj).set_Visible(@checked);
			((ToolStripItem)jPLSmallBodyDatabaseToolStripMenuItem).set_Enabled(optAsteroid.get_Checked() | optComets.get_Checked());
			SetAsteroidGroupLabel();
		}

		private void optPlanet_CheckedChanged(object sender, EventArgs e)
		{
			((Control)panelPlanets).set_Visible(optPlanet.get_Checked());
			((Control)pnlComet).set_Visible(optComets.get_Checked());
			Panel obj = pnlDAMIT_ISAM;
			bool @checked;
			((Control)pnlBinary).set_Visible(@checked = optAsteroid.get_Checked());
			((Control)obj).set_Visible(@checked);
			if (((Control)panelPlanets).get_Visible())
			{
				((Control)panelPlanets).set_Location(new Point(12, 40));
			}
			((ToolStripItem)jPLSmallBodyDatabaseToolStripMenuItem).set_Enabled(optAsteroid.get_Checked() | optComets.get_Checked());
			SetAsteroidGroupLabel();
		}

		private void optComets_CheckedChanged(object sender, EventArgs e)
		{
			((Control)panelPlanets).set_Visible(optPlanet.get_Checked());
			((Control)pnlComet).set_Visible(optComets.get_Checked());
			Panel obj = pnlDAMIT_ISAM;
			bool @checked;
			((Control)pnlBinary).set_Visible(@checked = optAsteroid.get_Checked());
			((Control)obj).set_Visible(@checked);
			if (((Control)pnlComet).get_Visible())
			{
				((Control)pnlComet).set_Location(new Point(2, 40));
			}
			((ToolStripItem)jPLSmallBodyDatabaseToolStripMenuItem).set_Enabled(optAsteroid.get_Checked() | optComets.get_Checked());
			SetAsteroidGroupLabel();
		}

		private void SetAsteroidGroupLabel()
		{
			if (optPlanet.get_Checked())
			{
				((Control)grpAsteroid).set_Text("Planet");
				CheckBox chkShapeModelCentered = Data_and_Plots.PlotForm.chkShapeModelCentered;
				bool @checked;
				((Control)Data_and_Plots.PlotForm.chkShapeModelCentered).set_Enabled(@checked = false);
				chkShapeModelCentered.set_Checked(@checked);
				Label obj = lblISAM;
				Label obj2 = lblInDAMIT;
				bool flag;
				((Control)lblBinary).set_Visible(flag = false);
				((Control)obj2).set_Visible(@checked = flag);
				((Control)obj).set_Visible(@checked);
				((ToolStripItem)binaryAsteroidDetailsToolStripMenuItem).set_Enabled(false);
			}
			else if (optAsteroid.get_Checked())
			{
				((Control)grpAsteroid).set_Text("Asteroid");
				if (int.TryParse(((Control)txtAsteroidNumber).get_Text(), out var result))
				{
					int NumModels = 0;
					((Control)lblISAM).set_Visible(DisplayMPOccultations.IsInISAM(result, out NumModels));
					if (NumModels == 0)
					{
						((Control)lblISAM).set_Text("in ISAM");
					}
					else
					{
						((Control)lblISAM).set_Text(NumModels + "-ISAM");
					}
					((Control)lblISAM).set_ForeColor(Color.Yellow);
					int NumModels2 = 0;
					((Control)lblInDAMIT).set_Visible(DisplayMPOccultations.IsInDAMIT(result, out NumModels2));
					((Control)lblInDAMIT).set_Text(NumModels2 + "-DAMIT");
					((Control)lblInDAMIT).set_ForeColor(Color.Yellow);
					if (NumModels2 > 0 || NumModels > 0)
					{
						if (Data_and_Plots.PlotForm.chkShapeModelCentered.get_Checked())
						{
							((Control)Data_and_Plots.PlotForm.chkShapeModelCentered).set_Enabled(true);
							CheckBox chkX = Data_and_Plots.PlotForm.chkX;
							bool @checked;
							((Control)Data_and_Plots.PlotForm.chkY).set_Enabled(@checked = false);
							((Control)chkX).set_Enabled(@checked);
							CheckBox chkX2 = Data_and_Plots.PlotForm.chkX;
							Data_and_Plots.PlotForm.chkY.set_Checked(@checked = true);
							chkX2.set_Checked(@checked);
							CheckBox chkCircle = Data_and_Plots.PlotForm.chkCircle;
							Data_and_Plots.PlotForm.chkUseAssumedDiameter.set_Checked(@checked = false);
							chkCircle.set_Checked(@checked);
							CheckBox chkCircle2 = Data_and_Plots.PlotForm.chkCircle;
							((Control)Data_and_Plots.PlotForm.chkUseAssumedDiameter).set_Enabled(@checked = false);
							((Control)chkCircle2).set_Enabled(@checked);
						}
						else
						{
							CheckBox chkX3 = Data_and_Plots.PlotForm.chkX;
							bool @checked;
							((Control)Data_and_Plots.PlotForm.chkY).set_Enabled(@checked = true);
							((Control)chkX3).set_Enabled(@checked);
							CheckBox chkCircle3 = Data_and_Plots.PlotForm.chkCircle;
							((Control)Data_and_Plots.PlotForm.chkUseAssumedDiameter).set_Enabled(@checked = true);
							((Control)chkCircle3).set_Enabled(@checked);
							((Control)Data_and_Plots.PlotForm.chkShapeModelCentered).set_Enabled(false);
						}
					}
					else
					{
						CheckBox chkX4 = Data_and_Plots.PlotForm.chkX;
						bool @checked;
						((Control)Data_and_Plots.PlotForm.chkY).set_Enabled(@checked = true);
						((Control)chkX4).set_Enabled(@checked);
						CheckBox chkCircle4 = Data_and_Plots.PlotForm.chkCircle;
						((Control)Data_and_Plots.PlotForm.chkUseAssumedDiameter).set_Enabled(@checked = true);
						((Control)chkCircle4).set_Enabled(@checked);
						CheckBox chkShapeModelCentered2 = Data_and_Plots.PlotForm.chkShapeModelCentered;
						((Control)Data_and_Plots.PlotForm.chkShapeModelCentered).set_Enabled(@checked = false);
						chkShapeModelCentered2.set_Checked(@checked);
					}
				}
				else
				{
					Label obj3 = lblISAM;
					bool @checked;
					((Control)lblInDAMIT).set_Visible(@checked = false);
					((Control)obj3).set_Visible(@checked);
				}
				if (BinaryAsteroids.BinaryAsteroidPresent(result))
				{
					((Control)lblBinary).set_Text("known binary     ");
					((Control)lblBinary).set_Visible(true);
					((ToolStripItem)binaryAsteroidDetailsToolStripMenuItem).set_Enabled(true);
				}
				else if (GaiaBinaries.GetGaiaBinary(result) > 0)
				{
					((Control)lblBinary).set_Text("Gaia binary ?     ");
					((Control)lblBinary).set_Visible(true);
					((ToolStripItem)binaryAsteroidDetailsToolStripMenuItem).set_Enabled(true);
				}
				else
				{
					((Control)lblBinary).set_Visible(false);
					((ToolStripItem)binaryAsteroidDetailsToolStripMenuItem).set_Enabled(false);
				}
			}
			else
			{
				((Control)grpAsteroid).set_Text("Comet");
				CheckBox chkShapeModelCentered3 = Data_and_Plots.PlotForm.chkShapeModelCentered;
				bool @checked;
				((Control)Data_and_Plots.PlotForm.chkShapeModelCentered).set_Enabled(@checked = false);
				chkShapeModelCentered3.set_Checked(@checked);
				Label obj4 = lblISAM;
				Label obj5 = lblInDAMIT;
				bool flag;
				((Control)lblBinary).set_Visible(flag = false);
				((Control)obj5).set_Visible(@checked = flag);
				((Control)obj4).set_Visible(@checked);
				((ToolStripItem)binaryAsteroidDetailsToolStripMenuItem).set_Enabled(false);
			}
		}

		private void cmbYearRange_SelectedIndexChanged(object sender, EventArgs e)
		{
			optByDate.set_Checked(true);
			DisplayIndex(ForCurrentEvent: false);
			((Control)optByDate).Focus();
		}

		private void cmbNumberRange_SelectedIndexChanged(object sender, EventArgs e)
		{
			optByNumber.set_Checked(true);
			DisplayIndex(ForCurrentEvent: false);
		}

		private void cmbAlpha_SelectedIndexChanged(object sender, EventArgs e)
		{
			optByName.set_Checked(true);
			DisplayIndex(ForCurrentEvent: false);
		}

		private void cmbAsteroidClasses_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (Loaded)
			{
				optByClass.set_Checked(true);
			}
			DisplayIndex(ForCurrentEvent: false);
		}

		private void optByDate_CheckedChanged(object sender, EventArgs e)
		{
			DisplayIndex(ForCurrentEvent: false);
		}

		private void optByName_CheckedChanged(object sender, EventArgs e)
		{
			DisplayIndex(ForCurrentEvent: false);
		}

		private void optByNumber_CheckedChanged(object sender, EventArgs e)
		{
			DisplayIndex(ForCurrentEvent: false);
		}

		private void optClass_CheckedChanged(object sender, EventArgs e)
		{
			DisplayIndex(ForCurrentEvent: false);
		}

		private void optByRecords_CheckedChanged(object sender, EventArgs e)
		{
			DisplayIndex(ForCurrentEvent: false);
		}

		private void optbyChords_CheckedChanged(object sender, EventArgs e)
		{
			DisplayIndex(ForCurrentEvent: false);
		}

		private void optByPlanets_CheckedChanged(object sender, EventArgs e)
		{
			DisplayIndex(ForCurrentEvent: false);
		}

		private void chkShapeModelsOnly_CheckedChanged(object sender, EventArgs e)
		{
			DisplayIndex(ForCurrentEvent: false);
		}

		private void ChkUnfitted_CheckedChanged(object sender, EventArgs e)
		{
			DisplayIndex(ForCurrentEvent: false);
		}

		internal void DisplayIndex(bool ForCurrentEvent)
		{
			int TotalNumber = 0;
			int num = 0;
			if (!Loaded)
			{
				return;
			}
			Asteroid_Observations_Reports.HistoricalIndex.Clear();
			string classTest = "X";
			if (optByClass.get_Checked())
			{
				if (AsteroidClassList.AClass.Count < 1)
				{
					AsteroidClassList.Fill_AllAsteroids();
				}
				classTest = cmbAsteroidClasses.get_Items().get_Item(((ListControl)cmbAsteroidClasses).get_SelectedIndex()).ToString();
			}
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count < 1)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			for (int i = 0; i < Data_and_Plots.Historical_AllEvents.OccEvents.Count; i++)
			{
				AddToIndexList(Data_and_Plots.Historical_AllEvents.OccEvents[i].IndexLine + i.ToString().PadLeft(5), ref TotalNumber, classTest);
			}
			if (optByDate.get_Checked())
			{
				HistoricalIndexData.SortField = 2;
			}
			else if (optByName.get_Checked())
			{
				HistoricalIndexData.SortField = 0;
			}
			else if (optByNumber.get_Checked())
			{
				HistoricalIndexData.SortField = 1;
			}
			else if (optByClass.get_Checked())
			{
				HistoricalIndexData.SortField = 1;
			}
			else if (optByPlanets.get_Checked())
			{
				HistoricalIndexData.SortField = 4;
			}
			else if (optbyChords.get_Checked())
			{
				HistoricalIndexData.SortField = 5;
			}
			else
			{
				HistoricalIndexData.SortField = 3;
			}
			if (ForCurrentEvent)
			{
				num = ((ListControl)cmbHistorical).get_SelectedIndex();
			}
			Asteroid_Observations_Reports.HistoricalIndex.Sort();
			cmbHistorical.get_Items().Clear();
			for (int j = 0; j < Asteroid_Observations_Reports.HistoricalIndex.Count; j++)
			{
				cmbHistorical.get_Items().Add((object)Asteroid_Observations_Reports.HistoricalIndex[j].ToString());
			}
			if (ForCurrentEvent)
			{
				try
				{
					((ListControl)cmbHistorical).set_SelectedIndex(num);
				}
				catch
				{
					try
					{
						((ListControl)cmbHistorical).set_SelectedIndex(num - 1);
					}
					catch
					{
						((Control)cmbHistorical).set_Text("");
						((ListControl)cmbHistorical).set_SelectedIndex(-1);
					}
				}
			}
			else
			{
				((Control)cmbHistorical).set_Text("");
				((ListControl)cmbHistorical).set_SelectedIndex(-1);
			}
			((Control)lblNumbers).set_Text(cmbHistorical.get_Items().get_Count() + " / " + TotalNumber);
			((Control)lblCurrentSelected).set_Text((((ListControl)cmbHistorical).get_SelectedIndex() + 1).ToString());
			StartTime = DateTime.Now;
		}

		private void AddToIndexList(string IndexLine, ref int TotalNumber, string ClassTest)
		{
			HistoricalIndexData historicalIndexData = new HistoricalIndexData();
			historicalIndexData.ReadIndexLine(IndexLine);
			string text = cmbAlpha.get_Items().get_Item(((ListControl)cmbAlpha).get_SelectedIndex()).ToString()!.Trim();
			int num = cmbYearRange.get_Items().get_Count() - 1 - ((ListControl)cmbYearRange).get_SelectedIndex();
			int num2 = cmbYearRange.get_Items().get_Count() - 22 - ((ListControl)cmbYearRange).get_SelectedIndex();
			int selectedIndex = ((ListControl)cmbNumberRange).get_SelectedIndex();
			TotalNumber++;
			if ((chkShapeModelsOnly.get_Checked() & !historicalIndexData.HasShapeModel) || (ChkUnfitted.get_Checked() & !historicalIndexData.HasUnfittedModel))
			{
				return;
			}
			if (optByDate.get_Checked())
			{
				switch (num)
				{
				case 0:
					Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					return;
				case 1:
					if (historicalIndexData.Year < 1990)
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
					return;
				case 2:
					if ((historicalIndexData.Year > 1989) & (historicalIndexData.Year < 2000))
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
					return;
				}
				if (num < 21)
				{
					if (historicalIndexData.Year == 1997 + num)
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
					return;
				}
				int num3 = 2018 + num2 / 2;
				int num4 = 6 * (num2 % 2) + 1;
				if (historicalIndexData.Year == num3)
				{
					if ((num4 > 6) & (historicalIndexData.Month > 6))
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
					else if ((num4 < 7) & (historicalIndexData.Month < 7))
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
				}
			}
			else if (optByName.get_Checked())
			{
				if (text == "All")
				{
					Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
				}
				else if (text == historicalIndexData.AsteroidID.Substring(0, 1))
				{
					Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
				}
			}
			else if (optByNumber.get_Checked())
			{
				switch (selectedIndex)
				{
				case 0:
					Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					break;
				case 1:
					if ((historicalIndexData.AsteroidNum > 0) & (historicalIndexData.AsteroidNum < 50))
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
					break;
				case 2:
					if ((historicalIndexData.AsteroidNum >= 50) & (historicalIndexData.AsteroidNum < 100))
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
					break;
				case 3:
					if ((historicalIndexData.AsteroidNum >= 100) & (historicalIndexData.AsteroidNum < 150))
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
					break;
				case 4:
					if ((historicalIndexData.AsteroidNum >= 150) & (historicalIndexData.AsteroidNum < 200))
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
					break;
				case 5:
					if ((historicalIndexData.AsteroidNum >= 200) & (historicalIndexData.AsteroidNum < 299))
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
					break;
				case 6:
					if ((historicalIndexData.AsteroidNum >= 300) & (historicalIndexData.AsteroidNum < 400))
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
					break;
				case 7:
					if ((historicalIndexData.AsteroidNum >= 400) & (historicalIndexData.AsteroidNum < 550))
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
					break;
				case 8:
					if ((historicalIndexData.AsteroidNum >= 550) & (historicalIndexData.AsteroidNum < 700))
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
					break;
				case 9:
					if ((historicalIndexData.AsteroidNum >= 700) & (historicalIndexData.AsteroidNum < 900))
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
					break;
				case 10:
					if ((historicalIndexData.AsteroidNum >= 900) & (historicalIndexData.AsteroidNum < 1300))
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
					break;
				case 11:
					if ((historicalIndexData.AsteroidNum >= 1300) & (historicalIndexData.AsteroidNum < 2000))
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
					break;
				case 12:
					if ((historicalIndexData.AsteroidNum >= 2000) & (historicalIndexData.AsteroidNum < 3000))
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
					break;
				case 13:
					if ((historicalIndexData.AsteroidNum >= 3000) & (historicalIndexData.AsteroidNum < 5000))
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
					break;
				case 14:
					if ((historicalIndexData.AsteroidNum >= 5000) & (historicalIndexData.AsteroidNum < 10000))
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
					break;
				case 15:
					if ((historicalIndexData.AsteroidNum >= 10000) & (historicalIndexData.AsteroidNum < 50000))
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
					break;
				case 16:
					if (historicalIndexData.AsteroidNum >= 50000)
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
					break;
				default:
					if (historicalIndexData.AsteroidNum == 0)
					{
						Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
					}
					break;
				}
			}
			else if (optByClass.get_Checked())
			{
				if (AsteroidClassList.ClassOfAnAsteroid(historicalIndexData.AsteroidNum).Contains(ClassTest))
				{
					Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
				}
			}
			else if (optByPlanets.get_Checked())
			{
				if (historicalIndexData.AsteroidNumber.Contains("P"))
				{
					Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
				}
			}
			else
			{
				Asteroid_Observations_Reports.HistoricalIndex.Add(historicalIndexData);
			}
		}

		private void EditTimer_Tick(object sender, EventArgs e)
		{
			TimeSpan timeSpan = DateTime.Now.Subtract(StartTime);
			if (timeSpan.Hours > 0)
			{
				((Control)lblLastSaveTime).set_Text("> 1hour)");
			}
			((Control)lblLastSaveTime).set_Text(timeSpan.Minutes + "m " + timeSpan.Seconds.ToString().PadLeft(2, '0') + "s");
		}

		private void MagDropTimer_Tick(object sender, EventArgs e)
		{
			CurrentColor++;
			if (CurrentColor >= ColorCount)
			{
				CurrentColor = 0;
			}
			((Control)lblMagDrop).set_ForeColor(C[CurrentColor]);
		}

		private void cmbHistorical_SelectedIndexChanged(object sender, EventArgs e)
		{
			EventLoadedFromFile = false;
			PreviousEventMidHour = -1.0;
			((Control)lblCurrentSelected).set_Text((((ListControl)cmbHistorical).get_SelectedIndex() + 1).ToString());
			((Control)lblLastSaveTime).set_ForeColor(Color.Blue);
			if (((ListControl)cmbHistorical).get_SelectedIndex() < 0)
			{
				return;
			}
			if (LightCurveViewer != null)
			{
				LightCurveViewer.CloseThis();
			}
			CurrentHistoricalRecord = Asteroid_Observations_Reports.HistoricalIndex[((ListControl)cmbHistorical).get_SelectedIndex()].StartRecord;
			Data_and_Plots.Historical_AllEvents.DecodeAnEvent_Into_EventDetails(CurrentHistoricalRecord);
			((Control)lblRecordNumber).set_Text("# " + CurrentHistoricalRecord);
			((Control)lblRecNoDel).set_Text(CurrentHistoricalRecord.ToString());
			ToDeleteFileName = EventDetails.Year + EventDetails.Month.ToString().PadLeft(2, '0') + EventDetails.Day.ToString().PadLeft(2, '0') + "_(" + EventDetails.AsteroidNumber + ")_" + EventDetails.AsteroidID + "_Del.xml";
			((Control)lblDeleteName).set_Text(ToDeleteFileName);
			Data_and_Plots.FirstTimePlot = true;
			NewEventForShapeModelling = true;
			VisibilityOfControls(HistoricalEnabled: true);
			((Control)this).set_Text("Asteroid observations editor : Historical file - " + cmbHistorical.get_Items().get_Item(((ListControl)cmbHistorical).get_SelectedIndex()).ToString());
			ReadingMultipleOBSFiles = false;
			ReadingMultipleEurasterEntries = false;
			ReadingMultipleJPEntries = false;
			((ToolStripItem)skipToNextFileToolStripMenuItem).set_Visible(false);
			SetPasteEnabled(PasteEnabled: true);
			((Control)Data_and_Plots.PlotForm.panelShapeModelControl).set_Visible(false);
			DisplayEventInEditor();
			PopulateExportCommentsForm(WhenExporting: false);
			if (chkAutoOpenSave.get_Checked() & !AutoSaveSingle)
			{
				GetAsteroidPosition(UseExistingStarPosition: true);
				string text = cmbHistorical.get_Items().get_Item(((ListControl)cmbHistorical).get_SelectedIndex()).ToString()!.Trim();
				int num = int.Parse(text.Substring(text.Length - 3));
				AutoSaveSingle = true;
				if (num < 1000)
				{
					UpdateHistorical(AutoSaveSingle);
				}
			}
			AutoSaveSingle = false;
			StartTime = DateTime.Now;
		}

		private void CloseInfoForms()
		{
			try
			{
				((Control)AltitudeCheck).Hide();
			}
			catch
			{
			}
			try
			{
				((Control)D_Doubles).Hide();
			}
			catch
			{
			}
			try
			{
				((Control)D_NearbyStars).Hide();
			}
			catch
			{
			}
			try
			{
				((Control)D_Satellites).Hide();
			}
			catch
			{
			}
			try
			{
				((Control)D_Stars).Hide();
			}
			catch
			{
			}
			try
			{
				((Control)BinaryAsteroids.BinaryAsteroidsDisplay).Hide();
			}
			catch
			{
			}
		}

		private void DisplayEventInEditor()
		{
			DisplayEventInEditor(Display_LightCurves_ShapeModels: true);
		}

		private void DisplayEventInEditor(bool Display_LightCurves_ShapeModels)
		{
			//IL_0184: Unknown result type (might be due to invalid IL or missing references)
			//IL_018a: Invalid comparison between Unknown and I4
			CloseInfoForms();
			bool flag = false;
			Data_and_Plots.KnownBinaryAsteroidElements_RecNums.Clear();
			Data_and_Plots.PlotForm.addShapeImageFromDAMIT_ISAMToolStripMenuItem.set_Checked(false);
			Data_and_Plots.PlotForm.mnuAlign.set_Checked(false);
			if (EventDetails.StarIsDouble)
			{
				Data_and_Plots.PlotForm.mnuAlign.set_Checked(true);
			}
			else if (EventDetails.AsteroidHasSatellite)
			{
				Data_and_Plots.PlotForm.mnuAlign.set_Checked(false);
			}
			Data_and_Plots.PlotForm.ErrorBarsForFittingToShapeModels_menu.set_Checked(true);
			Data_and_Plots.PlotForm.Set_ColorFor_cmdForDiameters();
			try
			{
				((Form)DisplayMPOccultations.Chart).Close();
			}
			catch
			{
			}
			Data_and_Plots.Year = EventDetails.Year;
			Data_and_Plots.Month = EventDetails.Month;
			Data_and_Plots.Day = EventDetails.Day;
			Data_and_Plots.TZero_forMotions = EventDetails.MidT_forMotions;
			((Control)txtYear).set_Text(EventDetails.Year.ToString());
			((Control)txtMonth).set_Text(EventDetails.Month.ToString());
			((Control)txtDay).set_Text(EventDetails.Day.ToString());
			TextBox obj2 = txtMidT;
			double DiameterUncertainty = EventDetails.MidT_forMotions;
			((Control)obj2).set_Text(DiameterUncertainty.ToString());
			Panel panelDouble = Data_and_Plots.PlotForm.panelDouble;
			bool visible;
			((Control)Data_and_Plots.PlotForm.panelDouble).set_Visible(visible = false);
			((Control)panelDouble).set_Visible(visible);
			if (EventDetails.StarCat == "HIP" && Gaia.GetBadHipStar_EntryName(EventDetails.StarNumber, out var CorrectedStarID) && (int)MessageBox.Show(EventDetails.StarCatwithNumber + " is held in the Occult Gaia catalogue as\r\n\r\n" + CorrectedStarID + "\r\n\r\nThe HIP star number MUST be corrected.\r\nIf you do not correct it, the astrometry from the observation will be wrong.\r\n\r\nDo you want to correct the star number ?", "HIP star identifier", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6)
			{
				if (CorrectedStarID.Contains("UCAC"))
				{
					EventDetails.StarCat = "UCAC4";
					EventDetails.StarNumber = CorrectedStarID.Substring(5).Trim();
				}
				else if (CorrectedStarID.Contains("J"))
				{
					EventDetails.StarCat = "J-coords";
					EventDetails.StarNumber = CorrectedStarID.Substring(1).Trim();
				}
				else if (CorrectedStarID.Contains("J"))
				{
					EventDetails.StarCat = "Tycho2";
					EventDetails.StarNumber = CorrectedStarID.Substring(3).Trim();
				}
				flag = true;
			}
			if (EventDetails.StarCat == "TYC")
			{
				EventDetails.StarCat = "Tycho2";
			}
			if (EventDetails.StarCat == "HIP")
			{
				((ListControl)cmbCatalogue).set_SelectedIndex(0);
				((Control)txtHip).set_Text(EventDetails.StarNumber.Trim());
			}
			else if (EventDetails.StarCat == "Tycho2")
			{
				((ListControl)cmbCatalogue).set_SelectedIndex(1);
				string[] array = EventDetails.StarNumber.Trim().Split(new char[1] { '-' });
				((Control)txtTycRegion).set_Text(array[0].Trim());
				((Control)txtTycSeqNum).set_Text(array[1].Trim());
				((Control)txtTycComp).set_Text(array[2].Replace("u", "").Trim());
				((Control)lblTycho_UCAC).set_Text("");
			}
			else if (EventDetails.StarCat == "UCAC4")
			{
				((ListControl)cmbCatalogue).set_SelectedIndex(2);
				string[] array = EventDetails.StarNumber.Trim().Split(new char[1] { '-' });
				((Control)txtU4Zone).set_Text(array[0].Trim());
				((Control)txtU4Number).set_Text(array[1].Trim());
			}
			else if (EventDetails.StarCat == "USNO-B1")
			{
				((ListControl)cmbCatalogue).set_SelectedIndex(3);
				string[] array = EventDetails.StarNumber.Trim().Split(new char[1] { '-' });
				((Control)txtB1zone).set_Text(array[0].Trim());
				((Control)txtB1number).set_Text(array[1].Trim());
			}
			else if (EventDetails.StarCat == "NOMAD")
			{
				((ListControl)cmbCatalogue).set_SelectedIndex(4);
				string[] array = EventDetails.StarNumber.Trim().Split(new char[1] { '-' });
				((Control)txtNOMADzone).set_Text(array[0].Trim());
				((Control)txtNOMADnumber).set_Text(array[1].Trim());
			}
			else if ((EventDetails.StarCat == "J-coords") | (EventDetails.StarCat == "G-coords"))
			{
				((ListControl)cmbCatalogue).set_SelectedIndex(5);
				((Control)txtGaiaCoords).set_Text(EventDetails.StarNumber.Trim());
			}
			Data_and_Plots.StarRA = EventDetails.RA_Star_2000;
			Data_and_Plots.StarDec = EventDetails.Dec_Star_2000;
			Data_and_Plots.RAStar_Apparent = EventDetails.RA_Star_Apparent;
			Data_and_Plots.DecStar_Apparent = EventDetails.Dec_Star_Apparent;
			StarMag = EventDetails.MgStar;
			SetStarIssues();
			((Control)lblRA).set_Text("RA = " + Utilities.DEGtoDMS(EventDetails.RA_Star_2000 * (180.0 / Math.PI) / 15.0, 2, 5, MinutesOnly: false));
			((Control)lblDec).set_Text("Dec = " + Utilities.DEGtoDMS(EventDetails.Dec_Star_2000 * (180.0 / Math.PI), 3, 4, MinutesOnly: false, IncludeLeadingZeros: false, IncludePlusSymbol: true));
			((Control)lblMag).set_Text(string.Format("Mv = {0,3:F2}", StarMag));
			((Control)lblinGaiaDR2).set_Visible(EventDetails.GaiaVersion >= 0);
			if (EventDetails.GaiaVersion >= 0)
			{
				((Control)lblinGaiaDR2).set_Text(Gaia.StarSourceCat[EventDetails.GaiaVersion]);
			}
			((Control)lblRUWE).set_Text(string.Format("RUWE = {0,1:f2}", EventDetails.StarReliability));
			if (EventDetails.StarReliability > 1.4)
			{
				((Control)lblRUWE).set_ForeColor(Color.Red);
			}
			else
			{
				((Control)lblRUWE).set_ForeColor(Color.Green);
			}
			if (BinaryAsteroids.BinElements.Count < 1)
			{
				BinaryAsteroids.Fill_AllAsteroids();
			}
			if (EventDetails.AsteroidNumber.EndsWith("P") | EventDetails.AsteroidNumber.Contains("P/") | EventDetails.AsteroidNumber.Contains("C/") | EventDetails.AsteroidNumber.Contains("A/"))
			{
				optComets.set_Checked(true);
				((Control)txtCometNumber).set_Text(EventDetails.AsteroidNumber);
				((Control)txtCometName).set_Text(EventDetails.AsteroidID);
			}
			else if (EventDetails.AsteroidNumber.Contains("P") & EventDetails.AsteroidNumber.Contains("M"))
			{
				optPlanet.set_Checked(true);
				string text = EventDetails.AsteroidNumber.Trim();
				int num = int.Parse(text.Substring(1, 1));
				((ListControl)cmbPlanets).set_SelectedIndex(num - Convert.ToInt16(num > 3));
				int num2 = int.Parse(text.Substring(3));
				if (num == 6 && num2 > 9)
				{
					((ListControl)cmbMoons).set_SelectedIndex(num2 - 2);
				}
				else
				{
					((ListControl)cmbMoons).set_SelectedIndex(num2);
				}
			}
			else
			{
				optAsteroid.set_Checked(true);
				((Control)txtAsteroidNumber).set_Text(EventDetails.AsteroidNumber);
				((Control)txtAsteroidName).set_Text(EventDetails.AsteroidID);
				BinaryAsteroids.GetAsteroidRecord_fromNumberName(EventDetails.AsteroidNo, EventDetails.AsteroidID, Data_and_Plots.KnownBinaryAsteroidElements_RecNums);
			}
			Data_and_Plots.Parallax = EventDetails.Parallax;
			Data_and_Plots.dParallax = EventDetails.dParallax;
			MagAsteroid = EventDetails.MvAsteroid;
			Data_and_Plots.Diameter = EventDetails.AsteroidNominalDiameter;
			string text2 = "{0,1:f1}";
			if (Data_and_Plots.Diameter > 200.0)
			{
				text2 = "{0,1:f0}";
			}
			((Control)Data_and_Plots.PlotForm.chkUseAssumedDiameter).set_Text(string.Format("Use assumed\r\ndia (" + text2 + " km)", Data_and_Plots.Diameter));
			Data_and_Plots.DiameterUncertainty = EventDetails.DiameterUncertainty;
			((Control)cmdPredict).set_Enabled((((Control)txtAsteroidNumber).get_Text().Trim().Length > 0) & optAsteroid.get_Checked());
			if (!EventDetails.SourceFileIsXML)
			{
				GetStarPosition(ForStarDiameter: false, UpdateStarOnly: false);
				GetAsteroidPosition();
			}
			((Control)lblMotions).set_Text(string.Format("dX= {0,6:F3} km/s  dY= {1,6:F3}km/s", EventDetails.dX * 6378.137 / 3600.0, EventDetails.dY * 6378.137 / 3600.0));
			((Control)lblParallax).set_Text(string.Format("={0,6:F4}\" {1:+0.0;-0.0}mas/hr", Data_and_Plots.Parallax, Data_and_Plots.dParallax * 1000.0));
			((Control)lblMagnitude).set_Text(string.Format("Mv: {0,4:F2}", MagAsteroid));
			double num3 = Data_and_Plots.Diameter / 6378.137 * Data_and_Plots.Parallax;
			if (Data_and_Plots.Diameter < 2.0)
			{
				((Control)lblDiameterKM).set_Text(string.Format("Dia: {0,1:F0}m ±{1,1:f0} m = ", Data_and_Plots.Diameter * 1000.0, Data_and_Plots.DiameterUncertainty * 1000.0));
			}
			else if (Data_and_Plots.Diameter < 5.0)
			{
				((Control)lblDiameterKM).set_Text(string.Format("Dia: {0,1:F2} ±{1,1:f2} km = ", Data_and_Plots.Diameter, Data_and_Plots.DiameterUncertainty));
			}
			else if (Data_and_Plots.Diameter < 10.0)
			{
				((Control)lblDiameterKM).set_Text(string.Format("Dia: {0,1:F1} ±{1,1:f1} km = ", Data_and_Plots.Diameter, Data_and_Plots.DiameterUncertainty));
			}
			else
			{
				((Control)lblDiameterKM).set_Text(string.Format("Dia: {0,1:F0} ±{1,1:f0} km = ", Data_and_Plots.Diameter, Data_and_Plots.DiameterUncertainty));
			}
			if (optAsteroid.get_Checked())
			{
				Utilities.Get_SingleAsteroid_Diameter_and_UncertaintyInfo(EventDetails.AsteroidNo, out var _, out DiameterUncertainty, out var Source);
				if ((Source == "6") | (EventDetails.AsteroidNo > 550000))
				{
					((Control)lblDiameterKM).set_Text("est." + ((Control)lblDiameterKM).get_Text());
				}
			}
			if (num3 < 0.05)
			{
				Label obj3 = lblDiameterKM;
				((Control)obj3).set_Text(((Control)obj3).get_Text() + string.Format("{0,1:F1} mas", num3 * 1000.0));
			}
			else if (num3 < 1.0)
			{
				Label obj4 = lblDiameterKM;
				((Control)obj4).set_Text(((Control)obj4).get_Text() + string.Format("{0,1:F0} mas", num3 * 1000.0));
			}
			else if (num3 < 10.0)
			{
				Label obj5 = lblDiameterKM;
				((Control)obj5).set_Text(((Control)obj5).get_Text() + string.Format("{0,1:F2}\"", num3));
			}
			else
			{
				Label obj6 = lblDiameterKM;
				((Control)obj6).set_Text(((Control)obj6).get_Text() + string.Format("{0,1:F1}\"", num3));
			}
			int width = ((Control)lblDiameterKM).get_Width();
			((Control)lblDiameterKM).set_Left((((Control)grpAsteroid).get_Width() - width) / 2);
			((Control)txtMPCyear).set_Text(EventDetails.MPCDate.PadRight(8).Substring(0, 4));
			((Control)txtMPCmonth).set_Text(EventDetails.MPCDate.PadRight(8).Substring(4, 2));
			((Control)txtMPCday).set_Text(EventDetails.MPCDate.PadRight(8).Substring(6, 2));
			((Control)txtMPEC).set_Text(EventDetails.MPCNumber);
			((Control)txtMPC_ID).set_Text(EventDetails.MPCsubmissionID);
			SetMPCtext();
			PopulateListOfObservers(0);
			((Control)cmdAdd).set_Text("ADD  as new #" + $"{EventDetails.Observers.Count + 1:F0}");
			Data_and_Plots.CurrentSolutionLabel();
			((Control)lblLastUpdate).set_Text("Added : " + EventDetails.YearAdded + " " + Utilities.ShortMonths[EventDetails.MonthAdded] + " " + EventDetails.DayAdded + "     Updated : " + EventDetails.YearEdited + " " + Utilities.ShortMonths[EventDetails.MonthEdited] + " " + EventDetails.DayEdited);
			SetFadeIndicator();
			SetStarID();
			((Control)Data_and_Plots.PlotForm).Focus();
			Data_and_Plots.PlotForm.Populate_ReviewList(EventDetails.FlagForReview);
			Data_and_Plots.PlotForm.CollapseReviewList();
			if (Display_LightCurves_ShapeModels)
			{
				SetLightCurveFlag();
			}
			if (EventDetails.ShapeModelFitted)
			{
				if (Settings.Default.AutoShapeModels)
				{
					DisplayShapeModelFits();
				}
			}
			else
			{
				try
				{
					((Control)D_ShapeModels).Hide();
				}
				catch
				{
				}
			}
			if (EventDetails.StarIsDouble)
			{
				if (Settings.Default.AutoDoubles)
				{
					DisplayDoubleStarSolution();
				}
			}
			else
			{
				try
				{
					((Control)D_Doubles).Hide();
				}
				catch
				{
				}
			}
			if (Settings.Default.AutoNearbyStars)
			{
				ShowNearbyStars();
			}
			UpdatingData = false;
			if ((((Control)lblInDAMIT).get_Visible() | ((Control)lblISAM).get_Visible()) && Display_LightCurves_ShapeModels)
			{
				GetShapeModels();
				Data_and_Plots.PlotForm.showShapeModelControlsOnPlotToolStripMenuItem.set_Checked(true);
				((Control)Data_and_Plots.PlotForm.panelShapeModelControl).set_Visible(true);
			}
			else
			{
				Data_and_Plots.PlotForm.ErrorBarsForFittingToShapeModels_menu.set_Checked(false);
				Data_and_Plots.PlotEventOnScreen();
			}
			if (global::GaiaDoubles.GaiaDoubles.GaiaDouble_Match(EventDetails.RA_Star_2000 * (180.0 / Math.PI), EventDetails.Dec_Star_2000 * (180.0 / Math.PI), GaiaCatIDOnly: false, DetailsOnly: false, out var FullDetails))
			{
				try
				{
					((Control)Non_Single_Gaia).Show();
				}
				catch
				{
					Non_Single_Gaia = new GaiaNonSingleStar();
					((Control)Non_Single_Gaia).set_BackColor(Color.Aquamarine);
					((Control)Non_Single_Gaia).Show();
				}
				((Control)Non_Single_Gaia.txtOut).set_Text(FullDetails);
				((Control)Non_Single_Gaia).set_Text("Gaia Non-Single-Star entry found for : " + EventDetails.StarCat + " " + EventDetails.StarNumber);
				((Control)Non_Single_Gaia).set_ForeColor(Color.DarkRed);
			}
			else
			{
				try
				{
					((Form)Non_Single_Gaia).Close();
				}
				catch
				{
				}
			}
			try
			{
				((Form)GaiaMagMeasures).Close();
			}
			catch
			{
			}
			if (flag)
			{
				GetStarPosition(ForStarDiameter: false, UpdateStarOnly: false);
			}
		}

		private void SetMPCtext()
		{
			if ((EventDetails.MPCDate.Trim().Length < 1) | (EventDetails.MPCsubmissionID.Trim().Length == 0))
			{
				if ((EventDetails.Quality == 0) | (EventDetails.Quality == 5) | (EventDetails.Quality == 6))
				{
					((Control)lblMPC).set_Text("Will not be reported to MPC");
				}
				else
				{
					((Control)lblMPC).set_Text("Not yet reported to MPC");
				}
				toolTip.SetToolTip((Control)(object)lblMPC, "");
			}
			else
			{
				string text = EventDetails.MPCDate.Substring(0, 4) + " " + Utilities.ShortMonths[int.Parse(EventDetails.MPCDate.Substring(4, 2))] + " " + EventDetails.MPCDate.Substring(6);
				if (EventDetails.MPCsubmissionID.Length > 5)
				{
					((Control)lblMPC).set_Text("MPC reported on " + text);
					toolTip.SetToolTip((Control)(object)lblMPC, "Observation ID = \r\n" + EventDetails.MPCsubmissionID);
				}
				else
				{
					toolTip.SetToolTip((Control)(object)lblMPC, "");
				}
				if (EventDetails.MPCNumber.Trim().Length > 0)
				{
					((Control)lblMPC).set_Text("→MPC " + text.Replace("  ", " ") + ", Circ# " + EventDetails.MPCNumber.Trim());
				}
			}
			((Control)lblMPC).set_Left(((Control)lstObservations).get_Right() - ((Control)lblMPC).get_Width());
		}

		internal void SetStarIssues()
		{
			((Control)lblStarIssues).set_Text("");
			if (EventDetails.GaiaVersion == 0)
			{
				((Control)lblStarIssues).set_Text("Position from Hipparcos2");
			}
			else if (EventDetails.GaiaPMfromUCAC4 == 1)
			{
				((Control)lblStarIssues).set_Text("Proper motion from UCAC4");
			}
			else if (EventDetails.NoGaiaPM == 1)
			{
				((Control)lblStarIssues).set_Text("No proper motion");
			}
		}

		internal void PopulateListOfObservers(int FocusLine)
		{
			lstObservations.get_Items().Clear();
			bool num = (EventDetails.UnseenBinaryPrimary = false);
			EventDetails.AsteroidHasSatellite = num;
			EventDetails.StarIsDouble = num;
			for (int i = 0; i < EventDetails.Observers.Count; i++)
			{
				if (EventDetails.Observers[i].PlotCode != "x")
				{
					if ("drm".Contains(EventDetails.Observers[i].Event_D) | "drm".Contains(EventDetails.Observers[i].Event_R))
					{
						EventDetails.StarIsDouble = true;
					}
					if ("GBgb".Contains(EventDetails.Observers[i].Event_D) | "GBgb".Contains(EventDetails.Observers[i].Event_R))
					{
						EventDetails.AsteroidHasSatellite = true;
					}
					if ("e".Contains(EventDetails.Observers[i].Event_D) | "f".Contains(EventDetails.Observers[i].Event_R))
					{
						EventDetails.UnseenBinaryPrimary = true;
					}
				}
			}
			for (int j = 0; j < EventDetails.Observers.Count; j++)
			{
				lstObservations.get_Items().Add((object)EventDetails.Observers[j].EncodeObserverInEditorToFixedFormatLine());
			}
			if (FocusLine == EventDetails.Observers.Count)
			{
				FocusLine--;
			}
			if (FocusLine >= 0)
			{
				if (FocusLine < EventDetails.Observers.Count)
				{
					((ListControl)lstObservations).set_SelectedIndex(FocusLine);
				}
				else if (EventDetails.Observers.Count > 0 && FocusLine > 0)
				{
					((ListControl)lstObservations).set_SelectedIndex(FocusLine);
				}
			}
			((Control)lstObservations).Focus();
			if (((ListControl)lstObservations).get_SelectedIndex() >= 0)
			{
				((Control)cmdReplace).set_Text("REPLACE  record #" + $"{EventDetails.Observers[((ListControl)lstObservations).get_SelectedIndex()].SeqNumber:F0}");
				((Control)cmdDelete).set_Text("DELETE  record #" + $"{EventDetails.Observers[((ListControl)lstObservations).get_SelectedIndex()].SeqNumber:F0}");
			}
			else
			{
				((Control)cmdReplace).set_Text("REPLACE  record #");
				((Control)cmdDelete).set_Text("DELETE  record #");
			}
		}

		private void lstObservations_SelectedIndexChanged(object sender, EventArgs e)
		{
			Observation_DisplayInEditor(((ListControl)lstObservations).get_SelectedIndex());
			((Control)cmdReplace).set_Text("REPLACE  record #" + $"{EventDetails.Observers[((ListControl)lstObservations).get_SelectedIndex()].SeqNumber:F0}");
			((Control)cmdDelete).set_Text("DELETE  record #" + $"{EventDetails.Observers[((ListControl)lstObservations).get_SelectedIndex()].SeqNumber:F0}");
		}

		private void Observation_DisplayInEditor(int Index)
		{
			UpdatingData = true;
			Data_and_Plots.CloseClosestMissTimes();
			((Control)txtObserver1).set_Text(EventDetails.Observers[Index].Observer1.Trim());
			((Control)txtObserver2).set_Text(EventDetails.Observers[Index].Observer2.Trim());
			chkEtAl.set_Checked(EventDetails.Observers[Index].MoreThan2Observers);
			((Control)txtLocatedNear).set_Text(EventDetails.Observers[Index].NearTo.Trim().Replace("&quot;", "\""));
			((Control)txtStateCountry).set_Text(EventDetails.Observers[Index].StateCountry.Trim().ToUpper());
			if (((Control)txtStateCountry).get_Text().Length > 3)
			{
				((Control)txtStateCountry).set_Text(ObserverData.CountryCode(((Control)txtStateCountry).get_Text()));
			}
			if (!EventDetails.Observers[Index].TelescopeAperture_Set)
			{
				((Control)txtAperture).set_Text("");
			}
			else
			{
				((Control)txtAperture).set_Text(EventDetails.Observers[Index].TelescopeAperture.ToString());
			}
			((ListControl)cmbTelescope).set_SelectedIndex(" 123456789".IndexOf(EventDetails.Observers[Index].TelescopeType));
			((ListControl)cmbDatum).set_SelectedIndex(" NETG*".IndexOf(EventDetails.Observers[Index].Datum));
			if (!EventDetails.Observers[Index].Altitude_Set)
			{
				TextBox obj = txtAlt_m;
				string text;
				((Control)txtAlt_ft).set_Text(text = "");
				((Control)obj).set_Text(text);
			}
			else
			{
				((Control)txtAlt_m).set_Text(EventDetails.Observers[Index].Altitude.ToString());
				((Control)txtAlt_ft).set_Text(string.Format("{0,1:F0}", (double)EventDetails.Observers[Index].Altitude / 0.3048));
			}
			string formattedLongitude = EventDetails.Observers[Index].FormattedLongitude;
			((Control)txtLongDeg).set_Text(formattedLongitude.Substring(0, 4).Replace(" ", ""));
			((Control)txtLongMin).set_Text(formattedLongitude.Substring(5, 2).Replace(" ", ""));
			((Control)txtLongSec).set_Text(formattedLongitude.Substring(8).Replace(" ", ""));
			UpdateLongitudes(0);
			formattedLongitude = EventDetails.Observers[Index].FormattedLatitude;
			((Control)txtLatDeg).set_Text(formattedLongitude.Substring(0, 3).Replace(" ", ""));
			((Control)txtLatMin).set_Text(formattedLongitude.Substring(4, 2).Replace(" ", ""));
			((Control)txtLatSec).set_Text(formattedLongitude.Substring(7).Replace(" ", ""));
			UpdateLatitudes(0);
			if (EventDetails.Observers[Index].T_Disappear != 0.0)
			{
				((Control)txtD_Hr).set_Text(EventDetails.Observers[Index].H_Dis.ToString());
				((Control)txtD_Min).set_Text(EventDetails.Observers[Index].M_Dis.ToString());
				((Control)txtD_Sec).set_Text(string.Format("{0:F" + EventDetails.Observers[Index].S_Dis_DecPlaces + "} ", EventDetails.Observers[Index].S_Dis));
			}
			else
			{
				TextBox obj2 = txtD_Hr;
				TextBox obj3 = txtD_Min;
				string text2;
				((Control)txtD_Sec).set_Text(text2 = "");
				string text;
				((Control)obj3).set_Text(text = text2);
				((Control)obj2).set_Text(text);
			}
			if (EventDetails.Observers[Index].Accuracy_D_Set)
			{
				((Control)txtD_Uncert).set_Text(string.Format("{0:F" + EventDetails.Observers[Index].Accuracy_D_DecPlaces + "} ", EventDetails.Observers[Index].Accuracy_D));
			}
			else
			{
				((Control)txtD_Uncert).set_Text("");
			}
			if (EventDetails.Observers[Index].PEQ_D_Set)
			{
				((Control)txtD_PEq).set_Text(string.Format("{0:F" + EventDetails.Observers[Index].PEQ_D_N + "} ", EventDetails.Observers[Index].PEQ_D));
			}
			else
			{
				((Control)txtD_PEq).set_Text("");
			}
			if (EventDetails.Observers[Index].Weight_D_Set)
			{
				((ListControl)cmbD_Wt).set_SelectedIndex(EventDetails.Observers[Index].Weight_D + 1);
			}
			else
			{
				((ListControl)cmbD_Wt).set_SelectedIndex(0);
			}
			if (EventDetails.Observers[Index].T_Reappear != 0.0)
			{
				((Control)txtR_Hr).set_Text(EventDetails.Observers[Index].H_Reap.ToString());
				((Control)txtR_Min).set_Text(EventDetails.Observers[Index].M_Reap.ToString());
				((Control)txtR_Sec).set_Text(string.Format("{0:F" + EventDetails.Observers[Index].S_Reap_DecPlaces + "} ", EventDetails.Observers[Index].S_Reap));
			}
			else
			{
				TextBox obj4 = txtR_Hr;
				TextBox obj5 = txtR_Min;
				string text2;
				((Control)txtR_Sec).set_Text(text2 = "");
				string text;
				((Control)obj5).set_Text(text = text2);
				((Control)obj4).set_Text(text);
			}
			if (EventDetails.Observers[Index].Accuracy_R_Set)
			{
				((Control)txtR_Uncert).set_Text(string.Format("{0:F" + EventDetails.Observers[Index].Accuracy_R_DecPlaces + "} ", EventDetails.Observers[Index].Accuracy_R));
			}
			else
			{
				((Control)txtR_Uncert).set_Text("");
			}
			if (EventDetails.Observers[Index].PEQ_R_Set)
			{
				((Control)txtR_PEq).set_Text(string.Format("{0:F" + EventDetails.Observers[Index].PEQ_R_N + "} ", EventDetails.Observers[Index].PEQ_R));
			}
			else
			{
				((Control)txtR_PEq).set_Text("");
			}
			if (EventDetails.Observers[Index].Weight_R_Set)
			{
				((ListControl)cmbR_Wt).set_SelectedIndex(EventDetails.Observers[Index].Weight_R + 1);
			}
			else
			{
				((ListControl)cmbR_Wt).set_SelectedIndex(0);
			}
			chkDoubleStar.set_Checked((EventDetails.Observers[Index].Event_D == "d") | (EventDetails.Observers[Index].Event_R == "r") | (EventDetails.Observers[Index].Event_D == "g") | (EventDetails.Observers[Index].Event_R == "b") | (EventDetails.Observers[Index].Event_D == "m") | (EventDetails.Observers[Index].Event_R == "m"));
			chkSatellite.set_Checked((EventDetails.Observers[Index].Event_D == "G") | (EventDetails.Observers[Index].Event_R == "B") | (EventDetails.Observers[Index].Event_D == "g") | (EventDetails.Observers[Index].Event_R == "b"));
			chkUnseen.set_Checked((EventDetails.Observers[Index].Event_D == "e") | (EventDetails.Observers[Index].Event_R == "f"));
			chkMiss.set_Checked((EventDetails.Observers[Index].Event_D.ToUpper() == "M") | (EventDetails.Observers[Index].Event_D == "n"));
			chkPredicted.set_Checked(EventDetails.Observers[Index].Event_D.ToUpper() == "P");
			chkRing.set_Checked((EventDetails.Observers[Index].Event_D.ToUpper() == "N") | (EventDetails.Observers[Index].Event_R.ToUpper() == "N"));
			chkNotSeen.set_Checked(EventDetails.Observers[Index].Event_D.ToUpper() == "C");
			((ListControl)cmbMethod).set_SelectedIndex(" abcdefg1234567890".IndexOf(EventDetails.Observers[Index].Method));
			((ListControl)cmbTime).set_SelectedIndex(" abcdefg1234567890".IndexOf(EventDetails.Observers[Index].TimeSource));
			((ListControl)cmbStability).set_SelectedIndex(" 12345678".IndexOf(EventDetails.Observers[Index].Stability));
			((ListControl)cmbTransparency).set_SelectedIndex(" 12345678".IndexOf(EventDetails.Observers[Index].Transparency));
			if (!EventDetails.Observers[Index].SNR_Set)
			{
				updnSNR.set_Value(0m);
			}
			if ((decimal)EventDetails.Observers[Index].SNR > updnSNR.get_Maximum())
			{
				updnSNR.set_Value(updnSNR.get_Maximum());
			}
			else
			{
				updnSNR.set_Value((decimal)((double)Convert.ToInt32(10.0 * EventDetails.Observers[Index].SNR) / 10.0));
			}
			((ListControl)cmbPlotControl).set_SelectedIndex(" yzx".IndexOf(EventDetails.Observers[Index].PlotCode));
			if (EventDetails.Observers[Index].TimeAdjustment_Set)
			{
				((Control)txtShift).set_Text($"{EventDetails.Observers[Index].TimeAdjustment:F2} ");
			}
			else
			{
				((Control)txtShift).set_Text("");
			}
			((Control)txtFree).set_Text(EventDetails.Observers[Index].FreeText);
			MagDrop();
			UpdatingData = false;
		}

		internal double MagDrop()
		{
			if ((EventDetails.MvAsteroid == 0.0) | (EventDetails.MgStar == 0.0))
			{
				((Control)lblMagDrop).set_Text("Mag. drop : --");
				((Control)lblMagDrop).set_ForeColor(SystemColors.ControlText);
				MagDropTimer.Stop();
				return 0.0;
			}
			double num = 0.0;
			double num2 = 0.0;
			double num3 = EventDetails.AsteroidNominalDiameter / 6378.137 * EventDetails.Parallax * 1000.0;
			double num4 = num3 * num3;
			double num5 = EventDetails.StarDia_mas * EventDetails.StarDia_mas;
			double num6 = num5 - num4;
			if (num6 > 0.0)
			{
				num2 = num6 / num5;
			}
			num = -2.5 * Math.Log10(num2 + 1E-09);
			double num7 = 0.0;
			double ExtraMagG = 30.0;
			double ExtraMagR = 30.0;
			int AllStars = -1;
			int BrightStars = -1;
			Gaia.GetNearbyStarsFromCoords(EventDetails.StarCatwithNumber, EventDetails.RA_Star_2000, EventDetails.Dec_Star_2000, EventDetails.MgStar, Utilities.BesselianYear(EventDetails.JD_EventDate) - 2000.0, 30.0, out ExtraMagG, out ExtraMagR, out BrightStars, out AllStars, out var _);
			double num8 = ((!(EventDetails.MvAsteroid > 0.0)) ? Utilities.CombinedMagnitude(ExtraMagG, EventDetails.MvAsteroid) : Utilities.CombinedMagnitude(ExtraMagG, EventDetails.MvAsteroid));
			double num9 = Utilities.CombinedMagnitude(num8, EventDetails.MgStar);
			num7 = Utilities.CombinedMagnitude(num8, EventDetails.MgStar + num) - num9;
			((Control)lblMagDrop).set_Text(string.Format("Mag. drop : {0:F1} ({1,1:f0}%)", num7, 100.0 * (1.0 - Math.Pow(10.0, (0.0 - num7) / 2.5))));
			if (num2 > 0.0)
			{
				Label obj = lblMagDrop;
				((Control)obj).set_Text(((Control)obj).get_Text() + " Annular");
			}
			if (EventDetails.MvAsteroid - num8 > 0.1)
			{
				Label obj2 = lblMagDrop;
				((Control)obj2).set_Text(((Control)obj2).get_Text() + " Nearby Corrected");
			}
			if (num7 > 0.5)
			{
				((Control)lblMagDrop).set_ForeColor(SystemColors.ControlText);
				MagDropTimer.Stop();
			}
			else if (num7 > 0.35)
			{
				((Control)lblMagDrop).set_ForeColor(Color.Red);
				MagDropTimer.Stop();
			}
			else
			{
				MagDropTimer.Start();
			}
			return num7;
		}

		private void SetFadeIndicator()
		{
			double starDia_mas = EventDetails.StarDia_mas;
			double num = EventDetails.AsteroidNominalDiameter / 6378.137 * EventDetails.Parallax * 1000.0;
			((Control)lblStarDiameter).set_Visible(false);
			if (starDia_mas > num)
			{
				((Control)lblStarDiameter).set_Visible(true);
				((Control)lblStarDiameter).set_Text("Annular occultation");
				return;
			}
			if (starDia_mas > 0.5 * num)
			{
				((Control)lblStarDiameter).set_Visible(true);
				((Control)lblStarDiameter).set_Text("Major fades expected");
				return;
			}
			double num2 = 3600.0 * EventDetails.AsteroidNominalDiameter / 6378.137 / Utilities.QuadratureAddition(EventDetails.dX, EventDetails.dY);
			double num3 = starDia_mas / num * num2;
			if (num3 > 0.05)
			{
				((Control)lblStarDiameter).set_Visible(true);
				((Control)lblStarDiameter).set_Text(string.Format("{0,1:f1} sec fades expected", num3));
			}
		}

		private void SetStarID()
		{
			string text = "";
			if (EventDetails.MgStar >= 6.0)
			{
				text = "";
			}
			else
			{
				if (EventDetails.StarCat.ToUpper() == "HIP")
				{
					text = Utilities.StarNameFromHip(int.Parse(EventDetails.StarNumber), EventDetails.MgStar);
				}
				string text2 = Utilities.StarIdentifier_ToMag6(EventDetails.RA_Star_2000 * (180.0 / Math.PI), EventDetails.Dec_Star_2000 * (180.0 / Math.PI), EventDetails.MgStar, WithConstellation: true);
				text = ((!(text == "")) ? (text + ", " + text2) : text2);
			}
			((Control)lblStarID).set_Text(text);
		}

		private void optMeters_CheckedChanged(object sender, EventArgs e)
		{
			AltitudeVisibility();
		}

		private void optFeet_CheckedChanged(object sender, EventArgs e)
		{
			AltitudeVisibility();
		}

		internal void AltitudeVisibility()
		{
			((Control)txtAlt_m).set_Visible(optMeters.get_Checked());
			((Control)txtAlt_ft).set_Visible(!((Control)txtAlt_m).get_Visible());
		}

		private void txtAlt_m_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				if (!double.TryParse(((Control)txtAlt_m).get_Text(), out var result))
				{
					result = 0.0;
				}
				((Control)txtAlt_m).set_Text(string.Format("{0,1:F0}", result));
				((Control)txtAlt_ft).set_Text(string.Format("{0,1:F0}", result / 0.3048));
				UpdatingData = false;
			}
		}

		private void txtAlt_ft_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				if (!double.TryParse(((Control)txtAlt_ft).get_Text(), out var result))
				{
					result = 0.0;
				}
				((Control)txtAlt_ft).set_Text(string.Format("{0,1:F0}", result));
				((Control)txtAlt_m).set_Text(string.Format("{0,1:F0}", result * 0.3048));
				UpdatingData = false;
			}
		}

		private void optDMS_CheckedChanged(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				DMSformatVisibility();
				UpdatingData = false;
			}
		}

		private void optDMM_CheckedChanged(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				DMSformatVisibility();
				UpdatingData = false;
			}
		}

		private void optDDD_CheckedChanged(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				DMSformatVisibility();
				UpdatingData = false;
			}
		}

		private void DMSformatVisibility()
		{
			((Control)panelDMS).set_Visible(optDMS.get_Checked());
			((Control)panelDMM).set_Visible(optDMM.get_Checked());
			((Control)panelDDD).set_Visible(optDDD.get_Checked());
		}

		private void txtLongDeg_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLongitudes(0);
				UpdatingData = false;
			}
		}

		private void txtLongMin_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLongitudes(0);
				UpdatingData = false;
			}
		}

		private void txtLongSec_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLongitudes(0);
				UpdatingData = false;
			}
		}

		private void txtLatDeg_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLatitudes(0);
				UpdatingData = false;
			}
		}

		private void txtLatMin_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLatitudes(0);
				UpdatingData = false;
			}
		}

		private void txtLatSec_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLatitudes(0);
				UpdatingData = false;
			}
		}

		private void txtLongDDD_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLongitudes(2);
				UpdatingData = false;
			}
		}

		private void txtLatDDD_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLatitudes(2);
				UpdatingData = false;
			}
		}

		private void txtLongDmm_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLongitudes(1);
				UpdatingData = false;
			}
		}

		private void txtLongMM_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLongitudes(1);
				UpdatingData = false;
			}
		}

		private void txtLatDmm_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLatitudes(1);
				UpdatingData = false;
			}
		}

		private void txtLatMM_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLatitudes(1);
				UpdatingData = false;
			}
		}

		internal void UpdateLongitudes(int style)
		{
			double result = 0.0;
			double result2 = 0.0;
			double result3 = 0.0;
			bool flag = false;
			switch (style)
			{
			case 0:
				flag = ((Control)txtLongDeg).get_Text().Contains("-");
				if (!double.TryParse(((Control)txtLongDeg).get_Text().Replace('-', ' '), out result))
				{
					result = 0.0;
				}
				if (!double.TryParse(((Control)txtLongMin).get_Text().Replace('-', ' '), out result2))
				{
					result2 = 0.0;
				}
				if (!double.TryParse(((Control)txtLongSec).get_Text().Replace('-', ' '), out result3))
				{
					result3 = 0.0;
				}
				break;
			case 1:
				flag = ((Control)txtLongDmm).get_Text().Contains("-");
				if (!double.TryParse(((Control)txtLongDmm).get_Text().Replace('-', ' '), out result))
				{
					result = 0.0;
				}
				if (!double.TryParse(((Control)txtLongMM).get_Text().Replace('-', ' '), out result2))
				{
					result2 = 0.0;
				}
				break;
			default:
				flag = ((Control)txtLongDDD).get_Text().Contains("-");
				if (!double.TryParse(((Control)txtLongDDD).get_Text().Replace('-', ' '), out result))
				{
					result = 0.0;
				}
				break;
			}
			double num = result + result2 / 60.0 + result3 / 3600.0;
			if (flag)
			{
				num = 0.0 - num;
			}
			string text = Utilities.DEGtoDMS(num, 4, 1, MinutesOnly: false);
			((Control)txtLongDeg).set_Text(text.Substring(0, 4).Replace(" ", ""));
			((Control)txtLongMin).set_Text(text.Substring(5, 2).Replace(" ", ""));
			((Control)txtLongSec).set_Text(text.Substring(8).Replace(" ", ""));
			text = Utilities.DEGtoDMS(num, 4, 3, MinutesOnly: true);
			((Control)txtLongDmm).set_Text(text.Substring(0, 4).Replace(" ", ""));
			((Control)txtLongMM).set_Text(text.Substring(5).Replace(" ", ""));
			((Control)txtLongDDD).set_Text(string.Format("{0,2:F6}", num));
		}

		internal void UpdateLatitudes(int style)
		{
			double result = 0.0;
			double result2 = 0.0;
			double result3 = 0.0;
			bool flag = false;
			switch (style)
			{
			case 0:
				flag = ((Control)txtLatDeg).get_Text().Contains("-");
				if (!double.TryParse(((Control)txtLatDeg).get_Text().Replace('-', ' '), out result))
				{
					result = 0.0;
				}
				if (!double.TryParse(((Control)txtLatMin).get_Text().Replace('-', ' '), out result2))
				{
					result2 = 0.0;
				}
				if (!double.TryParse(((Control)txtLatSec).get_Text().Replace('-', ' '), out result3))
				{
					result3 = 0.0;
				}
				break;
			case 1:
				flag = ((Control)txtLatDmm).get_Text().Contains("-");
				if (!double.TryParse(((Control)txtLatDmm).get_Text().Replace('-', ' '), out result))
				{
					result = 0.0;
				}
				if (!double.TryParse(((Control)txtLatMM).get_Text().Replace('-', ' '), out result2))
				{
					result2 = 0.0;
				}
				break;
			default:
				flag = ((Control)txtLatDDD).get_Text().Contains("-");
				if (!double.TryParse(((Control)txtLatDDD).get_Text().Replace('-', ' '), out result))
				{
					result = 0.0;
				}
				break;
			}
			double num = result + result2 / 60.0 + result3 / 3600.0;
			if (flag)
			{
				num = 0.0 - num;
			}
			string text = Utilities.DEGtoDMS(num, 3, 1, MinutesOnly: false);
			((Control)txtLatDeg).set_Text(text.Substring(0, 3).Replace(" ", ""));
			((Control)txtLatMin).set_Text(text.Substring(4, 2).Replace(" ", ""));
			((Control)txtLatSec).set_Text(text.Substring(7).Replace(" ", ""));
			text = Utilities.DEGtoDMS(num, 3, 3, MinutesOnly: true);
			((Control)txtLatDmm).set_Text(text.Substring(0, 3).Replace(" ", ""));
			((Control)txtLatMM).set_Text(text.Substring(4).Replace(" ", ""));
			((Control)txtLatDDD).set_Text(string.Format("{0,2:F6}", num));
		}

		private void ObservationsEditor_Resize(object sender, EventArgs e)
		{
			int num = ((Control)this).get_Width() - 1086;
			((Control)grpStar).set_Left(194 + num / 4);
			((Control)grpAsteroid).set_Left(457 + num / 2);
			((Control)grpHistory).set_Left(739 + 3 * num / 4);
			((Control)grpTimes).set_Left(330 + num / 4);
			GroupBox obj = grpConditions;
			Label obj2 = lblLastUpdate;
			int num2;
			((Control)cmdCloseExpandedFreeText).set_Left(num2 = 606 + num / 2);
			int left;
			((Control)obj2).set_Left(left = num2);
			((Control)obj).set_Left(left);
			GroupBox obj3 = grpManageObservers;
			((Control)grpManageHistorical).set_Left(left = 862 + 3 * num / 4);
			((Control)obj3).set_Left(left);
			((Control)grpDeletion).set_Location(((Control)grpManageObservers).get_Location());
			((Control)lstObservations).set_Width(((Control)this).get_Width() - 34);
			((Control)pnlMPC).set_Left(((Control)lstObservations).get_Right() - ((Control)pnlMPC).get_Width());
			((Control)lblMPC).set_Left(((Control)lstObservations).get_Right() - ((Control)lblMPC).get_Width());
			((Control)cmdPlusOne).set_Left(1025 + 3 * num / 4);
			((Control)chkAutoOpenSave).set_Left(1032 + 3 * num / 4);
			((Control)lblLastSaveTime).set_Left(1022 + 3 * num / 4);
			((Control)lblRecordNumber).set_Left(((Control)this).get_Width() - 90);
			((Control)cmdCreateLightCurve).set_Top(((Control)this).get_Height() - 55);
			((Control)cmdCreateLightCurve).set_Left(((Control)lstObservations).get_Left() + (((Control)lstObservations).get_Width() - ((Control)cmdCreateLightCurve).get_Width()) / 2);
			int num3 = ((Control)this).get_Height() - ((Control)lstObservations).get_Top() - 44;
			if (num3 > 60)
			{
				((Control)lstObservations).set_Height(num3);
			}
			Settings.Default.SizeAsterEditor = (Point)((Form)this).get_Size();
		}

		private void txtFree_TextChanged(object sender, EventArgs e)
		{
			((Control)lblFreeLeft).set_Text(((Control)txtFree).get_Text().TrimEnd(Array.Empty<char>()).Length.ToString());
		}

		private void txtFree_KeyUp(object sender, KeyEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_0008: Invalid comparison between Unknown and I4
			if ((int)e.get_KeyCode() == 13)
			{
				e.set_SuppressKeyPress(true);
			}
			((Control)lblFreeLeft).set_Text(((Control)txtFree).get_Text().TrimEnd(Array.Empty<char>()).Length.ToString());
		}

		private void txtFree_Leave(object sender, EventArgs e)
		{
			TextFree_Leave();
		}

		private void txtFree_KeyDown(object sender, KeyEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_0008: Invalid comparison between Unknown and I4
			//IL_0012: Unknown result type (might be due to invalid IL or missing references)
			//IL_0019: Invalid comparison between Unknown and I4
			if ((int)e.get_KeyCode() == 13)
			{
				e.set_SuppressKeyPress(true);
			}
			if ((int)e.get_KeyCode() == 9)
			{
				e.set_SuppressKeyPress(true);
			}
		}

		private void txtFree_PreviewKeyDown(object sender, PreviewKeyDownEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_0008: Invalid comparison between Unknown and I4
			if ((int)e.get_KeyCode() == 9)
			{
				e.set_IsInputKey(true);
			}
		}

		private void txtFree_Enter(object sender, EventArgs e)
		{
			((Control)txtFree).set_Text(((Control)txtFree).get_Text().TrimEnd(Array.Empty<char>()));
			((Control)lblFreeLeft).set_Text(((Control)txtFree).get_Text().Length.ToString());
			((TextBoxBase)txtFree).set_Multiline(true);
			((Control)txtFree).set_Height(180);
			((Control)txtFree).set_Top(20);
			((Control)lblFreeLeft).set_Top(7);
			((Control)txtFree).set_Font(new Font(new FontFamily("Consolas"), 8.25f, FontStyle.Regular));
			txtFree.set_ScrollBars((ScrollBars)2);
			((Control)lstObservations).set_Enabled(false);
			((Control)cmdCloseExpandedFreeText).set_Visible(true);
		}

		private void bySequenceNumberToolStripMenuItem_Click(object sender, EventArgs e)
		{
			int selectedIndex = ((ListControl)lstObservations).get_SelectedIndex();
			EventDetails.SortObservers(0);
			PopulateListOfObservers(selectedIndex);
			SetEditDate();
		}

		private void byNameFieldToolStripMenuItem_Click(object sender, EventArgs e)
		{
			int selectedIndex = ((ListControl)lstObservations).get_SelectedIndex();
			EventDetails.SortObservers(1);
			PopulateListOfObservers(selectedIndex);
			SetEditDate();
		}

		private void byNameFieldskip2ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			int selectedIndex = ((ListControl)lstObservations).get_SelectedIndex();
			EventDetails.SortObservers(5);
			PopulateListOfObservers(selectedIndex);
			SetEditDate();
		}

		private void byLongitudeToolStripMenuItem_Click(object sender, EventArgs e)
		{
			int selectedIndex = ((ListControl)lstObservations).get_SelectedIndex();
			EventDetails.SortObservers(2);
			PopulateListOfObservers(selectedIndex);
			SetEditDate();
		}

		private void byLatitudeToolStripMenuItem_Click(object sender, EventArgs e)
		{
			int selectedIndex = ((ListControl)lstObservations).get_SelectedIndex();
			EventDetails.SortObservers(3);
			PopulateListOfObservers(selectedIndex);
			SetEditDate();
		}

		private void byPathDistanceToolStripMenuItem_Click(object sender, EventArgs e)
		{
			int selectedIndex = ((ListControl)lstObservations).get_SelectedIndex();
			Data_and_Plots.Observations_InitialisePlot_Using_EventDetails();
			EventDetails.SortObservers(4);
			PopulateListOfObservers(selectedIndex);
			SetEditDate();
		}

		private void sortByPathDistanceRenumberToolStripMenuItem_Click(object sender, EventArgs e)
		{
			int selectedIndex = ((ListControl)lstObservations).get_SelectedIndex();
			Data_and_Plots.Observations_InitialisePlot_Using_EventDetails();
			EventDetails.SortObservers(4);
			PopulateListOfObservers(selectedIndex);
			Renumber();
			SetEditDate();
		}

		private void cmdUp_Click(object sender, EventArgs e)
		{
			Data_and_Plots.CloseClosestMissTimes();
			if (((ListControl)lstObservations).get_SelectedIndex() > 0)
			{
				int selectedIndex = ((ListControl)lstObservations).get_SelectedIndex();
				int seqNumber = EventDetails.Observers[selectedIndex].SeqNumber;
				int seqNumber2 = EventDetails.Observers[selectedIndex - 1].SeqNumber;
				EventDetails.Observers[selectedIndex - 1].SeqNumber = seqNumber;
				EventDetails.Observers[selectedIndex].SeqNumber = seqNumber2;
				EventDetails.SortObservers(0);
				PopulateListOfObservers(selectedIndex - 1);
				SetEditDate();
			}
			if (((Control)lblLastSaveTime).get_ForeColor() != Color.Red)
			{
				StartTime = DateTime.Now;
			}
			((Control)lblLastSaveTime).set_ForeColor(Color.Red);
		}

		private void cmdDown_Click(object sender, EventArgs e)
		{
			Data_and_Plots.CloseClosestMissTimes();
			if (((ListControl)lstObservations).get_SelectedIndex() < lstObservations.get_Items().get_Count() - 1)
			{
				int selectedIndex = ((ListControl)lstObservations).get_SelectedIndex();
				int seqNumber = EventDetails.Observers[selectedIndex].SeqNumber;
				int seqNumber2 = EventDetails.Observers[selectedIndex + 1].SeqNumber;
				EventDetails.Observers[selectedIndex + 1].SeqNumber = seqNumber;
				EventDetails.Observers[selectedIndex].SeqNumber = seqNumber2;
				EventDetails.SortObservers(0);
				PopulateListOfObservers(selectedIndex + 1);
				SetEditDate();
			}
			if (((Control)lblLastSaveTime).get_ForeColor() != Color.Red)
			{
				StartTime = DateTime.Now;
			}
			((Control)lblLastSaveTime).set_ForeColor(Color.Red);
		}

		private void renumberObservationsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			cmdRenumber_Click(sender, e);
		}

		private void cmdRenumber_Click(object sender, EventArgs e)
		{
			Renumber();
		}

		private void Renumber()
		{
			Data_and_Plots.CloseClosestMissTimes();
			int selectedIndex = ((ListControl)lstObservations).get_SelectedIndex();
			EventDetails.SequentiallyNumberObservations();
			PopulateListOfObservers(selectedIndex);
			SetEditDate();
			if (((Control)lblLastSaveTime).get_ForeColor() != Color.Red)
			{
				StartTime = DateTime.Now;
			}
			((Control)lblLastSaveTime).set_ForeColor(Color.Red);
		}

		internal void SetEditDate()
		{
			EventDetails.YearEdited = DateTime.Today.ToUniversalTime().Year;
			EventDetails.MonthEdited = DateTime.Today.ToUniversalTime().Month;
			EventDetails.DayEdited = DateTime.Today.ToUniversalTime().Day;
		}

		private void cmdAdd_Click(object sender, EventArgs e)
		{
			//IL_0097: Unknown result type (might be due to invalid IL or missing references)
			//IL_009d: Invalid comparison between Unknown and I4
			if (chkPredicted.get_Checked() && ((((Control)txtObserver1).get_Text() != "Predicted") | (((Control)txtObserver2).get_Text().Trim() != "") | (((Control)txtLocatedNear).get_Text().Trim() != "") | (((Control)txtStateCountry).get_Text().Trim() != "") | chkEtAl.get_Checked()))
			{
				if ((int)MessageBox.Show("The Observer identification fields are incompatible with a 'Prediction' entry.\r\n\r\nDo you want those fields to be corrected, with the event then being added?", "Incompatible observer data", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) != 6)
				{
					return;
				}
				((Control)txtObserver1).set_Text("Predicted");
				((Control)txtObserver2).set_Text("");
				((Control)txtLocatedNear).set_Text("");
				((Control)txtStateCountry).set_Text("");
				chkEtAl.set_Checked(false);
			}
			int num = EventDetails.AddNewObservationLine();
			if (((Control)lblLastSaveTime).get_ForeColor() != Color.Red)
			{
				StartTime = DateTime.Now;
			}
			((Control)lblLastSaveTime).set_ForeColor(Color.Red);
			Observation_EncodeFromEditor(num);
			PopulateListOfObservers(num);
			GetShapeModels();
			((Control)cmdAdd).set_Text("ADD  as new #" + $"{EventDetails.Observers.Count + 1:F0}");
			SetEditDate();
		}

		private void cmdReplace_Click(object sender, EventArgs e)
		{
			//IL_00a8: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ae: Invalid comparison between Unknown and I4
			//IL_0196: Unknown result type (might be due to invalid IL or missing references)
			//IL_019c: Invalid comparison between Unknown and I4
			//IL_02f2: Unknown result type (might be due to invalid IL or missing references)
			//IL_02f8: Invalid comparison between Unknown and I4
			int selectedIndex = ((ListControl)lstObservations).get_SelectedIndex();
			if (selectedIndex < 0)
			{
				return;
			}
			if (chkPredicted.get_Checked() && ((((Control)txtObserver1).get_Text() != "Predicted") | (((Control)txtObserver2).get_Text().Trim() != "") | (((Control)txtLocatedNear).get_Text().Trim() != "") | (((Control)txtStateCountry).get_Text().Trim() != "") | chkEtAl.get_Checked()))
			{
				if ((int)MessageBox.Show("The Observer identification fields are incompatible with a 'Prediction' entry.\r\n\r\nDo you want those fields to be corrected, with the event then being replaced?", "Incompatible observer data", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) != 6)
				{
					return;
				}
				((Control)txtObserver1).set_Text("Predicted");
				((Control)txtObserver2).set_Text("");
				((Control)txtLocatedNear).set_Text("");
				((Control)txtStateCountry).set_Text("");
				chkEtAl.set_Checked(false);
			}
			if (chkUnseen.get_Checked() && ((((Control)txtObserver1).get_Text() != "Unseen_Primary_Event") | (((Control)txtObserver2).get_Text().Trim() != "") | (((Control)txtLocatedNear).get_Text().Trim() != "") | (((Control)txtStateCountry).get_Text().Trim() != "") | chkEtAl.get_Checked()))
			{
				if ((int)MessageBox.Show("The event involves a binary asteroid where an occultation by the primary body was not observed.\r\n\r\nThis Observer entry is a 'predicted' entry, used as a reference point for the PA and Separation of the observed satellite\r\n\r\nThe Observer identification fields are incompatible this this type of entry.\r\n\r\nDo you want those fields to be corrected, with the event then being replaced?", "Incompatible observer data", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) != 6)
				{
					return;
				}
				((Control)txtObserver1).set_Text("Unseen_Primary_Event");
				((Control)txtObserver2).set_Text("");
				((Control)txtLocatedNear).set_Text("");
				((Control)txtStateCountry).set_Text("");
				chkEtAl.set_Checked(false);
			}
			string text = "Are you sure you want to replace record # " + EventDetails.Observers[selectedIndex].SeqNumber + "\r\n\r\nfor : " + EventDetails.Observers[selectedIndex].Observer1;
			if (EventDetails.Observers[selectedIndex].Observer2.Length > 0)
			{
				text = text + " and " + EventDetails.Observers[selectedIndex].Observer2;
			}
			text = ((EventDetails.Observers[selectedIndex].NearTo.Trim().Length <= 0) ? (text + "\r\n\r\nlocated in: " + EventDetails.Observers[selectedIndex].StateCountry) : (text + "\r\n\r\nlocated near: " + EventDetails.Observers[selectedIndex].NearTo.Trim() + ", in " + EventDetails.Observers[selectedIndex].StateCountry));
			if ((int)MessageBox.Show(text, "Check change", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				Observation_EncodeFromEditor(selectedIndex);
				PopulateListOfObservers(selectedIndex);
				GetShapeModels();
				SetEditDate();
				if (((Control)lblLastSaveTime).get_ForeColor() != Color.Red)
				{
					StartTime = DateTime.Now;
				}
				((Control)lblLastSaveTime).set_ForeColor(Color.Red);
			}
		}

		private void cmdDelete_Click(object sender, EventArgs e)
		{
			if (lstObservations.get_Items().get_Count() < 1)
			{
				return;
			}
			int selectedIndex = ((ListControl)lstObservations).get_SelectedIndex();
			EventDetails.DeleteObservationLine(selectedIndex);
			if (lstObservations.get_Items().get_Count() > 0)
			{
				if (selectedIndex < lstObservations.get_Items().get_Count() - 1)
				{
					PopulateListOfObservers(selectedIndex);
				}
				else
				{
					PopulateListOfObservers(lstObservations.get_Items().get_Count() - 1);
				}
			}
			Renumber();
			GetShapeModels();
		}

		private void Observation_EncodeFromEditor(int Index)
		{
			//IL_0fba: Unknown result type (might be due to invalid IL or missing references)
			bool flag = false;
			bool flag2 = false;
			EventDetails.Observers[Index].Observer1 = ((Control)txtObserver1).get_Text().Trim();
			EventDetails.Observers[Index].Observer2 = ((Control)txtObserver2).get_Text().Trim();
			EventDetails.Observers[Index].MoreThan2Observers = chkEtAl.get_Checked();
			EventDetails.Observers[Index].ObserversAll_Create();
			EventDetails.Observers[Index].NearTo = ((Control)txtLocatedNear).get_Text().Trim();
			EventDetails.Observers[Index].StateCountry = ((Control)txtStateCountry).get_Text().Trim();
			EventDetails.Observers[Index].TelescopeAperture_Set = int.TryParse(((Control)txtAperture).get_Text(), out var result);
			if (result > 2500)
			{
				result = 2500;
			}
			EventDetails.Observers[Index].TelescopeAperture = result;
			if (((ListControl)cmbTelescope).get_SelectedIndex() < 0)
			{
				((ListControl)cmbTelescope).set_SelectedIndex(0);
			}
			EventDetails.Observers[Index].TelescopeType = " 123456789".Substring(((ListControl)cmbTelescope).get_SelectedIndex(), 1);
			EventDetails.Observers[Index].Longitude_PlusSign = !((Control)txtLongDeg).get_Text().Contains("-");
			if (!int.TryParse(((Control)txtLongDeg).get_Text().Replace("-", ""), out result))
			{
				result = 0;
			}
			EventDetails.Observers[Index].Longitude_Deg = result;
			if (!int.TryParse(((Control)txtLongMin).get_Text(), out result))
			{
				result = 0;
			}
			EventDetails.Observers[Index].Longitude_Min = result;
			if (!double.TryParse(((Control)txtLongSec).get_Text(), out var result2))
			{
				result2 = 0.0;
			}
			EventDetails.Observers[Index].Longitude_Sec = result2;
			EventDetails.Observers[Index].Longitude_Sec_N = Utilities.DecimalPlaces(((Control)txtLongSec).get_Text());
			if (EventDetails.Observers[Index].Longitude_Sec_N > 1)
			{
				EventDetails.Observers[Index].Longitude_Sec_N = 1;
			}
			EventDetails.Observers[Index].Latitude_PlusSign = !((Control)txtLatDeg).get_Text().Contains("-");
			if (!int.TryParse(((Control)txtLatDeg).get_Text().Replace("-", ""), out result))
			{
				result = 0;
			}
			EventDetails.Observers[Index].Latitude_Deg = result;
			if (!int.TryParse(((Control)txtLatMin).get_Text(), out result))
			{
				result = 0;
			}
			EventDetails.Observers[Index].Latitude_Min = result;
			if (!double.TryParse(((Control)txtLatSec).get_Text(), out result2))
			{
				result2 = 0.0;
			}
			EventDetails.Observers[Index].Latitude_Sec = result2;
			EventDetails.Observers[Index].Latitude_Sec_N = Utilities.DecimalPlaces(((Control)txtLatSec).get_Text());
			if (EventDetails.Observers[Index].Latitude_Sec_N > 1)
			{
				EventDetails.Observers[Index].Latitude_Sec_N = 1;
			}
			if (((ListControl)cmbDatum).get_SelectedIndex() < 0)
			{
				((ListControl)cmbDatum).set_SelectedIndex(0);
			}
			EventDetails.Observers[Index].Datum = " NETG*".Substring(((ListControl)cmbDatum).get_SelectedIndex(), 1);
			EventDetails.Observers[Index].Altitude_Set = int.TryParse(((Control)txtAlt_m).get_Text(), out result);
			EventDetails.Observers[Index].Altitude = result;
			if (!int.TryParse(((Control)txtD_Hr).get_Text(), out result))
			{
				result = 0;
			}
			EventDetails.Observers[Index].H_Dis = result;
			if (!int.TryParse(((Control)txtD_Min).get_Text(), out result))
			{
				result = 0;
			}
			EventDetails.Observers[Index].M_Dis = result;
			if (!double.TryParse(((Control)txtD_Sec).get_Text(), out result2))
			{
				result2 = 0.0;
			}
			EventDetails.Observers[Index].S_Dis = result2;
			EventDetails.Observers[Index].S_Dis_DecPlaces = Utilities.DecimalPlaces(((Control)txtD_Sec).get_Text());
			EventDetails.Observers[Index].Accuracy_D_Set = double.TryParse(((Control)txtD_Uncert).get_Text(), out result2);
			if (!EventDetails.Observers[Index].Accuracy_D_Set)
			{
				if (EventDetails.Observers[Index].S_Dis_DecPlaces > 1)
				{
					EventDetails.Observers[Index].S_Dis_DecPlaces = 1;
				}
			}
			else
			{
				EventDetails.Observers[Index].Accuracy_D = result2;
				EventDetails.Observers[Index].Accuracy_D_DecPlaces = Utilities.DecimalPlaces(((Control)txtD_Uncert).get_Text());
				int num = Utilities.SignificantDigitLocation_DecimalNumbers(EventDetails.Observers[Index].Accuracy_D);
				if (num > EventDetails.Observers[Index].Accuracy_D_DecPlaces)
				{
					num = EventDetails.Observers[Index].Accuracy_D_DecPlaces;
				}
				if (num < EventDetails.Observers[Index].Accuracy_D_DecPlaces)
				{
					EventDetails.Observers[Index].Accuracy_D_DecPlaces = num;
					flag = true;
				}
				if (num < EventDetails.Observers[Index].S_Dis_DecPlaces)
				{
					EventDetails.Observers[Index].S_Dis_DecPlaces = num;
					flag = true;
				}
				else if (num > EventDetails.Observers[Index].S_Dis_DecPlaces)
				{
					EventDetails.Observers[Index].S_Dis_DecPlaces = num;
					flag2 = true;
				}
				if (flag && double.Parse(string.Format("{0:F" + EventDetails.Observers[Index].S_Dis_DecPlaces + "}", EventDetails.Observers[Index].S_Dis)) >= 60.0)
				{
					EventDetails.Observers[Index].S_Dis = 0.0;
					EventDetails.Observers[Index].M_Dis++;
					if (EventDetails.Observers[Index].M_Dis > 59)
					{
						EventDetails.Observers[Index].M_Dis = 0;
						EventDetails.Observers[Index].H_Dis++;
					}
				}
			}
			EventDetails.Observers[Index].PEQ_D_Set = double.TryParse(((Control)txtD_PEq).get_Text(), out result2);
			if (!EventDetails.Observers[Index].PEQ_D_Set)
			{
				result2 = 0.0;
			}
			else
			{
				EventDetails.Observers[Index].PEQ_D = result2;
				EventDetails.Observers[Index].PEQ_D_N = Utilities.DecimalPlaces(((Control)txtD_PEq).get_Text());
				if (EventDetails.Observers[Index].PEQ_D_N > 2)
				{
					EventDetails.Observers[Index].PEQ_D_N = 2;
				}
			}
			if (((ListControl)cmbD_Wt).get_SelectedIndex() < 0)
			{
				((ListControl)cmbD_Wt).set_SelectedIndex(0);
			}
			if (((ListControl)cmbD_Wt).get_SelectedIndex() == 0)
			{
				EventDetails.Observers[Index].Weight_D_Set = false;
				EventDetails.Observers[Index].Weight_D = 0;
			}
			else
			{
				EventDetails.Observers[Index].Weight_D_Set = true;
				EventDetails.Observers[Index].Weight_D = ((ListControl)cmbD_Wt).get_SelectedIndex() - 1;
			}
			if (!int.TryParse(((Control)txtR_Hr).get_Text(), out result))
			{
				result = 0;
			}
			EventDetails.Observers[Index].H_Reap = result;
			if (!int.TryParse(((Control)txtR_Min).get_Text(), out result))
			{
				result = 0;
			}
			EventDetails.Observers[Index].M_Reap = result;
			if (!double.TryParse(((Control)txtR_Sec).get_Text(), out result2))
			{
				result2 = 0.0;
			}
			EventDetails.Observers[Index].S_Reap = result2;
			EventDetails.Observers[Index].S_Reap_DecPlaces = Utilities.DecimalPlaces(((Control)txtR_Sec).get_Text());
			if (EventDetails.Observers[Index].S_Reap_DecPlaces > 3)
			{
				EventDetails.Observers[Index].S_Reap_DecPlaces = 3;
			}
			EventDetails.Observers[Index].Accuracy_R_Set = double.TryParse(((Control)txtR_Uncert).get_Text(), out result2);
			if (!EventDetails.Observers[Index].Accuracy_R_Set)
			{
				if (EventDetails.Observers[Index].S_Reap_DecPlaces > 1)
				{
					EventDetails.Observers[Index].S_Reap_DecPlaces = 1;
				}
			}
			else
			{
				EventDetails.Observers[Index].Accuracy_R = result2;
				EventDetails.Observers[Index].Accuracy_R_DecPlaces = Utilities.DecimalPlaces(((Control)txtR_Uncert).get_Text());
				int num2 = Utilities.SignificantDigitLocation_DecimalNumbers(EventDetails.Observers[Index].Accuracy_R);
				if (num2 > EventDetails.Observers[Index].Accuracy_R_DecPlaces)
				{
					num2 = EventDetails.Observers[Index].Accuracy_R_DecPlaces;
				}
				if (num2 < EventDetails.Observers[Index].Accuracy_R_DecPlaces)
				{
					EventDetails.Observers[Index].Accuracy_R_DecPlaces = num2;
					flag2 = true;
				}
				if (num2 < EventDetails.Observers[Index].S_Reap_DecPlaces)
				{
					EventDetails.Observers[Index].S_Reap_DecPlaces = num2;
					flag2 = true;
				}
				else if (num2 > EventDetails.Observers[Index].S_Reap_DecPlaces)
				{
					EventDetails.Observers[Index].S_Reap_DecPlaces = num2;
					flag2 = true;
				}
				if (flag2 && double.Parse(string.Format("{0:F" + EventDetails.Observers[Index].S_Reap_DecPlaces + "}", EventDetails.Observers[Index].S_Reap)) >= 60.0)
				{
					EventDetails.Observers[Index].S_Reap = 0.0;
					EventDetails.Observers[Index].M_Reap++;
					if (EventDetails.Observers[Index].M_Reap > 59)
					{
						EventDetails.Observers[Index].M_Reap = 0;
						EventDetails.Observers[Index].H_Reap++;
					}
				}
			}
			EventDetails.Observers[Index].PEQ_R_Set = double.TryParse(((Control)txtR_PEq).get_Text(), out result2);
			if (!EventDetails.Observers[Index].PEQ_R_Set)
			{
				result2 = 0.0;
			}
			else
			{
				EventDetails.Observers[Index].PEQ_R = result2;
				EventDetails.Observers[Index].PEQ_R_N = Utilities.DecimalPlaces(((Control)txtR_PEq).get_Text());
				if (EventDetails.Observers[Index].PEQ_R_N > 2)
				{
					EventDetails.Observers[Index].PEQ_R_N = 2;
				}
			}
			if (((ListControl)cmbR_Wt).get_SelectedIndex() < 0)
			{
				((ListControl)cmbR_Wt).set_SelectedIndex(0);
			}
			if (((ListControl)cmbR_Wt).get_SelectedIndex() == 0)
			{
				EventDetails.Observers[Index].Weight_R_Set = false;
				EventDetails.Observers[Index].Weight_R = 0;
			}
			else
			{
				EventDetails.Observers[Index].Weight_R_Set = true;
				EventDetails.Observers[Index].Weight_R = ((ListControl)cmbR_Wt).get_SelectedIndex() - 1;
			}
			if (chkMiss.get_Checked())
			{
				if (chkDoubleStar.get_Checked())
				{
					string text3 = (EventDetails.Observers[Index].Event_D = (EventDetails.Observers[Index].Event_R = "m"));
				}
				else if (!chkRing.get_Checked())
				{
					string text3 = (EventDetails.Observers[Index].Event_D = (EventDetails.Observers[Index].Event_R = "M"));
				}
				else
				{
					string text3 = (EventDetails.Observers[Index].Event_D = (EventDetails.Observers[Index].Event_R = "n"));
				}
			}
			else if (chkPredicted.get_Checked())
			{
				string text3 = (EventDetails.Observers[Index].Event_D = (EventDetails.Observers[Index].Event_R = "P"));
			}
			else if (chkRing.get_Checked())
			{
				string text3 = (EventDetails.Observers[Index].Event_D = (EventDetails.Observers[Index].Event_R = "N"));
			}
			else if (chkNotSeen.get_Checked())
			{
				string text3 = (EventDetails.Observers[Index].Event_D = (EventDetails.Observers[Index].Event_R = "C"));
			}
			else if (chkUnseen.get_Checked())
			{
				EventDetails.Observers[Index].Event_D = "e";
				EventDetails.Observers[Index].Event_R = "f";
			}
			else if (chkDoubleStar.get_Checked())
			{
				if (chkSatellite.get_Checked())
				{
					EventDetails.Observers[Index].Event_D = "g";
					EventDetails.Observers[Index].Event_R = "b";
				}
				else
				{
					EventDetails.Observers[Index].Event_D = "d";
					EventDetails.Observers[Index].Event_R = "r";
				}
			}
			else if (chkSatellite.get_Checked())
			{
				EventDetails.Observers[Index].Event_D = "G";
				EventDetails.Observers[Index].Event_R = "B";
			}
			else
			{
				EventDetails.Observers[Index].Event_D = "D";
				EventDetails.Observers[Index].Event_R = "R";
			}
			if (((ListControl)cmbMethod).get_SelectedIndex() < 0)
			{
				((ListControl)cmbMethod).set_SelectedIndex(0);
			}
			EventDetails.Observers[Index].Method = " abcdefg1234567890".Substring(((ListControl)cmbMethod).get_SelectedIndex(), 1);
			if (((ListControl)cmbTime).get_SelectedIndex() < 0)
			{
				((ListControl)cmbTime).set_SelectedIndex(0);
			}
			EventDetails.Observers[Index].TimeSource = " abcdefg1234567890".Substring(((ListControl)cmbTime).get_SelectedIndex(), 1);
			if (((ListControl)cmbStability).get_SelectedIndex() < 0)
			{
				((ListControl)cmbStability).set_SelectedIndex(0);
			}
			EventDetails.Observers[Index].Stability = " 12345678".Substring(((ListControl)cmbStability).get_SelectedIndex(), 1);
			if (((ListControl)cmbTransparency).get_SelectedIndex() < 0)
			{
				((ListControl)cmbTransparency).set_SelectedIndex(0);
			}
			EventDetails.Observers[Index].Transparency = " 12345678".Substring(((ListControl)cmbTransparency).get_SelectedIndex(), 1);
			EventDetails.Observers[Index].SNR_Set = updnSNR.get_Value() > 0m;
			EventDetails.Observers[Index].SNR = (double)updnSNR.get_Value();
			if (((ListControl)cmbPlotControl).get_SelectedIndex() < 0)
			{
				((ListControl)cmbPlotControl).set_SelectedIndex(0);
			}
			EventDetails.Observers[Index].PlotCode = " yzx".Substring(((ListControl)cmbPlotControl).get_SelectedIndex(), 1);
			EventDetails.Observers[Index].TimeAdjustment_Set = double.TryParse(((Control)txtShift).get_Text(), out result2);
			EventDetails.Observers[Index].TimeAdjustment = result2;
			EventDetails.Observers[Index].FreeText = ((Control)txtFree).get_Text();
			if (flag || flag2)
			{
				string text14 = "EVENT TIMES - SIGNIFICANT DIGITS\r\n\r\nThe number of decimal places for the event times and uncertainties have been limited to significant digits\r\n\r\n";
				if (flag)
				{
					text14 += " D times &/or uncertainty\r\n";
				}
				if (flag2)
				{
					text14 += " R times &/or uncertainty\r\n";
				}
				MessageBox.Show(text14, "Significant digits", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
			}
		}

		private void updateHistoricalFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			UpdateHistorical(AutoUpdate: false);
		}

		private void cmdUpdateHistorical_Click(object sender, EventArgs e)
		{
			UpdateHistorical(AutoUpdate: false);
			StartTime = DateTime.Now;
			((Control)lblLastSaveTime).set_ForeColor(Color.Green);
		}

		private void UpdateHistorical(bool AutoUpdate)
		{
			//IL_001f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0025: Invalid comparison between Unknown and I4
			//IL_0050: Unknown result type (might be due to invalid IL or missing references)
			//IL_0056: Invalid comparison between Unknown and I4
			//IL_00ca: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d0: Invalid comparison between Unknown and I4
			Data_and_Plots.ComputeSolution_and_UpDate_EventDetails();
			if (!AutoUpdate && (int)MessageBox.Show("This will replace the data in the file of Historical observations\r\nwith the currently displayed data.\r\n\r\nDo you wish to continue?", "Replacing data", (MessageBoxButtons)1, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 2)
			{
				return;
			}
			if (EventDetails.ShapeData.Count > Data_and_Plots.NumberOfShapeModels())
			{
				if ((int)MessageBox.Show("The number of shape models fitted against this event exceeds the number of current valid shape models.\r\n\r\nYou need to delete the fits of old shape models using the menu item\r\nSolutions... Delete current-event shape-model solution(s)\r\n\r\nDo you want tocontinue without deleting the old shape models?", "Old hshape model fits", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
				{
					return;
				}
			}
			else if (EventDetails.ShapeData.Count != Data_and_Plots.NumberOfShapeModels() && (int)MessageBox.Show("There are " + (Data_and_Plots.NumberOfShapeModels() - EventDetails.ShapeData.Count) + " / " + Data_and_Plots.NumberOfShapeModels() + " shape models that have not been fitted to this event.\r\n\r\nDo you want to continue without fitting all shape models?", "Shape models have not been fitted", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			((Control)this).Focus();
			Data_and_Plots.Historical_AllEvents.EncodeAnEvent_inXML(NewEvent: false, CurrentHistoricalRecord);
			Data_and_Plots.Historical_AllEvents.WriteHistoricalObservationsFile(HistoricalFile: true, "");
			bool @checked = Data_and_Plots.PlotForm.addShapeImageFromDAMIT_ISAMToolStripMenuItem.get_Checked();
			DisplayIndex(ForCurrentEvent: true);
			if (@checked)
			{
				ToolStripMenuItem showShapeModelControlsOnPlotToolStripMenuItem = Data_and_Plots.PlotForm.showShapeModelControlsOnPlotToolStripMenuItem;
				bool checked2;
				((Control)Data_and_Plots.PlotForm.panelShapeModelControl).set_Visible(checked2 = true);
				showShapeModelControlsOnPlotToolStripMenuItem.set_Checked(checked2);
			}
			Data_and_Plots.PlotEventOnScreen();
			((Control)lblLastUpdate).set_Text("Added : " + EventDetails.YearAdded + " " + Utilities.ShortMonths[EventDetails.MonthAdded] + " " + EventDetails.DayAdded + "     Updated : " + EventDetails.YearEdited + " " + Utilities.ShortMonths[EventDetails.MonthEdited] + " " + EventDetails.DayEdited);
			StartTime = DateTime.Now;
			((Control)lblLastSaveTime).set_ForeColor(Color.Green);
			EventLoadedFromFile = false;
			((Control)this).Focus();
		}

		private void cmdAddToHistorical_Click(object sender, EventArgs e)
		{
			Add_Event_to_HistoricalFile();
			StartTime = DateTime.Now;
			((Control)lblLastSaveTime).set_ForeColor(Color.Green);
		}

		private void plotToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Data_and_Plots.ShowPlotForm();
			((Control)Data_and_Plots.PlotForm).Focus();
		}

		private void possibleTimebaseCorrectionsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Data_and_Plots.ShowTimeBaseForm();
			((Form)Data_and_Plots.TimeBase_Offset).set_WindowState((FormWindowState)0);
			((Control)Data_and_Plots.TimeBase_Offset).Focus();
		}

		private void fromPredictionToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Data_and_Plots.ShowPredictOffsetForm();
			((Form)Data_and_Plots.Prediction_Offset).set_WindowState((FormWindowState)0);
			((Control)Data_and_Plots.Prediction_Offset).Focus();
		}

		private void chordLengthsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Data_and_Plots.ShowChordLengths();
			((Form)Data_and_Plots.ChordLengths).set_WindowState((FormWindowState)0);
			((Control)Data_and_Plots.ChordLengths).Focus();
		}

		private void eventCoordinatesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Data_and_Plots.ShowEventCoords();
			((Form)Data_and_Plots.Event_Coords).set_WindowState((FormWindowState)0);
			((Control)Data_and_Plots.Event_Coords).Focus();
		}

		private void relativePathDistancesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Data_and_Plots.ShowPathDistances();
			((Form)Data_and_Plots.PathDistances).set_WindowState((FormWindowState)0);
			((Control)Data_and_Plots.PathDistances).Focus();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void openToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_003d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0043: Expected O, but got Unknown
			//IL_009b: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a1: Invalid comparison between Unknown and I4
			//IL_043a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0471: Unknown result type (might be due to invalid IL or missing references)
			VisibilityOfControls(HistoricalEnabled: false);
			ReadingMultipleOBSFiles = false;
			ReadingMultipleEurasterEntries = false;
			ReadingMultipleJPEntries = false;
			((ToolStripItem)skipToNextFileToolStripMenuItem).set_Visible(false);
			PreviousEventMidHour = -1.0;
			SetPasteEnabled(PasteEnabled: true);
			OpenFileDialog val = new OpenFileDialog();
			((FileDialog)val).set_Title("Specify file to READ asteroidal observations from.");
			((FileDialog)val).set_Filter("XML file (*.xml)|*.XML|OBS file (*.obs)|*.obs|Text files (*.txt)|*.txt|All files (*.*)|*.*");
			((FileDialog)val).set_FilterIndex(Settings.Default.OBS_File_LastIndex);
			val.set_Multiselect(false);
			((FileDialog)val).set_FileName(Settings.Default.OBS_File_LastName);
			try
			{
				((FileDialog)val).set_InitialDirectory(Path.GetDirectoryName(Settings.Default.OBS_File_LastName));
			}
			catch
			{
			}
			if ((int)((CommonDialog)val).ShowDialog() != 1)
			{
				return;
			}
			ReSetForm();
			VisibilityOfControls(HistoricalEnabled: false);
			Current_OBS_File = ((FileDialog)val).get_FileName();
			Settings.Default.OBS_File_LastName = Current_OBS_File;
			Settings.Default.OBS_File_LastIndex = ((FileDialog)val).get_FilterIndex();
			string text = CheckObservationFile(Current_OBS_File);
			if (text.Length > 0)
			{
				text = "File has the following errors\r\n\r\n" + text;
				try
				{
					((Control)ReportFileCheck).Show();
				}
				catch
				{
					ReportFileCheck = new DisplayData();
					((Control)ReportFileCheck).Show();
				}
				((Control)ReportFileCheck).set_Text("Check report on observation files");
				((Control)ReportFileCheck.txtBox).set_Text("");
				((Control)ReportFileCheck.txtBox).set_Text(text);
				((Control)ReportFileCheck).Show();
				((Form)ReportFileCheck).set_TopMost(true);
			}
			else
			{
				try
				{
					((Control)ReportFileCheck).Hide();
				}
				catch
				{
				}
			}
			((Control)this).set_Text("Asteroid observations editor : File = " + Path.GetFileName(Current_OBS_File));
			Data_and_Plots.SingleEvent = new AllEvents();
			Data_and_Plots.SingleEvent.ReadObservationsFile(HistoricalFile: false, Current_OBS_File);
			EventLoadedFromFile = true;
			Data_and_Plots.SingleEvent.DecodeAnEvent_Into_EventDetails(0);
			for (int i = 0; i < EventDetails.Observers.Count; i++)
			{
				if (EventDetails.Observers[i].TelescopeType == "8")
				{
					EventDetails.Observers[i].TimeSource = "c";
					if (EventDetails.Observers[i].Event_D == "M")
					{
						ObserverData observerData = EventDetails.Observers[i];
						bool accuracy_D_Set = (EventDetails.Observers[i].Accuracy_R_Set = false);
						observerData.Accuracy_D_Set = accuracy_D_Set;
						double num3 = (EventDetails.Observers[i].Accuracy_D = (EventDetails.Observers[i].Accuracy_R = 0.0));
						EventDetails.Observers[i].Event_R = EventDetails.Observers[i].Event_D;
						EventDetails.Observers[i].H_Reap = EventDetails.Observers[i].H_Dis;
						EventDetails.Observers[i].M_Reap = EventDetails.Observers[i].M_Dis;
						EventDetails.Observers[i].S_Reap = EventDetails.Observers[i].S_Dis;
						int num6 = (EventDetails.Observers[i].S_Dis_DecPlaces = (EventDetails.Observers[i].S_Reap_DecPlaces = 0));
					}
				}
			}
			StartTime = DateTime.Now;
			((Control)lblLastSaveTime).set_ForeColor(Color.Blue);
			DisplayEventInEditor();
			if ((EventDetails.X_Dia == 0.0) | (EventDetails.Y_Dia == 0.0))
			{
				Data_and_Plots.PlotForm.chkUseAssumedDiameter.set_Checked(true);
			}
			GetStarPosition(ForStarDiameter: false, UpdateStarOnly: false);
			GetAsteroidPosition();
			try
			{
				((Control)Data_and_Plots.PlotForm.panelShapeModelControl).set_Visible(false);
			}
			catch
			{
			}
			((Control)cmdAltitude).set_ForeColor(Color.Red);
			if (Settings.Default.GE_Alts_Auto)
			{
				Validate_SiteAltitudes_MidTime("");
			}
			if (Settings.Default.Asteroid_Open_Sort)
			{
				EventDetails.SortObservers(4);
				Renumber();
				PopulateListOfObservers(0);
				((Control)Data_and_Plots.PlotForm).Focus();
			}
			Data_and_Plots.ShowMissTimesForm();
			if (Data_and_Plots.ClosestMissTimes())
			{
				Data_and_Plots.Miss_Times.CorrectTimes();
			}
			string StarId_Of_Found;
			if (CheckIfInHistoricalFile(out var ObserverNames))
			{
				MessageBox.Show("An entry for this Asteroid, Star and Date already exists,\r\nwith Observers:\r\n" + ObserverNames, "Possible duplicate entry", (MessageBoxButtons)0, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
			}
			else if (GetPreviousOccultsOfStar(Data_and_Plots.StarRA, Data_and_Plots.StarDec, DisplayOnlyIfFound: true, out StarId_Of_Found))
			{
				MessageBox.Show("A previous occultation of this star exists, with catalog number " + StarId_Of_Found, "Previous occultation", (MessageBoxButtons)0, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
			}
			Data_and_Plots.FirstTimePlot = true;
		}

		private void openMultipleFilesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0015: Unknown result type (might be due to invalid IL or missing references)
			//IL_001b: Expected O, but got Unknown
			//IL_0073: Unknown result type (might be due to invalid IL or missing references)
			//IL_0079: Invalid comparison between Unknown and I4
			VisibilityOfControls(HistoricalEnabled: false);
			IsEuraster = false;
			IsJP = false;
			OpenFileDialog val = new OpenFileDialog();
			((FileDialog)val).set_Title("Specify files to READ asteroidal observations from.");
			((FileDialog)val).set_Filter("XML file (*.xml)|*.XML|OBS file (*.obs)|*.obs|Text files (*.txt)|*.txt|All files (*.*)|*.*");
			((FileDialog)val).set_FilterIndex(Settings.Default.OBS_File_LastIndex);
			val.set_Multiselect(true);
			((FileDialog)val).set_FileName(Settings.Default.OBS_File_LastName);
			try
			{
				((FileDialog)val).set_InitialDirectory(Path.GetDirectoryName(Settings.Default.OBS_File_LastName));
			}
			catch
			{
			}
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				string text = "Files with errors\r\n\r\n";
				int num = 0;
				try
				{
					((Control)ReportFileCheck).Show();
				}
				catch
				{
					ReportFileCheck = new DisplayData();
					((Control)ReportFileCheck).Show();
				}
				((Control)ReportFileCheck).set_Text("Check report on observation files");
				((Control)ReportFileCheck.txtBox).set_Text("");
				MultipleOBSFiles = ((FileDialog)val).get_FileNames();
				for (int i = 0; i < MultipleOBSFiles.Length; i++)
				{
					string text2 = CheckObservationFile(MultipleOBSFiles[i]);
					if (text2.Length > 0)
					{
						text = text + text2 + "\r\n";
						num++;
					}
				}
				text = text + num + " files with errors";
				((Control)ReportFileCheck.txtBox).set_Text(text);
				((Control)ReportFileCheck).Show();
				((Form)ReportFileCheck).set_TopMost(true);
				MultipleOBSFiles = ((FileDialog)val).get_FileNames();
				ReadingMultipleOBSFiles = true;
				ReadingMultipleEurasterEntries = false;
				ReadingMultipleJPEntries = false;
				((ToolStripItem)skipToNextFileToolStripMenuItem).set_Visible(true);
				((ToolStripItem)skipToNextFileToolStripMenuItem).set_Text("Skip to next file");
				SetPasteEnabled(PasteEnabled: false);
				Current_MultipleOBSFiles = -1;
				ReadNextMultipleFile();
			}
			((Form)ReportFileCheck).set_TopMost(false);
		}

		private void ReadNextMultipleFile()
		{
			//IL_0072: Unknown result type (might be due to invalid IL or missing references)
			//IL_0749: Unknown result type (might be due to invalid IL or missing references)
			//IL_07f3: Unknown result type (might be due to invalid IL or missing references)
			IsEuraster = false;
			IsJP = false;
			PreviousEventMidHour = -1.0;
			Current_MultipleOBSFiles++;
			if (Current_MultipleOBSFiles >= MultipleOBSFiles.Length)
			{
				ReadingMultipleOBSFiles = false;
				((ToolStripItem)skipToNextFileToolStripMenuItem).set_Visible(false);
				SetPasteEnabled(PasteEnabled: true);
				((Control)this).set_Text("Asteroid observations editor : Multi-file open - All files completed");
				MessageBox.Show("Multi-file open: All files completed", "MultiFile complete", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return;
			}
			if (Current_MultipleOBSFiles >= MultipleOBSFiles.Length - 1)
			{
				((ToolStripItem)skipToNextFileToolStripMenuItem).set_Visible(false);
			}
			else
			{
				((ToolStripItem)skipToNextFileToolStripMenuItem).set_Text($"Skip to next file ({Current_MultipleOBSFiles + 2:f0}/{MultipleOBSFiles.Length:f0})");
			}
			ReSetForm();
			VisibilityOfControls(HistoricalEnabled: false);
			Current_OBS_File = MultipleOBSFiles[Current_MultipleOBSFiles];
			Settings.Default.OBS_File_LastName = Current_OBS_File;
			((Control)this).set_Text("Asteroid observations editor : " + Path.GetFileName(Current_OBS_File) + $" :  (file #{Current_MultipleOBSFiles + 1:f0} of {MultipleOBSFiles.Length:f0})");
			Data_and_Plots.SingleEvent = new AllEvents();
			Data_and_Plots.SingleEvent.ReadObservationsFile(HistoricalFile: false, Current_OBS_File);
			StartTime = DateTime.Now;
			((Control)lblLastSaveTime).set_ForeColor(Color.Blue);
			EventLoadedFromFile = true;
			Data_and_Plots.SingleEvent.DecodeAnEvent_Into_EventDetails(0);
			int num6;
			for (int i = 0; i < EventDetails.Observers.Count; i++)
			{
				if (EventDetails.Observers[i].TelescopeType == "8")
				{
					EventDetails.Observers[i].TimeSource = "c";
					if (EventDetails.Observers[i].Event_D == "M")
					{
						ObserverData observerData = EventDetails.Observers[i];
						bool accuracy_D_Set = (EventDetails.Observers[i].Accuracy_R_Set = false);
						observerData.Accuracy_D_Set = accuracy_D_Set;
						double num3 = (EventDetails.Observers[i].Accuracy_D = (EventDetails.Observers[i].Accuracy_R = 0.0));
						EventDetails.Observers[i].Event_R = EventDetails.Observers[i].Event_D;
						EventDetails.Observers[i].H_Reap = EventDetails.Observers[i].H_Dis;
						EventDetails.Observers[i].M_Reap = EventDetails.Observers[i].M_Dis;
						EventDetails.Observers[i].S_Reap = EventDetails.Observers[i].S_Dis;
						num6 = (EventDetails.Observers[i].S_Dis_DecPlaces = (EventDetails.Observers[i].S_Reap_DecPlaces = 0));
					}
				}
			}
			Data_and_Plots.DecimalPlacesInSolution = AllEvents.DecimalPlaces_Int_InSolution_Output(EventDetails.Y_Dia);
			NumericUpDown updnX = Data_and_Plots.PlotForm.updnX;
			NumericUpDown updnY = Data_and_Plots.PlotForm.updnY;
			NumericUpDown updnA = Data_and_Plots.PlotForm.updnA;
			int decimalPlacesInSolution;
			Data_and_Plots.PlotForm.updnB.set_DecimalPlaces(decimalPlacesInSolution = Data_and_Plots.DecimalPlacesInSolution);
			int num7;
			updnA.set_DecimalPlaces(num7 = decimalPlacesInSolution);
			updnY.set_DecimalPlaces(num6 = num7);
			updnX.set_DecimalPlaces(num6);
			Data_and_Plots.PlotForm.chkUseAssumedDiameter.set_Checked(EventDetails.UsedAssumedDiameter);
			Data_and_Plots.PlotForm.chkCircle.set_Checked(EventDetails.Solve_Circular | Data_and_Plots.PlotForm.chkUseAssumedDiameter.get_Checked());
			Data_and_Plots.PlotForm.updnX.set_Value((decimal)EventDetails.X);
			Data_and_Plots.PlotForm.updnY.set_Value((decimal)EventDetails.Y);
			Data_and_Plots.PlotForm.updnCenterOfMass_X.set_Value((decimal)EventDetails.CentreOfMass_Offset_X);
			Data_and_Plots.PlotForm.updnCenterOfMass_Y.set_Value((decimal)EventDetails.CentreOfMass_Offset_Y);
			Data_and_Plots.PlotForm.updnA.set_Value((decimal)EventDetails.X_Dia);
			Data_and_Plots.PlotForm.updnB.set_Value((decimal)EventDetails.Y_Dia);
			Data_and_Plots.PlotForm.updnPA.set_Value((decimal)EventDetails.PA_Ellipse);
			Data_and_Plots.PlotForm.chkShapeModelCentered.set_Checked(EventDetails.AstrometryShapeModelCentered);
			((Control)Data_and_Plots.PlotForm.chkShapeModelCentered).set_Enabled(Data_and_Plots.PlotForm.chkShapeModelCentered.get_Checked());
			try
			{
				for (int j = 0; j < 4; j++)
				{
					Data_and_Plots.PlotForm.updn_DoubleSep[j].set_Value((decimal)Math.Abs(EventDetails.Doubles[j].Sep_Companion));
					Data_and_Plots.PlotForm.updn_DoublePA[j].set_Value((decimal)EventDetails.Doubles[j].PA_Companion);
					if (Data_and_Plots.PlotForm.updn_DoublePA[j].get_Value() < 0m)
					{
						NumericUpDown obj = Data_and_Plots.PlotForm.updn_DoublePA[j];
						obj.set_Value(obj.get_Value() + 360m);
					}
				}
			}
			catch
			{
			}
			if (EventDetails.AsteroidHasSatellite)
			{
				Data_and_Plots.PlotForm.SetSatelliteControls();
			}
			Data_and_Plots.PlotForm.chkA.set_Checked(EventDetails.Solve_Major);
			Data_and_Plots.PlotForm.chkB.set_Checked(EventDetails.Solve_Minor);
			Data_and_Plots.PlotForm.chkPA.set_Checked(EventDetails.Solve_PA);
			if (Data_and_Plots.PlotForm.chkUseAssumedDiameter.get_Checked())
			{
				NumericUpDown updnA2 = Data_and_Plots.PlotForm.updnA;
				decimal value;
				Data_and_Plots.PlotForm.updnB.set_Value(value = (decimal)Data_and_Plots.GetAssumedDiameter());
				updnA2.set_Value(value);
				CheckBox chkA = Data_and_Plots.PlotForm.chkA;
				CheckBox chkB = Data_and_Plots.PlotForm.chkB;
				bool flag2;
				((Control)Data_and_Plots.PlotForm.chkPA).set_Enabled(flag2 = false);
				bool accuracy_D_Set;
				((Control)chkB).set_Enabled(accuracy_D_Set = flag2);
				((Control)chkA).set_Enabled(accuracy_D_Set);
				NumericUpDown updnA3 = Data_and_Plots.PlotForm.updnA;
				NumericUpDown updnB = Data_and_Plots.PlotForm.updnB;
				((Control)Data_and_Plots.PlotForm.updnPA).set_Enabled(flag2 = false);
				((Control)updnB).set_Enabled(accuracy_D_Set = flag2);
				((Control)updnA3).set_Enabled(accuracy_D_Set);
				Data_and_Plots.PlotForm.updnPA.set_Value(0m);
			}
			else if (Data_and_Plots.PlotForm.chkCircle.get_Checked())
			{
				CheckBox chkB2 = Data_and_Plots.PlotForm.chkB;
				bool accuracy_D_Set;
				((Control)Data_and_Plots.PlotForm.chkPA).set_Enabled(accuracy_D_Set = false);
				((Control)chkB2).set_Enabled(accuracy_D_Set);
				((Control)Data_and_Plots.PlotForm.updnA).set_Enabled(true);
				NumericUpDown updnB2 = Data_and_Plots.PlotForm.updnB;
				((Control)Data_and_Plots.PlotForm.updnPA).set_Enabled(accuracy_D_Set = false);
				((Control)updnB2).set_Enabled(accuracy_D_Set);
				Data_and_Plots.PlotForm.updnPA.set_Value(0m);
			}
			Data_and_Plots.PlotForm.chkMiss.set_Checked(EventDetails.Inc_Miss);
			((ListControl)Data_and_Plots.PlotForm.cmbQuality).set_SelectedIndex(EventDetails.Quality_To_cmbQuality);
			Data_and_Plots.PlotForm.Populate_ReviewList(EventDetails.FlagForReview);
			Data_and_Plots.PlotForm.CollapseReviewList();
			DisplayEventInEditor();
			if ((EventDetails.X_Dia == 0.0) | (EventDetails.Y_Dia == 0.0))
			{
				Data_and_Plots.PlotForm.chkUseAssumedDiameter.set_Checked(true);
			}
			GetStarPosition(ForStarDiameter: false, UpdateStarOnly: false);
			GetAsteroidPosition();
			if (GetPreviousOccultsOfStar(Data_and_Plots.StarRA, Data_and_Plots.StarDec, DisplayOnlyIfFound: true, out var _))
			{
				MessageBox.Show("A previous occultation of this star exists", "Previous occultation", (MessageBoxButtons)0, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
			}
			Data_and_Plots.FirstTimePlot = true;
			Data_and_Plots.ShowPlotForm();
			Application.DoEvents();
			((Control)Data_and_Plots.PlotForm).Focus();
			if (Settings.Default.Asteroid_Open_Sort)
			{
				EventDetails.SortObservers(4);
				Renumber();
				PopulateListOfObservers(0);
				((Control)Data_and_Plots.PlotForm).Focus();
			}
			((Control)cmdAltitude).set_ForeColor(Color.Red);
			if (Settings.Default.GE_Alts_Auto)
			{
				Validate_SiteAltitudes_MidTime("");
			}
			Data_and_Plots.ShowMissTimesForm();
			if (Data_and_Plots.ClosestMissTimes())
			{
				Data_and_Plots.Miss_Times.CorrectTimes();
			}
			if (CheckIfInHistoricalFile(out var ObserverNames))
			{
				MessageBox.Show("An entry for this Asteroid, Star and Date already exists,\r\nwith Observers:\r\n" + ObserverNames, "Possible duplicate entry", (MessageBoxButtons)0, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
			}
		}

		private void newEventOBSFormatToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_00a8: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ae: Invalid comparison between Unknown and I4
			//IL_031a: Unknown result type (might be due to invalid IL or missing references)
			ReSetForm();
			PreviousEventMidHour = -1.0;
			Data_and_Plots.ShowPlotForm();
			VisibilityOfControls(HistoricalEnabled: false);
			((ToolStripItem)saveToolStripMenuItem).set_Enabled(false);
			Current_OBS_File = "";
			ReadingMultipleOBSFiles = false;
			((ToolStripItem)skipToNextFileToolStripMenuItem).set_Visible(false);
			SetPasteEnabled(PasteEnabled: true);
			string text = Clipboard.GetText((TextDataFormat)0);
			if ((text.Contains("O+ |") | text.Contains("O- |") | text.Contains("O+1|") | text.Contains("O? |") | text.Contains("Observer: ")) && (int)MessageBox.Show("Event to be pasted appears to be from Euraster, or from JOIN.\r\n\r\nDo you want to continue as an Occult XML event?", "Paste from Occult XML", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			EventLoadedFromFile = false;
			if (Data_and_Plots.Historical_AllEvents.Paste_An_Event())
			{
				Data_and_Plots.FirstTimePlot = true;
				Data_and_Plots.SingleEvent.DecodeAnEvent_Into_EventDetails(0);
				for (int i = 0; i < EventDetails.Observers.Count; i++)
				{
					if (EventDetails.Observers[i].TelescopeType == "8")
					{
						EventDetails.Observers[i].TimeSource = "c";
						if (EventDetails.Observers[i].Event_D == "M")
						{
							ObserverData observerData = EventDetails.Observers[i];
							bool accuracy_D_Set = (EventDetails.Observers[i].Accuracy_R_Set = false);
							observerData.Accuracy_D_Set = accuracy_D_Set;
							double num3 = (EventDetails.Observers[i].Accuracy_D = (EventDetails.Observers[i].Accuracy_R = 0.0));
							EventDetails.Observers[i].Event_R = EventDetails.Observers[i].Event_D;
							EventDetails.Observers[i].H_Reap = EventDetails.Observers[i].H_Dis;
							EventDetails.Observers[i].M_Reap = EventDetails.Observers[i].M_Dis;
							EventDetails.Observers[i].S_Reap = EventDetails.Observers[i].S_Dis;
							int num6 = (EventDetails.Observers[i].S_Dis_DecPlaces = (EventDetails.Observers[i].S_Reap_DecPlaces = 0));
						}
					}
				}
				StartTime = DateTime.Now;
				DisplayEventInEditor();
				if ((EventDetails.X_Dia == 0.0001) | (EventDetails.Y_Dia == 0.0001))
				{
					EventDetails.UsedAssumedDiameter = true;
					EventDetails.AsteroidNominalDiameter = (EventDetails.X_Dia = (EventDetails.Y_Dia = Data_and_Plots.GetAssumedDiameter()));
					Data_and_Plots.PlotForm.chkUseAssumedDiameter.set_Checked(true);
				}
				GetStarPosition(ForStarDiameter: false, UpdateStarOnly: false);
				GetAsteroidPosition();
				if (Settings.Default.Asteroid_Paste_Sort)
				{
					EventDetails.SortObservers(4);
					Renumber();
					PopulateListOfObservers(0);
				}
				if (Settings.Default.GE_Alts_Auto)
				{
					Validate_SiteAltitudes_MidTime("");
				}
				Data_and_Plots.ShowMissTimesForm();
				if (Data_and_Plots.ClosestMissTimes())
				{
					Data_and_Plots.Miss_Times.CorrectTimes();
				}
				if (CheckIfInHistoricalFile(out var ObserverNames))
				{
					MessageBox.Show("An entry for this Asteroid, Star and Date already exists,\r\nwith Observers: \r\n" + ObserverNames, "Possible duplicate entry", (MessageBoxButtons)0, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				}
			}
			((Control)Data_and_Plots.PlotForm).Focus();
			((Control)cmdAltitude).set_ForeColor(Color.Red);
			Validate_SiteAltitudes_MidTime("");
			((Control)this).Focus();
		}

		private void newObservationObserverGroupXMLFormatToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0057: Unknown result type (might be due to invalid IL or missing references)
			//IL_005d: Invalid comparison between Unknown and I4
			string text = Clipboard.GetText((TextDataFormat)0);
			if ((text.Contains("O+ |") | text.Contains("O- |") | text.Contains("O+1|") | text.Contains("O? |") | text.Contains("Observer: ")) && (int)MessageBox.Show("Event to be pasted appears to be from Euraster, or from JOIN.\r\n\r\nDo you want to continue as an Occult XML event?", "Paste from Occult XML", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			PreviousEventMidHour = -1.0;
			Data_and_Plots.SingleEvent.Paste_Observers();
			for (int i = 0; i < EventDetails.Observers.Count; i++)
			{
				if (EventDetails.Observers[i].TelescopeType == "8")
				{
					EventDetails.Observers[i].TimeSource = "c";
					if (EventDetails.Observers[i].Event_D == "M")
					{
						ObserverData observerData = EventDetails.Observers[i];
						bool accuracy_D_Set = (EventDetails.Observers[i].Accuracy_R_Set = false);
						observerData.Accuracy_D_Set = accuracy_D_Set;
						double num3 = (EventDetails.Observers[i].Accuracy_D = (EventDetails.Observers[i].Accuracy_R = 0.0));
						EventDetails.Observers[i].Event_R = EventDetails.Observers[i].Event_D;
						EventDetails.Observers[i].H_Reap = EventDetails.Observers[i].H_Dis;
						EventDetails.Observers[i].M_Reap = EventDetails.Observers[i].M_Dis;
						EventDetails.Observers[i].S_Reap = EventDetails.Observers[i].S_Dis;
						int num6 = (EventDetails.Observers[i].S_Dis_DecPlaces = (EventDetails.Observers[i].S_Reap_DecPlaces = 0));
					}
				}
			}
			Data_and_Plots.Observations_InitialisePlot_Using_EventDetails();
			if (Settings.Default.Asteroid_Paste_Sort)
			{
				EventDetails.SortObservers(4);
				Renumber();
				PopulateListOfObservers(0);
			}
			Data_and_Plots.ShowMissTimesForm();
			if (Data_and_Plots.ClosestMissTimes())
			{
				Data_and_Plots.Miss_Times.CorrectTimes();
			}
			Validate_SiteAltitudes_MidTime("");
			((Control)Data_and_Plots.PlotForm).Focus();
			GetShapeModels();
		}

		private void newObservationLineoneOnlyOBSFormatToolStripMenuItem_Click(object sender, EventArgs e)
		{
		}

		private void eurasterEventAsNewEventToolStripMenuItem_Click(object sender, EventArgs e)
		{
		}

		private void eurasterObservationToCurrentEventToolStripMenuItem_Click(object sender, EventArgs e)
		{
		}

		private void ImportEuraster(bool NewObservationForExistingEvent, string DataIn)
		{
		}

		private void newEventFromJPToolStripMenuItem_Click(object sender, EventArgs e)
		{
		}

		private void newObservatToolStripMenuItem_Click(object sender, EventArgs e)
		{
		}

		private void ImportJP(bool NewObservationForExistingEvents, string DataIn)
		{
		}

		private void predictionLineToolStripMenuItem_Click(object sender, EventArgs e)
		{
			PastePredictionLine();
		}

		internal void PastePredictionLine()
		{
			//IL_001a: Unknown result type (might be due to invalid IL or missing references)
			if (EventDetails.Observers.Count < 1)
			{
				MessageBox.Show("Cannot add a prediction line before an event is added.", "No data", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			EventDetails.PasteAPredictionLine();
			if (((Control)lblLastSaveTime).get_ForeColor() != Color.Red)
			{
				StartTime = DateTime.Now;
			}
			((Control)lblLastSaveTime).set_ForeColor(Color.Red);
			DisplayEventInEditor();
			if (Settings.Default.Asteroid_Paste_Sort)
			{
				EventDetails.SortObservers(4);
				Renumber();
				PopulateListOfObservers(0);
			}
			((Control)Data_and_Plots.PlotForm).Focus();
		}

		private void LineForAsnUnseenBinaryasteroidPrimaryObjectToolStripMenuItem_Click(object sender, EventArgs e)
		{
			EventDetails.PasteAnUnseenPrimaryLine();
			if (((Control)lblLastSaveTime).get_ForeColor() != Color.Red)
			{
				StartTime = DateTime.Now;
			}
			((Control)lblLastSaveTime).set_ForeColor(Color.Red);
			DisplayEventInEditor();
			if (Settings.Default.Asteroid_Paste_Sort)
			{
				EventDetails.SortObservers(4);
				Renumber();
				PopulateListOfObservers(0);
			}
			((Control)Data_and_Plots.PlotForm).Focus();
		}

		private void plotSitesInGoogleEarthToolStripMenuItem_Click(object sender, EventArgs e)
		{
			PlotSitesInGoogleEarth();
		}

		private static void PlotSitesInGoogleEarth()
		{
			//IL_001f: Unknown result type (might be due to invalid IL or missing references)
			if (EventDetails.Observers.Count < 0)
			{
				return;
			}
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
				return;
			}
			string text = EventDetails.Year + " " + Utilities.ShortMonths[EventDetails.Month] + " " + EventDetails.Day + " (" + EventDetails.AsteroidNumber + ") " + EventDetails.AsteroidID;
			if (!GoogleEarth.Create_New_GoogleEarthKMZ_File(Utilities.AppPath + "\\Asteroid\\Sites " + text + ".KML", "Sites", AutoOpenFile: true, out var CreatedFile))
			{
				return;
			}
			for (int i = 0; i < EventDetails.Observers.Count; i++)
			{
				string text2 = EventDetails.Observers[i].ObserversAll;
				int pinColor_0Yel_1Blu_2Grn_3LtBlu_4Pink_5Purp_6Red_7White = 3;
				if ((text2 == null) | (text2 == ""))
				{
					text2 = "Predicted";
				}
				if (text2 == "Predicted")
				{
					pinColor_0Yel_1Blu_2Grn_3LtBlu_4Pink_5Purp_6Red_7White = 2;
				}
				text2 = EventDetails.Observers[i].SeqNumber + " " + text2;
				if (i == 0)
				{
					GoogleEarth.Write_PlaceMark_PinPlusName_GoogleEarthKML(text2, EventDetails.Observers[i].Longitude, EventDetails.Observers[i].Latitude, pinColor_0Yel_1Blu_2Grn_3LtBlu_4Pink_5Purp_6Red_7White);
				}
				else if ((EventDetails.Observers[i].Longitude != EventDetails.Observers[i - 1].Longitude) | (EventDetails.Observers[i].Latitude != EventDetails.Observers[i - 1].Latitude))
				{
					GoogleEarth.Write_PlaceMark_PinPlusName_GoogleEarthKML(text2, EventDetails.Observers[i].Longitude, EventDetails.Observers[i].Latitude, pinColor_0Yel_1Blu_2Grn_3LtBlu_4Pink_5Purp_6Red_7White);
				}
			}
			CreatedFile = GoogleEarth.Close_GoogleEarthKMZ_File(CreatedFile, ConvertToKMZ: true);
			GoogleEarth.DisplayGoogleMap(CreatedFile);
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Asteroid observations editor");
		}

		private void copyvalidObserverNamesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ExtractObserverNames(IncludeAll: false);
		}

		private void copyObserverNamesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ExtractObserverNames(IncludeAll: true);
		}

		private static void ExtractObserverNames(bool IncludeAll)
		{
			ArrayList arrayList = new ArrayList();
			for (int i = 0; i < EventDetails.Observers.Count; i++)
			{
				if (EventDetails.Observers[i].PlotCode != "x" || IncludeAll)
				{
					string observer = EventDetails.Observers[i].Observer1;
					if (!(observer.Contains("PREDICT") | observer.Contains("CENTER") | observer.Contains("CENTRE") | observer.Contains("TIME")))
					{
						MPCName mPCName = new MPCName();
						mPCName.ObserverID = Utilities.InitialPlusName(Utilities.ProperCase(observer.ToUpper().Replace(". ", " ").Replace(".", " ")).Trim());
						arrayList.Add(mPCName);
					}
					observer = EventDetails.Observers[i].Observer2;
					if (!((observer.Length == 0) | observer.Contains("PREDICT") | observer.Contains("CENTER") | observer.Contains("CENTRE") | observer.Contains("TIME")))
					{
						MPCName mPCName2 = new MPCName();
						mPCName2.ObserverID = Utilities.InitialPlusName(Utilities.ProperCase(observer.ToUpper().Replace(". ", " ").Replace(".", " ")).Trim());
						arrayList.Add(mPCName2);
					}
				}
			}
			arrayList.Sort();
			string text = "";
			string text2 = "";
			for (int j = 0; j < arrayList.Count; j++)
			{
				if (arrayList[j]!.ToString() != text2)
				{
					text2 = arrayList[j]!.ToString();
					string text3 = text2.ToUpper();
					if (!(text3.Contains("PREDICT") | text3.Contains("CENTER") | text3.Contains("CENTRE") | text3.Contains("TIME")))
					{
						text = ((text.Length >= 1) ? (text + ", " + text2) : text2);
					}
				}
			}
			Clipboard.SetText(text);
		}

		private void copyAllObserverDetailsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ArrayList arrayList = new ArrayList();
			for (int i = 0; i < EventDetails.Observers.Count; i++)
			{
				string text = EventDetails.Observers[i].ObserversAll;
				int num = text.IndexOf(",");
				if (num > 0)
				{
					text = text.Substring(0, num);
				}
				do
				{
					num = text.IndexOf("/");
					string text2;
					if (num > 0)
					{
						text2 = text.ToUpper().Substring(0, num);
						if (!(text2.Contains("PREDICT") | text2.Contains("CENTER") | text2.Contains("CENTRE") | text2.Contains("TIME")))
						{
							int num2 = text2.IndexOf("ET AL");
							if (num2 > 0)
							{
								text2 = text2.Substring(0, num2 - 1);
							}
							MPCName mPCName = new MPCName();
							mPCName.ObserverID = Utilities.InitialPlusName(Utilities.ProperCase(text2.Trim().Replace(". ", " ").Replace(".", " ")).Trim(), NameLast: true);
							mPCName.SiteLongitude = EventDetails.Observers[i].FormattedLongitude;
							mPCName.SiteLatitude = EventDetails.Observers[i].FormattedLatitude;
							mPCName.SiteAlt = string.Format("{0,4:f0}m", EventDetails.Observers[i].Altitude);
							mPCName.TelescopeAperture = string.Format("{0,3:f0}cm", EventDetails.Observers[i].TelescopeAperture);
							arrayList.Add(mPCName.ObserverDetails);
						}
						text = text.Substring(num + 1);
						continue;
					}
					text2 = text.ToUpper();
					if (!(text2.Contains("PREDICT") | text2.Contains("CENTER") | text2.Contains("CENTRE") | text2.Contains("TIME")))
					{
						int num2 = text2.IndexOf("ET AL");
						if (num2 > 0)
						{
							text2 = text2.Substring(0, num2 - 1);
						}
						MPCName mPCName2 = new MPCName();
						mPCName2.ObserverID = Utilities.InitialPlusName(Utilities.ProperCase(text2.ToUpper().Replace(". ", " ").Replace(".", " ")).Trim(), NameLast: true);
						mPCName2.SiteLongitude = EventDetails.Observers[i].FormattedLongitude;
						mPCName2.SiteLatitude = EventDetails.Observers[i].FormattedLatitude;
						mPCName2.SiteAlt = string.Format("{0,4:f0}m", EventDetails.Observers[i].Altitude);
						mPCName2.TelescopeAperture = string.Format("{0,3:f0}cm", EventDetails.Observers[i].TelescopeAperture);
						arrayList.Add(mPCName2.ObserverDetails);
					}
				}
				while (num > 0);
			}
			arrayList.Sort();
			string text3 = "";
			string text4 = "";
			for (int j = 0; j < arrayList.Count; j++)
			{
				if (arrayList[j]!.ToString() != text4)
				{
					text4 = arrayList[j]!.ToString();
					string text5 = text4.ToUpper();
					if (!(text5.Contains("PREDICT") | text5.Contains("CENTER") | text5.Contains("CENTRE") | text5.Contains("TIME")))
					{
						text3 = ((text3.Length >= 1) ? (text3 + "\r\n" + text4) : text4);
					}
				}
			}
			Clipboard.SetText(text3);
		}

		private void alternativeStarIdentifiersToolStripMenuItem_Click(object sender, EventArgs e)
		{
			string text = EventDetails.StarNumber;
			if (EventDetails.StarCat == null)
			{
				return;
			}
			if (EventDetails.StarCat.Contains("HIP"))
			{
				text = "HIP " + text;
			}
			else if (EventDetails.StarCat.Contains("Tyc"))
			{
				text = "TYC " + text.Replace("u", "").Trim();
			}
			else if (EventDetails.StarCat.Contains("UCAC4"))
			{
				text = "UCAC4 " + text;
			}
			Cursor.set_Current(Cursors.get_WaitCursor());
			string Content;
			bool flag = http.DownloadHTTP_string(SesameAddress + text, out Content);
			try
			{
				((Control)StarEquivalent).Show();
			}
			catch
			{
				StarEquivalent = new StarEquivalentList();
				((Control)StarEquivalent).Show();
			}
			StarEquivalent.lstStars.get_Items().Clear();
			StarEquivalent.lstStars.get_Items().Add((object)(text + " ="));
			StarEquivalent.lstStars.get_Items().Add((object)"");
			string[] array = Content.Replace('\r', ' ').Split(new char[1] { '\n' });
			bool flag2 = false;
			if (flag)
			{
				for (int i = 0; i < array.Length; i++)
				{
					int num = array[i].IndexOf("%I ");
					if (num >= 0)
					{
						StarEquivalent.lstStars.get_Items().Add((object)array[i].Substring(num + 3));
						flag2 = true;
					}
				}
				if (!flag2)
				{
					StarEquivalent.lstStars.get_Items().Add((object)"No equivalents found");
				}
			}
			else
			{
				StarEquivalent.lstStars.get_Items().Add((object)"Search failed");
			}
			Cursor.set_Current(Cursors.get_Default());
			((Control)StarEquivalent).Focus();
		}

		private void Get_StarDia_Fresnel()
		{
			if (EventDetails.StarNumber == null)
			{
				return;
			}
			double num = 0.0;
			double rAhrs = EventDetails.RA_Star_2000 * (180.0 / Math.PI) / 15.0;
			double decDeg = EventDetails.Dec_Star_2000 * (180.0 / Math.PI);
			double mgStar = EventDetails.MgStar;
			double mbStar = EventDetails.MbStar;
			double mrStar = EventDetails.MrStar;
			int GaiaVersion = 0;
			bool UsedGaia = false;
			DisplayMPOccultations.Show_StarDiameter();
			((Control)DisplayMPOccultations.StarDiameter).set_Height(390);
			((Control)DisplayMPOccultations.StarDiameter.lstDiameter).set_Height(((Control)DisplayMPOccultations.StarDiameter).get_Height() - 60);
			DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Clear();
			ObjectCollection items = DisplayMPOccultations.StarDiameter.lstDiameter.get_Items();
			string[] obj = new string[13]
			{
				EventDetails.AsteroidNumber.Trim(),
				" ",
				EventDetails.AsteroidID.Trim(),
				" occults ",
				EventDetails.StarCat,
				" ",
				EventDetails.StarNumber.Trim(),
				", on ",
				EventDetails.Year.ToString(),
				" ",
				Utilities.ShortMonths[EventDetails.Month],
				" ",
				null
			};
			int DuplicateSource = EventDetails.Day;
			obj[12] = DuplicateSource.ToString();
			items.Add((object)string.Concat(obj));
			DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)"");
			DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)"Star Diameter");
			string starCat = EventDetails.StarCat;
			string text = EventDetails.StarNumber.Trim();
			num = DisplayMPOccultations.StellarDiameter_mas / 1000.0;
			double RA;
			double Dec;
			double pmRA;
			double pmDec;
			double MagV;
			double MagB;
			double MagR;
			double Parallax_asec;
			double Epoch;
			ulong GaiaSourceID;
			string SourceFile;
			double RadialVelocity;
			double UncertRA;
			double UncertDec;
			double UncertPMRA;
			double UncertPMDec;
			double StarReliability;
			int NoGaiaPM;
			int GaiaPMfromUCAC;
			if (starCat.ToUpper().Contains("UCAC4"))
			{
				int num2 = text.IndexOf("-");
				int.TryParse(text.Substring(0, num2).Trim(), out var result);
				int.TryParse(text.Substring(num2 + 1).Trim(), out var result2);
				if (Occult.Star_Catalogues.GetStarPosition.GetUCAC4Position(result, result2, Parallax_IfNotInGaia_TryHipparcos: false, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out Parallax_asec, out Epoch, out UsedGaia, out GaiaVersion, out GaiaSourceID, out SourceFile, out RadialVelocity, out UncertRA, out UncertDec, out UncertPMRA, out UncertPMDec, out var StarDiameter_mas, out StarReliability, out DuplicateSource, out NoGaiaPM, out GaiaPMfromUCAC))
				{
					num = StarDiameter_mas / 1000.0;
				}
			}
			else if (starCat.ToUpper().Contains("TYC"))
			{
				int num3 = text.IndexOf("-");
				int num4 = text.IndexOf("-", num3 + 1);
				int.TryParse(text.Substring(0, num3).Trim(), out var result3);
				int.TryParse(text.Substring(num3 + 1, num4 - num3 - 1).Trim(), out var result4);
				int.TryParse(text.Substring(num4 + 1).Trim(), out var result5);
				if (Occult.Star_Catalogues.GetStarPosition.GetTycho2Position(result3, result4, result5, out StarReliability, out UncertPMDec, out UncertPMRA, out UncertDec, out UncertRA, out RadialVelocity, out Epoch, out Parallax_asec, out MagR, out UsedGaia, out GaiaVersion, out GaiaSourceID, out SourceFile, out MagB, out MagV, out pmDec, out pmRA, out Dec, out var StarDiameter_mas2, out RA, out GaiaPMfromUCAC, out NoGaiaPM, out DuplicateSource))
				{
					num = StarDiameter_mas2 / 1000.0;
				}
			}
			else if (starCat.ToUpper().Contains("HIP"))
			{
				int.TryParse(text.Trim(), out var result6);
				if (Occult.Star_Catalogues.GetStarPosition.GetHipparcosPosition(result6, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out Parallax_asec, out Epoch, out UsedGaia, out GaiaVersion, out GaiaSourceID, out SourceFile, out RadialVelocity, out UncertRA, out UncertDec, out UncertPMRA, out UncertPMDec, out var StarDiameter_mas3, out StarReliability, out DuplicateSource, out NoGaiaPM, out GaiaPMfromUCAC, out var _))
				{
					num = StarDiameter_mas3 / 1000.0;
				}
			}
			string Basis;
			int NumMeasures;
			bool InvalidDiameter;
			double num5 = Utilities.StarDiameter_CHARM2_CADARS(rAhrs, decDeg, mgStar, mbStar, mrStar, out Basis, out NumMeasures, out InvalidDiameter);
			if (num5 > 0.001)
			{
				num = num5;
			}
			else
			{
				Basis = "Gaia DR3";
				NumMeasures = 0;
			}
			_ = num / 3600.0 / (180.0 / Math.PI) * 8.794143836182533 / EventDetails.Parallax;
			double num6 = EventDetails.AsteroidNominalDiameter / 6378.137 * EventDetails.Parallax;
			double num7 = num / num6;
			double num8 = EventDetails.dX * EventDetails.Parallax / 3600.0;
			double num9 = EventDetails.dY * EventDetails.Parallax / 3600.0;
			double num10 = Math.Sqrt(num8 * num8 + num9 * num9);
			_ = num6 / num10;
			double num11 = num / num10;
			if (num > 0.0)
			{
				if (NumMeasures > 0)
				{
					DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)($"    Dia: {num:.0000}\"" + " [" + Basis + string.Format(", {0,1} measures]", NumMeasures)));
				}
				else
				{
					DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)($"    Dia: {num:.0000}\"" + " [" + Basis + "]"));
				}
				DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)string.Format("          = {0,2:f0}% of the asteroid's diameter", num7 * 100.0));
				if (num7 > 0.2)
				{
					DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)"  *** WARNING - special processing required ***");
				}
				if (num7 > 1.0)
				{
					DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)"  *** WARNING: Occultation is ANNULAR. ***");
				}
				if (num11 < 0.02)
				{
					DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)$"  => Fades caused by the star diameter are not expected.");
				}
				else
				{
					DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)string.Format("  => fades of about {0,1:F1} secs might be expected.", num11 * 0.7));
				}
			}
			else if (UsedGaia)
			{
				DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)("    Dia < 0.0001\" [" + Basis + "]"));
				DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)$"  => Fades caused by the star diameter are not expected.");
			}
			double mas;
			double num12 = Utilities.FresnelLength_m(8.794143836182533 / EventDetails.Parallax, 600.0, 150.0, out mas);
			DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)"");
			DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)"Fresnel diffraction");
			DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)string.Format(" distance from object edge to peak Fresnel brightness ~{0,4:f0}m", 1.225 * num12));
			DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)"");
			DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)string.Format(" diffraction for light drop of 2 mag (to 16%) = {0,1:F1} mas", mas));
			DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)string.Format("  => fades of {0,1:F2} secs might be expected", mas / 1000.0 / num10));
			double num13 = MagDrop();
			if (num13 > 2.0)
			{
				DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)string.Format(" diffraction for light drop of 4 mag (to 2.5%) = {0,1:F1} mas", 2.54 * mas));
				DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)string.Format("  => fades of {0,1:F2} secs might be expected", mas / 1000.0 / num10 * 2.54));
			}
			if (num13 > 4.0)
			{
				DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)string.Format(" diffraction for light drop of 6 mag (to 0.4%) = {0,1:F1} mas", 6.4 * mas));
				DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)string.Format("  => fades of {0,1:F2} secs might be expected", mas / 1000.0 / num10 * 6.4));
			}
			DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)"");
			DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)"");
			DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)"All times should be divided by sin(impact angle)");
			DisplayMPOccultations.StarDiameter.lstDiameter.get_Items().Add((object)"                * * *");
			((Control)DisplayMPOccultations.StarDiameter).Focus();
		}

		private void satelliteIRDiametersToolStripMenuItem_Click(object sender, EventArgs e)
		{
			int result = 0;
			if (!optPlanet.get_Checked() && int.TryParse(((Control)txtAsteroidNumber).get_Text(), out result))
			{
				Utilities.Display_IR_AsteroidDiameter(result, ShowInForm: true, out var _);
			}
		}

		private void previousOccultationsOfThisStarToolStripMenuItem_Click(object sender, EventArgs e)
		{
			GetPreviousOccultsOfStar(EventDetails.RA_Star_2000, EventDetails.Dec_Star_2000, DisplayOnlyIfFound: false, out var _);
		}

		private void cmbMethod_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (((ListControl)cmbMethod).get_SelectedIndex() > 7)
			{
				((ListControl)cmbMethod).set_SelectedIndex(7);
			}
		}

		private void cmbTime_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (((ListControl)cmbTime).get_SelectedIndex() > 7)
			{
				((ListControl)cmbTime).set_SelectedIndex(7);
			}
		}

		private string EventLabel()
		{
			string text = "(" + ((Control)txtAsteroidNumber).get_Text().Trim() + ") " + ((Control)txtAsteroidName).get_Text().Trim();
			string text2 = " " + ((Control)txtYear).get_Text() + " " + Utilities.ShortMonths[int.Parse(((Control)txtMonth).get_Text())] + " " + ((Control)txtDay).get_Text();
			return text + text2;
		}

		private void listCurrentEventDoubleSolutionsToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			//IL_0039: Unknown result type (might be due to invalid IL or missing references)
			if (((Form)Data_and_Plots.PlotForm).get_TopMost())
			{
				Data_and_Plots.PlotForm.EnsurePlotObservations_NotOnTop(EnsureNotOnTop: true);
			}
			((Control)this).Focus();
			if (EventDetails.NumberOfDoubleSolutions < 1)
			{
				MessageBox.Show("There are no double star solutions for this event", "No double star solutions", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
			}
			else
			{
				DisplayDoubleStarSolution();
			}
		}

		private void DisplayDoubleStarSolution()
		{
			string text = "Double star details\r\n";
			text = text + EventLabel() + "\r\n";
			text += "Soln   Sep (masec)      P.A. \r\n";
			for (int i = 0; i < EventDetails.Doubles.Count; i++)
			{
				if (EventDetails.Doubles[i].Sep_Companion != 0.0)
				{
					text = ((!(EventDetails.Doubles[i].Sep_Companion < 0.0)) ? (text + " " + EventDetails.Doubles[i].ToString() + "\r\n") : (text + " " + EventDetails.Doubles[i].ToString().Replace("-", ">") + "\r\n"));
				}
			}
			try
			{
				((Control)D_Doubles).Show();
			}
			catch
			{
				D_Doubles = new DisplayData();
				((Control)D_Doubles).Show();
				((Form)D_Doubles).set_Location(Settings.Default.LocationD_Doubles);
			}
			((ToolStripItem)D_Doubles.cmdCancel).set_Visible(false);
			((ToolStripItem)D_Doubles.cmdOK).set_Visible(false);
			((Control)D_Doubles).set_Text("Double star solutions " + EventLabel());
			((Control)D_Doubles).set_Width(400);
			((Control)D_Doubles).set_Height(200);
			((Control)D_Doubles.txtBox).set_Text(text);
			((Control)D_Doubles).Show();
		}

		private void deleteCurrentEventDoubleSolutionsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_003f: Unknown result type (might be due to invalid IL or missing references)
			if (((Form)Data_and_Plots.PlotForm).get_TopMost())
			{
				Data_and_Plots.PlotForm.EnsurePlotObservations_NotOnTop(EnsureNotOnTop: true);
			}
			((Control)this).Focus();
			DoubleStarDeletions doubleStarDeletions = new DoubleStarDeletions();
			((Control)doubleStarDeletions).set_Text("Delete double star solutions");
			((Control)doubleStarDeletions.lblEvent).set_Text(EventLabel());
			((Form)doubleStarDeletions).ShowDialog();
		}

		private void listCurrenteventSatelliteSolutionToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0038: Unknown result type (might be due to invalid IL or missing references)
			if (((Form)Data_and_Plots.PlotForm).get_TopMost())
			{
				Data_and_Plots.PlotForm.EnsurePlotObservations_NotOnTop(EnsureNotOnTop: true);
			}
			((Control)this).Focus();
			if (!EventDetails.AsteroidHasSatellite)
			{
				MessageBox.Show("There is no satellite solution for this event", "No satellite solutions", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return;
			}
			string text = "Satellite details\r\n";
			text = text + EventLabel() + "\r\n\r\n";
			text += "                Sep (masec)    P.A.   P.A.2000     A    B   P.A.\r\n";
			StringBuilder stringBuilder = new StringBuilder();
			for (int i = 0; i < EventDetails.NumberOfSatellites; i++)
			{
				stringBuilder.Append(EventDetails.Satellites[i].CompanionIAUname.PadRight(16));
				stringBuilder.Append(string.Format("{0,6:f1}mas", EventDetails.Satellites[i].SatelliteSeparation).PadLeft(6));
				stringBuilder.Append(string.Format("{0,5:f1}°", EventDetails.Satellites[i].SatellitePA_Apparent).PadLeft(11));
				stringBuilder.Append(string.Format("{0,5:f1}°", EventDetails.Satellites[i].SatellitePA_2000).PadLeft(11));
				stringBuilder.Append(string.Format("{0,2:f0}", EventDetails.Satellites[i].MajorAxisSatellite).PadLeft(5));
				stringBuilder.Append(string.Format("{0,2:f0}", EventDetails.Satellites[i].MinorAxisSatellite).PadLeft(5));
				stringBuilder.Append(string.Format("{0,2:f0}", EventDetails.Satellites[i].PAAxisSatellite).PadLeft(6));
				stringBuilder.Append("\r\n");
			}
			text = text + stringBuilder.ToString() + "\r\n";
			try
			{
				((Control)D_Satellites).Show();
			}
			catch
			{
				D_Satellites = new DisplayData();
				((Control)D_Satellites).Show();
				((Form)D_Satellites).set_Location(Settings.Default.LocationSatelliteSoln);
			}
			((ToolStripItem)D_Satellites.cmdCancel).set_Visible(false);
			((ToolStripItem)D_Satellites.cmdOK).set_Visible(false);
			((Control)D_Satellites).set_Text("Satellite solution " + EventLabel());
			((Control)D_Satellites).set_Width(800);
			((Control)D_Satellites).set_Height(300);
			((Control)D_Satellites.txtBox).set_Text(text);
			((TextBoxBase)D_Satellites.txtBox).set_SelectionStart(0);
			((TextBoxBase)D_Satellites.txtBox).Select(0, 0);
			((Control)D_Satellites).Show();
		}

		private void deeteCurrenteventSatelliteSolutionToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_001a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0060: Unknown result type (might be due to invalid IL or missing references)
			if (EventDetails.NumberOfSatellites == 0)
			{
				MessageBox.Show("There is no satellite solution for this asteroid", "No satellite solution", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return;
			}
			if (((Form)Data_and_Plots.PlotForm).get_TopMost())
			{
				Data_and_Plots.PlotForm.EnsurePlotObservations_NotOnTop(EnsureNotOnTop: true);
			}
			((Control)this).Focus();
			SatelliteDeletions satelliteDeletions = new SatelliteDeletions();
			((Control)satelliteDeletions).set_Text("Delete satellite solutions");
			((Control)satelliteDeletions.lblEvent).set_Text(EventLabel());
			((Form)satelliteDeletions).ShowDialog();
		}

		private void listCurrenteventShapeModelsSolutionsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0038: Unknown result type (might be due to invalid IL or missing references)
			if (((Form)Data_and_Plots.PlotForm).get_TopMost())
			{
				Data_and_Plots.PlotForm.EnsurePlotObservations_NotOnTop(EnsureNotOnTop: true);
			}
			((Control)this).Focus();
			if (!EventDetails.ShapeModelFitted)
			{
				MessageBox.Show("No shape models have been fitted to the observation", "No shape model fits", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return;
			}
			EventDetails.ShapeData.Sort();
			DisplayShapeModelFits();
		}

		private void DisplayShapeModelFits()
		{
			string text = "Shape model fit details  -  Volume-equivalent diameters (km)\r\n";
			text = text + EventLabel() + "\r\n";
			text += "                                          Phs  Diameter\r\n";
			text += "Source Model   Surf/Vol  Fit quality      Crn  Min  Max  Version\r\n";
			for (int i = 0; i < EventDetails.ShapeData.Count; i++)
			{
				text = text + EventDetails.ShapeData[i].ToString() + "\r\n";
			}
			try
			{
				((Control)D_ShapeModels).Show();
			}
			catch
			{
				D_ShapeModels = new DisplayData();
				((Control)D_ShapeModels).Show();
				((Form)D_ShapeModels).set_Location(Settings.Default.LocationD_ShapeModels);
			}
			((ToolStripItem)D_ShapeModels.cmdCancel).set_Visible(false);
			((ToolStripItem)D_ShapeModels.cmdOK).set_Visible(false);
			((Control)D_ShapeModels).set_Text("Shape model fits " + EventLabel());
			((Control)D_ShapeModels).set_Width(800);
			((Control)D_ShapeModels).set_Height(240);
			((Control)D_ShapeModels.txtBox).set_Text(text);
			((TextBoxBase)D_ShapeModels.txtBox).set_SelectionStart(0);
			((TextBoxBase)D_ShapeModels.txtBox).Select(0, 0);
			((Form)D_ShapeModels).set_TopMost(false);
		}

		private void createNewExportFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)Data_and_Plots.PlotForm).set_TopMost(false);
			string text = SelectExportImportFile(GetExportFiles(), Import: false, NewFileName: true);
			if (text != "")
			{
				Settings.Default.ExportFile = text;
			}
			((ToolStripItem)toolStripMenuItemExportFile2).set_Text("(  " + Settings.Default.ExportFile + "  )");
			AllEdits.ReadEditsFile(text);
		}

		private void selectExportFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_002c: Unknown result type (might be due to invalid IL or missing references)
			((Form)Data_and_Plots.PlotForm).set_TopMost(false);
			string exportFiles = GetExportFiles();
			if (exportFiles == "")
			{
				MessageBox.Show("There are no Export files to select for export", "No Export files", (MessageBoxButtons)0, (MessageBoxIcon)64);
				return;
			}
			string text = SelectExportImportFile(exportFiles, Import: false, NewFileName: false);
			if (text != "")
			{
				Settings.Default.ExportFile = text;
				AllEdits.ReadEditsFile(text);
				((ToolStripItem)toolStripMenuItemExportFile2).set_Text("(  " + text + "  ) " + AllEdits.All_Edits.Count + " events");
			}
		}

		private void selectImportFileImportToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_002c: Unknown result type (might be due to invalid IL or missing references)
			((Form)Data_and_Plots.PlotForm).set_TopMost(false);
			string exportFiles = GetExportFiles();
			if (exportFiles == "")
			{
				MessageBox.Show("There are no Export files to select for import", "No Export files", (MessageBoxButtons)0, (MessageBoxIcon)64);
				return;
			}
			string text = SelectExportImportFile(exportFiles, Import: true, NewFileName: false);
			if (text != "")
			{
				Settings.Default.ImportFile = text;
				AllEdits.ReadEditsFile(text);
				((ToolStripItem)toolStripMenuItemImportFile).set_Text("(  " + text + "  ) " + AllEdits.All_Edits.Count + " events");
			}
		}

		private string GetExportFiles()
		{
			string text = "";
			string[] files = Directory.GetFiles(Utilities.AppPath + "/Import_Export");
			for (int i = 0; i < files.Length; i++)
			{
				string fileName = Path.GetFileName(files[i]);
				if (fileName.ToLower().Contains("export.xml") && fileName.ToLower() != "export.xml")
				{
					text = ((!(text == "")) ? (text + "," + fileName) : fileName);
				}
			}
			return text;
		}

		private string SelectExportImportFile(string Files, bool Import, bool NewFileName)
		{
			//IL_000a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0010: Invalid comparison between Unknown and I4
			SelectExportImportFile selectExportImportFile = new SelectExportImportFile(Files, Import, NewFileName);
			if ((int)((Form)selectExportImportFile).ShowDialog() == 1)
			{
				if (Import)
				{
					Settings.Default.ImportFileLastEntry = 0;
				}
				return selectExportImportFile.SelectedFile;
			}
			return "";
		}

		private void exportCurrentEventToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0052: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00db: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e1: Invalid comparison between Unknown and I4
			string Comment = "";
			((Form)Data_and_Plots.PlotForm).set_TopMost(false);
			if (!File.Exists(Utilities.AppPath + "\\Import_Export\\" + Settings.Default.ExportFile))
			{
				MessageBox.Show("The Export file  " + Settings.Default.ExportFile + "  does not exist", "No export file", (MessageBoxButtons)0, (MessageBoxIcon)64);
				return;
			}
			if ((Settings.Default.ExportFile.ToUpper() == "EXPORT.XML") | !Settings.Default.ExportFile.ToUpper().Contains("EXPORT.XML"))
			{
				MessageBox.Show("The current Export file name:\r\n   " + Settings.Default.ExportFile + "\r\n is not a valid Export file name\r\n\r\nThe Export file needs to be in the form   *Export.xml\r\n\r\nPlease create a new Export file, or set a new Export file", "Invalid Export file", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return;
			}
			AllEdits.ReadEditsFile(Settings.Default.ExportFile);
			int num = AllEdits.EventInEditsFile(out Comment);
			if (num < 0 || (int)((Form)new CompareSolutions(num, ConfirmImport: false)).ShowDialog() != 2)
			{
				AllEdits.AddEditedEvent(num);
				AllEdits.WriteEditsFile(Settings.Default.ExportFile);
				((ToolStripItem)toolStripMenuItemExportFile2).set_Text("(  " + Settings.Default.ExportFile + "  ) " + AllEdits.All_Edits.Count + " events");
				if (Settings.Default.ImportFile == Settings.Default.ExportFile)
				{
					((ToolStripItem)toolStripMenuItemImportFile).set_Text("(  " + Settings.Default.ImportFile + "  ) " + AllEdits.All_Edits.Count + " events");
				}
			}
		}

		private void openCommentsFormForCurrentEventToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Data_and_Plots.ShowExportComments(WhenExporting: true);
			PopulateExportCommentsForm(WhenExporting: true);
		}

		internal void PopulateExportCommentsForm(bool WhenExporting)
		{
			string Comment = "";
			if (WhenExporting)
			{
				if (Data_and_Plots.Export_Comments == null)
				{
					Data_and_Plots.ShowExportComments(WhenExporting);
				}
				((Control)Data_and_Plots.Export_Comments.label1).set_Text("Comments in " + Settings.Default.ExportFile);
			}
			else
			{
				if (Data_and_Plots.Export_Comments == null || !((Control)Data_and_Plots.Export_Comments).get_Enabled())
				{
					return;
				}
				((Control)Data_and_Plots.Export_Comments.label1).set_Text("Comments in " + Settings.Default.ImportFile);
			}
			((Control)Data_and_Plots.Export_Comments).set_Text("Comments in the Export file for (" + EventDetails.AsteroidNumber + ") on " + EventDetails.Year + " " + Utilities.ShortMonths[EventDetails.Month] + " " + EventDetails.Day.ToString().PadLeft(2, '0'));
			if ((AllEdits.All_Edits.Count < 1) | (AllEdits.LastReadFile != Settings.Default.ImportFile))
			{
				AllEdits.ReadEditsFile(Settings.Default.ImportFile);
			}
			AllEdits.EventInEditsFile(out Comment);
			Comment = Comment.Replace("^", "\r\n").Replace(".gt.", ">").Replace(".lt.", "<");
			((Control)Data_and_Plots.Export_Comments.txtComments).set_Text(Comment);
		}

		private void listEventsInExportFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_004c: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ac: Unknown result type (might be due to invalid IL or missing references)
			//IL_0109: Unknown result type (might be due to invalid IL or missing references)
			//IL_010f: Invalid comparison between Unknown and I4
			((Form)Data_and_Plots.PlotForm).set_TopMost(false);
			if (!File.Exists(Utilities.AppPath + "\\Import_Export\\" + Settings.Default.ImportFile))
			{
				MessageBox.Show("The Import file  " + Settings.Default.ImportFile + "  does not exist", "No import file", (MessageBoxButtons)0, (MessageBoxIcon)64);
				return;
			}
			if ((Settings.Default.ImportFile.ToUpper() == "EXPORT.XML") | !Settings.Default.ImportFile.ToUpper().Contains("EXPORT.XML"))
			{
				MessageBox.Show("The current Import file name:\r\n   " + Settings.Default.ImportFile + "\r\n is not a valid Import file name\r\n\r\nThe Import file needs to be in the form   *Export.xml\r\n\r\nPlease create a new Import file, or set a new Import file", "Invalid Import file", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return;
			}
			AllEdits.ReadEditsFile(Settings.Default.ImportFile);
			if (AllEdits.All_Edits.Count <= 0)
			{
				return;
			}
			optByDate.set_Checked(true);
			((ListControl)cmbYearRange).set_SelectedIndex(cmbYearRange.get_Items().get_Count() - 1);
			DisplayIndex(ForCurrentEvent: false);
			AsteroidsInEditsFile asteroidsInEditsFile = new AsteroidsInEditsFile();
			if ((int)((Form)asteroidsInEditsFile).ShowDialog() != 2)
			{
				EntryForImportEdit = asteroidsInEditsFile.SelectedEntry;
				Settings.Default.ImportFileLastEntry = EntryForImportEdit;
				int num = AllEdits.EditEventInHistoricalFile(EntryForImportEdit);
				if (num >= 0)
				{
					((ListControl)cmbHistorical).set_SelectedIndex(num);
					CurrentHistoricalRecord = Asteroid_Observations_Reports.HistoricalIndex[((ListControl)cmbHistorical).get_SelectedIndex()].StartRecord;
					Data_and_Plots.Historical_AllEvents.DecodeAnEvent_Into_EventDetails(CurrentHistoricalRecord);
					Data_and_Plots.Observations_InitialisePlot_Using_EventDetails();
					VisibilityOfControls(HistoricalEnabled: true);
					((Control)this).set_Text("Asteroid observations editor : Historical file - " + cmbHistorical.get_Items().get_Item(((ListControl)cmbHistorical).get_SelectedIndex()).ToString());
					ReadingMultipleOBSFiles = false;
					ReadingMultipleEurasterEntries = false;
					ReadingMultipleJPEntries = false;
					((ToolStripItem)skipToNextFileToolStripMenuItem).set_Visible(false);
					SetPasteEnabled(PasteEnabled: true);
					((Control)Data_and_Plots.PlotForm.panelShapeModelControl).set_Visible(false);
					DisplayEventInEditor();
					Data_and_Plots.ShowExportComments(WhenExporting: true);
					PopulateExportCommentsForm(WhenExporting: false);
					AllEdits.UpdateCurrentEventWithEdits(EntryForImportEdit);
				}
			}
		}

		private void importSolutionsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)Data_and_Plots.PlotForm).set_TopMost(false);
			AllEdits.UpdateCurrentEventWithEdits(EntryForImportEdit);
			Data_and_Plots.ShowExportComments(WhenExporting: true);
			PopulateExportCommentsForm(WhenExporting: false);
		}

		private void astrometricSolutionToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			//IL_0024: Unknown result type (might be due to invalid IL or missing references)
			if (((ListControl)Data_and_Plots.PlotForm.cmbQuality).get_SelectedIndex() == 0)
			{
				MessageBox.Show("There is no astrometry to show, as the Quality setting\r\nis set to\r\n\r\nNo reliable position or size", "No astrometry", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
			}
			else
			{
				Asteroid_Observations_Reports.DisplayAstrometricSolution(IncludeConjunctionSolution: false, ReDoUpdate: true);
			}
		}

		private void astrometricSolutionIncludingatConjunctionSolutionToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0024: Unknown result type (might be due to invalid IL or missing references)
			if (((ListControl)Data_and_Plots.PlotForm.cmbQuality).get_SelectedIndex() == 0)
			{
				MessageBox.Show("There is no astrometry to show, as the Quality setting\r\nis set to\r\n\r\nNo reliable position or size", "No astrometry", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
			}
			else
			{
				Asteroid_Observations_Reports.DisplayAstrometricSolution(IncludeConjunctionSolution: true, ReDoUpdate: true);
			}
		}

		private void chkPredicted_Leave(object sender, EventArgs e)
		{
			if (chkPredicted.get_Checked() && ((((Control)txtObserver1).get_Text() != "Predicted") | (((Control)txtObserver2).get_Text().Trim() != "") | (((Control)txtLocatedNear).get_Text().Trim() != "") | (((Control)txtStateCountry).get_Text().Trim() != "") | chkEtAl.get_Checked()))
			{
				((Control)txtObserver1).set_Text("Predicted");
				((Control)txtObserver2).set_Text("");
				((Control)txtLocatedNear).set_Text("");
				((Control)txtStateCountry).set_Text("");
				chkEtAl.set_Checked(false);
			}
		}

		private void GetShapeModels()
		{
			//IL_025c: Unknown result type (might be due to invalid IL or missing references)
			Data_and_Plots.PlotForm.addShapeImageFromDAMIT_ISAMToolStripMenuItem.set_Checked(false);
			Data_and_Plots.PlotForm.showShapeModelControlsOnPlotToolStripMenuItem.set_Checked(false);
			((Control)Data_and_Plots.PlotForm.panelShapeModelControl).set_Visible(false);
			if (!((Control)lblInDAMIT).get_Visible() & !((Control)lblISAM).get_Visible())
			{
				return;
			}
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			double num;
			try
			{
				num = Data_and_Plots.Historical_AllEvents.Get_MidTime_of_Observations_in_Editor();
			}
			catch
			{
				num = 12.0;
			}
			if ((Math.Abs(num - PreviousEventMidHour) < 0.05) & !NewEventForShapeModelling)
			{
				((Control)this).set_Cursor(Cursors.get_Default());
				return;
			}
			PreviousEventMidHour = num;
			NewEventForShapeModelling = false;
			int.TryParse(((Control)txtYear).get_Text(), out var result);
			int.TryParse(((Control)txtMonth).get_Text(), out var result2);
			double.TryParse(((Control)txtDay).get_Text(), out var result3);
			Utilities.Date_from_JD(Utilities.JD_from_Date(result, result2, result3 + num / 24.0), out result, out result2, out result3);
			if (Asteroid_Observations_Reports.Display_ShapeModels == null)
			{
				Asteroid_Observations_Reports.Display_ShapeModels = new DisplayShapeModels();
				((Control)Asteroid_Observations_Reports.Display_ShapeModels).set_Top(10);
				((Control)Asteroid_Observations_Reports.Display_ShapeModels).set_Left(50);
			}
			if (Asteroid_Observations_Reports.Display_ShapeModels == null)
			{
				Asteroid_Observations_Reports.ShowShapeModels();
			}
			((Control)Asteroid_Observations_Reports.Display_ShapeModels.txtYear).set_Text(result.ToString());
			((Control)Asteroid_Observations_Reports.Display_ShapeModels.txtMonth).set_Text(result2.ToString());
			int num2 = (int)Math.Floor(result3);
			((Control)Asteroid_Observations_Reports.Display_ShapeModels.txtDay).set_Text(num2.ToString());
			double num3 = (result3 - (double)num2) * 24.0;
			((Control)Asteroid_Observations_Reports.Display_ShapeModels.txtHrs).set_Text(Math.Floor(num3).ToString());
			((Control)Asteroid_Observations_Reports.Display_ShapeModels.txtMin).set_Text(string.Format("{0,1:f0}", (num3 - Math.Floor(num3)) * 60.0));
			Asteroid_Observations_Reports.ShowShapeModels();
			Asteroid_Observations_Reports.Display_ShapeModels.ResetDownloadModels_Text();
			Asteroid_Observations_Reports.Display_ShapeModels.plotOnEarthPlaneToolStripMenuItem.set_Checked(false);
			DisplayShapeModels.PlotOnEarthPlane = false;
			if (!int.TryParse(((Control)txtAsteroidNumber).get_Text(), out var result4))
			{
				result4 = 0;
			}
			((Control)Asteroid_Observations_Reports.Display_ShapeModels).Focus();
			Application.DoEvents();
			if (Asteroid_Observations_Reports.Display_ShapeModels.SetAsteroid(result4))
			{
				Data_and_Plots.PlotForm.addShapeImageFromDAMIT_ISAMToolStripMenuItem.set_Checked(true);
			}
			else
			{
				MessageBox.Show("Asteroid is not in DAMIT or ISAM databases");
			}
			Data_and_Plots.PlotForm.addShapeImageFromDAMIT_ISAMToolStripMenuItem.set_Checked(true);
			Data_and_Plots.PlotForm.showShapeModelControlsOnPlotToolStripMenuItem.set_Checked(true);
			((Control)Data_and_Plots.PlotForm.panelShapeModelControl).set_Visible(true);
			if (Asteroid_Observations_Reports.Display_ShapeModels.NumDAMITImages > 0)
			{
				Data_and_Plots.PlotForm.TransferImages("1");
			}
			else
			{
				Data_and_Plots.PlotForm.TransferImages("7");
			}
			Application.DoEvents();
			((Control)this).set_Cursor(Cursors.get_Default());
		}

		private void ObservationsEditor_Activated(object sender, EventArgs e)
		{
			Data_and_Plots.PlotForm.Set_ColorFor_cmdForDiameters();
			Data_and_Plots.PlotForm.CollapseReviewList();
			if (EditorHasBeenActivated)
			{
				Data_and_Plots.PlotForm.EnsurePlotObservations_NotOnTop(EnsureNotOnTop: true);
				((Control)this).Focus();
				EditorHasBeenActivated = true;
			}
		}

		private void helpOnExportImportToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Export/Import");
		}

		private void deleteCurrenteventShapemodelSolutionsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0039: Unknown result type (might be due to invalid IL or missing references)
			if (((Form)Data_and_Plots.PlotForm).get_TopMost())
			{
				Data_and_Plots.PlotForm.EnsurePlotObservations_NotOnTop(EnsureNotOnTop: true);
			}
			((Control)this).Focus();
			ShapeModelDeletions shapeModelDeletions = new ShapeModelDeletions();
			((Control)shapeModelDeletions).set_Text("Delete shape models - " + EventLabel());
			((Form)shapeModelDeletions).ShowDialog();
			if (((Control)Data_and_Plots.PlotForm.cmdTransferImage[0]).get_Enabled())
			{
				Data_and_Plots.PlotForm.TransferImages("1");
			}
			else if (((Control)Data_and_Plots.PlotForm.cmdTransferImage[6]).get_Enabled())
			{
				Data_and_Plots.PlotForm.TransferImages("7");
			}
		}

		private void txtShift_Leave(object sender, EventArgs e)
		{
			//IL_004c: Unknown result type (might be due to invalid IL or missing references)
			double.TryParse(((Control)txtShift).get_Text(), out var result);
			if (Math.Abs(result) > 999.0)
			{
				MessageBox.Show("The value for the Time Shift (" + ((Control)txtShift).get_Text() + ") is too large, and will be removed", "Time shift too large", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				((Control)txtShift).set_Text("");
			}
		}

		private void downloadShapeModelsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			GetShapeModels();
		}

		private void GravitationalLightDeflectionToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.Show_Relativity();
			string text = Utilities.DEGtoDMS(EventDetails.RA_Star_2000 * (180.0 / Math.PI) / 15.0, 2, 2, MinutesOnly: false);
			((Control)DisplayMPOccultations.Relativity.txtRH1).set_Text(text.Substring(0, 2));
			((Control)DisplayMPOccultations.Relativity.txtRM1).set_Text(text.Substring(3, 2));
			((Control)DisplayMPOccultations.Relativity.txtRS1).set_Text(text.Substring(6));
			string text2 = Utilities.DEGtoDMS(EventDetails.Dec_Star_2000 * (180.0 / Math.PI), 3, 1, MinutesOnly: false);
			((Control)DisplayMPOccultations.Relativity.txtDD1).set_Text(text2.Substring(0, 3));
			((Control)DisplayMPOccultations.Relativity.txtDM1).set_Text(text2.Substring(4, 2));
			((Control)DisplayMPOccultations.Relativity.txtDS1).set_Text(text2.Substring(7));
			DisplayMPOccultations.Relativity.updnYear.set_Value((decimal)EventDetails.Year);
			DisplayMPOccultations.Relativity.updnMonth.set_Value((decimal)EventDetails.Month);
			DisplayMPOccultations.Relativity.updnDay.set_Value((decimal)((double)EventDetails.Day + EventDetails.MidT_forMotions / 24.0));
			((Control)DisplayMPOccultations.Relativity.txtDistance).set_Text($"{8.794143836182533 / EventDetails.Parallax:f5}");
			DisplayMPOccultations.Relativity.Compute();
		}

		private void stellarDiameterFresnelDiffractionToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Get_StarDia_Fresnel();
		}

		private void asteroidLightCurveDataToolStripMenuItem_Click(object sender, EventArgs e)
		{
		}

		private void eventsWhereDAMITHasBeenUpdatedToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Asteroid_Observations_Reports.ListDAMITupdatesNeeded();
		}

		private void testGaiaCorrnToolStripMenuItem_Click(object sender, EventArgs e)
		{
		}

		private void testGaiaDownloadToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Gaia_ESA.GetGaiaDetailsForStarsInBox(2458485.0, 3, 95.134958, 33.94253, 30.0, 14.0);
		}

		private void starDetailsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append("Star details for occultation by (" + EventDetails.AsteroidNo + ") " + EventDetails.AsteroidID + "\r\n");
			Utilities.Date_from_JD(EventDetails.JD_EventDate, out var Year, out var Month, out var day);
			stringBuilder.AppendFormat(" on {0} {1} {2}\r\n\r\n", Year, Utilities.ShortMonths[Month], Math.Floor(day));
			stringBuilder.Append("Star identifier : " + EventDetails.StarCat + "  " + EventDetails.StarNumber + "\r\n");
			if (EventDetails.Gaia_ID.Length > 1)
			{
				stringBuilder.Append("Gaia id = " + EventDetails.Gaia_ID + "\r\n");
			}
			if (EventDetails.GaiaVersion == -1)
			{
				stringBuilder.Append("Source = Other");
			}
			else if (EventDetails.GaiaVersion == 0)
			{
				stringBuilder.Append("Source = Hipparcos");
			}
			else
			{
				stringBuilder.Append("Source of position:  Gaia DR" + EventDetails.GaiaVersion);
			}
			stringBuilder.Append("\r\nConcerns\r\n  RUWE - ");
			if (EventDetails.StarReliability == 0.0)
			{
				stringBuilder.Append("NO PROPER MOTION");
			}
			else
			{
				stringBuilder.Append(string.Format("{0,1:f2}", EventDetails.StarReliability));
				if (EventDetails.StarReliability < 1.4)
				{
					stringBuilder.Append(" (Good)   ");
				}
				else
				{
					stringBuilder.Append(" (Concern)");
				}
			}
			stringBuilder.Append(":  Duplicate source - ");
			if (EventDetails.DuplicatedSource < 0)
			{
				stringBuilder.Append("Not set");
			}
			if (EventDetails.DuplicatedSource == 0)
			{
				stringBuilder.Append("No");
			}
			else
			{
				stringBuilder.Append("Yes ");
			}
			stringBuilder.AppendFormat("\r\n\r\nMb {0,5:f2}    Mg {1,5:f2}    Mr {2,5:f2}\r\n", EventDetails.MbStar, EventDetails.MgStar, EventDetails.MrStar);
			if (Utilities.StarTypeFromBVR(EventDetails.MbStar, EventDetails.MgStar, EventDetails.MrStar, out var SpecType))
			{
				stringBuilder.Append("Estimated class  " + SpecType + "\r\n");
			}
			string value = "Estimated Dia <0.1 mas [Gaia DR3]";
			if (EventDetails.StarDia_mas >= 0.1)
			{
				value = string.Format("Estimated Dia {0,4:f1} mas  [Gaia DR3]", EventDetails.StarDia_mas);
			}
			string Basis;
			int NumMeasures;
			bool InvalidDiameter;
			double num = 1000.0 * Utilities.StarDiameter_CHARM2_CADARS(EventDetails.RA_Star_2000 * (180.0 / Math.PI) / 15.0, EventDetails.Dec_Star_2000 * (180.0 / Math.PI), EventDetails.MgStar, EventDetails.MbStar, EventDetails.MrStar, out Basis, out NumMeasures, out InvalidDiameter);
			if (num > 1.0)
			{
				value = string.Format("Measured Dia {0,4:f1} mas [CHARM2/CADAR] {1,1} measures", num, NumMeasures);
			}
			stringBuilder.Append(value);
			stringBuilder.AppendFormat("\r\n\r\nAstrometric position at {0,8:f4}\r\n", Utilities.BesselianYear(EventDetails.JD_EventDate));
			stringBuilder.Append("  RA  " + Utilities.DEGtoDMS(EventDetails.RA_Star_2000 * (180.0 / Math.PI) / 15.0, 2, 6, MinutesOnly: false, IncludeLeadingZeros: true) + string.Format("  ±{0,5:f2} mas\r\n", EventDetails.RA_Star_Uncertainty_mas));
			stringBuilder.Append(" Dec " + Utilities.DEGtoDMS(EventDetails.Dec_Star_2000 * (180.0 / Math.PI), 3, 5, MinutesOnly: false, IncludeLeadingZeros: true, IncludePlusSymbol: true) + string.Format("   ±{0,5:f2} mas\r\n", EventDetails.Dec_Star_Uncertainty_mas));
			if ((EventDetails.GaiaVersion > 1) & (EventDetails.MgStar < 13.0))
			{
				Gaia.Gaia_FrameRotationCorrections(EventDetails.GaiaVersion, EventDetails.JD_EventDate, EventDetails.RA_Star_2000, EventDetails.Dec_Star_2000, EventDetails.MgStar, out var dRA_asec, out var dDec_asec, out var dRA_asec_Uncert, out var dDec_asec_Uncert);
				string text = "EDR3";
				if (EventDetails.GaiaVersion == 2)
				{
					text = "DR2";
				}
				stringBuilder.Append("\r\nGaia " + text + " frame rotation correction, to \r\nbe added to the Astrometric position\r\n");
				stringBuilder.AppendFormat("  RA {0,5:f2} ±{1,5:f2} mas    Dec {2,5:f2} ±{3,5:f2} mas", dRA_asec * 1000.0, dRA_asec_Uncert * 1000.0, dDec_asec * 1000.0, dDec_asec_Uncert * 1000.0);
			}
			if (EventDetails.NumberOfDoubleSolutions == 1)
			{
				double num2 = (0.0 - EventDetails.Doubles[0].Sep_Companion) * Math.Sin(EventDetails.Doubles[0].PA_Companion / (180.0 / Math.PI));
				double num3 = (0.0 - EventDetails.Doubles[0].Sep_Companion) * Math.Cos(EventDetails.Doubles[0].PA_Companion / (180.0 / Math.PI));
				stringBuilder.AppendFormat("\r\n\r\nEvent double star soln: Sep {0,1:f1}mas, PA {1,1:f1}°", EventDetails.Doubles[0].Sep_Companion, EventDetails.Doubles[0].PA_Companion);
				stringBuilder.AppendFormat("\r\nOffset to main component: dRA {0,1:f1} mas, dDec {1,1:f1} mas", (0.0 - num2) / 2.0, (0.0 - num3) / 2.0);
			}
			try
			{
				((Control)D_Stars).Show();
			}
			catch
			{
				D_Stars = new DisplayData();
				((Control)D_Stars).Show();
				((Form)D_Stars).set_Location(Settings.Default.LocationStarDetails);
			}
			((Control)D_Stars.txtBox).set_Text(stringBuilder.ToString());
			((TextBoxBase)D_Stars.txtBox).set_SelectionStart(0);
			((TextBoxBase)D_Stars.txtBox).Select(0, 0);
		}

		private bool GetPreviousOccultsOfStar(double StarRA, double StarDec, bool DisplayOnlyIfFound, out string StarId_Of_Found)
		{
			bool AnotherEventWithStarFound = false;
			string starCat = "";
			int zoneOrNum = 0;
			int num = 0;
			if (((ListControl)cmbCatalogue).get_SelectedIndex() == 0)
			{
				starCat = "Hip";
				zoneOrNum = int.Parse(((Control)txtHip).get_Text());
			}
			else if (((ListControl)cmbCatalogue).get_SelectedIndex() == 1)
			{
				starCat = "Tycho2";
				zoneOrNum = int.Parse(((Control)txtTycRegion).get_Text());
				num = int.Parse(((Control)txtTycSeqNum).get_Text());
			}
			else if (((ListControl)cmbCatalogue).get_SelectedIndex() == 2)
			{
				starCat = "UCAC4";
				zoneOrNum = int.Parse(((Control)txtU4Zone).get_Text());
				num = int.Parse(((Control)txtU4Number).get_Text());
			}
			string text = Asteroid_Observations_Reports.FindAsteroidOccultedStars(StarRA, StarDec, starCat, zoneOrNum, num, out AnotherEventWithStarFound, out StarId_Of_Found);
			try
			{
				((Form)DataBox).Close();
			}
			catch
			{
			}
			if (AnotherEventWithStarFound || !DisplayOnlyIfFound)
			{
				try
				{
					((Control)DataBox).Show();
				}
				catch
				{
					DataBox = new DisplayData();
					((Control)DataBox).Show();
				}
				((ToolStripItem)DataBox.cmdCancel).set_Visible(false);
				((ToolStripItem)DataBox.cmdOK).set_Visible(false);
				((Control)DataBox.txtBox).set_Text(text);
				((Control)DataBox).set_Width(600);
				((Control)DataBox).set_Height(260);
				((Control)DataBox).set_Text("Previous occultations of this star");
				((TextBoxBase)DataBox.txtBox).set_SelectionStart(0);
				((TextBoxBase)DataBox.txtBox).set_SelectionLength(0);
				((Control)DataBox).Focus();
			}
			return AnotherEventWithStarFound;
		}

		private void lblInDAMIT_Click(object sender, EventArgs e)
		{
			GetShapeModels();
		}

		private void addDiameterUncertaintiesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			int num = 0;
			int num2 = 0;
			double PlanetDiameterUncertainty_km = 1.0;
			double PlanetRadius = 0.0;
			Utilities.OpenAsteroidDiameterFileForReading();
			using (StreamReader streamReader = new StreamReader(AppPath + "\\Resource Files\\Asteroid~Observations.xml"))
			{
				using StreamWriter streamWriter = new StreamWriter(AppPath + "\\Resource Files\\Asteroid~ObservationsCircSoln.xml");
				do
				{
					string text = streamReader.ReadLine();
					if (text.Contains("<FileVersion>"))
					{
						text = text.Replace("2.1", "2.2");
					}
					text.Contains("1986|5|10");
					if (text.Contains(Tags.TagStart[4]))
					{
						string[] array = text.Split(new char[1] { '|' });
						string text2 = array[0].Substring(array[0].IndexOf(">") + 1);
						string PlanetName;
						if (text2.Contains("P"))
						{
							num = int.Parse(text2.Substring(1, 1));
							num2 = int.Parse(text2.Substring(3));
							if (num2 == 0)
							{
								PlanetOccultationElements.PlanetParameters(num, out PlanetName, out var _, out var _, out PlanetRadius, out PlanetDiameterUncertainty_km);
								PlanetRadius *= 12756.274;
							}
							else
							{
								PlanetRadius = Satellites.SatelliteDiameter(num, num2, out PlanetDiameterUncertainty_km);
							}
						}
						else
						{
							Utilities.GetAsteroidDiameter(int.Parse(text2), 15.0, out PlanetRadius, out PlanetDiameterUncertainty_km, out PlanetName, out var _);
						}
					}
					if (text.Contains(Tags.TagStart[6]) && text.Split(new char[1] { '|' })[6].Contains("1<"))
					{
						int num3 = text.IndexOf("|");
						num3 = text.IndexOf("|", num3 + 1);
						int num4 = text.IndexOf("|", num3 + 1);
						num4 = text.IndexOf("|", num4 + 1);
						string text3 = text.Substring(0, num3 + 1);
						string text4 = string.Format("{0,1:f1}", PlanetRadius) + "|";
						string text5 = string.Format("{0,1:f1}", PlanetRadius);
						string text6 = text.Substring(num4);
						text = text3 + text4 + text5 + text6;
					}
					streamWriter.WriteLine(text);
				}
				while (!streamReader.EndOfStream);
			}
			Utilities.CloseAsteroidDiameterFileForReading();
		}

		private void displayLightCurveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)LightCurveViewer).Show();
			}
			catch
			{
				LightCurveViewer = new LightCurveViewer();
				((Control)LightCurveViewer).Show();
			}
			((Control)LightCurveViewer.txtAsteroidNumber).set_Text(EventDetails.AsteroidNo.ToString());
			LightCurveViewer.PlotAllAsteroidByNumber = true;
			LightCurveViewer.PlotSelectedAsteroid(ByAsteroidNumber: true, ByAsteroidName: false);
		}

		private void lblISAM_Click(object sender, EventArgs e)
		{
			GetShapeModels();
		}

		private void updnSNR_ValueChanged(object sender, EventArgs e)
		{
		}

		private void cmdPlusOne_Click(object sender, EventArgs e)
		{
			if (((ListControl)cmbHistorical).get_SelectedIndex() < cmbHistorical.get_Items().get_Count() - 1)
			{
				((ListControl)cmbHistorical).set_SelectedIndex(((ListControl)cmbHistorical).get_SelectedIndex() + 1);
			}
		}

		private void chkAutoOpenSave_MouseClick(object sender, MouseEventArgs e)
		{
			chkAutoOpenSave.set_Checked(!chkAutoOpenSave.get_Checked());
			((Control)cmdPlusOne).set_Visible(chkAutoOpenSave.get_Checked());
		}

		private void cmbTelescope_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (((ListControl)cmbTelescope).get_SelectedIndex() == 8)
			{
				((ListControl)cmbDatum).set_SelectedIndex(0);
				((ListControl)cmbMethod).set_SelectedIndex(1);
				((ListControl)cmbTime).set_SelectedIndex(3);
			}
		}

		private void matchUnistellarObservationsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			try
			{
				if (!((Control)Unistellar).get_Visible())
				{
					((Control)Unistellar).Show();
				}
				else
				{
					((Control)Unistellar).BringToFront();
				}
			}
			catch
			{
				Unistellar = new Unistellar();
				((Control)Unistellar).Show();
			}
		}

		private void whenPastingSortByDistanceToolStripMenuItem_Click(object sender, EventArgs e)
		{
			whenPastingSortByDistanceToolStripMenuItem.set_Checked(!whenPastingSortByDistanceToolStripMenuItem.get_Checked());
			Settings.Default.Asteroid_Paste_Sort = whenPastingSortByDistanceToolStripMenuItem.get_Checked();
		}

		private void displayWDSInterferometricAndVariableStarDataToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Interferometric_Plus_WDS.Find_WDS_IF_Matches(EventDetails.RA_Star_2000 * (180.0 / Math.PI) / 15.0, EventDetails.Dec_Star_2000 * (180.0 / Math.PI), HighPrecision: true, ShowResults: true, out WDS_Interferometer_Display.VariableID);
		}

		private void sortByDistanceWhenOpeningToolStripMenuItem_Click(object sender, EventArgs e)
		{
			sortByDistanceWhenOpeningToolStripMenuItem.set_Checked(!sortByDistanceWhenOpeningToolStripMenuItem.get_Checked());
			Settings.Default.Asteroid_Open_Sort = sortByDistanceWhenOpeningToolStripMenuItem.get_Checked();
		}

		private void nearbyStarsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ShowNearbyStars();
		}

		private void ShowNearbyStars()
		{
			int num = 30;
			Gaia.GetNearbyStarsFromCoords(EventDetails.StarCatwithNumber, EventDetails.RA_Star_2000, EventDetails.Dec_Star_2000, EventDetails.MgStar, Utilities.BesselianYear(EventDetails.JD_EventDate) - 2000.0, num, out var ExtraMagG, out var _, out var _, out var AllStars, out var AllNearby);
			try
			{
				((Control)D_NearbyStars).Show();
			}
			catch
			{
				D_NearbyStars = new DisplayData();
				((Control)D_NearbyStars).Show();
				((Form)D_NearbyStars).set_Location(Settings.Default.LocationD_NearbyStars);
			}
			double num2 = EventDetails.MgStar + 2.0;
			if (num2 > 16.0)
			{
				num2 = 16.0;
			}
			AllNearby = "Stars within " + num + "\" of " + EventDetails.StarCatwithNumber + "\r\nbrighter than Mv " + string.Format("{0,1:f1}", num2) + "\r\n\r\n Sepn    Mv     Mr\r\n" + string.Format("Target  {0,5:f2}  {1,5:f2}  ", EventDetails.MgStar, EventDetails.MrStar) + "\r\n" + AllNearby;
			((Control)D_NearbyStars).set_Text("Nearby stars");
			((Control)D_NearbyStars.txtBox).set_Text(AllNearby);
			((Control)D_NearbyStars).set_Width(350);
			((Control)D_NearbyStars).set_Height(260);
			if (AllStars == 0)
			{
				D_NearbyStars.NoColors();
			}
			else if (EventDetails.MgStar - Utilities.CombinedMagnitude(EventDetails.MgStar, ExtraMagG) > 0.2)
			{
				D_NearbyStars.StartYellowRise();
			}
			else
			{
				D_NearbyStars.StartBeigeRise();
			}
			((TextBoxBase)D_NearbyStars.txtBox).set_SelectionStart(0);
			((TextBoxBase)D_NearbyStars.txtBox).Select(0, 0);
			((Control)D_NearbyStars).Focus();
		}

		private void cmdAltitude_Click(object sender, EventArgs e)
		{
			Validate_SiteAltitudes_MidTime("");
		}

		private void validateAllSiteAltitudesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Validate_SiteAltitudes_MidTime("");
		}

		private void Validate_SiteAltitudes_MidTime(string SaveFile)
		{
			//IL_0140: Unknown result type (might be due to invalid IL or missing references)
			GoogleMaps_Coords = new List<GoogleMapsCoords>();
			string text = "";
			bool flag = false;
			bool flag2 = false;
			string text2 = "A GoogleMaps API key for checking site coordinates is\r\nnot available. Read the Help topic for the Observations\r\nEditor, under Menu functions, to find out how to\r\nget a key\r\n\r\n";
			if (Settings.Default.GoogleMaps_API_key.Length > 10)
			{
				for (int i = 0; i < EventDetails.Observers.Count; i++)
				{
					if (!(EventDetails.Observers[i].Observer1 == "Predicted"))
					{
						GoogleMapsCoords googleMapsCoords = new GoogleMapsCoords();
						googleMapsCoords.Observer = EventDetails.Observers[i].Observer1;
						googleMapsCoords.SeqNum = EventDetails.Observers[i].SeqNumber;
						googleMapsCoords.Longitude = EventDetails.Observers[i].Longitude;
						if (googleMapsCoords.Longitude > 180.0)
						{
							googleMapsCoords.Longitude -= 360.0;
						}
						googleMapsCoords.Latitude = EventDetails.Observers[i].Latitude;
						googleMapsCoords.Altitude = EventDetails.Observers[i].Altitude;
						GoogleMaps_Coords.Add(googleMapsCoords);
					}
				}
				if (GoogleMaps_Coords.Count < 1)
				{
					MessageBox.Show("No Observer sites to check site altitudes", "No altitudes", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
					return;
				}
				http.Get_GoogleEarth_Elevations(ref GoogleMaps_Coords);
				text2 = "      " + EventDetails.Year + " " + Utilities.ShortMonths[EventDetails.Month] + " " + EventDetails.Day + ",  (" + EventDetails.AsteroidNumber + ") " + EventDetails.AsteroidID + "\r\n\r\nSite altitude validation\r\n      Site Observer          Reported   Google \r\n        #  Name                  Alt      Alt     Diff\r\n";
				for (int j = 0; j < GoogleMaps_Coords.Count; j++)
				{
					int num = (int)Math.Abs(GoogleMaps_Coords[j].GE_Altitude - GoogleMaps_Coords[j].Altitude);
					if (num > 30)
					{
						text = "XXXX";
						flag = true;
						if (num > 100)
						{
							flag2 = true;
						}
					}
					else
					{
						text = ((num <= 10) ? ((num <= 5) ? "    " : " ?  ") : " ?? ");
					}
					string text3 = text + string.Format("{0,5}  ", GoogleMaps_Coords[j].SeqNum) + GoogleMaps_Coords[j].Observer.PadRight(16).Substring(0, 16) + string.Format("{0,7} m", GoogleMaps_Coords[j].Altitude) + string.Format("{0,7} m", GoogleMaps_Coords[j].GE_Altitude) + string.Format("{0,7} m", num);
					text2 = text2 + text3 + "\r\n";
				}
			}
			text2 += "\r\n'Approx mid-time (hh.h)' validation";
			double AverageTime = 0.0;
			text2 = ((!GetAverageTimeOfEvents(out AverageTime)) ? (text2 + "\r\n    No observations to set an 'Approx Mid-time' setting") : ((!(Math.Abs(AverageTime - EventDetails.MidT_forMotions) >= 0.2)) ? (text2 + "\r\n    'Approx Mid-time' setting is OK") : (text2 + string.Format("\r\n    'Approx Mid-time' setting should be = {0,1:f1} hrs\r\n     NOTE When you change the setting, you must also\r\n     update the Asteroid details", AverageTime))));
			if (SaveFile == "")
			{
				try
				{
					((Control)AltitudeCheck).Show();
				}
				catch
				{
					AltitudeCheck = new DisplayData();
					((Control)AltitudeCheck).set_Text("Site altitude validation");
					((Control)AltitudeCheck).Show();
				}
				((Control)AltitudeCheck).set_Width(450);
				((Control)AltitudeCheck.txtBox).set_Text(text2);
				((Control)cmdAltitude).set_ForeColor(SystemColors.ControlText);
				return;
			}
			if (flag)
			{
				using StreamWriter streamWriter = new StreamWriter(SaveFile + "_30.txt", append: true);
				streamWriter.Write(text2);
				streamWriter.WriteLine("--------------------------------------------------------------");
			}
			if (!flag2)
			{
				return;
			}
			using StreamWriter streamWriter2 = new StreamWriter(SaveFile + "_100.txt", append: true);
			streamWriter2.Write(text2);
			streamWriter2.WriteLine("--------------------------------------------------------------");
		}

		private static bool GetAverageTimeOfEvents(out double AverageTime)
		{
			int num = 0;
			AverageTime = 0.0;
			double num2 = 0.0;
			for (int i = 0; i < EventDetails.Observers.Count; i++)
			{
				if ("DdGg".Contains(EventDetails.Observers[i].Event_D) & !"xz".Contains(EventDetails.Observers[i].PlotCode))
				{
					num++;
					num2 += EventDetails.Observers[i].T_Disappear;
				}
				if ("RrBb".Contains(EventDetails.Observers[i].Event_R) & !"xy".Contains(EventDetails.Observers[i].PlotCode))
				{
					num++;
					num2 += EventDetails.Observers[i].T_Reappear;
				}
			}
			if (num < 1)
			{
				return false;
			}
			AverageTime = num2 / (double)num;
			return true;
		}

		private void txtCometNumber_Leave(object sender, EventArgs e)
		{
			((Control)txtCometNumber).set_Text(((Control)txtCometNumber).get_Text().ToUpper().Trim());
			((Control)txtCometName).set_Text(GetCometName(((Control)txtCometNumber).get_Text()));
		}

		internal bool validCometID(string CometID)
		{
			if (CometID.Length < 2)
			{
				return false;
			}
			CometID = CometID.Trim();
			if (CometID.EndsWith("P") | CometID.EndsWith("I"))
			{
				int.TryParse(CometID.Substring(0, CometID.Length - 1), out var result);
				return result > 0;
			}
			if (CometID.Contains("P/") | CometID.Contains("C/") | CometID.Contains("A/"))
			{
				try
				{
					int.TryParse(CometID.Substring(2, 4), out var result2);
					if (result2 < 1900 || result2 > 2040)
					{
						return false;
					}
					if (CometID.Length > 6)
					{
						if (CometID.Substring(6, 1) != " ")
						{
							return false;
						}
						int num = Convert.ToChar(CometID.Substring(7, 1));
						if (num < 65 || num > 90 || num == 73)
						{
							return false;
						}
					}
					return true;
				}
				catch
				{
					return false;
				}
			}
			return false;
		}

		internal string GetCometName(string CometID)
		{
			string result = "";
			bool flag = false;
			int num = 0;
			CometID = CometID.Trim();
			if (CometID.EndsWith("P") | CometID.EndsWith("I"))
			{
				flag = true;
			}
			if (validCometID(CometID) && File.Exists(Utilities.AppPath + "\\Downloaded Files\\Comet.dat"))
			{
				using (StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Downloaded Files\\Comet.dat"))
				{
					do
					{
						string text = streamReader.ReadLine()!.Substring(102, 50).Trim();
						if (text.Contains(CometID))
						{
							num = ((!flag) ? text.IndexOf("(") : text.IndexOf("/"));
							return text.Substring(num + 1).Replace(")", "").Trim();
						}
					}
					while (!streamReader.EndOfStream);
					return result;
				}
			}
			return result;
		}

		private void lstObservations_MouseUp(object sender, MouseEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Invalid comparison between Unknown and I4
			//IL_006d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0073: Invalid comparison between Unknown and I4
			//IL_00ef: Unknown result type (might be due to invalid IL or missing references)
			//IL_012c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0132: Invalid comparison between Unknown and I4
			if ((int)e.get_Button() != 2097152)
			{
				return;
			}
			if (EventDetails.Observers[((ListControl)lstObservations).get_SelectedIndex()].TelescopeType == "8")
			{
				if ((int)MessageBox.Show("Do you want to open the Light Curve Report form\r\nto report a light curve from a .csv file, for\r\n\r\nthe Unistellar observer:  " + EventDetails.Observers[((ListControl)lstObservations).get_SelectedIndex()].Observer1, "Confirm light curve & Observer", (MessageBoxButtons)1, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 1)
				{
					string text = "";
					string freeText = EventDetails.Observers[((ListControl)lstObservations).get_SelectedIndex()].FreeText;
					int num = freeText.IndexOf("SN ");
					if (num >= 0 && freeText.Length >= num + 6)
					{
						text = freeText.Substring(num + 3, 3);
						LightData.ShowLightCurveForm();
						LightData.LightCurveForm.OpenNewFile(text, ((ListControl)lstObservations).get_SelectedIndex(), IsLunar: false);
					}
					else
					{
						MessageBox.Show("No Unistellar scope ID in free text field", "No Unistellar scope SN", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
					}
				}
			}
			else if ((int)MessageBox.Show("Do you want to open the Light Curve Report form\r\nto report a light curve from a .csv file, for\r\n\r\n" + EventDetails.Observers[((ListControl)lstObservations).get_SelectedIndex()].Observer1, "Confirm light curve & Observer", (MessageBoxButtons)1, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 1)
			{
				LightData.ShowLightCurveForm();
				LightData.LightCurveForm.OpenNewFile("", ((ListControl)lstObservations).get_SelectedIndex(), IsLunar: false);
			}
		}

		private void chkRing_Click(object sender, EventArgs e)
		{
			chkRing.set_Checked(!chkRing.get_Checked());
			if (chkSatellite.get_Checked())
			{
				chkSatellite.set_Checked(false);
			}
			if (chkDoubleStar.get_Checked())
			{
				chkDoubleStar.set_Checked(false);
			}
		}

		private void chkDoubleStar_Click(object sender, EventArgs e)
		{
			chkDoubleStar.set_Checked(!chkDoubleStar.get_Checked());
			if (chkSatellite.get_Checked())
			{
				chkSatellite.set_Checked(false);
			}
			if (chkRing.get_Checked())
			{
				chkRing.set_Checked(false);
			}
		}

		private void starChartToolStripMenuItem_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)DisplayMPOccultations.Chart).Show();
			}
			catch
			{
				DisplayMPOccultations.Chart = new StarChart(SetCoords: false);
				((Control)DisplayMPOccultations.Chart).Show();
			}
			DisplayMPOccultations.Chart.CentreRA = EventDetails.RA_Star_2000;
			DisplayMPOccultations.Chart.CentreDec = EventDetails.Dec_Star_2000;
			DisplayMPOccultations.Chart.ShowObjectPath = false;
			DisplayMPOccultations.Chart.Use_Gaia = true;
			DisplayMPOccultations.Chart.SetSize_x50DegMag160();
			((Control)DisplayMPOccultations.Chart).Focus();
			DisplayMPOccultations.Chart.PlotChart();
		}

		private void chkSatellite_Click(object sender, EventArgs e)
		{
			chkSatellite.set_Checked(!chkSatellite.get_Checked());
			if (chkDoubleStar.get_Checked())
			{
				chkDoubleStar.set_Checked(false);
			}
			if (chkRing.get_Checked())
			{
				chkRing.set_Checked(false);
			}
		}

		private void txtObserver1_Leave(object sender, EventArgs e)
		{
			if (!Utilities.CheckForPipesAndNonASCII(((Control)txtObserver1).get_Text(), "Observer 1", CheckForPipes: true, out var RevisedText))
			{
				((Control)txtObserver1).set_Text(RevisedText);
				((Control)txtObserver1).Focus();
			}
		}

		private void txtObserver2_Leave(object sender, EventArgs e)
		{
			if (!Utilities.CheckForPipesAndNonASCII(((Control)txtObserver2).get_Text(), "Observer 2", CheckForPipes: true, out var RevisedText))
			{
				((Control)txtObserver2).set_Text(RevisedText);
				((Control)txtObserver2).Focus();
			}
		}

		private void mPCEditModeToolStripMenuItem_Click(object sender, EventArgs e)
		{
			mPCEditModeToolStripMenuItem.set_Checked(!mPCEditModeToolStripMenuItem.get_Checked());
			SetMPC_visibility();
			SetMPCtext();
		}

		private void SetMPC_visibility()
		{
			((Control)lblMPC).set_Visible(!mPCEditModeToolStripMenuItem.get_Checked());
			((Control)pnlMPC).set_Visible(mPCEditModeToolStripMenuItem.get_Checked());
		}

		private void txtMPCyear_Leave(object sender, EventArgs e)
		{
			int.TryParse(((Control)txtMPCyear).get_Text(), out var result);
			if ((((Control)txtMPCyear).get_Text().Trim().Length > 0) & ((result < 2023) | (result > DateTime.Now.Year)))
			{
				((Control)txtMPCyear).set_BackColor(Color.LightPink);
				((Control)txtMPCyear).set_Text(DateTime.Now.Year.ToString());
				((Control)txtMPCyear).Focus();
			}
			else
			{
				EventDetails.MPCDate = MPCdate();
				((Control)txtMPCyear).set_BackColor(SystemColors.Window);
			}
		}

		private void txtMPCmonth_Leave(object sender, EventArgs e)
		{
			int.TryParse(((Control)txtMPCmonth).get_Text(), out var result);
			if (((Control)txtMPCmonth).get_Text().Trim().Length > 0 && (result < 1 || result > 12))
			{
				((Control)txtMPCmonth).set_BackColor(Color.LightPink);
				((Control)txtMPCmonth).set_Text(DateTime.Now.Month.ToString());
				((Control)txtMPCmonth).Focus();
			}
			else
			{
				((Control)txtMPCmonth).set_BackColor(SystemColors.Window);
				EventDetails.MPCDate = MPCdate();
			}
		}

		private void txtMPCday_Leave(object sender, EventArgs e)
		{
			int.TryParse(((Control)txtMPCday).get_Text(), out var result);
			if (((Control)txtMPCday).get_Text().Trim().Length > 0 && (result < 1 || result > 31))
			{
				((Control)txtMPCday).set_BackColor(Color.LightPink);
				((Control)txtMPCday).set_Text(DateTime.Now.Day.ToString());
				((Control)txtMPCday).Focus();
			}
			else
			{
				((Control)txtMPCday).set_BackColor(SystemColors.Window);
				EventDetails.MPCDate = MPCdate();
			}
		}

		private string MPCdate()
		{
			string text = ((Control)txtMPCyear).get_Text().Trim().PadLeft(4)
				.Substring(0, 4)
				.Trim();
			if (text.Length == 2)
			{
				text = "20" + text;
			}
			else if (text.Length == 3)
			{
				text = "2" + text;
			}
			((Control)txtMPCyear).set_Text(text);
			string text2 = ((Control)txtMPCmonth).get_Text().Trim().PadLeft(2)
				.Substring(0, 2);
			((Control)txtMPCmonth).set_Text(text2.Trim());
			string text3 = ((Control)txtMPCday).get_Text().Trim().PadLeft(2)
				.Substring(0, 2);
			((Control)txtMPCday).set_Text(text3.Trim());
			if (((Control)lblLastSaveTime).get_ForeColor() != Color.Red)
			{
				StartTime = DateTime.Now;
			}
			((Control)lblLastSaveTime).set_ForeColor(Color.Red);
			return (text + text2 + text3).Trim();
		}

		private void txtMPEC_Leave(object sender, EventArgs e)
		{
			EventDetails.MPCNumber = ((Control)txtMPEC).get_Text().Trim();
			if (((Control)lblLastSaveTime).get_ForeColor() != Color.Red)
			{
				StartTime = DateTime.Now;
			}
			((Control)lblLastSaveTime).set_ForeColor(Color.Red);
		}

		private void txtMPC_ID_Leave(object sender, EventArgs e)
		{
			EventDetails.MPCsubmissionID = ((Control)txtMPC_ID).get_Text().Trim();
			if (((Control)lblLastSaveTime).get_ForeColor() != Color.Red)
			{
				StartTime = DateTime.Now;
			}
			((Control)lblLastSaveTime).set_ForeColor(Color.Red);
		}

		private void txtMPCyear_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()))
			{
				e.set_Handled(true);
			}
		}

		private void txtMPCmonth_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()))
			{
				e.set_Handled(true);
			}
		}

		private void txtMPCday_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()))
			{
				e.set_Handled(true);
			}
		}

		private void txtMPEC_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()) && e.get_KeyChar() != '-')
			{
				e.set_Handled(true);
			}
		}

		private void cmbYearRange_DrawItem(object sender, DrawItemEventArgs e)
		{
			e.DrawBackground();
			if (e.get_Index() >= 0)
			{
				int.TryParse(cmbYearRange.get_Items().get_Item(e.get_Index()).ToString()!.PadRight(4).Substring(0, 4), out var result);
				Graphics graphics = e.get_Graphics();
				Brush brush = new SolidBrush(e.get_ForeColor());
				Brush brush2 = new SolidBrush(e.get_BackColor());
				if ((result > 2017) & (e.get_BackColor().Name != "HighLight"))
				{
					brush = ((result % 2 != 0) ? new SolidBrush(Color.DarkRed) : new SolidBrush(Color.Navy));
					graphics.FillRectangle(brush2, e.get_Bounds());
					e.get_Graphics().DrawString(cmbYearRange.get_Items().get_Item(e.get_Index()).ToString(), e.get_Font(), brush, e.get_Bounds(), StringFormat.GenericDefault);
				}
				else
				{
					graphics.FillRectangle(brush2, e.get_Bounds());
					e.get_Graphics().DrawString(cmbYearRange.get_Items().get_Item(e.get_Index()).ToString(), e.get_Font(), brush, e.get_Bounds(), StringFormat.GenericDefault);
				}
				brush2.Dispose();
				brush.Dispose();
			}
			e.DrawFocusRectangle();
		}

		private void displaySubmittedLightCurvesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (LightCurveViewer == null)
			{
				LightCurveViewer = new LightCurveViewer();
			}
			if (EventDetails.AsteroidNo > 0)
			{
				LightCurveViewer.Display_PendingLightCurves(EventDetails.AsteroidNo, ShowMessage: true, ShowReadErrorMessages: false);
			}
		}

		private void txtLocatedNear_Leave(object sender, EventArgs e)
		{
			if (!Utilities.CheckForPipesAndNonASCII(((Control)txtLocatedNear).get_Text(), "Located near", CheckForPipes: true, out var RevisedText))
			{
				((Control)txtLocatedNear).set_Text(RevisedText);
				((Control)txtLocatedNear).Focus();
			}
		}

		private void cmdPredict_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.ShowSearch();
			NumericUpDown updnStartYear = DisplayMPOccultations.SearchForm.updnStartYear;
			decimal value;
			DisplayMPOccultations.SearchForm.updnEndYear.set_Value(value = decimal.Parse(((Control)txtYear).get_Text()));
			updnStartYear.set_Value(value);
			NumericUpDown updnStartMonth = DisplayMPOccultations.SearchForm.updnStartMonth;
			DisplayMPOccultations.SearchForm.updnEndMonth.set_Value(value = decimal.Parse(((Control)txtMonth).get_Text()));
			updnStartMonth.set_Value(value);
			NumericUpDown updnStartDay = DisplayMPOccultations.SearchForm.updnStartDay;
			DisplayMPOccultations.SearchForm.updnEndDay.set_Value(value = decimal.Parse(((Control)txtDay).get_Text()));
			updnStartDay.set_Value(value);
			((UpDownBase)DisplayMPOccultations.SearchForm.updnEndDay).UpButton();
			DisplayMPOccultations.SearchForm.chkUseHorizonsEphemeris.set_Checked(true);
			DisplayMPOccultations.SearchForm.updnMinimumDuration.set_Value(0.5m);
			DisplayMPOccultations.SearchForm.SelectAsteroid(((Control)txtAsteroidNumber).get_Text());
			try
			{
				DisplayMPOccultations.ListAndDisplay.chkStarID.set_Checked(false);
				((Control)DisplayMPOccultations.ListAndDisplay.lstSummary).set_BackColor(SystemColors.Window);
			}
			catch
			{
			}
			if (((Control)DisplayMPOccultations.SearchForm.chkHorizons).get_Enabled())
			{
				DisplayMPOccultations.SearchForm.Search();
				return;
			}
			DisplayMPOccultations.SearchForm.lstResults.get_Items().Clear();
			DisplayMPOccultations.SearchForm.lstResults.get_Items().Add((object)"To search for ");
			DisplayMPOccultations.SearchForm.lstResults.get_Items().Add((object)("   " + ((Control)DisplayMPOccultations.SearchForm.lblSearchRange).get_Text()));
			DisplayMPOccultations.SearchForm.lstResults.get_Items().Add((object)"click Search");
		}

		private void asteroidNumberFromDAMITToolStripMenuItem_Click(object sender, EventArgs e)
		{
			GetShapeModelData.ShowDAMITtoAsteroid();
		}

		private void txtStateCountry_Leave(object sender, EventArgs e)
		{
			((Control)txtStateCountry).set_Text(((Control)txtStateCountry).get_Text().ToUpper());
			if (!Utilities.CheckForPipesAndNonASCII(((Control)txtStateCountry).get_Text(), "State-Country", CheckForPipes: true, out var RevisedText))
			{
				((Control)txtStateCountry).set_Text(RevisedText.ToUpper());
				((Control)txtStateCountry).Focus();
			}
		}

		private void gaiaDoubleStarsnearbyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			GaiaDoubles.Elements.Show_GaiaDoubles();
			string text = Utilities.DEGtoDMS(EventDetails.RA_Star_2000 * (180.0 / Math.PI) / 15.0, 2, 2, MinutesOnly: false);
			string text2 = Utilities.DEGtoDMS(EventDetails.Dec_Star_2000 * (180.0 / Math.PI), 3, 1, MinutesOnly: false);
			((Control)GaiaDoubles.Elements.GaiaDoubles.txtRAh).set_Text(text.Substring(0, 2));
			((Control)GaiaDoubles.Elements.GaiaDoubles.txtRAm).set_Text(text.Substring(3, 2));
			((Control)GaiaDoubles.Elements.GaiaDoubles.txtRAs).set_Text(text.Substring(6, 5));
			((Control)GaiaDoubles.Elements.GaiaDoubles.txtDecD).set_Text(text2.Substring(0, 3));
			((Control)GaiaDoubles.Elements.GaiaDoubles.txtDecM).set_Text(text2.Substring(4, 2));
			((Control)GaiaDoubles.Elements.GaiaDoubles.txtDecS).set_Text(text2.Substring(7, 4));
			GaiaDoubles.Elements.GaiaDoubles.DisplayDoubles_Within15arcmin();
		}

		private void txtGaiaCoords_KeyPress(object sender, KeyPressEventArgs e)
		{
			e.set_Handled(!(char.IsDigit(e.get_KeyChar()) | char.IsControl(e.get_KeyChar()) | e.get_KeyChar().Equals('-') | e.get_KeyChar().Equals('+') | e.get_KeyChar().Equals('.')));
		}

		private void checkSubmittedFilesForOmissionsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0053: Unknown result type (might be due to invalid IL or missing references)
			//IL_0059: Expected O, but got Unknown
			//IL_00b1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b7: Invalid comparison between Unknown and I4
			string text = "Files with errors\r\n\r\n";
			int num = 0;
			try
			{
				((Control)ReportFileCheck).Show();
			}
			catch
			{
				ReportFileCheck = new DisplayData();
				((Control)ReportFileCheck).Show();
			}
			((Control)ReportFileCheck).set_Text("Check report on observation files");
			((Control)ReportFileCheck.txtBox).set_Text("");
			OpenFileDialog val = new OpenFileDialog();
			((FileDialog)val).set_Title("Specify the asteroidal observations files to be checked.");
			((FileDialog)val).set_Filter("XML file (*.xml)|*.XML|OBS file (*.obs)|*.obs|Text files (*.txt)|*.txt|All files (*.*)|*.*");
			((FileDialog)val).set_FilterIndex(Settings.Default.OBS_File_LastIndex);
			val.set_Multiselect(true);
			((FileDialog)val).set_FileName(Settings.Default.OBS_File_LastName);
			try
			{
				((FileDialog)val).set_InitialDirectory(Path.GetDirectoryName(Settings.Default.OBS_File_LastName));
			}
			catch
			{
			}
			if ((int)((CommonDialog)val).ShowDialog() != 1)
			{
				return;
			}
			Settings.Default.OBS_File_LastName = Current_OBS_File;
			Model_Parameters.Sort_0bysteroidNumber_1byModelNumberNumeric_2forISAM = 0;
			GetShapeModelData.DAMITModelsByAsteroidNumber.Sort();
			if (GetShapeModelData.ISAMmodelsByAsteroidNumber.Count == 0)
			{
				GetShapeModelData.Fill_ISAM_AsteroidModels();
			}
			GetShapeModelData.ISAMmodelsByAsteroidNumber.Sort();
			MultipleOBSFiles = ((FileDialog)val).get_FileNames();
			for (int i = 0; i < MultipleOBSFiles.Length; i++)
			{
				string text2 = CheckObservationFile(MultipleOBSFiles[i]);
				if (text2.Length > 0)
				{
					text = text + text2 + "\r\n";
					num++;
				}
			}
			text = text + num + " files with errors";
			((Control)ReportFileCheck.txtBox).set_Text(text);
			((Control)ReportFileCheck).Show();
		}

		private string CheckObservationFile(string ObservationFile)
		{
			int num = 0;
			int num2 = 0;
			string text = "";
			text = text + Path.GetFileName(ObservationFile) + "\r\n";
			string[] array;
			using (StreamReader streamReader = new StreamReader(ObservationFile))
			{
				array = streamReader.ReadToEnd().Split(new char[1] { '\n' });
			}
			for (int i = 0; i < array.Length; i++)
			{
				if (array[i].Contains("<EllipticFit>"))
				{
					string[] array2 = array[i].Split(new char[1] { '|' });
					if (array2[5] == "0")
					{
						text += " ..'Quality of the fit' may not have been set\r\n";
						num++;
					}
					break;
				}
			}
			for (int j = 0; j < array.Length; j++)
			{
				if (!array[j].Contains("<Asteroid>"))
				{
					continue;
				}
				string[] array2 = array[j].Replace("<Asteroid>", "").Split(new char[1] { '|' });
				int.TryParse(array2[0], out var result);
				if (result <= 0)
				{
					continue;
				}
				int num3 = 0;
				int num4 = GetShapeModelData.DAMITModelsByAsteroidNumber.Count - 1;
				bool flag = false;
				int num5;
				do
				{
					num5 = (num4 + num3) / 2;
					if (GetShapeModelData.DAMITModelsByAsteroidNumber[num5].AsteroidNumber == result)
					{
						flag = true;
						break;
					}
					if (GetShapeModelData.DAMITModelsByAsteroidNumber[num5].AsteroidNumber < result)
					{
						num3 = num5 + 1;
					}
					else
					{
						num4 = num5 - 1;
					}
				}
				while (num3 <= num4);
				if (flag)
				{
					for (int k = num5 - 6; k < num5 + 6; k++)
					{
						if (k < 0)
						{
							continue;
						}
						if (k > GetShapeModelData.DAMITModelsByAsteroidNumber.Count - 1)
						{
							break;
						}
						if (GetShapeModelData.DAMITModelsByAsteroidNumber[k].AsteroidNumber != result)
						{
							continue;
						}
						string modelNumber_String = GetShapeModelData.DAMITModelsByAsteroidNumber[k].ModelNumber_String;
						bool flag2 = false;
						for (int l = 0; l < array.Length; l++)
						{
							if (!array[l].Contains("<Fit>DAMIT"))
							{
								continue;
							}
							array2 = array[l].Split(new char[1] { '|' });
							if (modelNumber_String == array2[1])
							{
								flag2 = true;
								if (!ShapeModelLineIsOK(ref array2, out var ReportLine))
								{
									text += ReportLine;
									num++;
								}
								break;
							}
						}
						if (!flag2)
						{
							text = text + " ..Model " + modelNumber_String + " - has not been fitted\r\n";
							num++;
						}
					}
				}
				for (int m = 0; m < GetShapeModelData.ISAMmodelsByAsteroidNumber.Count; m++)
				{
					if (GetShapeModelData.ISAMmodelsByAsteroidNumber[m].AsteroidNumber != result)
					{
						continue;
					}
					string modelNumber_String = GetShapeModelData.ISAMmodelsByAsteroidNumber[m].ModelNumber_String;
					bool flag3 = false;
					for (int n = 0; n < array.Length; n++)
					{
						if (!array[n].Contains("<Fit>ISAM"))
						{
							continue;
						}
						array2 = array[n].Split(new char[1] { '|' });
						if (modelNumber_String == array2[1])
						{
							flag3 = true;
							if (!ShapeModelLineIsOK(ref array2, out var ReportLine2))
							{
								text += ReportLine2;
								num++;
							}
							break;
						}
					}
					if (!flag3)
					{
						text = text + " ..Model " + modelNumber_String + " - has not been fitted\r\n";
						num++;
					}
				}
			}
			if (Settings.Default.GoogleMaps_API_key.Length > 10)
			{
				GoogleMaps_Coords = new List<GoogleMapsCoords>();
				for (int num6 = 0; num6 < array.Length; num6++)
				{
					if (array[num6].Contains("<ID>"))
					{
						string[] array2 = array[num6].Trim().Replace(Tags.TagStart[21], "").Replace(Tags.TagEnd[21], "")
							.Split(new char[1] { '|' });
						GoogleMapsCoords googleMapsCoords = new GoogleMapsCoords();
						int.TryParse(array2[0], out var result2);
						googleMapsCoords.SeqNum = result2;
						googleMapsCoords.Observer = array2[1];
						googleMapsCoords.Longitude = Utilities.DMStoDeg(array2[6]);
						googleMapsCoords.Latitude = Utilities.DMStoDeg(" " + array2[7]);
						double.TryParse(array2[8], out var result3);
						googleMapsCoords.Altitude = (int)result3;
						GoogleMaps_Coords.Add(googleMapsCoords);
					}
				}
				http.Get_GoogleEarth_Elevations(ref GoogleMaps_Coords);
				for (int num7 = 0; num7 < GoogleMaps_Coords.Count; num7++)
				{
					if ((int)Math.Abs(GoogleMaps_Coords[num7].GE_Altitude - GoogleMaps_Coords[num7].Altitude) > 30)
					{
						text = text + string.Format(" ..Check site coords for {0,1}, ", GoogleMaps_Coords[num7].SeqNum) + GoogleMaps_Coords[num7].Observer + "\r\n";
						num++;
					}
				}
			}
			else
			{
				text += "No GoogleMaps API key; site locations not checked";
			}
			double result4 = 0.0;
			num2 = 0;
			double num8 = 0.0;
			for (int num9 = 0; num9 < array.Length; num9++)
			{
				int result5;
				int result6;
				int result7;
				if (array[num9].Contains("<Date>"))
				{
					string[] array2 = array[num9].Trim().Replace(Tags.TagStart[1], "").Replace(Tags.TagEnd[1], "")
						.Split(new char[1] { '|' });
					double.TryParse(array2[3], out result4);
				}
				else if (array[num9].Contains("<D>"))
				{
					string[] array2 = array[num9].Trim().Replace(Tags.TagStart[23], "").Replace(Tags.TagEnd[23], "")
						.Split(new char[1] { '|' });
					if (("DdGg".Contains(array2[1]) & !"xz".Contains(array2[5])) && (true & int.TryParse(array2[0].Substring(0, 2), out result5) & int.TryParse(array2[0].Substring(3, 2), out result6) & int.TryParse(array2[0].Substring(6, 2), out result7)))
					{
						num2++;
						num8 += (double)result5 + (double)result6 / 60.0 + (double)result7 / 3600.0;
					}
				}
				else if (array[num9].Contains("<R>"))
				{
					string[] array2 = array[num9].Trim().Replace(Tags.TagStart[24], "").Replace(Tags.TagEnd[24], "")
						.Split(new char[1] { '|' });
					if (("RrBb".Contains(array2[1]) & !"xy".Contains(array2[5])) && (true & int.TryParse(array2[0].Substring(0, 2), out result5) & int.TryParse(array2[0].Substring(3, 2), out result6) & int.TryParse(array2[0].Substring(6, 2), out result7)))
					{
						num2++;
						num8 += (double)result5 + (double)result6 / 60.0 + (double)result7 / 3600.0;
					}
				}
			}
			double num10 = num8 / (double)num2;
			if (Math.Abs(result4 - num10) > 0.2)
			{
				text += "\r\n'Approx mid-time (hh.h)' validation";
				if (Math.Abs(num10 - EventDetails.MidT_forMotions) >= 0.02)
				{
					text += string.Format("\r\n    'Approx Mid-time' setting should be = {0,1:f1} hrs\r\n     NOTE When you change the setting, you must also\r\n     update the Asteroid details", num10);
				}
				num++;
			}
			if (num > 0)
			{
				return text;
			}
			return "";
		}

		internal bool ShapeModelLineIsOK(ref string[] Fields, out string ReportLine)
		{
			ReportLine = "";
			if (Fields[3] == "0")
			{
				ReportLine = ReportLine + " ..Model " + Fields[1] + " 'Fit Quality' has not been set\r\n";
			}
			if ("3456".Contains(Fields[3]) & !double.TryParse(Fields[5], out var _))
			{
				ReportLine = ReportLine + " ..Model " + Fields[1] + " - 'Minimum diameter has not been set\r\n";
			}
			if ("456".Contains(Fields[3]) & !double.TryParse(Fields[6], out var _))
			{
				ReportLine = ReportLine + " ..Model " + Fields[1] + " - 'Maximum diameter has not been set\r\n";
			}
			return ReportLine.Length == 0;
		}

		private void observerVelocityRelativeToAsteriodToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Data_and_Plots.ShowObserverVelocities();
		}

		private void asteroidLightCurvesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Process.Start("https://alcdef.org/");
		}

		private void summaryDataToolStripMenuItem_Click(object sender, EventArgs e)
		{
			GetLightCurveData();
		}

		private void asteroidLightcurvePhotometryDatabaseToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Process.Start("https://alcdef.org/");
		}

		private void toolStripMenuItem7_Click(object sender, EventArgs e)
		{
			string text = "#" + ((Control)txtAsteroidNumber).get_Text().Trim().PadLeft(6, '0');
			Process.Start("https://obswww.unige.ch/~behrend/page1cou.html" + text);
		}

		private void toolStripMenuItem9_Click(object sender, EventArgs e)
		{
			string text = "#" + ((Control)txtAsteroidNumber).get_Text().Trim().PadLeft(6, '0');
			Process.Start("https://obswww.unige.ch/~behrend/page2cou.html" + text);
		}

		private void toolStripMenuItem10_Click(object sender, EventArgs e)
		{
			string text = "#" + ((Control)txtAsteroidNumber).get_Text().Trim().PadLeft(6, '0');
			Process.Start("https://obswww.unige.ch/~behrend/page3cou.html" + text);
		}

		private void toolStripMenuItem11_Click(object sender, EventArgs e)
		{
			string text = "#" + ((Control)txtAsteroidNumber).get_Text().Trim().PadLeft(6, '0');
			Process.Start("https://obswww.unige.ch/~behrend/page4cou.html" + text);
		}

		private void toolStripMenuItem13_Click(object sender, EventArgs e)
		{
			string text = "#" + ((Control)txtAsteroidNumber).get_Text().Trim().PadLeft(6, '0');
			Process.Start("https://obswww.unige.ch/~behrend/page5cou.html" + text);
		}

		private void cometsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			_ = "#" + ((Control)txtAsteroidNumber).get_Text().Trim().PadLeft(6, '0');
			Process.Start("https://obswww.unige.ch/~behrend/page6cou.html");
		}

		private void astrometryHeldByTheMinorPlanetCenterToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (EventDetails.AsteroidNo > 0)
			{
				DisplayMPOccultations.AsteroidNumber.ToString();
				Process.Start("https://www.minorplanetcenter.net/db_search/show_object?object_id=" + EventDetails.AsteroidNo);
			}
		}

		private void binaryAsteroidDetailsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			BinaryAsteroids.BinaryAsteroidDetails(EventDetails.AsteroidNo, EventDetails.AsteroidID);
		}

		private void binaryAsteroidsEstimateOrbitSizeOrPeriodToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Control)new OrbitSizes()).Show();
		}

		private void jPLSmallBodyDatabaseToolStripMenuItem_Click(object sender, EventArgs e)
		{
			string text = "";
			if (optAsteroid.get_Checked())
			{
				if (EventDetails.AsteroidNo > 0)
				{
					text = EventDetails.AsteroidNo.ToString();
				}
				else if (EventDetails.AsteroidNumber.Length > 0)
				{
					text = EventDetails.AsteroidNumber;
				}
			}
			else if (((Control)txtCometNumber).get_Text().Trim().Length > 1)
			{
				text = ((Control)txtCometNumber).get_Text().ToUpper().Trim();
			}
			Process.Start("https://ssd.jpl.nasa.gov/tools/sbdb_lookup.html#/?sstr=" + text);
		}

		private void fresnelDiffractionLightCurvesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Fresnel light curves");
		}

		private void starDiameterModelLightCurvesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			decimal num = 1.378795m;
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
			((Control)PredictionLightCurves.updn_masPERsec).set_Enabled(true);
			((Control)PredictionLightCurves.updnParallax).set_Enabled(false);
			((Control)PredictionLightCurves.chkTopoDistances).set_Enabled(true);
			PredictionLightCurves.updnParallax.set_Value((decimal)EventDetails.Parallax);
			PredictionLightCurves.updnAsteroidMag.set_Value((decimal)EventDetails.MvAsteroid);
			PredictionLightCurves.updnStarMag.set_Value((decimal)EventDetails.MgStar);
			((Control)PredictionLightCurves.updnParallax).set_Enabled(true);
			((Control)PredictionLightCurves.chkTopoDistances).set_Enabled(false);
			PredictionLightCurves.TopoKmRatio = 1f;
			PredictionLightCurves.updn_masPERsec.set_Value((decimal)(Math.Sqrt(EventDetails.dX * EventDetails.dX + EventDetails.dY * EventDetails.dY) * EventDetails.Parallax / 3.6));
			int num2 = Convert.ToInt32(Math.Atan2(EventDetails.dX, EventDetails.dY) * (180.0 / Math.PI));
			if (num2 < 0)
			{
				num2 += 360;
			}
			PredictionLightCurves.PAofPathDirection = num2;
			((Control)PredictionLightCurves.lblPathPA).set_Text(num2 + "°");
			PredictionLightCurves.DiameterChanging = true;
			PredictionLightCurves.updnAsteroidDiameter_mas.set_Value(Math.Round(PredictionLightCurves.updnAsteroidDia_km.get_Value() * num * PredictionLightCurves.updnParallax.get_Value() / 8.79414383618253m * 10m) / 10m);
			PredictionLightCurves.updnMinor_mas.set_Value(Math.Round(PredictionLightCurves.updnMinor_km.get_Value() * num * PredictionLightCurves.updnParallax.get_Value() / 8.79414383618253m * 10m) / 10m);
			PredictionLightCurves.DiameterChanging = false;
			PredictionLightCurves.ReSize();
			PredictionLightCurves.Plot(GenerateModel: true);
		}

		private void whenOpeningToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings @default = Settings.Default;
			ToolStripMenuItem obj = validateFilesWhenEventOrObserversPastedToolStripMenuItem;
			bool flag;
			whenOpeningToolStripMenuItem.set_Checked(flag = !whenOpeningToolStripMenuItem.get_Checked());
			bool gE_Alts_Auto;
			obj.set_Checked(gE_Alts_Auto = flag);
			@default.GE_Alts_Auto = gE_Alts_Auto;
		}

		private void validateFilesWhenEventOrObserversPastedToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings @default = Settings.Default;
			ToolStripMenuItem obj = validateFilesWhenEventOrObserversPastedToolStripMenuItem;
			bool flag;
			whenOpeningToolStripMenuItem.set_Checked(flag = !validateFilesWhenEventOrObserversPastedToolStripMenuItem.get_Checked());
			bool gE_Alts_Auto;
			obj.set_Checked(gE_Alts_Auto = flag);
			@default.GE_Alts_Auto = gE_Alts_Auto;
		}

		private void cOMPILATIONModeToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0034: Unknown result type (might be due to invalid IL or missing references)
			//IL_003a: Invalid comparison between Unknown and I4
			//IL_0054: Unknown result type (might be due to invalid IL or missing references)
			//IL_005a: Invalid comparison between Unknown and I4
			if (dELETIONModeToolStripMenuItem.get_Checked())
			{
				return;
			}
			string text = "You have requested Compilation mode\r\n\r\nIn this mode, events can be added to a file of \r\nobservations that can be imported (by someone else)\r\nInto the main historical file of observations, without\r\nfurther review.\r\n\r\nA file name must be specified. That file will be written a file in\r\nthe subdirectory '/Import_Export', with a name structured as\r\n\r\n   Qchecked_xxxx.xml, with the xxxx being user specified.\r\n\r\nWhile in this mode, you can undertake all the usual functions\r\nof the editor - except opening an event from the\r\nHistorical file.\r\n\r\nAdding an event to the Historical file is replaced with \r\nadding the event to this Compilation file.\r\n\r\nIt is important to change the file name for this function\r\nafter you send a compilation file for inclusion, to avoid\r\nmultiple copies of an event being submitted for inclusion\r\n\r\nDo you wish to continue?";
			if (!cOMPILATIONModeToolStripMenuItem.get_Checked())
			{
				if ((int)MessageBox.Show(text, "Compilation mode", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 6)
				{
					return;
				}
			}
			else if ((int)MessageBox.Show("Do you want to exit Compilation mode?", "End Compilation mode", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			cOMPILATIONModeToolStripMenuItem.set_Checked(!cOMPILATIONModeToolStripMenuItem.get_Checked());
			GroupBox obj = grpHistory;
			bool enabled;
			((ToolStripItem)dELETIONModeToolStripMenuItem).set_Enabled(enabled = !cOMPILATIONModeToolStripMenuItem.get_Checked());
			((Control)obj).set_Enabled(enabled);
			ToolStripMenuItem obj2 = setCompilationFileNameToolStripMenuItem;
			ToolStripMenuItem obj3 = fileNameToolStripMenuItem;
			Button obj4 = cmdAddToCompiled;
			bool @checked;
			((ToolStripItem)setCompilationFileNameToolStripMenuItem).set_Enabled(@checked = cOMPILATIONModeToolStripMenuItem.get_Checked());
			bool flag;
			((Control)obj4).set_Visible(flag = @checked);
			((ToolStripItem)obj3).set_Visible(enabled = flag);
			((ToolStripItem)obj2).set_Visible(enabled);
			if (!cOMPILATIONModeToolStripMenuItem.get_Checked())
			{
				((Control)this).set_BackColor(SystemColors.Control);
			}
			else
			{
				((Control)this).set_BackColor(Color.FloralWhite);
			}
		}

		private void starDiameterFromVizieRToolStripMenuItem_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)GaiaStarDiameter).Show();
			}
			catch
			{
				GaiaStarDiameter = new Star_Diameter_from_Gaia();
				((Control)GaiaStarDiameter).Show();
			}
			((Control)GaiaStarDiameter.txtGaiaID).set_Text(EventDetails.Gaia_ID);
		}

		private void setCompilationFileNameToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_000f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0015: Invalid comparison between Unknown and I4
			SetCompileNames = new SetCompileFileNames();
			if ((int)((Form)SetCompileNames).ShowDialog() == 1)
			{
				Quality_FileName = Settings.Default.QualityFileName;
				((ToolStripItem)fileNameToolStripMenuItem).set_Text("File name :  Quality_" + Quality_FileName + ".xml");
			}
			((Form)SetCompileNames).Close();
			((Component)(object)SetCompileNames).Dispose();
		}

		private void loadSaveObserverDetailsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0005: Unknown result type (might be due to invalid IL or missing references)
			((Form)new StandardSites()).ShowDialog();
		}

		private void cmdAutoCompleteObserver_Click(object sender, EventArgs e)
		{
			//IL_0005: Unknown result type (might be due to invalid IL or missing references)
			((Form)new StandardSites()).ShowDialog();
		}

		private void eventDateStarAsteroidOccelmntPredictionXmlToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_04eb: Unknown result type (might be due to invalid IL or missing references)
			string[] array = Clipboard.GetText((TextDataFormat)0).Replace("\r", "").Split(new char[1] { '\n' });
			for (int i = 0; i < array.Length; i++)
			{
				if (array[i].Contains("<Elements>"))
				{
					string[] array2 = array[i].Trim().Split(new char[1] { ',' });
					((Control)txtYear).set_Text(array2[2]);
					((Control)txtMonth).set_Text(array2[3]);
					((Control)txtDay).set_Text(array2[4]);
					((Control)txtMidT).set_Text(string.Format("{0,1:f1}", double.Parse(array2[5])));
				}
				else if (array[i].Contains("<Object>"))
				{
					string[] array3 = array[i].Trim().Split(new char[1] { ',' });
					int num = array3[0].IndexOf(">");
					string text = array3[0].Substring(num + 1);
					if (int.TryParse(text, out var result) && result > 0)
					{
						optAsteroid.set_Checked(true);
						((Control)txtAsteroidNumber).set_Text(text);
						((Control)txtAsteroidName).set_Text(array3[1]);
					}
					else if (text.Substring(0, 1) == "P")
					{
						optPlanet.set_Checked(true);
						int.TryParse(text.Substring(1, 1), out var result2);
						((ListControl)cmbPlanets).set_SelectedIndex(result2 - Convert.ToInt16(result2 > 3));
						Application.DoEvents();
						int.TryParse(text.Substring(3), out var result3);
						if (result2 == 6 && result3 > 9)
						{
							((ListControl)cmbMoons).set_SelectedIndex(result3 - 2);
						}
						else
						{
							((ListControl)cmbMoons).set_SelectedIndex(result3);
						}
					}
					else if (array3[1].EndsWith("P") | array3[1].Contains("P/") | array3[1].Contains("C/") | array3[1].Contains("A/"))
					{
						optComets.set_Checked(true);
						int num2 = array3[1].IndexOf("(");
						int num3 = array3[1].IndexOf(")");
						array3[1].IndexOf("P");
						int num4 = array3[1].IndexOf("P/");
						if (num2 >= 0 && num3 > num2)
						{
							((Control)txtCometNumber).set_Text(array3[1].Substring(num2 + 1, num3 - num2 - 1));
							((Control)txtCometName).set_Text(array3[1].Substring(0, num2).Trim());
						}
						else if (num4 <= 0)
						{
							((Control)txtCometNumber).set_Text(array3[1]);
							((Control)txtCometName).set_Text("");
						}
						else if (num4 > 0)
						{
							((Control)txtCometNumber).set_Text(array3[1].Substring(0, num4 + 1));
							((Control)txtCometName).set_Text(array3[1].Substring(num4 + 2).Trim());
						}
					}
				}
				else if (array[i].Contains("<Star>"))
				{
					int num5 = array[i].IndexOf(">");
					int num6 = array[i].IndexOf(",");
					string[] array4 = array[i].Substring(num5 + 1, num6 - num5 - 1).Split(new char[1] { ' ' });
					string[] array5 = array4[1].Split(new char[1] { '-' });
					if (array4[0] == "HIP")
					{
						((ListControl)cmbCatalogue).set_SelectedIndex(0);
						((Control)txtHip).set_Text(array5[0]);
					}
					else if (array4[0] == "TYC")
					{
						((ListControl)cmbCatalogue).set_SelectedIndex(1);
						((Control)txtTycRegion).set_Text(array5[0]);
						((Control)txtTycSeqNum).set_Text(array5[1]);
						((Control)txtTycComp).set_Text(array5[2]);
					}
					else if (array4[0] == "UCAC4")
					{
						((ListControl)cmbCatalogue).set_SelectedIndex(2);
						((Control)txtU4Zone).set_Text(array5[0]);
						((Control)txtU4Number).set_Text(array5[1]);
					}
					else if (array4[0] == "USNO-B1")
					{
						((ListControl)cmbCatalogue).set_SelectedIndex(3);
						((Control)txtB1zone).set_Text(array5[0]);
						((Control)txtB1number).set_Text(array5[1]);
					}
					else if (array4[0] == "NOMAD")
					{
						((ListControl)cmbCatalogue).set_SelectedIndex(4);
						((Control)txtNOMADzone).set_Text(array5[0]);
						((Control)txtNOMADnumber).set_Text(array5[1]);
					}
					else if (array4[0] == "J-coords")
					{
						((ListControl)cmbCatalogue).set_SelectedIndex(5);
						((Control)txtGaiaCoords).set_Text(array5[0]);
					}
				}
			}
			GetAsteroidPosition(UseExistingStarPosition: false);
			MessageBox.Show("****  IMPORTANT  ****\r\n\r\nAfter you have added the observers, you need to:\r\n\r\n(i) set the Approximate MidTime to the mid-time of the\r\nactual observations, then\r\n\r\n(ii) click the 'Get Details' button in the Asteroid group.", "Set mid-time", (MessageBoxButtons)0, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
		}

		private void cmdAddToCompiled_Click(object sender, EventArgs e)
		{
			//IL_002b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0031: Invalid comparison between Unknown and I4
			//IL_0054: Unknown result type (might be due to invalid IL or missing references)
			//IL_007e: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a3: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a9: Invalid comparison between Unknown and I4
			//IL_00cb: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d1: Invalid comparison between Unknown and I4
			//IL_00f3: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f9: Invalid comparison between Unknown and I4
			//IL_016d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0173: Invalid comparison between Unknown and I4
			//IL_01a1: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a7: Invalid comparison between Unknown and I4
			//IL_01c6: Unknown result type (might be due to invalid IL or missing references)
			//IL_01cc: Invalid comparison between Unknown and I4
			//IL_0341: Unknown result type (might be due to invalid IL or missing references)
			//IL_0347: Invalid comparison between Unknown and I4
			//IL_03ba: Unknown result type (might be due to invalid IL or missing references)
			//IL_03e8: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ee: Invalid comparison between Unknown and I4
			//IL_045e: Unknown result type (might be due to invalid IL or missing references)
			//IL_04a3: Unknown result type (might be due to invalid IL or missing references)
			if ((int)MessageBox.Show("This will add the data for the currently displayed event to\r\nthe COMPILATION file.\r\n\r\n    " + Quality_FileNameBase + Quality_FileName + ".xml\r\n\r\nBefore adding the event, the Sites are plotted in GoogleEarth\r\nto confirm the site locations are correct.\r\n\r\nDo you wish to continue?", "Adding data", (MessageBoxButtons)1, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 2)
			{
				return;
			}
			if (EventDetails.Observers.Count < 1)
			{
				MessageBox.Show("There are no observations to be added to the Compilation File", "No observations", (MessageBoxButtons)0, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return;
			}
			if (EventDetails.AsteroidNominalDiameter < 0.05)
			{
				MessageBox.Show("The diameter of the asteroid has not been set\r\n\r\nYou will need to click 'Get details' in the Asteroid box", "No diameter", (MessageBoxButtons)0, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return;
			}
			if (EventDetails.Quality == 0)
			{
				if ((int)MessageBox.Show("The Quality rating is set as 'No reliable position or size'.\r\n\r\nDo you want to continue with this rating?", "Has quality rating been set?", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
				{
					return;
				}
			}
			else if (EventDetails.Quality == 5)
			{
				if ((int)MessageBox.Show("The Quality rating is set as 'Short duration event. No astrometry'.\r\n\r\nDo you want to continue with this rating?", "Has quality rating been correctly set?", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
				{
					return;
				}
			}
			else if (EventDetails.Quality == 6 && (int)MessageBox.Show("The Quality rating is set as 'Only 1 star of a double. No astrometry'.\r\n\r\nDo you want to continue with this rating?", "Has quality rating been correctly set?", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			if ((EventDetails.ShapeData.Count != Data_and_Plots.NumberOfShapeModels() && (int)MessageBox.Show("There are " + (Data_and_Plots.NumberOfShapeModels() - EventDetails.ShapeData.Count) + " / " + Data_and_Plots.NumberOfShapeModels() + " shape models that have not been fitted to this event.\r\n\r\nDo you want to continue without fitting all shape models?", "Shape models have not been fitted", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7) || (CheckIfInHistoricalFile(out var ObserverNames) && (int)MessageBox.Show("An entry for this Asteroid, Star and Date already exists,\r\nin the Historical file, with Observers: \r\n" + ObserverNames + "\r\n\r\nDo you want to add it to the Compilation file?", "Duplicate entry", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7))
			{
				return;
			}
			PlotSitesInGoogleEarth();
			if ((int)MessageBox.Show("The sites are being plotted in GoogleEarth.\r\n\r\nAre the site locations OK?", "GoogleEarth site check", (MessageBoxButtons)1, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 2)
			{
				return;
			}
			using (StreamWriter streamWriter = new StreamWriter(QualityFileRoot + Quality_FileNameBase + Quality_FileName + ".xml", append: true))
			{
				Data_and_Plots.ComputeSolution_and_UpDate_EventDetails();
				((Control)this).Focus();
				EventDetails.YearAdded = DateTime.Today.ToUniversalTime().Year;
				EventDetails.MonthAdded = DateTime.Today.ToUniversalTime().Month;
				EventDetails.DayAdded = DateTime.Today.ToUniversalTime().Day;
				AllEvents allEvents = new AllEvents();
				allEvents.EncodeAnEvent_inXML(NewEvent: true, 0);
				streamWriter.WriteLine(Tags.TagFileStart);
				streamWriter.WriteLine("  <FileVersion>" + Tags.ObservationsFileVersion + "</FileVersion>");
				for (int i = 0; i < allEvents.OccEvents.Count; i++)
				{
					for (int j = 0; j < allEvents.OccEvents[i].Lines.Count; j++)
					{
						streamWriter.WriteLine(allEvents.OccEvents[i].Lines[j]);
					}
				}
				streamWriter.WriteLine(Tags.TagFileEnd);
				((Control)lblLastSaveTime).set_ForeColor(Color.Green);
				StartTime = DateTime.Now;
			}
			if (EventLoadedFromFile)
			{
				if ((int)MessageBox.Show("Do you want to move the source file:\r\n\r\n   " + Current_OBS_File + "\r\n\r\nto the 'Processed' subdirectory of the file's location?", "Move source file", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6)
				{
					string text = Path.GetDirectoryName(Current_OBS_File) + "\\Processed";
					string destFileName = text + "\\" + Path.GetFileName(Current_OBS_File);
					try
					{
						if (!Directory.Exists(text))
						{
							Directory.CreateDirectory(text);
						}
						File.Move(Current_OBS_File, destFileName);
					}
					catch
					{
						MessageBox.Show("The file\r\n  " + Current_OBS_File + "has not been moved", "Move error", (MessageBoxButtons)0, (MessageBoxIcon)16);
					}
				}
				else if ((int)MessageBox.Show("Do you want to move the source file:\r\n\r\n   " + Current_OBS_File + "\r\n\r\nto the 'Not added' subdirectory of the file's location?", "Move source file", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6)
				{
					string text = Path.GetDirectoryName(Current_OBS_File) + "\\Not added";
					string destFileName = text + "\\" + Path.GetFileName(Current_OBS_File);
					try
					{
						if (!Directory.Exists(text))
						{
							Directory.CreateDirectory(text);
						}
						File.Move(Current_OBS_File, destFileName);
					}
					catch
					{
						MessageBox.Show("The file\r\n  " + Current_OBS_File + "has not been moved", "Move error", (MessageBoxButtons)0, (MessageBoxIcon)16);
					}
				}
				EventLoadedFromFile = false;
			}
			if (ReadingMultipleOBSFiles)
			{
				ReadNextMultipleFile();
			}
			else
			{
				MessageBox.Show("Event added to " + Quality_FileNameBase + Quality_FileName + ".xml", "Event added", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
			}
		}

		private void asteroidmagsFromGAIAToolStripMenuItem_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)GaiaMagMeasures).Show();
			}
			catch
			{
				GaiaMagMeasures = new GaiaMagMeasures();
				((Control)GaiaMagMeasures).Show();
			}
			((Control)GaiaMagMeasures.txtAsteroid).set_Text(EventDetails.AsteroidNo.ToString());
			if (MagValues.ColorMagnitudeData.Count < 10)
			{
				MagValues.ReadMagnitudeFile();
			}
			if (MagValues.ColorMagnitudeData.Count <= 0)
			{
				return;
			}
			int colorMagnitudeRecord = MagValues.GetColorMagnitudeRecord(EventDetails.AsteroidNo);
			if (colorMagnitudeRecord >= 0)
			{
				if (MagValues.ColorMagnitudeData[colorMagnitudeRecord].SHG1G2_R > 0.0)
				{
					((Control)GaiaMagMeasures).set_Width(880);
					((Control)GaiaMagMeasures.txtPoleRA).set_Text(string.Format("{0,1:f1}", MagValues.ColorMagnitudeData[colorMagnitudeRecord].SHG1G2_alpha));
					((Control)GaiaMagMeasures.txtPoleDec).set_Text(string.Format("{0,1:f1}", MagValues.ColorMagnitudeData[colorMagnitudeRecord].SHG1G2_delta));
					((Control)GaiaMagMeasures.txtOblate).set_Text(string.Format("{0,1:f2}", MagValues.ColorMagnitudeData[colorMagnitudeRecord].SHG1G2_R));
					((Control)GaiaMagMeasures.txtRA).set_Text(string.Format("{0,1:f1}", EventDetails.RA_Star_2000 * (180.0 / Math.PI)));
					((Control)GaiaMagMeasures.txtDec).set_Text(string.Format("{0,1:f1}", EventDetails.Dec_Star_2000 * (180.0 / Math.PI)));
					Utilities.PoleOrientation(MagValues.ColorMagnitudeData[colorMagnitudeRecord].SHG1G2_alpha, MagValues.ColorMagnitudeData[colorMagnitudeRecord].SHG1G2_delta, EventDetails.RA_Star_2000, EventDetails.Dec_Star_2000, out var Planetocentric_Latitude_deg, out var PAPole_deg);
					((Control)GaiaMagMeasures.txtPA1).set_Text(string.Format("{0,1:f0}", PAPole_deg));
					((Control)GaiaMagMeasures.txtLat1).set_Text(string.Format("{0,1:f0}", Planetocentric_Latitude_deg));
					GaiaMagMeasures.PA = PAPole_deg;
					GaiaMagMeasures.Lat = Planetocentric_Latitude_deg;
					GaiaMagMeasures.Oblate = (float)MagValues.ColorMagnitudeData[colorMagnitudeRecord].SHG1G2_R;
					GaiaMagMeasures.PlotAsteroid();
				}
				else
				{
					((Control)GaiaMagMeasures).set_Width(645);
					TextBox txtPoleRA = GaiaMagMeasures.txtPoleRA;
					TextBox txtPoleDec = GaiaMagMeasures.txtPoleDec;
					TextBox txtOblate = GaiaMagMeasures.txtOblate;
					TextBox txtRA = GaiaMagMeasures.txtRA;
					TextBox txtDec = GaiaMagMeasures.txtDec;
					TextBox txtPA = GaiaMagMeasures.txtPA1;
					string text;
					((Control)GaiaMagMeasures.txtLat1).set_Text(text = "---");
					string text2;
					((Control)txtPA).set_Text(text2 = text);
					string text3;
					((Control)txtDec).set_Text(text3 = text2);
					string text4;
					((Control)txtRA).set_Text(text4 = text3);
					string text5;
					((Control)txtOblate).set_Text(text5 = text4);
					string text6;
					((Control)txtPoleDec).set_Text(text6 = text5);
					((Control)txtPoleRA).set_Text(text6);
				}
			}
		}

		private void cmdCloseExpandedFreeText_Click(object sender, EventArgs e)
		{
			TextFree_Leave();
		}

		private void TextFree_Leave()
		{
			((TextBoxBase)txtFree).set_Multiline(false);
			((Control)txtFree).set_Height(20);
			((Control)txtFree).set_Top(154);
			txtFree.set_ScrollBars((ScrollBars)0);
			((Control)lblFreeLeft).set_Top(141);
			((Control)lstObservations).set_Enabled(true);
			((Control)cmdCloseExpandedFreeText).set_Visible(false);
			((Control)txtFree).set_Font(new Font(new FontFamily("Microsoft Sans Serif"), 8.25f, FontStyle.Regular));
			((TextBoxBase)txtFree).set_SelectionStart(0);
			if (!Utilities.CheckForPipesAndNonASCII(((Control)txtFree).get_Text(), "Free text", CheckForPipes: true, out var RevisedText))
			{
				((Control)txtFree).set_Text(RevisedText);
				((Control)txtFree).Focus();
			}
		}

		private void convertObsToolStripMenuItem_Click(object sender, EventArgs e)
		{
		}

		private void updateSolutionsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_000b: Unknown result type (might be due to invalid IL or missing references)
			MessageBox.Show("Make sure the Asteroid~Observations.xml file is not open in any other software before the end of the update processing. Otherwise an error may occur when the updated file needs to be written at the end of the updates", "Close copies", (MessageBoxButtons)0);
			PBar pBar = new PBar();
			pBar.pBarFTP.set_Minimum(0);
			pBar.pBarFTP.set_Maximum(Data_and_Plots.Historical_AllEvents.OccEvents.Count + 1);
			((Control)pBar).Show();
			UpdatingOldFile = true;
			for (int i = 0; i < Data_and_Plots.Historical_AllEvents.OccEvents.Count; i++)
			{
				pBar.pBarFTP.set_Value(i);
				Data_and_Plots.Historical_AllEvents.DecodeAnEvent_Into_EventDetails(i, out var _, out var _);
				DisplayEventInEditor();
				((Control)Data_and_Plots.PlotForm).Focus();
				((Control)this).Focus();
				Data_and_Plots.Historical_AllEvents.EncodeAnEvent_inXML(NewEvent: false, i);
			}
			UpdatingOldFile = false;
			Data_and_Plots.Historical_AllEvents.WriteHistoricalObservationsFile(HistoricalFile: true, "");
			((Form)pBar).Close();
		}

		private void getEventsWithAssumedUncertsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			PBar pBar = new PBar();
			pBar.pBarFTP.set_Minimum(0);
			pBar.pBarFTP.set_Maximum(Data_and_Plots.Historical_AllEvents.OccEvents.Count + 1);
			((Control)pBar).Show();
			using (StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Generated files\\Uncerts.csv"))
			{
				streamWriter.WriteLine("Date, Asteroid,Total, #Uncert, derived uncert");
				for (int i = 0; i < Data_and_Plots.Historical_AllEvents.OccEvents.Count; i++)
				{
					pBar.pBarFTP.set_Value(i);
					Data_and_Plots.Historical_AllEvents.DecodeAnEvent_Into_EventDetails(i, out var _, out var PrimaryChords);
					DisplayEventInEditor();
					((Control)Data_and_Plots.PlotForm).Focus();
					((Control)this).Focus();
					int TotalEvents;
					double AcrossPathUncertainty_mas;
					int NoUncert;
					double NTP_Phone_Offset;
					double eventTimeUncertainty_secs = Data_and_Plots.GetEventTimeUncertainty_secs(UseSatellite: false, out TotalEvents, out AcrossPathUncertainty_mas, out NoUncert, out NTP_Phone_Offset);
					StringBuilder stringBuilder = new StringBuilder();
					stringBuilder.AppendFormat("{0}-{1}-{2,0:f1},", EventDetails.Year, EventDetails.Month, EventDetails.Day);
					PrimaryChords = EventDetails.AsteroidNo;
					stringBuilder.AppendFormat("{0},", "(" + PrimaryChords + ") " + EventDetails.AsteroidID);
					stringBuilder.AppendFormat("{0},{1},{2,1:f2} s", TotalEvents, NoUncert, eventTimeUncertainty_secs);
					streamWriter.WriteLine(stringBuilder.ToString());
				}
			}
			((Form)pBar).Close();
		}

		private void updateAsteroidFitUncertaintiesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_000b: Unknown result type (might be due to invalid IL or missing references)
			MessageBox.Show("Make sure the Asteroid~Observations.xml file is not open in any other software before the end of the update processing. Otherwise an error may occur when the updated file needs to be written at the end of the updates", "Close copies", (MessageBoxButtons)0);
			PBar pBar = new PBar();
			pBar.pBarFTP.set_Minimum(0);
			pBar.pBarFTP.set_Maximum(Data_and_Plots.Historical_AllEvents.OccEvents.Count + 1);
			((Control)pBar).Show();
			using (new StreamWriter(Utilities.AppPath + "\\Generated Files\\NotUpdatedStars.txt"))
			{
				UpdatingOldFile = true;
				for (int i = 0; i < Data_and_Plots.Historical_AllEvents.OccEvents.Count; i++)
				{
					pBar.pBarFTP.set_Value(i);
					Data_and_Plots.Historical_AllEvents.DecodeAnEvent_Into_EventDetails(i, out var _, out var _);
					DisplayEventInEditor();
					((Control)Data_and_Plots.PlotForm).Focus();
					((Control)Data_and_Plots.Observations_Editor).Focus();
					Data_and_Plots.Historical_AllEvents.EncodeAnEvent_inXML(NewEvent: false, i);
				}
				UpdatingOldFile = false;
				Data_and_Plots.Historical_AllEvents.WriteHistoricalObservationsFile(HistoricalFile: true, "");
			}
			((Form)pBar).Close();
		}

		private void updateRUWEToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0032: Unknown result type (might be due to invalid IL or missing references)
			string[] array = new string[4] { "-----   ", "High RUWE   ", "Duplicate source   ", "Both " };
			MessageBox.Show("Make sure the Asteroid~Observations.xml file is not open in any other software before the end of the update processing. Otherwise an error may occur when the updated file needs to be written at the end of the updates", "Close copies", (MessageBoxButtons)0);
			PBar pBar = new PBar();
			pBar.pBarFTP.set_Minimum(0);
			pBar.pBarFTP.set_Maximum(Data_and_Plots.Historical_AllEvents.OccEvents.Count + 1);
			((Control)pBar).Show();
			File.Delete(Utilities.AppPath + "\\Generated Files\\RUWE changes.txt");
			using (StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Generated Files\\RUWE changes.txt"))
			{
				streamWriter.WriteLine("RUWE <1.4 changes; Duplicate changes ");
			}
			for (int i = 0; i < Data_and_Plots.Historical_AllEvents.OccEvents.Count; i++)
			{
				pBar.pBarFTP.set_Value(i);
				Data_and_Plots.Historical_AllEvents.DecodeAnEvent_Into_EventDetails(i, out var _, out var PrimaryChords);
				int month = EventDetails.Month;
				int day = EventDetails.Day;
				if (!Gaia_ESA.GetGaiaRUWE(EventDetails.JD_EventDate, EventDetails.RA_Star_2000 * (180.0 / Math.PI), EventDetails.Dec_Star_2000 * (180.0 / Math.PI), EventDetails.MgStar, out var RUWE, out var Issues))
				{
					continue;
				}
				string[] obj = new string[9] { "Fix issues - ", null, null, null, null, null, null, null, null };
				PrimaryChords = EventDetails.Year;
				obj[1] = PrimaryChords.ToString();
				obj[2] = " ";
				obj[3] = month.ToString().PadLeft(2);
				obj[4] = " ";
				obj[5] = day.ToString().PadLeft(2);
				obj[6] = "  RUWE = ";
				obj[7] = string.Format("{0,4:f3}", RUWE);
				obj[8] = "    No change";
				((Control)pBar).set_Text(string.Concat(obj));
				Application.DoEvents();
				if (RUWE > 0.0 && Issues != EventDetails.IssuesFlag)
				{
					string[] array2 = new string[5];
					PrimaryChords = EventDetails.Year;
					array2[0] = PrimaryChords.ToString();
					array2[1] = " ";
					array2[2] = month.ToString().PadLeft(2);
					array2[3] = " ";
					array2[4] = day.ToString().PadLeft(2);
					string text = string.Concat(array2);
					((Control)pBar).set_Text("Fix issues - " + text + "  RUWE = " + string.Format("{0,4:f3}", RUWE) + " --- " + EventDetails.AsteroidID);
					using (StreamWriter streamWriter2 = new StreamWriter(Utilities.AppPath + "\\Generated Files\\RUWE changes.txt", append: true))
					{
						streamWriter2.WriteLine(text + "  RUWE = " + string.Format("{0,4:f3}", RUWE) + " --- " + EventDetails.AsteroidID.PadRight(16) + "  Old = " + array[EventDetails.IssuesFlag].PadRight(15) + "   New = " + array[Issues]);
					}
					EventDetails.IssuesFlag = Issues;
					Data_and_Plots.Historical_AllEvents.EncodeAnEvent_inXML(NewEvent: false, i);
				}
			}
			Data_and_Plots.Historical_AllEvents.WriteHistoricalObservationsFile(HistoricalFile: true, "");
			((Form)pBar).Close();
		}

		private void updateStarPositionsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_000b: Unknown result type (might be due to invalid IL or missing references)
			MessageBox.Show("Make sure the Asteroid~Observations.xml file is not open in any other software before the end of the update processing. Otherwise an error may occur when the updated file needs to be written at the end of the updates", "Close copies", (MessageBoxButtons)0);
			PBar pBar = new PBar();
			pBar.pBarFTP.set_Minimum(0);
			pBar.pBarFTP.set_Maximum(Data_and_Plots.Historical_AllEvents.OccEvents.Count + 1);
			((Control)pBar).Show();
			using (new StreamWriter(Utilities.AppPath + "\\Generated Files\\NotUpdatedStars.txt"))
			{
				UpdatingOldFile = true;
				for (int i = 0; i < Data_and_Plots.Historical_AllEvents.OccEvents.Count; i++)
				{
					pBar.pBarFTP.set_Value(i);
					Data_and_Plots.Historical_AllEvents.DecodeAnEvent_Into_EventDetails(i, out var _, out var _);
					DisplayEventInEditor();
					GetStarPosition(ForStarDiameter: false, UpdateStarOnly: true);
					GetAsteroidPosition();
					Data_and_Plots.Historical_AllEvents.EncodeAnEvent_inXML(NewEvent: false, i);
				}
				UpdatingOldFile = false;
				Data_and_Plots.Historical_AllEvents.WriteHistoricalObservationsFile(HistoricalFile: true, "");
			}
			((Form)pBar).Close();
		}

		private void AddParallaxMotionToolStripMenuItem_Click(object sender, EventArgs e)
		{
			PBar pBar = new PBar();
			pBar.pBarFTP.set_Minimum(0);
			pBar.pBarFTP.set_Maximum(Data_and_Plots.Historical_AllEvents.OccEvents.Count + 1);
			((Control)pBar).Show();
			UpdatingOldFile = true;
			for (int i = 0; i < Data_and_Plots.Historical_AllEvents.OccEvents.Count; i++)
			{
				pBar.pBarFTP.set_Value(i);
				Data_and_Plots.Historical_AllEvents.DecodeAnEvent_Into_EventDetails(i, out var _, out var _);
				DisplayEventInEditor();
				Data_and_Plots.Observations_InitialisePlot_Using_EventDetails();
				((Control)Data_and_Plots.Observations_Editor).Focus();
				Data_and_Plots.Historical_AllEvents.EncodeAnEvent_inXML(NewEvent: false, i);
			}
			UpdatingOldFile = false;
			Data_and_Plots.Historical_AllEvents.WriteHistoricalObservationsFile(HistoricalFile: true, "");
			((Form)pBar).Close();
		}

		private void updateAsteroidMotionToolStripMenuItem_Click(object sender, EventArgs e)
		{
			PBar pBar = new PBar();
			pBar.pBarFTP.set_Minimum(0);
			pBar.pBarFTP.set_Maximum(Data_and_Plots.Historical_AllEvents.OccEvents.Count + 1);
			((Control)pBar).Show();
			UpdatingOldFile = true;
			for (int i = 0; i < Data_and_Plots.Historical_AllEvents.OccEvents.Count; i++)
			{
				pBar.pBarFTP.set_Value(i);
				Data_and_Plots.Historical_AllEvents.DecodeAnEvent_Into_EventDetails(i, out var _, out var _);
				DisplayEventInEditor();
				GetAsteroidPosition(UseExistingStarPosition: true);
				Data_and_Plots.Historical_AllEvents.EncodeAnEvent_inXML(NewEvent: false, i);
			}
			UpdatingOldFile = false;
			Data_and_Plots.Historical_AllEvents.WriteHistoricalObservationsFile(HistoricalFile: true, "");
			((Form)pBar).Close();
		}

		private void ConvertObsToXML()
		{
		}

		private void checkCountryStateToolStripMenuItem_Click(object sender, EventArgs e)
		{
		}

		private void cmdHelpCountryCodes_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Country Codes");
		}

		private void convertMPCorbEtcToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			try
			{
				((Form)new ConvertAstorb_etc(1)).ShowDialog();
			}
			catch
			{
			}
		}

		private void GetLightCurveData()
		{
			Asteroid_Observations_Reports.ShowLightCurveData(((Control)txtAsteroidNumber).get_Text().Trim(), ((Control)txtAsteroidName).get_Text().Trim());
		}

		internal void DisplayEvent_ByDate_Number(int year, int month, int day, string AsteroidNumber, string AsteroidName)
		{
			Data_and_Plots.ShowEditor();
			if (AsteroidNumber.Contains("P") | AsteroidNumber.Contains("C"))
			{
				optByPlanets.set_Checked(true);
			}
			else
			{
				int num = 0;
				if (year < 1990)
				{
					num = cmbYearRange.get_Items().get_Count() - 2;
				}
				else if (year < 2000)
				{
					num = cmbYearRange.get_Items().get_Count() - 3;
				}
				else if (year < 2018)
				{
					num = cmbYearRange.get_Items().get_Count() - 4 - (year - 2000);
				}
				else
				{
					num = cmbYearRange.get_Items().get_Count() - 22 - 2 * (year - 2018);
					if (month > 6)
					{
						num--;
					}
				}
				if (num < 0)
				{
					num = 0;
				}
				optByDate.set_Checked(true);
				((ListControl)cmbYearRange).set_SelectedIndex(num);
			}
			string text = year + month.ToString().PadLeft(3) + day.ToString().PadLeft(3);
			AsteroidNumber.Trim().PadLeft(6);
			for (int i = 0; i < cmbHistorical.get_Items().get_Count(); i++)
			{
				cmbHistorical.get_Items().get_Item(i).ToString();
				if (cmbHistorical.get_Items().get_Item(i).ToString()!.Substring(0, 10) == text && cmbHistorical.get_Items().get_Item(i).ToString()!.Substring(11).Contains(AsteroidNumber))
				{
					((ListControl)cmbHistorical).set_SelectedIndex(i);
					break;
				}
			}
		}

		private void ObservationsEditor_FormClosing(object sender, FormClosingEventArgs e)
		{
			CloseAsteroidObsEditor_PeripheralForms();
		}

		public void CloseAsteroidObsEditor_PeripheralForms()
		{
			Settings.Default.LocationAsterObsEditor = ((Form)this).get_Location();
			Settings.Default.SizeAsterEditor = (Point)((Form)this).get_Size();
			try
			{
				Settings.Default.LocationD_ShapeModels = ((Form)D_ShapeModels).get_Location();
				((Form)D_ShapeModels).Close();
			}
			catch
			{
			}
			try
			{
				Settings.Default.LocationD_Doubles = ((Form)D_Doubles).get_Location();
				((Form)D_Doubles).Close();
			}
			catch
			{
			}
			try
			{
				Settings.Default.LocationD_NearbyStars = ((Form)D_NearbyStars).get_Location();
				((Form)D_NearbyStars).Close();
			}
			catch
			{
			}
			try
			{
				Settings.Default.LocationIRDiameter = ((Form)Utilities.DataBoxDia).get_Location();
				((Form)Utilities.DataBoxDia).Close();
			}
			catch
			{
			}
			try
			{
				Settings.Default.LocationLightCurveData = ((Form)Asteroid_Observations_Reports.LightCurveData).get_Location();
				((Form)Utilities.DataBoxDia).Close();
			}
			catch
			{
			}
			try
			{
				Settings.Default.LocationShowStarDiameter = ((Form)DisplayMPOccultations.StarDiameter).get_Location();
				((Form)DisplayMPOccultations.StarDiameter).Close();
			}
			catch
			{
			}
			try
			{
				Settings.Default.LocationPredictionOffset = ((Form)Data_and_Plots.Prediction_Offset).get_Location();
				((Form)Data_and_Plots.Prediction_Offset).Close();
			}
			catch
			{
			}
			try
			{
				Settings.Default.LocationRelativePathDistances = ((Form)Data_and_Plots.PathDistances).get_Location();
				((Form)Data_and_Plots.PathDistances).Close();
			}
			catch
			{
			}
			try
			{
				Settings.Default.LocationTimeBaseOffset = ((Form)Data_and_Plots.TimeBase_Offset).get_Location();
				((Form)Data_and_Plots.TimeBase_Offset).Close();
			}
			catch
			{
			}
			try
			{
				Settings.Default.LocationSatelliteSoln = ((Form)D_Satellites).get_Location();
				((Form)D_Satellites).Close();
			}
			catch
			{
			}
			try
			{
				Settings.Default.LocationStarDetails = ((Form)D_Stars).get_Location();
				((Form)D_Stars).Close();
			}
			catch
			{
			}
			try
			{
				((Form)BinaryAsteroids.BinaryAsteroidsDisplay).Close();
			}
			catch
			{
			}
			try
			{
				Settings.Default.LocationChordLengths = ((Form)Data_and_Plots.ChordLengths).get_Location();
				((Form)Data_and_Plots.ChordLengths).Close();
			}
			catch
			{
			}
			try
			{
				((Form)Data_and_Plots.Prediction_Offset).Close();
			}
			catch
			{
			}
			try
			{
				((Form)Data_and_Plots.Miss_Times).Close();
			}
			catch
			{
			}
			try
			{
				((Form)Data_and_Plots.ChordLengths).Close();
			}
			catch
			{
			}
			try
			{
				((Form)Data_and_Plots.DiameterFromChords).Close();
			}
			catch
			{
			}
			try
			{
				((Form)Vizier_Sesame.StarList).Close();
			}
			catch
			{
			}
			try
			{
				((Form)Asteroid_Observations_Reports.Display_Diameters).Close();
			}
			catch
			{
			}
			try
			{
				((Form)Asteroid_Observations_Reports.Display_Doubles).Close();
			}
			catch
			{
			}
			try
			{
				((Form)LightCurveViewer.MultiPlot).Close();
			}
			catch
			{
			}
			try
			{
				((Form)Asteroid_Observations_Reports.LightCurveData).Close();
			}
			catch
			{
			}
			try
			{
				((Form)DisplayMPOccultations.StarDiameter).Close();
			}
			catch
			{
			}
			try
			{
				((Form)Interferometric_Plus_WDS.Display).Close();
			}
			catch
			{
			}
			try
			{
				((Form)Data_and_Plots.PlotForm).Close();
			}
			catch
			{
			}
		}

		private void txtAsteroidNumber_TextChanged(object sender, EventArgs e)
		{
			SetAsteroidGroupLabel();
			try
			{
				((Form)LightCurveViewer).Close();
			}
			catch
			{
			}
			SetLightCurveFlag();
			((Control)cmdPredict).set_Enabled((((Control)txtAsteroidNumber).get_Text().Trim().Length > 0) & optAsteroid.get_Checked());
		}

		private void SetLightCurveFlag()
		{
			try
			{
				Settings.Default.LocationMultiLightCurves = ((Form)LightData.MultiPlot).get_Location();
				((Form)LightData.MultiPlot).Close();
			}
			catch
			{
			}
			int NumLightCurves = 0;
			LightCurveRecordNumbers = new List<int>();
			SubmittedRecordNumbers = new List<int>();
			int result = 0;
			string asteroidName;
			if (!optAsteroid.get_Checked())
			{
				asteroidName = (optComets.get_Checked() ? ((Control)txtCometName).get_Text().Trim().ToLower() : ((((ListControl)cmbMoons).get_SelectedIndex() > 0) ? cmbMoons.get_Items().get_Item(((ListControl)cmbMoons).get_SelectedIndex()).ToString()!.ToLower() : cmbPlanets.get_Items().get_Item(((ListControl)cmbPlanets).get_SelectedIndex()).ToString()!.ToLower()));
			}
			else
			{
				int.TryParse(((Control)txtAsteroidNumber).get_Text(), out result);
				asteroidName = ((Control)txtAsteroidName).get_Text().Trim().ToLower();
			}
			try
			{
				LightData.LightCurveView.addTextToolStripMenuItem.set_Checked(false);
			}
			catch
			{
			}
			LightData.HeightRatioForSelectedLightLevel = 0f;
			LightData.HeaderExtraText = "";
			LightData.AddExtraHeader = false;
			LightData.HeaderExtraTime_UT1 = (LightData.HeaderExtraTime_UT2 = 0.0);
			LightData.AddTimeMarker1 = (LightData.AddTimeMarker2 = false);
			if (LightData.MainLightCurves.Count < 1)
			{
				LightData.ReadMainFile(Index: true, ReRead: true, ShowReadErrorMessages: false);
			}
			LightData.LoadLightCurvesReceived_Admin(IncludeEmbargoed: true, includeForReview: true, 0, ShowReadErrorMessages: true);
			LightData.MainLightCurve_EntriesForAnObject(ref LightCurveRecordNumbers, ref NumLightCurves, result, asteroidName, EventDetails.JD_EventDate, DateToWithin4Hours: true);
			LightData.SubmittedLightCurve_EntriesForAnObject(ref SubmittedRecordNumbers, result, asteroidName, EventDetails.JD_EventDate, DateToWithin4Hours: true);
			int num = LightCurveRecordNumbers.Count + SubmittedRecordNumbers.Count;
			((Control)txtNumberOfLightCurves).set_Visible(num > 0);
			((Control)txtNumberOfLightCurves).set_Text(num + " light curves for event");
			if (LightCurveViewer.MultiPlot != null)
			{
				((Form)LightCurveViewer.MultiPlot).Close();
			}
			if (num > 0)
			{
				LightCurveViewer = new LightCurveViewer();
				LightCurveViewer.MultiCurvePlot_Main_Submitted(ref LightCurveRecordNumbers, ref SubmittedRecordNumbers, ShowSkippedFrames: true);
			}
		}

		private void skipToNextFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			//IL_0043: Unknown result type (might be due to invalid IL or missing references)
			//IL_0049: Invalid comparison between Unknown and I4
			//IL_00b3: Unknown result type (might be due to invalid IL or missing references)
			if ((int)MessageBox.Show("Are you sure you want to skip to the next file without\r\nadding the current observations to the historical observations?", "Confirm skip", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			if ((int)MessageBox.Show("Do you want to move the source file:\r\n\r\n   " + Current_OBS_File + "\r\n\r\nto the 'Not added' subdirectory of the file's location?", "Move source file", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6)
			{
				string text = Path.GetDirectoryName(Current_OBS_File) + "\\Not added";
				string destFileName = text + "\\" + Path.GetFileName(Current_OBS_File);
				try
				{
					if (!Directory.Exists(text))
					{
						Directory.CreateDirectory(text);
					}
					File.Move(Current_OBS_File, destFileName);
				}
				catch
				{
					MessageBox.Show("The file\r\n  " + Current_OBS_File + "has not been moved", "Move error", (MessageBoxButtons)0, (MessageBoxIcon)16);
				}
			}
			EventLoadedFromFile = false;
			if (ReadingMultipleOBSFiles)
			{
				ReadNextMultipleFile();
			}
			else if (ReadingMultipleEurasterEntries)
			{
				ImportEuraster(NewObservationForExistingEvent: false, EurasterEvents[CurrentEuraster]);
			}
			else if (ReadingMultipleJPEntries)
			{
				ImportJP(NewObservationForExistingEvents: false, JPEvents[CurrentJP]);
			}
			if (Settings.Default.GE_Alts_Auto)
			{
				Validate_SiteAltitudes_MidTime("");
			}
		}

		private void SetPasteEnabled(bool PasteEnabled)
		{
			ToolStripMenuItem obj = newEventOBSFormatToolStripMenuItem;
			bool enabled;
			((ToolStripItem)newObservationLineoneOnlyOBSFormatToolStripMenuItem).set_Enabled(enabled = PasteEnabled);
			((ToolStripItem)obj).set_Enabled(enabled);
		}

		private void dELETIONModeToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0026: Unknown result type (might be due to invalid IL or missing references)
			//IL_002c: Invalid comparison between Unknown and I4
			//IL_0046: Unknown result type (might be due to invalid IL or missing references)
			//IL_004c: Invalid comparison between Unknown and I4
			string text = "You have requested Deletion mode\r\n\r\nIn this mode, events can be removed from the file of \r\nHistorical observations.\r\n\r\nWhen you delete an event, a file for that event will be\r\nwritten to the directory 'Resource Files/DeletedObservations\r\n\r\nIf you proceed, a copy of the current observations file\r\nwill be written to the Resource Files subdirectory, with \r\ntoday's date and 'DEL' included in the name. This will only\r\noccur with the first access to this mode on a given date\r\n\r\nIn this mode, observations cannot be edited or saved\r\n\r\nDo you wish to continue?";
			if (!dELETIONModeToolStripMenuItem.get_Checked())
			{
				if ((int)MessageBox.Show(text, "Deletion mode", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 6)
				{
					return;
				}
			}
			else if ((int)MessageBox.Show("Do you want to exit Deletion mode?", "End deletion mode", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			dELETIONModeToolStripMenuItem.set_Checked(!dELETIONModeToolStripMenuItem.get_Checked());
			if (!dELETIONModeToolStripMenuItem.get_Checked())
			{
				((Control)this).set_BackColor(SystemColors.Control);
			}
			else
			{
				((Control)this).set_BackColor(Color.PowderBlue);
			}
			ToolStripMenuItem obj = newToolStripMenuItem;
			ToolStripMenuItem obj2 = sortByDistanceWhenOpeningToolStripMenuItem;
			ToolStripMenuItem obj3 = openToolStripMenuItem;
			ToolStripMenuItem obj4 = openMultipleFilesToolStripMenuItem;
			ToolStripMenuItem obj5 = saveToolStripMenuItem;
			ToolStripMenuItem obj6 = saveAsToolStripMenuItem;
			ToolStripMenuItem obj7 = matchUnistellarObservationsToolStripMenuItem;
			ToolStripMenuItem obj8 = addToHistoricalFileToolStripMenuItem;
			ToolStripMenuItem obj9 = skipToNextFileToolStripMenuItem;
			ToolStripMenuItem obj10 = updateHistoricalFileToolStripMenuItem;
			ToolStripMenuItem obj11 = copyObserverNamesToolStripMenuItem;
			ToolStripMenuItem obj12 = copyvalidObserverNamesToolStripMenuItem;
			bool flag;
			((ToolStripItem)copyAllObserverDetailsToolStripMenuItem).set_Visible(flag = !dELETIONModeToolStripMenuItem.get_Checked());
			bool flag2;
			((ToolStripItem)obj12).set_Visible(flag2 = flag);
			bool flag3;
			((ToolStripItem)obj11).set_Visible(flag3 = flag2);
			bool flag4;
			((ToolStripItem)obj10).set_Visible(flag4 = flag3);
			bool flag5;
			((ToolStripItem)obj9).set_Visible(flag5 = flag4);
			bool flag6;
			((ToolStripItem)obj8).set_Visible(flag6 = flag5);
			bool flag7;
			((ToolStripItem)obj7).set_Visible(flag7 = flag6);
			bool flag8;
			((ToolStripItem)obj6).set_Visible(flag8 = flag7);
			bool flag9;
			((ToolStripItem)obj5).set_Visible(flag9 = flag8);
			bool flag10;
			((ToolStripItem)obj4).set_Visible(flag10 = flag9);
			bool flag11;
			((ToolStripItem)obj3).set_Visible(flag11 = flag10);
			bool visible;
			((ToolStripItem)obj2).set_Visible(visible = flag11);
			((ToolStripItem)obj).set_Visible(visible);
			ToolStripMenuItem obj13 = pasteToolStripMenuItem;
			ToolStripMenuItem obj14 = sortObserverLinesToolStripMenuItem;
			ToolStripMenuItem obj15 = plotToolStripMenuItem;
			((ToolStripItem)ExportImport).set_Enabled(flag10 = !dELETIONModeToolStripMenuItem.get_Checked());
			((ToolStripItem)obj15).set_Enabled(flag11 = flag10);
			((ToolStripItem)obj14).set_Enabled(visible = flag11);
			((ToolStripItem)obj13).set_Enabled(visible);
			GroupBox obj16 = grpDate;
			GroupBox obj17 = grpStar;
			GroupBox obj18 = grpAsteroid;
			GroupBox obj19 = grpObserver;
			GroupBox obj20 = grpTimes;
			GroupBox obj21 = grpConditions;
			GroupBox obj22 = grpManageObservers;
			((Control)grpManageHistorical).set_Enabled(flag6 = !dELETIONModeToolStripMenuItem.get_Checked());
			((Control)obj22).set_Enabled(flag7 = flag6);
			((Control)obj21).set_Enabled(flag8 = flag7);
			((Control)obj20).set_Enabled(flag9 = flag8);
			((Control)obj19).set_Enabled(flag10 = flag9);
			((Control)obj18).set_Enabled(flag11 = flag10);
			((Control)obj17).set_Enabled(visible = flag11);
			((Control)obj16).set_Enabled(visible);
			Button obj23 = cmdPlusOne;
			((Control)chkAutoOpenSave).set_Enabled(visible = !dELETIONModeToolStripMenuItem.get_Checked());
			((Control)obj23).set_Enabled(visible);
			((Control)grpDeletion).set_Visible(dELETIONModeToolStripMenuItem.get_Checked());
			DeletionPath = Utilities.AppPath + "\\Resource Files\\DeletedObservations";
			if (!Directory.Exists(DeletionPath))
			{
				Directory.CreateDirectory(DeletionPath);
			}
			Observations_BUPfile = DeletionPath + "\\" + Utilities.AsteroidObservationsFile.Replace(".xml", "_PreDel_" + DateTime.Now.Year + "-" + Utilities.Months[DateTime.Now.Month].PadLeft(2, '0') + "-" + DateTime.Now.Day.ToString().PadLeft(2, '0') + "_" + DateTime.Now.Hour + "h");
			if (!File.Exists(Observations_BUPfile))
			{
				File.Copy(Utilities.AppPath + "\\Resource Files\\" + Utilities.AsteroidObservationsFile, Observations_BUPfile);
			}
			ToolStripMenuItem obj24 = cOMPILATIONModeToolStripMenuItem;
			((ToolStripItem)setCompilationFileNameToolStripMenuItem).set_Enabled(visible = !dELETIONModeToolStripMenuItem.get_Checked());
			((ToolStripItem)obj24).set_Enabled(visible);
		}

		private void cmdDeleteEvent_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Are you sure you want to delete the currently\r\ndisplayed event from the Historical observations file?", "Confirm deletion", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 6)
			{
				string singleFileName = DeletionPath + "\\" + EventDetails.Year + EventDetails.Month.ToString().PadLeft(2, '0') + EventDetails.Day.ToString().PadLeft(2, '0') + "_" + EventDetails.AsteroidNumber + "_" + EventDetails.AsteroidID + "_OBS_Del.xml";
				AllEvents allEvents = new AllEvents();
				allEvents.EncodeAnEvent_inXML(NewEvent: true, 0);
				allEvents.WriteHistoricalObservationsFile(HistoricalFile: false, singleFileName);
				int num = ((ListControl)cmbHistorical).get_SelectedIndex();
				Data_and_Plots.Historical_AllEvents.OccEvents.RemoveAt(CurrentHistoricalRecord);
				Data_and_Plots.Historical_AllEvents.WriteHistoricalObservationsFile(HistoricalFile: true, "");
				DisplayIndex(ForCurrentEvent: false);
				if (num >= cmbHistorical.get_Items().get_Count())
				{
					num = cmbHistorical.get_Items().get_Count() - 1;
				}
				((ListControl)cmbHistorical).set_SelectedIndex(num);
				((Control)lblLastUpdate).set_Text("Added : " + EventDetails.YearAdded + " " + Utilities.ShortMonths[EventDetails.MonthAdded] + " " + EventDetails.DayAdded + "     Updated : " + EventDetails.YearEdited + " " + Utilities.ShortMonths[EventDetails.MonthEdited] + " " + EventDetails.DayEdited);
				StartTime = DateTime.Now;
				((Control)lblLastSaveTime).set_ForeColor(Color.Green);
			}
		}

		private void removeAllMPCReferencesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_000b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0011: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("This will remove ALL MPC circular numbers and dates. This is irreversible. Make sure the Asteroid~Observations.xml file is not open in any other software before the end of the update processing. Otherwise an error may occur when the updated file needs to be written at the end of the updates.\r\n\r\nDo you want to continue?", "Close copies", (MessageBoxButtons)4) == 7)
			{
				return;
			}
			using StreamReader streamReader = new StreamReader(Utilities.AppPath + "/Resource Files/Asteroid~Observations.xml");
			using StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "/Resource Files/Asteroid~Observations_NoMPC.xml");
			do
			{
				string text = streamReader.ReadLine();
				if (text.Contains("<MPC>") & text.Contains("</MPC>"))
				{
					text = "        <MPC>||</MPC>";
				}
				streamWriter.WriteLine(text);
			}
			while (!streamReader.EndOfStream);
		}

		private void iSAMUpdateToUniqueOnlyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_01b6: Unknown result type (might be due to invalid IL or missing references)
			List<string> list = new List<string>();
			List<string> list2 = new List<string>();
			using (StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\ShapeModels\\ISAM_AvailableAsteroids.csv"))
			{
				streamReader.ReadLine();
				do
				{
					string[] array = streamReader.ReadLine()!.Split(new char[1] { ',' });
					list.Add("A" + array[0] + "-" + array[2].Replace("10", ""));
					list2.Add("A" + array[0] + "-" + array[2]);
					if (array[1] == "2")
					{
						list.Add("A" + array[0] + "-" + array[4].Replace("10", ""));
						list2.Add("A" + array[0] + "-" + array[4]);
					}
				}
				while (!streamReader.EndOfStream);
			}
			using (StreamReader streamReader2 = new StreamReader(Utilities.AppPath + "/Resource Files/Asteroid~Observations.xml"))
			{
				using StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "/Resource Files/Asteroid~Observations_ISAMfix.xml");
				do
				{
					string text = streamReader2.ReadLine();
					if (text.Contains("<Fit>ISAM"))
					{
						for (int i = 0; i < list.Count; i++)
						{
							if (text.Contains(list[i] + "|"))
							{
								streamWriter.WriteLine(text.Replace(list[i], list2[i]));
								break;
							}
						}
					}
					else
					{
						streamWriter.WriteLine(text);
					}
				}
				while (!streamReader2.EndOfStream);
			}
			MessageBox.Show("ISAM fix completed");
		}

		private void altitudeErrorsFindAndSaveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_000b: Unknown result type (might be due to invalid IL or missing references)
			MessageBox.Show("Make sure the Asteroid~Observations.xml file is not open in any other software before the end of the update processing. Otherwise an error may occur when the updated file needs to be written at the end of the updates", "Close copies", (MessageBoxButtons)0);
			PBar pBar = new PBar();
			pBar.pBarFTP.set_Minimum(0);
			pBar.pBarFTP.set_Maximum(Data_and_Plots.Historical_AllEvents.OccEvents.Count + 1);
			((Control)pBar).Show();
			string text = Utilities.AppPath + "\\Generated Files\\SiteAltitudeErrors";
			if (File.Exists(text + "_30.txt"))
			{
				File.Delete(text + "_30.txt");
			}
			if (File.Exists(text + "_100.txt"))
			{
				File.Delete(text + "_100.txt");
			}
			for (int i = 0; i < Data_and_Plots.Historical_AllEvents.OccEvents.Count; i++)
			{
				pBar.pBarFTP.set_Value(i);
				((Control)pBar).set_Text(string.Format("{0,1} out of {1,1}", i, Data_and_Plots.Historical_AllEvents.OccEvents.Count));
				Application.DoEvents();
				Data_and_Plots.Historical_AllEvents.DecodeAnEvent_Into_EventDetails(i, out var _, out var _);
				Validate_SiteAltitudes_MidTime(text);
			}
			((Form)pBar).Close();
		}

		private void doubleStarFormatErrorsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_000b: Unknown result type (might be due to invalid IL or missing references)
			MessageBox.Show("Make sure the Asteroid~Observations.xml file is not open in any other software before the end of the update processing. Otherwise an error may occur when the updated file needs to be written at the end of the updates", "Close copies", (MessageBoxButtons)0);
			PBar pBar = new PBar();
			pBar.pBarFTP.set_Minimum(0);
			pBar.pBarFTP.set_Maximum(Data_and_Plots.Historical_AllEvents.OccEvents.Count + 1);
			((Control)pBar).Show();
			using (StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "/Resource Files/Asteroid~Observations_DoubleFormat.txt"))
			{
				for (int i = 0; i < Data_and_Plots.Historical_AllEvents.OccEvents.Count; i++)
				{
					pBar.pBarFTP.set_Value(i);
					bool flag = false;
					string indexLine = Data_and_Plots.Historical_AllEvents.OccEvents[i].IndexLine;
					for (int j = 0; j < Data_and_Plots.Historical_AllEvents.OccEvents[i].Lines.Count; j++)
					{
						if (Data_and_Plots.Historical_AllEvents.OccEvents[i].Lines[j].Contains("<Solution>"))
						{
							string[] array = PDS.StripTags(Data_and_Plots.Historical_AllEvents.OccEvents[i].Lines[j].ToString()).Split(new char[1] { '|' });
							for (int k = 0; k < 4; k++)
							{
								if (Utilities.DecimalPlaces(array[k]) > 1)
								{
									flag = true;
									break;
								}
							}
						}
						if (flag)
						{
							break;
						}
					}
					if (flag)
					{
						streamWriter.WriteLine(indexLine);
					}
				}
			}
			((Form)pBar).Close();
		}

		private void updateUncertaintiesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_000b: Unknown result type (might be due to invalid IL or missing references)
			MessageBox.Show("Make sure the Asteroid~Observations.xml file is not open in any other software before the end of the update processing. Otherwise an error may occur when the updated file needs to be written at the end of the updates", "Close copies", (MessageBoxButtons)0);
			PBar pBar = new PBar();
			pBar.pBarFTP.set_Minimum(0);
			pBar.pBarFTP.set_Maximum(Data_and_Plots.Historical_AllEvents.OccEvents.Count + 1);
			((Control)pBar).Show();
			for (int i = 0; i < Data_and_Plots.Historical_AllEvents.OccEvents.Count; i++)
			{
				pBar.pBarFTP.set_Value(i);
				((Control)pBar).set_Text(string.Format("{0,1} out of {1,1}", i, Data_and_Plots.Historical_AllEvents.OccEvents.Count));
				Application.DoEvents();
				for (int j = 0; j < Data_and_Plots.Historical_AllEvents.OccEvents[i].Lines.Count; j++)
				{
					if (Data_and_Plots.Historical_AllEvents.OccEvents[i].Lines[j].Contains("<MainBody>"))
					{
						Data_and_Plots.Historical_AllEvents.DecodeAnEvent_Into_EventDetails(i, out var _, out var PrimaryChords);
						DisplayEventInEditor(Display_LightCurves_ShapeModels: false);
						string text = AllEvents.DecimalPlaces_String_InSolution_Output(EventDetails.Y_Dia);
						string value = "".PadLeft(Tags.TagIndent[13]) + Tags.TagStart[13] + EventDetails.AsteroidNumber.Trim() + string.Format("|{0,2:F" + text + "}", EventDetails.X_Geo_atEvent) + string.Format("|{0,2:F" + text + "}", EventDetails.Y_Geo_atEvent) + string.Format("|{0,2:F4}", EventDetails.dRACosDec_atEvent) + string.Format("|{0,2:F4}", EventDetails.dDec_atEvent) + string.Format("|{0,2:F4}", EventDetails.Sdev_dRACosDec_atEvent) + string.Format("|{0,2:F4}", EventDetails.Sdev_dDec_atEvent) + "|" + Convert.ToInt32(EventDetails.AstrometryShapeModelCentered) + "|" + EventDetails.Number_Chords + "|" + Convert.ToInt32(EventDetails.UnseenBinaryPrimary) + "|" + EventDetails.FitUncertaintyCategory + string.Format("|{0,1:F1}", EventDetails.AdjustmentTo_sDev_AlongTrack) + string.Format("|{0,1:F1}", EventDetails.AdjustmentTo_sDev_AcrossTrack) + string.Format("|{0,1:F2}", EventDetails.MaxHitPlus) + string.Format("|{0,1:F2}", EventDetails.MaxHitMinus) + string.Format("|{0,1:F2}", EventDetails.MinMissPlus) + string.Format("|{0,1:F2}", EventDetails.MinMissMinus) + Tags.TagEnd[13];
						Data_and_Plots.Historical_AllEvents.OccEvents[i].Lines[j] = value;
						string[] obj = new string[19]
						{
							"".PadLeft(Tags.TagIndent[29]),
							Tags.TagStart[29],
							EventDetails.AsteroidNumber.Trim(),
							"|",
							string.Format("{0,2:F" + text + "}|", EventDetails.X_Geo_atConj),
							string.Format("{0,2:F" + text + "}|", EventDetails.Y_Geo_atConj),
							EventDetails.Year_Conj.ToString(),
							"|",
							EventDetails.Month_Conj.ToString(),
							"|",
							string.Format("{0,2:F7}|", EventDetails.Day_Conj),
							string.Format("{0,2:F7}|", EventDetails.Sdev_T_Conj),
							string.Format("{0,2:F" + text + "}|", EventDetails.Sdev_AlongTrack),
							string.Format("{0,2:F4}|", EventDetails.Sep_Conj),
							string.Format("{0,2:F" + text + "}|", EventDetails.Sdev_Sep_Conj),
							string.Format("{0,2:F3}", EventDetails.PA_Conj_2000),
							"|",
							null,
							null
						};
						PrimaryChords = Convert.ToInt32(EventDetails.UnseenBinaryPrimary);
						obj[17] = PrimaryChords.ToString();
						obj[18] = Tags.TagEnd[29];
						string value2 = string.Concat(obj);
						Data_and_Plots.Historical_AllEvents.OccEvents[i].Lines[j + 1] = value2;
						break;
					}
				}
				if (i % 1000 == 0)
				{
					Data_and_Plots.Historical_AllEvents.WriteHistoricalObservationsFile(HistoricalFile: true, "");
				}
			}
			((Form)pBar).Close();
			Data_and_Plots.Historical_AllEvents.WriteHistoricalObservationsFile(HistoricalFile: true, "");
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing && components != null)
			{
				components.Dispose();
			}
			((Form)this).Dispose(disposing);
		}

		private void InitializeComponent()
		{
			//IL_001c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0026: Expected O, but got Unknown
			//IL_0027: Unknown result type (might be due to invalid IL or missing references)
			//IL_0031: Expected O, but got Unknown
			//IL_0032: Unknown result type (might be due to invalid IL or missing references)
			//IL_003c: Expected O, but got Unknown
			//IL_003d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0047: Expected O, but got Unknown
			//IL_0048: Unknown result type (might be due to invalid IL or missing references)
			//IL_0052: Expected O, but got Unknown
			//IL_0053: Unknown result type (might be due to invalid IL or missing references)
			//IL_005d: Expected O, but got Unknown
			//IL_005e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0068: Expected O, but got Unknown
			//IL_0069: Unknown result type (might be due to invalid IL or missing references)
			//IL_0073: Expected O, but got Unknown
			//IL_0074: Unknown result type (might be due to invalid IL or missing references)
			//IL_007e: Expected O, but got Unknown
			//IL_007f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0089: Expected O, but got Unknown
			//IL_008a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0094: Expected O, but got Unknown
			//IL_0095: Unknown result type (might be due to invalid IL or missing references)
			//IL_009f: Expected O, but got Unknown
			//IL_00a0: Unknown result type (might be due to invalid IL or missing references)
			//IL_00aa: Expected O, but got Unknown
			//IL_00ab: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b5: Expected O, but got Unknown
			//IL_00b6: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c0: Expected O, but got Unknown
			//IL_00c1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00cb: Expected O, but got Unknown
			//IL_00cc: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d6: Expected O, but got Unknown
			//IL_00d7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e1: Expected O, but got Unknown
			//IL_00e2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ec: Expected O, but got Unknown
			//IL_00ed: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f7: Expected O, but got Unknown
			//IL_00f8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0102: Expected O, but got Unknown
			//IL_0103: Unknown result type (might be due to invalid IL or missing references)
			//IL_010d: Expected O, but got Unknown
			//IL_010e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0118: Expected O, but got Unknown
			//IL_0119: Unknown result type (might be due to invalid IL or missing references)
			//IL_0123: Expected O, but got Unknown
			//IL_0124: Unknown result type (might be due to invalid IL or missing references)
			//IL_012e: Expected O, but got Unknown
			//IL_012f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0139: Expected O, but got Unknown
			//IL_013a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0144: Expected O, but got Unknown
			//IL_0145: Unknown result type (might be due to invalid IL or missing references)
			//IL_014f: Expected O, but got Unknown
			//IL_0150: Unknown result type (might be due to invalid IL or missing references)
			//IL_015a: Expected O, but got Unknown
			//IL_015b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0165: Expected O, but got Unknown
			//IL_0166: Unknown result type (might be due to invalid IL or missing references)
			//IL_0170: Expected O, but got Unknown
			//IL_0171: Unknown result type (might be due to invalid IL or missing references)
			//IL_017b: Expected O, but got Unknown
			//IL_017c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0186: Expected O, but got Unknown
			//IL_0187: Unknown result type (might be due to invalid IL or missing references)
			//IL_0191: Expected O, but got Unknown
			//IL_0192: Unknown result type (might be due to invalid IL or missing references)
			//IL_019c: Expected O, but got Unknown
			//IL_019d: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a7: Expected O, but got Unknown
			//IL_01a8: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b2: Expected O, but got Unknown
			//IL_01b3: Unknown result type (might be due to invalid IL or missing references)
			//IL_01bd: Expected O, but got Unknown
			//IL_01be: Unknown result type (might be due to invalid IL or missing references)
			//IL_01c8: Expected O, but got Unknown
			//IL_01c9: Unknown result type (might be due to invalid IL or missing references)
			//IL_01d3: Expected O, but got Unknown
			//IL_01d4: Unknown result type (might be due to invalid IL or missing references)
			//IL_01de: Expected O, but got Unknown
			//IL_01df: Unknown result type (might be due to invalid IL or missing references)
			//IL_01e9: Expected O, but got Unknown
			//IL_01ea: Unknown result type (might be due to invalid IL or missing references)
			//IL_01f4: Expected O, but got Unknown
			//IL_01f5: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ff: Expected O, but got Unknown
			//IL_0200: Unknown result type (might be due to invalid IL or missing references)
			//IL_020a: Expected O, but got Unknown
			//IL_020b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0215: Expected O, but got Unknown
			//IL_0216: Unknown result type (might be due to invalid IL or missing references)
			//IL_0220: Expected O, but got Unknown
			//IL_0221: Unknown result type (might be due to invalid IL or missing references)
			//IL_022b: Expected O, but got Unknown
			//IL_022c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0236: Expected O, but got Unknown
			//IL_0237: Unknown result type (might be due to invalid IL or missing references)
			//IL_0241: Expected O, but got Unknown
			//IL_0242: Unknown result type (might be due to invalid IL or missing references)
			//IL_024c: Expected O, but got Unknown
			//IL_024d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0257: Expected O, but got Unknown
			//IL_0258: Unknown result type (might be due to invalid IL or missing references)
			//IL_0262: Expected O, but got Unknown
			//IL_0263: Unknown result type (might be due to invalid IL or missing references)
			//IL_026d: Expected O, but got Unknown
			//IL_026e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0278: Expected O, but got Unknown
			//IL_0279: Unknown result type (might be due to invalid IL or missing references)
			//IL_0283: Expected O, but got Unknown
			//IL_0284: Unknown result type (might be due to invalid IL or missing references)
			//IL_028e: Expected O, but got Unknown
			//IL_028f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0299: Expected O, but got Unknown
			//IL_029a: Unknown result type (might be due to invalid IL or missing references)
			//IL_02a4: Expected O, but got Unknown
			//IL_02a5: Unknown result type (might be due to invalid IL or missing references)
			//IL_02af: Expected O, but got Unknown
			//IL_02b0: Unknown result type (might be due to invalid IL or missing references)
			//IL_02ba: Expected O, but got Unknown
			//IL_02bb: Unknown result type (might be due to invalid IL or missing references)
			//IL_02c5: Expected O, but got Unknown
			//IL_02c6: Unknown result type (might be due to invalid IL or missing references)
			//IL_02d0: Expected O, but got Unknown
			//IL_02d1: Unknown result type (might be due to invalid IL or missing references)
			//IL_02db: Expected O, but got Unknown
			//IL_02dc: Unknown result type (might be due to invalid IL or missing references)
			//IL_02e6: Expected O, but got Unknown
			//IL_02e7: Unknown result type (might be due to invalid IL or missing references)
			//IL_02f1: Expected O, but got Unknown
			//IL_02f2: Unknown result type (might be due to invalid IL or missing references)
			//IL_02fc: Expected O, but got Unknown
			//IL_02fd: Unknown result type (might be due to invalid IL or missing references)
			//IL_0307: Expected O, but got Unknown
			//IL_0308: Unknown result type (might be due to invalid IL or missing references)
			//IL_0312: Expected O, but got Unknown
			//IL_0313: Unknown result type (might be due to invalid IL or missing references)
			//IL_031d: Expected O, but got Unknown
			//IL_031e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0328: Expected O, but got Unknown
			//IL_0329: Unknown result type (might be due to invalid IL or missing references)
			//IL_0333: Expected O, but got Unknown
			//IL_0334: Unknown result type (might be due to invalid IL or missing references)
			//IL_033e: Expected O, but got Unknown
			//IL_033f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0349: Expected O, but got Unknown
			//IL_034a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0354: Expected O, but got Unknown
			//IL_0355: Unknown result type (might be due to invalid IL or missing references)
			//IL_035f: Expected O, but got Unknown
			//IL_0360: Unknown result type (might be due to invalid IL or missing references)
			//IL_036a: Expected O, but got Unknown
			//IL_036b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0375: Expected O, but got Unknown
			//IL_0376: Unknown result type (might be due to invalid IL or missing references)
			//IL_0380: Expected O, but got Unknown
			//IL_0381: Unknown result type (might be due to invalid IL or missing references)
			//IL_038b: Expected O, but got Unknown
			//IL_038c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0396: Expected O, but got Unknown
			//IL_0397: Unknown result type (might be due to invalid IL or missing references)
			//IL_03a1: Expected O, but got Unknown
			//IL_03a2: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ac: Expected O, but got Unknown
			//IL_03ad: Unknown result type (might be due to invalid IL or missing references)
			//IL_03b7: Expected O, but got Unknown
			//IL_03b8: Unknown result type (might be due to invalid IL or missing references)
			//IL_03c2: Expected O, but got Unknown
			//IL_03c3: Unknown result type (might be due to invalid IL or missing references)
			//IL_03cd: Expected O, but got Unknown
			//IL_03ce: Unknown result type (might be due to invalid IL or missing references)
			//IL_03d8: Expected O, but got Unknown
			//IL_03d9: Unknown result type (might be due to invalid IL or missing references)
			//IL_03e3: Expected O, but got Unknown
			//IL_03e4: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ee: Expected O, but got Unknown
			//IL_03ef: Unknown result type (might be due to invalid IL or missing references)
			//IL_03f9: Expected O, but got Unknown
			//IL_03fa: Unknown result type (might be due to invalid IL or missing references)
			//IL_0404: Expected O, but got Unknown
			//IL_0405: Unknown result type (might be due to invalid IL or missing references)
			//IL_040f: Expected O, but got Unknown
			//IL_0410: Unknown result type (might be due to invalid IL or missing references)
			//IL_041a: Expected O, but got Unknown
			//IL_041b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0425: Expected O, but got Unknown
			//IL_0426: Unknown result type (might be due to invalid IL or missing references)
			//IL_0430: Expected O, but got Unknown
			//IL_0431: Unknown result type (might be due to invalid IL or missing references)
			//IL_043b: Expected O, but got Unknown
			//IL_043c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0446: Expected O, but got Unknown
			//IL_0447: Unknown result type (might be due to invalid IL or missing references)
			//IL_0451: Expected O, but got Unknown
			//IL_0452: Unknown result type (might be due to invalid IL or missing references)
			//IL_045c: Expected O, but got Unknown
			//IL_045d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0467: Expected O, but got Unknown
			//IL_0468: Unknown result type (might be due to invalid IL or missing references)
			//IL_0472: Expected O, but got Unknown
			//IL_0473: Unknown result type (might be due to invalid IL or missing references)
			//IL_047d: Expected O, but got Unknown
			//IL_047e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0488: Expected O, but got Unknown
			//IL_0489: Unknown result type (might be due to invalid IL or missing references)
			//IL_0493: Expected O, but got Unknown
			//IL_0494: Unknown result type (might be due to invalid IL or missing references)
			//IL_049e: Expected O, but got Unknown
			//IL_049f: Unknown result type (might be due to invalid IL or missing references)
			//IL_04a9: Expected O, but got Unknown
			//IL_04aa: Unknown result type (might be due to invalid IL or missing references)
			//IL_04b4: Expected O, but got Unknown
			//IL_04b5: Unknown result type (might be due to invalid IL or missing references)
			//IL_04bf: Expected O, but got Unknown
			//IL_04c0: Unknown result type (might be due to invalid IL or missing references)
			//IL_04ca: Expected O, but got Unknown
			//IL_04cb: Unknown result type (might be due to invalid IL or missing references)
			//IL_04d5: Expected O, but got Unknown
			//IL_04d6: Unknown result type (might be due to invalid IL or missing references)
			//IL_04e0: Expected O, but got Unknown
			//IL_04e1: Unknown result type (might be due to invalid IL or missing references)
			//IL_04eb: Expected O, but got Unknown
			//IL_04ec: Unknown result type (might be due to invalid IL or missing references)
			//IL_04f6: Expected O, but got Unknown
			//IL_04f7: Unknown result type (might be due to invalid IL or missing references)
			//IL_0501: Expected O, but got Unknown
			//IL_0502: Unknown result type (might be due to invalid IL or missing references)
			//IL_050c: Expected O, but got Unknown
			//IL_050d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0517: Expected O, but got Unknown
			//IL_0518: Unknown result type (might be due to invalid IL or missing references)
			//IL_0522: Expected O, but got Unknown
			//IL_0523: Unknown result type (might be due to invalid IL or missing references)
			//IL_052d: Expected O, but got Unknown
			//IL_052e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0538: Expected O, but got Unknown
			//IL_0539: Unknown result type (might be due to invalid IL or missing references)
			//IL_0543: Expected O, but got Unknown
			//IL_0544: Unknown result type (might be due to invalid IL or missing references)
			//IL_054e: Expected O, but got Unknown
			//IL_054f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0559: Expected O, but got Unknown
			//IL_055a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0564: Expected O, but got Unknown
			//IL_0565: Unknown result type (might be due to invalid IL or missing references)
			//IL_056f: Expected O, but got Unknown
			//IL_0570: Unknown result type (might be due to invalid IL or missing references)
			//IL_057a: Expected O, but got Unknown
			//IL_057b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0585: Expected O, but got Unknown
			//IL_0586: Unknown result type (might be due to invalid IL or missing references)
			//IL_0590: Expected O, but got Unknown
			//IL_0591: Unknown result type (might be due to invalid IL or missing references)
			//IL_059b: Expected O, but got Unknown
			//IL_059c: Unknown result type (might be due to invalid IL or missing references)
			//IL_05a6: Expected O, but got Unknown
			//IL_05a7: Unknown result type (might be due to invalid IL or missing references)
			//IL_05b1: Expected O, but got Unknown
			//IL_05b2: Unknown result type (might be due to invalid IL or missing references)
			//IL_05bc: Expected O, but got Unknown
			//IL_05bd: Unknown result type (might be due to invalid IL or missing references)
			//IL_05c7: Expected O, but got Unknown
			//IL_05c8: Unknown result type (might be due to invalid IL or missing references)
			//IL_05d2: Expected O, but got Unknown
			//IL_05d3: Unknown result type (might be due to invalid IL or missing references)
			//IL_05dd: Expected O, but got Unknown
			//IL_05de: Unknown result type (might be due to invalid IL or missing references)
			//IL_05e8: Expected O, but got Unknown
			//IL_05e9: Unknown result type (might be due to invalid IL or missing references)
			//IL_05f3: Expected O, but got Unknown
			//IL_05f4: Unknown result type (might be due to invalid IL or missing references)
			//IL_05fe: Expected O, but got Unknown
			//IL_05ff: Unknown result type (might be due to invalid IL or missing references)
			//IL_0609: Expected O, but got Unknown
			//IL_060a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0614: Expected O, but got Unknown
			//IL_0615: Unknown result type (might be due to invalid IL or missing references)
			//IL_061f: Expected O, but got Unknown
			//IL_0620: Unknown result type (might be due to invalid IL or missing references)
			//IL_062a: Expected O, but got Unknown
			//IL_062b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0635: Expected O, but got Unknown
			//IL_0636: Unknown result type (might be due to invalid IL or missing references)
			//IL_0640: Expected O, but got Unknown
			//IL_0641: Unknown result type (might be due to invalid IL or missing references)
			//IL_064b: Expected O, but got Unknown
			//IL_064c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0656: Expected O, but got Unknown
			//IL_0657: Unknown result type (might be due to invalid IL or missing references)
			//IL_0661: Expected O, but got Unknown
			//IL_0662: Unknown result type (might be due to invalid IL or missing references)
			//IL_066c: Expected O, but got Unknown
			//IL_066d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0677: Expected O, but got Unknown
			//IL_0678: Unknown result type (might be due to invalid IL or missing references)
			//IL_0682: Expected O, but got Unknown
			//IL_0683: Unknown result type (might be due to invalid IL or missing references)
			//IL_068d: Expected O, but got Unknown
			//IL_068e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0698: Expected O, but got Unknown
			//IL_0699: Unknown result type (might be due to invalid IL or missing references)
			//IL_06a3: Expected O, but got Unknown
			//IL_06a4: Unknown result type (might be due to invalid IL or missing references)
			//IL_06ae: Expected O, but got Unknown
			//IL_06af: Unknown result type (might be due to invalid IL or missing references)
			//IL_06b9: Expected O, but got Unknown
			//IL_06ba: Unknown result type (might be due to invalid IL or missing references)
			//IL_06c4: Expected O, but got Unknown
			//IL_06c5: Unknown result type (might be due to invalid IL or missing references)
			//IL_06cf: Expected O, but got Unknown
			//IL_06d0: Unknown result type (might be due to invalid IL or missing references)
			//IL_06da: Expected O, but got Unknown
			//IL_06db: Unknown result type (might be due to invalid IL or missing references)
			//IL_06e5: Expected O, but got Unknown
			//IL_06e6: Unknown result type (might be due to invalid IL or missing references)
			//IL_06f0: Expected O, but got Unknown
			//IL_06f1: Unknown result type (might be due to invalid IL or missing references)
			//IL_06fb: Expected O, but got Unknown
			//IL_06fc: Unknown result type (might be due to invalid IL or missing references)
			//IL_0706: Expected O, but got Unknown
			//IL_0707: Unknown result type (might be due to invalid IL or missing references)
			//IL_0711: Expected O, but got Unknown
			//IL_0712: Unknown result type (might be due to invalid IL or missing references)
			//IL_071c: Expected O, but got Unknown
			//IL_071d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0727: Expected O, but got Unknown
			//IL_0728: Unknown result type (might be due to invalid IL or missing references)
			//IL_0732: Expected O, but got Unknown
			//IL_0733: Unknown result type (might be due to invalid IL or missing references)
			//IL_073d: Expected O, but got Unknown
			//IL_073e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0748: Expected O, but got Unknown
			//IL_0749: Unknown result type (might be due to invalid IL or missing references)
			//IL_0753: Expected O, but got Unknown
			//IL_0754: Unknown result type (might be due to invalid IL or missing references)
			//IL_075e: Expected O, but got Unknown
			//IL_075f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0769: Expected O, but got Unknown
			//IL_076a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0774: Expected O, but got Unknown
			//IL_0775: Unknown result type (might be due to invalid IL or missing references)
			//IL_077f: Expected O, but got Unknown
			//IL_0780: Unknown result type (might be due to invalid IL or missing references)
			//IL_078a: Expected O, but got Unknown
			//IL_078b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0795: Expected O, but got Unknown
			//IL_0796: Unknown result type (might be due to invalid IL or missing references)
			//IL_07a0: Expected O, but got Unknown
			//IL_07a1: Unknown result type (might be due to invalid IL or missing references)
			//IL_07ab: Expected O, but got Unknown
			//IL_07ac: Unknown result type (might be due to invalid IL or missing references)
			//IL_07b6: Expected O, but got Unknown
			//IL_07b7: Unknown result type (might be due to invalid IL or missing references)
			//IL_07c1: Expected O, but got Unknown
			//IL_07c2: Unknown result type (might be due to invalid IL or missing references)
			//IL_07cc: Expected O, but got Unknown
			//IL_07cd: Unknown result type (might be due to invalid IL or missing references)
			//IL_07d7: Expected O, but got Unknown
			//IL_07d8: Unknown result type (might be due to invalid IL or missing references)
			//IL_07e2: Expected O, but got Unknown
			//IL_07e3: Unknown result type (might be due to invalid IL or missing references)
			//IL_07ed: Expected O, but got Unknown
			//IL_07ee: Unknown result type (might be due to invalid IL or missing references)
			//IL_07f8: Expected O, but got Unknown
			//IL_07f9: Unknown result type (might be due to invalid IL or missing references)
			//IL_0803: Expected O, but got Unknown
			//IL_0804: Unknown result type (might be due to invalid IL or missing references)
			//IL_080e: Expected O, but got Unknown
			//IL_080f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0819: Expected O, but got Unknown
			//IL_081a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0824: Expected O, but got Unknown
			//IL_0825: Unknown result type (might be due to invalid IL or missing references)
			//IL_082f: Expected O, but got Unknown
			//IL_0830: Unknown result type (might be due to invalid IL or missing references)
			//IL_083a: Expected O, but got Unknown
			//IL_083b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0845: Expected O, but got Unknown
			//IL_0846: Unknown result type (might be due to invalid IL or missing references)
			//IL_0850: Expected O, but got Unknown
			//IL_0851: Unknown result type (might be due to invalid IL or missing references)
			//IL_085b: Expected O, but got Unknown
			//IL_085c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0866: Expected O, but got Unknown
			//IL_0867: Unknown result type (might be due to invalid IL or missing references)
			//IL_0871: Expected O, but got Unknown
			//IL_0872: Unknown result type (might be due to invalid IL or missing references)
			//IL_087c: Expected O, but got Unknown
			//IL_087d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0887: Expected O, but got Unknown
			//IL_0888: Unknown result type (might be due to invalid IL or missing references)
			//IL_0892: Expected O, but got Unknown
			//IL_0893: Unknown result type (might be due to invalid IL or missing references)
			//IL_089d: Expected O, but got Unknown
			//IL_089e: Unknown result type (might be due to invalid IL or missing references)
			//IL_08a8: Expected O, but got Unknown
			//IL_08a9: Unknown result type (might be due to invalid IL or missing references)
			//IL_08b3: Expected O, but got Unknown
			//IL_08b4: Unknown result type (might be due to invalid IL or missing references)
			//IL_08be: Expected O, but got Unknown
			//IL_08bf: Unknown result type (might be due to invalid IL or missing references)
			//IL_08c9: Expected O, but got Unknown
			//IL_08ca: Unknown result type (might be due to invalid IL or missing references)
			//IL_08d4: Expected O, but got Unknown
			//IL_08d5: Unknown result type (might be due to invalid IL or missing references)
			//IL_08df: Expected O, but got Unknown
			//IL_08e0: Unknown result type (might be due to invalid IL or missing references)
			//IL_08ea: Expected O, but got Unknown
			//IL_08eb: Unknown result type (might be due to invalid IL or missing references)
			//IL_08f5: Expected O, but got Unknown
			//IL_08f6: Unknown result type (might be due to invalid IL or missing references)
			//IL_0900: Expected O, but got Unknown
			//IL_0901: Unknown result type (might be due to invalid IL or missing references)
			//IL_090b: Expected O, but got Unknown
			//IL_090c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0916: Expected O, but got Unknown
			//IL_0917: Unknown result type (might be due to invalid IL or missing references)
			//IL_0921: Expected O, but got Unknown
			//IL_0922: Unknown result type (might be due to invalid IL or missing references)
			//IL_092c: Expected O, but got Unknown
			//IL_092d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0937: Expected O, but got Unknown
			//IL_0938: Unknown result type (might be due to invalid IL or missing references)
			//IL_0942: Expected O, but got Unknown
			//IL_0943: Unknown result type (might be due to invalid IL or missing references)
			//IL_094d: Expected O, but got Unknown
			//IL_094e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0958: Expected O, but got Unknown
			//IL_0959: Unknown result type (might be due to invalid IL or missing references)
			//IL_0963: Expected O, but got Unknown
			//IL_0964: Unknown result type (might be due to invalid IL or missing references)
			//IL_096e: Expected O, but got Unknown
			//IL_096f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0979: Expected O, but got Unknown
			//IL_097a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0984: Expected O, but got Unknown
			//IL_0985: Unknown result type (might be due to invalid IL or missing references)
			//IL_098f: Expected O, but got Unknown
			//IL_0990: Unknown result type (might be due to invalid IL or missing references)
			//IL_099a: Expected O, but got Unknown
			//IL_099b: Unknown result type (might be due to invalid IL or missing references)
			//IL_09a5: Expected O, but got Unknown
			//IL_09a6: Unknown result type (might be due to invalid IL or missing references)
			//IL_09b0: Expected O, but got Unknown
			//IL_09b1: Unknown result type (might be due to invalid IL or missing references)
			//IL_09bb: Expected O, but got Unknown
			//IL_09bc: Unknown result type (might be due to invalid IL or missing references)
			//IL_09c6: Expected O, but got Unknown
			//IL_09c7: Unknown result type (might be due to invalid IL or missing references)
			//IL_09d1: Expected O, but got Unknown
			//IL_09d2: Unknown result type (might be due to invalid IL or missing references)
			//IL_09dc: Expected O, but got Unknown
			//IL_09dd: Unknown result type (might be due to invalid IL or missing references)
			//IL_09e7: Expected O, but got Unknown
			//IL_09e8: Unknown result type (might be due to invalid IL or missing references)
			//IL_09f2: Expected O, but got Unknown
			//IL_09f3: Unknown result type (might be due to invalid IL or missing references)
			//IL_09fd: Expected O, but got Unknown
			//IL_09fe: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a08: Expected O, but got Unknown
			//IL_0a09: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a13: Expected O, but got Unknown
			//IL_0a14: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a1e: Expected O, but got Unknown
			//IL_0a1f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a29: Expected O, but got Unknown
			//IL_0a2a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a34: Expected O, but got Unknown
			//IL_0a35: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a3f: Expected O, but got Unknown
			//IL_0a40: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a4a: Expected O, but got Unknown
			//IL_0a4b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a55: Expected O, but got Unknown
			//IL_0a56: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a60: Expected O, but got Unknown
			//IL_0a61: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a6b: Expected O, but got Unknown
			//IL_0a6c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a76: Expected O, but got Unknown
			//IL_0a77: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a81: Expected O, but got Unknown
			//IL_0a82: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a8c: Expected O, but got Unknown
			//IL_0a8d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a97: Expected O, but got Unknown
			//IL_0a98: Unknown result type (might be due to invalid IL or missing references)
			//IL_0aa2: Expected O, but got Unknown
			//IL_0aa3: Unknown result type (might be due to invalid IL or missing references)
			//IL_0aad: Expected O, but got Unknown
			//IL_0aae: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ab8: Expected O, but got Unknown
			//IL_0ab9: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ac3: Expected O, but got Unknown
			//IL_0ac4: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ace: Expected O, but got Unknown
			//IL_0acf: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ad9: Expected O, but got Unknown
			//IL_0ada: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ae4: Expected O, but got Unknown
			//IL_0ae5: Unknown result type (might be due to invalid IL or missing references)
			//IL_0aef: Expected O, but got Unknown
			//IL_0af0: Unknown result type (might be due to invalid IL or missing references)
			//IL_0afa: Expected O, but got Unknown
			//IL_0afb: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b05: Expected O, but got Unknown
			//IL_0b06: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b10: Expected O, but got Unknown
			//IL_0b11: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b1b: Expected O, but got Unknown
			//IL_0b1c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b26: Expected O, but got Unknown
			//IL_0b27: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b31: Expected O, but got Unknown
			//IL_0b32: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b3c: Expected O, but got Unknown
			//IL_0b3d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b47: Expected O, but got Unknown
			//IL_0b48: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b52: Expected O, but got Unknown
			//IL_0b53: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b5d: Expected O, but got Unknown
			//IL_0b5e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b68: Expected O, but got Unknown
			//IL_0b69: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b73: Expected O, but got Unknown
			//IL_0b74: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b7e: Expected O, but got Unknown
			//IL_0b7f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b89: Expected O, but got Unknown
			//IL_0b8a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b94: Expected O, but got Unknown
			//IL_0b95: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b9f: Expected O, but got Unknown
			//IL_0ba0: Unknown result type (might be due to invalid IL or missing references)
			//IL_0baa: Expected O, but got Unknown
			//IL_0bab: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bb5: Expected O, but got Unknown
			//IL_0bb6: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bc0: Expected O, but got Unknown
			//IL_0bc1: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bcb: Expected O, but got Unknown
			//IL_0bcc: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bd6: Expected O, but got Unknown
			//IL_0bd7: Unknown result type (might be due to invalid IL or missing references)
			//IL_0be1: Expected O, but got Unknown
			//IL_0be2: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bec: Expected O, but got Unknown
			//IL_0bed: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bf7: Expected O, but got Unknown
			//IL_0bf8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c02: Expected O, but got Unknown
			//IL_0c03: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c0d: Expected O, but got Unknown
			//IL_0c0e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c18: Expected O, but got Unknown
			//IL_0c19: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c23: Expected O, but got Unknown
			//IL_0c24: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c2e: Expected O, but got Unknown
			//IL_0c2f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c39: Expected O, but got Unknown
			//IL_0c3a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c44: Expected O, but got Unknown
			//IL_0c45: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c4f: Expected O, but got Unknown
			//IL_0c50: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c5a: Expected O, but got Unknown
			//IL_0c5b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c65: Expected O, but got Unknown
			//IL_0c66: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c70: Expected O, but got Unknown
			//IL_0c71: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c7b: Expected O, but got Unknown
			//IL_0c7c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c86: Expected O, but got Unknown
			//IL_0c87: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c91: Expected O, but got Unknown
			//IL_0c92: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c9c: Expected O, but got Unknown
			//IL_0c9d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ca7: Expected O, but got Unknown
			//IL_0ca8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cb2: Expected O, but got Unknown
			//IL_0cb3: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cbd: Expected O, but got Unknown
			//IL_0cbe: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cc8: Expected O, but got Unknown
			//IL_0cc9: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cd3: Expected O, but got Unknown
			//IL_0cd4: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cde: Expected O, but got Unknown
			//IL_0cdf: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ce9: Expected O, but got Unknown
			//IL_0cea: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cf4: Expected O, but got Unknown
			//IL_0cf5: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cff: Expected O, but got Unknown
			//IL_0d00: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d0a: Expected O, but got Unknown
			//IL_0d0b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d15: Expected O, but got Unknown
			//IL_0d16: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d20: Expected O, but got Unknown
			//IL_0d21: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d2b: Expected O, but got Unknown
			//IL_0d2c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d36: Expected O, but got Unknown
			//IL_0d37: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d41: Expected O, but got Unknown
			//IL_0d42: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d4c: Expected O, but got Unknown
			//IL_0d4d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d57: Expected O, but got Unknown
			//IL_0d58: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d62: Expected O, but got Unknown
			//IL_0d63: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d6d: Expected O, but got Unknown
			//IL_0d6e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d78: Expected O, but got Unknown
			//IL_0d79: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d83: Expected O, but got Unknown
			//IL_0d84: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d8e: Expected O, but got Unknown
			//IL_0d8f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d99: Expected O, but got Unknown
			//IL_0d9a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0da4: Expected O, but got Unknown
			//IL_0da5: Unknown result type (might be due to invalid IL or missing references)
			//IL_0daf: Expected O, but got Unknown
			//IL_0db0: Unknown result type (might be due to invalid IL or missing references)
			//IL_0dba: Expected O, but got Unknown
			//IL_0dbb: Unknown result type (might be due to invalid IL or missing references)
			//IL_0dc5: Expected O, but got Unknown
			//IL_0dc6: Unknown result type (might be due to invalid IL or missing references)
			//IL_0dd0: Expected O, but got Unknown
			//IL_0dd1: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ddb: Expected O, but got Unknown
			//IL_0ddc: Unknown result type (might be due to invalid IL or missing references)
			//IL_0de6: Expected O, but got Unknown
			//IL_0de7: Unknown result type (might be due to invalid IL or missing references)
			//IL_0df1: Expected O, but got Unknown
			//IL_0df2: Unknown result type (might be due to invalid IL or missing references)
			//IL_0dfc: Expected O, but got Unknown
			//IL_0dfd: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e07: Expected O, but got Unknown
			//IL_0e08: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e12: Expected O, but got Unknown
			//IL_0e13: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e1d: Expected O, but got Unknown
			//IL_0e1e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e28: Expected O, but got Unknown
			//IL_0e29: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e33: Expected O, but got Unknown
			//IL_0e34: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e3e: Expected O, but got Unknown
			//IL_0e3f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e49: Expected O, but got Unknown
			//IL_0e4a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e54: Expected O, but got Unknown
			//IL_0e55: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e5f: Expected O, but got Unknown
			//IL_0e60: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e6a: Expected O, but got Unknown
			//IL_0e6b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e75: Expected O, but got Unknown
			//IL_0e76: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e80: Expected O, but got Unknown
			//IL_0e81: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e8b: Expected O, but got Unknown
			//IL_0e8c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e96: Expected O, but got Unknown
			//IL_0e97: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ea1: Expected O, but got Unknown
			//IL_0ea2: Unknown result type (might be due to invalid IL or missing references)
			//IL_0eac: Expected O, but got Unknown
			//IL_0ead: Unknown result type (might be due to invalid IL or missing references)
			//IL_0eb7: Expected O, but got Unknown
			//IL_0eb8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ec2: Expected O, but got Unknown
			//IL_0ec3: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ecd: Expected O, but got Unknown
			//IL_0ece: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ed8: Expected O, but got Unknown
			//IL_0ed9: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ee3: Expected O, but got Unknown
			//IL_0ee4: Unknown result type (might be due to invalid IL or missing references)
			//IL_0eee: Expected O, but got Unknown
			//IL_0eef: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ef9: Expected O, but got Unknown
			//IL_0efa: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f04: Expected O, but got Unknown
			//IL_0f05: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f0f: Expected O, but got Unknown
			//IL_0f10: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f1a: Expected O, but got Unknown
			//IL_0f1b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f25: Expected O, but got Unknown
			//IL_0f26: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f30: Expected O, but got Unknown
			//IL_0f31: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f3b: Expected O, but got Unknown
			//IL_0f3c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f46: Expected O, but got Unknown
			//IL_0f47: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f51: Expected O, but got Unknown
			//IL_0f52: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f5c: Expected O, but got Unknown
			//IL_0f5d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f67: Expected O, but got Unknown
			//IL_0f68: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f72: Expected O, but got Unknown
			//IL_0f73: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f7d: Expected O, but got Unknown
			//IL_0f7e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f88: Expected O, but got Unknown
			//IL_0f89: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f93: Expected O, but got Unknown
			//IL_0f94: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f9e: Expected O, but got Unknown
			//IL_0f9f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0fa9: Expected O, but got Unknown
			//IL_0faa: Unknown result type (might be due to invalid IL or missing references)
			//IL_0fb4: Expected O, but got Unknown
			//IL_0fb5: Unknown result type (might be due to invalid IL or missing references)
			//IL_0fbf: Expected O, but got Unknown
			//IL_0fc0: Unknown result type (might be due to invalid IL or missing references)
			//IL_0fca: Expected O, but got Unknown
			//IL_0fcb: Unknown result type (might be due to invalid IL or missing references)
			//IL_0fd5: Expected O, but got Unknown
			//IL_0fd6: Unknown result type (might be due to invalid IL or missing references)
			//IL_0fe0: Expected O, but got Unknown
			//IL_0fe1: Unknown result type (might be due to invalid IL or missing references)
			//IL_0feb: Expected O, but got Unknown
			//IL_0fec: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ff6: Expected O, but got Unknown
			//IL_0ff7: Unknown result type (might be due to invalid IL or missing references)
			//IL_1001: Expected O, but got Unknown
			//IL_1002: Unknown result type (might be due to invalid IL or missing references)
			//IL_100c: Expected O, but got Unknown
			//IL_100d: Unknown result type (might be due to invalid IL or missing references)
			//IL_1017: Expected O, but got Unknown
			//IL_1018: Unknown result type (might be due to invalid IL or missing references)
			//IL_1022: Expected O, but got Unknown
			//IL_1023: Unknown result type (might be due to invalid IL or missing references)
			//IL_102d: Expected O, but got Unknown
			//IL_102e: Unknown result type (might be due to invalid IL or missing references)
			//IL_1038: Expected O, but got Unknown
			//IL_1039: Unknown result type (might be due to invalid IL or missing references)
			//IL_1043: Expected O, but got Unknown
			//IL_1044: Unknown result type (might be due to invalid IL or missing references)
			//IL_104e: Expected O, but got Unknown
			//IL_104f: Unknown result type (might be due to invalid IL or missing references)
			//IL_1059: Expected O, but got Unknown
			//IL_105a: Unknown result type (might be due to invalid IL or missing references)
			//IL_1064: Expected O, but got Unknown
			//IL_1065: Unknown result type (might be due to invalid IL or missing references)
			//IL_106f: Expected O, but got Unknown
			//IL_1070: Unknown result type (might be due to invalid IL or missing references)
			//IL_107a: Expected O, but got Unknown
			//IL_107b: Unknown result type (might be due to invalid IL or missing references)
			//IL_1085: Expected O, but got Unknown
			//IL_1086: Unknown result type (might be due to invalid IL or missing references)
			//IL_1090: Expected O, but got Unknown
			//IL_1091: Unknown result type (might be due to invalid IL or missing references)
			//IL_109b: Expected O, but got Unknown
			//IL_109c: Unknown result type (might be due to invalid IL or missing references)
			//IL_10a6: Expected O, but got Unknown
			//IL_10a7: Unknown result type (might be due to invalid IL or missing references)
			//IL_10b1: Expected O, but got Unknown
			//IL_10b2: Unknown result type (might be due to invalid IL or missing references)
			//IL_10bc: Expected O, but got Unknown
			//IL_10bd: Unknown result type (might be due to invalid IL or missing references)
			//IL_10c7: Expected O, but got Unknown
			//IL_10c8: Unknown result type (might be due to invalid IL or missing references)
			//IL_10d2: Expected O, but got Unknown
			//IL_10d3: Unknown result type (might be due to invalid IL or missing references)
			//IL_10dd: Expected O, but got Unknown
			//IL_10de: Unknown result type (might be due to invalid IL or missing references)
			//IL_10e8: Expected O, but got Unknown
			//IL_10e9: Unknown result type (might be due to invalid IL or missing references)
			//IL_10f3: Expected O, but got Unknown
			//IL_10f4: Unknown result type (might be due to invalid IL or missing references)
			//IL_10fe: Expected O, but got Unknown
			//IL_10ff: Unknown result type (might be due to invalid IL or missing references)
			//IL_1109: Expected O, but got Unknown
			//IL_110a: Unknown result type (might be due to invalid IL or missing references)
			//IL_1114: Expected O, but got Unknown
			//IL_1115: Unknown result type (might be due to invalid IL or missing references)
			//IL_111f: Expected O, but got Unknown
			//IL_1120: Unknown result type (might be due to invalid IL or missing references)
			//IL_112a: Expected O, but got Unknown
			//IL_112b: Unknown result type (might be due to invalid IL or missing references)
			//IL_1135: Expected O, but got Unknown
			//IL_1136: Unknown result type (might be due to invalid IL or missing references)
			//IL_1140: Expected O, but got Unknown
			//IL_1141: Unknown result type (might be due to invalid IL or missing references)
			//IL_114b: Expected O, but got Unknown
			//IL_114c: Unknown result type (might be due to invalid IL or missing references)
			//IL_1156: Expected O, but got Unknown
			//IL_1157: Unknown result type (might be due to invalid IL or missing references)
			//IL_1161: Expected O, but got Unknown
			//IL_1162: Unknown result type (might be due to invalid IL or missing references)
			//IL_116c: Expected O, but got Unknown
			//IL_1173: Unknown result type (might be due to invalid IL or missing references)
			//IL_117d: Expected O, but got Unknown
			//IL_117e: Unknown result type (might be due to invalid IL or missing references)
			//IL_1188: Expected O, but got Unknown
			//IL_1189: Unknown result type (might be due to invalid IL or missing references)
			//IL_1193: Expected O, but got Unknown
			//IL_1194: Unknown result type (might be due to invalid IL or missing references)
			//IL_119e: Expected O, but got Unknown
			//IL_119f: Unknown result type (might be due to invalid IL or missing references)
			//IL_11a9: Expected O, but got Unknown
			//IL_11aa: Unknown result type (might be due to invalid IL or missing references)
			//IL_11b4: Expected O, but got Unknown
			//IL_11b5: Unknown result type (might be due to invalid IL or missing references)
			//IL_11bf: Expected O, but got Unknown
			//IL_11c0: Unknown result type (might be due to invalid IL or missing references)
			//IL_11ca: Expected O, but got Unknown
			//IL_11cb: Unknown result type (might be due to invalid IL or missing references)
			//IL_11d5: Expected O, but got Unknown
			//IL_11d6: Unknown result type (might be due to invalid IL or missing references)
			//IL_11e0: Expected O, but got Unknown
			//IL_11e1: Unknown result type (might be due to invalid IL or missing references)
			//IL_11eb: Expected O, but got Unknown
			//IL_11ec: Unknown result type (might be due to invalid IL or missing references)
			//IL_11f6: Expected O, but got Unknown
			//IL_11f7: Unknown result type (might be due to invalid IL or missing references)
			//IL_1201: Expected O, but got Unknown
			//IL_1202: Unknown result type (might be due to invalid IL or missing references)
			//IL_120c: Expected O, but got Unknown
			//IL_120d: Unknown result type (might be due to invalid IL or missing references)
			//IL_1217: Expected O, but got Unknown
			//IL_1218: Unknown result type (might be due to invalid IL or missing references)
			//IL_1222: Expected O, but got Unknown
			//IL_1223: Unknown result type (might be due to invalid IL or missing references)
			//IL_122d: Expected O, but got Unknown
			//IL_122e: Unknown result type (might be due to invalid IL or missing references)
			//IL_1238: Expected O, but got Unknown
			//IL_1239: Unknown result type (might be due to invalid IL or missing references)
			//IL_1243: Expected O, but got Unknown
			//IL_1244: Unknown result type (might be due to invalid IL or missing references)
			//IL_124e: Expected O, but got Unknown
			//IL_124f: Unknown result type (might be due to invalid IL or missing references)
			//IL_1259: Expected O, but got Unknown
			//IL_125a: Unknown result type (might be due to invalid IL or missing references)
			//IL_1264: Expected O, but got Unknown
			//IL_126b: Unknown result type (might be due to invalid IL or missing references)
			//IL_1275: Expected O, but got Unknown
			//IL_1276: Unknown result type (might be due to invalid IL or missing references)
			//IL_1280: Expected O, but got Unknown
			//IL_1281: Unknown result type (might be due to invalid IL or missing references)
			//IL_128b: Expected O, but got Unknown
			//IL_2361: Unknown result type (might be due to invalid IL or missing references)
			//IL_236b: Expected O, but got Unknown
			//IL_6d81: Unknown result type (might be due to invalid IL or missing references)
			//IL_6d8b: Expected O, but got Unknown
			//IL_8575: Unknown result type (might be due to invalid IL or missing references)
			//IL_857f: Expected O, but got Unknown
			//IL_858c: Unknown result type (might be due to invalid IL or missing references)
			//IL_8596: Expected O, but got Unknown
			//IL_85ba: Unknown result type (might be due to invalid IL or missing references)
			//IL_85c4: Expected O, but got Unknown
			//IL_d5e0: Unknown result type (might be due to invalid IL or missing references)
			//IL_d5ea: Expected O, but got Unknown
			//IL_e1e0: Unknown result type (might be due to invalid IL or missing references)
			//IL_e1ea: Expected O, but got Unknown
			//IL_e3f4: Unknown result type (might be due to invalid IL or missing references)
			//IL_e3fe: Expected O, but got Unknown
			//IL_ead8: Unknown result type (might be due to invalid IL or missing references)
			//IL_eae2: Expected O, but got Unknown
			//IL_eb77: Unknown result type (might be due to invalid IL or missing references)
			//IL_eb81: Expected O, but got Unknown
			//IL_ec16: Unknown result type (might be due to invalid IL or missing references)
			//IL_ec20: Expected O, but got Unknown
			//IL_f198: Unknown result type (might be due to invalid IL or missing references)
			//IL_f1a2: Expected O, but got Unknown
			//IL_f238: Unknown result type (might be due to invalid IL or missing references)
			//IL_f242: Expected O, but got Unknown
			components = new Container();
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(ObservationsEditor));
			grpDate = new GroupBox();
			cmdPredict = new Button();
			txtNumberOfLightCurves = new Label();
			txtMidT = new TextBox();
			label5 = new Label();
			label4 = new Label();
			label3 = new Label();
			label2 = new Label();
			label1 = new Label();
			txtDay = new TextBox();
			txtMonth = new TextBox();
			txtYear = new TextBox();
			label54 = new Label();
			grpStar = new GroupBox();
			cmbCatalogue = new ComboBox();
			lblStarID = new Label();
			cmdIdentifyGstar = new Button();
			lblStarIssues = new Label();
			lblRA = new Label();
			label6 = new Label();
			lblStarDiameter = new Label();
			panelGaiaCoords = new Panel();
			txtGaiaCoords = new TextBox();
			label48 = new Label();
			panelUCAC4 = new Panel();
			label41 = new Label();
			txtU4Number = new TextBox();
			txtU4Zone = new TextBox();
			panelNOMAD = new Panel();
			label40 = new Label();
			txtNOMADnumber = new TextBox();
			txtNOMADzone = new TextBox();
			panelTycho2 = new Panel();
			txtTycComp = new TextBox();
			txtTycSeqNum = new TextBox();
			txtTycRegion = new TextBox();
			lblTycho_UCAC = new Label();
			label8 = new Label();
			label7 = new Label();
			panelHip = new Panel();
			txtHip = new TextBox();
			lblMag = new Label();
			lblDec = new Label();
			cmdGetStarPos = new Button();
			panelB1 = new Panel();
			label9 = new Label();
			txtB1number = new TextBox();
			txtB1zone = new TextBox();
			label10 = new Label();
			lblinGaiaDR2 = new Label();
			grpAsteroid = new GroupBox();
			pnlComet = new Panel();
			label42 = new Label();
			txtCometName = new TextBox();
			txtCometNumber = new TextBox();
			label39 = new Label();
			pnlBinary = new Panel();
			lblBinary = new Label();
			label57 = new Label();
			pnlDAMIT_ISAM = new Panel();
			lblISAM = new Label();
			lblInDAMIT = new Label();
			label52 = new Label();
			label53 = new Label();
			optComets = new RadioButton();
			lblMagnitude = new Label();
			lblDiameterKM = new Label();
			lblParallax = new Label();
			lblMotions = new Label();
			cmdGetAsteroid = new Button();
			optPlanet = new RadioButton();
			optAsteroid = new RadioButton();
			panelPlanets = new Panel();
			label12 = new Label();
			label11 = new Label();
			cmbMoons = new ComboBox();
			cmbPlanets = new ComboBox();
			label14 = new Label();
			label13 = new Label();
			txtAsteroidName = new TextBox();
			txtAsteroidNumber = new TextBox();
			label50 = new Label();
			label51 = new Label();
			grpObserver = new GroupBox();
			cmdHelpCountryCodes = new Button();
			label21 = new Label();
			label47 = new Label();
			txtStateCountry = new TextBox();
			label46 = new Label();
			txtLocatedNear = new TextBox();
			label45 = new Label();
			label44 = new Label();
			chkEtAl = new CheckBox();
			txtObserver2 = new TextBox();
			txtObserver1 = new TextBox();
			panelDDD = new Panel();
			txtLongDDD = new TextBox();
			txtLatDDD = new TextBox();
			panelDMM = new Panel();
			txtLongDmm = new TextBox();
			txtLatMM = new TextBox();
			txtLatDmm = new TextBox();
			txtLongMM = new TextBox();
			txtAlt_ft = new TextBox();
			panelDMS = new Panel();
			txtLongDeg = new TextBox();
			txtLatSec = new TextBox();
			txtLatMin = new TextBox();
			txtLatDeg = new TextBox();
			txtLongSec = new TextBox();
			txtLongMin = new TextBox();
			label20 = new Label();
			label19 = new Label();
			label18 = new Label();
			label17 = new Label();
			label16 = new Label();
			panel2 = new Panel();
			optFeet = new RadioButton();
			optMeters = new RadioButton();
			cmbDatum = new ComboBox();
			cmbTelescope = new ComboBox();
			txtAperture = new TextBox();
			txtAlt_m = new TextBox();
			panel1 = new Panel();
			optDMM = new RadioButton();
			optDDD = new RadioButton();
			optDMS = new RadioButton();
			label49 = new Label();
			cmdAltitude = new Button();
			cmdAutoCompleteObserver = new Label();
			grpHistory = new GroupBox();
			panel3 = new Panel();
			ChkUnfitted = new CheckBox();
			chkShapeModelsOnly = new CheckBox();
			optByPlanets = new RadioButton();
			optbyChords = new RadioButton();
			optByClass = new RadioButton();
			cmbAsteroidClasses = new ComboBox();
			optByRecords = new RadioButton();
			optByNumber = new RadioButton();
			optByName = new RadioButton();
			optByDate = new RadioButton();
			cmbNumberRange = new ComboBox();
			cmbAlpha = new ComboBox();
			cmbYearRange = new ComboBox();
			cmbHistorical = new ComboBox();
			lblCurrentSelected = new Label();
			lblNumbers = new Label();
			grpTimes = new GroupBox();
			panel4 = new Panel();
			chkRing = new CheckBox();
			chkUnseen = new CheckBox();
			label31 = new Label();
			chkMiss = new CheckBox();
			chkPredicted = new CheckBox();
			chkSatellite = new CheckBox();
			chkDoubleStar = new CheckBox();
			label30 = new Label();
			label29 = new Label();
			label28 = new Label();
			label27 = new Label();
			label26 = new Label();
			label25 = new Label();
			label24 = new Label();
			label23 = new Label();
			label22 = new Label();
			chkNotSeen = new CheckBox();
			txtD_Min = new TextBox();
			cmbD_Wt = new ComboBox();
			txtD_Sec = new TextBox();
			txtR_Hr = new TextBox();
			txtR_Min = new TextBox();
			txtR_Sec = new TextBox();
			txtD_Uncert = new TextBox();
			txtR_Uncert = new TextBox();
			txtD_PEq = new TextBox();
			txtR_PEq = new TextBox();
			txtShift = new TextBox();
			cmbR_Wt = new ComboBox();
			txtD_Hr = new TextBox();
			grpConditions = new GroupBox();
			txtFree = new TextBox();
			updnSNR = new NumericUpDown();
			label43 = new Label();
			lblFreeLeft = new Label();
			label37 = new Label();
			label36 = new Label();
			label35 = new Label();
			label33 = new Label();
			cmbMethod = new ComboBox();
			cmbTime = new ComboBox();
			cmbTransparency = new ComboBox();
			cmbPlotControl = new ComboBox();
			cmbStability = new ComboBox();
			label32 = new Label();
			label34 = new Label();
			menuStrip1 = new MenuStrip();
			fileToolStripMenuItem = new ToolStripMenuItem();
			newToolStripMenuItem = new ToolStripMenuItem();
			sortByDistanceWhenOpeningToolStripMenuItem = new ToolStripMenuItem();
			whenOpeningToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator25 = new ToolStripSeparator();
			openToolStripMenuItem = new ToolStripMenuItem();
			openMultipleFilesToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator12 = new ToolStripSeparator();
			checkSubmittedFilesForOmissionsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator26 = new ToolStripSeparator();
			saveToolStripMenuItem = new ToolStripMenuItem();
			saveAsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			addToHistoricalFileToolStripMenuItem = new ToolStripMenuItem();
			matchUnistellarObservationsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator11 = new ToolStripSeparator();
			skipToNextFileToolStripMenuItem = new ToolStripMenuItem();
			updateHistoricalFileToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator5 = new ToolStripSeparator();
			copyEventRecordToolStripMenuItem = new ToolStripMenuItem();
			copyObserverNamesToolStripMenuItem = new ToolStripMenuItem();
			copyvalidObserverNamesToolStripMenuItem = new ToolStripMenuItem();
			copyAllObserverDetailsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator9 = new ToolStripSeparator();
			convertMPCorbEtcToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator24 = new ToolStripSeparator();
			mPCEditModeToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator38 = new ToolStripSeparator();
			dELETIONModeToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator37 = new ToolStripSeparator();
			cOMPILATIONModeToolStripMenuItem = new ToolStripMenuItem();
			fileNameToolStripMenuItem = new ToolStripMenuItem();
			setCompilationFileNameToolStripMenuItem = new ToolStripMenuItem();
			pasteToolStripMenuItem = new ToolStripMenuItem();
			theseAreMaintenaceFunctionsToolStripMenuItem = new ToolStripMenuItem();
			whenPastingSortByDistanceToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator14 = new ToolStripSeparator();
			newEventOBSFormatToolStripMenuItem = new ToolStripMenuItem();
			newObservationObserverGroupXMLFormatToolStripMenuItem = new ToolStripMenuItem();
			validateFilesWhenEventOrObserversPastedToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator10 = new ToolStripSeparator();
			predictionLineToolStripMenuItem = new ToolStripMenuItem();
			lineForAsnUnseenBinaryasteroidPrimaryObjectToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator7 = new ToolStripSeparator();
			loadSaveObserverDetailsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator39 = new ToolStripSeparator();
			eurasterEventAsNewEventToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator3 = new ToolStripSeparator();
			newEventFromJPToolStripMenuItem = new ToolStripMenuItem();
			newObservatToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator6 = new ToolStripSeparator();
			eurasterObservationToCurrentEventToolStripMenuItem = new ToolStripMenuItem();
			newObservationLineoneOnlyOBSFormatToolStripMenuItem = new ToolStripMenuItem();
			sortObserverLinesToolStripMenuItem = new ToolStripMenuItem();
			byLatitudeToolStripMenuItem = new ToolStripMenuItem();
			byLongitudeToolStripMenuItem = new ToolStripMenuItem();
			byNameFieldToolStripMenuItem = new ToolStripMenuItem();
			byNameFieldskip2ToolStripMenuItem = new ToolStripMenuItem();
			byPathDistanceToolStripMenuItem = new ToolStripMenuItem();
			sortByPathDistanceRenumberToolStripMenuItem = new ToolStripMenuItem();
			bySequenceNumberToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			renumberObservationsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator4 = new ToolStripSeparator();
			validateAllSiteAltitudesToolStripMenuItem = new ToolStripMenuItem();
			plotSitesInGoogleEarthToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator23 = new ToolStripSeparator();
			plotToolStripMenuItem = new ToolStripMenuItem();
			solutionsToolStripMenuItem = new ToolStripMenuItem();
			doubleStarsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator33 = new ToolStripSeparator();
			listCurrentEventSolutionsToolStripMenuItem1 = new ToolStripMenuItem();
			deleteCurrentEventSolutionsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator15 = new ToolStripSeparator();
			satelliteToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator34 = new ToolStripSeparator();
			listCurrenteventSatelliteSolutionToolStripMenuItem = new ToolStripMenuItem();
			deeteCurrenteventSatelliteSolutionToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator16 = new ToolStripSeparator();
			shapeModelsToolStripMenuItem1 = new ToolStripMenuItem();
			toolStripSeparator35 = new ToolStripSeparator();
			listCurrenteventShapeModelsSolutionsToolStripMenuItem = new ToolStripMenuItem();
			deleteCurrenteventShapemodelSolutionsToolStripMenuItem = new ToolStripMenuItem();
			downloadShapeModelsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator21 = new ToolStripSeparator();
			astrometricSolutionToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator36 = new ToolStripSeparator();
			astrometricSolutionToolStripMenuItem1 = new ToolStripMenuItem();
			astrometricSolutionIncludingatConjunctionSolutionToolStripMenuItem = new ToolStripMenuItem();
			listOffsetsToolStripMenuItem = new ToolStripMenuItem();
			observerrelatedInformationToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator28 = new ToolStripSeparator();
			fromPredictionToolStripMenuItem = new ToolStripMenuItem();
			possibleTimebaseCorrectionsToolStripMenuItem = new ToolStripMenuItem();
			relativePathDistancesToolStripMenuItem = new ToolStripMenuItem();
			chordLengthsToolStripMenuItem = new ToolStripMenuItem();
			eventCoordinatesToolStripMenuItem = new ToolStripMenuItem();
			observerVelocityRelativeToAsteriodToolStripMenuItem = new ToolStripMenuItem();
			gravitationalLightDeflectionToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator27 = new ToolStripSeparator();
			informationRelatedToTheStarToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator29 = new ToolStripSeparator();
			starChartToolStripMenuItem = new ToolStripMenuItem();
			nearbyStarsToolStripMenuItem = new ToolStripMenuItem();
			starDetailsToolStripMenuItem = new ToolStripMenuItem();
			starDiameterFromVizieRToolStripMenuItem = new ToolStripMenuItem();
			fresnelDiffractionLightCurvesToolStripMenuItem = new ToolStripMenuItem();
			stellarDiameterFresnelDiffractionToolStripMenuItem = new ToolStripMenuItem();
			starDiameterModelLightCurvesToolStripMenuItem = new ToolStripMenuItem();
			gaiaDoubleStarsnearbyToolStripMenuItem = new ToolStripMenuItem();
			displayWDSInterferometricAndVariableStarDataToolStripMenuItem = new ToolStripMenuItem();
			alternativeStarIdentifiersToolStripMenuItem = new ToolStripMenuItem();
			previousOccultationsOfThisStarToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator13 = new ToolStripSeparator();
			informationRelatedToTheAsteroidToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator30 = new ToolStripSeparator();
			asteroidLightCurveDataToolStripMenuItem = new ToolStripMenuItem();
			summaryDataToolStripMenuItem = new ToolStripMenuItem();
			rotationLightCurvesBehrandToolStripMenuItem = new ToolStripMenuItem();
			approximateRangeToolStripMenuItem = new ToolStripMenuItem();
			toolStripMenuItem7 = new ToolStripMenuItem();
			toolStripMenuItem9 = new ToolStripMenuItem();
			toolStripMenuItem10 = new ToolStripMenuItem();
			toolStripMenuItem11 = new ToolStripMenuItem();
			toolStripMenuItem13 = new ToolStripMenuItem();
			cometsToolStripMenuItem = new ToolStripMenuItem();
			asteroidLightcurvePhotometryDatabaseToolStripMenuItem = new ToolStripMenuItem();
			asteroidmagsFromGAIAToolStripMenuItem = new ToolStripMenuItem();
			satelliteIRDiametersToolStripMenuItem = new ToolStripMenuItem();
			jPLSmallBodyDatabaseToolStripMenuItem = new ToolStripMenuItem();
			binaryAsteroidDetailsToolStripMenuItem = new ToolStripMenuItem();
			binaryAsteroidsEstimateOrbitSizeOrPeriodToolStripMenuItem = new ToolStripMenuItem();
			eventsWhereDAMITHasBeenUpdatedToolStripMenuItem = new ToolStripMenuItem();
			asteroidNumberFromDAMITToolStripMenuItem = new ToolStripMenuItem();
			astrometryHeldByTheMinorPlanetCenterToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator31 = new ToolStripSeparator();
			lightCurvesToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator32 = new ToolStripSeparator();
			displayLightCurveToolStripMenuItem = new ToolStripMenuItem();
			displaySubmittedLightCurvesToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator22 = new ToolStripSeparator();
			setListsToAutodisplayToolStripMenuItem = new ToolStripMenuItem();
			AutoNearbyStarstoolStripMenuItem7 = new ToolStripMenuItem();
			AutoStarDiametertoolStripMenuItem = new ToolStripMenuItem();
			AutoLightCuveDatatoolStripMenuItem = new ToolStripMenuItem();
			shapeModelFitsToolStripMenuItem = new ToolStripMenuItem();
			doubleStarSolutionsToolStripMenuItem = new ToolStripMenuItem();
			toolStripMenuItem1 = new ToolStripMenuItem();
			toolStripMenuItem2 = new ToolStripMenuItem();
			toolStripMenuItem3 = new ToolStripMenuItem();
			toolStripMenuItem5 = new ToolStripMenuItem();
			toolStripMenuItem4 = new ToolStripMenuItem();
			toolStripMenuItem6 = new ToolStripMenuItem();
			toolStripMenuItem8 = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			ExportImport = new ToolStripMenuItem();
			helpOnExportImportToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator8 = new ToolStripSeparator();
			createNewExportFileToolStripMenuItem = new ToolStripMenuItem();
			selectExportFileToolStripMenuItem = new ToolStripMenuItem();
			toolStripMenuItemExportFile2 = new ToolStripMenuItem();
			toolStripSeparator17 = new ToolStripSeparator();
			openCommentsFormForCurrentEventToolStripMenuItem = new ToolStripMenuItem();
			exportCurrentEventToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator18 = new ToolStripSeparator();
			toolStripSeparator19 = new ToolStripSeparator();
			selectImportFileImportToolStripMenuItem = new ToolStripMenuItem();
			toolStripMenuItemImportFile = new ToolStripMenuItem();
			toolStripSeparator20 = new ToolStripSeparator();
			listEventsInExportFileToolStripMenuItem = new ToolStripMenuItem();
			importSolutionsToolStripMenuItem = new ToolStripMenuItem();
			transitionToolStripMenuItem = new ToolStripMenuItem();
			updateSolutionsToolStripMenuItem = new ToolStripMenuItem();
			convertObsToolStripMenuItem = new ToolStripMenuItem();
			checkCountryStateToolStripMenuItem = new ToolStripMenuItem();
			updateStarPositionsToolStripMenuItem = new ToolStripMenuItem();
			addParallaxMotionToolStripMenuItem = new ToolStripMenuItem();
			updateAsteroidMotionToolStripMenuItem = new ToolStripMenuItem();
			testGaiaCorrnToolStripMenuItem = new ToolStripMenuItem();
			testGaiaDownloadToolStripMenuItem = new ToolStripMenuItem();
			getEventsWithAssumedUncertsToolStripMenuItem = new ToolStripMenuItem();
			addDiameterUncertaintiesToolStripMenuItem = new ToolStripMenuItem();
			updateRUWEFrom2019ToolStripMenuItem = new ToolStripMenuItem();
			updateAsteroidFitUncertaintiesToolStripMenuItem = new ToolStripMenuItem();
			altitudeErrorsFindAndSaveToolStripMenuItem = new ToolStripMenuItem();
			removeAllMPCReferencesToolStripMenuItem = new ToolStripMenuItem();
			iSAMUpdateToUniqueOnlyToolStripMenuItem = new ToolStripMenuItem();
			doubleStarFormatErrorsToolStripMenuItem = new ToolStripMenuItem();
			updateUncertaintiesToolStripMenuItem = new ToolStripMenuItem();
			lstObservations = new ListBox();
			cmdAdd = new Button();
			cmdReplace = new Button();
			cmdDelete = new Button();
			cmdUp = new Button();
			cmdDown = new Button();
			label38 = new Label();
			cmdRenumber = new Button();
			grpManageObservers = new GroupBox();
			cmdUpdateHistorical = new Button();
			lblCurrentSolution = new Label();
			lblMagDrop = new Label();
			lblMPC = new Label();
			cmdAddToHistorical = new Button();
			grpManageHistorical = new GroupBox();
			cmdAddToCompiled = new Button();
			toolTip = new ToolTip(components);
			txtMPEC = new TextBox();
			txtMPC_ID = new TextBox();
			lblLastUpdate = new Label();
			chkAutoOpenSave = new CheckBox();
			cmdPlusOne = new Button();
			lblLastSaveTime = new Label();
			cmdCloseExpandedFreeText = new Button();
			lblRecordNumber = new Label();
			cmdDeleteEvent = new Button();
			grpDeletion = new GroupBox();
			label55 = new Label();
			lblDeleteName = new Label();
			lblRecNoDel = new Label();
			label15 = new Label();
			txtMPCyear = new TextBox();
			txtMPCmonth = new TextBox();
			txtMPCday = new TextBox();
			lblMPEC = new Label();
			pnlMPC = new Panel();
			label56 = new Label();
			lblRUWE = new Label();
			toolTipLightCurve = new ToolTip(components);
			cmdCreateLightCurve = new Label();
			eventDateStarAsteroidOccelmntPredictionXmlToolStripMenuItem = new ToolStripMenuItem();
			((Control)grpDate).SuspendLayout();
			((Control)grpStar).SuspendLayout();
			((Control)panelGaiaCoords).SuspendLayout();
			((Control)panelUCAC4).SuspendLayout();
			((Control)panelNOMAD).SuspendLayout();
			((Control)panelTycho2).SuspendLayout();
			((Control)panelHip).SuspendLayout();
			((Control)panelB1).SuspendLayout();
			((Control)grpAsteroid).SuspendLayout();
			((Control)pnlComet).SuspendLayout();
			((Control)pnlBinary).SuspendLayout();
			((Control)pnlDAMIT_ISAM).SuspendLayout();
			((Control)panelPlanets).SuspendLayout();
			((Control)grpObserver).SuspendLayout();
			((Control)panelDDD).SuspendLayout();
			((Control)panelDMM).SuspendLayout();
			((Control)panelDMS).SuspendLayout();
			((Control)panel2).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((Control)grpHistory).SuspendLayout();
			((Control)panel3).SuspendLayout();
			((Control)grpTimes).SuspendLayout();
			((Control)panel4).SuspendLayout();
			((Control)grpConditions).SuspendLayout();
			((ISupportInitialize)updnSNR).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((Control)grpManageObservers).SuspendLayout();
			((Control)grpManageHistorical).SuspendLayout();
			((Control)grpDeletion).SuspendLayout();
			((Control)pnlMPC).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)grpDate).set_BackColor(Color.AntiqueWhite);
			((Control)grpDate).get_Controls().Add((Control)(object)cmdPredict);
			((Control)grpDate).get_Controls().Add((Control)(object)txtNumberOfLightCurves);
			((Control)grpDate).get_Controls().Add((Control)(object)txtMidT);
			((Control)grpDate).get_Controls().Add((Control)(object)label5);
			((Control)grpDate).get_Controls().Add((Control)(object)label4);
			((Control)grpDate).get_Controls().Add((Control)(object)label3);
			((Control)grpDate).get_Controls().Add((Control)(object)label2);
			((Control)grpDate).get_Controls().Add((Control)(object)label1);
			((Control)grpDate).get_Controls().Add((Control)(object)txtDay);
			((Control)grpDate).get_Controls().Add((Control)(object)txtMonth);
			((Control)grpDate).get_Controls().Add((Control)(object)txtYear);
			((Control)grpDate).get_Controls().Add((Control)(object)label54);
			((Control)grpDate).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpDate).set_Location(new Point(9, 28));
			((Control)grpDate).set_Name("grpDate");
			((Control)grpDate).set_Size(new Size(156, 149));
			((Control)grpDate).set_TabIndex(0);
			grpDate.set_TabStop(false);
			((Control)grpDate).set_Text("Date");
			((Control)cmdPredict).set_BackColor(Color.LightGreen);
			((ButtonBase)cmdPredict).set_FlatStyle((FlatStyle)0);
			((Control)cmdPredict).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdPredict).set_Location(new Point(42, 106));
			((Control)cmdPredict).set_Name("cmdPredict");
			((Control)cmdPredict).set_Size(new Size(73, 22));
			((Control)cmdPredict).set_TabIndex(21);
			((Control)cmdPredict).set_Text("Prediction");
			((ButtonBase)cmdPredict).set_UseVisualStyleBackColor(false);
			((Control)cmdPredict).add_Click((EventHandler)cmdPredict_Click);
			((Control)txtNumberOfLightCurves).set_AutoSize(true);
			((Control)txtNumberOfLightCurves).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)txtNumberOfLightCurves).set_ForeColor(Color.Red);
			((Control)txtNumberOfLightCurves).set_Location(new Point(9, 131));
			((Control)txtNumberOfLightCurves).set_Name("txtNumberOfLightCurves");
			((Control)txtNumberOfLightCurves).set_Size(new Size(139, 13));
			((Control)txtNumberOfLightCurves).set_TabIndex(20);
			((Control)txtNumberOfLightCurves).set_Text("2 light curves for event");
			((Control)txtNumberOfLightCurves).set_Visible(false);
			((Control)txtMidT).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMidT).set_Location(new Point(105, 78));
			((Control)txtMidT).set_Name("txtMidT");
			((Control)txtMidT).set_Size(new Size(30, 20));
			((Control)txtMidT).set_TabIndex(8);
			((Control)txtMidT).add_TextChanged((EventHandler)txtMidT_TextChanged);
			((Control)txtMidT).add_Leave((EventHandler)txtMidT_Leave);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(107, 65));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(26, 13));
			((Control)label5).set_TabIndex(7);
			((Control)label5).set_Text("Hrs");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(28, 73));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(75, 26));
			((Control)label4).set_TabIndex(6);
			((Control)label4).set_Text("Approximate\r\nmid-time (hh.h)");
			label4.set_TextAlign(ContentAlignment.TopRight);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(106, 30));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(29, 13));
			((Control)label3).set_TabIndex(4);
			((Control)label3).set_Text("Day");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(70, 30));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(28, 13));
			((Control)label2).set_TabIndex(2);
			((Control)label2).set_Text("Mth");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(20, 29));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(33, 13));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text("Year");
			((Control)txtDay).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtDay).set_Location(new Point(105, 43));
			((Control)txtDay).set_Name("txtDay");
			((Control)txtDay).set_Size(new Size(30, 20));
			((Control)txtDay).set_TabIndex(5);
			((Control)txtDay).add_TextChanged((EventHandler)txtDay_TextChanged);
			((Control)txtDay).add_Leave((EventHandler)txtDay_Leave);
			((Control)txtMonth).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMonth).set_Location(new Point(69, 43));
			((Control)txtMonth).set_Name("txtMonth");
			((Control)txtMonth).set_Size(new Size(30, 20));
			((Control)txtMonth).set_TabIndex(3);
			((Control)txtMonth).add_TextChanged((EventHandler)txtMonth_TextChanged);
			((Control)txtMonth).add_Leave((EventHandler)txtMonth_Leave);
			((Control)txtYear).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtYear).set_Location(new Point(15, 43));
			((Control)txtYear).set_Name("txtYear");
			((Control)txtYear).set_Size(new Size(42, 20));
			((Control)txtYear).set_TabIndex(1);
			((Control)txtYear).add_TextChanged((EventHandler)txtYear_TextChanged);
			((Control)txtYear).add_Leave((EventHandler)txtYear_Leave);
			((Control)label54).set_AutoSize(true);
			((Control)label54).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label54).set_Location(new Point(5, 17));
			((Control)label54).set_Name("label54");
			((Control)label54).set_Size(new Size(147, 13));
			((Control)label54).set_TabIndex(22);
			((Control)label54).set_Text("Date of the earliest D/R event");
			((Control)grpStar).set_BackColor(Color.AntiqueWhite);
			((Control)grpStar).get_Controls().Add((Control)(object)cmbCatalogue);
			((Control)grpStar).get_Controls().Add((Control)(object)lblStarID);
			((Control)grpStar).get_Controls().Add((Control)(object)cmdIdentifyGstar);
			((Control)grpStar).get_Controls().Add((Control)(object)lblStarIssues);
			((Control)grpStar).get_Controls().Add((Control)(object)lblRA);
			((Control)grpStar).get_Controls().Add((Control)(object)label6);
			((Control)grpStar).get_Controls().Add((Control)(object)lblStarDiameter);
			((Control)grpStar).get_Controls().Add((Control)(object)panelGaiaCoords);
			((Control)grpStar).get_Controls().Add((Control)(object)panelUCAC4);
			((Control)grpStar).get_Controls().Add((Control)(object)panelNOMAD);
			((Control)grpStar).get_Controls().Add((Control)(object)panelTycho2);
			((Control)grpStar).get_Controls().Add((Control)(object)panelHip);
			((Control)grpStar).get_Controls().Add((Control)(object)lblMag);
			((Control)grpStar).get_Controls().Add((Control)(object)lblDec);
			((Control)grpStar).get_Controls().Add((Control)(object)cmdGetStarPos);
			((Control)grpStar).get_Controls().Add((Control)(object)panelB1);
			((Control)grpStar).get_Controls().Add((Control)(object)label10);
			((Control)grpStar).get_Controls().Add((Control)(object)lblinGaiaDR2);
			((Control)grpStar).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpStar).set_Location(new Point(194, 28));
			((Control)grpStar).set_Name("grpStar");
			((Control)grpStar).set_Size(new Size(234, 149));
			((Control)grpStar).set_TabIndex(1);
			grpStar.set_TabStop(false);
			((Control)grpStar).set_Text("Star");
			cmbCatalogue.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbCatalogue).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((ListControl)cmbCatalogue).set_FormattingEnabled(true);
			cmbCatalogue.get_Items().AddRange(new object[6] { "Hipparcos", "Tycho-2", "UCAC4", "USNO-B1", "NOMAD", "Coordinates" });
			((Control)cmbCatalogue).set_Location(new Point(99, 19));
			((Control)cmbCatalogue).set_Name("cmbCatalogue");
			((Control)cmbCatalogue).set_Size(new Size(96, 21));
			((Control)cmbCatalogue).set_TabIndex(1);
			cmbCatalogue.add_SelectedIndexChanged((EventHandler)cmbCatalogue_SelectedIndexChanged);
			((Control)lblStarID).set_AutoSize(true);
			((Control)lblStarID).set_BackColor(Color.AntiqueWhite);
			((Control)lblStarID).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblStarID).set_ForeColor(Color.Blue);
			((Control)lblStarID).set_Location(new Point(80, 0));
			((Control)lblStarID).set_Name("lblStarID");
			((Control)lblStarID).set_Size(new Size(53, 17));
			((Control)lblStarID).set_TabIndex(26);
			((Control)lblStarID).set_Text("StarID");
			((Control)cmdIdentifyGstar).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdIdentifyGstar).set_Location(new Point(5, 118));
			((Control)cmdIdentifyGstar).set_Name("cmdIdentifyGstar");
			((Control)cmdIdentifyGstar).set_Size(new Size(64, 23));
			((Control)cmdIdentifyGstar).set_TabIndex(25);
			((Control)cmdIdentifyGstar).set_Text("Identify J*");
			((ButtonBase)cmdIdentifyGstar).set_UseVisualStyleBackColor(true);
			((Control)cmdIdentifyGstar).add_Click((EventHandler)cmdIdentifyGstar_Click);
			((Control)lblStarIssues).set_AutoSize(true);
			((Control)lblStarIssues).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblStarIssues).set_ForeColor(Color.Red);
			((Control)lblStarIssues).set_Location(new Point(76, 118));
			((Control)lblStarIssues).set_Name("lblStarIssues");
			((Control)lblStarIssues).set_Size(new Size(156, 13));
			((Control)lblStarIssues).set_TabIndex(24);
			((Control)lblStarIssues).set_Text("Proper motion from UCAC4");
			((Control)lblRA).set_AutoSize(true);
			((Control)lblRA).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblRA).set_Location(new Point(76, 69));
			((Control)lblRA).set_Name("lblRA");
			((Control)lblRA).set_Size(new Size(21, 14));
			((Control)lblRA).set_TabIndex(5);
			((Control)lblRA).set_Text("RA");
			toolTip.SetToolTip((Control)(object)lblRA, "GCRS position");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(34, 23));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(64, 13));
			((Control)label6).set_TabIndex(0);
			((Control)label6).set_Text("Catalogue");
			((Control)lblStarDiameter).set_AutoSize(true);
			((Control)lblStarDiameter).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblStarDiameter).set_ForeColor(Color.Fuchsia);
			((Control)lblStarDiameter).set_Location(new Point(76, 130));
			((Control)lblStarDiameter).set_Name("lblStarDiameter");
			((Control)lblStarDiameter).set_Size(new Size(138, 13));
			((Control)lblStarDiameter).set_TabIndex(21);
			((Control)lblStarDiameter).set_Text("1.2 sec fades expected");
			((Control)lblStarDiameter).set_Visible(false);
			((Control)panelGaiaCoords).get_Controls().Add((Control)(object)txtGaiaCoords);
			((Control)panelGaiaCoords).get_Controls().Add((Control)(object)label48);
			((Control)panelGaiaCoords).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)panelGaiaCoords).set_Location(new Point(77, 38));
			((Control)panelGaiaCoords).set_Name("panelGaiaCoords");
			((Control)panelGaiaCoords).set_Size(new Size(141, 33));
			((Control)panelGaiaCoords).set_TabIndex(17);
			((Control)txtGaiaCoords).set_Location(new Point(18, 7));
			((Control)txtGaiaCoords).set_Name("txtGaiaCoords");
			((Control)txtGaiaCoords).set_Size(new Size(119, 20));
			((Control)txtGaiaCoords).set_TabIndex(1);
			toolTip.SetToolTip((Control)(object)txtGaiaCoords, "hhmmss.ss+ddmmss.s\r\nOR\r\nhhmmss.s+ddmmss\r\n\r\nReturns closest star within 2\"\r\nwith mag diff <2.0");
			((Control)txtGaiaCoords).add_KeyPress(new KeyPressEventHandler(txtGaiaCoords_KeyPress));
			((Control)label48).set_AutoSize(true);
			((Control)label48).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label48).set_Location(new Point(7, 10));
			((Control)label48).set_Name("label48");
			((Control)label48).set_Size(new Size(13, 13));
			((Control)label48).set_TabIndex(2);
			((Control)label48).set_Text("J");
			((Control)panelUCAC4).get_Controls().Add((Control)(object)label41);
			((Control)panelUCAC4).get_Controls().Add((Control)(object)txtU4Number);
			((Control)panelUCAC4).get_Controls().Add((Control)(object)txtU4Zone);
			((Control)panelUCAC4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)panelUCAC4).set_Location(new Point(77, 38));
			((Control)panelUCAC4).set_Name("panelUCAC4");
			((Control)panelUCAC4).set_Size(new Size(141, 33));
			((Control)panelUCAC4).set_TabIndex(11);
			((Control)label41).set_AutoSize(true);
			((Control)label41).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label41).set_Location(new Point(48, 10));
			((Control)label41).set_Name("label41");
			((Control)label41).set_Size(new Size(11, 13));
			((Control)label41).set_TabIndex(2);
			((Control)label41).set_Text("-");
			((Control)txtU4Number).set_Location(new Point(60, 7));
			((Control)txtU4Number).set_Name("txtU4Number");
			((Control)txtU4Number).set_Size(new Size(70, 20));
			((Control)txtU4Number).set_TabIndex(1);
			((Control)txtU4Zone).set_Location(new Point(16, 7));
			((Control)txtU4Zone).set_Name("txtU4Zone");
			((Control)txtU4Zone).set_Size(new Size(31, 20));
			((Control)txtU4Zone).set_TabIndex(0);
			((Control)panelNOMAD).get_Controls().Add((Control)(object)label40);
			((Control)panelNOMAD).get_Controls().Add((Control)(object)txtNOMADnumber);
			((Control)panelNOMAD).get_Controls().Add((Control)(object)txtNOMADzone);
			((Control)panelNOMAD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)panelNOMAD).set_Location(new Point(77, 38));
			((Control)panelNOMAD).set_Name("panelNOMAD");
			((Control)panelNOMAD).set_Size(new Size(141, 33));
			((Control)panelNOMAD).set_TabIndex(10);
			((Control)label40).set_AutoSize(true);
			((Control)label40).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label40).set_Location(new Point(48, 10));
			((Control)label40).set_Name("label40");
			((Control)label40).set_Size(new Size(11, 13));
			((Control)label40).set_TabIndex(2);
			((Control)label40).set_Text("-");
			((Control)txtNOMADnumber).set_Location(new Point(60, 7));
			((Control)txtNOMADnumber).set_Name("txtNOMADnumber");
			((Control)txtNOMADnumber).set_Size(new Size(70, 20));
			((Control)txtNOMADnumber).set_TabIndex(1);
			((Control)txtNOMADzone).set_Location(new Point(16, 7));
			((Control)txtNOMADzone).set_Name("txtNOMADzone");
			((Control)txtNOMADzone).set_Size(new Size(31, 20));
			((Control)txtNOMADzone).set_TabIndex(0);
			((Control)panelTycho2).get_Controls().Add((Control)(object)txtTycComp);
			((Control)panelTycho2).get_Controls().Add((Control)(object)txtTycSeqNum);
			((Control)panelTycho2).get_Controls().Add((Control)(object)txtTycRegion);
			((Control)panelTycho2).get_Controls().Add((Control)(object)lblTycho_UCAC);
			((Control)panelTycho2).get_Controls().Add((Control)(object)label8);
			((Control)panelTycho2).get_Controls().Add((Control)(object)label7);
			((Control)panelTycho2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)panelTycho2).set_Location(new Point(77, 38));
			((Control)panelTycho2).set_Name("panelTycho2");
			((Control)panelTycho2).set_Size(new Size(142, 33));
			((Control)panelTycho2).set_TabIndex(8);
			((Control)txtTycComp).set_Location(new Point(111, 7));
			((Control)txtTycComp).set_Name("txtTycComp");
			((Control)txtTycComp).set_Size(new Size(17, 20));
			((Control)txtTycComp).set_TabIndex(1);
			((Control)txtTycSeqNum).set_Location(new Point(56, 7));
			((Control)txtTycSeqNum).set_Name("txtTycSeqNum");
			((Control)txtTycSeqNum).set_Size(new Size(47, 20));
			((Control)txtTycSeqNum).set_TabIndex(2);
			((Control)txtTycRegion).set_Location(new Point(8, 7));
			((Control)txtTycRegion).set_Name("txtTycRegion");
			((Control)txtTycRegion).set_Size(new Size(39, 20));
			((Control)txtTycRegion).set_TabIndex(1);
			((Control)lblTycho_UCAC).set_AutoSize(true);
			((Control)lblTycho_UCAC).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblTycho_UCAC).set_Location(new Point(127, 10));
			((Control)lblTycho_UCAC).set_Name("lblTycho_UCAC");
			((Control)lblTycho_UCAC).set_Size(new Size(13, 13));
			((Control)lblTycho_UCAC).set_TabIndex(5);
			((Control)lblTycho_UCAC).set_Text("u");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(101, 10));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(11, 13));
			((Control)label8).set_TabIndex(0);
			((Control)label8).set_Text("-");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(46, 10));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(11, 13));
			((Control)label7).set_TabIndex(4);
			((Control)label7).set_Text("-");
			((Control)panelHip).get_Controls().Add((Control)(object)txtHip);
			((Control)panelHip).set_Location(new Point(77, 38));
			((Control)panelHip).set_Name("panelHip");
			((Control)panelHip).set_Size(new Size(141, 33));
			((Control)panelHip).set_TabIndex(3);
			((Control)txtHip).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtHip).set_Location(new Point(25, 7));
			((Control)txtHip).set_Name("txtHip");
			((Control)txtHip).set_Size(new Size(70, 20));
			((Control)txtHip).set_TabIndex(2);
			((Control)lblMag).set_AutoSize(true);
			((Control)lblMag).set_Font(new Font("Courier New", 9.75f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblMag).set_Location(new Point(76, 100));
			((Control)lblMag).set_Name("lblMag");
			((Control)lblMag).set_Size(new Size(23, 16));
			((Control)lblMag).set_TabIndex(7);
			((Control)lblMag).set_Text("Mv");
			toolTip.SetToolTip((Control)(object)lblMag, "GCRS position");
			((Control)lblDec).set_AutoSize(true);
			((Control)lblDec).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblDec).set_Location(new Point(76, 85));
			((Control)lblDec).set_Name("lblDec");
			((Control)lblDec).set_Size(new Size(28, 14));
			((Control)lblDec).set_TabIndex(6);
			((Control)lblDec).set_Text("Dec");
			toolTip.SetToolTip((Control)(object)lblDec, "GCRS position");
			((Control)cmdGetStarPos).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdGetStarPos).set_Location(new Point(6, 75));
			((Control)cmdGetStarPos).set_Name("cmdGetStarPos");
			((Control)cmdGetStarPos).set_Size(new Size(62, 35));
			((Control)cmdGetStarPos).set_TabIndex(4);
			((Control)cmdGetStarPos).set_Text("Get\r\ndetails");
			((ButtonBase)cmdGetStarPos).set_UseVisualStyleBackColor(true);
			((Control)cmdGetStarPos).add_Click((EventHandler)cmdGetStarPos_Click);
			((Control)panelB1).get_Controls().Add((Control)(object)label9);
			((Control)panelB1).get_Controls().Add((Control)(object)txtB1number);
			((Control)panelB1).get_Controls().Add((Control)(object)txtB1zone);
			((Control)panelB1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)panelB1).set_Location(new Point(77, 38));
			((Control)panelB1).set_Name("panelB1");
			((Control)panelB1).set_Size(new Size(141, 33));
			((Control)panelB1).set_TabIndex(4);
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(48, 10));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(11, 13));
			((Control)label9).set_TabIndex(2);
			((Control)label9).set_Text("-");
			((Control)txtB1number).set_Location(new Point(60, 7));
			((Control)txtB1number).set_Name("txtB1number");
			((Control)txtB1number).set_Size(new Size(70, 20));
			((Control)txtB1number).set_TabIndex(1);
			((Control)txtB1zone).set_Location(new Point(8, 7));
			((Control)txtB1zone).set_Name("txtB1zone");
			((Control)txtB1zone).set_Size(new Size(39, 20));
			((Control)txtB1zone).set_TabIndex(0);
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label10).set_Location(new Point(24, 48));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(50, 13));
			((Control)label10).set_TabIndex(2);
			((Control)label10).set_Text("Number");
			((Control)lblinGaiaDR2).set_AutoSize(true);
			((Control)lblinGaiaDR2).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblinGaiaDR2).set_Location(new Point(170, 102));
			((Control)lblinGaiaDR2).set_Name("lblinGaiaDR2");
			((Control)lblinGaiaDR2).set_Size(new Size(57, 12));
			((Control)lblinGaiaDR2).set_TabIndex(16);
			((Control)lblinGaiaDR2).set_Text("GaiaEDR3");
			toolTip.SetToolTip((Control)(object)lblinGaiaDR2, "GCRS position");
			((Control)lblinGaiaDR2).set_Visible(false);
			((Control)grpAsteroid).set_BackColor(Color.AntiqueWhite);
			((Control)grpAsteroid).get_Controls().Add((Control)(object)pnlComet);
			((Control)grpAsteroid).get_Controls().Add((Control)(object)pnlBinary);
			((Control)grpAsteroid).get_Controls().Add((Control)(object)pnlDAMIT_ISAM);
			((Control)grpAsteroid).get_Controls().Add((Control)(object)optComets);
			((Control)grpAsteroid).get_Controls().Add((Control)(object)lblMagnitude);
			((Control)grpAsteroid).get_Controls().Add((Control)(object)lblDiameterKM);
			((Control)grpAsteroid).get_Controls().Add((Control)(object)lblParallax);
			((Control)grpAsteroid).get_Controls().Add((Control)(object)lblMotions);
			((Control)grpAsteroid).get_Controls().Add((Control)(object)cmdGetAsteroid);
			((Control)grpAsteroid).get_Controls().Add((Control)(object)optPlanet);
			((Control)grpAsteroid).get_Controls().Add((Control)(object)optAsteroid);
			((Control)grpAsteroid).get_Controls().Add((Control)(object)panelPlanets);
			((Control)grpAsteroid).get_Controls().Add((Control)(object)label14);
			((Control)grpAsteroid).get_Controls().Add((Control)(object)label13);
			((Control)grpAsteroid).get_Controls().Add((Control)(object)txtAsteroidName);
			((Control)grpAsteroid).get_Controls().Add((Control)(object)txtAsteroidNumber);
			((Control)grpAsteroid).get_Controls().Add((Control)(object)label50);
			((Control)grpAsteroid).get_Controls().Add((Control)(object)label51);
			((Control)grpAsteroid).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpAsteroid).set_Location(new Point(457, 28));
			((Control)grpAsteroid).set_Name("grpAsteroid");
			((Control)grpAsteroid).set_Size(new Size(253, 149));
			((Control)grpAsteroid).set_TabIndex(2);
			grpAsteroid.set_TabStop(false);
			((Control)grpAsteroid).set_Text("Asteroid");
			((Control)pnlComet).get_Controls().Add((Control)(object)label42);
			((Control)pnlComet).get_Controls().Add((Control)(object)txtCometName);
			((Control)pnlComet).get_Controls().Add((Control)(object)txtCometNumber);
			((Control)pnlComet).get_Controls().Add((Control)(object)label39);
			((Control)pnlComet).set_Location(new Point(62, 114));
			((Control)pnlComet).set_Name("pnlComet");
			((Control)pnlComet).set_Size(new Size(169, 60));
			((Control)pnlComet).set_TabIndex(25);
			((Control)pnlComet).set_Visible(false);
			((Control)label42).set_AutoSize(true);
			((Control)label42).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label42).set_Location(new Point(1, 24));
			((Control)label42).set_Name("label42");
			((Control)label42).set_Size(new Size(66, 13));
			((Control)label42).set_TabIndex(8);
			((Control)label42).set_Text("Comet name");
			((Control)txtCometName).set_BackColor(Color.FloralWhite);
			((Control)txtCometName).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtCometName).set_Location(new Point(4, 37));
			((Control)txtCometName).set_Name("txtCometName");
			((TextBoxBase)txtCometName).set_ReadOnly(true);
			((Control)txtCometName).set_Size(new Size(161, 20));
			((Control)txtCometName).set_TabIndex(6);
			toolTip.SetToolTip((Control)(object)txtCometName, "This field is read-only. Comet names  are not unique");
			((Control)txtCometNumber).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtCometNumber).set_Location(new Point(88, 6));
			((Control)txtCometNumber).set_Name("txtCometNumber");
			((Control)txtCometNumber).set_Size(new Size(77, 20));
			((Control)txtCometNumber).set_TabIndex(5);
			toolTip.SetToolTip((Control)(object)txtCometNumber, "Valid comet numbers are in the form\r\n  nnnP\r\n  P/yyyy xxxx\r\n  C/yyyy xxxx\r\n  nI\r\n");
			((Control)txtCometNumber).add_Leave((EventHandler)txtCometNumber_Leave);
			((Control)label39).set_AutoSize(true);
			((Control)label39).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label39).set_Location(new Point(1, 9));
			((Control)label39).set_Name("label39");
			((Control)label39).set_Size(new Size(87, 13));
			((Control)label39).set_TabIndex(7);
			((Control)label39).set_Text("Comet number");
			pnlBinary.set_BorderStyle((BorderStyle)2);
			((Control)pnlBinary).get_Controls().Add((Control)(object)lblBinary);
			((Control)pnlBinary).get_Controls().Add((Control)(object)label57);
			((Control)pnlBinary).set_Location(new Point(150, 34));
			((Control)pnlBinary).set_Name("pnlBinary");
			((Control)pnlBinary).set_Size(new Size(89, 18));
			((Control)pnlBinary).set_TabIndex(27);
			((Control)lblBinary).set_AutoSize(true);
			((Control)lblBinary).set_BackColor(Color.SpringGreen);
			((Control)lblBinary).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold | FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)lblBinary).set_ForeColor(Color.MediumBlue);
			((Control)lblBinary).set_Location(new Point(0, 0));
			((Control)lblBinary).set_Name("lblBinary");
			((Control)lblBinary).set_Size(new Size(53, 13));
			((Control)lblBinary).set_TabIndex(0);
			((Control)lblBinary).set_Text("Binary ?");
			((Control)label57).set_AutoSize(true);
			((Control)label57).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label57).set_Location(new Point(7, 0));
			((Control)label57).set_Name("label57");
			((Control)label57).set_Size(new Size(74, 13));
			((Control)label57).set_TabIndex(1);
			((Control)label57).set_Text("single asteroid");
			pnlDAMIT_ISAM.set_BorderStyle((BorderStyle)2);
			((Control)pnlDAMIT_ISAM).get_Controls().Add((Control)(object)lblISAM);
			((Control)pnlDAMIT_ISAM).get_Controls().Add((Control)(object)lblInDAMIT);
			((Control)pnlDAMIT_ISAM).get_Controls().Add((Control)(object)label52);
			((Control)pnlDAMIT_ISAM).get_Controls().Add((Control)(object)label53);
			((Control)pnlDAMIT_ISAM).set_Location(new Point(13, 34));
			((Control)pnlDAMIT_ISAM).set_Name("pnlDAMIT_ISAM");
			((Control)pnlDAMIT_ISAM).set_Size(new Size(119, 18));
			((Control)pnlDAMIT_ISAM).set_TabIndex(26);
			((Control)lblISAM).set_AutoSize(true);
			((Control)lblISAM).set_BackColor(Color.Blue);
			lblISAM.set_BorderStyle((BorderStyle)1);
			((Control)lblISAM).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold | FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)lblISAM).set_ForeColor(Color.Yellow);
			((Control)lblISAM).set_Location(new Point(62, -1));
			((Control)lblISAM).set_Name("lblISAM");
			((Control)lblISAM).set_Size(new Size(39, 15));
			((Control)lblISAM).set_TabIndex(9);
			((Control)lblISAM).set_Text("ISAM");
			((Control)lblISAM).set_Visible(false);
			((Control)lblISAM).add_Click((EventHandler)lblISAM_Click);
			((Control)lblInDAMIT).set_AutoSize(true);
			((Control)lblInDAMIT).set_BackColor(Color.Blue);
			lblInDAMIT.set_BorderStyle((BorderStyle)1);
			((Control)lblInDAMIT).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold | FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)lblInDAMIT).set_ForeColor(Color.Yellow);
			((Control)lblInDAMIT).set_Location(new Point(-1, -1));
			((Control)lblInDAMIT).set_Name("lblInDAMIT");
			((Control)lblInDAMIT).set_Size(new Size(46, 15));
			((Control)lblInDAMIT).set_TabIndex(10);
			((Control)lblInDAMIT).set_Text("DAMIT");
			((Control)lblInDAMIT).set_Visible(false);
			((Control)lblInDAMIT).add_Click((EventHandler)lblInDAMIT_Click);
			((Control)label52).set_AutoSize(true);
			((Control)label52).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label52).set_Location(new Point(4, 1));
			((Control)label52).set_Name("label52");
			((Control)label52).set_Size(new Size(36, 12));
			((Control)label52).set_TabIndex(25);
			((Control)label52).set_Text("DAMIT");
			((Control)label53).set_AutoSize(true);
			((Control)label53).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label53).set_Location(new Point(66, 1));
			((Control)label53).set_Name("label53");
			((Control)label53).set_Size(new Size(30, 12));
			((Control)label53).set_TabIndex(26);
			((Control)label53).set_Text("ISAM");
			((Control)optComets).set_AutoSize(true);
			((Control)optComets).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optComets).set_Location(new Point(179, 18));
			((Control)optComets).set_Name("optComets");
			((Control)optComets).set_Size(new Size(66, 17));
			((Control)optComets).set_TabIndex(14);
			((Control)optComets).set_Text("Comets");
			((ButtonBase)optComets).set_UseVisualStyleBackColor(true);
			optComets.add_CheckedChanged((EventHandler)optComets_CheckedChanged);
			((Control)lblMagnitude).set_AutoSize(true);
			((Control)lblMagnitude).set_Font(new Font("Courier New", 9.75f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblMagnitude).set_Location(new Point(167, 115));
			((Control)lblMagnitude).set_Name("lblMagnitude");
			((Control)lblMagnitude).set_Size(new Size(23, 16));
			((Control)lblMagnitude).set_TabIndex(11);
			((Control)lblMagnitude).set_Text("Mv");
			((Control)lblDiameterKM).set_AutoSize(true);
			((Control)lblDiameterKM).set_Font(new Font("Courier New", 9.75f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblDiameterKM).set_Location(new Point(3, 131));
			((Control)lblDiameterKM).set_Name("lblDiameterKM");
			((Control)lblDiameterKM).set_Size(new Size(103, 16));
			((Control)lblDiameterKM).set_TabIndex(8);
			((Control)lblDiameterKM).set_Text("Dia       km");
			((Control)lblParallax).set_AutoSize(true);
			((Control)lblParallax).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblParallax).set_Location(new Point(13, 116));
			((Control)lblParallax).set_Name("lblParallax");
			((Control)lblParallax).set_Size(new Size(28, 14));
			((Control)lblParallax).set_TabIndex(7);
			((Control)lblParallax).set_Text("...");
			((Control)lblMotions).set_AutoSize(true);
			((Control)lblMotions).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblMotions).set_Location(new Point(3, 101));
			((Control)lblMotions).set_Name("lblMotions");
			((Control)lblMotions).set_Size(new Size(119, 14));
			((Control)lblMotions).set_TabIndex(6);
			((Control)lblMotions).set_Text("dRA         dDec");
			((Control)cmdGetAsteroid).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdGetAsteroid).set_Location(new Point(178, 59));
			((Control)cmdGetAsteroid).set_Name("cmdGetAsteroid");
			((Control)cmdGetAsteroid).set_Size(new Size(62, 35));
			((Control)cmdGetAsteroid).set_TabIndex(5);
			((Control)cmdGetAsteroid).set_Text("Get\r\ndetails");
			((ButtonBase)cmdGetAsteroid).set_UseVisualStyleBackColor(true);
			((Control)cmdGetAsteroid).add_Click((EventHandler)cmdGetAsteroid_Click);
			((Control)optPlanet).set_AutoSize(true);
			((Control)optPlanet).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optPlanet).set_Location(new Point(101, 18));
			((Control)optPlanet).set_Name("optPlanet");
			((Control)optPlanet).set_Size(new Size(67, 17));
			((Control)optPlanet).set_TabIndex(1);
			((Control)optPlanet).set_Text("Planets");
			((ButtonBase)optPlanet).set_UseVisualStyleBackColor(true);
			optPlanet.add_CheckedChanged((EventHandler)optPlanet_CheckedChanged);
			((Control)optAsteroid).set_AutoSize(true);
			optAsteroid.set_Checked(true);
			((Control)optAsteroid).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optAsteroid).set_Location(new Point(13, 18));
			((Control)optAsteroid).set_Name("optAsteroid");
			((Control)optAsteroid).set_Size(new Size(77, 17));
			((Control)optAsteroid).set_TabIndex(0);
			optAsteroid.set_TabStop(true);
			((Control)optAsteroid).set_Text("Asteroids");
			((ButtonBase)optAsteroid).set_UseVisualStyleBackColor(true);
			optAsteroid.add_CheckedChanged((EventHandler)optAsteroid_CheckedChanged);
			((Control)panelPlanets).get_Controls().Add((Control)(object)label12);
			((Control)panelPlanets).get_Controls().Add((Control)(object)label11);
			((Control)panelPlanets).get_Controls().Add((Control)(object)cmbMoons);
			((Control)panelPlanets).get_Controls().Add((Control)(object)cmbPlanets);
			((Control)panelPlanets).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)panelPlanets).set_Location(new Point(74, 52));
			((Control)panelPlanets).set_Name("panelPlanets");
			((Control)panelPlanets).set_Size(new Size(136, 60));
			((Control)panelPlanets).set_TabIndex(3);
			((Control)panelPlanets).set_Visible(false);
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(10, 36));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(38, 13));
			((Control)label12).set_TabIndex(3);
			((Control)label12).set_Text("Moon");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label11).set_Location(new Point(4, 8));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(43, 13));
			((Control)label11).set_TabIndex(0);
			((Control)label11).set_Text("Planet");
			cmbMoons.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbMoons).set_FormattingEnabled(true);
			((Control)cmbMoons).set_Location(new Point(50, 33));
			((Control)cmbMoons).set_Name("cmbMoons");
			((Control)cmbMoons).set_Size(new Size(84, 21));
			((Control)cmbMoons).set_TabIndex(0);
			cmbPlanets.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbPlanets).set_FormattingEnabled(true);
			cmbPlanets.get_Items().AddRange(new object[9] { "", "Mercury", "Venus", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune", "Pluto" });
			((Control)cmbPlanets).set_Location(new Point(50, 5));
			((Control)cmbPlanets).set_Name("cmbPlanets");
			((Control)cmbPlanets).set_Size(new Size(84, 21));
			((Control)cmbPlanets).set_TabIndex(1);
			cmbPlanets.add_SelectedIndexChanged((EventHandler)cmbPlanets_SelectedIndexChanged);
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label14).set_Location(new Point(21, 81));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(39, 13));
			((Control)label14).set_TabIndex(6);
			((Control)label14).set_Text("Name");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label13).set_Location(new Point(10, 58));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(50, 13));
			((Control)label13).set_TabIndex(2);
			((Control)label13).set_Text("Number");
			((Control)txtAsteroidName).set_BackColor(Color.AliceBlue);
			((Control)txtAsteroidName).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAsteroidName).set_Location(new Point(62, 78));
			((Control)txtAsteroidName).set_Name("txtAsteroidName");
			((Control)txtAsteroidName).set_Size(new Size(83, 20));
			((Control)txtAsteroidName).set_TabIndex(4);
			toolTip.SetToolTip((Control)(object)txtAsteroidName, componentResourceManager.GetString("txtAsteroidName.ToolTip"));
			((Control)txtAsteroidNumber).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAsteroidNumber).set_Location(new Point(62, 55));
			((Control)txtAsteroidNumber).set_Name("txtAsteroidNumber");
			((Control)txtAsteroidNumber).set_Size(new Size(55, 20));
			((Control)txtAsteroidNumber).set_TabIndex(3);
			toolTip.SetToolTip((Control)(object)txtAsteroidNumber, "For un-numbered asteroids, set this field to blank, and use the  Name field");
			((Control)txtAsteroidNumber).add_TextChanged((EventHandler)txtAsteroidNumber_TextChanged);
			((Control)label50).set_AutoSize(true);
			((Control)label50).set_Font(new Font("Symbol", 9.75f, FontStyle.Regular, GraphicsUnit.Point, 2));
			((Control)label50).set_Location(new Point(2, 115));
			((Control)label50).set_Name("label50");
			((Control)label50).set_Size(new Size(13, 16));
			((Control)label50).set_TabIndex(12);
			((Control)label50).set_Text("p");
			((Control)label51).set_AutoSize(true);
			((Control)label51).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label51).set_Location(new Point(31, 68));
			((Control)label51).set_Name("label51");
			((Control)label51).set_Size(new Size(16, 13));
			((Control)label51).set_TabIndex(13);
			((Control)label51).set_Text("or");
			((Control)grpObserver).set_BackColor(Color.Honeydew);
			((Control)grpObserver).get_Controls().Add((Control)(object)cmdHelpCountryCodes);
			((Control)grpObserver).get_Controls().Add((Control)(object)label21);
			((Control)grpObserver).get_Controls().Add((Control)(object)label47);
			((Control)grpObserver).get_Controls().Add((Control)(object)txtStateCountry);
			((Control)grpObserver).get_Controls().Add((Control)(object)label46);
			((Control)grpObserver).get_Controls().Add((Control)(object)txtLocatedNear);
			((Control)grpObserver).get_Controls().Add((Control)(object)label45);
			((Control)grpObserver).get_Controls().Add((Control)(object)label44);
			((Control)grpObserver).get_Controls().Add((Control)(object)chkEtAl);
			((Control)grpObserver).get_Controls().Add((Control)(object)txtObserver2);
			((Control)grpObserver).get_Controls().Add((Control)(object)txtObserver1);
			((Control)grpObserver).get_Controls().Add((Control)(object)panelDDD);
			((Control)grpObserver).get_Controls().Add((Control)(object)panelDMM);
			((Control)grpObserver).get_Controls().Add((Control)(object)txtAlt_ft);
			((Control)grpObserver).get_Controls().Add((Control)(object)panelDMS);
			((Control)grpObserver).get_Controls().Add((Control)(object)label20);
			((Control)grpObserver).get_Controls().Add((Control)(object)label19);
			((Control)grpObserver).get_Controls().Add((Control)(object)label18);
			((Control)grpObserver).get_Controls().Add((Control)(object)label17);
			((Control)grpObserver).get_Controls().Add((Control)(object)label16);
			((Control)grpObserver).get_Controls().Add((Control)(object)panel2);
			((Control)grpObserver).get_Controls().Add((Control)(object)cmbDatum);
			((Control)grpObserver).get_Controls().Add((Control)(object)cmbTelescope);
			((Control)grpObserver).get_Controls().Add((Control)(object)txtAperture);
			((Control)grpObserver).get_Controls().Add((Control)(object)txtAlt_m);
			((Control)grpObserver).get_Controls().Add((Control)(object)panel1);
			((Control)grpObserver).get_Controls().Add((Control)(object)label49);
			((Control)grpObserver).get_Controls().Add((Control)(object)cmdAltitude);
			((Control)grpObserver).get_Controls().Add((Control)(object)cmdAutoCompleteObserver);
			((Control)grpObserver).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpObserver).set_Location(new Point(9, 180));
			((Control)grpObserver).set_Name("grpObserver");
			((Control)grpObserver).set_Size(new Size(296, 208));
			((Control)grpObserver).set_TabIndex(4);
			grpObserver.set_TabStop(false);
			((Control)grpObserver).set_Text("Observer");
			((ButtonBase)cmdHelpCountryCodes).set_Image((Image)Resources.Help16x16);
			((Control)cmdHelpCountryCodes).set_Location(new Point(274, 47));
			((Control)cmdHelpCountryCodes).set_Name("cmdHelpCountryCodes");
			((Control)cmdHelpCountryCodes).set_Size(new Size(18, 19));
			((Control)cmdHelpCountryCodes).set_TabIndex(30);
			((ButtonBase)cmdHelpCountryCodes).set_UseVisualStyleBackColor(true);
			((Control)cmdHelpCountryCodes).add_Click((EventHandler)cmdHelpCountryCodes_Click);
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label21).set_Location(new Point(187, 51));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(86, 13));
			((Control)label21).set_TabIndex(28);
			((Control)label21).set_Text("2 or 3 letter code");
			((Control)label47).set_AutoSize(true);
			((Control)label47).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label47).set_Location(new Point(202, 62));
			((Control)label47).set_Name("label47");
			((Control)label47).set_Size(new Size(50, 26));
			((Control)label47).set_TabIndex(7);
			((Control)label47).set_Text("country\r\nor state");
			((Control)txtStateCountry).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtStateCountry).set_Location(new Point(257, 66));
			((Control)txtStateCountry).set_Name("txtStateCountry");
			((Control)txtStateCountry).set_Size(new Size(30, 20));
			((Control)txtStateCountry).set_TabIndex(8);
			((Control)txtStateCountry).add_Leave((EventHandler)txtStateCountry_Leave);
			((Control)label46).set_AutoSize(true);
			((Control)label46).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label46).set_Location(new Point(4, 63));
			((Control)label46).set_Name("label46");
			((Control)label46).set_Size(new Size(49, 26));
			((Control)label46).set_TabIndex(5);
			((Control)label46).set_Text("located\r\nnear");
			((Control)txtLocatedNear).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLocatedNear).set_Location(new Point(54, 66));
			((Control)txtLocatedNear).set_Name("txtLocatedNear");
			((Control)txtLocatedNear).set_Size(new Size(130, 20));
			((Control)txtLocatedNear).set_TabIndex(6);
			((Control)txtLocatedNear).add_Leave((EventHandler)txtLocatedNear_Leave);
			((Control)label45).set_AutoSize(true);
			((Control)label45).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label45).set_Location(new Point(157, 24));
			((Control)label45).set_Name("label45");
			((Control)label45).set_Size(new Size(50, 13));
			((Control)label45).set_TabIndex(2);
			((Control)label45).set_Text("Name 2");
			((Control)label44).set_AutoSize(true);
			((Control)label44).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label44).set_Location(new Point(3, 24));
			((Control)label44).set_Name("label44");
			((Control)label44).set_Size(new Size(50, 13));
			((Control)label44).set_TabIndex(0);
			((Control)label44).set_Text("Name 1");
			((Control)chkEtAl).set_AutoSize(true);
			((Control)chkEtAl).set_BackColor(Color.Honeydew);
			chkEtAl.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)chkEtAl).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkEtAl).set_Location(new Point(2, 44));
			((Control)chkEtAl).set_Name("chkEtAl");
			((Control)chkEtAl).set_Size(new Size(153, 17));
			((Control)chkEtAl).set_TabIndex(4);
			((Control)chkEtAl).set_Text("More than 2 observers");
			((ButtonBase)chkEtAl).set_UseVisualStyleBackColor(false);
			((Control)txtObserver2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtObserver2).set_Location(new Point(207, 21));
			((Control)txtObserver2).set_Name("txtObserver2");
			((Control)txtObserver2).set_Size(new Size(85, 20));
			((Control)txtObserver2).set_TabIndex(3);
			((Control)txtObserver2).add_Leave((EventHandler)txtObserver2_Leave);
			((Control)txtObserver1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtObserver1).set_Location(new Point(54, 21));
			((Control)txtObserver1).set_Name("txtObserver1");
			((Control)txtObserver1).set_Size(new Size(99, 20));
			((Control)txtObserver1).set_TabIndex(1);
			((Control)txtObserver1).add_Leave((EventHandler)txtObserver1_Leave);
			((Control)panelDDD).get_Controls().Add((Control)(object)txtLongDDD);
			((Control)panelDDD).get_Controls().Add((Control)(object)txtLatDDD);
			((Control)panelDDD).set_Location(new Point(260, 114));
			((Control)panelDDD).set_Name("panelDDD");
			((Control)panelDDD).set_Size(new Size(113, 52));
			((Control)panelDDD).set_TabIndex(10);
			((Control)txtLongDDD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongDDD).set_Location(new Point(4, 2));
			((Control)txtLongDDD).set_Name("txtLongDDD");
			((Control)txtLongDDD).set_Size(new Size(82, 20));
			((Control)txtLongDDD).set_TabIndex(0);
			((Control)txtLongDDD).add_Leave((EventHandler)txtLongDDD_Leave);
			((Control)txtLatDDD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatDDD).set_Location(new Point(4, 28));
			((Control)txtLatDDD).set_Name("txtLatDDD");
			((Control)txtLatDDD).set_Size(new Size(82, 20));
			((Control)txtLatDDD).set_TabIndex(1);
			((Control)txtLatDDD).add_Leave((EventHandler)txtLatDDD_Leave);
			((Control)panelDMM).get_Controls().Add((Control)(object)txtLongDmm);
			((Control)panelDMM).get_Controls().Add((Control)(object)txtLatMM);
			((Control)panelDMM).get_Controls().Add((Control)(object)txtLatDmm);
			((Control)panelDMM).get_Controls().Add((Control)(object)txtLongMM);
			((Control)panelDMM).set_Location(new Point(250, 74));
			((Control)panelDMM).set_Name("panelDMM");
			((Control)panelDMM).set_Size(new Size(113, 52));
			((Control)panelDMM).set_TabIndex(9);
			((Control)txtLongDmm).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongDmm).set_Location(new Point(4, 2));
			((Control)txtLongDmm).set_Name("txtLongDmm");
			((Control)txtLongDmm).set_Size(new Size(34, 20));
			((Control)txtLongDmm).set_TabIndex(0);
			((Control)txtLongDmm).add_Leave((EventHandler)txtLongDmm_Leave);
			((Control)txtLatMM).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatMM).set_Location(new Point(44, 28));
			((Control)txtLatMM).set_Name("txtLatMM");
			((Control)txtLatMM).set_Size(new Size(44, 20));
			((Control)txtLatMM).set_TabIndex(3);
			((Control)txtLatMM).add_Leave((EventHandler)txtLatMM_Leave);
			((Control)txtLatDmm).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatDmm).set_Location(new Point(4, 28));
			((Control)txtLatDmm).set_Name("txtLatDmm");
			((Control)txtLatDmm).set_Size(new Size(34, 20));
			((Control)txtLatDmm).set_TabIndex(2);
			((Control)txtLatDmm).add_Leave((EventHandler)txtLatDmm_Leave);
			((Control)txtLongMM).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongMM).set_Location(new Point(44, 2));
			((Control)txtLongMM).set_Name("txtLongMM");
			((Control)txtLongMM).set_Size(new Size(44, 20));
			((Control)txtLongMM).set_TabIndex(1);
			((Control)txtLongMM).add_Leave((EventHandler)txtLongMM_Leave);
			((Control)txtAlt_ft).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAlt_ft).set_Location(new Point(197, 184));
			((Control)txtAlt_ft).set_Name("txtAlt_ft");
			((Control)txtAlt_ft).set_Size(new Size(53, 20));
			((Control)txtAlt_ft).set_TabIndex(21);
			((Control)txtAlt_ft).set_Visible(false);
			((Control)txtAlt_ft).add_Leave((EventHandler)txtAlt_ft_Leave);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLongDeg);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLatSec);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLatMin);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLatDeg);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLongSec);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLongMin);
			((Control)panelDMS).set_Location(new Point(85, 121));
			((Control)panelDMS).set_Name("panelDMS");
			((Control)panelDMS).set_Size(new Size(113, 52));
			((Control)panelDMS).set_TabIndex(14);
			((Control)txtLongDeg).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongDeg).set_Location(new Point(4, 2));
			((Control)txtLongDeg).set_Name("txtLongDeg");
			((Control)txtLongDeg).set_Size(new Size(34, 20));
			((Control)txtLongDeg).set_TabIndex(0);
			((Control)txtLongDeg).add_Leave((EventHandler)txtLongDeg_Leave);
			((Control)txtLatSec).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatSec).set_Location(new Point(71, 28));
			((Control)txtLatSec).set_Name("txtLatSec");
			((Control)txtLatSec).set_Size(new Size(39, 20));
			((Control)txtLatSec).set_TabIndex(5);
			((Control)txtLatSec).add_Leave((EventHandler)txtLatSec_Leave);
			((Control)txtLatMin).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatMin).set_Location(new Point(44, 28));
			((Control)txtLatMin).set_Name("txtLatMin");
			((Control)txtLatMin).set_Size(new Size(21, 20));
			((Control)txtLatMin).set_TabIndex(4);
			((Control)txtLatMin).add_Leave((EventHandler)txtLatMin_Leave);
			((Control)txtLatDeg).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatDeg).set_Location(new Point(4, 28));
			((Control)txtLatDeg).set_Name("txtLatDeg");
			((Control)txtLatDeg).set_Size(new Size(34, 20));
			((Control)txtLatDeg).set_TabIndex(3);
			((Control)txtLatDeg).add_Leave((EventHandler)txtLatDeg_Leave);
			((Control)txtLongSec).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongSec).set_Location(new Point(71, 2));
			((Control)txtLongSec).set_Name("txtLongSec");
			((Control)txtLongSec).set_Size(new Size(40, 20));
			((Control)txtLongSec).set_TabIndex(2);
			((Control)txtLongSec).add_Leave((EventHandler)txtLongSec_Leave);
			((Control)txtLongMin).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongMin).set_Location(new Point(44, 2));
			((Control)txtLongMin).set_Name("txtLongMin");
			((Control)txtLongMin).set_Size(new Size(21, 20));
			((Control)txtLongMin).set_TabIndex(1);
			((Control)txtLongMin).add_Leave((EventHandler)txtLongMin_Leave);
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label20).set_Location(new Point(131, 100));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(23, 13));
			((Control)label20).set_TabIndex(11);
			((Control)label20).set_Text("cm");
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label19).set_Location(new Point(11, 181));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(43, 13));
			((Control)label19).set_TabIndex(17);
			((Control)label19).set_Text("Datum");
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label18).set_Location(new Point(33, 148));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(53, 13));
			((Control)label18).set_TabIndex(15);
			((Control)label18).set_Text("Latitude");
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label17).set_Location(new Point(7, 126));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(79, 13));
			((Control)label17).set_TabIndex(13);
			((Control)label17).set_Text("E. Longitude");
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label16).set_Location(new Point(33, 99));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(55, 13));
			((Control)label16).set_TabIndex(9);
			((Control)label16).set_Text("Aperture");
			((Control)panel2).get_Controls().Add((Control)(object)optFeet);
			((Control)panel2).get_Controls().Add((Control)(object)optMeters);
			((Control)panel2).set_Location(new Point(254, 171));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(36, 34));
			((Control)panel2).set_TabIndex(22);
			((Control)optFeet).set_AutoSize(true);
			((Control)optFeet).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optFeet).set_Location(new Point(2, 16));
			((Control)optFeet).set_Name("optFeet");
			((Control)optFeet).set_Size(new Size(33, 17));
			((Control)optFeet).set_TabIndex(1);
			optFeet.set_TabStop(true);
			((Control)optFeet).set_Text("ft");
			((ButtonBase)optFeet).set_UseVisualStyleBackColor(true);
			optFeet.add_CheckedChanged((EventHandler)optFeet_CheckedChanged);
			((Control)optMeters).set_AutoSize(true);
			optMeters.set_Checked(true);
			((Control)optMeters).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optMeters).set_Location(new Point(2, 1));
			((Control)optMeters).set_Name("optMeters");
			((Control)optMeters).set_Size(new Size(34, 17));
			((Control)optMeters).set_TabIndex(0);
			optMeters.set_TabStop(true);
			((Control)optMeters).set_Text("m");
			((ButtonBase)optMeters).set_UseVisualStyleBackColor(true);
			optMeters.add_CheckedChanged((EventHandler)optMeters_CheckedChanged);
			cmbDatum.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbDatum).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbDatum).set_FormattingEnabled(true);
			cmbDatum.get_Items().AddRange(new object[6] { "WGS84", "NAD1927", "ED1950", "Tokyo", "GB1936", "?" });
			((Control)cmbDatum).set_Location(new Point(57, 178));
			((Control)cmbDatum).set_Name("cmbDatum");
			((Control)cmbDatum).set_Size(new Size(64, 21));
			((Control)cmbDatum).set_TabIndex(18);
			cmbTelescope.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbTelescope).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbTelescope).set_FormattingEnabled(true);
			cmbTelescope.get_Items().AddRange(new object[9] { "?", "Refractor", "Newtonian", "SCT", "Dobsonian", "Binoculars", "Other", "None", "Electronic" });
			((Control)cmbTelescope).set_Location(new Point(162, 96));
			((Control)cmbTelescope).set_Name("cmbTelescope");
			((Control)cmbTelescope).set_Size(new Size(75, 21));
			((Control)cmbTelescope).set_TabIndex(12);
			cmbTelescope.add_SelectedIndexChanged((EventHandler)cmbTelescope_SelectedIndexChanged);
			((Control)txtAperture).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAperture).set_Location(new Point(90, 96));
			((Control)txtAperture).set_Name("txtAperture");
			((Control)txtAperture).set_Size(new Size(38, 20));
			((Control)txtAperture).set_TabIndex(10);
			((Control)txtAlt_m).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAlt_m).set_Location(new Point(197, 177));
			((Control)txtAlt_m).set_Name("txtAlt_m");
			((Control)txtAlt_m).set_Size(new Size(53, 20));
			((Control)txtAlt_m).set_TabIndex(20);
			((Control)txtAlt_m).add_Leave((EventHandler)txtAlt_m_Leave);
			((Control)panel1).get_Controls().Add((Control)(object)optDMM);
			((Control)panel1).get_Controls().Add((Control)(object)optDDD);
			((Control)panel1).get_Controls().Add((Control)(object)optDMS);
			((Control)panel1).set_Location(new Point(221, 118));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(61, 56));
			((Control)panel1).set_TabIndex(16);
			((Control)optDMM).set_AutoSize(true);
			((Control)optDMM).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optDMM).set_Location(new Point(3, 20));
			((Control)optDMM).set_Name("optDMM");
			((Control)optDMM).set_Size(new Size(54, 17));
			((Control)optDMM).set_TabIndex(1);
			optDMM.set_TabStop(true);
			((Control)optDMM).set_Text("dm.m");
			((ButtonBase)optDMM).set_UseVisualStyleBackColor(true);
			optDMM.add_CheckedChanged((EventHandler)optDMM_CheckedChanged);
			((Control)optDDD).set_AutoSize(true);
			((Control)optDDD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optDDD).set_Location(new Point(3, 35));
			((Control)optDDD).set_Name("optDDD");
			((Control)optDDD).set_Size(new Size(50, 17));
			((Control)optDDD).set_TabIndex(2);
			optDDD.set_TabStop(true);
			((Control)optDDD).set_Text("d.dd");
			((ButtonBase)optDDD).set_UseVisualStyleBackColor(true);
			optDDD.add_CheckedChanged((EventHandler)optDDD_CheckedChanged);
			((Control)optDMS).set_AutoSize(true);
			optDMS.set_Checked(true);
			((Control)optDMS).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optDMS).set_Location(new Point(3, 5));
			((Control)optDMS).set_Name("optDMS");
			((Control)optDMS).set_Size(new Size(47, 17));
			((Control)optDMS).set_TabIndex(0);
			optDMS.set_TabStop(true);
			((Control)optDMS).set_Text("dms");
			((ButtonBase)optDMS).set_UseVisualStyleBackColor(true);
			optDMS.add_CheckedChanged((EventHandler)optDMS_CheckedChanged);
			((Control)label49).set_AutoSize(true);
			((Control)label49).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label49).set_Location(new Point(158, 10));
			((Control)label49).set_Name("label49");
			((Control)label49).set_Size(new Size(46, 13));
			((Control)label49).set_TabIndex(29);
			((Control)label49).set_Text("Optional");
			((Control)cmdAltitude).set_BackColor(Color.Honeydew);
			((ButtonBase)cmdAltitude).get_FlatAppearance().set_BorderColor(Color.LimeGreen);
			((ButtonBase)cmdAltitude).set_FlatStyle((FlatStyle)0);
			((Control)cmdAltitude).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdAltitude).set_Location(new Point(143, 177));
			((Control)cmdAltitude).set_Name("cmdAltitude");
			((Control)cmdAltitude).set_Size(new Size(60, 21));
			((Control)cmdAltitude).set_TabIndex(19);
			((Control)cmdAltitude).set_Text("Altitude");
			((ButtonBase)cmdAltitude).set_UseVisualStyleBackColor(false);
			((Control)cmdAltitude).add_Click((EventHandler)cmdAltitude_Click);
			((Control)cmdAutoCompleteObserver).set_AutoSize(true);
			((Control)cmdAutoCompleteObserver).set_BackColor(Color.Wheat);
			((Control)cmdAutoCompleteObserver).set_Font(new Font("Consolas", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdAutoCompleteObserver).set_Location(new Point(241, 10));
			((Control)cmdAutoCompleteObserver).set_Name("cmdAutoCompleteObserver");
			((Control)cmdAutoCompleteObserver).set_Size(new Size(50, 10));
			((Control)cmdAutoCompleteObserver).set_TabIndex(32);
			((Control)cmdAutoCompleteObserver).set_Text("Auto-fill");
			toolTip.SetToolTip((Control)(object)cmdAutoCompleteObserver, "Open a form to auto-fill Observer details");
			((Control)cmdAutoCompleteObserver).add_Click((EventHandler)cmdAutoCompleteObserver_Click);
			((Control)grpHistory).set_BackColor(Color.PaleTurquoise);
			((Control)grpHistory).get_Controls().Add((Control)(object)panel3);
			((Control)grpHistory).get_Controls().Add((Control)(object)optByPlanets);
			((Control)grpHistory).get_Controls().Add((Control)(object)optbyChords);
			((Control)grpHistory).get_Controls().Add((Control)(object)optByClass);
			((Control)grpHistory).get_Controls().Add((Control)(object)cmbAsteroidClasses);
			((Control)grpHistory).get_Controls().Add((Control)(object)optByRecords);
			((Control)grpHistory).get_Controls().Add((Control)(object)optByNumber);
			((Control)grpHistory).get_Controls().Add((Control)(object)optByName);
			((Control)grpHistory).get_Controls().Add((Control)(object)optByDate);
			((Control)grpHistory).get_Controls().Add((Control)(object)cmbNumberRange);
			((Control)grpHistory).get_Controls().Add((Control)(object)cmbAlpha);
			((Control)grpHistory).get_Controls().Add((Control)(object)cmbYearRange);
			((Control)grpHistory).get_Controls().Add((Control)(object)cmbHistorical);
			((Control)grpHistory).get_Controls().Add((Control)(object)lblCurrentSelected);
			((Control)grpHistory).get_Controls().Add((Control)(object)lblNumbers);
			((Control)grpHistory).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpHistory).set_Location(new Point(739, 28));
			((Control)grpHistory).set_Name("grpHistory");
			((Control)grpHistory).set_Size(new Size(322, 149));
			((Control)grpHistory).set_TabIndex(3);
			grpHistory.set_TabStop(false);
			((Control)grpHistory).set_Text("Historical observations");
			panel3.set_BorderStyle((BorderStyle)2);
			((Control)panel3).get_Controls().Add((Control)(object)ChkUnfitted);
			((Control)panel3).get_Controls().Add((Control)(object)chkShapeModelsOnly);
			((Control)panel3).set_Location(new Point(216, 4));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(99, 35));
			((Control)panel3).set_TabIndex(8);
			((Control)ChkUnfitted).set_AutoSize(true);
			((Control)ChkUnfitted).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)ChkUnfitted).set_ForeColor(Color.Brown);
			((Control)ChkUnfitted).set_Location(new Point(0, 16));
			((Control)ChkUnfitted).set_Name("ChkUnfitted");
			((Control)ChkUnfitted).set_Size(new Size(99, 17));
			((Control)ChkUnfitted).set_TabIndex(1);
			((Control)ChkUnfitted).set_Text("Unfitted Models");
			((ButtonBase)ChkUnfitted).set_UseVisualStyleBackColor(true);
			ChkUnfitted.add_CheckedChanged((EventHandler)ChkUnfitted_CheckedChanged);
			((Control)chkShapeModelsOnly).set_AutoSize(true);
			((Control)chkShapeModelsOnly).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)chkShapeModelsOnly).set_Location(new Point(0, 0));
			((Control)chkShapeModelsOnly).set_Name("chkShapeModelsOnly");
			((Control)chkShapeModelsOnly).set_Size(new Size(94, 17));
			((Control)chkShapeModelsOnly).set_TabIndex(0);
			((Control)chkShapeModelsOnly).set_Text("Shape Models");
			((ButtonBase)chkShapeModelsOnly).set_TextAlign(ContentAlignment.MiddleCenter);
			((ButtonBase)chkShapeModelsOnly).set_UseVisualStyleBackColor(true);
			chkShapeModelsOnly.add_CheckedChanged((EventHandler)chkShapeModelsOnly_CheckedChanged);
			((Control)optByPlanets).set_AutoSize(true);
			((Control)optByPlanets).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optByPlanets).set_Location(new Point(218, 76));
			((Control)optByPlanets).set_Name("optByPlanets");
			((Control)optByPlanets).set_Size(new Size(107, 30));
			((Control)optByPlanets).set_TabIndex(11);
			((Control)optByPlanets).set_Text("Planets && Comets\r\nonly\r\n");
			((ButtonBase)optByPlanets).set_UseVisualStyleBackColor(true);
			optByPlanets.add_CheckedChanged((EventHandler)optByPlanets_CheckedChanged);
			((Control)optbyChords).set_AutoSize(true);
			((Control)optbyChords).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optbyChords).set_Location(new Point(218, 58));
			((Control)optbyChords).set_Name("optbyChords");
			((Control)optbyChords).set_Size(new Size(82, 17));
			((Control)optbyChords).set_TabIndex(10);
			((Control)optbyChords).set_Text("by # Chords");
			((ButtonBase)optbyChords).set_UseVisualStyleBackColor(true);
			optbyChords.add_CheckedChanged((EventHandler)optbyChords_CheckedChanged);
			((Control)optByClass).set_AutoSize(true);
			optByClass.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)optByClass).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optByClass).set_Location(new Point(13, 99));
			((Control)optByClass).set_Name("optByClass");
			((Control)optByClass).set_Size(new Size(64, 17));
			((Control)optByClass).set_TabIndex(6);
			((Control)optByClass).set_Text("by Class");
			((ButtonBase)optByClass).set_UseVisualStyleBackColor(true);
			optByClass.add_CheckedChanged((EventHandler)optClass_CheckedChanged);
			cmbAsteroidClasses.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbAsteroidClasses).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbAsteroidClasses).set_FormattingEnabled(true);
			cmbAsteroidClasses.get_Items().AddRange(new object[7] { "Amor", "Apollo", "Aten", "Centaur", "PHA", "TNO", "Trojan" });
			((Control)cmbAsteroidClasses).set_Location(new Point(81, 97));
			((Control)cmbAsteroidClasses).set_Name("cmbAsteroidClasses");
			((Control)cmbAsteroidClasses).set_Size(new Size(81, 21));
			((Control)cmbAsteroidClasses).set_TabIndex(7);
			cmbAsteroidClasses.add_SelectedIndexChanged((EventHandler)cmbAsteroidClasses_SelectedIndexChanged);
			((Control)optByRecords).set_AutoSize(true);
			((Control)optByRecords).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optByRecords).set_Location(new Point(218, 40));
			((Control)optByRecords).set_Name("optByRecords");
			((Control)optByRecords).set_Size(new Size(89, 17));
			((Control)optByRecords).set_TabIndex(9);
			((Control)optByRecords).set_Text("by # Records");
			((ButtonBase)optByRecords).set_UseVisualStyleBackColor(true);
			optByRecords.add_CheckedChanged((EventHandler)optByRecords_CheckedChanged);
			((Control)optByNumber).set_AutoSize(true);
			optByNumber.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)optByNumber).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optByNumber).set_Location(new Point(1, 74));
			((Control)optByNumber).set_Name("optByNumber");
			((Control)optByNumber).set_Size(new Size(76, 17));
			((Control)optByNumber).set_TabIndex(4);
			((Control)optByNumber).set_Text("by Number");
			((ButtonBase)optByNumber).set_UseVisualStyleBackColor(true);
			optByNumber.add_CheckedChanged((EventHandler)optByNumber_CheckedChanged);
			((Control)optByName).set_AutoSize(true);
			optByName.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)optByName).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optByName).set_Location(new Point(10, 48));
			((Control)optByName).set_Name("optByName");
			((Control)optByName).set_Size(new Size(67, 17));
			((Control)optByName).set_TabIndex(2);
			((Control)optByName).set_Text("by Name");
			((ButtonBase)optByName).set_UseVisualStyleBackColor(true);
			optByName.add_CheckedChanged((EventHandler)optByName_CheckedChanged);
			((Control)optByDate).set_AutoSize(true);
			optByDate.set_CheckAlign(ContentAlignment.MiddleRight);
			optByDate.set_Checked(true);
			((Control)optByDate).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optByDate).set_Location(new Point(15, 22));
			((Control)optByDate).set_Name("optByDate");
			((Control)optByDate).set_Size(new Size(62, 17));
			((Control)optByDate).set_TabIndex(0);
			optByDate.set_TabStop(true);
			((Control)optByDate).set_Text("by Date");
			((ButtonBase)optByDate).set_UseVisualStyleBackColor(true);
			optByDate.add_CheckedChanged((EventHandler)optByDate_CheckedChanged);
			cmbNumberRange.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbNumberRange).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbNumberRange).set_FormattingEnabled(true);
			cmbNumberRange.get_Items().AddRange(new object[18]
			{
				"All", "1 - 49", "50 - 99", "100 - 149", "150 - 199", "200 - 299", "300 - 399", "400 - 549", "550 - 699", "700 - 899",
				"900 - 1299", "1300 - 1999", "2000 - 2999", "3000 - 4999", "5000 - 9999", "10000 - 49999", ">50000", "Planets/Moons"
			});
			((Control)cmbNumberRange).set_Location(new Point(81, 71));
			cmbNumberRange.set_MaxDropDownItems(12);
			((Control)cmbNumberRange).set_Name("cmbNumberRange");
			((Control)cmbNumberRange).set_Size(new Size(123, 22));
			((Control)cmbNumberRange).set_TabIndex(5);
			cmbNumberRange.add_SelectedIndexChanged((EventHandler)cmbNumberRange_SelectedIndexChanged);
			cmbAlpha.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbAlpha).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbAlpha).set_FormattingEnabled(true);
			cmbAlpha.get_Items().AddRange(new object[37]
			{
				"All", "A", "B", "C", "D", "E", "F", "G", "H", "I",
				"J", "K", "L", "M", "N", "O", "P", "Q", "R", "S",
				"T", "U", "V", "W", "X", "Y", "Z", "1", "2", "3",
				"4", "5", "6", "7", "8", "9", "0"
			});
			((Control)cmbAlpha).set_Location(new Point(81, 45));
			cmbAlpha.set_MaxDropDownItems(30);
			((Control)cmbAlpha).set_Name("cmbAlpha");
			((Control)cmbAlpha).set_Size(new Size(51, 22));
			((Control)cmbAlpha).set_TabIndex(3);
			cmbAlpha.add_SelectedIndexChanged((EventHandler)cmbAlpha_SelectedIndexChanged);
			cmbYearRange.set_DrawMode((DrawMode)2);
			cmbYearRange.set_DropDownHeight(800);
			cmbYearRange.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbYearRange).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbYearRange).set_FormattingEnabled(true);
			cmbYearRange.set_IntegralHeight(false);
			cmbYearRange.get_Items().AddRange(new object[3] { "1990-1999", "1800-1989", "All" });
			((Control)cmbYearRange).set_Location(new Point(81, 19));
			cmbYearRange.set_MaxDropDownItems(20);
			((Control)cmbYearRange).set_Name("cmbYearRange");
			((Control)cmbYearRange).set_Size(new Size(130, 21));
			((Control)cmbYearRange).set_TabIndex(1);
			cmbYearRange.add_DrawItem(new DrawItemEventHandler(cmbYearRange_DrawItem));
			cmbYearRange.add_SelectedIndexChanged((EventHandler)cmbYearRange_SelectedIndexChanged);
			((Control)cmbHistorical).set_BackColor(SystemColors.Window);
			cmbHistorical.set_DropDownHeight(600);
			cmbHistorical.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbHistorical).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbHistorical).set_FormattingEnabled(true);
			cmbHistorical.set_IntegralHeight(false);
			((Control)cmbHistorical).set_Location(new Point(5, 123));
			cmbHistorical.set_MaxDropDownItems(20);
			((Control)cmbHistorical).set_Name("cmbHistorical");
			((Control)cmbHistorical).set_Size(new Size(310, 22));
			((Control)cmbHistorical).set_TabIndex(12);
			cmbHistorical.add_SelectedIndexChanged((EventHandler)cmbHistorical_SelectedIndexChanged);
			((Control)lblCurrentSelected).set_AutoSize(true);
			((Control)lblCurrentSelected).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblCurrentSelected).set_ForeColor(Color.Firebrick);
			((Control)lblCurrentSelected).set_Location(new Point(287, 106));
			((Control)lblCurrentSelected).set_Name("lblCurrentSelected");
			((Control)lblCurrentSelected).set_Size(new Size(18, 15));
			((Control)lblCurrentSelected).set_TabIndex(14);
			((Control)lblCurrentSelected).set_Text("-1");
			((Control)lblNumbers).set_AutoSize(true);
			((Control)lblNumbers).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblNumbers).set_Location(new Point(201, 105));
			((Control)lblNumbers).set_Name("lblNumbers");
			((Control)lblNumbers).set_Size(new Size(28, 15));
			((Control)lblNumbers).set_TabIndex(13);
			((Control)lblNumbers).set_Text("A/B");
			((Control)grpTimes).set_BackColor(Color.Honeydew);
			((Control)grpTimes).get_Controls().Add((Control)(object)panel4);
			((Control)grpTimes).get_Controls().Add((Control)(object)label30);
			((Control)grpTimes).get_Controls().Add((Control)(object)label29);
			((Control)grpTimes).get_Controls().Add((Control)(object)label28);
			((Control)grpTimes).get_Controls().Add((Control)(object)label27);
			((Control)grpTimes).get_Controls().Add((Control)(object)label26);
			((Control)grpTimes).get_Controls().Add((Control)(object)label25);
			((Control)grpTimes).get_Controls().Add((Control)(object)label24);
			((Control)grpTimes).get_Controls().Add((Control)(object)label23);
			((Control)grpTimes).get_Controls().Add((Control)(object)label22);
			((Control)grpTimes).get_Controls().Add((Control)(object)chkNotSeen);
			((Control)grpTimes).get_Controls().Add((Control)(object)txtD_Min);
			((Control)grpTimes).get_Controls().Add((Control)(object)cmbD_Wt);
			((Control)grpTimes).get_Controls().Add((Control)(object)txtD_Sec);
			((Control)grpTimes).get_Controls().Add((Control)(object)txtR_Hr);
			((Control)grpTimes).get_Controls().Add((Control)(object)txtR_Min);
			((Control)grpTimes).get_Controls().Add((Control)(object)txtR_Sec);
			((Control)grpTimes).get_Controls().Add((Control)(object)txtD_Uncert);
			((Control)grpTimes).get_Controls().Add((Control)(object)txtR_Uncert);
			((Control)grpTimes).get_Controls().Add((Control)(object)txtD_PEq);
			((Control)grpTimes).get_Controls().Add((Control)(object)txtR_PEq);
			((Control)grpTimes).get_Controls().Add((Control)(object)txtShift);
			((Control)grpTimes).get_Controls().Add((Control)(object)cmbR_Wt);
			((Control)grpTimes).get_Controls().Add((Control)(object)txtD_Hr);
			((Control)grpTimes).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpTimes).set_Location(new Point(325, 179));
			((Control)grpTimes).set_Name("grpTimes");
			((Control)grpTimes).set_Size(new Size(252, 208));
			((Control)grpTimes).set_TabIndex(5);
			grpTimes.set_TabStop(false);
			((Control)grpTimes).set_Text("Times  [UT]");
			((Control)panel4).set_BackColor(Color.MintCream);
			panel4.set_BorderStyle((BorderStyle)2);
			((Control)panel4).get_Controls().Add((Control)(object)chkRing);
			((Control)panel4).get_Controls().Add((Control)(object)chkUnseen);
			((Control)panel4).get_Controls().Add((Control)(object)label31);
			((Control)panel4).get_Controls().Add((Control)(object)chkMiss);
			((Control)panel4).get_Controls().Add((Control)(object)chkPredicted);
			((Control)panel4).get_Controls().Add((Control)(object)chkSatellite);
			((Control)panel4).get_Controls().Add((Control)(object)chkDoubleStar);
			((Control)panel4).set_Location(new Point(8, 127));
			((Control)panel4).set_Name("panel4");
			((Control)panel4).set_Size(new Size(237, 76));
			((Control)panel4).set_TabIndex(30);
			chkRing.set_AutoCheck(false);
			((Control)chkRing).set_AutoSize(true);
			((Control)chkRing).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkRing).set_Location(new Point(151, 38));
			((Control)chkRing).set_Name("chkRing");
			((Control)chkRing).set_Size(new Size(52, 17));
			((Control)chkRing).set_TabIndex(5);
			((Control)chkRing).set_Text("Ring");
			toolTip.SetToolTip((Control)(object)chkRing, componentResourceManager.GetString("chkRing.ToolTip"));
			((ButtonBase)chkRing).set_UseVisualStyleBackColor(true);
			((Control)chkRing).add_Click((EventHandler)chkRing_Click);
			((Control)chkUnseen).set_AutoSize(true);
			((Control)chkUnseen).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkUnseen).set_Location(new Point(4, 55));
			((Control)chkUnseen).set_Name("chkUnseen");
			((Control)chkUnseen).set_Size(new Size(134, 17));
			((Control)chkUnseen).set_TabIndex(3);
			((Control)chkUnseen).set_Text("unseen Satellite parent");
			toolTip.SetToolTip((Control)(object)chkUnseen, componentResourceManager.GetString("chkUnseen.ToolTip"));
			((ButtonBase)chkUnseen).set_UseVisualStyleBackColor(true);
			((Control)label31).set_AutoSize(true);
			((Control)label31).set_ForeColor(Color.DarkGreen);
			((Control)label31).set_Location(new Point(54, -1));
			((Control)label31).set_Name("label31");
			((Control)label31).set_Size(new Size(124, 20));
			((Control)label31).set_TabIndex(0);
			((Control)label31).set_Text("Event qualifier");
			((Control)chkMiss).set_AutoSize(true);
			((Control)chkMiss).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkMiss).set_Location(new Point(4, 21));
			((Control)chkMiss).set_Name("chkMiss");
			((Control)chkMiss).set_Size(new Size(128, 17));
			((Control)chkMiss).set_TabIndex(1);
			((Control)chkMiss).set_Text("No occn detected");
			((ButtonBase)chkMiss).set_UseVisualStyleBackColor(true);
			((Control)chkPredicted).set_AutoSize(true);
			((Control)chkPredicted).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkPredicted).set_Location(new Point(151, 21));
			((Control)chkPredicted).set_Name("chkPredicted");
			((Control)chkPredicted).set_Size(new Size(83, 17));
			((Control)chkPredicted).set_TabIndex(2);
			((Control)chkPredicted).set_Text("Prediction");
			((ButtonBase)chkPredicted).set_UseVisualStyleBackColor(true);
			((Control)chkPredicted).add_Leave((EventHandler)chkPredicted_Leave);
			chkSatellite.set_AutoCheck(false);
			((Control)chkSatellite).set_AutoSize(true);
			((Control)chkSatellite).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkSatellite).set_Location(new Point(151, 55));
			((Control)chkSatellite).set_Name("chkSatellite");
			((Control)chkSatellite).set_Size(new Size(72, 17));
			((Control)chkSatellite).set_TabIndex(6);
			((Control)chkSatellite).set_Text("Satellite");
			((ButtonBase)chkSatellite).set_UseVisualStyleBackColor(true);
			((Control)chkSatellite).add_Click((EventHandler)chkSatellite_Click);
			chkDoubleStar.set_AutoCheck(false);
			((Control)chkDoubleStar).set_AutoSize(true);
			((Control)chkDoubleStar).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkDoubleStar).set_Location(new Point(4, 38));
			((Control)chkDoubleStar).set_Name("chkDoubleStar");
			((Control)chkDoubleStar).set_Size(new Size(146, 17));
			((Control)chkDoubleStar).set_TabIndex(4);
			((Control)chkDoubleStar).set_Text("Companion (2nd) star");
			((ButtonBase)chkDoubleStar).set_UseVisualStyleBackColor(true);
			((Control)chkDoubleStar).add_Click((EventHandler)chkDoubleStar_Click);
			((Control)label30).set_AutoSize(true);
			((Control)label30).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label30).set_Location(new Point(101, 104));
			((Control)label30).set_Name("label30");
			((Control)label30).set_Size(new Size(78, 13));
			((Control)label30).set_TabIndex(20);
			((Control)label30).set_Text("Time shift (sec)");
			((Control)label29).set_AutoSize(true);
			((Control)label29).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label29).set_Location(new Point(214, 28));
			((Control)label29).set_Name("label29");
			((Control)label29).set_Size(new Size(23, 13));
			((Control)label29).set_TabIndex(5);
			((Control)label29).set_Text("Wt");
			((Control)label28).set_AutoSize(true);
			((Control)label28).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label28).set_Location(new Point(173, 27));
			((Control)label28).set_Name("label28");
			((Control)label28).set_Size(new Size(30, 13));
			((Control)label28).set_TabIndex(4);
			((Control)label28).set_Text("PEq");
			((Control)label27).set_AutoSize(true);
			((Control)label27).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label27).set_Location(new Point(133, 27));
			((Control)label27).set_Name("label27");
			((Control)label27).set_Size(new Size(24, 13));
			((Control)label27).set_TabIndex(3);
			((Control)label27).set_Text("+/-");
			((Control)label26).set_AutoSize(true);
			((Control)label26).set_Font(new Font("Microsoft Sans Serif", 9.75f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label26).set_Location(new Point(1, 44));
			((Control)label26).set_Name("label26");
			((Control)label26).set_Size(new Size(18, 16));
			((Control)label26).set_TabIndex(6);
			((Control)label26).set_Text("D");
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Font(new Font("Microsoft Sans Serif", 9.75f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label25).set_Location(new Point(1, 74));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(18, 16));
			((Control)label25).set_TabIndex(13);
			((Control)label25).set_Text("R");
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label24).set_Location(new Point(24, 27));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(14, 13));
			((Control)label24).set_TabIndex(0);
			((Control)label24).set_Text("h");
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label23).set_Location(new Point(48, 27));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(16, 13));
			((Control)label23).set_TabIndex(1);
			((Control)label23).set_Text("m");
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label22).set_Location(new Point(77, 27));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(13, 13));
			((Control)label22).set_TabIndex(2);
			((Control)label22).set_Text("s");
			((Control)chkNotSeen).set_AutoSize(true);
			((Control)chkNotSeen).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)chkNotSeen).set_ForeColor(Color.ForestGreen);
			((Control)chkNotSeen).set_Location(new Point(11, 103));
			((Control)chkNotSeen).set_Name("chkNotSeen");
			((Control)chkNotSeen).set_Size(new Size(83, 17));
			((Control)chkNotSeen).set_TabIndex(28);
			((Control)chkNotSeen).set_Text("Clouded out");
			toolTip.SetToolTip((Control)(object)chkNotSeen, "This  is provided to cater for observers who\r\nwere 'clouded out' but want recognition.\r\nSuch observations should be excluded from\r\nany final report.\r\n");
			((ButtonBase)chkNotSeen).set_UseVisualStyleBackColor(true);
			((Control)txtD_Min).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtD_Min).set_Location(new Point(45, 42));
			((Control)txtD_Min).set_Name("txtD_Min");
			((Control)txtD_Min).set_Size(new Size(22, 20));
			((Control)txtD_Min).set_TabIndex(8);
			cmbD_Wt.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbD_Wt).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbD_Wt).set_FormattingEnabled(true);
			cmbD_Wt.get_Items().AddRange(new object[7] { "", "0", "1", "2", "3", "4", "5" });
			((Control)cmbD_Wt).set_Location(new Point(209, 42));
			((Control)cmbD_Wt).set_Name("cmbD_Wt");
			((Control)cmbD_Wt).set_Size(new Size(33, 21));
			((Control)cmbD_Wt).set_TabIndex(12);
			toolTip.SetToolTip((Control)(object)cmbD_Wt, componentResourceManager.GetString("cmbD_Wt.ToolTip"));
			((Control)txtD_Sec).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtD_Sec).set_Location(new Point(70, 42));
			((Control)txtD_Sec).set_Name("txtD_Sec");
			((Control)txtD_Sec).set_Size(new Size(48, 20));
			((Control)txtD_Sec).set_TabIndex(9);
			((Control)txtR_Hr).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtR_Hr).set_Location(new Point(20, 72));
			((Control)txtR_Hr).set_Name("txtR_Hr");
			((Control)txtR_Hr).set_Size(new Size(22, 20));
			((Control)txtR_Hr).set_TabIndex(14);
			((Control)txtR_Min).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtR_Min).set_Location(new Point(45, 72));
			((Control)txtR_Min).set_Name("txtR_Min");
			((Control)txtR_Min).set_Size(new Size(22, 20));
			((Control)txtR_Min).set_TabIndex(15);
			((Control)txtR_Sec).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtR_Sec).set_Location(new Point(70, 72));
			((Control)txtR_Sec).set_Name("txtR_Sec");
			((Control)txtR_Sec).set_Size(new Size(48, 20));
			((Control)txtR_Sec).set_TabIndex(16);
			((Control)txtD_Uncert).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtD_Uncert).set_Location(new Point(129, 42));
			((Control)txtD_Uncert).set_Name("txtD_Uncert");
			((Control)txtD_Uncert).set_Size(new Size(39, 20));
			((Control)txtD_Uncert).set_TabIndex(10);
			((Control)txtR_Uncert).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtR_Uncert).set_Location(new Point(129, 72));
			((Control)txtR_Uncert).set_Name("txtR_Uncert");
			((Control)txtR_Uncert).set_Size(new Size(39, 20));
			((Control)txtR_Uncert).set_TabIndex(17);
			((Control)txtD_PEq).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtD_PEq).set_Location(new Point(174, 42));
			((Control)txtD_PEq).set_Name("txtD_PEq");
			((Control)txtD_PEq).set_Size(new Size(28, 20));
			((Control)txtD_PEq).set_TabIndex(11);
			((Control)txtR_PEq).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtR_PEq).set_Location(new Point(174, 72));
			((Control)txtR_PEq).set_Name("txtR_PEq");
			((Control)txtR_PEq).set_Size(new Size(28, 20));
			((Control)txtR_PEq).set_TabIndex(18);
			((Control)txtShift).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtShift).set_Location(new Point(179, 100));
			((Control)txtShift).set_Name("txtShift");
			((Control)txtShift).set_Size(new Size(59, 20));
			((Control)txtShift).set_TabIndex(21);
			((Control)txtShift).add_Leave((EventHandler)txtShift_Leave);
			cmbR_Wt.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbR_Wt).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbR_Wt).set_FormattingEnabled(true);
			cmbR_Wt.get_Items().AddRange(new object[7] { "", "0", "1", "2", "3", "4", "5" });
			((Control)cmbR_Wt).set_Location(new Point(209, 72));
			((Control)cmbR_Wt).set_Name("cmbR_Wt");
			((Control)cmbR_Wt).set_Size(new Size(33, 21));
			((Control)cmbR_Wt).set_TabIndex(19);
			toolTip.SetToolTip((Control)(object)cmbR_Wt, componentResourceManager.GetString("cmbR_Wt.ToolTip"));
			((Control)txtD_Hr).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtD_Hr).set_Location(new Point(20, 42));
			((Control)txtD_Hr).set_Name("txtD_Hr");
			((Control)txtD_Hr).set_Size(new Size(22, 20));
			((Control)txtD_Hr).set_TabIndex(7);
			((Control)grpConditions).set_BackColor(Color.Honeydew);
			((Control)grpConditions).get_Controls().Add((Control)(object)txtFree);
			((Control)grpConditions).get_Controls().Add((Control)(object)updnSNR);
			((Control)grpConditions).get_Controls().Add((Control)(object)label43);
			((Control)grpConditions).get_Controls().Add((Control)(object)lblFreeLeft);
			((Control)grpConditions).get_Controls().Add((Control)(object)label37);
			((Control)grpConditions).get_Controls().Add((Control)(object)label36);
			((Control)grpConditions).get_Controls().Add((Control)(object)label35);
			((Control)grpConditions).get_Controls().Add((Control)(object)label33);
			((Control)grpConditions).get_Controls().Add((Control)(object)cmbMethod);
			((Control)grpConditions).get_Controls().Add((Control)(object)cmbTime);
			((Control)grpConditions).get_Controls().Add((Control)(object)cmbTransparency);
			((Control)grpConditions).get_Controls().Add((Control)(object)cmbPlotControl);
			((Control)grpConditions).get_Controls().Add((Control)(object)cmbStability);
			((Control)grpConditions).get_Controls().Add((Control)(object)label32);
			((Control)grpConditions).get_Controls().Add((Control)(object)label34);
			((Control)grpConditions).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpConditions).set_Location(new Point(597, 180));
			((Control)grpConditions).set_Name("grpConditions");
			((Control)grpConditions).set_Size(new Size(234, 208));
			((Control)grpConditions).set_TabIndex(6);
			grpConditions.set_TabStop(false);
			((Control)grpConditions).set_Text("Conditions");
			((Control)txtFree).set_AllowDrop(true);
			((Control)txtFree).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtFree).set_Location(new Point(5, 154));
			((TextBoxBase)txtFree).set_MaxLength(0);
			((Control)txtFree).set_Name("txtFree");
			((Control)txtFree).set_Size(new Size(225, 20));
			((Control)txtFree).set_TabIndex(12);
			((Control)txtFree).add_TextChanged((EventHandler)txtFree_TextChanged);
			((Control)txtFree).add_Enter((EventHandler)txtFree_Enter);
			((Control)txtFree).add_KeyDown(new KeyEventHandler(txtFree_KeyDown));
			((Control)txtFree).add_KeyUp(new KeyEventHandler(txtFree_KeyUp));
			((Control)txtFree).add_Leave((EventHandler)txtFree_Leave);
			((Control)txtFree).add_PreviewKeyDown(new PreviewKeyDownEventHandler(txtFree_PreviewKeyDown));
			updnSNR.set_DecimalPlaces(1);
			((Control)updnSNR).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnSNR.set_Increment(new decimal(new int[4] { 2, 0, 0, 65536 }));
			((Control)updnSNR).set_Location(new Point(122, 69));
			updnSNR.set_Maximum(new decimal(new int[4] { 20, 0, 0, 0 }));
			((Control)updnSNR).set_Name("updnSNR");
			((Control)updnSNR).set_Size(new Size(49, 20));
			((Control)updnSNR).set_TabIndex(5);
			updnSNR.set_Value(new decimal(new int[4] { 101, 0, 0, 65536 }));
			updnSNR.add_ValueChanged((EventHandler)updnSNR_ValueChanged);
			((Control)label43).set_AutoSize(true);
			((Control)label43).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label43).set_Location(new Point(26, 72));
			((Control)label43).set_Name("label43");
			((Control)label43).set_Size(new Size(93, 13));
			((Control)label43).set_TabIndex(4);
			((Control)label43).set_Text("Signal-to-Noise");
			((Control)lblFreeLeft).set_AutoSize(true);
			((Control)lblFreeLeft).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblFreeLeft).set_Location(new Point(204, 141));
			((Control)lblFreeLeft).set_Name("lblFreeLeft");
			((Control)lblFreeLeft).set_Size(new Size(13, 13));
			((Control)lblFreeLeft).set_TabIndex(11);
			((Control)lblFreeLeft).set_Text("n");
			((Control)label37).set_AutoSize(true);
			((Control)label37).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label37).set_Location(new Point(3, 139));
			((Control)label37).set_Name("label37");
			((Control)label37).set_Size(new Size(57, 13));
			((Control)label37).set_TabIndex(10);
			((Control)label37).set_Text("Free text");
			((Control)label36).set_AutoSize(true);
			((Control)label36).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label36).set_Location(new Point(36, 120));
			((Control)label36).set_Name("label36");
			((Control)label36).set_Size(new Size(84, 13));
			((Control)label36).set_TabIndex(8);
			((Control)label36).set_Text("Transparency");
			((Control)label35).set_AutoSize(true);
			((Control)label35).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label35).set_Location(new Point(68, 96));
			((Control)label35).set_Name("label35");
			((Control)label35).set_Size(new Size(52, 13));
			((Control)label35).set_TabIndex(6);
			((Control)label35).set_Text("Stability");
			((Control)label33).set_AutoSize(true);
			((Control)label33).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label33).set_Location(new Point(13, 49));
			((Control)label33).set_Name("label33");
			((Control)label33).set_Size(new Size(34, 13));
			((Control)label33).set_TabIndex(2);
			((Control)label33).set_Text("Time");
			cmbMethod.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbMethod).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbMethod).set_FormattingEnabled(true);
			cmbMethod.get_Items().AddRange(new object[8] { "Unspecified", "Analogue or Digital video", "Digital SLR-camera video", "Photometer", "Sequential images", "Drift scan", "Visual", "Other" });
			((Control)cmbMethod).set_Location(new Point(51, 21));
			((Control)cmbMethod).set_Name("cmbMethod");
			((Control)cmbMethod).set_Size(new Size(175, 21));
			((Control)cmbMethod).set_TabIndex(1);
			cmbMethod.add_SelectedIndexChanged((EventHandler)cmbMethod_SelectedIndexChanged);
			cmbTime.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbTime).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbTime).set_FormattingEnabled(true);
			cmbTime.get_Items().AddRange(new object[8] { "Unspecified", "GPS", "NTP", "Telephone (fixed or mobile)", "Radio time signal", "Internal clock of recorder", "Stopwatch", "Other" });
			((Control)cmbTime).set_Location(new Point(51, 45));
			((Control)cmbTime).set_Name("cmbTime");
			((Control)cmbTime).set_Size(new Size(175, 21));
			((Control)cmbTime).set_TabIndex(3);
			cmbTime.add_SelectedIndexChanged((EventHandler)cmbTime_SelectedIndexChanged);
			cmbTransparency.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbTransparency).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbTransparency).set_FormattingEnabled(true);
			cmbTransparency.get_Items().AddRange(new object[8] { "Unspecified", "Clear", "Fog", "Thin cloud <2", "Thick cloud >2", "Broken cloud", "Star faint", "Averted vision" });
			((Control)cmbTransparency).set_Location(new Point(122, 116));
			((Control)cmbTransparency).set_Name("cmbTransparency");
			((Control)cmbTransparency).set_Size(new Size(104, 21));
			((Control)cmbTransparency).set_TabIndex(9);
			cmbPlotControl.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbPlotControl).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbPlotControl).set_FormattingEnabled(true);
			cmbPlotControl.get_Items().AddRange(new object[4] { "Include observation", "Include D only", "Include R only", "Exclude observation" });
			((Control)cmbPlotControl).set_Location(new Point(110, 179));
			((Control)cmbPlotControl).set_Name("cmbPlotControl");
			((Control)cmbPlotControl).set_Size(new Size(120, 21));
			((Control)cmbPlotControl).set_TabIndex(14);
			cmbStability.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbStability).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbStability).set_FormattingEnabled(true);
			cmbStability.get_Items().AddRange(new object[4] { "Unspecified", "Steady", "Slight flickering", "Strong flickering" });
			((Control)cmbStability).set_Location(new Point(122, 92));
			((Control)cmbStability).set_Name("cmbStability");
			((Control)cmbStability).set_Size(new Size(104, 21));
			((Control)cmbStability).set_TabIndex(7);
			((Control)label32).set_AutoSize(true);
			((Control)label32).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label32).set_Location(new Point(1, 25));
			((Control)label32).set_Name("label32");
			((Control)label32).set_Size(new Size(49, 13));
			((Control)label32).set_TabIndex(0);
			((Control)label32).set_Text("Method");
			((Control)label34).set_AutoSize(true);
			((Control)label34).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label34).set_Location(new Point(1, 183));
			((Control)label34).set_Name("label34");
			((Control)label34).set_Size(new Size(111, 13));
			((Control)label34).set_TabIndex(13);
			((Control)label34).set_Text("Include in solution");
			((ToolStrip)menuStrip1).set_ImageScalingSize(new Size(20, 20));
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[10]
			{
				(ToolStripItem)fileToolStripMenuItem,
				(ToolStripItem)pasteToolStripMenuItem,
				(ToolStripItem)sortObserverLinesToolStripMenuItem,
				(ToolStripItem)plotToolStripMenuItem,
				(ToolStripItem)solutionsToolStripMenuItem,
				(ToolStripItem)listOffsetsToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem,
				(ToolStripItem)ExportImport,
				(ToolStripItem)transitionToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(1077, 28));
			((Control)menuStrip1).set_TabIndex(0);
			((ToolStripDropDownItem)fileToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[32]
			{
				(ToolStripItem)newToolStripMenuItem,
				(ToolStripItem)sortByDistanceWhenOpeningToolStripMenuItem,
				(ToolStripItem)whenOpeningToolStripMenuItem,
				(ToolStripItem)toolStripSeparator25,
				(ToolStripItem)openToolStripMenuItem,
				(ToolStripItem)openMultipleFilesToolStripMenuItem,
				(ToolStripItem)toolStripSeparator12,
				(ToolStripItem)checkSubmittedFilesForOmissionsToolStripMenuItem,
				(ToolStripItem)toolStripSeparator26,
				(ToolStripItem)saveToolStripMenuItem,
				(ToolStripItem)saveAsToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)addToHistoricalFileToolStripMenuItem,
				(ToolStripItem)matchUnistellarObservationsToolStripMenuItem,
				(ToolStripItem)toolStripSeparator11,
				(ToolStripItem)skipToNextFileToolStripMenuItem,
				(ToolStripItem)updateHistoricalFileToolStripMenuItem,
				(ToolStripItem)toolStripSeparator5,
				(ToolStripItem)copyEventRecordToolStripMenuItem,
				(ToolStripItem)copyObserverNamesToolStripMenuItem,
				(ToolStripItem)copyvalidObserverNamesToolStripMenuItem,
				(ToolStripItem)copyAllObserverDetailsToolStripMenuItem,
				(ToolStripItem)toolStripSeparator9,
				(ToolStripItem)convertMPCorbEtcToolStripMenuItem,
				(ToolStripItem)toolStripSeparator24,
				(ToolStripItem)mPCEditModeToolStripMenuItem,
				(ToolStripItem)toolStripSeparator38,
				(ToolStripItem)dELETIONModeToolStripMenuItem,
				(ToolStripItem)toolStripSeparator37,
				(ToolStripItem)cOMPILATIONModeToolStripMenuItem,
				(ToolStripItem)fileNameToolStripMenuItem,
				(ToolStripItem)setCompilationFileNameToolStripMenuItem
			});
			((ToolStripItem)fileToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)fileToolStripMenuItem).set_Name("fileToolStripMenuItem");
			((ToolStripItem)fileToolStripMenuItem).set_Size(new Size(59, 24));
			((ToolStripItem)fileToolStripMenuItem).set_Text("File...    ");
			((ToolStripItem)newToolStripMenuItem).set_Image((Image)componentResourceManager.GetObject("newToolStripMenuItem.Image"));
			((ToolStripItem)newToolStripMenuItem).set_Name("newToolStripMenuItem");
			newToolStripMenuItem.set_ShortcutKeys((Keys)131150);
			((ToolStripItem)newToolStripMenuItem).set_Size(new Size(276, 24));
			((ToolStripItem)newToolStripMenuItem).set_Text("New");
			((ToolStripItem)newToolStripMenuItem).add_Click((EventHandler)newToolStripMenuItem_Click);
			((ToolStripItem)sortByDistanceWhenOpeningToolStripMenuItem).set_BackColor(Color.Honeydew);
			((ToolStripItem)sortByDistanceWhenOpeningToolStripMenuItem).set_Name("sortByDistanceWhenOpeningToolStripMenuItem");
			((ToolStripItem)sortByDistanceWhenOpeningToolStripMenuItem).set_Size(new Size(276, 24));
			((ToolStripItem)sortByDistanceWhenOpeningToolStripMenuItem).set_Text("When opening, sort by path distance");
			((ToolStripItem)sortByDistanceWhenOpeningToolStripMenuItem).add_Click((EventHandler)sortByDistanceWhenOpeningToolStripMenuItem_Click);
			((ToolStripItem)whenOpeningToolStripMenuItem).set_BackColor(Color.Cornsilk);
			((ToolStripItem)whenOpeningToolStripMenuItem).set_Name("whenOpeningToolStripMenuItem");
			((ToolStripItem)whenOpeningToolStripMenuItem).set_Size(new Size(276, 24));
			((ToolStripItem)whenOpeningToolStripMenuItem).set_Text("Validate files when Opened");
			((ToolStripItem)whenOpeningToolStripMenuItem).add_Click((EventHandler)whenOpeningToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator25).set_Name("toolStripSeparator25");
			((ToolStripItem)toolStripSeparator25).set_Size(new Size(273, 6));
			((ToolStripItem)openToolStripMenuItem).set_Image((Image)componentResourceManager.GetObject("openToolStripMenuItem.Image"));
			((ToolStripItem)openToolStripMenuItem).set_Name("openToolStripMenuItem");
			openToolStripMenuItem.set_ShortcutKeys((Keys)131151);
			((ToolStripItem)openToolStripMenuItem).set_Size(new Size(276, 24));
			((ToolStripItem)openToolStripMenuItem).set_Text("Open");
			((ToolStripItem)openToolStripMenuItem).add_Click((EventHandler)openToolStripMenuItem_Click);
			((ToolStripItem)openMultipleFilesToolStripMenuItem).set_Image((Image)componentResourceManager.GetObject("openMultipleFilesToolStripMenuItem.Image"));
			((ToolStripItem)openMultipleFilesToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)openMultipleFilesToolStripMenuItem).set_Name("openMultipleFilesToolStripMenuItem");
			((ToolStripItem)openMultipleFilesToolStripMenuItem).set_Size(new Size(276, 24));
			((ToolStripItem)openMultipleFilesToolStripMenuItem).set_Text("Open multiple files");
			((ToolStripItem)openMultipleFilesToolStripMenuItem).add_Click((EventHandler)openMultipleFilesToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator12).set_Name("toolStripSeparator12");
			((ToolStripItem)toolStripSeparator12).set_Size(new Size(273, 6));
			((ToolStripItem)checkSubmittedFilesForOmissionsToolStripMenuItem).set_BackColor(SystemColors.Control);
			((ToolStripItem)checkSubmittedFilesForOmissionsToolStripMenuItem).set_Font(new Font("Segoe UI", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((ToolStripItem)checkSubmittedFilesForOmissionsToolStripMenuItem).set_ForeColor(SystemColors.ActiveCaptionText);
			((ToolStripItem)checkSubmittedFilesForOmissionsToolStripMenuItem).set_Image((Image)Resources.check);
			((ToolStripItem)checkSubmittedFilesForOmissionsToolStripMenuItem).set_Name("checkSubmittedFilesForOmissionsToolStripMenuItem");
			((ToolStripItem)checkSubmittedFilesForOmissionsToolStripMenuItem).set_Size(new Size(276, 24));
			((ToolStripItem)checkSubmittedFilesForOmissionsToolStripMenuItem).set_Text("Check submitted files for omissions");
			((ToolStripItem)checkSubmittedFilesForOmissionsToolStripMenuItem).add_Click((EventHandler)checkSubmittedFilesForOmissionsToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator26).set_Name("toolStripSeparator26");
			((ToolStripItem)toolStripSeparator26).set_Size(new Size(273, 6));
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)componentResourceManager.GetObject("saveToolStripMenuItem.Image"));
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(276, 24));
			((ToolStripItem)saveToolStripMenuItem).set_Text("Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)saveAsToolStripMenuItem).set_Image((Image)componentResourceManager.GetObject("saveAsToolStripMenuItem.Image"));
			((ToolStripItem)saveAsToolStripMenuItem).set_Name("saveAsToolStripMenuItem");
			((ToolStripItem)saveAsToolStripMenuItem).set_Size(new Size(276, 24));
			((ToolStripItem)saveAsToolStripMenuItem).set_Text("Save as...");
			((ToolStripItem)saveAsToolStripMenuItem).add_Click((EventHandler)saveAsToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(273, 6));
			((ToolStripItem)addToHistoricalFileToolStripMenuItem).set_Name("addToHistoricalFileToolStripMenuItem");
			addToHistoricalFileToolStripMenuItem.set_ShortcutKeys((Keys)131137);
			((ToolStripItem)addToHistoricalFileToolStripMenuItem).set_Size(new Size(276, 24));
			((ToolStripItem)addToHistoricalFileToolStripMenuItem).set_Text("Add to Historical file");
			((ToolStripItem)addToHistoricalFileToolStripMenuItem).add_Click((EventHandler)addToHistoricalFileToolStripMenuItem_Click);
			((ToolStripItem)matchUnistellarObservationsToolStripMenuItem).set_Image((Image)Resources.telescope);
			((ToolStripItem)matchUnistellarObservationsToolStripMenuItem).set_Name("matchUnistellarObservationsToolStripMenuItem");
			((ToolStripItem)matchUnistellarObservationsToolStripMenuItem).set_Size(new Size(276, 24));
			((ToolStripItem)matchUnistellarObservationsToolStripMenuItem).set_Text("Unistellar: match to existing events");
			((ToolStripItem)matchUnistellarObservationsToolStripMenuItem).set_ToolTipText("Sorts the Unistellar files according\r\nto whether there are events, or\r\nobservations, that need to be \r\nadded to the Historical observations");
			((ToolStripItem)matchUnistellarObservationsToolStripMenuItem).add_Click((EventHandler)matchUnistellarObservationsToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator11).set_Name("toolStripSeparator11");
			((ToolStripItem)toolStripSeparator11).set_Size(new Size(273, 6));
			((ToolStripItem)skipToNextFileToolStripMenuItem).set_Image((Image)componentResourceManager.GetObject("skipToNextFileToolStripMenuItem.Image"));
			((ToolStripItem)skipToNextFileToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)skipToNextFileToolStripMenuItem).set_Name("skipToNextFileToolStripMenuItem");
			((ToolStripItem)skipToNextFileToolStripMenuItem).set_Size(new Size(276, 24));
			((ToolStripItem)skipToNextFileToolStripMenuItem).set_Text("Skip to next file");
			((ToolStripItem)skipToNextFileToolStripMenuItem).set_Visible(false);
			((ToolStripItem)skipToNextFileToolStripMenuItem).add_Click((EventHandler)skipToNextFileToolStripMenuItem_Click);
			((ToolStripItem)updateHistoricalFileToolStripMenuItem).set_Name("updateHistoricalFileToolStripMenuItem");
			updateHistoricalFileToolStripMenuItem.set_ShortcutKeys((Keys)131157);
			((ToolStripItem)updateHistoricalFileToolStripMenuItem).set_Size(new Size(276, 24));
			((ToolStripItem)updateHistoricalFileToolStripMenuItem).set_Text("Update Historical file");
			((ToolStripItem)updateHistoricalFileToolStripMenuItem).add_Click((EventHandler)updateHistoricalFileToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator5).set_Name("toolStripSeparator5");
			((ToolStripItem)toolStripSeparator5).set_Size(new Size(273, 6));
			((ToolStripItem)copyEventRecordToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyEventRecordToolStripMenuItem).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)copyEventRecordToolStripMenuItem).set_Name("copyEventRecordToolStripMenuItem");
			((ToolStripItem)copyEventRecordToolStripMenuItem).set_Size(new Size(276, 24));
			((ToolStripItem)copyEventRecordToolStripMenuItem).set_Text("copy event in XML");
			((ToolStripItem)copyEventRecordToolStripMenuItem).add_Click((EventHandler)copyEventRecordToolStripMenuItem_Click);
			((ToolStripItem)copyObserverNamesToolStripMenuItem).set_Image((Image)componentResourceManager.GetObject("copyObserverNamesToolStripMenuItem.Image"));
			((ToolStripItem)copyObserverNamesToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyObserverNamesToolStripMenuItem).set_Name("copyObserverNamesToolStripMenuItem");
			((ToolStripItem)copyObserverNamesToolStripMenuItem).set_Size(new Size(276, 24));
			((ToolStripItem)copyObserverNamesToolStripMenuItem).set_Text("copy all Observer names");
			((ToolStripItem)copyObserverNamesToolStripMenuItem).add_Click((EventHandler)copyObserverNamesToolStripMenuItem_Click);
			((ToolStripItem)copyvalidObserverNamesToolStripMenuItem).set_Image((Image)componentResourceManager.GetObject("copyvalidObserverNamesToolStripMenuItem.Image"));
			((ToolStripItem)copyvalidObserverNamesToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyvalidObserverNamesToolStripMenuItem).set_Name("copyvalidObserverNamesToolStripMenuItem");
			((ToolStripItem)copyvalidObserverNamesToolStripMenuItem).set_Size(new Size(276, 24));
			((ToolStripItem)copyvalidObserverNamesToolStripMenuItem).set_Text("copy 'valid' Observer names");
			((ToolStripItem)copyvalidObserverNamesToolStripMenuItem).add_Click((EventHandler)copyvalidObserverNamesToolStripMenuItem_Click);
			((ToolStripItem)copyAllObserverDetailsToolStripMenuItem).set_Image((Image)componentResourceManager.GetObject("copyAllObserverDetailsToolStripMenuItem.Image"));
			((ToolStripItem)copyAllObserverDetailsToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyAllObserverDetailsToolStripMenuItem).set_Name("copyAllObserverDetailsToolStripMenuItem");
			((ToolStripItem)copyAllObserverDetailsToolStripMenuItem).set_Size(new Size(276, 24));
			((ToolStripItem)copyAllObserverDetailsToolStripMenuItem).set_Text("copy all Observer details");
			((ToolStripItem)copyAllObserverDetailsToolStripMenuItem).add_Click((EventHandler)copyAllObserverDetailsToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator9).set_Name("toolStripSeparator9");
			((ToolStripItem)toolStripSeparator9).set_Size(new Size(273, 6));
			((ToolStripItem)convertMPCorbEtcToolStripMenuItem).set_Name("convertMPCorbEtcToolStripMenuItem");
			((ToolStripItem)convertMPCorbEtcToolStripMenuItem).set_Size(new Size(276, 24));
			((ToolStripItem)convertMPCorbEtcToolStripMenuItem).set_Text("Convert MPCorb etc");
			((ToolStripItem)convertMPCorbEtcToolStripMenuItem).add_Click((EventHandler)convertMPCorbEtcToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator24).set_Name("toolStripSeparator24");
			((ToolStripItem)toolStripSeparator24).set_Size(new Size(273, 6));
			((ToolStripItem)mPCEditModeToolStripMenuItem).set_BackColor(Color.FloralWhite);
			((ToolStripItem)mPCEditModeToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Italic));
			((ToolStripItem)mPCEditModeToolStripMenuItem).set_ForeColor(Color.Black);
			((ToolStripItem)mPCEditModeToolStripMenuItem).set_Name("mPCEditModeToolStripMenuItem");
			((ToolStripItem)mPCEditModeToolStripMenuItem).set_Size(new Size(276, 24));
			((ToolStripItem)mPCEditModeToolStripMenuItem).set_Text("MPC: allow edit of date && MPEC ");
			((ToolStripItem)mPCEditModeToolStripMenuItem).add_Click((EventHandler)mPCEditModeToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator38).set_Name("toolStripSeparator38");
			((ToolStripItem)toolStripSeparator38).set_Size(new Size(273, 6));
			((ToolStripItem)dELETIONModeToolStripMenuItem).set_BackColor(Color.PowderBlue);
			((ToolStripItem)dELETIONModeToolStripMenuItem).set_Font(new Font("Segoe UI", 10f, FontStyle.Bold | FontStyle.Italic));
			((ToolStripItem)dELETIONModeToolStripMenuItem).set_ForeColor(Color.DarkGreen);
			((ToolStripItem)dELETIONModeToolStripMenuItem).set_Name("dELETIONModeToolStripMenuItem");
			((ToolStripItem)dELETIONModeToolStripMenuItem).set_Size(new Size(276, 24));
			((ToolStripItem)dELETIONModeToolStripMenuItem).set_Text("DELETION mode");
			((ToolStripItem)dELETIONModeToolStripMenuItem).add_Click((EventHandler)dELETIONModeToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator37).set_Name("toolStripSeparator37");
			((ToolStripItem)toolStripSeparator37).set_Size(new Size(273, 6));
			((ToolStripItem)cOMPILATIONModeToolStripMenuItem).set_BackColor(Color.LightSteelBlue);
			((ToolStripItem)cOMPILATIONModeToolStripMenuItem).set_Font(new Font("Segoe UI", 10f, FontStyle.Bold | FontStyle.Italic));
			((ToolStripItem)cOMPILATIONModeToolStripMenuItem).set_ForeColor(Color.MediumBlue);
			((ToolStripItem)cOMPILATIONModeToolStripMenuItem).set_Name("cOMPILATIONModeToolStripMenuItem");
			((ToolStripItem)cOMPILATIONModeToolStripMenuItem).set_Size(new Size(276, 24));
			((ToolStripItem)cOMPILATIONModeToolStripMenuItem).set_Text("COMPILATION mode");
			((ToolStripItem)cOMPILATIONModeToolStripMenuItem).add_Click((EventHandler)cOMPILATIONModeToolStripMenuItem_Click);
			((ToolStripItem)fileNameToolStripMenuItem).set_BackColor(Color.Azure);
			((ToolStripItem)fileNameToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)fileNameToolStripMenuItem).set_ForeColor(Color.MediumBlue);
			((ToolStripItem)fileNameToolStripMenuItem).set_Name("fileNameToolStripMenuItem");
			((ToolStripItem)fileNameToolStripMenuItem).set_Size(new Size(276, 24));
			((ToolStripItem)fileNameToolStripMenuItem).set_Text("file name");
			((ToolStripItem)setCompilationFileNameToolStripMenuItem).set_BackColor(Color.Azure);
			((ToolStripItem)setCompilationFileNameToolStripMenuItem).set_ForeColor(Color.MediumBlue);
			((ToolStripItem)setCompilationFileNameToolStripMenuItem).set_Name("setCompilationFileNameToolStripMenuItem");
			((ToolStripItem)setCompilationFileNameToolStripMenuItem).set_Size(new Size(276, 24));
			((ToolStripItem)setCompilationFileNameToolStripMenuItem).set_Text("Set Compilation file name");
			((ToolStripItem)setCompilationFileNameToolStripMenuItem).add_Click((EventHandler)setCompilationFileNameToolStripMenuItem_Click);
			((ToolStripDropDownItem)pasteToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[20]
			{
				(ToolStripItem)theseAreMaintenaceFunctionsToolStripMenuItem,
				(ToolStripItem)whenPastingSortByDistanceToolStripMenuItem,
				(ToolStripItem)toolStripSeparator14,
				(ToolStripItem)newEventOBSFormatToolStripMenuItem,
				(ToolStripItem)newObservationObserverGroupXMLFormatToolStripMenuItem,
				(ToolStripItem)validateFilesWhenEventOrObserversPastedToolStripMenuItem,
				(ToolStripItem)toolStripSeparator10,
				(ToolStripItem)predictionLineToolStripMenuItem,
				(ToolStripItem)lineForAsnUnseenBinaryasteroidPrimaryObjectToolStripMenuItem,
				(ToolStripItem)toolStripSeparator7,
				(ToolStripItem)loadSaveObserverDetailsToolStripMenuItem,
				(ToolStripItem)eventDateStarAsteroidOccelmntPredictionXmlToolStripMenuItem,
				(ToolStripItem)toolStripSeparator39,
				(ToolStripItem)eurasterEventAsNewEventToolStripMenuItem,
				(ToolStripItem)toolStripSeparator3,
				(ToolStripItem)newEventFromJPToolStripMenuItem,
				(ToolStripItem)newObservatToolStripMenuItem,
				(ToolStripItem)toolStripSeparator6,
				(ToolStripItem)eurasterObservationToCurrentEventToolStripMenuItem,
				(ToolStripItem)newObservationLineoneOnlyOBSFormatToolStripMenuItem
			});
			((ToolStripItem)pasteToolStripMenuItem).set_Font(new Font("Segoe UI", 8.25f, FontStyle.Bold | FontStyle.Italic));
			((ToolStripItem)pasteToolStripMenuItem).set_Name("pasteToolStripMenuItem");
			((ToolStripItem)pasteToolStripMenuItem).set_Size(new Size(72, 24));
			((ToolStripItem)pasteToolStripMenuItem).set_Text("Paste....    ");
			((ToolStripItem)theseAreMaintenaceFunctionsToolStripMenuItem).set_Name("theseAreMaintenaceFunctionsToolStripMenuItem");
			((ToolStripItem)theseAreMaintenaceFunctionsToolStripMenuItem).set_Size(new Size(504, 22));
			((ToolStripItem)theseAreMaintenaceFunctionsToolStripMenuItem).set_Text("[ THESE ARE MAINTENANCE ROUTINES ]");
			((ToolStripItem)whenPastingSortByDistanceToolStripMenuItem).set_Name("whenPastingSortByDistanceToolStripMenuItem");
			((ToolStripItem)whenPastingSortByDistanceToolStripMenuItem).set_Size(new Size(504, 22));
			((ToolStripItem)whenPastingSortByDistanceToolStripMenuItem).set_Text("When pasting, sort by path distance");
			((ToolStripItem)whenPastingSortByDistanceToolStripMenuItem).add_Click((EventHandler)whenPastingSortByDistanceToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator14).set_Name("toolStripSeparator14");
			((ToolStripItem)toolStripSeparator14).set_Size(new Size(501, 6));
			((ToolStripItem)newEventOBSFormatToolStripMenuItem).set_BackColor(Color.PaleGoldenrod);
			((ToolStripItem)newEventOBSFormatToolStripMenuItem).set_Name("newEventOBSFormatToolStripMenuItem");
			((ToolStripItem)newEventOBSFormatToolStripMenuItem).set_Size(new Size(504, 22));
			((ToolStripItem)newEventOBSFormatToolStripMenuItem).set_Text("Single event - XML or old OBS format");
			((ToolStripItem)newEventOBSFormatToolStripMenuItem).add_Click((EventHandler)newEventOBSFormatToolStripMenuItem_Click);
			((ToolStripItem)newObservationObserverGroupXMLFormatToolStripMenuItem).set_BackColor(Color.LightGoldenrodYellow);
			((ToolStripItem)newObservationObserverGroupXMLFormatToolStripMenuItem).set_Name("newObservationObserverGroupXMLFormatToolStripMenuItem");
			((ToolStripItem)newObservationObserverGroupXMLFormatToolStripMenuItem).set_Size(new Size(504, 22));
			((ToolStripItem)newObservationObserverGroupXMLFormatToolStripMenuItem).set_Text("New observer lines [one or more groups] - XML format");
			((ToolStripItem)newObservationObserverGroupXMLFormatToolStripMenuItem).add_Click((EventHandler)newObservationObserverGroupXMLFormatToolStripMenuItem_Click);
			((ToolStripItem)validateFilesWhenEventOrObserversPastedToolStripMenuItem).set_BackColor(Color.Cornsilk);
			((ToolStripItem)validateFilesWhenEventOrObserversPastedToolStripMenuItem).set_Name("validateFilesWhenEventOrObserversPastedToolStripMenuItem");
			((ToolStripItem)validateFilesWhenEventOrObserversPastedToolStripMenuItem).set_Size(new Size(504, 22));
			((ToolStripItem)validateFilesWhenEventOrObserversPastedToolStripMenuItem).set_Text(". . . Validate data when Event or Observers pasted");
			((ToolStripItem)validateFilesWhenEventOrObserversPastedToolStripMenuItem).add_Click((EventHandler)validateFilesWhenEventOrObserversPastedToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator10).set_Name("toolStripSeparator10");
			((ToolStripItem)toolStripSeparator10).set_Size(new Size(501, 6));
			((ToolStripItem)predictionLineToolStripMenuItem).set_BackColor(Color.PaleGreen);
			((ToolStripItem)predictionLineToolStripMenuItem).set_Name("predictionLineToolStripMenuItem");
			((ToolStripItem)predictionLineToolStripMenuItem).set_Size(new Size(504, 22));
			((ToolStripItem)predictionLineToolStripMenuItem).set_Text("Prediction line");
			((ToolStripItem)predictionLineToolStripMenuItem).add_Click((EventHandler)predictionLineToolStripMenuItem_Click);
			((ToolStripItem)lineForAsnUnseenBinaryasteroidPrimaryObjectToolStripMenuItem).set_BackColor(Color.Honeydew);
			((ToolStripItem)lineForAsnUnseenBinaryasteroidPrimaryObjectToolStripMenuItem).set_Name("lineForAsnUnseenBinaryasteroidPrimaryObjectToolStripMenuItem");
			((ToolStripItem)lineForAsnUnseenBinaryasteroidPrimaryObjectToolStripMenuItem).set_Size(new Size(504, 22));
			((ToolStripItem)lineForAsnUnseenBinaryasteroidPrimaryObjectToolStripMenuItem).set_Text("Prediction for the Primary body of a binary asteroid (which has not been observed)");
			((ToolStripItem)lineForAsnUnseenBinaryasteroidPrimaryObjectToolStripMenuItem).add_Click((EventHandler)LineForAsnUnseenBinaryasteroidPrimaryObjectToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator7).set_Name("toolStripSeparator7");
			((ToolStripItem)toolStripSeparator7).set_Size(new Size(501, 6));
			((ToolStripItem)loadSaveObserverDetailsToolStripMenuItem).set_BackColor(Color.LavenderBlush);
			((ToolStripItem)loadSaveObserverDetailsToolStripMenuItem).set_ForeColor(Color.DarkBlue);
			((ToolStripItem)loadSaveObserverDetailsToolStripMenuItem).set_Name("loadSaveObserverDetailsToolStripMenuItem");
			((ToolStripItem)loadSaveObserverDetailsToolStripMenuItem).set_Size(new Size(504, 22));
			((ToolStripItem)loadSaveObserverDetailsToolStripMenuItem).set_Text("set (or save) Observer details - using Saved Observer details");
			((ToolStripItem)loadSaveObserverDetailsToolStripMenuItem).add_Click((EventHandler)loadSaveObserverDetailsToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator39).set_Name("toolStripSeparator39");
			((ToolStripItem)toolStripSeparator39).set_Size(new Size(501, 6));
			((ToolStripItem)eurasterEventAsNewEventToolStripMenuItem).set_BackColor(Color.LightCyan);
			((ToolStripItem)eurasterEventAsNewEventToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)eurasterEventAsNewEventToolStripMenuItem).set_Name("eurasterEventAsNewEventToolStripMenuItem");
			eurasterEventAsNewEventToolStripMenuItem.set_ShortcutKeys((Keys)131141);
			((ToolStripItem)eurasterEventAsNewEventToolStripMenuItem).set_Size(new Size(504, 22));
			((ToolStripItem)eurasterEventAsNewEventToolStripMenuItem).set_Text("Multiple events [dates + observations] from Euraster  (Europe)");
			((ToolStripItem)eurasterEventAsNewEventToolStripMenuItem).set_Visible(false);
			((ToolStripItem)eurasterEventAsNewEventToolStripMenuItem).add_Click((EventHandler)eurasterEventAsNewEventToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator3).set_Name("toolStripSeparator3");
			((ToolStripItem)toolStripSeparator3).set_Size(new Size(501, 6));
			((ToolStripItem)toolStripSeparator3).set_Visible(false);
			((ToolStripItem)newEventFromJPToolStripMenuItem).set_BackColor(Color.LightYellow);
			((ToolStripItem)newEventFromJPToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)newEventFromJPToolStripMenuItem).set_Name("newEventFromJPToolStripMenuItem");
			newEventFromJPToolStripMenuItem.set_ShortcutKeys((Keys)131146);
			((ToolStripItem)newEventFromJPToolStripMenuItem).set_Size(new Size(504, 22));
			((ToolStripItem)newEventFromJPToolStripMenuItem).set_Text("Multiple events [dates + observations] from JOIN  (Japan)");
			((ToolStripItem)newEventFromJPToolStripMenuItem).set_Visible(false);
			((ToolStripItem)newEventFromJPToolStripMenuItem).add_Click((EventHandler)newEventFromJPToolStripMenuItem_Click);
			((ToolStripItem)newObservatToolStripMenuItem).set_BackColor(Color.LightYellow);
			((ToolStripItem)newObservatToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)newObservatToolStripMenuItem).set_Name("newObservatToolStripMenuItem");
			newObservatToolStripMenuItem.set_ShortcutKeys((Keys)196682);
			((ToolStripItem)newObservatToolStripMenuItem).set_Size(new Size(504, 22));
			((ToolStripItem)newObservatToolStripMenuItem).set_Text("New observation lines [ one group only] from JOIN  (Japan)");
			((ToolStripItem)newObservatToolStripMenuItem).set_Visible(false);
			((ToolStripItem)newObservatToolStripMenuItem).add_Click((EventHandler)newObservatToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator6).set_Name("toolStripSeparator6");
			((ToolStripItem)toolStripSeparator6).set_Size(new Size(501, 6));
			((ToolStripItem)toolStripSeparator6).set_Visible(false);
			((ToolStripItem)eurasterObservationToCurrentEventToolStripMenuItem).set_BackColor(Color.LightCyan);
			((ToolStripItem)eurasterObservationToCurrentEventToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)eurasterObservationToCurrentEventToolStripMenuItem).set_Name("eurasterObservationToCurrentEventToolStripMenuItem");
			eurasterObservationToCurrentEventToolStripMenuItem.set_ShortcutKeys((Keys)196677);
			((ToolStripItem)eurasterObservationToCurrentEventToolStripMenuItem).set_Size(new Size(504, 22));
			((ToolStripItem)eurasterObservationToCurrentEventToolStripMenuItem).set_Text("New observation lines [one group only] from Euraster  (Europe)");
			((ToolStripItem)eurasterObservationToCurrentEventToolStripMenuItem).set_Visible(false);
			((ToolStripItem)eurasterObservationToCurrentEventToolStripMenuItem).add_Click((EventHandler)eurasterObservationToCurrentEventToolStripMenuItem_Click);
			((ToolStripItem)newObservationLineoneOnlyOBSFormatToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)newObservationLineoneOnlyOBSFormatToolStripMenuItem).set_Font(new Font("Segoe UI", 6.5f, FontStyle.Bold | FontStyle.Italic));
			((ToolStripItem)newObservationLineoneOnlyOBSFormatToolStripMenuItem).set_Name("newObservationLineoneOnlyOBSFormatToolStripMenuItem");
			((ToolStripItem)newObservationLineoneOnlyOBSFormatToolStripMenuItem).set_Size(new Size(504, 22));
			((ToolStripItem)newObservationLineoneOnlyOBSFormatToolStripMenuItem).set_Text("New observer line [or lines] - old OBS format");
			((ToolStripItem)newObservationLineoneOnlyOBSFormatToolStripMenuItem).set_Visible(false);
			((ToolStripItem)newObservationLineoneOnlyOBSFormatToolStripMenuItem).add_Click((EventHandler)newObservationLineoneOnlyOBSFormatToolStripMenuItem_Click);
			((ToolStripDropDownItem)sortObserverLinesToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[13]
			{
				(ToolStripItem)byLatitudeToolStripMenuItem,
				(ToolStripItem)byLongitudeToolStripMenuItem,
				(ToolStripItem)byNameFieldToolStripMenuItem,
				(ToolStripItem)byNameFieldskip2ToolStripMenuItem,
				(ToolStripItem)byPathDistanceToolStripMenuItem,
				(ToolStripItem)sortByPathDistanceRenumberToolStripMenuItem,
				(ToolStripItem)bySequenceNumberToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)renumberObservationsToolStripMenuItem,
				(ToolStripItem)toolStripSeparator4,
				(ToolStripItem)validateAllSiteAltitudesToolStripMenuItem,
				(ToolStripItem)plotSitesInGoogleEarthToolStripMenuItem,
				(ToolStripItem)toolStripSeparator23
			});
			((ToolStripItem)sortObserverLinesToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)sortObserverLinesToolStripMenuItem).set_Name("sortObserverLinesToolStripMenuItem");
			sortObserverLinesToolStripMenuItem.set_ShortcutKeys((Keys)116);
			((ToolStripItem)sortObserverLinesToolStripMenuItem).set_Size(new Size(150, 24));
			((ToolStripItem)sortObserverLinesToolStripMenuItem).set_Text("with Observations...       ");
			((ToolStripItem)byLatitudeToolStripMenuItem).set_Name("byLatitudeToolStripMenuItem");
			((ToolStripItem)byLatitudeToolStripMenuItem).set_Size(new Size(287, 22));
			((ToolStripItem)byLatitudeToolStripMenuItem).set_Text("sort by Latitude");
			((ToolStripItem)byLatitudeToolStripMenuItem).add_Click((EventHandler)byLatitudeToolStripMenuItem_Click);
			((ToolStripItem)byLongitudeToolStripMenuItem).set_Name("byLongitudeToolStripMenuItem");
			((ToolStripItem)byLongitudeToolStripMenuItem).set_Size(new Size(287, 22));
			((ToolStripItem)byLongitudeToolStripMenuItem).set_Text("sort by Longitude");
			((ToolStripItem)byLongitudeToolStripMenuItem).add_Click((EventHandler)byLongitudeToolStripMenuItem_Click);
			((ToolStripItem)byNameFieldToolStripMenuItem).set_Name("byNameFieldToolStripMenuItem");
			((ToolStripItem)byNameFieldToolStripMenuItem).set_Size(new Size(287, 22));
			((ToolStripItem)byNameFieldToolStripMenuItem).set_Text("sort by Observer's full name");
			((ToolStripItem)byNameFieldToolStripMenuItem).add_Click((EventHandler)byNameFieldToolStripMenuItem_Click);
			((ToolStripItem)byNameFieldskip2ToolStripMenuItem).set_Name("byNameFieldskip2ToolStripMenuItem");
			((ToolStripItem)byNameFieldskip2ToolStripMenuItem).set_Size(new Size(287, 22));
			((ToolStripItem)byNameFieldskip2ToolStripMenuItem).set_Text("sort by Observer's Family name");
			((ToolStripItem)byNameFieldskip2ToolStripMenuItem).add_Click((EventHandler)byNameFieldskip2ToolStripMenuItem_Click);
			((ToolStripItem)byPathDistanceToolStripMenuItem).set_Name("byPathDistanceToolStripMenuItem");
			byPathDistanceToolStripMenuItem.set_ShortcutKeys((Keys)117);
			((ToolStripItem)byPathDistanceToolStripMenuItem).set_Size(new Size(287, 22));
			((ToolStripItem)byPathDistanceToolStripMenuItem).set_Text("sort by Path distance");
			((ToolStripItem)byPathDistanceToolStripMenuItem).add_Click((EventHandler)byPathDistanceToolStripMenuItem_Click);
			((ToolStripItem)sortByPathDistanceRenumberToolStripMenuItem).set_Name("sortByPathDistanceRenumberToolStripMenuItem");
			sortByPathDistanceRenumberToolStripMenuItem.set_ShortcutKeys((Keys)118);
			((ToolStripItem)sortByPathDistanceRenumberToolStripMenuItem).set_Size(new Size(287, 22));
			((ToolStripItem)sortByPathDistanceRenumberToolStripMenuItem).set_Text("sort by Path distance, && Renumber");
			((ToolStripItem)sortByPathDistanceRenumberToolStripMenuItem).add_Click((EventHandler)sortByPathDistanceRenumberToolStripMenuItem_Click);
			((ToolStripItem)bySequenceNumberToolStripMenuItem).set_Name("bySequenceNumberToolStripMenuItem");
			((ToolStripItem)bySequenceNumberToolStripMenuItem).set_Size(new Size(287, 22));
			((ToolStripItem)bySequenceNumberToolStripMenuItem).set_Text("sort by Sequence number");
			((ToolStripItem)bySequenceNumberToolStripMenuItem).add_Click((EventHandler)bySequenceNumberToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(284, 6));
			((ToolStripItem)renumberObservationsToolStripMenuItem).set_Name("renumberObservationsToolStripMenuItem");
			((ToolStripItem)renumberObservationsToolStripMenuItem).set_Size(new Size(287, 22));
			((ToolStripItem)renumberObservationsToolStripMenuItem).set_Text("Renumber observations");
			((ToolStripItem)renumberObservationsToolStripMenuItem).add_Click((EventHandler)renumberObservationsToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator4).set_Name("toolStripSeparator4");
			((ToolStripItem)toolStripSeparator4).set_Size(new Size(284, 6));
			((ToolStripItem)validateAllSiteAltitudesToolStripMenuItem).set_Name("validateAllSiteAltitudesToolStripMenuItem");
			validateAllSiteAltitudesToolStripMenuItem.set_ShortcutKeys((Keys)116);
			((ToolStripItem)validateAllSiteAltitudesToolStripMenuItem).set_Size(new Size(287, 22));
			((ToolStripItem)validateAllSiteAltitudesToolStripMenuItem).set_Text("Site Altitudes - validate  ");
			((ToolStripItem)validateAllSiteAltitudesToolStripMenuItem).add_Click((EventHandler)validateAllSiteAltitudesToolStripMenuItem_Click);
			((ToolStripItem)plotSitesInGoogleEarthToolStripMenuItem).set_Image((Image)componentResourceManager.GetObject("plotSitesInGoogleEarthToolStripMenuItem.Image"));
			((ToolStripItem)plotSitesInGoogleEarthToolStripMenuItem).set_Name("plotSitesInGoogleEarthToolStripMenuItem");
			plotSitesInGoogleEarthToolStripMenuItem.set_ShortcutKeys((Keys)131143);
			((ToolStripItem)plotSitesInGoogleEarthToolStripMenuItem).set_Size(new Size(287, 22));
			((ToolStripItem)plotSitesInGoogleEarthToolStripMenuItem).set_Text("Plot sites in GoogleEarth");
			((ToolStripItem)plotSitesInGoogleEarthToolStripMenuItem).add_Click((EventHandler)plotSitesInGoogleEarthToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator23).set_Name("toolStripSeparator23");
			((ToolStripItem)toolStripSeparator23).set_Size(new Size(284, 6));
			((ToolStripItem)plotToolStripMenuItem).set_Font(new Font("Segoe UI", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((ToolStripItem)plotToolStripMenuItem).set_Name("plotToolStripMenuItem");
			((ToolStripItem)plotToolStripMenuItem).set_Size(new Size(109, 24));
			((ToolStripItem)plotToolStripMenuItem).set_Text("&Plot event     ");
			((ToolStripItem)plotToolStripMenuItem).add_Click((EventHandler)plotToolStripMenuItem_Click);
			((ToolStripDropDownItem)solutionsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[20]
			{
				(ToolStripItem)doubleStarsToolStripMenuItem,
				(ToolStripItem)toolStripSeparator33,
				(ToolStripItem)listCurrentEventSolutionsToolStripMenuItem1,
				(ToolStripItem)deleteCurrentEventSolutionsToolStripMenuItem,
				(ToolStripItem)toolStripSeparator15,
				(ToolStripItem)satelliteToolStripMenuItem,
				(ToolStripItem)toolStripSeparator34,
				(ToolStripItem)listCurrenteventSatelliteSolutionToolStripMenuItem,
				(ToolStripItem)deeteCurrenteventSatelliteSolutionToolStripMenuItem,
				(ToolStripItem)toolStripSeparator16,
				(ToolStripItem)shapeModelsToolStripMenuItem1,
				(ToolStripItem)toolStripSeparator35,
				(ToolStripItem)listCurrenteventShapeModelsSolutionsToolStripMenuItem,
				(ToolStripItem)deleteCurrenteventShapemodelSolutionsToolStripMenuItem,
				(ToolStripItem)downloadShapeModelsToolStripMenuItem,
				(ToolStripItem)toolStripSeparator21,
				(ToolStripItem)astrometricSolutionToolStripMenuItem,
				(ToolStripItem)toolStripSeparator36,
				(ToolStripItem)astrometricSolutionToolStripMenuItem1,
				(ToolStripItem)astrometricSolutionIncludingatConjunctionSolutionToolStripMenuItem
			});
			((ToolStripItem)solutionsToolStripMenuItem).set_Font(new Font("Segoe UI", 10f, FontStyle.Bold));
			((ToolStripItem)solutionsToolStripMenuItem).set_Name("solutionsToolStripMenuItem");
			((ToolStripItem)solutionsToolStripMenuItem).set_Size(new Size(114, 24));
			((ToolStripItem)solutionsToolStripMenuItem).set_Text("Solutions...     ");
			((ToolStripItem)doubleStarsToolStripMenuItem).set_BackColor(Color.LemonChiffon);
			((ToolStripItem)doubleStarsToolStripMenuItem).set_Font(new Font("Segoe UI", 12f, FontStyle.Bold));
			((ToolStripItem)doubleStarsToolStripMenuItem).set_ForeColor(Color.DarkGreen);
			((ToolStripItem)doubleStarsToolStripMenuItem).set_Name("doubleStarsToolStripMenuItem");
			((ToolStripItem)doubleStarsToolStripMenuItem).set_Size(new Size(380, 26));
			((ToolStripItem)doubleStarsToolStripMenuItem).set_Text("Double stars");
			((ToolStripItem)toolStripSeparator33).set_Name("toolStripSeparator33");
			((ToolStripItem)toolStripSeparator33).set_Size(new Size(377, 6));
			((ToolStripItem)listCurrentEventSolutionsToolStripMenuItem1).set_Image((Image)Resources.ListMembers_2407_32);
			((ToolStripItem)listCurrentEventSolutionsToolStripMenuItem1).set_Name("listCurrentEventSolutionsToolStripMenuItem1");
			((ToolStripItem)listCurrentEventSolutionsToolStripMenuItem1).set_Size(new Size(380, 26));
			((ToolStripItem)listCurrentEventSolutionsToolStripMenuItem1).set_Text("List current-event double-star solution(s)");
			((ToolStripItem)listCurrentEventSolutionsToolStripMenuItem1).add_Click((EventHandler)listCurrentEventDoubleSolutionsToolStripMenuItem1_Click);
			((ToolStripItem)deleteCurrentEventSolutionsToolStripMenuItem).set_Image((Image)Resources.DeleteTablefromDatabase_270_32);
			((ToolStripItem)deleteCurrentEventSolutionsToolStripMenuItem).set_Name("deleteCurrentEventSolutionsToolStripMenuItem");
			((ToolStripItem)deleteCurrentEventSolutionsToolStripMenuItem).set_Size(new Size(380, 26));
			((ToolStripItem)deleteCurrentEventSolutionsToolStripMenuItem).set_Text("Delete current-event double-star solution(s)");
			((ToolStripItem)deleteCurrentEventSolutionsToolStripMenuItem).add_Click((EventHandler)deleteCurrentEventDoubleSolutionsToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator15).set_Name("toolStripSeparator15");
			((ToolStripItem)toolStripSeparator15).set_Size(new Size(377, 6));
			((ToolStripItem)satelliteToolStripMenuItem).set_BackColor(Color.LemonChiffon);
			((ToolStripItem)satelliteToolStripMenuItem).set_Font(new Font("Segoe UI", 12f, FontStyle.Bold));
			((ToolStripItem)satelliteToolStripMenuItem).set_ForeColor(Color.DarkGreen);
			((ToolStripItem)satelliteToolStripMenuItem).set_Name("satelliteToolStripMenuItem");
			((ToolStripItem)satelliteToolStripMenuItem).set_Size(new Size(380, 26));
			((ToolStripItem)satelliteToolStripMenuItem).set_Text("Satellite");
			((ToolStripItem)toolStripSeparator34).set_Name("toolStripSeparator34");
			((ToolStripItem)toolStripSeparator34).set_Size(new Size(377, 6));
			((ToolStripItem)listCurrenteventSatelliteSolutionToolStripMenuItem).set_Image((Image)Resources.ListMembers_2407_32);
			((ToolStripItem)listCurrenteventSatelliteSolutionToolStripMenuItem).set_Name("listCurrenteventSatelliteSolutionToolStripMenuItem");
			((ToolStripItem)listCurrenteventSatelliteSolutionToolStripMenuItem).set_Size(new Size(380, 26));
			((ToolStripItem)listCurrenteventSatelliteSolutionToolStripMenuItem).set_Text("List current-event satellite solution");
			((ToolStripItem)listCurrenteventSatelliteSolutionToolStripMenuItem).add_Click((EventHandler)listCurrenteventSatelliteSolutionToolStripMenuItem_Click);
			((ToolStripItem)deeteCurrenteventSatelliteSolutionToolStripMenuItem).set_Image((Image)Resources.DeleteTablefromDatabase_270_32);
			((ToolStripItem)deeteCurrenteventSatelliteSolutionToolStripMenuItem).set_Name("deeteCurrenteventSatelliteSolutionToolStripMenuItem");
			((ToolStripItem)deeteCurrenteventSatelliteSolutionToolStripMenuItem).set_Size(new Size(380, 26));
			((ToolStripItem)deeteCurrenteventSatelliteSolutionToolStripMenuItem).set_Text("Delete current-event satellite solution(s)");
			((ToolStripItem)deeteCurrenteventSatelliteSolutionToolStripMenuItem).add_Click((EventHandler)deeteCurrenteventSatelliteSolutionToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator16).set_Name("toolStripSeparator16");
			((ToolStripItem)toolStripSeparator16).set_Size(new Size(377, 6));
			((ToolStripItem)shapeModelsToolStripMenuItem1).set_BackColor(Color.LemonChiffon);
			((ToolStripItem)shapeModelsToolStripMenuItem1).set_Font(new Font("Segoe UI", 12f, FontStyle.Bold));
			((ToolStripItem)shapeModelsToolStripMenuItem1).set_ForeColor(Color.DarkGreen);
			((ToolStripItem)shapeModelsToolStripMenuItem1).set_Name("shapeModelsToolStripMenuItem1");
			((ToolStripItem)shapeModelsToolStripMenuItem1).set_Size(new Size(380, 26));
			((ToolStripItem)shapeModelsToolStripMenuItem1).set_Text("Shape models");
			((ToolStripItem)toolStripSeparator35).set_Name("toolStripSeparator35");
			((ToolStripItem)toolStripSeparator35).set_Size(new Size(377, 6));
			((ToolStripItem)listCurrenteventShapeModelsSolutionsToolStripMenuItem).set_Image((Image)Resources.ListMembers_2407_32);
			((ToolStripItem)listCurrenteventShapeModelsSolutionsToolStripMenuItem).set_ImageAlign(ContentAlignment.MiddleRight);
			((ToolStripItem)listCurrenteventShapeModelsSolutionsToolStripMenuItem).set_Name("listCurrenteventShapeModelsSolutionsToolStripMenuItem");
			((ToolStripItem)listCurrenteventShapeModelsSolutionsToolStripMenuItem).set_Size(new Size(380, 26));
			((ToolStripItem)listCurrenteventShapeModelsSolutionsToolStripMenuItem).set_Text("List current-event shape models solution(s)");
			((ToolStripItem)listCurrenteventShapeModelsSolutionsToolStripMenuItem).add_Click((EventHandler)listCurrenteventShapeModelsSolutionsToolStripMenuItem_Click);
			((ToolStripItem)deleteCurrenteventShapemodelSolutionsToolStripMenuItem).set_Image((Image)Resources.DeleteTablefromDatabase_270_32);
			((ToolStripItem)deleteCurrenteventShapemodelSolutionsToolStripMenuItem).set_Name("deleteCurrenteventShapemodelSolutionsToolStripMenuItem");
			((ToolStripItem)deleteCurrenteventShapemodelSolutionsToolStripMenuItem).set_Size(new Size(380, 26));
			((ToolStripItem)deleteCurrenteventShapemodelSolutionsToolStripMenuItem).set_Text("Delete current-event shape-model solution(s)");
			((ToolStripItem)deleteCurrenteventShapemodelSolutionsToolStripMenuItem).add_Click((EventHandler)deleteCurrenteventShapemodelSolutionsToolStripMenuItem_Click);
			((ToolStripItem)downloadShapeModelsToolStripMenuItem).set_Image((Image)Resources.Downloadwebsettings_10413);
			((ToolStripItem)downloadShapeModelsToolStripMenuItem).set_Name("downloadShapeModelsToolStripMenuItem");
			((ToolStripItem)downloadShapeModelsToolStripMenuItem).set_Size(new Size(380, 26));
			((ToolStripItem)downloadShapeModelsToolStripMenuItem).set_Text("Download shape models");
			((ToolStripItem)downloadShapeModelsToolStripMenuItem).add_Click((EventHandler)downloadShapeModelsToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator21).set_Name("toolStripSeparator21");
			((ToolStripItem)toolStripSeparator21).set_Size(new Size(377, 6));
			((ToolStripItem)astrometricSolutionToolStripMenuItem).set_BackColor(Color.LemonChiffon);
			((ToolStripItem)astrometricSolutionToolStripMenuItem).set_Font(new Font("Segoe UI", 12f, FontStyle.Bold));
			((ToolStripItem)astrometricSolutionToolStripMenuItem).set_ForeColor(Color.DarkGreen);
			((ToolStripItem)astrometricSolutionToolStripMenuItem).set_Name("astrometricSolutionToolStripMenuItem");
			((ToolStripItem)astrometricSolutionToolStripMenuItem).set_Size(new Size(380, 26));
			((ToolStripItem)astrometricSolutionToolStripMenuItem).set_Text("Astrometric solution");
			((ToolStripItem)toolStripSeparator36).set_Name("toolStripSeparator36");
			((ToolStripItem)toolStripSeparator36).set_Size(new Size(377, 6));
			((ToolStripItem)astrometricSolutionToolStripMenuItem1).set_Name("astrometricSolutionToolStripMenuItem1");
			astrometricSolutionToolStripMenuItem1.set_ShortcutKeys((Keys)121);
			((ToolStripItem)astrometricSolutionToolStripMenuItem1).set_Size(new Size(380, 26));
			((ToolStripItem)astrometricSolutionToolStripMenuItem1).set_Text("Astrometric solution");
			((ToolStripItem)astrometricSolutionToolStripMenuItem1).add_Click((EventHandler)astrometricSolutionToolStripMenuItem1_Click);
			((ToolStripItem)astrometricSolutionIncludingatConjunctionSolutionToolStripMenuItem).set_Font(new Font("Segoe UI", 8.25f));
			((ToolStripItem)astrometricSolutionIncludingatConjunctionSolutionToolStripMenuItem).set_Name("astrometricSolutionIncludingatConjunctionSolutionToolStripMenuItem");
			((ToolStripItem)astrometricSolutionIncludingatConjunctionSolutionToolStripMenuItem).set_Size(new Size(380, 26));
			((ToolStripItem)astrometricSolutionIncludingatConjunctionSolutionToolStripMenuItem).set_Text("Astrometric solution - including 'at conjunction' solution");
			((ToolStripItem)astrometricSolutionIncludingatConjunctionSolutionToolStripMenuItem).add_Click((EventHandler)astrometricSolutionIncludingatConjunctionSolutionToolStripMenuItem_Click);
			((ToolStripDropDownItem)listOffsetsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[41]
			{
				(ToolStripItem)observerrelatedInformationToolStripMenuItem,
				(ToolStripItem)toolStripSeparator28,
				(ToolStripItem)fromPredictionToolStripMenuItem,
				(ToolStripItem)possibleTimebaseCorrectionsToolStripMenuItem,
				(ToolStripItem)relativePathDistancesToolStripMenuItem,
				(ToolStripItem)chordLengthsToolStripMenuItem,
				(ToolStripItem)eventCoordinatesToolStripMenuItem,
				(ToolStripItem)observerVelocityRelativeToAsteriodToolStripMenuItem,
				(ToolStripItem)gravitationalLightDeflectionToolStripMenuItem,
				(ToolStripItem)toolStripSeparator27,
				(ToolStripItem)informationRelatedToTheStarToolStripMenuItem,
				(ToolStripItem)toolStripSeparator29,
				(ToolStripItem)starChartToolStripMenuItem,
				(ToolStripItem)nearbyStarsToolStripMenuItem,
				(ToolStripItem)starDetailsToolStripMenuItem,
				(ToolStripItem)starDiameterFromVizieRToolStripMenuItem,
				(ToolStripItem)fresnelDiffractionLightCurvesToolStripMenuItem,
				(ToolStripItem)stellarDiameterFresnelDiffractionToolStripMenuItem,
				(ToolStripItem)starDiameterModelLightCurvesToolStripMenuItem,
				(ToolStripItem)gaiaDoubleStarsnearbyToolStripMenuItem,
				(ToolStripItem)displayWDSInterferometricAndVariableStarDataToolStripMenuItem,
				(ToolStripItem)alternativeStarIdentifiersToolStripMenuItem,
				(ToolStripItem)previousOccultationsOfThisStarToolStripMenuItem,
				(ToolStripItem)toolStripSeparator13,
				(ToolStripItem)informationRelatedToTheAsteroidToolStripMenuItem,
				(ToolStripItem)toolStripSeparator30,
				(ToolStripItem)asteroidLightCurveDataToolStripMenuItem,
				(ToolStripItem)satelliteIRDiametersToolStripMenuItem,
				(ToolStripItem)jPLSmallBodyDatabaseToolStripMenuItem,
				(ToolStripItem)binaryAsteroidDetailsToolStripMenuItem,
				(ToolStripItem)binaryAsteroidsEstimateOrbitSizeOrPeriodToolStripMenuItem,
				(ToolStripItem)eventsWhereDAMITHasBeenUpdatedToolStripMenuItem,
				(ToolStripItem)asteroidNumberFromDAMITToolStripMenuItem,
				(ToolStripItem)astrometryHeldByTheMinorPlanetCenterToolStripMenuItem,
				(ToolStripItem)toolStripSeparator31,
				(ToolStripItem)lightCurvesToolStripMenuItem,
				(ToolStripItem)toolStripSeparator32,
				(ToolStripItem)displayLightCurveToolStripMenuItem,
				(ToolStripItem)displaySubmittedLightCurvesToolStripMenuItem,
				(ToolStripItem)toolStripSeparator22,
				(ToolStripItem)setListsToAutodisplayToolStripMenuItem
			});
			((ToolStripItem)listOffsetsToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)listOffsetsToolStripMenuItem).set_Name("listOffsetsToolStripMenuItem");
			((ToolStripItem)listOffsetsToolStripMenuItem).set_Size(new Size(65, 24));
			((ToolStripItem)listOffsetsToolStripMenuItem).set_Text("List...      ");
			((ToolStripItem)observerrelatedInformationToolStripMenuItem).set_BackColor(Color.LemonChiffon);
			((ToolStripItem)observerrelatedInformationToolStripMenuItem).set_Font(new Font("Segoe UI", 12f, FontStyle.Bold));
			((ToolStripItem)observerrelatedInformationToolStripMenuItem).set_ForeColor(Color.DarkGreen);
			((ToolStripItem)observerrelatedInformationToolStripMenuItem).set_Name("observerrelatedInformationToolStripMenuItem");
			((ToolStripItem)observerrelatedInformationToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)observerrelatedInformationToolStripMenuItem).set_Text("Event-related information");
			((ToolStripItem)toolStripSeparator28).set_Name("toolStripSeparator28");
			((ToolStripItem)toolStripSeparator28).set_Size(new Size(379, 6));
			((ToolStripItem)fromPredictionToolStripMenuItem).set_Name("fromPredictionToolStripMenuItem");
			((ToolStripItem)fromPredictionToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)fromPredictionToolStripMenuItem).set_Text("Offset from Prediction");
			((ToolStripItem)fromPredictionToolStripMenuItem).add_Click((EventHandler)fromPredictionToolStripMenuItem_Click);
			((ToolStripItem)possibleTimebaseCorrectionsToolStripMenuItem).set_Name("possibleTimebaseCorrectionsToolStripMenuItem");
			((ToolStripItem)possibleTimebaseCorrectionsToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)possibleTimebaseCorrectionsToolStripMenuItem).set_Text("Possible time-base corrections");
			((ToolStripItem)possibleTimebaseCorrectionsToolStripMenuItem).add_Click((EventHandler)possibleTimebaseCorrectionsToolStripMenuItem_Click);
			((ToolStripItem)relativePathDistancesToolStripMenuItem).set_Name("relativePathDistancesToolStripMenuItem");
			((ToolStripItem)relativePathDistancesToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)relativePathDistancesToolStripMenuItem).set_Text("Relative path distances");
			((ToolStripItem)relativePathDistancesToolStripMenuItem).add_Click((EventHandler)relativePathDistancesToolStripMenuItem_Click);
			((ToolStripItem)chordLengthsToolStripMenuItem).set_Name("chordLengthsToolStripMenuItem");
			((ToolStripItem)chordLengthsToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)chordLengthsToolStripMenuItem).set_Text("Chord lengths");
			((ToolStripItem)chordLengthsToolStripMenuItem).add_Click((EventHandler)chordLengthsToolStripMenuItem_Click);
			((ToolStripItem)eventCoordinatesToolStripMenuItem).set_Name("eventCoordinatesToolStripMenuItem");
			((ToolStripItem)eventCoordinatesToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)eventCoordinatesToolStripMenuItem).set_Text("Event coords, and motion PA, on the Fundamental Plane");
			((ToolStripItem)eventCoordinatesToolStripMenuItem).add_Click((EventHandler)eventCoordinatesToolStripMenuItem_Click);
			((ToolStripItem)observerVelocityRelativeToAsteriodToolStripMenuItem).set_Name("observerVelocityRelativeToAsteriodToolStripMenuItem");
			((ToolStripItem)observerVelocityRelativeToAsteriodToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)observerVelocityRelativeToAsteriodToolStripMenuItem).set_Text("Observer velocity relative to asteriod");
			((ToolStripItem)observerVelocityRelativeToAsteriodToolStripMenuItem).add_Click((EventHandler)observerVelocityRelativeToAsteriodToolStripMenuItem_Click);
			((ToolStripItem)gravitationalLightDeflectionToolStripMenuItem).set_Name("gravitationalLightDeflectionToolStripMenuItem");
			((ToolStripItem)gravitationalLightDeflectionToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)gravitationalLightDeflectionToolStripMenuItem).set_Text("Gravitational light deflection");
			((ToolStripItem)gravitationalLightDeflectionToolStripMenuItem).add_Click((EventHandler)GravitationalLightDeflectionToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator27).set_Name("toolStripSeparator27");
			((ToolStripItem)toolStripSeparator27).set_Size(new Size(379, 6));
			((ToolStripItem)informationRelatedToTheStarToolStripMenuItem).set_BackColor(Color.PaleTurquoise);
			((ToolStripItem)informationRelatedToTheStarToolStripMenuItem).set_Font(new Font("Segoe UI", 12f, FontStyle.Bold));
			((ToolStripItem)informationRelatedToTheStarToolStripMenuItem).set_ForeColor(Color.DarkGreen);
			((ToolStripItem)informationRelatedToTheStarToolStripMenuItem).set_Name("informationRelatedToTheStarToolStripMenuItem");
			((ToolStripItem)informationRelatedToTheStarToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)informationRelatedToTheStarToolStripMenuItem).set_Text("Star");
			((ToolStripItem)toolStripSeparator29).set_Name("toolStripSeparator29");
			((ToolStripItem)toolStripSeparator29).set_Size(new Size(379, 6));
			((ToolStripItem)starChartToolStripMenuItem).set_Image((Image)Resources.Stars2);
			((ToolStripItem)starChartToolStripMenuItem).set_Name("starChartToolStripMenuItem");
			((ToolStripItem)starChartToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)starChartToolStripMenuItem).set_Text("Star chart");
			((ToolStripItem)starChartToolStripMenuItem).add_Click((EventHandler)starChartToolStripMenuItem_Click);
			((ToolStripItem)nearbyStarsToolStripMenuItem).set_Name("nearbyStarsToolStripMenuItem");
			((ToolStripItem)nearbyStarsToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)nearbyStarsToolStripMenuItem).set_Text("Nearby stars");
			((ToolStripItem)nearbyStarsToolStripMenuItem).add_Click((EventHandler)nearbyStarsToolStripMenuItem_Click);
			((ToolStripItem)starDetailsToolStripMenuItem).set_Name("starDetailsToolStripMenuItem");
			((ToolStripItem)starDetailsToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)starDetailsToolStripMenuItem).set_Text("Star details");
			((ToolStripItem)starDetailsToolStripMenuItem).add_Click((EventHandler)starDetailsToolStripMenuItem_Click);
			((ToolStripItem)starDiameterFromVizieRToolStripMenuItem).set_Name("starDiameterFromVizieRToolStripMenuItem");
			((ToolStripItem)starDiameterFromVizieRToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)starDiameterFromVizieRToolStripMenuItem).set_Text("Star diameter - accurate Gaia estimate from VizieR");
			((ToolStripItem)starDiameterFromVizieRToolStripMenuItem).add_Click((EventHandler)starDiameterFromVizieRToolStripMenuItem_Click);
			((ToolStripItem)fresnelDiffractionLightCurvesToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)fresnelDiffractionLightCurvesToolStripMenuItem).set_Name("fresnelDiffractionLightCurvesToolStripMenuItem");
			((ToolStripItem)fresnelDiffractionLightCurvesToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)fresnelDiffractionLightCurvesToolStripMenuItem).set_Text("Fresnel diffraction light curve examples");
			((ToolStripItem)fresnelDiffractionLightCurvesToolStripMenuItem).add_Click((EventHandler)fresnelDiffractionLightCurvesToolStripMenuItem_Click);
			((ToolStripItem)stellarDiameterFresnelDiffractionToolStripMenuItem).set_Image((Image)Resources.Fresnel);
			((ToolStripItem)stellarDiameterFresnelDiffractionToolStripMenuItem).set_Name("stellarDiameterFresnelDiffractionToolStripMenuItem");
			((ToolStripItem)stellarDiameterFresnelDiffractionToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)stellarDiameterFresnelDiffractionToolStripMenuItem).set_Text("Stellar diameter && Fresnel diffraction");
			((ToolStripItem)stellarDiameterFresnelDiffractionToolStripMenuItem).add_Click((EventHandler)stellarDiameterFresnelDiffractionToolStripMenuItem_Click);
			((ToolStripItem)starDiameterModelLightCurvesToolStripMenuItem).set_Name("starDiameterModelLightCurvesToolStripMenuItem");
			((ToolStripItem)starDiameterModelLightCurvesToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)starDiameterModelLightCurvesToolStripMenuItem).set_Text("Star diameter - model  light curves");
			((ToolStripItem)starDiameterModelLightCurvesToolStripMenuItem).add_Click((EventHandler)starDiameterModelLightCurvesToolStripMenuItem_Click);
			((ToolStripItem)gaiaDoubleStarsnearbyToolStripMenuItem).set_Image((Image)Resources.Gaia);
			((ToolStripItem)gaiaDoubleStarsnearbyToolStripMenuItem).set_Name("gaiaDoubleStarsnearbyToolStripMenuItem");
			((ToolStripItem)gaiaDoubleStarsnearbyToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)gaiaDoubleStarsnearbyToolStripMenuItem).set_Text("Gaia double stars (nearby)");
			((ToolStripItem)gaiaDoubleStarsnearbyToolStripMenuItem).add_Click((EventHandler)gaiaDoubleStarsnearbyToolStripMenuItem_Click);
			((ToolStripItem)displayWDSInterferometricAndVariableStarDataToolStripMenuItem).set_Image((Image)Resources.services);
			((ToolStripItem)displayWDSInterferometricAndVariableStarDataToolStripMenuItem).set_Name("displayWDSInterferometricAndVariableStarDataToolStripMenuItem");
			((ToolStripItem)displayWDSInterferometricAndVariableStarDataToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)displayWDSInterferometricAndVariableStarDataToolStripMenuItem).set_Text("WDS, Interferometric, and Variable star data");
			((ToolStripItem)displayWDSInterferometricAndVariableStarDataToolStripMenuItem).add_Click((EventHandler)displayWDSInterferometricAndVariableStarDataToolStripMenuItem_Click);
			((ToolStripItem)alternativeStarIdentifiersToolStripMenuItem).set_Name("alternativeStarIdentifiersToolStripMenuItem");
			((ToolStripItem)alternativeStarIdentifiersToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)alternativeStarIdentifiersToolStripMenuItem).set_Text("Alternative star identifiers");
			((ToolStripItem)alternativeStarIdentifiersToolStripMenuItem).add_Click((EventHandler)alternativeStarIdentifiersToolStripMenuItem_Click);
			((ToolStripItem)previousOccultationsOfThisStarToolStripMenuItem).set_Name("previousOccultationsOfThisStarToolStripMenuItem");
			((ToolStripItem)previousOccultationsOfThisStarToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)previousOccultationsOfThisStarToolStripMenuItem).set_Text("Previous occultations of this star");
			((ToolStripItem)previousOccultationsOfThisStarToolStripMenuItem).add_Click((EventHandler)previousOccultationsOfThisStarToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator13).set_Name("toolStripSeparator13");
			((ToolStripItem)toolStripSeparator13).set_Size(new Size(379, 6));
			((ToolStripItem)informationRelatedToTheAsteroidToolStripMenuItem).set_BackColor(Color.PaleGreen);
			((ToolStripItem)informationRelatedToTheAsteroidToolStripMenuItem).set_Font(new Font("Segoe UI", 12f, FontStyle.Bold));
			((ToolStripItem)informationRelatedToTheAsteroidToolStripMenuItem).set_ForeColor(Color.DarkGreen);
			((ToolStripItem)informationRelatedToTheAsteroidToolStripMenuItem).set_Name("informationRelatedToTheAsteroidToolStripMenuItem");
			((ToolStripItem)informationRelatedToTheAsteroidToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)informationRelatedToTheAsteroidToolStripMenuItem).set_Text("Asteroid");
			((ToolStripItem)toolStripSeparator30).set_Name("toolStripSeparator30");
			((ToolStripItem)toolStripSeparator30).set_Size(new Size(379, 6));
			((ToolStripDropDownItem)asteroidLightCurveDataToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)summaryDataToolStripMenuItem,
				(ToolStripItem)rotationLightCurvesBehrandToolStripMenuItem,
				(ToolStripItem)asteroidLightcurvePhotometryDatabaseToolStripMenuItem,
				(ToolStripItem)asteroidmagsFromGAIAToolStripMenuItem
			});
			((ToolStripItem)asteroidLightCurveDataToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)asteroidLightCurveDataToolStripMenuItem).set_Image((Image)Resources.GRAPH07);
			((ToolStripItem)asteroidLightCurveDataToolStripMenuItem).set_Name("asteroidLightCurveDataToolStripMenuItem");
			((ToolStripItem)asteroidLightCurveDataToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)asteroidLightCurveDataToolStripMenuItem).set_Text("Asteroid light curve data");
			((ToolStripItem)asteroidLightCurveDataToolStripMenuItem).add_Click((EventHandler)asteroidLightCurveDataToolStripMenuItem_Click);
			((ToolStripItem)summaryDataToolStripMenuItem).set_Name("summaryDataToolStripMenuItem");
			((ToolStripItem)summaryDataToolStripMenuItem).set_Size(new Size(372, 22));
			((ToolStripItem)summaryDataToolStripMenuItem).set_Text("Asteroid LightCurve DataBase (ALCD) - Summary data");
			((ToolStripItem)summaryDataToolStripMenuItem).add_Click((EventHandler)summaryDataToolStripMenuItem_Click);
			((ToolStripDropDownItem)rotationLightCurvesBehrandToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[7]
			{
				(ToolStripItem)approximateRangeToolStripMenuItem,
				(ToolStripItem)toolStripMenuItem7,
				(ToolStripItem)toolStripMenuItem9,
				(ToolStripItem)toolStripMenuItem10,
				(ToolStripItem)toolStripMenuItem11,
				(ToolStripItem)toolStripMenuItem13,
				(ToolStripItem)cometsToolStripMenuItem
			});
			((ToolStripItem)rotationLightCurvesBehrandToolStripMenuItem).set_Name("rotationLightCurvesBehrandToolStripMenuItem");
			((ToolStripItem)rotationLightCurvesBehrandToolStripMenuItem).set_Size(new Size(372, 22));
			((ToolStripItem)rotationLightCurvesBehrandToolStripMenuItem).set_Text("Rotation light curves - Behrend");
			((ToolStripItem)approximateRangeToolStripMenuItem).set_ForeColor(Color.DarkGreen);
			((ToolStripItem)approximateRangeToolStripMenuItem).set_Name("approximateRangeToolStripMenuItem");
			((ToolStripItem)approximateRangeToolStripMenuItem).set_Size(new Size(187, 22));
			((ToolStripItem)approximateRangeToolStripMenuItem).set_Text("Approximate ranges");
			((ToolStripItem)toolStripMenuItem7).set_Name("toolStripMenuItem7");
			((ToolStripItem)toolStripMenuItem7).set_Size(new Size(187, 22));
			((ToolStripItem)toolStripMenuItem7).set_Text("(1) - (351)");
			((ToolStripItem)toolStripMenuItem7).add_Click((EventHandler)toolStripMenuItem7_Click);
			((ToolStripItem)toolStripMenuItem9).set_Name("toolStripMenuItem9");
			((ToolStripItem)toolStripMenuItem9).set_Size(new Size(187, 22));
			((ToolStripItem)toolStripMenuItem9).set_Text("(352) - (741)");
			((ToolStripItem)toolStripMenuItem9).add_Click((EventHandler)toolStripMenuItem9_Click);
			((ToolStripItem)toolStripMenuItem10).set_Name("toolStripMenuItem10");
			((ToolStripItem)toolStripMenuItem10).set_Size(new Size(187, 22));
			((ToolStripItem)toolStripMenuItem10).set_Text("(742) - (1275)");
			((ToolStripItem)toolStripMenuItem10).add_Click((EventHandler)toolStripMenuItem10_Click);
			((ToolStripItem)toolStripMenuItem11).set_Name("toolStripMenuItem11");
			((ToolStripItem)toolStripMenuItem11).set_Size(new Size(187, 22));
			((ToolStripItem)toolStripMenuItem11).set_Text("(1276) - (4490)");
			((ToolStripItem)toolStripMenuItem11).add_Click((EventHandler)toolStripMenuItem11_Click);
			((ToolStripItem)toolStripMenuItem13).set_Name("toolStripMenuItem13");
			((ToolStripItem)toolStripMenuItem13).set_Size(new Size(187, 22));
			((ToolStripItem)toolStripMenuItem13).set_Text("(4492) +");
			((ToolStripItem)toolStripMenuItem13).add_Click((EventHandler)toolStripMenuItem13_Click);
			((ToolStripItem)cometsToolStripMenuItem).set_Name("cometsToolStripMenuItem");
			((ToolStripItem)cometsToolStripMenuItem).set_Size(new Size(187, 22));
			((ToolStripItem)cometsToolStripMenuItem).set_Text("Comets");
			((ToolStripItem)cometsToolStripMenuItem).add_Click((EventHandler)cometsToolStripMenuItem_Click);
			((ToolStripItem)asteroidLightcurvePhotometryDatabaseToolStripMenuItem).set_Name("asteroidLightcurvePhotometryDatabaseToolStripMenuItem");
			asteroidLightcurvePhotometryDatabaseToolStripMenuItem.set_ShowShortcutKeys(false);
			((ToolStripItem)asteroidLightcurvePhotometryDatabaseToolStripMenuItem).set_Size(new Size(372, 22));
			((ToolStripItem)asteroidLightcurvePhotometryDatabaseToolStripMenuItem).set_Text("Asteroid Lightcurve Photometry Database [ ALCDEF ]");
			((ToolStripItem)asteroidLightcurvePhotometryDatabaseToolStripMenuItem).add_Click((EventHandler)asteroidLightcurvePhotometryDatabaseToolStripMenuItem_Click);
			((ToolStripItem)asteroidmagsFromGAIAToolStripMenuItem).set_Name("asteroidmagsFromGAIAToolStripMenuItem");
			((ToolStripItem)asteroidmagsFromGAIAToolStripMenuItem).set_Size(new Size(372, 22));
			((ToolStripItem)asteroidmagsFromGAIAToolStripMenuItem).set_Text("Assess Gaia mag data; Assess  photometric data");
			((ToolStripItem)asteroidmagsFromGAIAToolStripMenuItem).add_Click((EventHandler)asteroidmagsFromGAIAToolStripMenuItem_Click);
			((ToolStripItem)satelliteIRDiametersToolStripMenuItem).set_Name("satelliteIRDiametersToolStripMenuItem");
			((ToolStripItem)satelliteIRDiametersToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)satelliteIRDiametersToolStripMenuItem).set_Text("Asteroid IR diameters ;  diameters from H0");
			((ToolStripItem)satelliteIRDiametersToolStripMenuItem).add_Click((EventHandler)satelliteIRDiametersToolStripMenuItem_Click);
			((ToolStripItem)jPLSmallBodyDatabaseToolStripMenuItem).set_Name("jPLSmallBodyDatabaseToolStripMenuItem");
			((ToolStripItem)jPLSmallBodyDatabaseToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)jPLSmallBodyDatabaseToolStripMenuItem).set_Text("JPL Small Body Database for this asteroid or comet");
			((ToolStripItem)jPLSmallBodyDatabaseToolStripMenuItem).add_Click((EventHandler)jPLSmallBodyDatabaseToolStripMenuItem_Click);
			((ToolStripItem)binaryAsteroidDetailsToolStripMenuItem).set_Name("binaryAsteroidDetailsToolStripMenuItem");
			((ToolStripItem)binaryAsteroidDetailsToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)binaryAsteroidDetailsToolStripMenuItem).set_Text("Binary asteroid - information for this asteroid");
			((ToolStripItem)binaryAsteroidDetailsToolStripMenuItem).add_Click((EventHandler)binaryAsteroidDetailsToolStripMenuItem_Click);
			((ToolStripItem)binaryAsteroidsEstimateOrbitSizeOrPeriodToolStripMenuItem).set_Name("binaryAsteroidsEstimateOrbitSizeOrPeriodToolStripMenuItem");
			((ToolStripItem)binaryAsteroidsEstimateOrbitSizeOrPeriodToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)binaryAsteroidsEstimateOrbitSizeOrPeriodToolStripMenuItem).set_Text("Binary asteroids - estimate orbit size, or period");
			((ToolStripItem)binaryAsteroidsEstimateOrbitSizeOrPeriodToolStripMenuItem).add_Click((EventHandler)binaryAsteroidsEstimateOrbitSizeOrPeriodToolStripMenuItem_Click);
			((ToolStripItem)eventsWhereDAMITHasBeenUpdatedToolStripMenuItem).set_Name("eventsWhereDAMITHasBeenUpdatedToolStripMenuItem");
			((ToolStripItem)eventsWhereDAMITHasBeenUpdatedToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)eventsWhereDAMITHasBeenUpdatedToolStripMenuItem).set_Text("List events where DAMIT has been updated");
			((ToolStripItem)eventsWhereDAMITHasBeenUpdatedToolStripMenuItem).add_Click((EventHandler)eventsWhereDAMITHasBeenUpdatedToolStripMenuItem_Click);
			((ToolStripItem)asteroidNumberFromDAMITToolStripMenuItem).set_Name("asteroidNumberFromDAMITToolStripMenuItem");
			((ToolStripItem)asteroidNumberFromDAMITToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)asteroidNumberFromDAMITToolStripMenuItem).set_Text("Asteroid number from DAMIT #");
			((ToolStripItem)asteroidNumberFromDAMITToolStripMenuItem).add_Click((EventHandler)asteroidNumberFromDAMITToolStripMenuItem_Click);
			((ToolStripItem)astrometryHeldByTheMinorPlanetCenterToolStripMenuItem).set_Font(new Font("Segoe UI", 7f, FontStyle.Bold));
			((ToolStripItem)astrometryHeldByTheMinorPlanetCenterToolStripMenuItem).set_ForeColor(Color.MidnightBlue);
			((ToolStripItem)astrometryHeldByTheMinorPlanetCenterToolStripMenuItem).set_Image((Image)Resources.Ruler_48x);
			((ToolStripItem)astrometryHeldByTheMinorPlanetCenterToolStripMenuItem).set_Name("astrometryHeldByTheMinorPlanetCenterToolStripMenuItem");
			((ToolStripItem)astrometryHeldByTheMinorPlanetCenterToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)astrometryHeldByTheMinorPlanetCenterToolStripMenuItem).set_Text("Astrometry at Minor Planet Center  [Occn : search for '275 ']");
			((ToolStripItem)astrometryHeldByTheMinorPlanetCenterToolStripMenuItem).add_Click((EventHandler)astrometryHeldByTheMinorPlanetCenterToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator31).set_Name("toolStripSeparator31");
			((ToolStripItem)toolStripSeparator31).set_Size(new Size(379, 6));
			((ToolStripItem)lightCurvesToolStripMenuItem).set_BackColor(Color.Thistle);
			((ToolStripItem)lightCurvesToolStripMenuItem).set_Font(new Font("Segoe UI", 12f, FontStyle.Bold));
			((ToolStripItem)lightCurvesToolStripMenuItem).set_ForeColor(Color.DarkGreen);
			((ToolStripItem)lightCurvesToolStripMenuItem).set_Name("lightCurvesToolStripMenuItem");
			((ToolStripItem)lightCurvesToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)lightCurvesToolStripMenuItem).set_Text("Light curves");
			((ToolStripItem)toolStripSeparator32).set_Name("toolStripSeparator32");
			((ToolStripItem)toolStripSeparator32).set_Size(new Size(379, 6));
			((ToolStripItem)displayLightCurveToolStripMenuItem).set_Name("displayLightCurveToolStripMenuItem");
			((ToolStripItem)displayLightCurveToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)displayLightCurveToolStripMenuItem).set_Text("ALL Light curves by this asteroid");
			((ToolStripItem)displayLightCurveToolStripMenuItem).add_Click((EventHandler)displayLightCurveToolStripMenuItem_Click);
			((ToolStripItem)displaySubmittedLightCurvesToolStripMenuItem).set_Font(new Font("Segoe UI", 8.25f, FontStyle.Bold));
			((ToolStripItem)displaySubmittedLightCurvesToolStripMenuItem).set_ForeColor(Color.Gray);
			((ToolStripItem)displaySubmittedLightCurvesToolStripMenuItem).set_Name("displaySubmittedLightCurvesToolStripMenuItem");
			((ToolStripItem)displaySubmittedLightCurvesToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)displaySubmittedLightCurvesToolStripMenuItem).set_Text("... all pending light curves by this asteroid [admin only]");
			((ToolStripItem)displaySubmittedLightCurvesToolStripMenuItem).add_Click((EventHandler)displaySubmittedLightCurvesToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator22).set_Name("toolStripSeparator22");
			((ToolStripItem)toolStripSeparator22).set_Size(new Size(379, 6));
			setListsToAutodisplayToolStripMenuItem.set_Checked(true);
			setListsToAutodisplayToolStripMenuItem.set_CheckOnClick(true);
			setListsToAutodisplayToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripDropDownItem)setListsToAutodisplayToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[12]
			{
				(ToolStripItem)AutoNearbyStarstoolStripMenuItem7,
				(ToolStripItem)AutoStarDiametertoolStripMenuItem,
				(ToolStripItem)AutoLightCuveDatatoolStripMenuItem,
				(ToolStripItem)shapeModelFitsToolStripMenuItem,
				(ToolStripItem)doubleStarSolutionsToolStripMenuItem,
				(ToolStripItem)toolStripMenuItem1,
				(ToolStripItem)toolStripMenuItem2,
				(ToolStripItem)toolStripMenuItem3,
				(ToolStripItem)toolStripMenuItem5,
				(ToolStripItem)toolStripMenuItem4,
				(ToolStripItem)toolStripMenuItem6,
				(ToolStripItem)toolStripMenuItem8
			});
			((ToolStripItem)setListsToAutodisplayToolStripMenuItem).set_Name("setListsToAutodisplayToolStripMenuItem");
			((ToolStripItem)setListsToAutodisplayToolStripMenuItem).set_Size(new Size(382, 26));
			((ToolStripItem)setListsToAutodisplayToolStripMenuItem).set_Text("Set lists to auto-display");
			AutoNearbyStarstoolStripMenuItem7.set_Checked(Settings.Default.AutoNearbyStars);
			AutoNearbyStarstoolStripMenuItem7.set_CheckOnClick(true);
			AutoNearbyStarstoolStripMenuItem7.set_CheckState((CheckState)1);
			((ToolStripItem)AutoNearbyStarstoolStripMenuItem7).set_Name("AutoNearbyStarstoolStripMenuItem7");
			((ToolStripItem)AutoNearbyStarstoolStripMenuItem7).set_Size(new Size(282, 22));
			((ToolStripItem)AutoNearbyStarstoolStripMenuItem7).set_Text("Nearby stars");
			AutoStarDiametertoolStripMenuItem.set_Checked(Settings.Default.AutoStarDiameter);
			AutoStarDiametertoolStripMenuItem.set_CheckOnClick(true);
			AutoStarDiametertoolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)AutoStarDiametertoolStripMenuItem).set_Name("AutoStarDiametertoolStripMenuItem");
			((ToolStripItem)AutoStarDiametertoolStripMenuItem).set_Size(new Size(282, 22));
			((ToolStripItem)AutoStarDiametertoolStripMenuItem).set_Text("Stellar diameter && Fresnel diffraction");
			AutoLightCuveDatatoolStripMenuItem.set_Checked(Settings.Default.AutoLightCurveData);
			AutoLightCuveDatatoolStripMenuItem.set_CheckOnClick(true);
			AutoLightCuveDatatoolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)AutoLightCuveDatatoolStripMenuItem).set_Name("AutoLightCuveDatatoolStripMenuItem");
			((ToolStripItem)AutoLightCuveDatatoolStripMenuItem).set_Size(new Size(282, 22));
			((ToolStripItem)AutoLightCuveDatatoolStripMenuItem).set_Text("Asteroid light curve data");
			shapeModelFitsToolStripMenuItem.set_Checked(Settings.Default.AutoShapeModels);
			shapeModelFitsToolStripMenuItem.set_CheckOnClick(true);
			shapeModelFitsToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)shapeModelFitsToolStripMenuItem).set_Name("shapeModelFitsToolStripMenuItem");
			((ToolStripItem)shapeModelFitsToolStripMenuItem).set_Size(new Size(282, 22));
			((ToolStripItem)shapeModelFitsToolStripMenuItem).set_Text("Shape model solutions");
			doubleStarSolutionsToolStripMenuItem.set_Checked(Settings.Default.AutoDoubles);
			doubleStarSolutionsToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)doubleStarSolutionsToolStripMenuItem).set_Name("doubleStarSolutionsToolStripMenuItem");
			((ToolStripItem)doubleStarSolutionsToolStripMenuItem).set_Size(new Size(282, 22));
			((ToolStripItem)doubleStarSolutionsToolStripMenuItem).set_Text("Double star solutions");
			toolStripMenuItem1.set_Checked(Settings.Default.AutoPredictionOffset);
			toolStripMenuItem1.set_CheckOnClick(true);
			toolStripMenuItem1.set_CheckState((CheckState)1);
			((ToolStripItem)toolStripMenuItem1).set_Enabled(false);
			((ToolStripItem)toolStripMenuItem1).set_Name("toolStripMenuItem1");
			((ToolStripItem)toolStripMenuItem1).set_Size(new Size(282, 22));
			((ToolStripItem)toolStripMenuItem1).set_Text("Offset from Prediction");
			toolStripMenuItem2.set_Checked(Settings.Default.AutoTimeBaseOffset);
			toolStripMenuItem2.set_CheckOnClick(true);
			toolStripMenuItem2.set_CheckState((CheckState)1);
			((ToolStripItem)toolStripMenuItem2).set_Enabled(false);
			((ToolStripItem)toolStripMenuItem2).set_Name("toolStripMenuItem2");
			((ToolStripItem)toolStripMenuItem2).set_Size(new Size(282, 22));
			((ToolStripItem)toolStripMenuItem2).set_Text("Possible time-base corrections");
			toolStripMenuItem3.set_Checked(Settings.Default.AutoRelativePathDistances);
			toolStripMenuItem3.set_CheckOnClick(true);
			toolStripMenuItem3.set_CheckState((CheckState)1);
			((ToolStripItem)toolStripMenuItem3).set_Enabled(false);
			((ToolStripItem)toolStripMenuItem3).set_Name("toolStripMenuItem3");
			((ToolStripItem)toolStripMenuItem3).set_Size(new Size(282, 22));
			((ToolStripItem)toolStripMenuItem3).set_Text("Relative path distances");
			toolStripMenuItem5.set_Checked(Settings.Default.AutoChordLengths);
			toolStripMenuItem5.set_CheckOnClick(true);
			toolStripMenuItem5.set_CheckState((CheckState)1);
			((ToolStripItem)toolStripMenuItem5).set_Enabled(false);
			((ToolStripItem)toolStripMenuItem5).set_Name("toolStripMenuItem5");
			((ToolStripItem)toolStripMenuItem5).set_Size(new Size(282, 22));
			((ToolStripItem)toolStripMenuItem5).set_Text("Chord lengths");
			toolStripMenuItem4.set_Checked(Settings.Default.AutoLightCurve);
			toolStripMenuItem4.set_CheckOnClick(true);
			toolStripMenuItem4.set_CheckState((CheckState)1);
			((ToolStripItem)toolStripMenuItem4).set_Enabled(false);
			((ToolStripItem)toolStripMenuItem4).set_Name("toolStripMenuItem4");
			((ToolStripItem)toolStripMenuItem4).set_Size(new Size(282, 22));
			((ToolStripItem)toolStripMenuItem4).set_Text("Display Star light curve");
			toolStripMenuItem6.set_Checked(Settings.Default.AutoStarDetails);
			toolStripMenuItem6.set_CheckOnClick(true);
			toolStripMenuItem6.set_CheckState((CheckState)1);
			((ToolStripItem)toolStripMenuItem6).set_Enabled(false);
			((ToolStripItem)toolStripMenuItem6).set_Name("toolStripMenuItem6");
			((ToolStripItem)toolStripMenuItem6).set_Size(new Size(282, 22));
			((ToolStripItem)toolStripMenuItem6).set_Text("Star details");
			toolStripMenuItem8.set_Checked(Settings.Default.AutoIRDiameter);
			toolStripMenuItem8.set_CheckOnClick(true);
			toolStripMenuItem8.set_CheckState((CheckState)1);
			((ToolStripItem)toolStripMenuItem8).set_Enabled(false);
			((ToolStripItem)toolStripMenuItem8).set_Name("toolStripMenuItem8");
			((ToolStripItem)toolStripMenuItem8).set_Size(new Size(282, 22));
			((ToolStripItem)toolStripMenuItem8).set_Text("Asteroid IR diameters");
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)componentResourceManager.GetObject("helpToolStripMenuItem.Image"));
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			helpToolStripMenuItem.set_ShortcutKeys((Keys)131144);
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(76, 24));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)componentResourceManager.GetObject("exitToolStripMenuItem.Image"));
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(70, 24));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit    ");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((ToolStripDropDownItem)ExportImport).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[15]
			{
				(ToolStripItem)helpOnExportImportToolStripMenuItem,
				(ToolStripItem)toolStripSeparator8,
				(ToolStripItem)createNewExportFileToolStripMenuItem,
				(ToolStripItem)selectExportFileToolStripMenuItem,
				(ToolStripItem)toolStripMenuItemExportFile2,
				(ToolStripItem)toolStripSeparator17,
				(ToolStripItem)openCommentsFormForCurrentEventToolStripMenuItem,
				(ToolStripItem)exportCurrentEventToolStripMenuItem,
				(ToolStripItem)toolStripSeparator18,
				(ToolStripItem)toolStripSeparator19,
				(ToolStripItem)selectImportFileImportToolStripMenuItem,
				(ToolStripItem)toolStripMenuItemImportFile,
				(ToolStripItem)toolStripSeparator20,
				(ToolStripItem)listEventsInExportFileToolStripMenuItem,
				(ToolStripItem)importSolutionsToolStripMenuItem
			});
			((ToolStripItem)ExportImport).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)ExportImport).set_Name("ExportImport");
			((ToolStripItem)ExportImport).set_Size(new Size(124, 24));
			((ToolStripItem)ExportImport).set_Text("Export/Import...     ");
			((ToolStripItem)helpOnExportImportToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpOnExportImportToolStripMenuItem).set_Name("helpOnExportImportToolStripMenuItem");
			((ToolStripItem)helpOnExportImportToolStripMenuItem).set_Size(new Size(373, 22));
			((ToolStripItem)helpOnExportImportToolStripMenuItem).set_Text("Help on Export/Import");
			((ToolStripItem)helpOnExportImportToolStripMenuItem).add_Click((EventHandler)helpOnExportImportToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator8).set_Name("toolStripSeparator8");
			((ToolStripItem)toolStripSeparator8).set_Size(new Size(370, 6));
			((ToolStripItem)createNewExportFileToolStripMenuItem).set_Name("createNewExportFileToolStripMenuItem");
			((ToolStripItem)createNewExportFileToolStripMenuItem).set_Size(new Size(373, 22));
			((ToolStripItem)createNewExportFileToolStripMenuItem).set_Text("1.  Create new Export  file");
			((ToolStripItem)createNewExportFileToolStripMenuItem).add_Click((EventHandler)createNewExportFileToolStripMenuItem_Click);
			((ToolStripItem)selectExportFileToolStripMenuItem).set_Name("selectExportFileToolStripMenuItem");
			((ToolStripItem)selectExportFileToolStripMenuItem).set_Size(new Size(373, 22));
			((ToolStripItem)selectExportFileToolStripMenuItem).set_Text("2.  Set export file");
			((ToolStripItem)selectExportFileToolStripMenuItem).add_Click((EventHandler)selectExportFileToolStripMenuItem_Click);
			((ToolStripItem)toolStripMenuItemExportFile2).set_Enabled(false);
			((ToolStripItem)toolStripMenuItemExportFile2).set_Name("toolStripMenuItemExportFile2");
			((ToolStripItem)toolStripMenuItemExportFile2).set_Size(new Size(373, 22));
			((ToolStripItem)toolStripMenuItemExportFile2).set_Text("()");
			((ToolStripItem)toolStripSeparator17).set_Name("toolStripSeparator17");
			((ToolStripItem)toolStripSeparator17).set_Size(new Size(370, 6));
			((ToolStripItem)openCommentsFormForCurrentEventToolStripMenuItem).set_Name("openCommentsFormForCurrentEventToolStripMenuItem");
			((ToolStripItem)openCommentsFormForCurrentEventToolStripMenuItem).set_Size(new Size(373, 22));
			((ToolStripItem)openCommentsFormForCurrentEventToolStripMenuItem).set_Text("3.  Open Comments form for current event");
			((ToolStripItem)openCommentsFormForCurrentEventToolStripMenuItem).add_Click((EventHandler)openCommentsFormForCurrentEventToolStripMenuItem_Click);
			((ToolStripItem)exportCurrentEventToolStripMenuItem).set_Name("exportCurrentEventToolStripMenuItem");
			exportCurrentEventToolStripMenuItem.set_ShortcutKeys((Keys)196696);
			((ToolStripItem)exportCurrentEventToolStripMenuItem).set_Size(new Size(373, 22));
			((ToolStripItem)exportCurrentEventToolStripMenuItem).set_Text("4.  Export all solutions for current event");
			((ToolStripItem)exportCurrentEventToolStripMenuItem).add_Click((EventHandler)exportCurrentEventToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator18).set_Name("toolStripSeparator18");
			((ToolStripItem)toolStripSeparator18).set_Size(new Size(370, 6));
			((ToolStripItem)toolStripSeparator19).set_Name("toolStripSeparator19");
			((ToolStripItem)toolStripSeparator19).set_Size(new Size(370, 6));
			((ToolStripItem)selectImportFileImportToolStripMenuItem).set_Name("selectImportFileImportToolStripMenuItem");
			((ToolStripItem)selectImportFileImportToolStripMenuItem).set_Size(new Size(373, 22));
			((ToolStripItem)selectImportFileImportToolStripMenuItem).set_Text("5.  Set import file");
			((ToolStripItem)selectImportFileImportToolStripMenuItem).add_Click((EventHandler)selectImportFileImportToolStripMenuItem_Click);
			((ToolStripItem)toolStripMenuItemImportFile).set_Enabled(false);
			((ToolStripItem)toolStripMenuItemImportFile).set_Name("toolStripMenuItemImportFile");
			((ToolStripItem)toolStripMenuItemImportFile).set_Size(new Size(373, 22));
			((ToolStripItem)toolStripMenuItemImportFile).set_Text("()");
			((ToolStripItem)toolStripSeparator20).set_Name("toolStripSeparator20");
			((ToolStripItem)toolStripSeparator20).set_Size(new Size(370, 6));
			((ToolStripItem)listEventsInExportFileToolStripMenuItem).set_Name("listEventsInExportFileToolStripMenuItem");
			listEventsInExportFileToolStripMenuItem.set_ShortcutKeys((Keys)131145);
			((ToolStripItem)listEventsInExportFileToolStripMenuItem).set_Size(new Size(373, 22));
			((ToolStripItem)listEventsInExportFileToolStripMenuItem).set_Text("6.  Select  an event in the &Import file");
			((ToolStripItem)listEventsInExportFileToolStripMenuItem).add_Click((EventHandler)listEventsInExportFileToolStripMenuItem_Click);
			((ToolStripItem)importSolutionsToolStripMenuItem).set_Name("importSolutionsToolStripMenuItem");
			((ToolStripItem)importSolutionsToolStripMenuItem).set_Size(new Size(373, 22));
			((ToolStripItem)importSolutionsToolStripMenuItem).set_Text("7.  Import solution for the selected event");
			((ToolStripItem)importSolutionsToolStripMenuItem).add_Click((EventHandler)importSolutionsToolStripMenuItem_Click);
			((ToolStripDropDownItem)transitionToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[17]
			{
				(ToolStripItem)updateSolutionsToolStripMenuItem,
				(ToolStripItem)convertObsToolStripMenuItem,
				(ToolStripItem)checkCountryStateToolStripMenuItem,
				(ToolStripItem)updateStarPositionsToolStripMenuItem,
				(ToolStripItem)addParallaxMotionToolStripMenuItem,
				(ToolStripItem)updateAsteroidMotionToolStripMenuItem,
				(ToolStripItem)testGaiaCorrnToolStripMenuItem,
				(ToolStripItem)testGaiaDownloadToolStripMenuItem,
				(ToolStripItem)getEventsWithAssumedUncertsToolStripMenuItem,
				(ToolStripItem)addDiameterUncertaintiesToolStripMenuItem,
				(ToolStripItem)updateRUWEFrom2019ToolStripMenuItem,
				(ToolStripItem)updateAsteroidFitUncertaintiesToolStripMenuItem,
				(ToolStripItem)altitudeErrorsFindAndSaveToolStripMenuItem,
				(ToolStripItem)removeAllMPCReferencesToolStripMenuItem,
				(ToolStripItem)iSAMUpdateToUniqueOnlyToolStripMenuItem,
				(ToolStripItem)doubleStarFormatErrorsToolStripMenuItem,
				(ToolStripItem)updateUncertaintiesToolStripMenuItem
			});
			((ToolStripItem)transitionToolStripMenuItem).set_Name("transitionToolStripMenuItem");
			((ToolStripItem)transitionToolStripMenuItem).set_Size(new Size(96, 24));
			((ToolStripItem)transitionToolStripMenuItem).set_Text("2020 transition");
			((ToolStripItem)updateSolutionsToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)updateSolutionsToolStripMenuItem).set_Name("updateSolutionsToolStripMenuItem");
			((ToolStripItem)updateSolutionsToolStripMenuItem).set_Size(new Size(249, 22));
			((ToolStripItem)updateSolutionsToolStripMenuItem).set_Text("Update Solutions");
			((ToolStripItem)updateSolutionsToolStripMenuItem).add_Click((EventHandler)updateSolutionsToolStripMenuItem_Click);
			((ToolStripItem)convertObsToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)convertObsToolStripMenuItem).set_Name("convertObsToolStripMenuItem");
			((ToolStripItem)convertObsToolStripMenuItem).set_Size(new Size(249, 22));
			((ToolStripItem)convertObsToolStripMenuItem).set_Text("Convert Obs");
			((ToolStripItem)convertObsToolStripMenuItem).add_Click((EventHandler)convertObsToolStripMenuItem_Click);
			((ToolStripItem)checkCountryStateToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)checkCountryStateToolStripMenuItem).set_Name("checkCountryStateToolStripMenuItem");
			((ToolStripItem)checkCountryStateToolStripMenuItem).set_Size(new Size(249, 22));
			((ToolStripItem)checkCountryStateToolStripMenuItem).set_Text("Check Country/State");
			((ToolStripItem)checkCountryStateToolStripMenuItem).add_Click((EventHandler)checkCountryStateToolStripMenuItem_Click);
			((ToolStripItem)updateStarPositionsToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)updateStarPositionsToolStripMenuItem).set_Name("updateStarPositionsToolStripMenuItem");
			((ToolStripItem)updateStarPositionsToolStripMenuItem).set_Size(new Size(249, 22));
			((ToolStripItem)updateStarPositionsToolStripMenuItem).set_Text("Update star positions");
			((ToolStripItem)updateStarPositionsToolStripMenuItem).add_Click((EventHandler)updateStarPositionsToolStripMenuItem_Click);
			((ToolStripItem)addParallaxMotionToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)addParallaxMotionToolStripMenuItem).set_Name("addParallaxMotionToolStripMenuItem");
			((ToolStripItem)addParallaxMotionToolStripMenuItem).set_Size(new Size(249, 22));
			((ToolStripItem)addParallaxMotionToolStripMenuItem).set_Text("Add parallax motion");
			((ToolStripItem)addParallaxMotionToolStripMenuItem).add_Click((EventHandler)AddParallaxMotionToolStripMenuItem_Click);
			((ToolStripItem)updateAsteroidMotionToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)updateAsteroidMotionToolStripMenuItem).set_Name("updateAsteroidMotionToolStripMenuItem");
			((ToolStripItem)updateAsteroidMotionToolStripMenuItem).set_Size(new Size(249, 22));
			((ToolStripItem)updateAsteroidMotionToolStripMenuItem).set_Text("Update Asteroid motion");
			((ToolStripItem)updateAsteroidMotionToolStripMenuItem).add_Click((EventHandler)updateAsteroidMotionToolStripMenuItem_Click);
			((ToolStripItem)testGaiaCorrnToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)testGaiaCorrnToolStripMenuItem).set_Name("testGaiaCorrnToolStripMenuItem");
			((ToolStripItem)testGaiaCorrnToolStripMenuItem).set_Size(new Size(249, 22));
			((ToolStripItem)testGaiaCorrnToolStripMenuItem).set_Text("TestGaiaCorrn");
			((ToolStripItem)testGaiaCorrnToolStripMenuItem).add_Click((EventHandler)testGaiaCorrnToolStripMenuItem_Click);
			((ToolStripItem)testGaiaDownloadToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)testGaiaDownloadToolStripMenuItem).set_Name("testGaiaDownloadToolStripMenuItem");
			((ToolStripItem)testGaiaDownloadToolStripMenuItem).set_Size(new Size(249, 22));
			((ToolStripItem)testGaiaDownloadToolStripMenuItem).set_Text("TestGaiaDownload");
			((ToolStripItem)testGaiaDownloadToolStripMenuItem).add_Click((EventHandler)testGaiaDownloadToolStripMenuItem_Click);
			((ToolStripItem)getEventsWithAssumedUncertsToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)getEventsWithAssumedUncertsToolStripMenuItem).set_Name("getEventsWithAssumedUncertsToolStripMenuItem");
			((ToolStripItem)getEventsWithAssumedUncertsToolStripMenuItem).set_Size(new Size(249, 22));
			((ToolStripItem)getEventsWithAssumedUncertsToolStripMenuItem).set_Text("Get Events with assumed uncerts");
			((ToolStripItem)getEventsWithAssumedUncertsToolStripMenuItem).add_Click((EventHandler)getEventsWithAssumedUncertsToolStripMenuItem_Click);
			((ToolStripItem)addDiameterUncertaintiesToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)addDiameterUncertaintiesToolStripMenuItem).set_Name("addDiameterUncertaintiesToolStripMenuItem");
			((ToolStripItem)addDiameterUncertaintiesToolStripMenuItem).set_Size(new Size(249, 22));
			((ToolStripItem)addDiameterUncertaintiesToolStripMenuItem).set_Text("Add diameter uncertainties");
			((ToolStripItem)addDiameterUncertaintiesToolStripMenuItem).add_Click((EventHandler)addDiameterUncertaintiesToolStripMenuItem_Click);
			((ToolStripItem)updateRUWEFrom2019ToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)updateRUWEFrom2019ToolStripMenuItem).set_Name("updateRUWEFrom2019ToolStripMenuItem");
			((ToolStripItem)updateRUWEFrom2019ToolStripMenuItem).set_Size(new Size(249, 22));
			((ToolStripItem)updateRUWEFrom2019ToolStripMenuItem).set_Text("Update RUWE");
			((ToolStripItem)updateRUWEFrom2019ToolStripMenuItem).add_Click((EventHandler)updateRUWEToolStripMenuItem_Click);
			((ToolStripItem)updateAsteroidFitUncertaintiesToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)updateAsteroidFitUncertaintiesToolStripMenuItem).set_Name("updateAsteroidFitUncertaintiesToolStripMenuItem");
			((ToolStripItem)updateAsteroidFitUncertaintiesToolStripMenuItem).set_Size(new Size(249, 22));
			((ToolStripItem)updateAsteroidFitUncertaintiesToolStripMenuItem).set_Text("Update Asteroid Fit uncertainties ");
			((ToolStripItem)updateAsteroidFitUncertaintiesToolStripMenuItem).add_Click((EventHandler)updateAsteroidFitUncertaintiesToolStripMenuItem_Click);
			((ToolStripItem)altitudeErrorsFindAndSaveToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)altitudeErrorsFindAndSaveToolStripMenuItem).set_Name("altitudeErrorsFindAndSaveToolStripMenuItem");
			((ToolStripItem)altitudeErrorsFindAndSaveToolStripMenuItem).set_Size(new Size(249, 22));
			((ToolStripItem)altitudeErrorsFindAndSaveToolStripMenuItem).set_Text("Altitude errors - find and save");
			((ToolStripItem)altitudeErrorsFindAndSaveToolStripMenuItem).add_Click((EventHandler)altitudeErrorsFindAndSaveToolStripMenuItem_Click);
			((ToolStripItem)removeAllMPCReferencesToolStripMenuItem).set_Name("removeAllMPCReferencesToolStripMenuItem");
			((ToolStripItem)removeAllMPCReferencesToolStripMenuItem).set_Size(new Size(249, 22));
			((ToolStripItem)removeAllMPCReferencesToolStripMenuItem).set_Text("Remove all MPC references");
			((ToolStripItem)removeAllMPCReferencesToolStripMenuItem).add_Click((EventHandler)removeAllMPCReferencesToolStripMenuItem_Click);
			((ToolStripItem)iSAMUpdateToUniqueOnlyToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)iSAMUpdateToUniqueOnlyToolStripMenuItem).set_Name("iSAMUpdateToUniqueOnlyToolStripMenuItem");
			((ToolStripItem)iSAMUpdateToUniqueOnlyToolStripMenuItem).set_Size(new Size(249, 22));
			((ToolStripItem)iSAMUpdateToUniqueOnlyToolStripMenuItem).set_Text("ISAM - update to unique only");
			((ToolStripItem)iSAMUpdateToUniqueOnlyToolStripMenuItem).add_Click((EventHandler)iSAMUpdateToUniqueOnlyToolStripMenuItem_Click);
			((ToolStripItem)doubleStarFormatErrorsToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)doubleStarFormatErrorsToolStripMenuItem).set_Name("doubleStarFormatErrorsToolStripMenuItem");
			((ToolStripItem)doubleStarFormatErrorsToolStripMenuItem).set_Size(new Size(249, 22));
			((ToolStripItem)doubleStarFormatErrorsToolStripMenuItem).set_Text("doubleStarFormat errors");
			((ToolStripItem)doubleStarFormatErrorsToolStripMenuItem).add_Click((EventHandler)doubleStarFormatErrorsToolStripMenuItem_Click);
			((ToolStripItem)updateUncertaintiesToolStripMenuItem).set_Name("updateUncertaintiesToolStripMenuItem");
			((ToolStripItem)updateUncertaintiesToolStripMenuItem).set_Size(new Size(249, 22));
			((ToolStripItem)updateUncertaintiesToolStripMenuItem).set_Text("Update Uncertainties");
			((ToolStripItem)updateUncertaintiesToolStripMenuItem).add_Click((EventHandler)updateUncertaintiesToolStripMenuItem_Click);
			((Control)lstObservations).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstObservations).set_FormattingEnabled(true);
			lstObservations.set_HorizontalExtent(1048);
			lstObservations.set_HorizontalScrollbar(true);
			lstObservations.set_ItemHeight(14);
			((Control)lstObservations).set_Location(new Point(12, 418));
			((Control)lstObservations).set_Name("lstObservations");
			((Control)lstObservations).set_Size(new Size(1052, 214));
			((Control)lstObservations).set_TabIndex(9);
			toolTipLightCurve.SetToolTip((Control)(object)lstObservations, "Right-click to create a light curve \r\nfile from a .csv file");
			lstObservations.add_SelectedIndexChanged((EventHandler)lstObservations_SelectedIndexChanged);
			((Control)lstObservations).add_MouseUp(new MouseEventHandler(lstObservations_MouseUp));
			((Control)cmdAdd).set_Anchor((AnchorStyles)1);
			((Control)cmdAdd).set_BackColor(Color.PaleGreen);
			((Control)cmdAdd).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdAdd).set_Location(new Point(10, 14));
			((Control)cmdAdd).set_Name("cmdAdd");
			((Control)cmdAdd).set_Size(new Size(132, 26));
			((Control)cmdAdd).set_TabIndex(0);
			((Control)cmdAdd).set_Text("ADD   as new #");
			((ButtonBase)cmdAdd).set_UseVisualStyleBackColor(false);
			((Control)cmdAdd).add_Click((EventHandler)cmdAdd_Click);
			((Control)cmdReplace).set_Anchor((AnchorStyles)1);
			((Control)cmdReplace).set_BackColor(Color.PowderBlue);
			((Control)cmdReplace).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdReplace).set_Location(new Point(10, 42));
			((Control)cmdReplace).set_Name("cmdReplace");
			((Control)cmdReplace).set_Size(new Size(132, 26));
			((Control)cmdReplace).set_TabIndex(1);
			((Control)cmdReplace).set_Text("REPLACE    record #");
			((ButtonBase)cmdReplace).set_UseVisualStyleBackColor(false);
			((Control)cmdReplace).add_Click((EventHandler)cmdReplace_Click);
			((Control)cmdDelete).set_Anchor((AnchorStyles)1);
			((Control)cmdDelete).set_BackColor(Color.LightSalmon);
			((Control)cmdDelete).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDelete).set_Location(new Point(10, 95));
			((Control)cmdDelete).set_Name("cmdDelete");
			((Control)cmdDelete).set_Size(new Size(132, 26));
			((Control)cmdDelete).set_TabIndex(5);
			((Control)cmdDelete).set_Text("DELETE    record #");
			((ButtonBase)cmdDelete).set_UseVisualStyleBackColor(false);
			((Control)cmdDelete).add_Click((EventHandler)cmdDelete_Click);
			((Control)cmdUp).set_Anchor((AnchorStyles)1);
			((Control)cmdUp).set_BackColor(Color.FromArgb(255, 200, 40));
			((Control)cmdUp).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdUp).set_Location(new Point(49, 70));
			((Control)cmdUp).set_Name("cmdUp");
			((Control)cmdUp).set_Size(new Size(41, 23));
			((Control)cmdUp).set_TabIndex(3);
			((Control)cmdUp).set_Text("up");
			((ButtonBase)cmdUp).set_UseVisualStyleBackColor(false);
			((Control)cmdUp).add_Click((EventHandler)cmdUp_Click);
			((Control)cmdDown).set_Anchor((AnchorStyles)1);
			((Control)cmdDown).set_BackColor(Color.FromArgb(255, 200, 40));
			((Control)cmdDown).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDown).set_Location(new Point(95, 70));
			((Control)cmdDown).set_Name("cmdDown");
			((Control)cmdDown).set_Size(new Size(41, 23));
			((Control)cmdDown).set_TabIndex(4);
			((Control)cmdDown).set_Text("down");
			((ButtonBase)cmdDown).set_UseVisualStyleBackColor(false);
			((Control)cmdDown).add_Click((EventHandler)cmdDown_Click);
			((Control)label38).set_Anchor((AnchorStyles)1);
			((Control)label38).set_AutoSize(true);
			((Control)label38).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label38).set_Location(new Point(11, 68));
			((Control)label38).set_Name("label38");
			((Control)label38).set_Size(new Size(37, 26));
			((Control)label38).set_TabIndex(2);
			((Control)label38).set_Text("Move \r\nrecord");
			((Control)cmdRenumber).set_Anchor((AnchorStyles)1);
			((Control)cmdRenumber).set_BackColor(Color.HotPink);
			((Control)cmdRenumber).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdRenumber).set_Location(new Point(9, 123));
			((Control)cmdRenumber).set_Name("cmdRenumber");
			((Control)cmdRenumber).set_Size(new Size(132, 26));
			((Control)cmdRenumber).set_TabIndex(6);
			((Control)cmdRenumber).set_Text("RENUMBER   list");
			((ButtonBase)cmdRenumber).set_UseVisualStyleBackColor(false);
			((Control)cmdRenumber).add_Click((EventHandler)cmdRenumber_Click);
			((Control)grpManageObservers).set_BackColor(Color.LemonChiffon);
			((Control)grpManageObservers).get_Controls().Add((Control)(object)cmdRenumber);
			((Control)grpManageObservers).get_Controls().Add((Control)(object)cmdDelete);
			((Control)grpManageObservers).get_Controls().Add((Control)(object)cmdReplace);
			((Control)grpManageObservers).get_Controls().Add((Control)(object)cmdDown);
			((Control)grpManageObservers).get_Controls().Add((Control)(object)cmdAdd);
			((Control)grpManageObservers).get_Controls().Add((Control)(object)cmdUp);
			((Control)grpManageObservers).get_Controls().Add((Control)(object)label38);
			((Control)grpManageObservers).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpManageObservers).set_Location(new Point(851, 183));
			((Control)grpManageObservers).set_Name("grpManageObservers");
			((Control)grpManageObservers).set_Size(new Size(152, 153));
			((Control)grpManageObservers).set_TabIndex(7);
			grpManageObservers.set_TabStop(false);
			((Control)grpManageObservers).set_Text("Manage observers");
			((Control)cmdUpdateHistorical).set_Anchor((AnchorStyles)1);
			((Control)cmdUpdateHistorical).set_BackColor(Color.FromArgb(128, 255, 128));
			((ButtonBase)cmdUpdateHistorical).set_FlatStyle((FlatStyle)1);
			((Control)cmdUpdateHistorical).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdUpdateHistorical).set_Location(new Point(23, 16));
			((Control)cmdUpdateHistorical).set_Name("cmdUpdateHistorical");
			((Control)cmdUpdateHistorical).set_Size(new Size(146, 36));
			((Control)cmdUpdateHistorical).set_TabIndex(0);
			((Control)cmdUpdateHistorical).set_Text("Update Historical file\r\nwith revised event");
			((ButtonBase)cmdUpdateHistorical).set_UseVisualStyleBackColor(false);
			((Control)cmdUpdateHistorical).add_Click((EventHandler)cmdUpdateHistorical_Click);
			((Control)lblCurrentSolution).set_Font(new Font("Microsoft Sans Serif", 7.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblCurrentSolution).set_Location(new Point(154, 398));
			((Control)lblCurrentSolution).set_Name("lblCurrentSolution");
			((Control)lblCurrentSolution).set_Size(new Size(709, 19));
			((Control)lblCurrentSolution).set_TabIndex(17);
			((Control)lblCurrentSolution).set_Text("Current Solution : ");
			lblCurrentSolution.set_TextAlign(ContentAlignment.BottomRight);
			((Control)lblMagDrop).set_AutoSize(true);
			((Control)lblMagDrop).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblMagDrop).set_Location(new Point(10, 390));
			((Control)lblMagDrop).set_Name("lblMagDrop");
			((Control)lblMagDrop).set_Size(new Size(84, 13));
			((Control)lblMagDrop).set_TabIndex(18);
			((Control)lblMagDrop).set_Text("Mag. drop : --");
			lblMagDrop.set_TextAlign(ContentAlignment.BottomLeft);
			((Control)lblMPC).set_AutoSize(true);
			((Control)lblMPC).set_BackColor(Color.Transparent);
			((Control)lblMPC).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblMPC).set_Location(new Point(886, 402));
			((Control)lblMPC).set_Name("lblMPC");
			((Control)lblMPC).set_Size(new Size(41, 13));
			((Control)lblMPC).set_TabIndex(19);
			((Control)lblMPC).set_Text("MPC:-");
			lblMPC.set_TextAlign(ContentAlignment.BottomLeft);
			toolTip.SetToolTip((Control)(object)lblMPC, "MPC ID");
			((Control)cmdAddToHistorical).set_Anchor((AnchorStyles)1);
			((Control)cmdAddToHistorical).set_BackColor(Color.FromArgb(128, 255, 128));
			((ButtonBase)cmdAddToHistorical).set_FlatStyle((FlatStyle)1);
			((Control)cmdAddToHistorical).set_Location(new Point(3, 16));
			((Control)cmdAddToHistorical).set_Name("cmdAddToHistorical");
			((Control)cmdAddToHistorical).set_Size(new Size(146, 36));
			((Control)cmdAddToHistorical).set_TabIndex(1);
			((Control)cmdAddToHistorical).set_Text("Add event to Historical file");
			((ButtonBase)cmdAddToHistorical).set_UseVisualStyleBackColor(false);
			((Control)cmdAddToHistorical).add_Click((EventHandler)cmdAddToHistorical_Click);
			((Control)grpManageHistorical).set_Anchor((AnchorStyles)1);
			((Control)grpManageHistorical).set_BackColor(Color.Plum);
			((Control)grpManageHistorical).get_Controls().Add((Control)(object)cmdAddToCompiled);
			((Control)grpManageHistorical).get_Controls().Add((Control)(object)cmdUpdateHistorical);
			((Control)grpManageHistorical).get_Controls().Add((Control)(object)cmdAddToHistorical);
			((Control)grpManageHistorical).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpManageHistorical).set_Location(new Point(851, 338));
			((Control)grpManageHistorical).set_Name("grpManageHistorical");
			((Control)grpManageHistorical).set_Size(new Size(152, 58));
			((Control)grpManageHistorical).set_TabIndex(8);
			grpManageHistorical.set_TabStop(false);
			((Control)grpManageHistorical).set_Text("Manage Historical file");
			((Control)cmdAddToCompiled).set_Anchor((AnchorStyles)1);
			((Control)cmdAddToCompiled).set_BackColor(Color.FromArgb(128, 255, 128));
			((ButtonBase)cmdAddToCompiled).set_FlatStyle((FlatStyle)1);
			((Control)cmdAddToCompiled).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdAddToCompiled).set_Location(new Point(30, 16));
			((Control)cmdAddToCompiled).set_Name("cmdAddToCompiled");
			((Control)cmdAddToCompiled).set_Size(new Size(146, 36));
			((Control)cmdAddToCompiled).set_TabIndex(35);
			((Control)cmdAddToCompiled).set_Text("Add event to Compilation file");
			((ButtonBase)cmdAddToCompiled).set_UseVisualStyleBackColor(false);
			((Control)cmdAddToCompiled).set_Visible(false);
			((Control)cmdAddToCompiled).add_Click((EventHandler)cmdAddToCompiled_Click);
			toolTip.set_AutomaticDelay(100);
			toolTip.set_AutoPopDelay(12000);
			toolTip.set_InitialDelay(100);
			toolTip.set_IsBalloon(true);
			toolTip.set_ReshowDelay(20);
			toolTip.set_ShowAlways(true);
			((Control)txtMPEC).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMPEC).set_Location(new Point(99, 0));
			((Control)txtMPEC).set_Name("txtMPEC");
			((Control)txtMPEC).set_Size(new Size(51, 18));
			((Control)txtMPEC).set_TabIndex(30);
			((Control)txtMPEC).set_Text("2024");
			toolTip.SetToolTip((Control)(object)txtMPEC, "The MPC circular number of\r\nthe summary report of the event\r\n[It may not be up-to-date]");
			((Control)txtMPEC).add_KeyPress(new KeyPressEventHandler(txtMPEC_KeyPress));
			((Control)txtMPEC).add_Leave((EventHandler)txtMPEC_Leave);
			((Control)txtMPC_ID).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMPC_ID).set_Location(new Point(184, 0));
			((Control)txtMPC_ID).set_Name("txtMPC_ID");
			((Control)txtMPC_ID).set_Size(new Size(136, 18));
			((Control)txtMPC_ID).set_TabIndex(32);
			((Control)txtMPC_ID).set_Text("- - - - ");
			toolTip.SetToolTip((Control)(object)txtMPC_ID, "Observation ID\r\nA unique ID of the observation,\r\nused to identify an observation\r\nwhen it is to be updated or removed");
			((Control)txtMPC_ID).add_Leave((EventHandler)txtMPC_ID_Leave);
			((Control)lblLastUpdate).set_AutoSize(true);
			((Control)lblLastUpdate).set_Font(new Font("Microsoft Sans Serif", 6.8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblLastUpdate).set_Location(new Point(630, 389));
			((Control)lblLastUpdate).set_Name("lblLastUpdate");
			((Control)lblLastUpdate).set_Size(new Size(208, 13));
			((Control)lblLastUpdate).set_TabIndex(20);
			((Control)lblLastUpdate).set_Text("Added : 1900 Jan     Updated : 1900 Jan 1");
			chkAutoOpenSave.set_AutoCheck(false);
			((Control)chkAutoOpenSave).set_AutoSize(true);
			chkAutoOpenSave.set_CheckAlign(ContentAlignment.TopCenter);
			((Control)chkAutoOpenSave).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkAutoOpenSave).set_Location(new Point(1032, 228));
			((Control)chkAutoOpenSave).set_Name("chkAutoOpenSave");
			((Control)chkAutoOpenSave).set_Size(new Size(19, 135));
			((Control)chkAutoOpenSave).set_TabIndex(21);
			((Control)chkAutoOpenSave).set_Text("O\r\np\r\ne\r\nn\r\n-\r\ns\r\na\r\nv\r\ne");
			((ButtonBase)chkAutoOpenSave).set_TextAlign(ContentAlignment.BottomCenter);
			((ButtonBase)chkAutoOpenSave).set_UseVisualStyleBackColor(true);
			((Control)chkAutoOpenSave).add_MouseClick(new MouseEventHandler(chkAutoOpenSave_MouseClick));
			((Control)cmdPlusOne).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdPlusOne).set_Location(new Point(1023, 182));
			((Control)cmdPlusOne).set_Name("cmdPlusOne");
			((Control)cmdPlusOne).set_Size(new Size(32, 30));
			((Control)cmdPlusOne).set_TabIndex(22);
			((Control)cmdPlusOne).set_Text("+1");
			((ButtonBase)cmdPlusOne).set_UseVisualStyleBackColor(true);
			((Control)cmdPlusOne).add_Click((EventHandler)cmdPlusOne_Click);
			((Control)lblLastSaveTime).set_AutoSize(true);
			((Control)lblLastSaveTime).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblLastSaveTime).set_Location(new Point(1022, 378));
			((Control)lblLastSaveTime).set_Name("lblLastSaveTime");
			((Control)lblLastSaveTime).set_Size(new Size(34, 12));
			((Control)lblLastSaveTime).set_TabIndex(23);
			((Control)lblLastSaveTime).set_Text("label39");
			lblLastSaveTime.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)cmdCloseExpandedFreeText).set_BackColor(Color.SandyBrown);
			((Control)cmdCloseExpandedFreeText).set_Font(new Font("Cascadia Code", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdCloseExpandedFreeText).set_Location(new Point(606, 387));
			((Control)cmdCloseExpandedFreeText).set_Name("cmdCloseExpandedFreeText");
			((Control)cmdCloseExpandedFreeText).set_Size(new Size(231, 24));
			((Control)cmdCloseExpandedFreeText).set_TabIndex(24);
			((Control)cmdCloseExpandedFreeText).set_Text("Close expanded free text");
			((ButtonBase)cmdCloseExpandedFreeText).set_UseVisualStyleBackColor(false);
			((Control)cmdCloseExpandedFreeText).set_Visible(false);
			((Control)cmdCloseExpandedFreeText).add_Click((EventHandler)cmdCloseExpandedFreeText_Click);
			((Control)lblRecordNumber).set_AutoSize(true);
			((Control)lblRecordNumber).set_Font(new Font("Cascadia Code", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblRecordNumber).set_Location(new Point(961, 7));
			((Control)lblRecordNumber).set_Name("lblRecordNumber");
			((Control)lblRecordNumber).set_Size(new Size(37, 13));
			((Control)lblRecordNumber).set_TabIndex(25);
			((Control)lblRecordNumber).set_Text(".....");
			((Control)cmdDeleteEvent).set_BackColor(Color.PowderBlue);
			((Control)cmdDeleteEvent).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdDeleteEvent).set_ForeColor(Color.DarkGreen);
			((Control)cmdDeleteEvent).set_Location(new Point(13, 123));
			((Control)cmdDeleteEvent).set_Name("cmdDeleteEvent");
			((Control)cmdDeleteEvent).set_Size(new Size(126, 79));
			((Control)cmdDeleteEvent).set_TabIndex(0);
			((Control)cmdDeleteEvent).set_Text("Remove the displayed\r\nevent from the\r\nHistorical file");
			((ButtonBase)cmdDeleteEvent).set_UseVisualStyleBackColor(false);
			((Control)cmdDeleteEvent).add_Click((EventHandler)cmdDeleteEvent_Click);
			((Control)grpDeletion).set_BackColor(Color.Lavender);
			((Control)grpDeletion).get_Controls().Add((Control)(object)label55);
			((Control)grpDeletion).get_Controls().Add((Control)(object)lblDeleteName);
			((Control)grpDeletion).get_Controls().Add((Control)(object)lblRecNoDel);
			((Control)grpDeletion).get_Controls().Add((Control)(object)label15);
			((Control)grpDeletion).get_Controls().Add((Control)(object)cmdDeleteEvent);
			((Control)grpDeletion).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpDeletion).set_ForeColor(Color.Red);
			((Control)grpDeletion).set_Location(new Point(422, 398));
			((Control)grpDeletion).set_Name("grpDeletion");
			((Control)grpDeletion).set_Size(new Size(152, 213));
			((Control)grpDeletion).set_TabIndex(26);
			grpDeletion.set_TabStop(false);
			((Control)grpDeletion).set_Text("Deletion");
			((Control)label55).set_AutoSize(true);
			((Control)label55).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label55).set_ForeColor(SystemColors.ControlText);
			((Control)label55).set_Location(new Point(12, 53));
			((Control)label55).set_Name("label55");
			((Control)label55).set_Size(new Size(99, 13));
			((Control)label55).set_TabIndex(4);
			((Control)label55).set_Text("Delete file name");
			((Control)lblDeleteName).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblDeleteName).set_ForeColor(SystemColors.ControlText);
			((Control)lblDeleteName).set_Location(new Point(12, 66));
			((Control)lblDeleteName).set_Name("lblDeleteName");
			((Control)lblDeleteName).set_Size(new Size(128, 50));
			((Control)lblDeleteName).set_TabIndex(3);
			((Control)lblDeleteName).set_Text(".....");
			((Control)lblRecNoDel).set_AutoSize(true);
			((Control)lblRecNoDel).set_Font(new Font("Cascadia Code", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblRecNoDel).set_ForeColor(SystemColors.ControlText);
			((Control)lblRecNoDel).set_Location(new Point(85, 27));
			((Control)lblRecNoDel).set_Name("lblRecNoDel");
			((Control)lblRecNoDel).set_Size(new Size(37, 15));
			((Control)lblRecNoDel).set_TabIndex(2);
			((Control)lblRecNoDel).set_Text(".....");
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label15).set_ForeColor(SystemColors.ControlText);
			((Control)label15).set_Location(new Point(11, 28));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(76, 13));
			((Control)label15).set_TabIndex(1);
			((Control)label15).set_Text("Record No. ");
			((Control)txtMPCyear).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMPCyear).set_Location(new Point(3, 0));
			((Control)txtMPCyear).set_Name("txtMPCyear");
			((Control)txtMPCyear).set_Size(new Size(27, 18));
			((Control)txtMPCyear).set_TabIndex(27);
			((Control)txtMPCyear).set_Text("2024");
			((Control)txtMPCyear).add_KeyPress(new KeyPressEventHandler(txtMPCyear_KeyPress));
			((Control)txtMPCyear).add_Leave((EventHandler)txtMPCyear_Leave);
			((Control)txtMPCmonth).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMPCmonth).set_Location(new Point(32, 0));
			((Control)txtMPCmonth).set_Name("txtMPCmonth");
			((Control)txtMPCmonth).set_Size(new Size(17, 18));
			((Control)txtMPCmonth).set_TabIndex(28);
			((Control)txtMPCmonth).set_Text("12");
			((Control)txtMPCmonth).add_KeyPress(new KeyPressEventHandler(txtMPCmonth_KeyPress));
			((Control)txtMPCmonth).add_Leave((EventHandler)txtMPCmonth_Leave);
			((Control)txtMPCday).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMPCday).set_Location(new Point(52, 0));
			((Control)txtMPCday).set_Name("txtMPCday");
			((Control)txtMPCday).set_Size(new Size(17, 18));
			((Control)txtMPCday).set_TabIndex(29);
			((Control)txtMPCday).set_Text("22");
			((Control)txtMPCday).add_KeyPress(new KeyPressEventHandler(txtMPCday_KeyPress));
			((Control)txtMPCday).add_Leave((EventHandler)txtMPCday_Leave);
			((Control)lblMPEC).set_AutoSize(true);
			((Control)lblMPEC).set_Font(new Font("Microsoft Sans Serif", 6f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblMPEC).set_Location(new Point(71, 5));
			((Control)lblMPEC).set_Name("lblMPEC");
			((Control)lblMPEC).set_Size(new Size(28, 9));
			((Control)lblMPEC).set_TabIndex(31);
			((Control)lblMPEC).set_Text("MPEC");
			((Control)pnlMPC).set_BackColor(Color.Yellow);
			pnlMPC.set_BorderStyle((BorderStyle)2);
			((Control)pnlMPC).get_Controls().Add((Control)(object)txtMPC_ID);
			((Control)pnlMPC).get_Controls().Add((Control)(object)txtMPCyear);
			((Control)pnlMPC).get_Controls().Add((Control)(object)lblMPEC);
			((Control)pnlMPC).get_Controls().Add((Control)(object)txtMPCmonth);
			((Control)pnlMPC).get_Controls().Add((Control)(object)txtMPEC);
			((Control)pnlMPC).get_Controls().Add((Control)(object)txtMPCday);
			((Control)pnlMPC).get_Controls().Add((Control)(object)label56);
			((Control)pnlMPC).set_Location(new Point(737, 394));
			((Control)pnlMPC).set_Name("pnlMPC");
			((Control)pnlMPC).set_Size(new Size(327, 22));
			((Control)pnlMPC).set_TabIndex(32);
			((Control)label56).set_AutoSize(true);
			((Control)label56).set_Font(new Font("Microsoft Sans Serif", 6f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label56).set_Location(new Point(152, 5));
			((Control)label56).set_Name("label56");
			((Control)label56).set_Size(new Size(33, 9));
			((Control)label56).set_TabIndex(33);
			((Control)label56).set_Text("MPC ID");
			((Control)lblRUWE).set_AutoSize(true);
			((Control)lblRUWE).set_Font(new Font("Microsoft Sans Serif", 7.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblRUWE).set_Location(new Point(10, 404));
			((Control)lblRUWE).set_Name("lblRUWE");
			((Control)lblRUWE).set_Size(new Size(68, 13));
			((Control)lblRUWE).set_TabIndex(33);
			((Control)lblRUWE).set_Text("RUWE = ...");
			toolTipLightCurve.set_Active(false);
			toolTipLightCurve.set_AutomaticDelay(150);
			((Control)cmdCreateLightCurve).set_AutoSize(true);
			((Control)cmdCreateLightCurve).set_BackColor(SystemColors.Window);
			((Control)cmdCreateLightCurve).set_ForeColor(Color.DimGray);
			((Control)cmdCreateLightCurve).set_Location(new Point(369, 622));
			((Control)cmdCreateLightCurve).set_Name("cmdCreateLightCurve");
			((Control)cmdCreateLightCurve).set_Size(new Size(338, 13));
			((Control)cmdCreateLightCurve).set_TabIndex(34);
			((Control)cmdCreateLightCurve).set_Text("Right-click on an observation to create a light curve file from a .csv file");
			((ToolStripItem)eventDateStarAsteroidOccelmntPredictionXmlToolStripMenuItem).set_BackColor(Color.LavenderBlush);
			((ToolStripItem)eventDateStarAsteroidOccelmntPredictionXmlToolStripMenuItem).set_ForeColor(Color.DarkBlue);
			((ToolStripItem)eventDateStarAsteroidOccelmntPredictionXmlToolStripMenuItem).set_Name("eventDateStarAsteroidOccelmntPredictionXmlToolStripMenuItem");
			((ToolStripItem)eventDateStarAsteroidOccelmntPredictionXmlToolStripMenuItem).set_Size(new Size(504, 22));
			((ToolStripItem)eventDateStarAsteroidOccelmntPredictionXmlToolStripMenuItem).set_Text("set Event Date+Star+Asteroid - using Occelmnt prediction ");
			((ToolStripItem)eventDateStarAsteroidOccelmntPredictionXmlToolStripMenuItem).add_Click((EventHandler)eventDateStarAsteroidOccelmntPredictionXmlToolStripMenuItem_Click);
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)0);
			((Form)this).set_ClientSize(new Size(1077, 637));
			((Control)this).get_Controls().Add((Control)(object)cmdCreateLightCurve);
			((Control)this).get_Controls().Add((Control)(object)lblMagDrop);
			((Control)this).get_Controls().Add((Control)(object)lblRUWE);
			((Control)this).get_Controls().Add((Control)(object)pnlMPC);
			((Control)this).get_Controls().Add((Control)(object)grpDeletion);
			((Control)this).get_Controls().Add((Control)(object)lblRecordNumber);
			((Control)this).get_Controls().Add((Control)(object)cmdCloseExpandedFreeText);
			((Control)this).get_Controls().Add((Control)(object)lblLastSaveTime);
			((Control)this).get_Controls().Add((Control)(object)cmdPlusOne);
			((Control)this).get_Controls().Add((Control)(object)chkAutoOpenSave);
			((Control)this).get_Controls().Add((Control)(object)lblLastUpdate);
			((Control)this).get_Controls().Add((Control)(object)lstObservations);
			((Control)this).get_Controls().Add((Control)(object)grpManageHistorical);
			((Control)this).get_Controls().Add((Control)(object)lblMPC);
			((Control)this).get_Controls().Add((Control)(object)lblCurrentSolution);
			((Control)this).get_Controls().Add((Control)(object)grpManageObservers);
			((Control)this).get_Controls().Add((Control)(object)grpAsteroid);
			((Control)this).get_Controls().Add((Control)(object)grpObserver);
			((Control)this).get_Controls().Add((Control)(object)grpHistory);
			((Control)this).get_Controls().Add((Control)(object)grpTimes);
			((Control)this).get_Controls().Add((Control)(object)grpConditions);
			((Control)this).get_Controls().Add((Control)(object)grpStar);
			((Control)this).get_Controls().Add((Control)(object)grpDate);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationAsterObsEditor", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationAsterObsEditor);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MinimumSize(new Size(1040, 480));
			((Control)this).set_Name("ObservationsEditor");
			((Form)this).set_StartPosition((FormStartPosition)1);
			((Control)this).set_Text("Asteroid observations editor :");
			((Form)this).add_Activated((EventHandler)ObservationsEditor_Activated);
			((Form)this).add_FormClosing(new FormClosingEventHandler(ObservationsEditor_FormClosing));
			((Form)this).add_Load((EventHandler)ObservationsEditor_Load);
			((Control)this).add_Resize((EventHandler)ObservationsEditor_Resize);
			((Control)grpDate).ResumeLayout(false);
			((Control)grpDate).PerformLayout();
			((Control)grpStar).ResumeLayout(false);
			((Control)grpStar).PerformLayout();
			((Control)panelGaiaCoords).ResumeLayout(false);
			((Control)panelGaiaCoords).PerformLayout();
			((Control)panelUCAC4).ResumeLayout(false);
			((Control)panelUCAC4).PerformLayout();
			((Control)panelNOMAD).ResumeLayout(false);
			((Control)panelNOMAD).PerformLayout();
			((Control)panelTycho2).ResumeLayout(false);
			((Control)panelTycho2).PerformLayout();
			((Control)panelHip).ResumeLayout(false);
			((Control)panelHip).PerformLayout();
			((Control)panelB1).ResumeLayout(false);
			((Control)panelB1).PerformLayout();
			((Control)grpAsteroid).ResumeLayout(false);
			((Control)grpAsteroid).PerformLayout();
			((Control)pnlComet).ResumeLayout(false);
			((Control)pnlComet).PerformLayout();
			((Control)pnlBinary).ResumeLayout(false);
			((Control)pnlBinary).PerformLayout();
			((Control)pnlDAMIT_ISAM).ResumeLayout(false);
			((Control)pnlDAMIT_ISAM).PerformLayout();
			((Control)panelPlanets).ResumeLayout(false);
			((Control)panelPlanets).PerformLayout();
			((Control)grpObserver).ResumeLayout(false);
			((Control)grpObserver).PerformLayout();
			((Control)panelDDD).ResumeLayout(false);
			((Control)panelDDD).PerformLayout();
			((Control)panelDMM).ResumeLayout(false);
			((Control)panelDMM).PerformLayout();
			((Control)panelDMS).ResumeLayout(false);
			((Control)panelDMS).PerformLayout();
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)grpHistory).ResumeLayout(false);
			((Control)grpHistory).PerformLayout();
			((Control)panel3).ResumeLayout(false);
			((Control)panel3).PerformLayout();
			((Control)grpTimes).ResumeLayout(false);
			((Control)grpTimes).PerformLayout();
			((Control)panel4).ResumeLayout(false);
			((Control)panel4).PerformLayout();
			((Control)grpConditions).ResumeLayout(false);
			((Control)grpConditions).PerformLayout();
			((ISupportInitialize)updnSNR).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)grpManageObservers).ResumeLayout(false);
			((Control)grpManageObservers).PerformLayout();
			((Control)grpManageHistorical).ResumeLayout(false);
			((Control)grpDeletion).ResumeLayout(false);
			((Control)grpDeletion).PerformLayout();
			((Control)pnlMPC).ResumeLayout(false);
			((Control)pnlMPC).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
