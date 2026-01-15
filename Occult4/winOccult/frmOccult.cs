using System;
using System.Collections;
using System.ComponentModel;
using System.Configuration;
using System.Diagnostics;
using System.Drawing;
using System.Globalization;
using System.IO;
using System.IO.Compression;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Windows.Forms;
using AOTA;
using GaiaDoubles;
using LightCurves;
using Occult;
using Occult.Asteroid_Observations;
using Occult.Asteroids;
using Occult.BailyBeads;
using Occult.Eclipses;
using Occult.Ephemerides;
using Occult.File_Actions;
using Occult.Lunar_Observations;
using Occult.Properties;
using Occult.Star_Catalogues;
using Shapes;

namespace winOccult
{
	public class frmOccult : Form
	{
		private static string AppPath;

		private const double Radian = 180.0 / Math.PI;

		private bool RunUpdater;

		private Asteroids AsteroidEphemeris;

		private Distances Distances;

		private AsteroidDiameters AsterDiameters;

		private AsteroidSatellites AsteroidSatellites;

		internal AsteroidSearch Search;

		private Occult.Calendar Calendar;

		private CheckInstallation CheckInstall;

		private Comets Comet;

		private Conversion Convert;

		private ConvertAstorb_etc Astorb;

		private DatumConversions DatumConversions;

		private Diary Diary;

		private Meridians Meridian;

		private MoonPhases MoonPhase;

		private MoonRiseSet MoonRise;

		private MutualConjunctions MutualConjunction;

		private MutualAsteroidals MutualAsteroids;

		private ObjectAltitude ObjectAlt;

		private Downloads Downloads;

		private PlanetEphemeris Planets;

		private PlanetRiseSet RiseSet;

		private PlanetViewer PlotPlanet;

		private Relativity Relativity;

		private SatellitePlanetEclipsesTransits SatEvents;

		private SatellitePositions SatPos;

		private SiderealTime_etc Sidereal;

		private StarChart Chart;

		private Transits Transit;

		private UserAsteroids_Edit EditAsteroid;

		private UserStar_Edit EditStar;

		private SelectAndDisplay SelectAndDisplay;

		private HelpNavigator navigator = (HelpNavigator)(-2147483642);

		private int InstanceCount;

		private IContainer components;

		private Button cmdLunarEclipse;

		private Button cmdTransits;

		private Button cmddeltaT;

		private Button cmdPlanetPlot;

		private Button cmdMutualEvents;

		private Button cmdPlanetEphemeris;

		private Button cmdCalendar;

		private Button MoonPhases;

		private Button cmdRiseSet;

		private Button cmdConversions;

		private Button cmdMutualConjunctions;

		private Button cmdSatellitePositions;

		private Button cmdDiary;

		private GroupBox grpEphemeris;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private Button cmdMoonRiseSet;

		private Button cmdSatelliteEvents;

		private Button cmdMeridians;

		private Button CmdSidereal;

		private Button cmdConvertAstorb;

		private Button cmdComets;

		private Button cmdAsteroidSearch;

		private Button cmdEditUserStar;

		private Button cmdEditUserMinorPlanetsLunar;

		private Button cmdEditUserMinorPlanets;

		private ToolStrip MainToolStrip;

		private ToolStripButton StripEclipses;

		private ToolStripButton StripLunar;

		private ToolStripButton StripAsteroids;

		private ToolStripButton StripEphem;

		private ToolStripButton StripShared;

		private ToolStripButton StripSatellites;

		private Panel PanelAsteroidPredictions;

		private Panel PanelEphemeris;

		private GroupBox groupBox1;

		private GroupBox groupBox3;

		private GroupBox groupBox2;

		private GroupBox groupBox4;

		private Panel PanelSatellites;

		private Panel PanelMaintenance;

		private ToolStripButton StripMPReduce;

		private Panel PanelAsteroidObservations;

		private Label label9;

		private Label label10;

		private Panel PanelEclipsesTransits;

		private Label label11;

		private Label label12;

		private Button cmdDownloadFuture;

		private Button button1;

		private Button cmdSolarEclipse;

		private Button cmdDefault;

		private Label label19;

		private Label label20;

		private Button cmdDiameters;

		private Panel PanelLunarPredictions;

		private Label label21;

		private Label label22;

		private Label label24;

		private Label label23;

		private Button cmdDisplayMoonInStarField;

		private Button cmdFindOccultations;

		private GroupBox groupBox6;

		private Button cmdDrawStarChart;

		private ToolStripButton StripLunarObservations;

		private Panel PanelLunarObservations;

		private Label label25;

		private Label label26;

		private Label label27;

		private ToolStrip toolStrip1;

		private ToolStripButton TS_AsteroidPredict;

		private ToolStripButton TS_AsteroidReduce;

		private ToolStripButton TS_Eclipses;

		private ToolStripButton TS_Ephemeris;

		private ToolStripButton TS_LunarPredict;

		private ToolStripButton TS_LunarObservations;

		private ToolStripButton TS_Satellites;

		private ToolStripButton TS_Maintenance;

		private ToolStripButton toolStripButton7;

		private Button cmdSingleLunar;

		private Button cmdEditSites;

		private Button cmdXZ80Maintain;

		private Button cmdStarCatDetails;

		private Button ViewStarCatDetails2;

		private Button cmdAutoGrazeOutput;

		private Button cmdEditBinaryAsteroids;

		private GroupBox grpNomad;

		private Button cmbDisplayCats;

		private Label label29;

		private Button cmdListDiameters;

		private Button cmdListDoubleStars;

		private Button cmdListPositions;

		private GroupBox grpAsteroidLists;

		private GroupBox grpMaintainObsFile;

		private Button cmdGenerateAsteroidGraphicFiles;

		private Button cmdGenerateDistantAsteroidList;

		private Button cmdNASA_PDS;

		private Button cmdListPoorStars;

		private Button cmdListDuplicates;

		private Button cmdListAsteroidObserver;

		private Button cmdAsteroidObservations;

		private Button cmdAddEdit;

		private Button cmdViewHistoricalGrazes;

		private Button cmdViewHistoricalOccults;

		private Button cmdBailyBeads;

		private Label label32;

		private Button cmdLaSilla;

		private Label label33;

		private Button cmdMagnitudes;

		private Button cmdCheckInstall;

		private HelpProvider helpProvider1;

		private Button cmdObjectAltitude;

		private Label lblPreserveFuture;

		private Button cmdArchiveEditor;

		private Button cmdDatumConversions;

		private Label lblEphemerisSource;

		private Button cmdCloseAllLunarPredictions;

		private Button button3;

		private Button cmdCloseAllEphemerisForms;

		private GroupBox grpLunarReduceAdmin;

		private Button cmdPlotLunarAgainstLimb;

		private Button cmdDownloads;

		private Label label8;

		private Button cmdCompareStarPositions;

		private Button cmdSolveDouble;

		private Button cmdCloseAllAsteroidObservations;

		private Button cmdViewWDS;

		private ToolStripMenuItem weatherToolStripMenuItem;

		private ToolStripMenuItem dayWeatherForecastForhomeToolStripMenuItem;

		private ToolStripMenuItem regionalCloudMapPredictionsToolStripMenuItem;

		private ToolStripMenuItem checkForUpdatesToolStripMenuItem;

		private ToolStripMenuItem checkForUpdatesToolStripMenuItem1;

		private ToolStripMenuItem listChangesSinceCurrentVersionToolStripMenuItem;

		private ToolStripLabel toolStripUpdates;

		private Button cmdCreateDowloadControl;

		private ToolStripLabel toolStripDataUpdates;

		private ToolStripMenuItem checkForDataUpdatesToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripMenuItem updateWithLatestVersionToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator2;

		private ToolStripMenuItem updateProgramWithLatestBetaVersionifAnyToolStripMenuItem;

		private Button cmdAsteroidEphemeris;

		private GroupBox grpShapeModels;

		private ToolStripMenuItem restartOccultWatcherToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator3;

		private ToolStripMenuItem helpWithUpdatesToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator4;

		private Button cmdListMissingGrazes;

		private Button cmdStarAnalysis;

		private Button cmdProcessRegionalFiles;

		private Button cmdSelectDEEphemeris;

		private Button cmdSelectDE;

		private Button cmdStarDiameterAnalyser;

		private Button cmdAsteroidDiameter;

		private Button cmdAsteroidSatellites;

		private Button cmdValidate;

		private Label label3;

		private Label label2;

		private Button cmdDistances;

		private Button cmdConvertAstorb_MPCOrb;

		private Button cmdCraterTimings;

		private Button cmdAOTA;

		private GroupBox groupBox5;

		private Button cmdShapeModels;

		private TextBox txtStarNumber;

		private ComboBox cmbStarCat;

		private Button cmdListEquivalents;

		private Label label4;

		private Label label5;

		private Button cmdConvertRIO;

		private GroupBox groupBox8;

		private Button cmdEditAsteroidRings;

		private Button cmdCreateDEEphemerisFiles;

		private Button cmdSelectDE_eclipses;

		private CheckBox chkExcludeOldRio;

		private Button cmdK2Events;

		private Button cmdListLunarsOfK2Stars;

		private ToolStripMenuItem virtualDubTimerToolStripMenuItem;

		private Button cmdArchiveLightCurve;

		private Button cmdViewLightCurve;

		private Button cmdViewLightCurveLunar;

		private GroupBox groupBox9;

		private Button cmdProcessLightCurves;

		private Button Process_LightCurves;

		private CheckBox chkUseExistingFiles;

		private Label label6;

		private Button cmdCreateGaia;

		private GroupBox groupBox10;

		private GroupBox groupBox7;

		private ToolStripMenuItem testsToolStripMenuItem;

		private ToolStripMenuItem runCurrentTestRoutineToolStripMenuItem;

		private ToolStripMenuItem runExtrnalAccessTestRoutineToolStripMenuItem;

		private Button cmdLunarEventFromStarAltitude;

		private Button cmdFindOccultedStars_Asteroidal;

		private Button cmdListAndDisplayAsteroidals;

		private Button cmdLuckyStar;

		private GroupBox groupBox11;

		private CheckBox chkExcludeOldLuckyStar;

		private CheckBox chkUseLocal;

		private ToolTip toolTip1;

		private ToolStripMenuItem helpToolStripMenuItem1;

		private ToolStripMenuItem helpNotWorkingToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator5;

		private ToolStripSeparator toolStripSeparator6;

		private Button cmdAsteroidClasses;

		private ToolStripMenuItem cascadeFormsToolStripMenuItem;

		private CheckBox chkHighPriority;

		private Button cmdCheckAsteroidalMidTimes;

		private Button cmdShapeModelFits;

		private Button cmdBinaryAsteroids;

		private Button cmdUnfittedEvents;

		private Button cmdUpdateKeplerLinks;

		private Button cmdRelativity;

		private Button cmdListWrongUncertainties;

		private Label label13;

		private Button cmdNewDAMIT;

		private Label label1;

		private ToolStripMenuItem unzipToolStripMenuItem;

		private Button cmdForReview;

		private ToolStripMenuItem previousVersionsAreInUpdatesToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator9;

		private ToolStripSeparator toolStripSeparator8;

		private ToolStripSeparator toolStripSeparator7;

		private Button cmdRingedAsteroids;

		private Button cmdMergeEditedWithMain;

		private Button cmdReduceArchiveFile;

		private Button cmdVizierArchive;

		private Button cmdListEventsWithNoMainBody;

		private Button cmdMutualAsteroidals;

		private Label label7;

		private Button cmdLightCurveSimulator;

		private GroupBox groupBox12;

		private Button cmdLightCurves;

		private Button cmdGaiaDoubleStars;

		private Button cmdAsteroidTaxonomy;

		private Button cmdDeleteSuperceded;

		private ToolStripMenuItem backUpSourceCodeToolStripMenuItem;

		private Button cmdSearchFreeText;

		public frmOccult()
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			InitializeComponent();
			AppPath = Application.get_StartupPath();
			Utilities.RunningInDevelopmentEnvironment = false;
			if (Utilities.IsInVisualStudio)
			{
				Utilities.RunningInDevelopmentEnvironment = true;
				((ToolStripItem)testsToolStripMenuItem).set_Visible(true);
				if (Directory.Exists("d:\\Occult") & Directory.Exists("d:\\Occult\\Resource Files"))
				{
					AppPath = "d:\\Occult";
				}
				else if (Directory.Exists("D:\\Occult"))
				{
					AppPath = "D:\\Occult";
				}
				else if (Directory.Exists("C:\\Occult"))
				{
					AppPath = "C:\\Occult";
				}
				else if (Directory.Exists("C:\\Program Files\\Occult"))
				{
					AppPath = "C:\\Program Files\\Occult";
				}
				else if (Directory.Exists("C:\\Program Files\\Occult 4"))
				{
					AppPath = "C:\\Program Files\\Occult 4";
				}
			}
			else
			{
				((ToolStripItem)testsToolStripMenuItem).set_Visible(false);
			}
		}

		private void frmOccult_Load(object sender, EventArgs e)
		{
			//IL_00c0: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c6: Invalid comparison between Unknown and I4
			//IL_0103: Unknown result type (might be due to invalid IL or missing references)
			//IL_0109: Invalid comparison between Unknown and I4
			//IL_0280: Unknown result type (might be due to invalid IL or missing references)
			//IL_0286: Invalid comparison between Unknown and I4
			//IL_0302: Unknown result type (might be due to invalid IL or missing references)
			//IL_07c2: Unknown result type (might be due to invalid IL or missing references)
			//IL_0964: Unknown result type (might be due to invalid IL or missing references)
			RunUpdater = false;
			InstanceCount = 0;
			if (!Utilities.RunningInDevelopmentEnvironment)
			{
				using Process process = Process.GetCurrentProcess();
				if (chkHighPriority.get_Checked())
				{
					process.PriorityClass = ProcessPriorityClass.High;
				}
				Process[] processes = Process.GetProcesses();
				int num = 0;
				Process[] array = processes;
				foreach (Process process2 in array)
				{
					try
					{
						if (Path.GetFileName(process2.MainModule!.FileName) == "Occult.exe" && !process2.MainWindowTitle.Contains("[Dev"))
						{
							InstanceCount++;
						}
					}
					catch
					{
					}
					num++;
				}
				if (InstanceCount == 2)
				{
					if ((int)MessageBox.Show("Occult is already running.\r\n\r\nDo you really want to run another instance?", "2nd instance of Occult", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256) == 7)
					{
						Application.Exit();
						return;
					}
				}
				else if (InstanceCount > 2 && (int)MessageBox.Show(InstanceCount - 1 + " instances of Occult are already running.\r\n\r\nDo you really want to run yet another instance?", "Many instances of Occult", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256) == 7)
				{
					Application.Exit();
					return;
				}
			}
			Utilities.Set_Path_for_DLL_Data(AppPath);
			Utilities.Set_JPLDE_Availability();
			DisplayMPOccultations.HasBeenCalledFromOccult = true;
			if (Occult.Properties.Settings.Default.UpgradeRequired)
			{
				((ApplicationSettingsBase)Occult.Properties.Settings.Default).Upgrade();
				Utilities.UpgradeSettings();
				Occult.Properties.Settings.Default.UpgradeRequired = false;
			}
			Utilities.RefreshOccultServer();
			if (!File.Exists(Utilities.AppPath + "\\Resource Files\\Gaia\\Gaia_Binaries.csv"))
			{
				http.GetGaiaBinariesFile();
			}
			Utilities.UpdateSettingsForDLL();
			CreateOccultSubDirectories();
			if (!File.Exists(AppPath + "\\Resource Files\\Earth.bin"))
			{
				string[] array2 = new string[2] { "InstallResources.zip", "InstallSites.zip" };
				for (int j = 0; j < array2.Length; j++)
				{
					if (File.Exists(AppPath + "\\" + array2[j]) & !File.Exists(AppPath + "\\Downloaded Files\\" + array2[j]))
					{
						File.Move(AppPath + "\\" + array2[j], AppPath + "\\Downloaded Files\\" + array2[j]);
					}
				}
				if ((!File.Exists(AppPath + "\\Downloaded Files\\" + array2[0]) | !File.Exists(AppPath + "\\Downloaded Files\\" + array2[1])) && Utilities.InternetIsAvailable() && (int)MessageBox.Show("Occult needs to retrieve two files to complete the installation. The download is about 38MB.\r\n\r\nDo you want to download the files now?", "Download installation files", (MessageBoxButtons)4, (MessageBoxIcon)64, (MessageBoxDefaultButton)0) == 6)
				{
					http.GetInstallationDataFiles();
				}
				if (!File.Exists(AppPath + "\\Downloaded Files\\" + array2[0]) | !File.Exists(AppPath + "\\Downloaded Files\\" + array2[1]))
				{
					MessageBox.Show("Execution of the program cannot continue. The files required to\r\ncomplete the basic installation are not present, and cannot be downloaded\r\n\r\nYou must either:\r\n1. Connect to the internet, so that the required files\r\n   can be automatically downloaded; or\r\n2.  Manually download (or otherwise obtain) the files \r\n      (i) 'InstallResources.zip' and \r\n     (ii) 'InstallSites.zip'\r\n  and place them ( do not UnZip them! ) in either the\r\n     " + AppPath + ",  or\r\n     " + AppPath + "\\Downloaded Files\r\n  directory.", "Inadequate installation", (MessageBoxButtons)0, (MessageBoxIcon)48);
					((Form)this).Close();
					return;
				}
				using (ZipArchive source = ZipFile.Open(AppPath + "\\Downloaded Files\\" + array2[0], ZipArchiveMode.Read))
				{
					source.ExtractToDirectory(AppPath + "\\Resource Files");
				}
				using ZipArchive source2 = ZipFile.Open(AppPath + "\\Downloaded Files\\" + array2[1], ZipArchiveMode.Read);
				source2.ExtractToDirectory(AppPath + "\\Sites");
			}
			string[] array3 = new string[2] { "OccultUpdate.zip", "OccultUpdateBeta.zip" };
			string text = Utilities.AppPath + "\\DownLoaded Files\\";
			string text2 = Utilities.AppPath + "\\Updates\\";
			for (int k = 0; k < 2; k++)
			{
				if (File.Exists(text + array3[k]))
				{
					string text3 = array3[k].Replace(".zip", " v" + Utilities.OccultVersion.Replace(".", "_") + ".zip");
					int num2 = 0;
					while (File.Exists(text2 + text3))
					{
						num2++;
						text3 = array3[k].Replace(".zip", " v" + Utilities.OccultVersion.Replace(".", "_") + " [" + num2 + "].zip");
					}
					File.Move(text + array3[k], text2 + text3);
				}
			}
			string[] files = Directory.GetFiles(Utilities.AppPath + "\\Updates\\", "OccultUpdate*.zip", SearchOption.TopDirectoryOnly);
			((ToolStripItem)previousVersionsAreInUpdatesToolStripMenuItem).set_Text(files.Length + " previous versions are in the subdirectory 'Updates'");
			Utilities.UpdateDefaultDownloadAddresses();
			Utilities.SetLunarLimbAvailability();
			Utilities.PurgeLunarEphemerisCache_UpdatedEphemeris();
			if (!File.Exists(Utilities.AppPath + "\\Resource Files\\Gaia\\UBSC.txt") && Utilities.InternetIsAvailable())
			{
				http.GetUBSCcatalogue();
			}
			if (!File.Exists(AppPath + "\\Resource Files\\AsteroidDias_Indiv.bin") && Utilities.InternetIsAvailable())
			{
				http.Download_AsteroidDias(SupressMessages: false);
			}
			if (!File.Exists(Utilities.AppPath + "\\Resource Files\\Gaia\\Gaia_ED3_BadHipStars.csv") && Utilities.InternetIsAvailable())
			{
				http.DownloadBadHipStarsFile();
			}
			if (Occult.Properties.Settings.Default.OWaddress.ToLower().Contains("occultwatcher.exe") && File.Exists(Occult.Properties.Settings.Default.OWaddress))
			{
				((ToolStripItem)restartOccultWatcherToolStripMenuItem).set_Enabled(true);
			}
			if (Occult.Properties.Settings.Default.OWKilledAtLastRun)
			{
				StartOW(AtStartUp: true);
			}
			Occult.Properties.Settings.Default.OWKilledAtLastRun = false;
			Panel panelLunarPredictions = PanelLunarPredictions;
			Panel panelAsteroidPredictions = PanelAsteroidPredictions;
			Panel panelEclipsesTransits = PanelEclipsesTransits;
			int num3;
			((Control)PanelEphemeris).set_Left(num3 = 160);
			int num4;
			((Control)panelEclipsesTransits).set_Left(num4 = num3);
			int i;
			((Control)panelAsteroidPredictions).set_Left(i = num4);
			((Control)panelLunarPredictions).set_Left(i);
			Panel panelSatellites = PanelSatellites;
			Panel panelMaintenance = PanelMaintenance;
			Panel panelAsteroidObservations = PanelAsteroidObservations;
			((Control)PanelLunarObservations).set_Left(num3 = 160);
			((Control)panelAsteroidObservations).set_Left(num4 = num3);
			((Control)panelMaintenance).set_Left(i = num4);
			((Control)panelSatellites).set_Left(i);
			Panel panelLunarPredictions2 = PanelLunarPredictions;
			Panel panelAsteroidPredictions2 = PanelAsteroidPredictions;
			Panel panelEclipsesTransits2 = PanelEclipsesTransits;
			((Control)PanelEphemeris).set_Top(num3 = 28);
			((Control)panelEclipsesTransits2).set_Top(num4 = num3);
			((Control)panelAsteroidPredictions2).set_Top(i = num4);
			((Control)panelLunarPredictions2).set_Top(i);
			Panel panelSatellites2 = PanelSatellites;
			Panel panelMaintenance2 = PanelMaintenance;
			Panel panelAsteroidObservations2 = PanelAsteroidObservations;
			((Control)PanelLunarObservations).set_Top(num3 = 28);
			((Control)panelAsteroidObservations2).set_Top(num4 = num3);
			((Control)panelMaintenance2).set_Top(i = num4);
			((Control)panelSatellites2).set_Top(i);
			((ListControl)cmbStarCat).set_SelectedIndex(9);
			ShowGroups(Occult.Properties.Settings.Default.InitialDisplayGroup);
			SetLabelPreserve();
			Utilities.AdviseDE_EphemStatus(Fatal: false);
			((Control)cmdSelectDEEphemeris).set_Enabled(File.Exists(AppPath + "\\Resource Files\\DE405_413.bin") | File.Exists(AppPath + "\\Resource Files\\DE_Ephemeris.bin"));
			((Control)cmdViewHistoricalGrazes).set_Enabled(File.Exists(AppPath + "\\Resource Files\\Archive Graze Index.dat"));
			((Control)cmdViewHistoricalOccults).set_Enabled(File.Exists(AppPath + "\\Resource Files\\Archive Observations 1600_1949.dat"));
			if (Thread.CurrentThread.CurrentCulture.NumberFormat.NumberDecimalSeparator != ".")
			{
				Thread.CurrentThread.CurrentCulture = new CultureInfo("en-US");
			}
			((Control)lblEphemerisSource).set_Text(Utilities.EphemerisBasis());
			if (Occult.Properties.Settings.Default.NewInstallation)
			{
				Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Occult3 transition");
				Defaults defaults = new Defaults();
				((Control)defaults).set_Text("Default settings : NEW INSTALLATION - complete SITE DETAILS before continuing");
				((Form)defaults).ShowDialog();
				Occult.Properties.Settings.Default.NewInstallation = false;
			}
			string text4 = "";
			if (Utilities.RunningInDevelopmentEnvironment)
			{
				text4 = " [Development]";
			}
			else if (InstanceCount > 1)
			{
				text4 = " [" + InstanceCount + "]";
			}
			((Control)this).set_Text("Occult " + Utilities.OccultVersion + text4 + "        Main menu");
			((ToolStripItem)toolStripUpdates).set_Text("");
			((ToolStripItem)toolStripDataUpdates).set_Visible(false);
			if (Utilities.UpdateIsDue() && Utilities.InternetIsAvailable())
			{
				string Version = "";
				if (UpdatesExist(out Version))
				{
					((ToolStripItem)toolStripUpdates).set_Text("Occult " + Version + " is available");
				}
				else
				{
					((ToolStripItem)toolStripUpdates).set_Text("");
				}
				Utilities.SetLastUpdateDate(DateTime.Now);
				((ToolStripItem)toolStripDataUpdates).set_Visible(UpdatesAvailable.DataUpdatesAvailable());
			}
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			((Control)grpMaintainObsFile).set_Enabled(Utilities.Administrator);
			((Control)cmdCreateDowloadControl).set_Enabled(Utilities.Administrator);
			((Control)cmdValidate).set_Enabled(Utilities.Administrator);
			((Control)grpLunarReduceAdmin).set_Enabled(Utilities.AdministratorGlobal);
			if (Utilities.InternetIsAvailable() && Utilities.DisplayDownloadsAtStartUP)
			{
				if (Utilities.DisplayNewInstallationDownloadMessage)
				{
					MessageBox.Show("PLEASE READ THE FOLLOWING CAREFULLY\r\n\r\nYou are about to be taken to a page for downloading a\r\nnumber of data files.\r\n\r\nTo ensure a number of relatively small data files are\r\nup-to-date, you should select the 'Download All the above'\r\nbutton in the first group. The total download is about 4MB.\r\n\r\nIf you have just updated or installed Occult, there may be\r\nthree important large files that you should download. They \r\nare near the bottom of the page (under 'Static data files'),\r\nand marked with '>>' if a download is required or desirable.\r\nThose files are:\r\n\r\n * Historical Lunar occultations (new)  [28MB download]\r\n * Tycho2 catalogue (revised)  [61MB download]; and\r\n * JPL ephemeris (updated)  [45MB download]\r\n\r\nWhile you do not need to download these files immediately,\r\nyou should download them as soon as it is convenient.\r\n\r\n", "Download advice", (MessageBoxButtons)0, (MessageBoxIcon)64);
				}
				ShowDownloadPage();
			}
			Utilities.UploadLightCurvesIsDue(AutoShow: true);
		}

		private void CreateOccultSubDirectories()
		{
			string[] array = new string[21]
			{
				"\\Asteroid\\Results", "\\Asteroid\\Observations", "\\Asteroids\\AnimatedGIF", "\\AutoGenerated Asteroids\\MJD", "\\AutoGenerated Grazes\\GoogleMap files", "\\AutoGenerated Lunar", "\\DownLoaded Files\\DE_SourceFiles", "\\Generated Files", "\\Import_Export", "\\LightCurves",
				"\\LightCurveReports", "\\Lunar Archive", "\\Observations\\Doubles", "\\Observations\\LightCurves\\Reported", "\\Observations\\Results", "\\Predictions\\AnimatedGIF", "\\Resource Files\\Moon", "\\Resource Files\\Gaia", "\\ShapeModels", "\\Sites",
				"\\Updates"
			};
			for (int i = 0; i < array.Length; i++)
			{
				string path = AppPath + array[i];
				if (!Directory.Exists(path))
				{
					Directory.CreateDirectory(path);
				}
			}
		}

		private void cmdDefault_Click(object sender, EventArgs e)
		{
			//IL_0005: Unknown result type (might be due to invalid IL or missing references)
			((Form)new Defaults()).ShowDialog();
		}

		private void cmddeltaT_Click(object sender, EventArgs e)
		{
			//IL_0005: Unknown result type (might be due to invalid IL or missing references)
			((Form)new EditDeltaT()).ShowDialog();
		}

		private void cmdConvertAstorb_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)Astorb).Show();
			}
			catch
			{
				Astorb = new ConvertAstorb_etc(1);
				((Control)Astorb).Show();
			}
			((Control)Astorb).Focus();
		}

		private void cmdConvertAstorb_MPCOrb_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)Astorb).Show();
			}
			catch
			{
				Astorb = new ConvertAstorb_etc(1);
				((Control)Astorb).Show();
			}
			((Control)Astorb).Focus();
		}

		private void cmdDownloads_Click(object sender, EventArgs e)
		{
			ShowDownloadPage();
		}

		private void cmdCreateDEEphemerisFiles_Click(object sender, EventArgs e)
		{
			//IL_001a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0024: Expected O, but got Unknown
			if (JPL_DE.CreateDEfile == null)
			{
				JPL_DE.Show_CreateJPL_DE(0);
				((Form)JPL_DE.CreateDEfile).add_FormClosing(new FormClosingEventHandler(SetDELabel));
			}
		}

		private void ShowDownloadPage()
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			//IL_0020: Unknown result type (might be due to invalid IL or missing references)
			try
			{
				((Form)Downloads).ShowDialog();
			}
			catch
			{
				Downloads = new Downloads();
				((Form)Downloads).ShowDialog();
			}
		}

		private void cmdDiameters_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)AsterDiameters).Show();
			}
			catch
			{
				AsterDiameters = new AsteroidDiameters();
				((Control)AsterDiameters).Show();
			}
			((Control)AsterDiameters).Focus();
		}

		private void cmdEditSites_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.ShowSiteEditor();
		}

		private void cmdXZ80Maintain_Click(object sender, EventArgs e)
		{
			LunarOccultations.XZManageShowForm(CreateLunarK2Report: false);
		}

		private void cmdStarCatDetails_Click(object sender, EventArgs e)
		{
			LunarOccultations.StarCatalogueShowForm();
		}

		private void ViewStarCatDetails2_Click(object sender, EventArgs e)
		{
			LunarOccultations.StarCatalogueShowForm();
		}

		private void cmdEditBinaryAsteroids_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.ShowBinaryAsteroidEditor();
		}

		private void cmdEditAsteroidRings_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.ShowAsteroidRingsEditor();
		}

		private void cmbDisplayCats_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.Show_DisplayStarsFromCatalogue();
		}

		private void cmdCheckInstall_Click(object sender, EventArgs e)
		{
			//IL_001f: Unknown result type (might be due to invalid IL or missing references)
			try
			{
				((Control)CheckInstall).Show();
			}
			catch
			{
				CheckInstall = new CheckInstallation();
				((Form)CheckInstall).ShowDialog();
			}
		}

		private void cmdLaSilla_Click(object sender, EventArgs e)
		{
			UserCatalogues.Show_CreateUser();
		}

		private void cmdCompareStarPositions_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
			}
			else
			{
				GetStarPosition.ShowCompareCatalogues(fromAsteroidPrediction: false);
			}
		}

		private void cmdViewWDS_Click(object sender, EventArgs e)
		{
			Interferometric_Plus_WDS.ShowInterferometerDisplay();
		}

		private void cmdCreateGaia_Click(object sender, EventArgs e)
		{
			GaiaCatalogues.Show_CreateGaiaCatalogue();
		}

		private void cmdSingleLunar_Click(object sender, EventArgs e)
		{
			LunarOccultations.LunarPredictionsShowForm();
			((Form)LunarOccultations.LunarPrediction).set_WindowState((FormWindowState)0);
		}

		private void cmdFindOccultations_Click(object sender, EventArgs e)
		{
			LunarOccultations.Lunar_FindShowForm();
		}

		private void cmdDisplayMoonInStarField_Click(object sender, EventArgs e)
		{
			LunarOccultations.MoonStarFieldShow();
		}

		private void cmdAutoGrazeOutput_Click(object sender, EventArgs e)
		{
			LunarOccultations.LunarAutoGenerate_Show();
		}

		private void cmdEditUserMinorPlanetsLunar_Click(object sender, EventArgs e)
		{
			//IL_0012: Unknown result type (might be due to invalid IL or missing references)
			EditAsteroid = new UserAsteroids_Edit(LunarOccultations: true);
			((Form)EditAsteroid).ShowDialog();
		}

		private void cmdCloseAllLunarPredictions_Click(object sender, EventArgs e)
		{
			//IL_0012: Unknown result type (might be due to invalid IL or missing references)
			//IL_0018: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Do you want to close all Lunar Occultation Prediction forms?", "Close All", (MessageBoxButtons)1, (MessageBoxIcon)32, (MessageBoxDefaultButton)256) != 2)
			{
				LunarOccultations.CloseAllLunarPredictionForms();
			}
		}

		private void cmdLunarEclipse_Click(object sender, EventArgs e)
		{
			SolarEclipses.ShowLunarEclipse();
		}

		private void cmdTransits_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)Transit).Show();
			}
			catch
			{
				Transit = new Transits();
				((Control)Transit).Show();
			}
			((Control)Transit).Focus();
		}

		private void cmdSolarEclipse_Click(object sender, EventArgs e)
		{
			SolarEclipses.ShowWorld();
		}

		private void cmdSelectDE_eclipses_Click(object sender, EventArgs e)
		{
			//IL_001a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0024: Expected O, but got Unknown
			if (JPL_DE.CreateDEfile == null)
			{
				JPL_DE.Show_CreateJPL_DE(3);
				((Form)JPL_DE.CreateDEfile).add_FormClosing(new FormClosingEventHandler(SetDELabel));
			}
		}

		private void cmdBailyBeads_Click(object sender, EventArgs e)
		{
			BailyBeads.Show_All();
		}

		private void cmdCraterTimings_Click(object sender, EventArgs e)
		{
			ReduceCraters.ShowCraterReductions();
		}

		private void cmdPlanetPlot_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)PlotPlanet).Show();
			}
			catch
			{
				PlotPlanet = new PlanetViewer();
				((Control)PlotPlanet).Show();
				PlotPlanet.EnablePlanetSelection(Enable: true);
				PlotPlanet.ShowStarPath = false;
				PlotPlanet.DrawPlanetsAndMoons();
			}
			((Control)PlotPlanet).Focus();
		}

		private void cmdPlanetEphemeris_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)Planets).Show();
			}
			catch
			{
				Planets = new PlanetEphemeris();
				((Control)Planets).Show();
			}
			((Control)Planets).Focus();
		}

		private void cmdCalendar_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)Calendar).Show();
			}
			catch
			{
				Calendar = new Occult.Calendar();
				((Control)Calendar).Show();
			}
			((Control)Calendar).Focus();
		}

		private void MoonPhases_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)MoonPhase).Show();
			}
			catch
			{
				MoonPhase = new MoonPhases();
				((Control)MoonPhase).Show();
			}
			((Control)MoonPhase).Focus();
		}

		private void cmdRiseSet_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)RiseSet).Show();
			}
			catch
			{
				RiseSet = new PlanetRiseSet();
				((Control)RiseSet).Show();
			}
			((Control)RiseSet).Focus();
		}

		private void cmdConversions_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)Convert).Show();
			}
			catch
			{
				Convert = new Conversion();
				((Control)Convert).Show();
			}
			((Control)Convert).Focus();
		}

		private void cmdMutualConjunctions_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)MutualConjunction).Show();
			}
			catch
			{
				MutualConjunction = new MutualConjunctions();
				((Control)MutualConjunction).Show();
			}
			((Control)MutualConjunction).Focus();
		}

		private void cmdSatellitePositions_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)SatPos).Show();
			}
			catch
			{
				SatPos = new SatellitePositions();
				((Control)SatPos).Show();
			}
			((Control)SatPos).Focus();
		}

		private void cmdDiary_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)Diary).Show();
			}
			catch
			{
				Diary = new Diary();
				((Control)Diary).Show();
			}
			((Control)Diary).Focus();
		}

		private void cmdMoonRiseSet_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)MoonRise).Show();
			}
			catch
			{
				MoonRise = new MoonRiseSet();
				((Control)MoonRise).Show();
			}
			((Control)MoonRise).Focus();
		}

		private void cmdObjectAltitude_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)ObjectAlt).Show();
			}
			catch
			{
				ObjectAlt = new ObjectAltitude();
				((Control)ObjectAlt).Show();
			}
		}

		private void cmdMeridians_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)Meridian).Show();
			}
			catch
			{
				Meridian = new Meridians();
				((Control)Meridian).Show();
			}
			((Control)Meridian).Focus();
		}

		private void CmdSidereal_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)Sidereal).Show();
			}
			catch
			{
				Sidereal = new SiderealTime_etc();
				((Control)Sidereal).Show();
			}
			((Control)Sidereal).Focus();
		}

		private void cmdComets_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)Comet).Show();
			}
			catch
			{
				Comet = new Comets();
				((Control)Comet).Show();
			}
			((Control)Comet).Focus();
		}

		private void cmdAsteroidEphemeris_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)AsteroidEphemeris).Show();
			}
			catch
			{
				AsteroidEphemeris = new Asteroids();
				((Control)AsteroidEphemeris).Show();
			}
		}

		private void cmdEditUserStar_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			EditStar = new UserStar_Edit();
			((Form)EditStar).ShowDialog();
		}

		private void cmdLightCurveSimulator_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.Show_PredictionLightCurves(ForAnEvent: false);
		}

		private void cmdDrawStarChart_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)Chart).Show();
			}
			catch
			{
				Chart = new StarChart(SetCoords: true);
				((Control)Chart).Show();
			}
			Chart.ShowObjectPath = false;
			Chart.PlotChart();
			((Control)Chart).Focus();
		}

		private void cmdMagnitudes_Click(object sender, EventArgs e)
		{
			LunarObservations.Show_MagnitudeCalculator();
		}

		private void cmdShapeModels_Click(object sender, EventArgs e)
		{
			Asteroid_Observations_Reports.ShowShapeModels();
		}

		private void cmdLightCurves_Click(object sender, EventArgs e)
		{
			LightData.ShowLightCurveViewer();
			((Control)LightData.LightCurveView).Focus();
		}

		private void CmdRelativity_Click(object sender, EventArgs e)
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
		}

		private void cmdDatumConversions_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)DatumConversions).Show();
			}
			catch
			{
				DatumConversions = new DatumConversions();
				((Control)DatumConversions).Show();
			}
		}

		private void cmdSelectDEEphemeris_Click(object sender, EventArgs e)
		{
			//IL_001a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0024: Expected O, but got Unknown
			if (JPL_DE.CreateDEfile == null)
			{
				JPL_DE.Show_CreateJPL_DE(3);
				((Form)JPL_DE.CreateDEfile).add_FormClosing(new FormClosingEventHandler(SetDELabel));
			}
		}

		private void SetDELabel(object o, EventArgs e)
		{
			((Control)lblEphemerisSource).set_Text(Utilities.EphemerisBasis());
		}

		private void CloseAllEphemerisForms()
		{
			try
			{
				((Form)PlotPlanet).Close();
				((Component)(object)PlotPlanet).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)Planets).Close();
				((Component)(object)Planets).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)Calendar).Close();
				((Component)(object)Calendar).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)MoonPhase).Close();
				((Component)(object)MoonPhase).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)RiseSet).Close();
				((Component)(object)RiseSet).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)Convert).Close();
				((Component)(object)Convert).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)MutualConjunction).Close();
				((Component)(object)MutualConjunction).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)SatPos).Close();
				((Component)(object)SatPos).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)Diary).Close();
				((Component)(object)Diary).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)MoonRise).Close();
				((Component)(object)MoonRise).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)ObjectAlt).Close();
				((Component)(object)ObjectAlt).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)Meridian).Close();
				((Component)(object)Meridian).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)Sidereal).Close();
				((Component)(object)Sidereal).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)Comet).Close();
				((Component)(object)Comet).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)Chart).Close();
				((Component)(object)Chart).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)DatumConversions).Close();
				((Component)(object)DatumConversions).Dispose();
			}
			catch
			{
			}
		}

		private void cmdCloseAllEphemerisForms_Click(object sender, EventArgs e)
		{
			//IL_0012: Unknown result type (might be due to invalid IL or missing references)
			//IL_0018: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Do you want to close all Ephemeris forms?", "Close All", (MessageBoxButtons)1, (MessageBoxIcon)32, (MessageBoxDefaultButton)256) != 2)
			{
				CloseAllEphemerisForms();
			}
		}

		private void cmdMutualEvents_Click(object sender, EventArgs e)
		{
			Satellites.MutualEvents_Show();
		}

		private void cmdSatelliteEvents_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)SatEvents).Show();
			}
			catch
			{
				SatEvents = new SatellitePlanetEclipsesTransits();
				((Control)SatEvents).Show();
			}
			((Control)SatEvents).Focus();
		}

		private void ShowGroups(int Group)
		{
			Occult.Properties.Settings.Default.InitialDisplayGroup = Group;
			((Control)PanelLunarPredictions).set_Visible(Group == 0);
			((Control)PanelAsteroidPredictions).set_Visible(Group == 1);
			((Control)PanelEclipsesTransits).set_Visible(Group == 2);
			((Control)PanelEphemeris).set_Visible(Group == 3);
			((Control)PanelSatellites).set_Visible(Group == 4);
			((Control)PanelMaintenance).set_Visible(Group == 5);
			((Control)PanelAsteroidObservations).set_Visible(Group == 6);
			((Control)PanelLunarObservations).set_Visible(Group == 7);
		}

		private void TS_AsteroidPredict_Click(object sender, EventArgs e)
		{
			ShowGroups(1);
			SetLabelPreserve();
		}

		private void TS_AsteroidReduce_Click(object sender, EventArgs e)
		{
			ShowGroups(6);
		}

		private void TS_Eclipses_Click(object sender, EventArgs e)
		{
			ShowGroups(2);
		}

		private void TS_Ephemeris_Click(object sender, EventArgs e)
		{
			ShowGroups(3);
		}

		private void TS_LunarPredict_Click(object sender, EventArgs e)
		{
			ShowGroups(0);
			Utilities.AdviseDE_EphemStatus(Fatal: true);
		}

		private void TS_LunarObservations_Click(object sender, EventArgs e)
		{
			ShowGroups(7);
			Utilities.AdviseDE_EphemStatus(Fatal: true);
		}

		private void TS_Satellites_Click(object sender, EventArgs e)
		{
			ShowGroups(4);
		}

		private void TS_Maintenance_Click(object sender, EventArgs e)
		{
			ShowGroups(5);
		}

		private void cmdAsteroidSearch_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.ShowSearch();
		}

		private void cmdMutualAsteroidals_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)MutualAsteroids).Show();
			}
			catch
			{
				MutualAsteroids = new MutualAsteroidals();
				((Control)MutualAsteroids).Show();
			}
			((Control)MutualAsteroids).Focus();
		}

		private void cmdDownloadFuture_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
				return;
			}
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.Download_Future(SupressMessages: false);
			((Control)this).set_Cursor(Cursors.get_Default());
		}

		private void cmdDownloadFutureAll_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
				return;
			}
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.Download_Future_ALL(SupressMessages: false);
			((Control)this).set_Cursor(Cursors.get_Default());
		}

		private void cmdConvertRIO_Click(object sender, EventArgs e)
		{
			MinorPlanetConvertRIO.DownloadANDconvertRIO(chkExcludeOldRio.get_Checked(), chkUseExistingFiles.get_Checked(), chkUseLocal.get_Checked());
		}

		private void cmdLuckyStar_Click(object sender, EventArgs e)
		{
			MinorPlanetConvertRIO.DownloadConvert_LuckyStar(chkExcludeOldLuckyStar.get_Checked());
		}

		private void SetLabelPreserve()
		{
			if (Utilities.GetPreserveFuture())
			{
				((Control)lblPreserveFuture).set_Text("User setting:\r\nExisting files will be Renamed");
			}
			else
			{
				((Control)lblPreserveFuture).set_Text("User setting:\r\nExisting files will be Overwritten");
			}
		}

		private void cmdEditUserMinorPlanets_Click(object sender, EventArgs e)
		{
			//IL_0012: Unknown result type (might be due to invalid IL or missing references)
			EditAsteroid = new UserAsteroids_Edit(LunarOccultations: false);
			((Form)EditAsteroid).ShowDialog();
		}

		private void cmdSelectDE_Click(object sender, EventArgs e)
		{
			//IL_001a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0024: Expected O, but got Unknown
			if (JPL_DE.CreateDEfile == null)
			{
				JPL_DE.Show_CreateJPL_DE(3);
				((Form)JPL_DE.CreateDEfile).add_FormClosing(new FormClosingEventHandler(SetDELabel));
			}
		}

		private void frmOccult_FormClosing(object sender, FormClosingEventArgs e)
		{
			Utilities.SaveDefaultSettings();
			((SettingsBase)Occult.Properties.Settings.Default).Save();
		}

		private void frmOccult_FormClosed(object sender, FormClosedEventArgs e)
		{
			if (RunUpdater)
			{
				Process process = new Process();
				process.StartInfo.CreateNoWindow = true;
				process.StartInfo.WindowStyle = ProcessWindowStyle.Hidden;
				process.StartInfo.FileName = Utilities.AppPath + "\\OccultUpdater.exe";
				if (Environment.OSVersion.Version.Major > 5)
				{
					process.StartInfo.Verb = "runas";
				}
				process.Start();
			}
		}

		private void cmdAsteroidObservations_Click(object sender, EventArgs e)
		{
			//IL_002b: Unknown result type (might be due to invalid IL or missing references)
			if (InstanceCount > 1)
			{
				MessageBox.Show("When there are multiple instances of Occult running, the Observations Editor can only be used with the first instance of Occult.\r\n\r\nThis instance is #" + InstanceCount + "\r\n\r\nUse the editor in the first instance, [which does not have a number in brackets}", "Multiple instances of Occult", (MessageBoxButtons)0, (MessageBoxIcon)16);
			}
			else
			{
				Data_and_Plots.ShowEditor();
			}
		}

		private void cmdAOTA_Click(object sender, EventArgs e)
		{
			AOTAData.ShowPlotForm((IWin32Window)(object)this, Modal: false);
		}

		private void cmdStarDiameterAnalyser_Click(object sender, EventArgs e)
		{
			StarDiameterAnalysis.ShowStarDiameterAnalyser();
		}

		private void cmdListDiameters_Click(object sender, EventArgs e)
		{
			Asteroid_Observations_Reports.ListAsteroidDiameters(ForPDS: false);
		}

		private void cmdShapeModelFits_Click(object sender, EventArgs e)
		{
			Asteroid_Observations_Reports.ListShapeModels();
		}

		private void cmdBinaryAsteroids_Click(object sender, EventArgs e)
		{
			Asteroid_Observations_Reports.ListBinaryAsteroids();
		}

		private void cmdListEventsWithNoMainBody_Click(object sender, EventArgs e)
		{
			Asteroid_Observations_Reports.ListEventsWithNoMainBody();
		}

		private void cmdRingedAsteroids_Click(object sender, EventArgs e)
		{
			Asteroid_Observations_Reports.ListRingedAsteroids();
		}

		private void cmdListDoubleStars_Click(object sender, EventArgs e)
		{
			Asteroid_Observations_Reports.ListDoublesFromAsteroids();
		}

		private void cmdListPositions_Click(object sender, EventArgs e)
		{
			Asteroid_Observations_Reports.ListPositionsFromAsteroids();
		}

		private void cmdEmailAsteroidReport_Click(object sender, EventArgs e)
		{
			Data_and_Plots.ShowEmailObservations();
		}

		private void cmdGenerateDistantAsteroidList_Click(object sender, EventArgs e)
		{
			Asteroid_Observations_Reports.ListDistantAsteroidEvents();
		}

		private void cmdK2Events_Click(object sender, EventArgs e)
		{
			Asteroid_Observations_Reports.ListKepler2AsteroidEvents();
		}

		private void cmdFindOccultedStars_Asteroidal_Click(object sender, EventArgs e)
		{
			Asteroid_Observations_Reports.ShowFindOccultedStars();
		}

		private void cmdUnfittedEvents_Click(object sender, EventArgs e)
		{
			Asteroid_Observations_Reports.ListUnfittedEvents();
		}

		private void CmdListWrongUncertainties_Click(object sender, EventArgs e)
		{
			Asteroid_Observations_Reports.ListWrongUncertainties();
		}

		private void cmdForReview_Click(object sender, EventArgs e)
		{
			Asteroid_Observations_Reports.ListEventsForReview();
		}

		private void cmdGenerateAsteroidGraphicFiles_Click(object sender, EventArgs e)
		{
			Data_and_Plots.ShowReductionPlotForm(0);
		}

		private void cmdNASA_PDS_Click(object sender, EventArgs e)
		{
			Asteroid_Observations_Reports.Show_MPCandPDS();
		}

		private void cmdListAsteroidObserver_Click(object sender, EventArgs e)
		{
			Asteroid_Observations_Reports.ShowEventsForObservers();
		}

		private void cmdListDuplicates_Click(object sender, EventArgs e)
		{
			Asteroid_Observations_Reports.ListDuplicates();
		}

		private void cmdListPoorStars_Click(object sender, EventArgs e)
		{
			Data_and_Plots.ShowReductionPlotForm(1);
		}

		private void cmdValidate_Click(object sender, EventArgs e)
		{
			Data_and_Plots.ValidateHistoricalFile();
		}

		private void cmdCheckAsteroidalMidTimes_Click(object sender, EventArgs e)
		{
			Asteroid_Observations_Reports.CheckMidTofAllEvents();
		}

		private void cmdMergeEditedWithMain_Click(object sender, EventArgs e)
		{
			Data_and_Plots.ShowMergeObservations();
		}

		private void cmdCloseAllAsteroidObservations_Click(object sender, EventArgs e)
		{
			Asteroid_Observations_Reports.CloseAsteroidObservationForms();
		}

		private void cmdAsteroidDiameter_Click(object sender, EventArgs e)
		{
			Data_and_Plots.ShowDiameterFromChords();
		}

		private void cmdAddEdit_Click(object sender, EventArgs e)
		{
			LunarObservations.Show_ObservationsEditor();
		}

		private void cmdArchiveLightCurve_Click(object sender, EventArgs e)
		{
			LightData.ShowLightCurveForm();
			LightData.LightCurveForm.SetAsAsteroidal();
			((Control)LightData.LightCurveForm).Focus();
		}

		private void cmdViewLightCurve_Click(object sender, EventArgs e)
		{
			LightData.ShowLightCurveViewer();
			((Control)LightData.LightCurveView).Focus();
		}

		private void cmdSolveDouble_Click(object sender, EventArgs e)
		{
			LunarObservations.Show_DoublesSolve();
		}

		private void cmdViewHistoricalGrazes_Click(object sender, EventArgs e)
		{
			LunarObservations.Show_HistoricalGrazes();
		}

		private void cmdViewHistoricalOccults_Click(object sender, EventArgs e)
		{
			LunarObservations.Show_HistoricalOccults();
		}

		private void cmdListLunarsOfK2Stars_Click(object sender, EventArgs e)
		{
			LunarOccultations.XZManageShowForm(CreateLunarK2Report: true);
		}

		private void cmdListMissingGrazes_Click(object sender, EventArgs e)
		{
			LunarObservations.Show_MissingGrazes();
		}

		private void cmdViewLightCurveLunar_Click(object sender, EventArgs e)
		{
			LightData.ShowLightCurveViewer();
			((Control)LightData.LightCurveView).Focus();
		}

		private void cmdProcessLightCurves_Click(object sender, EventArgs e)
		{
			LightData.ShowLightCurveViewer();
			((Control)LightData.LightCurveView).Focus();
		}

		private void cmdPlotLunarAgainstLimb_Click(object sender, EventArgs e)
		{
			ReductionProfile.Show_ReductionProfile(-10);
		}

		private void cmdProcessRegionalFiles_Click(object sender, EventArgs e)
		{
			LunarObservations.ShowGlobalLunarProcessing();
		}

		private void cmdReduceArchiveFile_Click(object sender, EventArgs e)
		{
			LunarObservations.ReduceArchiveFile();
		}

		private void cmdVizierArchive_Click(object sender, EventArgs e)
		{
			LunarObservations.CreateVizierArchiveFile();
		}

		private void cmdArchiveEditor_Click(object sender, EventArgs e)
		{
			LunarObservations.Show_ArchiveEditor();
		}

		private void cmdLunarEventFromStarAltitude_Click(object sender, EventArgs e)
		{
			LunarObservations.Show_UTfromStarAlt();
		}

		private void cmdLunarEventFromStarAltitude_MouseHover(object sender, EventArgs e)
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0005: Unknown result type (might be due to invalid IL or missing references)
			//IL_000d: Unknown result type (might be due to invalid IL or missing references)
			ToolTip val = new ToolTip();
			val.set_AutomaticDelay(100);
			val.set_AutoPopDelay(10000);
			val.SetToolTip((Control)(object)cmdLunarEventFromStarAltitude, "This function derives the UT of an event using the\r\naltitude of a star at the time of the event. \r\n\r\nIt is used to determine the event time for occultation \r\nobservations made before the invention of \r\npendulum clocks, where the altitude of a bright star \r\nwas used as a measure of the event time.");
		}

		private void button3_Click(object sender, EventArgs e)
		{
			//IL_0012: Unknown result type (might be due to invalid IL or missing references)
			//IL_0018: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Do you want to close all Lunar Occultation Observation forms?", "Close All", (MessageBoxButtons)1, (MessageBoxIcon)32, (MessageBoxDefaultButton)256) != 2)
			{
				LunarObservations.CloseAllLunarObservationForms();
			}
		}

		private bool UpdatesExist(out string Version)
		{
			//IL_002a: Unknown result type (might be due to invalid IL or missing references)
			string text = Utilities.AppPath + "\\Resource Files\\";
			Version = "";
			bool flag = false;
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
				return flag;
			}
			if (http.DownloadHTTP(Utilities.OccultServer(), "DownloadControl.txt", Utilities.AppPath + "\\Resource Files\\downloadcontrol.txt", unzip: false, gunzip: false, ShowMessages: false, ShowProgressbar: false))
			{
				if (!File.Exists(text + "DownloadControl.txt"))
				{
					return flag;
				}
				using StreamReader streamReader = new StreamReader(text + "DownloadControl.txt");
				while (!streamReader.EndOfStream)
				{
					string text2 = streamReader.ReadLine()!.PadRight(12);
					if (text2.Substring(0, 8) == "<Occult>")
					{
						string text3 = text2.Substring(8);
						((ToolStripItem)updateWithLatestVersionToolStripMenuItem).set_Text("Update/revert program to latest release version - " + text3);
						int result = 0;
						int result2 = 0;
						int result3 = 0;
						int result4 = 0;
						int num = 0;
						int num2 = 0;
						flag = false;
						Version = text2.Substring(8);
						num = 0;
						num2 = Version.IndexOf(".");
						if (num2 > num)
						{
							if (!int.TryParse(Version.Substring(0, num2 - num), out result))
							{
								result = 0;
							}
							flag = result > Assembly.GetExecutingAssembly().GetName(copiedName: false).Version!.Major;
							if (!flag & (result == Assembly.GetExecutingAssembly().GetName(copiedName: false).Version!.Major))
							{
								num = num2 + 1;
								num2 = Version.IndexOf(".", num);
								if (num2 > num)
								{
									if (!int.TryParse(Version.Substring(num, num2 - num), out result2))
									{
										result2 = 0;
									}
									flag = result2 > Assembly.GetExecutingAssembly().GetName(copiedName: false).Version!.Minor;
									if (!flag & (result2 == Assembly.GetExecutingAssembly().GetName(copiedName: false).Version!.Minor))
									{
										num = num2 + 1;
										num2 = Version.IndexOf(".", num);
										if (num2 > num)
										{
											if (!int.TryParse(Version.Substring(num, num2 - num), out result3))
											{
												result3 = 0;
											}
											flag = result3 > Assembly.GetExecutingAssembly().GetName(copiedName: false).Version!.Build;
											if (!flag & (result3 == Assembly.GetExecutingAssembly().GetName(copiedName: false).Version!.Build))
											{
												num = num2 + 1;
												if (num < Version.Length)
												{
													if (!int.TryParse(Version.Substring(num), out result4))
													{
														result4 = 0;
													}
													flag = result4 > Assembly.GetExecutingAssembly().GetName(copiedName: false).Version!.Revision;
												}
											}
										}
									}
								}
							}
						}
					}
					if (text2.Substring(1, 10) == "OccultBeta")
					{
						string text4 = text2.Substring(12);
						((ToolStripItem)updateProgramWithLatestBetaVersionifAnyToolStripMenuItem).set_Text("Update program to latest beta version - " + text4);
					}
					if (text2.Substring(1, 9) == "Addresses" && File.GetLastWriteTime(text + "Addresses.txt") < Utilities.GetUpdateDate(text2) && http.DownloadHTTP(Utilities.OccultServer(), "Addresses.zip", Utilities.AppPath + "\\Downloaded Files", unzip: true, gunzip: false, ShowMessages: false, ShowProgressbar: false))
					{
						File.Copy(Utilities.AppPath + "\\Downloaded Files\\addresses.txt", Utilities.AppPath + "\\Resource Files\\Addresses.txt", overwrite: true);
					}
				}
				return flag;
			}
			return flag;
		}

		private void UpdateOccult(bool beta)
		{
			//IL_006e: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f7: Invalid comparison between Unknown and I4
			//IL_0145: Unknown result type (might be due to invalid IL or missing references)
			//IL_014b: Invalid comparison between Unknown and I4
			//IL_028b: Unknown result type (might be due to invalid IL or missing references)
			//IL_029c: Unknown result type (might be due to invalid IL or missing references)
			RunUpdater = false;
			using (Process.GetCurrentProcess())
			{
				int num = 0;
				Process[] processes = Process.GetProcesses();
				foreach (Process process in processes)
				{
					try
					{
						if (Path.GetFileName(process.MainModule!.FileName) == "Occult.exe")
						{
							num++;
						}
					}
					catch
					{
					}
				}
				if (num > 1)
				{
					MessageBox.Show(num + " instances of Occult are running.\r\n\r\nThe update of Occult requires only one instance of Occult to be running.\r\n\r\nPlease close all other instances of Occult, and try again.", "Several instances of Occult", (MessageBoxButtons)0);
					return;
				}
			}
			using (Process.GetCurrentProcess())
			{
				Process[] processes = Process.GetProcesses();
				foreach (Process process3 in processes)
				{
					try
					{
						if (Path.GetFileName(process3.MainModule!.FileName) == "OccultWatcher.exe")
						{
							Occult.Properties.Settings.Default.OWaddress = process3.MainModule!.FileName;
							((ToolStripItem)restartOccultWatcherToolStripMenuItem).set_Enabled(true);
							if ((int)MessageBox.Show("OccultWatcher is running. To procede with the update, OccultWatcher must be closed.\r\n\r\nDo you want to close OccultWatcher now?", "OccultWatcher running", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256) == 7)
							{
								return;
							}
							process3.Kill();
							Occult.Properties.Settings.Default.OWKilledAtLastRun = true;
						}
					}
					catch
					{
					}
					try
					{
						if (Path.GetFileName(process3.MainModule!.FileName) == "Tangra.exe")
						{
							if ((int)MessageBox.Show("Tangra is running. To procede with the update, Tangra must be closed.\r\n\r\nDo you want to close Tangra now?", "Tangra running", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256) == 7)
							{
								return;
							}
							process3.Kill();
						}
					}
					catch
					{
					}
				}
			}
			bool flag = false;
			if ((!beta) ? http.DownloadHTTP(Utilities.OccultServer(), "OccultUpdate.zip", Utilities.AppPath + "\\Updates", unzip: true, gunzip: false, ShowMessages: false, ShowProgressbar: true) : http.DownloadHTTP(Utilities.OccultServer(), "OccultUpdateBeta.zip", Utilities.AppPath + "\\Updates", unzip: true, gunzip: false, ShowMessages: false, ShowProgressbar: true))
			{
				string text = AppPath + "\\Updates\\OccultUpdater.exe";
				string text2 = AppPath + "\\" + Path.GetFileName(text);
				string text3 = AppPath + "\\" + Path.GetFileName(text) + ".old";
				if (File.GetLastWriteTimeUtc(text) > File.GetLastWriteTimeUtc(text2).AddHours(1.0))
				{
					if (File.Exists(text2))
					{
						if (File.Exists(text3))
						{
							File.Delete(text3);
						}
						File.Move(text2, text3);
					}
					File.Move(text, text2);
				}
				else
				{
					File.Delete(text);
				}
				RunUpdater = true;
				((Form)this).Close();
			}
			else if (beta)
			{
				MessageBox.Show("Update file was not downloaded successfully.\r\nA beta version of Occult may not be available.", "Update failed");
			}
			else
			{
				MessageBox.Show("Update file was not downloaded successfully.\r\nMake sure you are connected to the internet, or try again later.", "Update failed");
			}
		}

		private void restartOccultWatcherToolStripMenuItem_Click(object sender, EventArgs e)
		{
			StartOW(AtStartUp: false);
		}

		private static void StartOW(bool AtStartUp)
		{
			//IL_0040: Unknown result type (might be due to invalid IL or missing references)
			using (Process.GetCurrentProcess())
			{
				Process[] processes = Process.GetProcesses();
				foreach (Process process in processes)
				{
					try
					{
						if (Path.GetFileName(process.MainModule!.FileName) == "OccultWatcher.exe")
						{
							if (!AtStartUp)
							{
								MessageBox.Show("OccultWatcher is already running", "OccultWatcher running", (MessageBoxButtons)0, (MessageBoxIcon)48);
							}
							return;
						}
					}
					catch
					{
					}
				}
			}
			if (Occult.Properties.Settings.Default.OWaddress.ToLower().Contains("occultwatcher.exe") && File.Exists(Occult.Properties.Settings.Default.OWaddress))
			{
				Process process3 = new Process();
				process3.StartInfo.FileName = Occult.Properties.Settings.Default.OWaddress;
				process3.Start();
			}
		}

		private void checkForUpdatesToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_002c: Unknown result type (might be due to invalid IL or missing references)
			//IL_004c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0052: Invalid comparison between Unknown and I4
			string Version;
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
			}
			else if (!UpdatesExist(out Version))
			{
				MessageBox.Show("There are no updates available", "No updates");
			}
			else if ((int)MessageBox.Show("An update to Version " + Version + " is available.\r\n\r\nDo you want to proceed with the update?", "Update available", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0) != 7)
			{
				UpdateOccult(beta: false);
			}
		}

		private void listChangesSinceCurrentVersionToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ab: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
			}
			else if (http.DownloadHTTP(Utilities.OccultServer(), "Updates.txt", Utilities.AppPath + "\\Downloaded Files\\updates.txt", unzip: false, gunzip: false, ShowMessages: false, ShowProgressbar: false))
			{
				UpdatesText updatesText = new UpdatesText();
				using (StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Downloaded Files\\Updates.txt"))
				{
					((Control)updatesText.txtUpdate).set_Text(streamReader.ReadToEnd().Replace("\r\n", "\n").Replace("\n", Environment.NewLine));
					((TextBoxBase)updatesText.txtUpdate).set_SelectionStart(0);
					((TextBoxBase)updatesText.txtUpdate).set_SelectionLength(0);
				}
				((Form)updatesText).ShowDialog();
			}
		}

		private void toolStripUpdates_MouseEnter(object sender, EventArgs e)
		{
			((Control)this).set_Cursor(Cursors.get_Hand());
		}

		private void toolStripUpdates_MouseLeave(object sender, EventArgs e)
		{
			((Control)this).set_Cursor(Cursors.get_Default());
		}

		private void toolStripUpdates_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_0026: Unknown result type (might be due to invalid IL or missing references)
			//IL_002c: Invalid comparison between Unknown and I4
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
			}
			else if ((int)MessageBox.Show("Do you want to proceed to update Occult?\r\n\r\n[Any processes running will be terminated.]", "Update available", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0) != 7)
			{
				UpdateOccult(beta: false);
			}
		}

		private void cmdCreateDowloadControl_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
			}
			else
			{
				((Form)new DownloadControl()).ShowDialog();
			}
		}

		private void toolStripDataUpdates_Click_1(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
				return;
			}
			ShowDownloadPage();
			((ToolStripItem)toolStripDataUpdates).set_Visible(UpdatesAvailable.DataUpdatesAvailable());
		}

		private void checkForDataUpdatesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_0029: Unknown result type (might be due to invalid IL or missing references)
			//IL_003e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0044: Invalid comparison between Unknown and I4
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
			}
			else if (!UpdatesAvailable.DataUpdatesAvailable())
			{
				MessageBox.Show("No data updates are indicated as being required", "No data updates");
			}
			else if ((int)MessageBox.Show("Data updates are available.\r\n\r\nDo you want to proceed with the update?", "Update available", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0) != 7)
			{
				ShowDownloadPage();
			}
		}

		private void updateWithLatestVersionToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
			}
			else
			{
				UpdateOccult(beta: false);
			}
		}

		private void updateProgramWithLatestBetaVersionifAnyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
			}
			else
			{
				UpdateOccult(beta: true);
			}
		}

		private void dayWeatherForecastForhomeToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
			}
			else
			{
				APanelWeatherInfo.ShowWeatherForHome();
			}
		}

		private void regionalCloudMapPredictionsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
				return;
			}
			APanelWeatherInfo.ShowCloudMap();
			APanelWeatherInfo.Cloud_Map.SetRequestDate(DateTime.Now.ToUniversalTime().AddHours(2.0));
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (Data_and_Plots.Observations_Editor != null)
			{
				Data_and_Plots.Observations_Editor.CloseAsteroidObsEditor_PeripheralForms();
			}
			((Form)this).Close();
		}

		private void cmdDisplayOccultations_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)SelectAndDisplay).Show();
			}
			catch
			{
				SelectAndDisplay = new SelectAndDisplay();
				((Control)SelectAndDisplay).Show();
			}
		}

		private void cmdListAndDisplayAsteroidals_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.ShowListAndDisplay();
		}

		private void helpWithUpdatesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", navigator, (object)"Updating Occult");
		}

		private void cmdStarAnalysis_Click(object sender, EventArgs e)
		{
			LunarObservations.ShowStarAnalysis();
		}

		private void cmdAsteroidSatellites_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)AsteroidSatellites).Show();
			}
			catch
			{
				AsteroidSatellites = new AsteroidSatellites();
				((Control)AsteroidSatellites).Show();
			}
		}

		private void cmdDistances_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)Distances).Show();
			}
			catch
			{
				Distances = new Distances();
				((Control)Distances).Show();
			}
		}

		private void cmdListEquivalents_Click(object sender, EventArgs e)
		{
			Vizier_Sesame.GetStarEquivalents(cmbStarCat.get_Items().get_Item(((ListControl)cmbStarCat).get_SelectedIndex()).ToString(), ((Control)txtStarNumber).get_Text());
		}

		private void virtualDubTimerToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarOccultations.Show_RecordingTimer();
		}

		private void Process_LightCurves_Click(object sender, EventArgs e)
		{
			LightData.ShowLightCurveViewer();
			((Control)LightData.LightCurveView).Focus();
		}

		private void chkUseExistingFiles_Click(object sender, EventArgs e)
		{
			//IL_006e: Unknown result type (might be due to invalid IL or missing references)
			chkUseExistingFiles.set_Checked(!chkUseExistingFiles.get_Checked());
			if (chkUseExistingFiles.get_Checked())
			{
				((Control)cmdConvertRIO).set_Text("Convert\r\nexisting files");
				chkUseLocal.set_Checked(false);
			}
			else
			{
				((Control)cmdConvertRIO).set_Text("Download and\r\nConvert");
			}
			if (chkUseExistingFiles.get_Checked())
			{
				MessageBox.Show("You should only use the existing files if\r\n  (i) there is a need to edit TNO source files for some reason, and\r\n  (ii) you know what you are doing!\r\n\r\nOtherwise you should NOT use this option.", "RIO-TNO option to use existing files", (MessageBoxButtons)0, (MessageBoxIcon)48);
			}
		}

		private void chkUseLocal_Click(object sender, EventArgs e)
		{
			chkUseLocal.set_Checked(!chkUseLocal.get_Checked());
			if (chkUseLocal.get_Checked())
			{
				((Control)cmdConvertRIO).set_Text("Convert LOG.dat &&\r\ntable_occult.txt");
				chkUseExistingFiles.set_Checked(false);
			}
			else
			{
				((Control)cmdConvertRIO).set_Text("Download and\r\nConvert");
			}
		}

		private void runCurrentTestRoutineToolStripMenuItem_Click(object sender, EventArgs e)
		{
			string ResponseText = "";
			int CountFromISAM = 0;
			http.IsISAM_UpToDate(out ResponseText, out CountFromISAM);
		}

		private void runExtrnalAccessTestRoutineToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_001c: Unknown result type (might be due to invalid IL or missing references)
			MessageBox.Show("Return value = " + ExternalCalls.CreateOutputFiles("d:\\Occult\\Asteroid\\Preston\\DidymosTest\\ControlFile.txt"));
		}

		private void testsToolStripMenuItem_Click(object sender, EventArgs e)
		{
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
		}

		private void helpToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			//IL_0047: Unknown result type (might be due to invalid IL or missing references)
			//IL_0068: Unknown result type (might be due to invalid IL or missing references)
			//IL_0089: Unknown result type (might be due to invalid IL or missing references)
			//IL_00aa: Unknown result type (might be due to invalid IL or missing references)
			//IL_00cb: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ec: Unknown result type (might be due to invalid IL or missing references)
			//IL_010d: Unknown result type (might be due to invalid IL or missing references)
			//IL_012e: Unknown result type (might be due to invalid IL or missing references)
			//IL_014f: Unknown result type (might be due to invalid IL or missing references)
			switch (Occult.Properties.Settings.Default.InitialDisplayGroup)
			{
			case 0:
				Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", navigator, (object)"Main form Lunar Predictions");
				break;
			case 1:
				Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", navigator, (object)"Main form Asteroid Search");
				break;
			case 2:
				Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", navigator, (object)"Main form Eclipses");
				break;
			case 3:
				Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", navigator, (object)"Main form Ephemeris");
				break;
			case 4:
				Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", navigator, (object)"Main form Satellites");
				break;
			case 5:
				Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", navigator, (object)"Main form Maintenance");
				break;
			case 6:
				Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", navigator, (object)"Main form Asteroid Results");
				break;
			case 7:
				Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", navigator, (object)"Main form Lunar Results");
				break;
			default:
				Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", navigator, (object)"Occult v4 - overview");
				break;
			}
		}

		private void helpNotWorkingToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0005: Unknown result type (might be due to invalid IL or missing references)
			((Form)new HelpAssist()).ShowDialog();
		}

		private void cmdAsteroidClasses_Click(object sender, EventArgs e)
		{
			http.GetAsteroidClasses();
		}

		private void cmdAsteroidTaxonomy_Click(object sender, EventArgs e)
		{
			http.GetAsteroidClasses();
		}

		private void cascadeFormsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0019: Unknown result type (might be due to invalid IL or missing references)
			//IL_001e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0039: Unknown result type (might be due to invalid IL or missing references)
			int num = 10;
			int num2 = 10;
			foreach (Form item in (ReadOnlyCollectionBase)(object)Application.get_OpenForms())
			{
				((Control)item).set_Top(num);
				num += 25;
				if (num > ((Control)this).get_Height() - 100)
				{
					num = 22;
				}
				((Control)item).set_Left(num2);
				num2 += 25;
				if (num > ((Control)this).get_Width() - 200)
				{
					num2 = 22;
				}
				((Control)item).Focus();
			}
		}

		private void cmdUpdateKeplerLinks_Click(object sender, EventArgs e)
		{
		}

		private void button2_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", navigator, (object)"PDS");
		}

		private void cmdNewDAMIT_Click(object sender, EventArgs e)
		{
			((Control)new global::Shapes.Shapes()).Show();
		}

		private void unzipToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Unzip.UnZip("e:\\Tunzip\\GaiaEDR3_16.zip", "e:\\Tunzip", "", "");
		}

		private void cmdGaiaDoubleStars_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)GaiaDoubles.Elements.GaiaDoubles).Show();
			}
			catch
			{
				GaiaDoubles.Elements.GaiaDoubles = new GaiaDoubles_Display();
				((Control)GaiaDoubles.Elements.GaiaDoubles).Show();
			}
		}

		private void cmdDeleteSuperceded_Click(object sender, EventArgs e)
		{
			Utilities.DeleteSupercededFiles();
		}

		private void backUpSourceCodeToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0055: Unknown result type (might be due to invalid IL or missing references)
			string text = "";
			string directoryName = Path.GetDirectoryName(GetThisFilePath("C:\\Occult C#14\\Occult\\WindowsApplication1\\Occult_Main.cs"));
			int num = directoryName.IndexOf("C#14");
			if (num > 0)
			{
				string text2 = directoryName.Substring(0, num + 4);
				text = text2.Replace("C#14", "Backups");
				BackupOccult.BackupOccultSourceCode(text2, text);
			}
			else
			{
				MessageBox.Show("Occult C#14 directory not found", "", (MessageBoxButtons)0, (MessageBoxIcon)16);
			}
		}

		private static string GetThisFilePath([CallerFilePath] string path = null)
		{
			return path;
		}

		private void cmdSearchFreeText_Click(object sender, EventArgs e)
		{
			Asteroid_Observations_Reports.Show_FreeText();
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
			//IL_0a51: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a5b: Expected O, but got Unknown
			//IL_0a5c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a66: Expected O, but got Unknown
			//IL_29a3: Unknown result type (might be due to invalid IL or missing references)
			//IL_2a42: Unknown result type (might be due to invalid IL or missing references)
			//IL_2b08: Unknown result type (might be due to invalid IL or missing references)
			//IL_2bce: Unknown result type (might be due to invalid IL or missing references)
			//IL_2c94: Unknown result type (might be due to invalid IL or missing references)
			//IL_2d63: Unknown result type (might be due to invalid IL or missing references)
			//IL_2e29: Unknown result type (might be due to invalid IL or missing references)
			//IL_2eef: Unknown result type (might be due to invalid IL or missing references)
			//IL_306f: Unknown result type (might be due to invalid IL or missing references)
			//IL_3094: Unknown result type (might be due to invalid IL or missing references)
			//IL_3165: Unknown result type (might be due to invalid IL or missing references)
			//IL_9e4c: Unknown result type (might be due to invalid IL or missing references)
			//IL_9e56: Expected O, but got Unknown
			//IL_9ffe: Unknown result type (might be due to invalid IL or missing references)
			//IL_a008: Expected O, but got Unknown
			//IL_a085: Unknown result type (might be due to invalid IL or missing references)
			//IL_a08f: Expected O, but got Unknown
			//IL_a097: Unknown result type (might be due to invalid IL or missing references)
			//IL_a0a1: Expected O, but got Unknown
			components = new Container();
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(frmOccult));
			cmdLunarEclipse = new Button();
			cmdTransits = new Button();
			cmddeltaT = new Button();
			cmdPlanetPlot = new Button();
			cmdMutualEvents = new Button();
			cmdPlanetEphemeris = new Button();
			cmdCalendar = new Button();
			MoonPhases = new Button();
			cmdRiseSet = new Button();
			cmdConversions = new Button();
			cmdMutualConjunctions = new Button();
			cmdSatellitePositions = new Button();
			cmdDiary = new Button();
			cmdEditUserMinorPlanetsLunar = new Button();
			lblPreserveFuture = new Label();
			cmdDownloadFuture = new Button();
			button1 = new Button();
			cmdEditUserMinorPlanets = new Button();
			cmdEditUserStar = new Button();
			cmdAsteroidSearch = new Button();
			cmdSatelliteEvents = new Button();
			grpEphemeris = new GroupBox();
			CmdSidereal = new Button();
			cmdComets = new Button();
			cmdMeridians = new Button();
			cmdMoonRiseSet = new Button();
			cmdConvertAstorb = new Button();
			menuStrip1 = new MenuStrip();
			weatherToolStripMenuItem = new ToolStripMenuItem();
			dayWeatherForecastForhomeToolStripMenuItem = new ToolStripMenuItem();
			regionalCloudMapPredictionsToolStripMenuItem = new ToolStripMenuItem();
			virtualDubTimerToolStripMenuItem = new ToolStripMenuItem();
			checkForUpdatesToolStripMenuItem = new ToolStripMenuItem();
			listChangesSinceCurrentVersionToolStripMenuItem = new ToolStripMenuItem();
			checkForUpdatesToolStripMenuItem1 = new ToolStripMenuItem();
			checkForDataUpdatesToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			toolStripSeparator2 = new ToolStripSeparator();
			updateWithLatestVersionToolStripMenuItem = new ToolStripMenuItem();
			updateProgramWithLatestBetaVersionifAnyToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator9 = new ToolStripSeparator();
			toolStripSeparator3 = new ToolStripSeparator();
			previousVersionsAreInUpdatesToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator8 = new ToolStripSeparator();
			restartOccultWatcherToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator4 = new ToolStripSeparator();
			helpWithUpdatesToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator7 = new ToolStripSeparator();
			cascadeFormsToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem1 = new ToolStripMenuItem();
			toolStripSeparator5 = new ToolStripSeparator();
			toolStripSeparator6 = new ToolStripSeparator();
			helpNotWorkingToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			testsToolStripMenuItem = new ToolStripMenuItem();
			backUpSourceCodeToolStripMenuItem = new ToolStripMenuItem();
			runCurrentTestRoutineToolStripMenuItem = new ToolStripMenuItem();
			runExtrnalAccessTestRoutineToolStripMenuItem = new ToolStripMenuItem();
			unzipToolStripMenuItem = new ToolStripMenuItem();
			MainToolStrip = new ToolStrip();
			TS_AsteroidPredict = new ToolStripButton();
			TS_AsteroidReduce = new ToolStripButton();
			TS_Eclipses = new ToolStripButton();
			TS_Ephemeris = new ToolStripButton();
			TS_LunarPredict = new ToolStripButton();
			TS_LunarObservations = new ToolStripButton();
			TS_Satellites = new ToolStripButton();
			TS_Maintenance = new ToolStripButton();
			toolStripUpdates = new ToolStripLabel();
			toolStripDataUpdates = new ToolStripLabel();
			PanelAsteroidPredictions = new Panel();
			cmdAsteroidTaxonomy = new Button();
			cmdLightCurveSimulator = new Button();
			cmdMutualAsteroidals = new Button();
			cmdSelectDE = new Button();
			cmdListAndDisplayAsteroidals = new Button();
			groupBox8 = new GroupBox();
			groupBox11 = new GroupBox();
			chkExcludeOldLuckyStar = new CheckBox();
			cmdLuckyStar = new Button();
			groupBox10 = new GroupBox();
			chkUseLocal = new CheckBox();
			chkUseExistingFiles = new CheckBox();
			chkExcludeOldRio = new CheckBox();
			cmdConvertRIO = new Button();
			groupBox7 = new GroupBox();
			cmdConvertAstorb_MPCOrb = new Button();
			label3 = new Label();
			label2 = new Label();
			label9 = new Label();
			label7 = new Label();
			PanelEphemeris = new Panel();
			groupBox12 = new GroupBox();
			cmdLightCurves = new Button();
			cmdSelectDEEphemeris = new Button();
			grpShapeModels = new GroupBox();
			cmdNewDAMIT = new Button();
			cmdShapeModels = new Button();
			cmdCloseAllEphemerisForms = new Button();
			groupBox6 = new GroupBox();
			cmdRelativity = new Button();
			cmdDistances = new Button();
			cmdDatumConversions = new Button();
			cmdMagnitudes = new Button();
			cmdDrawStarChart = new Button();
			label10 = new Label();
			groupBox4 = new GroupBox();
			groupBox3 = new GroupBox();
			groupBox2 = new GroupBox();
			cmdObjectAltitude = new Button();
			groupBox1 = new GroupBox();
			cmdAsteroidSatellites = new Button();
			cmdAsteroidEphemeris = new Button();
			ViewStarCatDetails2 = new Button();
			PanelSatellites = new Panel();
			label24 = new Label();
			label23 = new Label();
			label12 = new Label();
			PanelMaintenance = new Panel();
			cmdDeleteSuperceded = new Button();
			cmdGaiaDoubleStars = new Button();
			cmdAsteroidClasses = new Button();
			label6 = new Label();
			cmdCreateGaia = new Button();
			Process_LightCurves = new Button();
			cmdCreateDEEphemerisFiles = new Button();
			cmdEditAsteroidRings = new Button();
			label5 = new Label();
			label4 = new Label();
			txtStarNumber = new TextBox();
			cmbStarCat = new ComboBox();
			cmdListEquivalents = new Button();
			cmdCreateDowloadControl = new Button();
			cmdViewWDS = new Button();
			cmdCompareStarPositions = new Button();
			cmdDownloads = new Button();
			label8 = new Label();
			cmdCheckInstall = new Button();
			label29 = new Label();
			cmbDisplayCats = new Button();
			grpNomad = new GroupBox();
			cmdLaSilla = new Button();
			label33 = new Label();
			cmdEditBinaryAsteroids = new Button();
			cmdXZ80Maintain = new Button();
			cmdEditSites = new Button();
			label26 = new Label();
			label22 = new Label();
			cmdDiameters = new Button();
			cmdDefault = new Button();
			PanelAsteroidObservations = new Panel();
			groupBox9 = new GroupBox();
			cmdViewLightCurve = new Button();
			cmdArchiveLightCurve = new Button();
			groupBox5 = new GroupBox();
			cmdAOTA = new Button();
			cmdStarDiameterAnalyser = new Button();
			label13 = new Label();
			cmdCloseAllAsteroidObservations = new Button();
			cmdAsteroidObservations = new Button();
			grpMaintainObsFile = new GroupBox();
			cmdSearchFreeText = new Button();
			cmdMergeEditedWithMain = new Button();
			cmdNASA_PDS = new Button();
			cmdUpdateKeplerLinks = new Button();
			cmdCheckAsteroidalMidTimes = new Button();
			cmdValidate = new Button();
			cmdListPoorStars = new Button();
			cmdListDuplicates = new Button();
			grpAsteroidLists = new GroupBox();
			cmdListEventsWithNoMainBody = new Button();
			cmdRingedAsteroids = new Button();
			cmdForReview = new Button();
			cmdListWrongUncertainties = new Button();
			cmdUnfittedEvents = new Button();
			cmdBinaryAsteroids = new Button();
			cmdShapeModelFits = new Button();
			cmdListAsteroidObserver = new Button();
			cmdFindOccultedStars_Asteroidal = new Button();
			cmdK2Events = new Button();
			cmdAsteroidDiameter = new Button();
			cmdGenerateDistantAsteroidList = new Button();
			cmdGenerateAsteroidGraphicFiles = new Button();
			cmdListDiameters = new Button();
			cmdListPositions = new Button();
			cmdListDoubleStars = new Button();
			label27 = new Label();
			PanelEclipsesTransits = new Panel();
			label1 = new Label();
			cmdSelectDE_eclipses = new Button();
			cmdCraterTimings = new Button();
			label32 = new Label();
			cmdBailyBeads = new Button();
			label20 = new Label();
			label19 = new Label();
			cmdSolarEclipse = new Button();
			label11 = new Label();
			PanelLunarPredictions = new Panel();
			cmdCloseAllLunarPredictions = new Button();
			cmdAutoGrazeOutput = new Button();
			cmdSingleLunar = new Button();
			cmdStarCatDetails = new Button();
			cmdDisplayMoonInStarField = new Button();
			cmdFindOccultations = new Button();
			label21 = new Label();
			PanelLunarObservations = new Panel();
			cmdLunarEventFromStarAltitude = new Button();
			cmdViewLightCurveLunar = new Button();
			cmdListLunarsOfK2Stars = new Button();
			cmdStarAnalysis = new Button();
			cmdListMissingGrazes = new Button();
			cmdSolveDouble = new Button();
			cmdPlotLunarAgainstLimb = new Button();
			grpLunarReduceAdmin = new GroupBox();
			cmdVizierArchive = new Button();
			cmdReduceArchiveFile = new Button();
			cmdProcessLightCurves = new Button();
			cmdProcessRegionalFiles = new Button();
			cmdArchiveEditor = new Button();
			button3 = new Button();
			cmdViewHistoricalOccults = new Button();
			cmdViewHistoricalGrazes = new Button();
			cmdAddEdit = new Button();
			label25 = new Label();
			toolStrip1 = new ToolStrip();
			StripAsteroids = new ToolStripButton();
			StripMPReduce = new ToolStripButton();
			StripEclipses = new ToolStripButton();
			StripEphem = new ToolStripButton();
			StripLunar = new ToolStripButton();
			StripLunarObservations = new ToolStripButton();
			StripSatellites = new ToolStripButton();
			StripShared = new ToolStripButton();
			toolStripButton7 = new ToolStripButton();
			helpProvider1 = new HelpProvider();
			lblEphemerisSource = new Label();
			toolTip1 = new ToolTip(components);
			chkHighPriority = new CheckBox();
			((Control)grpEphemeris).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((Control)MainToolStrip).SuspendLayout();
			((Control)PanelAsteroidPredictions).SuspendLayout();
			((Control)groupBox8).SuspendLayout();
			((Control)groupBox11).SuspendLayout();
			((Control)groupBox10).SuspendLayout();
			((Control)groupBox7).SuspendLayout();
			((Control)PanelEphemeris).SuspendLayout();
			((Control)groupBox12).SuspendLayout();
			((Control)grpShapeModels).SuspendLayout();
			((Control)groupBox6).SuspendLayout();
			((Control)groupBox4).SuspendLayout();
			((Control)groupBox3).SuspendLayout();
			((Control)groupBox2).SuspendLayout();
			((Control)groupBox1).SuspendLayout();
			((Control)PanelSatellites).SuspendLayout();
			((Control)PanelMaintenance).SuspendLayout();
			((Control)grpNomad).SuspendLayout();
			((Control)PanelAsteroidObservations).SuspendLayout();
			((Control)groupBox9).SuspendLayout();
			((Control)groupBox5).SuspendLayout();
			((Control)grpMaintainObsFile).SuspendLayout();
			((Control)grpAsteroidLists).SuspendLayout();
			((Control)PanelEclipsesTransits).SuspendLayout();
			((Control)PanelLunarPredictions).SuspendLayout();
			((Control)PanelLunarObservations).SuspendLayout();
			((Control)grpLunarReduceAdmin).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)cmdLunarEclipse).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdLunarEclipse).set_Location(new Point(228, 76));
			((Control)cmdLunarEclipse).set_Name("cmdLunarEclipse");
			((Control)cmdLunarEclipse).set_Size(new Size(124, 61));
			((Control)cmdLunarEclipse).set_TabIndex(2);
			((Control)cmdLunarEclipse).set_Text("Lunar Eclipses");
			((ButtonBase)cmdLunarEclipse).set_UseVisualStyleBackColor(true);
			((Control)cmdLunarEclipse).add_Click((EventHandler)cmdLunarEclipse_Click);
			((Control)cmdTransits).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdTransits).set_Location(new Point(392, 76));
			((Control)cmdTransits).set_Name("cmdTransits");
			((Control)cmdTransits).set_Size(new Size(124, 61));
			((Control)cmdTransits).set_TabIndex(3);
			((Control)cmdTransits).set_Text("Transits of Mercury && Venus");
			((ButtonBase)cmdTransits).set_UseVisualStyleBackColor(true);
			((Control)cmdTransits).add_Click((EventHandler)cmdTransits_Click);
			((Control)cmddeltaT).set_Location(new Point(59, 392));
			((Control)cmddeltaT).set_Name("cmddeltaT");
			((Control)cmddeltaT).set_Size(new Size(74, 36));
			((Control)cmddeltaT).set_TabIndex(19);
			((Control)cmddeltaT).set_Text("Edit deltaT");
			toolTip1.SetToolTip((Control)(object)cmddeltaT, "Tool to edit the file of deltaT\r\nvalues. Administrator use only.");
			((ButtonBase)cmddeltaT).set_UseVisualStyleBackColor(true);
			((Control)cmddeltaT).add_Click((EventHandler)cmddeltaT_Click);
			((Control)cmdPlanetPlot).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdPlanetPlot).set_Location(new Point(15, 23));
			((Control)cmdPlanetPlot).set_Name("cmdPlanetPlot");
			((Control)cmdPlanetPlot).set_Size(new Size(111, 54));
			((Control)cmdPlanetPlot).set_TabIndex(0);
			((Control)cmdPlanetPlot).set_Text("Graphics\r\nof the planets\r\nand their moons");
			toolTip1.SetToolTip((Control)(object)cmdPlanetPlot, "Show a pictorial representation of the \r\nmajor planets and their satellites");
			((ButtonBase)cmdPlanetPlot).set_UseVisualStyleBackColor(true);
			((Control)cmdPlanetPlot).add_Click((EventHandler)cmdPlanetPlot_Click);
			((Control)cmdMutualEvents).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdMutualEvents).set_Location(new Point(198, 321));
			((Control)cmdMutualEvents).set_Name("cmdMutualEvents");
			((Control)cmdMutualEvents).set_Size(new Size(185, 111));
			((Control)cmdMutualEvents).set_TabIndex(3);
			((Control)cmdMutualEvents).set_Text("MUTUAL eclipses && occultations\r\n-\r\n Jupiter, Saturn and Uranus");
			((ButtonBase)cmdMutualEvents).set_UseVisualStyleBackColor(true);
			((Control)cmdMutualEvents).add_Click((EventHandler)cmdMutualEvents_Click);
			((Control)cmdPlanetEphemeris).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdPlanetEphemeris).set_Location(new Point(15, 27));
			((Control)cmdPlanetEphemeris).set_Name("cmdPlanetEphemeris");
			((Control)cmdPlanetEphemeris).set_Size(new Size(85, 51));
			((Control)cmdPlanetEphemeris).set_TabIndex(0);
			((Control)cmdPlanetEphemeris).set_Text("Planets");
			toolTip1.SetToolTip((Control)(object)cmdPlanetEphemeris, "Generate ephemerides of the major planets");
			((ButtonBase)cmdPlanetEphemeris).set_UseVisualStyleBackColor(true);
			((Control)cmdPlanetEphemeris).add_Click((EventHandler)cmdPlanetEphemeris_Click);
			((Control)cmdCalendar).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdCalendar).set_Location(new Point(15, 21));
			((Control)cmdCalendar).set_Name("cmdCalendar");
			((Control)cmdCalendar).set_Size(new Size(111, 61));
			((Control)cmdCalendar).set_TabIndex(0);
			((Control)cmdCalendar).set_Text("Calendar");
			((ButtonBase)cmdCalendar).set_UseVisualStyleBackColor(true);
			((Control)cmdCalendar).add_Click((EventHandler)cmdCalendar_Click);
			((Control)MoonPhases).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)MoonPhases).set_Location(new Point(164, 23));
			((Control)MoonPhases).set_Name("MoonPhases");
			((Control)MoonPhases).set_Size(new Size(111, 54));
			((Control)MoonPhases).set_TabIndex(1);
			((Control)MoonPhases).set_Text("Moon phases,\r\nPerigee / Apogee,\r\nPhysical ephemeris");
			toolTip1.SetToolTip((Control)(object)MoonPhases, "List the dates of phases, perigee/apogee, \r\nand generate a physical ephemeris");
			((ButtonBase)MoonPhases).set_UseVisualStyleBackColor(true);
			((Control)MoonPhases).add_Click((EventHandler)MoonPhases_Click);
			((Control)cmdRiseSet).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdRiseSet).set_Location(new Point(15, 22));
			((Control)cmdRiseSet).set_Name("cmdRiseSet");
			((Control)cmdRiseSet).set_Size(new Size(111, 56));
			((Control)cmdRiseSet).set_TabIndex(0);
			((Control)cmdRiseSet).set_Text("Rise and set times of the major planets");
			((ButtonBase)cmdRiseSet).set_UseVisualStyleBackColor(true);
			((Control)cmdRiseSet).add_Click((EventHandler)cmdRiseSet_Click);
			((Control)cmdConversions).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdConversions).set_Location(new Point(313, 21));
			((Control)cmdConversions).set_Name("cmdConversions");
			((Control)cmdConversions).set_Size(new Size(111, 61));
			((Control)cmdConversions).set_TabIndex(2);
			((Control)cmdConversions).set_Text("JD <=> Date,\r\nAngle conversions,\r\nPrecession,\r\nEarth Orientation");
			toolTip1.SetToolTip((Control)(object)cmdConversions, "*Convert betwee: Date and JD\r\n*Precess between different dates\r\n*Express angles in different formats\r\n*Display Earth Orientation parameters");
			((ButtonBase)cmdConversions).set_UseVisualStyleBackColor(true);
			((Control)cmdConversions).add_Click((EventHandler)cmdConversions_Click);
			((Control)cmdMutualConjunctions).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdMutualConjunctions).set_Location(new Point(103, 22));
			((Control)cmdMutualConjunctions).set_Name("cmdMutualConjunctions");
			((Control)cmdMutualConjunctions).set_Size(new Size(82, 54));
			((Control)cmdMutualConjunctions).set_TabIndex(1);
			((Control)cmdMutualConjunctions).set_Text("Mutual conjunctions of the planets");
			toolTip1.SetToolTip((Control)(object)cmdMutualConjunctions, "Generate a list of mutual conjunctions\r\nbetween the major planets");
			((ButtonBase)cmdMutualConjunctions).set_UseVisualStyleBackColor(true);
			((Control)cmdMutualConjunctions).add_Click((EventHandler)cmdMutualConjunctions_Click);
			((Control)cmdSatellitePositions).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSatellitePositions).set_Location(new Point(300, 27));
			((Control)cmdSatellitePositions).set_Name("cmdSatellitePositions");
			((Control)cmdSatellitePositions).set_Size(new Size(58, 51));
			((Control)cmdSatellitePositions).set_TabIndex(3);
			((Control)cmdSatellitePositions).set_Text("Planet\r\nSatellites");
			toolTip1.SetToolTip((Control)(object)cmdSatellitePositions, "Generate ephemerides of the \r\nsatellites of the major planets");
			((ButtonBase)cmdSatellitePositions).set_UseVisualStyleBackColor(true);
			((Control)cmdSatellitePositions).add_Click((EventHandler)cmdSatellitePositions_Click);
			((Control)cmdDiary).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDiary).set_Location(new Point(15, 23));
			((Control)cmdDiary).set_Name("cmdDiary");
			((Control)cmdDiary).set_Size(new Size(82, 54));
			((Control)cmdDiary).set_TabIndex(0);
			((Control)cmdDiary).set_Text("Diary of astronomical phenomena");
			((ButtonBase)cmdDiary).set_UseVisualStyleBackColor(true);
			((Control)cmdDiary).add_Click((EventHandler)cmdDiary_Click);
			((Control)cmdEditUserMinorPlanetsLunar).set_Location(new Point(61, 322));
			((Control)cmdEditUserMinorPlanetsLunar).set_Name("cmdEditUserMinorPlanetsLunar");
			((Control)cmdEditUserMinorPlanetsLunar).set_Size(new Size(115, 41));
			((Control)cmdEditUserMinorPlanetsLunar).set_TabIndex(4);
			((Control)cmdEditUserMinorPlanetsLunar).set_Text("Edit the User file\r\nof minor planets");
			((ButtonBase)cmdEditUserMinorPlanetsLunar).set_UseVisualStyleBackColor(true);
			((Control)cmdEditUserMinorPlanetsLunar).add_Click((EventHandler)cmdEditUserMinorPlanetsLunar_Click);
			((Control)lblPreserveFuture).set_AutoSize(true);
			((Control)lblPreserveFuture).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblPreserveFuture).set_ForeColor(Color.MediumBlue);
			((Control)lblPreserveFuture).set_Location(new Point(29, 62));
			((Control)lblPreserveFuture).set_Name("lblPreserveFuture");
			((Control)lblPreserveFuture).set_Size(new Size(64, 13));
			((Control)lblPreserveFuture).set_TabIndex(7);
			((Control)lblPreserveFuture).set_Text("<Overwrite>");
			lblPreserveFuture.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)cmdDownloadFuture).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDownloadFuture).set_Location(new Point(7, 27));
			((Control)cmdDownloadFuture).set_Name("cmdDownloadFuture");
			((Control)cmdDownloadFuture).set_Size(new Size(86, 25));
			((Control)cmdDownloadFuture).set_TabIndex(5);
			((Control)cmdDownloadFuture).set_Text("future.xml");
			((ButtonBase)cmdDownloadFuture).set_UseVisualStyleBackColor(true);
			((Control)cmdDownloadFuture).add_Click((EventHandler)cmdDownloadFuture_Click);
			((Control)button1).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)button1).set_Location(new Point(99, 27));
			((Control)button1).set_Name("button1");
			((Control)button1).set_Size(new Size(86, 25));
			((Control)button1).set_TabIndex(6);
			((Control)button1).set_Text("futureALL.xml");
			((ButtonBase)button1).set_UseVisualStyleBackColor(true);
			((Control)button1).add_Click((EventHandler)cmdDownloadFutureAll_Click);
			((Control)cmdEditUserMinorPlanets).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdEditUserMinorPlanets).set_Location(new Point(217, 249));
			((Control)cmdEditUserMinorPlanets).set_Name("cmdEditUserMinorPlanets");
			((Control)cmdEditUserMinorPlanets).set_Size(new Size(136, 38));
			((Control)cmdEditUserMinorPlanets).set_TabIndex(3);
			((Control)cmdEditUserMinorPlanets).set_Text("Edit the 'User' file of minor planets");
			((ButtonBase)cmdEditUserMinorPlanets).set_UseVisualStyleBackColor(true);
			((Control)cmdEditUserMinorPlanets).add_Click((EventHandler)cmdEditUserMinorPlanets_Click);
			((Control)cmdEditUserStar).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdEditUserStar).set_Location(new Point(217, 297));
			((Control)cmdEditUserStar).set_Name("cmdEditUserStar");
			((Control)cmdEditUserStar).set_Size(new Size(136, 38));
			((Control)cmdEditUserStar).set_TabIndex(2);
			((Control)cmdEditUserStar).set_Text("Edit the coordinates of a 'User' star ");
			((ButtonBase)cmdEditUserStar).set_UseVisualStyleBackColor(true);
			((Control)cmdEditUserStar).add_Click((EventHandler)cmdEditUserStar_Click);
			((Control)cmdAsteroidSearch).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdAsteroidSearch).set_Location(new Point(99, 84));
			((Control)cmdAsteroidSearch).set_Name("cmdAsteroidSearch");
			((Control)cmdAsteroidSearch).set_Size(new Size(165, 84));
			((Control)cmdAsteroidSearch).set_TabIndex(0);
			((Control)cmdAsteroidSearch).set_Text("Search for, \r\n&& list, \r\noccultations\r\n");
			toolTip1.SetToolTip((Control)(object)cmdAsteroidSearch, "Search for occultations");
			((ButtonBase)cmdAsteroidSearch).set_UseVisualStyleBackColor(true);
			((Control)cmdAsteroidSearch).add_Click((EventHandler)cmdAsteroidSearch_Click);
			((Control)cmdSatelliteEvents).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdSatelliteEvents).set_Location(new Point(198, 119));
			((Control)cmdSatelliteEvents).set_Name("cmdSatelliteEvents");
			((Control)cmdSatelliteEvents).set_Size(new Size(185, 111));
			((Control)cmdSatelliteEvents).set_TabIndex(15);
			((Control)cmdSatelliteEvents).set_Text("PLANETARY eclipses, occultations && transits\r\n-\r\nJupiter, Saturn, Uranus && Neptune ");
			((ButtonBase)cmdSatelliteEvents).set_UseVisualStyleBackColor(true);
			((Control)cmdSatelliteEvents).add_Click((EventHandler)cmdSatelliteEvents_Click);
			((Control)grpEphemeris).get_Controls().Add((Control)(object)CmdSidereal);
			((Control)grpEphemeris).get_Controls().Add((Control)(object)cmdConversions);
			((Control)grpEphemeris).get_Controls().Add((Control)(object)cmdCalendar);
			((Control)grpEphemeris).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpEphemeris).set_Location(new Point(70, 413));
			((Control)grpEphemeris).set_Name("grpEphemeris");
			((Control)grpEphemeris).set_Size(new Size(440, 88));
			((Control)grpEphemeris).set_TabIndex(5);
			grpEphemeris.set_TabStop(false);
			((Control)grpEphemeris).set_Text("Calendar, dates and conversions");
			((Control)CmdSidereal).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)CmdSidereal).set_Location(new Point(164, 21));
			((Control)CmdSidereal).set_Name("CmdSidereal");
			((Control)CmdSidereal).set_Size(new Size(111, 61));
			((Control)CmdSidereal).set_TabIndex(1);
			((Control)CmdSidereal).set_Text("Julian day no.,\r\nSidereal time,\r\nSolar transit,\r\nEcliptic");
			toolTip1.SetToolTip((Control)(object)CmdSidereal, "Generate a table of JD, Sidereal time,\r\n solar transit, and true obliquity \r\nof the ecliptic");
			((ButtonBase)CmdSidereal).set_UseVisualStyleBackColor(true);
			((Control)CmdSidereal).add_Click((EventHandler)CmdSidereal_Click);
			((Control)cmdComets).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdComets).set_Location(new Point(205, 27));
			((Control)cmdComets).set_Name("cmdComets");
			((Control)cmdComets).set_Size(new Size(85, 51));
			((Control)cmdComets).set_TabIndex(2);
			((Control)cmdComets).set_Text("Comets");
			toolTip1.SetToolTip((Control)(object)cmdComets, "Generate ephemerides of comets");
			((ButtonBase)cmdComets).set_UseVisualStyleBackColor(true);
			((Control)cmdComets).add_Click((EventHandler)cmdComets_Click);
			((Control)cmdMeridians).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdMeridians).set_Location(new Point(313, 23));
			((Control)cmdMeridians).set_Name("cmdMeridians");
			((Control)cmdMeridians).set_Size(new Size(111, 54));
			((Control)cmdMeridians).set_TabIndex(2);
			((Control)cmdMeridians).set_Text("Central meridians of Mars, Jupiter and Saturn");
			toolTip1.SetToolTip((Control)(object)cmdMeridians, "Generate an ephemeris of the \r\ncentral meridian of Mars,\r\n Jupiter or Saturn");
			((ButtonBase)cmdMeridians).set_UseVisualStyleBackColor(true);
			((Control)cmdMeridians).add_Click((EventHandler)cmdMeridians_Click);
			((Control)cmdMoonRiseSet).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdMoonRiseSet).set_Location(new Point(164, 24));
			((Control)cmdMoonRiseSet).set_Name("cmdMoonRiseSet");
			((Control)cmdMoonRiseSet).set_Size(new Size(111, 54));
			((Control)cmdMoonRiseSet).set_TabIndex(1);
			((Control)cmdMoonRiseSet).set_Text("Moonrise and Moonset");
			((ButtonBase)cmdMoonRiseSet).set_UseVisualStyleBackColor(true);
			((Control)cmdMoonRiseSet).add_Click((EventHandler)cmdMoonRiseSet_Click);
			((Control)cmdConvertAstorb).set_Location(new Point(227, 133));
			((Control)cmdConvertAstorb).set_Name("cmdConvertAstorb");
			((Control)cmdConvertAstorb).set_Size(new Size(128, 40));
			((Control)cmdConvertAstorb).set_TabIndex(5);
			((Control)cmdConvertAstorb).set_Text("Convert Astorb\r\nMPCOrb && AstDyS-2");
			toolTip1.SetToolTip((Control)(object)cmdConvertAstorb, "Tool to convert various sources of \r\nasteroid orbital elements into a single\r\nconsistent format for use in Occule");
			((ButtonBase)cmdConvertAstorb).set_UseVisualStyleBackColor(true);
			((Control)cmdConvertAstorb).add_Click((EventHandler)cmdConvertAstorb_Click);
			((Control)menuStrip1).set_AutoSize(false);
			((ToolStrip)menuStrip1).set_BackColor(SystemColors.Control);
			((ToolStrip)menuStrip1).set_ImageScalingSize(new Size(20, 20));
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[7]
			{
				(ToolStripItem)weatherToolStripMenuItem,
				(ToolStripItem)virtualDubTimerToolStripMenuItem,
				(ToolStripItem)checkForUpdatesToolStripMenuItem,
				(ToolStripItem)cascadeFormsToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem,
				(ToolStripItem)testsToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			menuStrip1.set_ShowItemToolTips(true);
			((Control)menuStrip1).set_Size(new Size(750, 28));
			((Control)menuStrip1).set_TabIndex(19);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)weatherToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)dayWeatherForecastForhomeToolStripMenuItem,
				(ToolStripItem)regionalCloudMapPredictionsToolStripMenuItem
			});
			((ToolStripItem)weatherToolStripMenuItem).set_Image((Image)Occult.Properties.Resources.CLOUD);
			((ToolStripItem)weatherToolStripMenuItem).set_Name("weatherToolStripMenuItem");
			((ToolStripItem)weatherToolStripMenuItem).set_Size(new Size(92, 24));
			((ToolStripItem)weatherToolStripMenuItem).set_Text("Weather...");
			((ToolStripItem)weatherToolStripMenuItem).set_ToolTipText("7Timer weather predictions");
			((ToolStripItem)dayWeatherForecastForhomeToolStripMenuItem).set_Name("dayWeatherForecastForhomeToolStripMenuItem");
			((ToolStripItem)dayWeatherForecastForhomeToolStripMenuItem).set_Size(new Size(252, 22));
			((ToolStripItem)dayWeatherForecastForhomeToolStripMenuItem).set_Text("3-day weather forecast for 'home'");
			((ToolStripItem)dayWeatherForecastForhomeToolStripMenuItem).add_Click((EventHandler)dayWeatherForecastForhomeToolStripMenuItem_Click);
			((ToolStripItem)regionalCloudMapPredictionsToolStripMenuItem).set_Name("regionalCloudMapPredictionsToolStripMenuItem");
			((ToolStripItem)regionalCloudMapPredictionsToolStripMenuItem).set_Size(new Size(252, 22));
			((ToolStripItem)regionalCloudMapPredictionsToolStripMenuItem).set_Text("Regional 16-day forecasts");
			((ToolStripItem)regionalCloudMapPredictionsToolStripMenuItem).add_Click((EventHandler)regionalCloudMapPredictionsToolStripMenuItem_Click);
			((ToolStripItem)virtualDubTimerToolStripMenuItem).set_Image((Image)Occult.Properties.Resources.CLOCK02);
			((ToolStripItem)virtualDubTimerToolStripMenuItem).set_Name("virtualDubTimerToolStripMenuItem");
			((ToolStripItem)virtualDubTimerToolStripMenuItem).set_Size(new Size(138, 24));
			((ToolStripItem)virtualDubTimerToolStripMenuItem).set_Text("Recording Timer    ");
			((ToolStripItem)virtualDubTimerToolStripMenuItem).set_ToolTipText("For automatic recording of video");
			((ToolStripItem)virtualDubTimerToolStripMenuItem).add_Click((EventHandler)virtualDubTimerToolStripMenuItem_Click);
			((ToolStripItem)checkForUpdatesToolStripMenuItem).set_BackColor(SystemColors.Control);
			((ToolStripDropDownItem)checkForUpdatesToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[15]
			{
				(ToolStripItem)listChangesSinceCurrentVersionToolStripMenuItem,
				(ToolStripItem)checkForUpdatesToolStripMenuItem1,
				(ToolStripItem)checkForDataUpdatesToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)updateWithLatestVersionToolStripMenuItem,
				(ToolStripItem)updateProgramWithLatestBetaVersionifAnyToolStripMenuItem,
				(ToolStripItem)toolStripSeparator9,
				(ToolStripItem)toolStripSeparator3,
				(ToolStripItem)previousVersionsAreInUpdatesToolStripMenuItem,
				(ToolStripItem)toolStripSeparator8,
				(ToolStripItem)restartOccultWatcherToolStripMenuItem,
				(ToolStripItem)toolStripSeparator4,
				(ToolStripItem)helpWithUpdatesToolStripMenuItem,
				(ToolStripItem)toolStripSeparator7
			});
			((ToolStripItem)checkForUpdatesToolStripMenuItem).set_Image((Image)Occult.Properties.Resources.genericInternet);
			((ToolStripItem)checkForUpdatesToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)checkForUpdatesToolStripMenuItem).set_Name("checkForUpdatesToolStripMenuItem");
			((ToolStripItem)checkForUpdatesToolStripMenuItem).set_Size(new Size(100, 24));
			((ToolStripItem)checkForUpdatesToolStripMenuItem).set_Text("Updates...   ");
			((ToolStripItem)checkForUpdatesToolStripMenuItem).set_ToolTipText("Check for program and data updates");
			((ToolStripItem)listChangesSinceCurrentVersionToolStripMenuItem).set_Image((Image)Occult.Properties.Resources.book_reportHS);
			((ToolStripItem)listChangesSinceCurrentVersionToolStripMenuItem).set_ImageTransparentColor(Color.Black);
			((ToolStripItem)listChangesSinceCurrentVersionToolStripMenuItem).set_Name("listChangesSinceCurrentVersionToolStripMenuItem");
			((ToolStripItem)listChangesSinceCurrentVersionToolStripMenuItem).set_Size(new Size(329, 22));
			((ToolStripItem)listChangesSinceCurrentVersionToolStripMenuItem).set_Text("List version changes");
			((ToolStripItem)listChangesSinceCurrentVersionToolStripMenuItem).add_Click((EventHandler)listChangesSinceCurrentVersionToolStripMenuItem_Click);
			((ToolStripItem)checkForUpdatesToolStripMenuItem1).set_Image((Image)Occult.Properties.Resources.SearchWebHS);
			((ToolStripItem)checkForUpdatesToolStripMenuItem1).set_ImageTransparentColor(Color.Black);
			((ToolStripItem)checkForUpdatesToolStripMenuItem1).set_Name("checkForUpdatesToolStripMenuItem1");
			((ToolStripItem)checkForUpdatesToolStripMenuItem1).set_Size(new Size(329, 22));
			((ToolStripItem)checkForUpdatesToolStripMenuItem1).set_Text("Check for program updates");
			((ToolStripItem)checkForUpdatesToolStripMenuItem1).add_Click((EventHandler)checkForUpdatesToolStripMenuItem1_Click);
			((ToolStripItem)checkForDataUpdatesToolStripMenuItem).set_Name("checkForDataUpdatesToolStripMenuItem");
			((ToolStripItem)checkForDataUpdatesToolStripMenuItem).set_Size(new Size(329, 22));
			((ToolStripItem)checkForDataUpdatesToolStripMenuItem).set_Text("Check for data updates");
			((ToolStripItem)checkForDataUpdatesToolStripMenuItem).add_Click((EventHandler)checkForDataUpdatesToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(326, 6));
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(326, 6));
			((ToolStripItem)updateWithLatestVersionToolStripMenuItem).set_BackColor(Color.Aquamarine);
			((ToolStripItem)updateWithLatestVersionToolStripMenuItem).set_Image((Image)Occult.Properties.Resources.DoubleRightArrowHS);
			((ToolStripItem)updateWithLatestVersionToolStripMenuItem).set_ImageTransparentColor(Color.Black);
			((ToolStripItem)updateWithLatestVersionToolStripMenuItem).set_Name("updateWithLatestVersionToolStripMenuItem");
			((ToolStripItem)updateWithLatestVersionToolStripMenuItem).set_Size(new Size(329, 22));
			((ToolStripItem)updateWithLatestVersionToolStripMenuItem).set_Text("Force program update with latest release version");
			((ToolStripItem)updateWithLatestVersionToolStripMenuItem).add_Click((EventHandler)updateWithLatestVersionToolStripMenuItem_Click);
			((ToolStripItem)updateProgramWithLatestBetaVersionifAnyToolStripMenuItem).set_BackColor(Color.Aquamarine);
			((ToolStripItem)updateProgramWithLatestBetaVersionifAnyToolStripMenuItem).set_Image((Image)Occult.Properties.Resources.DataContainer_NewRecordHS);
			((ToolStripItem)updateProgramWithLatestBetaVersionifAnyToolStripMenuItem).set_ImageTransparentColor(Color.Black);
			((ToolStripItem)updateProgramWithLatestBetaVersionifAnyToolStripMenuItem).set_Name("updateProgramWithLatestBetaVersionifAnyToolStripMenuItem");
			((ToolStripItem)updateProgramWithLatestBetaVersionifAnyToolStripMenuItem).set_Size(new Size(329, 22));
			((ToolStripItem)updateProgramWithLatestBetaVersionifAnyToolStripMenuItem).set_Text("Update program with latest beta version (if any)");
			((ToolStripItem)updateProgramWithLatestBetaVersionifAnyToolStripMenuItem).add_Click((EventHandler)updateProgramWithLatestBetaVersionifAnyToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator9).set_Name("toolStripSeparator9");
			((ToolStripItem)toolStripSeparator9).set_Size(new Size(326, 6));
			((ToolStripItem)toolStripSeparator3).set_Name("toolStripSeparator3");
			((ToolStripItem)toolStripSeparator3).set_Size(new Size(326, 6));
			((ToolStripItem)previousVersionsAreInUpdatesToolStripMenuItem).set_BackColor(Color.PowderBlue);
			((ToolStripItem)previousVersionsAreInUpdatesToolStripMenuItem).set_Name("previousVersionsAreInUpdatesToolStripMenuItem");
			((ToolStripItem)previousVersionsAreInUpdatesToolStripMenuItem).set_Size(new Size(329, 22));
			((ToolStripItem)previousVersionsAreInUpdatesToolStripMenuItem).set_Text("0 Previous versions are in subdirectory 'Updates'");
			((ToolStripItem)toolStripSeparator8).set_Name("toolStripSeparator8");
			((ToolStripItem)toolStripSeparator8).set_Size(new Size(326, 6));
			((ToolStripItem)restartOccultWatcherToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)restartOccultWatcherToolStripMenuItem).set_Image((Image)Occult.Properties.Resources.OWIcon2);
			((ToolStripItem)restartOccultWatcherToolStripMenuItem).set_Name("restartOccultWatcherToolStripMenuItem");
			((ToolStripItem)restartOccultWatcherToolStripMenuItem).set_Size(new Size(329, 22));
			((ToolStripItem)restartOccultWatcherToolStripMenuItem).set_Text("Restart OccultWatcher");
			((ToolStripItem)restartOccultWatcherToolStripMenuItem).add_Click((EventHandler)restartOccultWatcherToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator4).set_Name("toolStripSeparator4");
			((ToolStripItem)toolStripSeparator4).set_Size(new Size(326, 6));
			((ToolStripItem)helpWithUpdatesToolStripMenuItem).set_Image((Image)Occult.Properties.Resources.help);
			((ToolStripItem)helpWithUpdatesToolStripMenuItem).set_Name("helpWithUpdatesToolStripMenuItem");
			((ToolStripItem)helpWithUpdatesToolStripMenuItem).set_Size(new Size(329, 22));
			((ToolStripItem)helpWithUpdatesToolStripMenuItem).set_Text("Help with Updates");
			((ToolStripItem)helpWithUpdatesToolStripMenuItem).add_Click((EventHandler)helpWithUpdatesToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator7).set_Name("toolStripSeparator7");
			((ToolStripItem)toolStripSeparator7).set_Size(new Size(326, 6));
			((ToolStripItem)cascadeFormsToolStripMenuItem).set_Image((Image)Occult.Properties.Resources.ARW11SE);
			((ToolStripItem)cascadeFormsToolStripMenuItem).set_Name("cascadeFormsToolStripMenuItem");
			((ToolStripItem)cascadeFormsToolStripMenuItem).set_Size(new Size(126, 24));
			((ToolStripItem)cascadeFormsToolStripMenuItem).set_Text("Cascade forms   ");
			((ToolStripItem)cascadeFormsToolStripMenuItem).set_ToolTipText("Cascades all opened forms ");
			((ToolStripItem)cascadeFormsToolStripMenuItem).add_Click((EventHandler)cascadeFormsToolStripMenuItem_Click);
			((ToolStripDropDownItem)helpToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)helpToolStripMenuItem1,
				(ToolStripItem)toolStripSeparator5,
				(ToolStripItem)toolStripSeparator6,
				(ToolStripItem)helpNotWorkingToolStripMenuItem
			});
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Occult.Properties.Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(82, 24));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help      ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem1).set_Name("helpToolStripMenuItem1");
			((ToolStripItem)helpToolStripMenuItem1).set_Size(new Size(185, 22));
			((ToolStripItem)helpToolStripMenuItem1).set_Text("Help");
			((ToolStripItem)helpToolStripMenuItem1).add_Click((EventHandler)helpToolStripMenuItem1_Click);
			((ToolStripItem)toolStripSeparator5).set_Name("toolStripSeparator5");
			((ToolStripItem)toolStripSeparator5).set_Size(new Size(182, 6));
			((ToolStripItem)toolStripSeparator6).set_Name("toolStripSeparator6");
			((ToolStripItem)toolStripSeparator6).set_Size(new Size(182, 6));
			((ToolStripItem)helpNotWorkingToolStripMenuItem).set_Name("helpNotWorkingToolStripMenuItem");
			((ToolStripItem)helpNotWorkingToolStripMenuItem).set_Size(new Size(185, 22));
			((ToolStripItem)helpNotWorkingToolStripMenuItem).set_Text("if Help not working...");
			((ToolStripItem)helpNotWorkingToolStripMenuItem).add_Click((EventHandler)helpNotWorkingToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Occult.Properties.Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(73, 24));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit     ");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((ToolStripDropDownItem)testsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)backUpSourceCodeToolStripMenuItem,
				(ToolStripItem)runCurrentTestRoutineToolStripMenuItem,
				(ToolStripItem)runExtrnalAccessTestRoutineToolStripMenuItem,
				(ToolStripItem)unzipToolStripMenuItem
			});
			((ToolStripItem)testsToolStripMenuItem).set_Font(new Font("Segoe UI", 9f));
			((ToolStripItem)testsToolStripMenuItem).set_Name("testsToolStripMenuItem");
			((ToolStripItem)testsToolStripMenuItem).set_Size(new Size(129, 24));
			((ToolStripItem)testsToolStripMenuItem).set_Text("Backup, && Devt Tests");
			((ToolStripItem)testsToolStripMenuItem).add_Click((EventHandler)testsToolStripMenuItem_Click);
			((ToolStripItem)backUpSourceCodeToolStripMenuItem).set_BackColor(Color.Cyan);
			((ToolStripItem)backUpSourceCodeToolStripMenuItem).set_Font(new Font("Segoe UI", 10f, FontStyle.Bold));
			((ToolStripItem)backUpSourceCodeToolStripMenuItem).set_ForeColor(Color.DarkRed);
			((ToolStripItem)backUpSourceCodeToolStripMenuItem).set_Name("backUpSourceCodeToolStripMenuItem");
			((ToolStripItem)backUpSourceCodeToolStripMenuItem).set_Size(new Size(242, 24));
			((ToolStripItem)backUpSourceCodeToolStripMenuItem).set_Text("BackUp source code");
			((ToolStripItem)backUpSourceCodeToolStripMenuItem).add_Click((EventHandler)backUpSourceCodeToolStripMenuItem_Click);
			((ToolStripItem)runCurrentTestRoutineToolStripMenuItem).set_Name("runCurrentTestRoutineToolStripMenuItem");
			((ToolStripItem)runCurrentTestRoutineToolStripMenuItem).set_Size(new Size(242, 24));
			((ToolStripItem)runCurrentTestRoutineToolStripMenuItem).set_Text("Run current test routine");
			((ToolStripItem)runCurrentTestRoutineToolStripMenuItem).add_Click((EventHandler)runCurrentTestRoutineToolStripMenuItem_Click);
			((ToolStripItem)runExtrnalAccessTestRoutineToolStripMenuItem).set_Name("runExtrnalAccessTestRoutineToolStripMenuItem");
			((ToolStripItem)runExtrnalAccessTestRoutineToolStripMenuItem).set_Size(new Size(242, 24));
			((ToolStripItem)runExtrnalAccessTestRoutineToolStripMenuItem).set_Text("Run External Access test routine");
			((ToolStripItem)runExtrnalAccessTestRoutineToolStripMenuItem).add_Click((EventHandler)runExtrnalAccessTestRoutineToolStripMenuItem_Click);
			((ToolStripItem)unzipToolStripMenuItem).set_BackColor(SystemColors.Control);
			((ToolStripItem)unzipToolStripMenuItem).set_Name("unzipToolStripMenuItem");
			((ToolStripItem)unzipToolStripMenuItem).set_Size(new Size(242, 24));
			((ToolStripItem)unzipToolStripMenuItem).set_Text("Unzip");
			((ToolStripItem)unzipToolStripMenuItem).add_Click((EventHandler)unzipToolStripMenuItem_Click);
			MainToolStrip.set_BackColor(SystemColors.Control);
			((Control)MainToolStrip).set_Dock((DockStyle)0);
			((Control)MainToolStrip).set_Font(new Font("Arial Narrow", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			MainToolStrip.set_GripStyle((ToolStripGripStyle)0);
			MainToolStrip.set_ImageScalingSize(new Size(48, 48));
			MainToolStrip.get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[10]
			{
				(ToolStripItem)TS_AsteroidPredict,
				(ToolStripItem)TS_AsteroidReduce,
				(ToolStripItem)TS_Eclipses,
				(ToolStripItem)TS_Ephemeris,
				(ToolStripItem)TS_LunarPredict,
				(ToolStripItem)TS_LunarObservations,
				(ToolStripItem)TS_Satellites,
				(ToolStripItem)TS_Maintenance,
				(ToolStripItem)toolStripUpdates,
				(ToolStripItem)toolStripDataUpdates
			});
			MainToolStrip.set_LayoutStyle((ToolStripLayoutStyle)2);
			((Control)MainToolStrip).set_Location(new Point(0, 28));
			((Control)MainToolStrip).set_Name("MainToolStrip");
			((Control)MainToolStrip).set_Padding(new Padding(0, 0, 3, 0));
			((Control)MainToolStrip).set_Size(new Size(163, 610));
			MainToolStrip.set_Stretch(true);
			((Control)MainToolStrip).set_TabIndex(21);
			((Control)MainToolStrip).set_Text("\\\\");
			((ToolStripItem)TS_AsteroidPredict).set_AutoSize(false);
			((ToolStripItem)TS_AsteroidPredict).set_Font(new Font("Times New Roman", 12f, FontStyle.Bold));
			((ToolStripItem)TS_AsteroidPredict).set_Image((Image)Occult.Properties.Resources.Prediction_graphic_e1);
			((ToolStripItem)TS_AsteroidPredict).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)TS_AsteroidPredict).set_Margin(new Padding(0, 1, 0, 9));
			((ToolStripItem)TS_AsteroidPredict).set_Name("TS_AsteroidPredict");
			((ToolStripItem)TS_AsteroidPredict).set_Size(new Size(160, 60));
			((ToolStripItem)TS_AsteroidPredict).set_Text("Asteroid predictions");
			((ToolStripItem)TS_AsteroidPredict).set_TextAlign(ContentAlignment.BottomLeft);
			((ToolStripItem)TS_AsteroidPredict).set_TextImageRelation((TextImageRelation)1);
			((ToolStripItem)TS_AsteroidPredict).add_Click((EventHandler)TS_AsteroidPredict_Click);
			((ToolStripItem)TS_AsteroidReduce).set_AutoSize(false);
			((ToolStripItem)TS_AsteroidReduce).set_Font(new Font("Times New Roman", 12f, FontStyle.Bold));
			((ToolStripItem)TS_AsteroidReduce).set_Image((Image)Occult.Properties.Resources.Eros);
			((ToolStripItem)TS_AsteroidReduce).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)TS_AsteroidReduce).set_Margin(new Padding(0, 1, 0, 9));
			((ToolStripItem)TS_AsteroidReduce).set_Name("TS_AsteroidReduce");
			((ToolStripItem)TS_AsteroidReduce).set_Size(new Size(160, 60));
			((ToolStripItem)TS_AsteroidReduce).set_Text("Asteroid observations");
			((ToolStripItem)TS_AsteroidReduce).set_TextAlign(ContentAlignment.BottomLeft);
			((ToolStripItem)TS_AsteroidReduce).set_TextImageRelation((TextImageRelation)1);
			((ToolStripItem)TS_AsteroidReduce).add_Click((EventHandler)TS_AsteroidReduce_Click);
			((ToolStripItem)TS_Eclipses).set_AutoSize(false);
			((ToolStripItem)TS_Eclipses).set_Font(new Font("Times New Roman", 12f, FontStyle.Bold));
			((ToolStripItem)TS_Eclipses).set_Image((Image)Occult.Properties.Resources._2002_Dec_4_Totality_small);
			((ToolStripItem)TS_Eclipses).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)TS_Eclipses).set_Margin(new Padding(0, 1, 0, 9));
			((ToolStripItem)TS_Eclipses).set_Name("TS_Eclipses");
			((ToolStripItem)TS_Eclipses).set_Size(new Size(160, 60));
			((ToolStripItem)TS_Eclipses).set_Text("Eclipses && transits");
			((ToolStripItem)TS_Eclipses).set_TextAlign(ContentAlignment.BottomLeft);
			((ToolStripItem)TS_Eclipses).set_TextImageRelation((TextImageRelation)1);
			((ToolStripItem)TS_Eclipses).add_Click((EventHandler)TS_Eclipses_Click);
			((ToolStripItem)TS_Ephemeris).set_AutoSize(false);
			((ToolStripItem)TS_Ephemeris).set_Font(new Font("Times New Roman", 12f, FontStyle.Bold));
			((ToolStripItem)TS_Ephemeris).set_Image((Image)Occult.Properties.Resources.NOTE04);
			((ToolStripItem)TS_Ephemeris).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)TS_Ephemeris).set_Margin(new Padding(0, 1, 0, 9));
			((ToolStripItem)TS_Ephemeris).set_Name("TS_Ephemeris");
			((ToolStripItem)TS_Ephemeris).set_Size(new Size(160, 60));
			((ToolStripItem)TS_Ephemeris).set_Text("Ephemerides");
			((ToolStripItem)TS_Ephemeris).set_TextAlign(ContentAlignment.MiddleLeft);
			((ToolStripItem)TS_Ephemeris).set_TextImageRelation((TextImageRelation)1);
			((ToolStripItem)TS_Ephemeris).add_Click((EventHandler)TS_Ephemeris_Click);
			((ToolStripItem)TS_LunarPredict).set_AutoSize(false);
			((ToolStripItem)TS_LunarPredict).set_Font(new Font("Times New Roman", 12f, FontStyle.Bold));
			((ToolStripItem)TS_LunarPredict).set_Image((Image)Occult.Properties.Resources.OCCULT);
			((ToolStripItem)TS_LunarPredict).set_ImageScaling((ToolStripItemImageScaling)0);
			((ToolStripItem)TS_LunarPredict).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)TS_LunarPredict).set_Margin(new Padding(0, 1, 0, 9));
			((ToolStripItem)TS_LunarPredict).set_Name("TS_LunarPredict");
			((ToolStripItem)TS_LunarPredict).set_Size(new Size(160, 60));
			((ToolStripItem)TS_LunarPredict).set_Text("Lunar predictions");
			((ToolStripItem)TS_LunarPredict).set_TextAlign(ContentAlignment.BottomLeft);
			((ToolStripItem)TS_LunarPredict).set_TextImageRelation((TextImageRelation)1);
			((ToolStripItem)TS_LunarPredict).add_Click((EventHandler)TS_LunarPredict_Click);
			((ToolStripItem)TS_LunarObservations).set_AutoSize(false);
			((ToolStripItem)TS_LunarObservations).set_Font(new Font("Times New Roman", 12f, FontStyle.Bold));
			((ToolStripItem)TS_LunarObservations).set_Image((Image)Occult.Properties.Resources.Graze5);
			((ToolStripItem)TS_LunarObservations).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)TS_LunarObservations).set_Margin(new Padding(0, 1, 0, 9));
			((ToolStripItem)TS_LunarObservations).set_Name("TS_LunarObservations");
			((ToolStripItem)TS_LunarObservations).set_Size(new Size(160, 60));
			((ToolStripItem)TS_LunarObservations).set_Text("Lunar observations");
			((ToolStripItem)TS_LunarObservations).set_TextAlign(ContentAlignment.BottomLeft);
			((ToolStripItem)TS_LunarObservations).set_TextImageRelation((TextImageRelation)1);
			((ToolStripItem)TS_LunarObservations).add_Click((EventHandler)TS_LunarObservations_Click);
			((ToolStripItem)TS_Satellites).set_AutoSize(false);
			((ToolStripItem)TS_Satellites).set_Font(new Font("Times New Roman", 12f, FontStyle.Bold));
			((ToolStripItem)TS_Satellites).set_Image((Image)Occult.Properties.Resources.SaturnSystem);
			((ToolStripItem)TS_Satellites).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)TS_Satellites).set_Margin(new Padding(0, 1, 0, 9));
			((ToolStripItem)TS_Satellites).set_Name("TS_Satellites");
			((ToolStripItem)TS_Satellites).set_Size(new Size(160, 60));
			((ToolStripItem)TS_Satellites).set_Text("Satellite phenomena");
			((ToolStripItem)TS_Satellites).set_TextAlign(ContentAlignment.BottomLeft);
			((ToolStripItem)TS_Satellites).set_TextImageRelation((TextImageRelation)1);
			((ToolStripItem)TS_Satellites).add_Click((EventHandler)TS_Satellites_Click);
			((ToolStripItem)TS_Maintenance).set_AutoSize(false);
			((ToolStripItem)TS_Maintenance).set_Font(new Font("Times New Roman", 12f, FontStyle.Bold));
			((ToolStripItem)TS_Maintenance).set_Image((Image)Occult.Properties.Resources.WRENCH);
			((ToolStripItem)TS_Maintenance).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)TS_Maintenance).set_Name("TS_Maintenance");
			((ToolStripItem)TS_Maintenance).set_Size(new Size(160, 60));
			((ToolStripItem)TS_Maintenance).set_Text("Maintenance");
			((ToolStripItem)TS_Maintenance).set_TextAlign(ContentAlignment.BottomLeft);
			((ToolStripItem)TS_Maintenance).set_TextImageRelation((TextImageRelation)1);
			((ToolStripItem)TS_Maintenance).add_Click((EventHandler)TS_Maintenance_Click);
			((ToolStripItem)toolStripUpdates).set_BackColor(SystemColors.Control);
			((ToolStripItem)toolStripUpdates).set_Font(new Font("Times New Roman", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			toolStripUpdates.set_IsLink(true);
			toolStripUpdates.set_LinkColor(Color.FromArgb(0, 0, 255));
			((ToolStripItem)toolStripUpdates).set_Margin(new Padding(0, 12, 0, 0));
			((ToolStripItem)toolStripUpdates).set_Name("toolStripUpdates");
			((ToolStripItem)toolStripUpdates).set_Padding(new Padding(0, 15, 0, 0));
			((ToolStripItem)toolStripUpdates).set_Size(new Size(159, 30));
			((ToolStripItem)toolStripUpdates).set_Text("No update available");
			((ToolStripItem)toolStripUpdates).add_Click((EventHandler)toolStripUpdates_Click);
			((ToolStripItem)toolStripUpdates).add_MouseEnter((EventHandler)toolStripUpdates_MouseEnter);
			((ToolStripItem)toolStripUpdates).add_MouseLeave((EventHandler)toolStripUpdates_MouseLeave);
			((ToolStripItem)toolStripDataUpdates).set_BackColor(SystemColors.Control);
			((ToolStripItem)toolStripDataUpdates).set_Font(new Font("Times New Roman", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			toolStripDataUpdates.set_IsLink(true);
			toolStripDataUpdates.set_LinkColor(Color.FromArgb(0, 0, 255));
			((ToolStripItem)toolStripDataUpdates).set_Margin(new Padding(0, -4, 0, 2));
			((ToolStripItem)toolStripDataUpdates).set_Name("toolStripDataUpdates");
			((ToolStripItem)toolStripDataUpdates).set_Size(new Size(159, 15));
			((ToolStripItem)toolStripDataUpdates).set_Text("Data updates are available");
			((ToolStripItem)toolStripDataUpdates).add_Click((EventHandler)toolStripDataUpdates_Click_1);
			((Control)PanelAsteroidPredictions).set_BackColor(SystemColors.Control);
			((Control)PanelAsteroidPredictions).get_Controls().Add((Control)(object)cmdAsteroidTaxonomy);
			((Control)PanelAsteroidPredictions).get_Controls().Add((Control)(object)cmdLightCurveSimulator);
			((Control)PanelAsteroidPredictions).get_Controls().Add((Control)(object)cmdMutualAsteroidals);
			((Control)PanelAsteroidPredictions).get_Controls().Add((Control)(object)cmdSelectDE);
			((Control)PanelAsteroidPredictions).get_Controls().Add((Control)(object)cmdListAndDisplayAsteroidals);
			((Control)PanelAsteroidPredictions).get_Controls().Add((Control)(object)groupBox8);
			((Control)PanelAsteroidPredictions).get_Controls().Add((Control)(object)cmdConvertAstorb_MPCOrb);
			((Control)PanelAsteroidPredictions).get_Controls().Add((Control)(object)label3);
			((Control)PanelAsteroidPredictions).get_Controls().Add((Control)(object)label2);
			((Control)PanelAsteroidPredictions).get_Controls().Add((Control)(object)label9);
			((Control)PanelAsteroidPredictions).get_Controls().Add((Control)(object)cmdEditUserMinorPlanets);
			((Control)PanelAsteroidPredictions).get_Controls().Add((Control)(object)cmdAsteroidSearch);
			((Control)PanelAsteroidPredictions).get_Controls().Add((Control)(object)cmdEditUserStar);
			((Control)PanelAsteroidPredictions).get_Controls().Add((Control)(object)label7);
			((Control)PanelAsteroidPredictions).set_Location(new Point(620, 34));
			((Control)PanelAsteroidPredictions).set_Name("PanelAsteroidPredictions");
			((Control)PanelAsteroidPredictions).set_Size(new Size(600, 650));
			((Control)PanelAsteroidPredictions).set_TabIndex(0);
			((Control)cmdAsteroidTaxonomy).set_Location(new Point(217, 345));
			((Control)cmdAsteroidTaxonomy).set_Name("cmdAsteroidTaxonomy");
			((Control)cmdAsteroidTaxonomy).set_Size(new Size(136, 48));
			((Control)cmdAsteroidTaxonomy).set_TabIndex(27);
			((Control)cmdAsteroidTaxonomy).set_Text("Update file of Asteroid\r\ntaxonomy, including\r\nbinary asteroids");
			toolTip1.SetToolTip((Control)(object)cmdAsteroidTaxonomy, "Updates the file of asteroids by\r\ntaxanomic class");
			((ButtonBase)cmdAsteroidTaxonomy).set_UseVisualStyleBackColor(true);
			((Control)cmdAsteroidTaxonomy).add_Click((EventHandler)cmdAsteroidTaxonomy_Click);
			((Control)cmdLightCurveSimulator).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdLightCurveSimulator).set_Location(new Point(41, 345));
			((Control)cmdLightCurveSimulator).set_Name("cmdLightCurveSimulator");
			((Control)cmdLightCurveSimulator).set_Size(new Size(136, 48));
			((Control)cmdLightCurveSimulator).set_TabIndex(22);
			((Control)cmdLightCurveSimulator).set_Text("Prediction light curve \r\nfor large diameter stars");
			((ButtonBase)cmdLightCurveSimulator).set_UseVisualStyleBackColor(true);
			((Control)cmdLightCurveSimulator).add_Click((EventHandler)cmdLightCurveSimulator_Click);
			((Control)cmdMutualAsteroidals).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdMutualAsteroidals).set_Location(new Point(393, 250));
			((Control)cmdMutualAsteroidals).set_Name("cmdMutualAsteroidals");
			((Control)cmdMutualAsteroidals).set_Size(new Size(136, 38));
			((Control)cmdMutualAsteroidals).set_TabIndex(20);
			((Control)cmdMutualAsteroidals).set_Text("Mutual Asteroidal Occultations");
			((ButtonBase)cmdMutualAsteroidals).set_UseVisualStyleBackColor(true);
			((Control)cmdMutualAsteroidals).add_Click((EventHandler)cmdMutualAsteroidals_Click);
			((Control)cmdSelectDE).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSelectDE).set_Location(new Point(41, 297));
			((Control)cmdSelectDE).set_Name("cmdSelectDE");
			((Control)cmdSelectDE).set_Size(new Size(136, 38));
			((Control)cmdSelectDE).set_TabIndex(8);
			((Control)cmdSelectDE).set_Text("Select DE Ephemeris");
			((ButtonBase)cmdSelectDE).set_UseVisualStyleBackColor(true);
			((Control)cmdSelectDE).add_Click((EventHandler)cmdSelectDE_Click);
			((Control)cmdListAndDisplayAsteroidals).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdListAndDisplayAsteroidals).set_Location(new Point(360, 84));
			((Control)cmdListAndDisplayAsteroidals).set_Name("cmdListAndDisplayAsteroidals");
			((Control)cmdListAndDisplayAsteroidals).set_Size(new Size(165, 84));
			((Control)cmdListAndDisplayAsteroidals).set_TabIndex(19);
			((Control)cmdListAndDisplayAsteroidals).set_Text("List && Display\r\noccultations");
			toolTip1.SetToolTip((Control)(object)cmdListAndDisplayAsteroidals, "If you have saved the results of a\r\nsearch, use this to list and display\r\nthe results");
			((ButtonBase)cmdListAndDisplayAsteroidals).set_UseVisualStyleBackColor(true);
			((Control)cmdListAndDisplayAsteroidals).add_Click((EventHandler)cmdListAndDisplayAsteroidals_Click);
			((Control)groupBox8).set_BackColor(Color.OldLace);
			((Control)groupBox8).get_Controls().Add((Control)(object)groupBox11);
			((Control)groupBox8).get_Controls().Add((Control)(object)groupBox10);
			((Control)groupBox8).get_Controls().Add((Control)(object)groupBox7);
			((Control)groupBox8).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox8).set_Location(new Point(7, 440));
			((Control)groupBox8).set_Name("groupBox8");
			((Control)groupBox8).set_Size(new Size(575, 150));
			((Control)groupBox8).set_TabIndex(15);
			groupBox8.set_TabStop(false);
			((Control)groupBox8).set_Text("Download prediction files");
			((Control)groupBox11).set_BackColor(Color.Cornsilk);
			((Control)groupBox11).get_Controls().Add((Control)(object)chkExcludeOldLuckyStar);
			((Control)groupBox11).get_Controls().Add((Control)(object)cmdLuckyStar);
			((Control)groupBox11).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			helpProvider1.SetHelpString((Control)(object)groupBox11, "");
			((Control)groupBox11).set_Location(new Point(206, 28));
			((Control)groupBox11).set_Name("groupBox11");
			helpProvider1.SetShowHelp((Control)(object)groupBox11, false);
			((Control)groupBox11).set_Size(new Size(177, 75));
			((Control)groupBox11).set_TabIndex(20);
			groupBox11.set_TabStop(false);
			((Control)groupBox11).set_Text("'Lucky Star' predictions");
			toolTip1.SetToolTip((Control)(object)groupBox11, "Predictions of TNOs and Centaurs  generated by IMCCE {Paris Observatory} as part of their 'Lucky Star' project");
			((Control)chkExcludeOldLuckyStar).set_AutoSize(true);
			chkExcludeOldLuckyStar.set_Checked(true);
			chkExcludeOldLuckyStar.set_CheckState((CheckState)1);
			((Control)chkExcludeOldLuckyStar).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)chkExcludeOldLuckyStar).set_Location(new Point(6, 16));
			((Control)chkExcludeOldLuckyStar).set_Name("chkExcludeOldLuckyStar");
			((Control)chkExcludeOldLuckyStar).set_Size(new Size(148, 30));
			((Control)chkExcludeOldLuckyStar).set_TabIndex(20);
			((Control)chkExcludeOldLuckyStar).set_Text("Exclude events more than\r\none month old");
			((ButtonBase)chkExcludeOldLuckyStar).set_UseVisualStyleBackColor(true);
			((Control)cmdLuckyStar).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdLuckyStar).set_Location(new Point(6, 46));
			((Control)cmdLuckyStar).set_Name("cmdLuckyStar");
			((Control)cmdLuckyStar).set_Size(new Size(165, 25));
			((Control)cmdLuckyStar).set_TabIndex(19);
			((Control)cmdLuckyStar).set_Text("Download /convert Lucky Star");
			((ButtonBase)cmdLuckyStar).set_UseVisualStyleBackColor(true);
			((Control)cmdLuckyStar).add_Click((EventHandler)cmdLuckyStar_Click);
			((Control)groupBox10).set_BackColor(Color.Cornsilk);
			((Control)groupBox10).get_Controls().Add((Control)(object)chkUseLocal);
			((Control)groupBox10).get_Controls().Add((Control)(object)chkUseExistingFiles);
			((Control)groupBox10).get_Controls().Add((Control)(object)chkExcludeOldRio);
			((Control)groupBox10).get_Controls().Add((Control)(object)cmdConvertRIO);
			((Control)groupBox10).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			helpProvider1.SetHelpString((Control)(object)groupBox10, "");
			((Control)groupBox10).set_Location(new Point(389, 28));
			((Control)groupBox10).set_Name("groupBox10");
			helpProvider1.SetShowHelp((Control)(object)groupBox10, false);
			((Control)groupBox10).set_Size(new Size(181, 112));
			((Control)groupBox10).set_TabIndex(18);
			groupBox10.set_TabStop(false);
			((Control)groupBox10).set_Text("'RIO' TNO, Jupiter Satellites");
			toolTip1.SetToolTip((Control)(object)groupBox10, "Predictions of TNOs and Centaurs  generated by the RIO group ");
			chkUseLocal.set_AutoCheck(false);
			((Control)chkUseLocal).set_AutoSize(true);
			((Control)chkUseLocal).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)chkUseLocal).set_Location(new Point(6, 55));
			((Control)chkUseLocal).set_Name("chkUseLocal");
			((Control)chkUseLocal).set_Size(new Size(128, 30));
			((Control)chkUseLocal).set_TabIndex(17);
			((Control)chkUseLocal).set_Text("Use local 'LOG.dat' && \r\n'table_occult.txt' (only)");
			((ButtonBase)chkUseLocal).set_UseVisualStyleBackColor(true);
			((Control)chkUseLocal).add_Click((EventHandler)chkUseLocal_Click);
			chkUseExistingFiles.set_AutoCheck(false);
			((Control)chkUseExistingFiles).set_AutoSize(true);
			((Control)chkUseExistingFiles).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)chkUseExistingFiles).set_Location(new Point(6, 40));
			((Control)chkUseExistingFiles).set_Name("chkUseExistingFiles");
			((Control)chkUseExistingFiles).set_Size(new Size(103, 17));
			((Control)chkUseExistingFiles).set_TabIndex(16);
			((Control)chkUseExistingFiles).set_Text("Use existing files");
			((ButtonBase)chkUseExistingFiles).set_UseVisualStyleBackColor(true);
			((Control)chkUseExistingFiles).add_Click((EventHandler)chkUseExistingFiles_Click);
			((Control)chkExcludeOldRio).set_AutoSize(true);
			chkExcludeOldRio.set_Checked(true);
			chkExcludeOldRio.set_CheckState((CheckState)1);
			((Control)chkExcludeOldRio).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)chkExcludeOldRio).set_Location(new Point(6, 13));
			((Control)chkExcludeOldRio).set_Name("chkExcludeOldRio");
			((Control)chkExcludeOldRio).set_Size(new Size(151, 30));
			((Control)chkExcludeOldRio).set_TabIndex(15);
			((Control)chkExcludeOldRio).set_Text("Exclude events more than \r\none month old");
			((ButtonBase)chkExcludeOldRio).set_UseVisualStyleBackColor(true);
			((Control)cmdConvertRIO).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdConvertRIO).set_Location(new Point(6, 84));
			((Control)cmdConvertRIO).set_Name("cmdConvertRIO");
			((Control)cmdConvertRIO).set_Size(new Size(165, 25));
			((Control)cmdConvertRIO).set_TabIndex(13);
			((Control)cmdConvertRIO).set_Text("Download and convert RIO");
			((ButtonBase)cmdConvertRIO).set_UseVisualStyleBackColor(true);
			((Control)cmdConvertRIO).add_Click((EventHandler)cmdConvertRIO_Click);
			((Control)groupBox7).set_BackColor(Color.Cornsilk);
			((Control)groupBox7).get_Controls().Add((Control)(object)cmdDownloadFuture);
			((Control)groupBox7).get_Controls().Add((Control)(object)button1);
			((Control)groupBox7).get_Controls().Add((Control)(object)lblPreserveFuture);
			((Control)groupBox7).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox7).set_Location(new Point(8, 28));
			((Control)groupBox7).set_Name("groupBox7");
			((Control)groupBox7).set_Size(new Size(192, 96));
			((Control)groupBox7).set_TabIndex(17);
			groupBox7.set_TabStop(false);
			((Control)groupBox7).set_Text("'Preston' files");
			toolTip1.SetToolTip((Control)(object)groupBox7, "Predictions generated by Steve Preston using the latest and best astrometry of asteroids");
			((Control)cmdConvertAstorb_MPCOrb).set_Location(new Point(41, 249));
			((Control)cmdConvertAstorb_MPCOrb).set_Name("cmdConvertAstorb_MPCOrb");
			((Control)cmdConvertAstorb_MPCOrb).set_Size(new Size(136, 38));
			((Control)cmdConvertAstorb_MPCOrb).set_TabIndex(12);
			((Control)cmdConvertAstorb_MPCOrb).set_Text("Convert Astorb\r\nMPCOrb, && AstDyS-2");
			((ButtonBase)cmdConvertAstorb_MPCOrb).set_UseVisualStyleBackColor(true);
			((Control)cmdConvertAstorb_MPCOrb).add_Click((EventHandler)cmdConvertAstorb_MPCOrb_Click);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(85, 173));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(192, 35));
			((Control)label3).set_TabIndex(11);
			((Control)label3).set_Text("Searches for possible occultations, and creates a file of occultation elements");
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(364, 173));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(156, 29));
			((Control)label2).set_TabIndex(10);
			((Control)label2).set_Text("Displays occultations from a file of occultation elements");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Times New Roman", 18f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(0, 21));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(574, 26));
			((Control)label9).set_TabIndex(0);
			((Control)label9).set_Text("Occultations by Asteroids, Planets, && planetary Moons");
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(393, 288));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(168, 68));
			((Control)label7).set_TabIndex(21);
			((Control)label7).set_Text("Compute the circumstances of\r\nmutual occultations or transits of\r\ntwo asteroids. Relies on tables of \r\nAsteroid Appulses provided at:\r\nhttps://minorplanet.info/");
			((Control)PanelEphemeris).set_BackColor(SystemColors.Control);
			((Control)PanelEphemeris).get_Controls().Add((Control)(object)groupBox12);
			((Control)PanelEphemeris).get_Controls().Add((Control)(object)cmdSelectDEEphemeris);
			((Control)PanelEphemeris).get_Controls().Add((Control)(object)grpShapeModels);
			((Control)PanelEphemeris).get_Controls().Add((Control)(object)cmdCloseAllEphemerisForms);
			((Control)PanelEphemeris).get_Controls().Add((Control)(object)groupBox6);
			((Control)PanelEphemeris).get_Controls().Add((Control)(object)label10);
			((Control)PanelEphemeris).get_Controls().Add((Control)(object)groupBox4);
			((Control)PanelEphemeris).get_Controls().Add((Control)(object)groupBox3);
			((Control)PanelEphemeris).get_Controls().Add((Control)(object)groupBox2);
			((Control)PanelEphemeris).get_Controls().Add((Control)(object)groupBox1);
			((Control)PanelEphemeris).get_Controls().Add((Control)(object)grpEphemeris);
			((Control)PanelEphemeris).set_Location(new Point(615, 55));
			((Control)PanelEphemeris).set_Name("PanelEphemeris");
			((Control)PanelEphemeris).set_Size(new Size(600, 650));
			((Control)PanelEphemeris).set_TabIndex(0);
			((Control)groupBox12).get_Controls().Add((Control)(object)cmdLightCurves);
			((Control)groupBox12).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox12).set_Location(new Point(405, 225));
			((Control)groupBox12).set_Name("groupBox12");
			((Control)groupBox12).set_Size(new Size(105, 88));
			((Control)groupBox12).set_TabIndex(53);
			groupBox12.set_TabStop(false);
			((Control)groupBox12).set_Text("Light curves");
			((Control)cmdLightCurves).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdLightCurves).set_Location(new Point(15, 22));
			((Control)cmdLightCurves).set_Name("cmdLightCurves");
			((Control)cmdLightCurves).set_Size(new Size(75, 54));
			((Control)cmdLightCurves).set_TabIndex(3);
			((Control)cmdLightCurves).set_Text("Light curve viewer");
			toolTip1.SetToolTip((Control)(object)cmdLightCurves, "Primary tool for managing and\r\ndisplaying asteroid shape models");
			((ButtonBase)cmdLightCurves).set_UseVisualStyleBackColor(true);
			((Control)cmdLightCurves).add_Click((EventHandler)cmdLightCurves_Click);
			((Control)cmdSelectDEEphemeris).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdSelectDEEphemeris).set_Location(new Point(116, 598));
			((Control)cmdSelectDEEphemeris).set_Name("cmdSelectDEEphemeris");
			((Control)cmdSelectDEEphemeris).set_Size(new Size(140, 21));
			((Control)cmdSelectDEEphemeris).set_TabIndex(52);
			((Control)cmdSelectDEEphemeris).set_Text("Select DE Ephemeris");
			((ButtonBase)cmdSelectDEEphemeris).set_UseVisualStyleBackColor(true);
			((Control)cmdSelectDEEphemeris).add_Click((EventHandler)cmdSelectDEEphemeris_Click);
			((Control)grpShapeModels).get_Controls().Add((Control)(object)cmdNewDAMIT);
			((Control)grpShapeModels).get_Controls().Add((Control)(object)cmdShapeModels);
			((Control)grpShapeModels).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpShapeModels).set_Location(new Point(279, 225));
			((Control)grpShapeModels).set_Name("grpShapeModels");
			((Control)grpShapeModels).set_Size(new Size(105, 88));
			((Control)grpShapeModels).set_TabIndex(3);
			grpShapeModels.set_TabStop(false);
			((Control)grpShapeModels).set_Text("Shape models");
			((Control)cmdNewDAMIT).set_BackColor(SystemColors.ControlLight);
			((Control)cmdNewDAMIT).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdNewDAMIT).set_Location(new Point(10, 64));
			((Control)cmdNewDAMIT).set_Name("cmdNewDAMIT");
			((Control)cmdNewDAMIT).set_Size(new Size(85, 19));
			((Control)cmdNewDAMIT).set_TabIndex(14);
			((Control)cmdNewDAMIT).set_Text("Demonstration");
			toolTip1.SetToolTip((Control)(object)cmdNewDAMIT, componentResourceManager.GetString("cmdNewDAMIT.ToolTip"));
			((ButtonBase)cmdNewDAMIT).set_UseVisualStyleBackColor(false);
			((Control)cmdNewDAMIT).add_Click((EventHandler)cmdNewDAMIT_Click);
			((Control)cmdShapeModels).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdShapeModels).set_Location(new Point(9, 22));
			((Control)cmdShapeModels).set_Name("cmdShapeModels");
			((Control)cmdShapeModels).set_Size(new Size(87, 37));
			((Control)cmdShapeModels).set_TabIndex(3);
			((Control)cmdShapeModels).set_Text("Asteroid shape\r\nmodels");
			toolTip1.SetToolTip((Control)(object)cmdShapeModels, "Primary tool for managing and\r\ndisplaying asteroid shape models");
			((ButtonBase)cmdShapeModels).set_UseVisualStyleBackColor(true);
			((Control)cmdShapeModels).add_Click((EventHandler)cmdShapeModels_Click);
			((Control)cmdCloseAllEphemerisForms).set_Location(new Point(314, 598));
			((Control)cmdCloseAllEphemerisForms).set_Name("cmdCloseAllEphemerisForms");
			((Control)cmdCloseAllEphemerisForms).set_Size(new Size(140, 21));
			((Control)cmdCloseAllEphemerisForms).set_TabIndex(51);
			((Control)cmdCloseAllEphemerisForms).set_Text("Close all Ephemeris forms");
			((ButtonBase)cmdCloseAllEphemerisForms).set_UseVisualStyleBackColor(true);
			((Control)cmdCloseAllEphemerisForms).add_Click((EventHandler)cmdCloseAllEphemerisForms_Click);
			((Control)groupBox6).get_Controls().Add((Control)(object)cmdRelativity);
			((Control)groupBox6).get_Controls().Add((Control)(object)cmdDistances);
			((Control)groupBox6).get_Controls().Add((Control)(object)cmdDatumConversions);
			((Control)groupBox6).get_Controls().Add((Control)(object)cmdMagnitudes);
			((Control)groupBox6).get_Controls().Add((Control)(object)cmdDrawStarChart);
			((Control)groupBox6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox6).set_Location(new Point(70, 507));
			((Control)groupBox6).set_Name("groupBox6");
			((Control)groupBox6).set_Size(new Size(440, 88));
			((Control)groupBox6).set_TabIndex(6);
			groupBox6.set_TabStop(false);
			((Control)groupBox6).set_Text("Other");
			((Control)cmdRelativity).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdRelativity).set_Location(new Point(226, 22));
			((Control)cmdRelativity).set_Name("cmdRelativity");
			((Control)cmdRelativity).set_Size(new Size(66, 54));
			((Control)cmdRelativity).set_TabIndex(6);
			((Control)cmdRelativity).set_Text("Relativistic bending");
			toolTip1.SetToolTip((Control)(object)cmdRelativity, "Calculates relativistic defletion by the Sun\r\nfor stars and asteroids");
			((ButtonBase)cmdRelativity).set_UseVisualStyleBackColor(true);
			((Control)cmdRelativity).add_Click((EventHandler)CmdRelativity_Click);
			((Control)cmdDistances).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDistances).set_Location(new Point(150, 22));
			((Control)cmdDistances).set_Name("cmdDistances");
			((Control)cmdDistances).set_Size(new Size(66, 54));
			((Control)cmdDistances).set_TabIndex(5);
			((Control)cmdDistances).set_Text("Distance between two points");
			toolTip1.SetToolTip((Control)(object)cmdDistances, componentResourceManager.GetString("cmdDistances.ToolTip"));
			((ButtonBase)cmdDistances).set_UseVisualStyleBackColor(true);
			((Control)cmdDistances).add_Click((EventHandler)cmdDistances_Click);
			((Control)cmdDatumConversions).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDatumConversions).set_Location(new Point(302, 21));
			((Control)cmdDatumConversions).set_Name("cmdDatumConversions");
			((Control)cmdDatumConversions).set_Size(new Size(122, 54));
			((Control)cmdDatumConversions).set_TabIndex(4);
			((Control)cmdDatumConversions).set_Text("Datum conversions\r\nGeoid height of MSL\r\nElevations above MSL");
			toolTip1.SetToolTip((Control)(object)cmdDatumConversions, "Conversion from 'old' geodetic \r\ndatums to WGS84");
			((ButtonBase)cmdDatumConversions).set_UseVisualStyleBackColor(true);
			((Control)cmdDatumConversions).add_Click((EventHandler)cmdDatumConversions_Click);
			((Control)cmdMagnitudes).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdMagnitudes).set_Location(new Point(74, 22));
			((Control)cmdMagnitudes).set_Name("cmdMagnitudes");
			((Control)cmdMagnitudes).set_Size(new Size(66, 54));
			((Control)cmdMagnitudes).set_TabIndex(3);
			((Control)cmdMagnitudes).set_Text("Magnitude calculator");
			toolTip1.SetToolTip((Control)(object)cmdMagnitudes, "Calculate:\r\n* Combined magnitude of two stars\r\n* Magnitude change to intensity change\r\n& derive the magnitude of two stars from\r\na stepped light curve");
			((ButtonBase)cmdMagnitudes).set_UseVisualStyleBackColor(true);
			((Control)cmdMagnitudes).add_Click((EventHandler)cmdMagnitudes_Click);
			((Control)cmdDrawStarChart).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDrawStarChart).set_Location(new Point(15, 21));
			((Control)cmdDrawStarChart).set_Name("cmdDrawStarChart");
			((Control)cmdDrawStarChart).set_Size(new Size(49, 54));
			((Control)cmdDrawStarChart).set_TabIndex(2);
			((Control)cmdDrawStarChart).set_Text("Star chart");
			toolTip1.SetToolTip((Control)(object)cmdDrawStarChart, "Display a star chart");
			((ButtonBase)cmdDrawStarChart).set_UseVisualStyleBackColor(true);
			((Control)cmdDrawStarChart).add_Click((EventHandler)cmdDrawStarChart_Click);
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Times New Roman", 18f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label10).set_Location(new Point(16, 5));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(548, 26));
			((Control)label10).set_TabIndex(20);
			((Control)label10).set_Text("General ephemerides, appearances, dates and events");
			((Control)groupBox4).get_Controls().Add((Control)(object)cmdDiary);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmdMutualConjunctions);
			((Control)groupBox4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox4).set_Location(new Point(70, 225));
			((Control)groupBox4).set_Name("groupBox4");
			((Control)groupBox4).set_Size(new Size(189, 88));
			((Control)groupBox4).set_TabIndex(2);
			groupBox4.set_TabStop(false);
			((Control)groupBox4).set_Text("Astronomical events");
			((Control)groupBox3).get_Controls().Add((Control)(object)cmdMeridians);
			((Control)groupBox3).get_Controls().Add((Control)(object)cmdPlanetPlot);
			((Control)groupBox3).get_Controls().Add((Control)(object)MoonPhases);
			((Control)groupBox3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox3).set_Location(new Point(70, 131));
			((Control)groupBox3).set_Name("groupBox3");
			((Control)groupBox3).set_Size(new Size(440, 88));
			((Control)groupBox3).set_TabIndex(1);
			groupBox3.set_TabStop(false);
			((Control)groupBox3).set_Text("Appearances of the Planets and Moon");
			((Control)groupBox2).get_Controls().Add((Control)(object)cmdObjectAltitude);
			((Control)groupBox2).get_Controls().Add((Control)(object)cmdRiseSet);
			((Control)groupBox2).get_Controls().Add((Control)(object)cmdMoonRiseSet);
			((Control)groupBox2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox2).set_Location(new Point(70, 319));
			((Control)groupBox2).set_Name("groupBox2");
			((Control)groupBox2).set_Size(new Size(440, 88));
			((Control)groupBox2).set_TabIndex(4);
			groupBox2.set_TabStop(false);
			((Control)groupBox2).set_Text("Rise and Set times");
			((Control)cmdObjectAltitude).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdObjectAltitude).set_Location(new Point(313, 24));
			((Control)cmdObjectAltitude).set_Name("cmdObjectAltitude");
			((Control)cmdObjectAltitude).set_Size(new Size(111, 54));
			((Control)cmdObjectAltitude).set_TabIndex(2);
			((Control)cmdObjectAltitude).set_Text("Altitude \r\nof an object");
			((ButtonBase)cmdObjectAltitude).set_UseVisualStyleBackColor(true);
			((Control)cmdObjectAltitude).add_Click((EventHandler)cmdObjectAltitude_Click);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdAsteroidSatellites);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdAsteroidEphemeris);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdComets);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdPlanetEphemeris);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdSatellitePositions);
			((Control)groupBox1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox1).set_ForeColor(SystemColors.ControlText);
			((Control)groupBox1).set_Location(new Point(70, 37));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(440, 88));
			((Control)groupBox1).set_TabIndex(0);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Positions");
			((Control)cmdAsteroidSatellites).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdAsteroidSatellites).set_Location(new Point(368, 27));
			((Control)cmdAsteroidSatellites).set_Name("cmdAsteroidSatellites");
			((Control)cmdAsteroidSatellites).set_Size(new Size(58, 51));
			((Control)cmdAsteroidSatellites).set_TabIndex(4);
			((Control)cmdAsteroidSatellites).set_Text("Asteroid \r\nSatellites");
			toolTip1.SetToolTip((Control)(object)cmdAsteroidSatellites, "Generate ephemerides of asteroid \r\nsatellites, using the Miriade \r\nsystem of Paris Observatory");
			((ButtonBase)cmdAsteroidSatellites).set_UseVisualStyleBackColor(true);
			((Control)cmdAsteroidSatellites).add_Click((EventHandler)cmdAsteroidSatellites_Click);
			((Control)cmdAsteroidEphemeris).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdAsteroidEphemeris).set_Location(new Point(110, 27));
			((Control)cmdAsteroidEphemeris).set_Name("cmdAsteroidEphemeris");
			((Control)cmdAsteroidEphemeris).set_Size(new Size(85, 51));
			((Control)cmdAsteroidEphemeris).set_TabIndex(1);
			((Control)cmdAsteroidEphemeris).set_Text("Asteroids");
			toolTip1.SetToolTip((Control)(object)cmdAsteroidEphemeris, "Generate ephemerides of asteroids");
			((ButtonBase)cmdAsteroidEphemeris).set_UseVisualStyleBackColor(true);
			((Control)cmdAsteroidEphemeris).add_Click((EventHandler)cmdAsteroidEphemeris_Click);
			((Control)ViewStarCatDetails2).set_Location(new Point(59, 228));
			((Control)ViewStarCatDetails2).set_Name("ViewStarCatDetails2");
			((Control)ViewStarCatDetails2).set_Size(new Size(128, 54));
			((Control)ViewStarCatDetails2).set_TabIndex(9);
			((Control)ViewStarCatDetails2).set_Text("View catalogue details\r\nfor\r\nSelected stars");
			toolTip1.SetToolTip((Control)(object)ViewStarCatDetails2, "Display star catalogue details\r\nusing a star number and catalogue \r\n\r\nDouble and variable star entries are \r\nalso retrieved");
			((ButtonBase)ViewStarCatDetails2).set_UseVisualStyleBackColor(true);
			((Control)ViewStarCatDetails2).add_Click((EventHandler)ViewStarCatDetails2_Click);
			((Control)PanelSatellites).set_BackColor(SystemColors.Control);
			((Control)PanelSatellites).get_Controls().Add((Control)(object)label24);
			((Control)PanelSatellites).get_Controls().Add((Control)(object)label23);
			((Control)PanelSatellites).get_Controls().Add((Control)(object)label12);
			((Control)PanelSatellites).get_Controls().Add((Control)(object)cmdSatelliteEvents);
			((Control)PanelSatellites).get_Controls().Add((Control)(object)cmdMutualEvents);
			((Control)PanelSatellites).set_Location(new Point(670, 35));
			((Control)PanelSatellites).set_Name("PanelSatellites");
			((Control)PanelSatellites).set_Size(new Size(600, 650));
			((Control)PanelSatellites).set_TabIndex(25);
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Font(new Font("Times New Roman", 14.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label24).set_Location(new Point(78, 86));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(424, 22));
			((Control)label24).set_TabIndex(18);
			((Control)label24).set_Text("Eclipses, occultations and transits - with the Planet\r\n");
			label24.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Font(new Font("Times New Roman", 14.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label23).set_Location(new Point(153, 288));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(275, 22));
			((Control)label23).set_TabIndex(17);
			((Control)label23).set_Text("Mutual eclipses and occultations");
			label23.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Times New Roman", 18f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(100, 13));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(380, 26));
			((Control)label12).set_TabIndex(16);
			((Control)label12).set_Text("Events involving the major satellites");
			label12.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)PanelMaintenance).set_BackColor(SystemColors.Control);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)cmdDeleteSuperceded);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)cmdGaiaDoubleStars);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)cmdAsteroidClasses);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)label6);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)cmdCreateGaia);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)Process_LightCurves);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)cmdCreateDEEphemerisFiles);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)cmdEditAsteroidRings);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)label5);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)label4);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)txtStarNumber);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)cmbStarCat);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)cmdListEquivalents);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)cmdCreateDowloadControl);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)cmdViewWDS);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)cmdCompareStarPositions);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)cmdDownloads);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)label8);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)cmdCheckInstall);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)ViewStarCatDetails2);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)label29);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)cmbDisplayCats);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)grpNomad);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)cmdEditBinaryAsteroids);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)cmdXZ80Maintain);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)cmdEditSites);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)label26);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)label22);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)cmdDiameters);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)cmdDefault);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)cmdConvertAstorb);
			((Control)PanelMaintenance).get_Controls().Add((Control)(object)cmddeltaT);
			((Control)PanelMaintenance).set_Location(new Point(660, 16));
			((Control)PanelMaintenance).set_Name("PanelMaintenance");
			((Control)PanelMaintenance).set_Size(new Size(600, 650));
			((Control)PanelMaintenance).set_TabIndex(26);
			((Control)cmdDeleteSuperceded).set_BackColor(Color.OldLace);
			((Control)cmdDeleteSuperceded).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdDeleteSuperceded).set_ForeColor(Color.DarkRed);
			((Control)cmdDeleteSuperceded).set_Location(new Point(393, 41));
			((Control)cmdDeleteSuperceded).set_Name("cmdDeleteSuperceded");
			((Control)cmdDeleteSuperceded).set_Size(new Size(128, 54));
			((Control)cmdDeleteSuperceded).set_TabIndex(31);
			((Control)cmdDeleteSuperceded).set_Text("Move/Delete superseded files");
			((ButtonBase)cmdDeleteSuperceded).set_UseVisualStyleBackColor(false);
			((Control)cmdDeleteSuperceded).add_Click((EventHandler)cmdDeleteSuperceded_Click);
			((Control)cmdGaiaDoubleStars).set_Location(new Point(393, 295));
			((Control)cmdGaiaDoubleStars).set_Name("cmdGaiaDoubleStars");
			((Control)cmdGaiaDoubleStars).set_Size(new Size(128, 54));
			((Control)cmdGaiaDoubleStars).set_TabIndex(30);
			((Control)cmdGaiaDoubleStars).set_Text("View double stars in\r\nGaia DR3\r\nnon-single-star files");
			toolTip1.SetToolTip((Control)(object)cmdGaiaDoubleStars, "Search for double and variable\r\nstar information using a \r\nstar's coordinates");
			((ButtonBase)cmdGaiaDoubleStars).set_UseVisualStyleBackColor(true);
			((Control)cmdGaiaDoubleStars).add_Click((EventHandler)cmdGaiaDoubleStars_Click);
			((Control)cmdAsteroidClasses).set_Location(new Point(393, 436));
			((Control)cmdAsteroidClasses).set_Name("cmdAsteroidClasses");
			((Control)cmdAsteroidClasses).set_Size(new Size(109, 36));
			((Control)cmdAsteroidClasses).set_TabIndex(26);
			((Control)cmdAsteroidClasses).set_Text("Asteroid taxonomy, \r\nincl. binary asteroids");
			toolTip1.SetToolTip((Control)(object)cmdAsteroidClasses, "Updates the file of asteroids by\r\ntaxanomic class\r\n\r\nAdministrator use. to create file \r\nfor dissemination");
			((ButtonBase)cmdAsteroidClasses).set_UseVisualStyleBackColor(true);
			((Control)cmdAsteroidClasses).add_Click((EventHandler)cmdAsteroidClasses_Click);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(370, 493));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(174, 15));
			((Control)label6).set_TabIndex(28);
			((Control)label6).set_Text("Create catalogs from GAIA");
			((Control)cmdCreateGaia).set_Location(new Point(393, 516));
			((Control)cmdCreateGaia).set_Name("cmdCreateGaia");
			((Control)cmdCreateGaia).set_Size(new Size(128, 54));
			((Control)cmdCreateGaia).set_TabIndex(29);
			((Control)cmdCreateGaia).set_Text("Create GAIA catalogue from TAPVizier output");
			((ButtonBase)cmdCreateGaia).set_UseVisualStyleBackColor(true);
			((Control)cmdCreateGaia).add_Click((EventHandler)cmdCreateGaia_Click);
			((Control)Process_LightCurves).set_Location(new Point(169, 436));
			((Control)Process_LightCurves).set_Name("Process_LightCurves");
			((Control)Process_LightCurves).set_Size(new Size(74, 36));
			((Control)Process_LightCurves).set_TabIndex(22);
			((Control)Process_LightCurves).set_Text("Process light curves");
			toolTip1.SetToolTip((Control)(object)Process_LightCurves, "Tool to process submitted \r\noccultation light curves ready\r\nfor distribution and archiving");
			((ButtonBase)Process_LightCurves).set_UseVisualStyleBackColor(true);
			((Control)Process_LightCurves).add_Click((EventHandler)Process_LightCurves_Click);
			((Control)cmdCreateDEEphemerisFiles).set_Location(new Point(393, 133));
			((Control)cmdCreateDEEphemerisFiles).set_Name("cmdCreateDEEphemerisFiles");
			((Control)cmdCreateDEEphemerisFiles).set_Size(new Size(128, 40));
			((Control)cmdCreateDEEphemerisFiles).set_TabIndex(6);
			((Control)cmdCreateDEEphemerisFiles).set_Text("Download && convert\r\nJPL-DE ephemeris  file");
			toolTip1.SetToolTip((Control)(object)cmdCreateDEEphemerisFiles, "Tool to manage/update the JPLE-DE\r\nEphemeris when new versions\r\nbecome available");
			((ButtonBase)cmdCreateDEEphemerisFiles).set_UseVisualStyleBackColor(true);
			((Control)cmdCreateDEEphemerisFiles).add_Click((EventHandler)cmdCreateDEEphemerisFiles_Click);
			((Control)cmdEditAsteroidRings).set_Location(new Point(279, 436));
			((Control)cmdEditAsteroidRings).set_Name("cmdEditAsteroidRings");
			((Control)cmdEditAsteroidRings).set_Size(new Size(74, 36));
			((Control)cmdEditAsteroidRings).set_TabIndex(25);
			((Control)cmdEditAsteroidRings).set_Text("Edit asteroid rings");
			toolTip1.SetToolTip((Control)(object)cmdEditAsteroidRings, "Tool to edit the file of asteroid \r\nrings\r\n\r\nAdministrator use, to create a file \r\nfor distribution");
			((ButtonBase)cmdEditAsteroidRings).set_UseVisualStyleBackColor(true);
			((Control)cmdEditAsteroidRings).add_Click((EventHandler)cmdEditAsteroidRings_Click);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(275, 318));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(44, 13));
			((Control)label5).set_TabIndex(15);
			((Control)label5).set_Text("Number");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(227, 318));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(43, 13));
			((Control)label4).set_TabIndex(14);
			((Control)label4).set_Text("Catalog");
			((Control)txtStarNumber).set_Location(new Point(275, 332));
			((Control)txtStarNumber).set_Name("txtStarNumber");
			((Control)txtStarNumber).set_Size(new Size(77, 20));
			((Control)txtStarNumber).set_TabIndex(17);
			((Control)txtStarNumber).set_Text("1");
			((ListControl)cmbStarCat).set_FormattingEnabled(true);
			cmbStarCat.get_Items().AddRange(new object[13]
			{
				"BD", "CD", "CPD", "FK5", "GC", "GSC", "HD", "HIP", "PPM", "SAO",
				"TYC", "XZ", "ZC"
			});
			((Control)cmbStarCat).set_Location(new Point(227, 332));
			((Control)cmbStarCat).set_Name("cmbStarCat");
			((Control)cmbStarCat).set_Size(new Size(46, 21));
			((Control)cmbStarCat).set_TabIndex(16);
			((Control)cmdListEquivalents).set_Location(new Point(227, 295));
			((Control)cmdListEquivalents).set_Name("cmdListEquivalents");
			((Control)cmdListEquivalents).set_Size(new Size(127, 23));
			((Control)cmdListEquivalents).set_TabIndex(13);
			((Control)cmdListEquivalents).set_Text("List equivalents");
			toolTip1.SetToolTip((Control)(object)cmdListEquivalents, "Query Vizier to retrieve a list\r\nof star designations that are \r\nequivalent to the specified star");
			((ButtonBase)cmdListEquivalents).set_UseVisualStyleBackColor(true);
			((Control)cmdListEquivalents).add_Click((EventHandler)cmdListEquivalents_Click);
			((Control)cmdCreateDowloadControl).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdCreateDowloadControl).set_Location(new Point(202, 599));
			((Control)cmdCreateDowloadControl).set_Name("cmdCreateDowloadControl");
			((Control)cmdCreateDowloadControl).set_Size(new Size(151, 24));
			((Control)cmdCreateDowloadControl).set_TabIndex(7);
			((Control)cmdCreateDowloadControl).set_Text("Create download control file");
			toolTip1.SetToolTip((Control)(object)cmdCreateDowloadControl, "Tool to create the file that \r\nspecifies new files available for\r\ndownload from the IOTA site");
			((ButtonBase)cmdCreateDowloadControl).set_UseVisualStyleBackColor(true);
			((Control)cmdCreateDowloadControl).add_Click((EventHandler)cmdCreateDowloadControl_Click);
			((Control)cmdViewWDS).set_Location(new Point(393, 228));
			((Control)cmdViewWDS).set_Name("cmdViewWDS");
			((Control)cmdViewWDS).set_Size(new Size(128, 54));
			((Control)cmdViewWDS).set_TabIndex(11);
			((Control)cmdViewWDS).set_Text("View double stars in\r\nWDS and IF, and\r\nvariables + light curve");
			toolTip1.SetToolTip((Control)(object)cmdViewWDS, "Search for double and variable\r\nstar information using a \r\nstar's coordinates");
			((ButtonBase)cmdViewWDS).set_UseVisualStyleBackColor(true);
			((Control)cmdViewWDS).add_Click((EventHandler)cmdViewWDS_Click);
			((Control)cmdCompareStarPositions).set_Location(new Point(227, 228));
			((Control)cmdCompareStarPositions).set_Name("cmdCompareStarPositions");
			((Control)cmdCompareStarPositions).set_Size(new Size(128, 54));
			((Control)cmdCompareStarPositions).set_TabIndex(10);
			((Control)cmdCompareStarPositions).set_Text("Single star:\r\nCompare positions from\r\nmany catalogues");
			toolTip1.SetToolTip((Control)(object)cmdCompareStarPositions, "Tool to query Vizier for positons \r\nof a star assourced from different\r\ncatalogues.\r\n\r\nThis tool is essentially obsolete following\r\nGaia DR2");
			((ButtonBase)cmdCompareStarPositions).set_UseVisualStyleBackColor(true);
			((Control)cmdCompareStarPositions).add_Click((EventHandler)cmdCompareStarPositions_Click);
			((Control)cmdDownloads).set_BackColor(Color.Honeydew);
			((Control)cmdDownloads).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdDownloads).set_Location(new Point(59, 133));
			((Control)cmdDownloads).set_Name("cmdDownloads");
			((Control)cmdDownloads).set_Size(new Size(128, 54));
			((Control)cmdDownloads).set_TabIndex(4);
			((Control)cmdDownloads).set_Text("General\r\ndownloads");
			toolTip1.SetToolTip((Control)(object)cmdDownloads, "Tool to manage the dowloading \r\nof over 40 sources of support data");
			((ButtonBase)cmdDownloads).set_UseVisualStyleBackColor(false);
			((Control)cmdDownloads).add_Click((EventHandler)cmdDownloads_Click);
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Times New Roman", 18f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(165, 106));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(251, 26));
			((Control)label8).set_TabIndex(3);
			((Control)label8).set_Text("Download data updates");
			((Control)cmdCheckInstall).set_Location(new Point(59, 436));
			((Control)cmdCheckInstall).set_Name("cmdCheckInstall");
			((Control)cmdCheckInstall).set_Size(new Size(74, 36));
			((Control)cmdCheckInstall).set_TabIndex(21);
			((Control)cmdCheckInstall).set_Text("Check Installation");
			((ButtonBase)cmdCheckInstall).set_UseVisualStyleBackColor(true);
			((Control)cmdCheckInstall).add_Click((EventHandler)cmdCheckInstall_Click);
			((Control)label29).set_AutoSize(true);
			((Control)label29).set_Font(new Font("Times New Roman", 18f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label29).set_Location(new Point(100, 202));
			((Control)label29).set_Name("label29");
			((Control)label29).set_Size(new Size(381, 26));
			((Control)label29).set_TabIndex(8);
			((Control)label29).set_Text("Star catalogues - display star details");
			((Control)cmbDisplayCats).set_Location(new Point(59, 293));
			((Control)cmbDisplayCats).set_Name("cmbDisplayCats");
			((Control)cmbDisplayCats).set_Size(new Size(128, 54));
			((Control)cmbDisplayCats).set_TabIndex(12);
			((Control)cmbDisplayCats).set_Text("View catalogue details\r\nfor a\r\nRange of stars");
			toolTip1.SetToolTip((Control)(object)cmbDisplayCats, "Tool to scan through the entries\r\nof installed star catalogues\r\n");
			((ButtonBase)cmbDisplayCats).set_UseVisualStyleBackColor(true);
			((Control)cmbDisplayCats).add_Click((EventHandler)cmbDisplayCats_Click);
			((Control)grpNomad).get_Controls().Add((Control)(object)cmdLaSilla);
			((Control)grpNomad).get_Controls().Add((Control)(object)label33);
			((Control)grpNomad).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpNomad).set_Location(new Point(43, 488));
			((Control)grpNomad).set_Name("grpNomad");
			((Control)grpNomad).set_Size(new Size(319, 100));
			((Control)grpNomad).set_TabIndex(27);
			grpNomad.set_TabStop(false);
			((Control)grpNomad).set_Text("Create User star catalogue");
			((Control)cmdLaSilla).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdLaSilla).set_Location(new Point(219, 18));
			((Control)cmdLaSilla).set_Name("cmdLaSilla");
			((Control)cmdLaSilla).set_Size(new Size(88, 73));
			((Control)cmdLaSilla).set_TabIndex(1);
			((Control)cmdLaSilla).set_Text("Create User catalogue\r\nfrom ASCII catalogue");
			((ButtonBase)cmdLaSilla).set_UseVisualStyleBackColor(true);
			((Control)cmdLaSilla).add_Click((EventHandler)cmdLaSilla_Click);
			((Control)label33).set_AutoSize(true);
			((Control)label33).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label33).set_Location(new Point(6, 20));
			((Control)label33).set_Name("label33");
			((Control)label33).set_Size(new Size(215, 65));
			((Control)label33).set_TabIndex(0);
			((Control)label33).set_Text("This option creates a star catalogue in the\r\n format of the Tycho2  catalogue as used in \r\nOccult. It is provided for users who create\r\na catalogue using their own astrometry.\r\nNot for general users!");
			((Control)cmdEditBinaryAsteroids).set_Location(new Point(279, 392));
			((Control)cmdEditBinaryAsteroids).set_Name("cmdEditBinaryAsteroids");
			((Control)cmdEditBinaryAsteroids).set_Size(new Size(74, 36));
			((Control)cmdEditBinaryAsteroids).set_TabIndex(23);
			((Control)cmdEditBinaryAsteroids).set_Text("Edit binary asteroids");
			toolTip1.SetToolTip((Control)(object)cmdEditBinaryAsteroids, "Tool to edit the file of binary\r\nasteroids.\r\n\r\nAdministrator use, to create a file \r\nfor distribution\r\n");
			((ButtonBase)cmdEditBinaryAsteroids).set_UseVisualStyleBackColor(true);
			((Control)cmdEditBinaryAsteroids).add_Click((EventHandler)cmdEditBinaryAsteroids_Click);
			((Control)cmdXZ80Maintain).set_Location(new Point(169, 392));
			((Control)cmdXZ80Maintain).set_Name("cmdXZ80Maintain");
			((Control)cmdXZ80Maintain).set_Size(new Size(74, 36));
			((Control)cmdXZ80Maintain).set_TabIndex(20);
			((Control)cmdXZ80Maintain).set_Text("Maintain XZ catalogue");
			toolTip1.SetToolTip((Control)(object)cmdXZ80Maintain, "Tool to update the XZ catalogue\r\nwith new data for dould and \r\nvariable stars");
			((ButtonBase)cmdXZ80Maintain).set_UseVisualStyleBackColor(true);
			((Control)cmdXZ80Maintain).add_Click((EventHandler)cmdXZ80Maintain_Click);
			((Control)cmdEditSites).set_Location(new Point(227, 41));
			((Control)cmdEditSites).set_Name("cmdEditSites");
			((Control)cmdEditSites).set_Size(new Size(128, 36));
			((Control)cmdEditSites).set_TabIndex(2);
			((Control)cmdEditSites).set_Text("Edit SITE files");
			toolTip1.SetToolTip((Control)(object)cmdEditSites, "Some function require a 'Site file'\r\ncontaining site coordinates. This \r\ntool is to manage Site files");
			((ButtonBase)cmdEditSites).set_UseVisualStyleBackColor(true);
			((Control)cmdEditSites).add_Click((EventHandler)cmdEditSites_Click);
			((Control)label26).set_AutoSize(true);
			((Control)label26).set_Font(new Font("Times New Roman", 18f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label26).set_Location(new Point(149, 364));
			((Control)label26).set_Name("label26");
			((Control)label26).set_Size(new Size(284, 26));
			((Control)label26).set_TabIndex(18);
			((Control)label26).set_Text("Maintain certain data files");
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Font(new Font("Times New Roman", 18f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label22).set_Location(new Point(125, 14));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(330, 26));
			((Control)label22).set_TabIndex(0);
			((Control)label22).set_Text("General maintenance functions");
			((Control)cmdDiameters).set_Location(new Point(393, 392));
			((Control)cmdDiameters).set_Name("cmdDiameters");
			((Control)cmdDiameters).set_Size(new Size(74, 36));
			((Control)cmdDiameters).set_TabIndex(24);
			((Control)cmdDiameters).set_Text("Edit asteroid diameters");
			toolTip1.SetToolTip((Control)(object)cmdDiameters, "Tool to edit the file of asteroid diameters\r\n\r\nAdministrator use, to create a file \r\nfor distribution\r\n\r\nAs at 2020, not being used");
			((ButtonBase)cmdDiameters).set_UseVisualStyleBackColor(true);
			((Control)cmdDiameters).add_Click((EventHandler)cmdDiameters_Click);
			((Control)cmdDefault).set_BackColor(Color.Honeydew);
			((Control)cmdDefault).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdDefault).set_Location(new Point(59, 41));
			((Control)cmdDefault).set_Name("cmdDefault");
			((Control)cmdDefault).set_Size(new Size(128, 54));
			((Control)cmdDefault).set_TabIndex(1);
			((Control)cmdDefault).set_Text("User settings");
			toolTip1.SetToolTip((Control)(object)cmdDefault, "Set a number of values to \r\n'personalise' occult");
			((ButtonBase)cmdDefault).set_UseVisualStyleBackColor(false);
			((Control)cmdDefault).add_Click((EventHandler)cmdDefault_Click);
			((Control)PanelAsteroidObservations).set_BackColor(SystemColors.Control);
			((Control)PanelAsteroidObservations).get_Controls().Add((Control)(object)groupBox9);
			((Control)PanelAsteroidObservations).get_Controls().Add((Control)(object)groupBox5);
			((Control)PanelAsteroidObservations).get_Controls().Add((Control)(object)cmdCloseAllAsteroidObservations);
			((Control)PanelAsteroidObservations).get_Controls().Add((Control)(object)cmdAsteroidObservations);
			((Control)PanelAsteroidObservations).get_Controls().Add((Control)(object)grpMaintainObsFile);
			((Control)PanelAsteroidObservations).get_Controls().Add((Control)(object)grpAsteroidLists);
			((Control)PanelAsteroidObservations).get_Controls().Add((Control)(object)label27);
			((Control)PanelAsteroidObservations).set_Location(new Point(64, 25));
			((Control)PanelAsteroidObservations).set_Name("PanelAsteroidObservations");
			((Control)PanelAsteroidObservations).set_Size(new Size(600, 650));
			((Control)PanelAsteroidObservations).set_TabIndex(5);
			((Control)groupBox9).set_BackColor(Color.FloralWhite);
			((Control)groupBox9).get_Controls().Add((Control)(object)cmdViewLightCurve);
			((Control)groupBox9).get_Controls().Add((Control)(object)cmdArchiveLightCurve);
			((Control)groupBox9).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox9).set_Location(new Point(315, 145));
			((Control)groupBox9).set_Name("groupBox9");
			((Control)groupBox9).set_Size(new Size(220, 118));
			((Control)groupBox9).set_TabIndex(9);
			groupBox9.set_TabStop(false);
			((Control)groupBox9).set_Text("Light curves");
			((Control)cmdViewLightCurve).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdViewLightCurve).set_Location(new Point(37, 69));
			((Control)cmdViewLightCurve).set_Name("cmdViewLightCurve");
			((Control)cmdViewLightCurve).set_Size(new Size(146, 37));
			((Control)cmdViewLightCurve).set_TabIndex(1);
			((Control)cmdViewLightCurve).set_Text("Light curve viewer");
			toolTip1.SetToolTip((Control)(object)cmdViewLightCurve, "View past occultation light curves \r\nthat have been reported");
			((ButtonBase)cmdViewLightCurve).set_UseVisualStyleBackColor(true);
			((Control)cmdViewLightCurve).add_Click((EventHandler)cmdViewLightCurve_Click);
			((Control)cmdArchiveLightCurve).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdArchiveLightCurve).set_Location(new Point(37, 21));
			((Control)cmdArchiveLightCurve).set_Name("cmdArchiveLightCurve");
			((Control)cmdArchiveLightCurve).set_Size(new Size(146, 33));
			((Control)cmdArchiveLightCurve).set_TabIndex(0);
			((Control)cmdArchiveLightCurve).set_Text("Report a light curve");
			toolTip1.SetToolTip((Control)(object)cmdArchiveLightCurve, "Generate an  occultation light curve \r\nreport to be archived at VizieR");
			((ButtonBase)cmdArchiveLightCurve).set_UseVisualStyleBackColor(true);
			((Control)cmdArchiveLightCurve).add_Click((EventHandler)cmdArchiveLightCurve_Click);
			((Control)groupBox5).set_BackColor(Color.Honeydew);
			((Control)groupBox5).get_Controls().Add((Control)(object)cmdAOTA);
			((Control)groupBox5).get_Controls().Add((Control)(object)cmdStarDiameterAnalyser);
			((Control)groupBox5).get_Controls().Add((Control)(object)label13);
			((Control)groupBox5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox5).set_Location(new Point(66, 145));
			((Control)groupBox5).set_Name("groupBox5");
			((Control)groupBox5).set_Size(new Size(220, 118));
			((Control)groupBox5).set_TabIndex(3);
			groupBox5.set_TabStop(false);
			((Control)groupBox5).set_Text("Analysis tools");
			((Control)cmdAOTA).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdAOTA).set_Location(new Point(35, 17));
			((Control)cmdAOTA).set_Name("cmdAOTA");
			((Control)cmdAOTA).set_Size(new Size(150, 53));
			((Control)cmdAOTA).set_TabIndex(0);
			((Control)cmdAOTA).set_Text("AOTA\r\nAsteroidal Occn Time Analyser");
			toolTip1.SetToolTip((Control)(object)cmdAOTA, "Analyse an occultation light curve");
			((ButtonBase)cmdAOTA).set_UseVisualStyleBackColor(true);
			((Control)cmdAOTA).add_Click((EventHandler)cmdAOTA_Click);
			((Control)cmdStarDiameterAnalyser).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdStarDiameterAnalyser).set_Location(new Point(49, 89));
			((Control)cmdStarDiameterAnalyser).set_Name("cmdStarDiameterAnalyser");
			((Control)cmdStarDiameterAnalyser).set_Size(new Size(122, 25));
			((Control)cmdStarDiameterAnalyser).set_TabIndex(1);
			((Control)cmdStarDiameterAnalyser).set_Text("Star diameter analyser");
			toolTip1.SetToolTip((Control)(object)cmdStarDiameterAnalyser, "Measure a star's diameter by adjusting\r\na theoretical light curve to match an\r\nobserved gradual light curve");
			((ButtonBase)cmdStarDiameterAnalyser).set_UseVisualStyleBackColor(true);
			((Control)cmdStarDiameterAnalyser).add_Click((EventHandler)cmdStarDiameterAnalyser_Click);
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_BackColor(Color.Aquamarine);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label13).set_Location(new Point(37, 70));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(146, 12));
			((Control)label13).set_TabIndex(3);
			((Control)label13).set_Text("For Limovie, Tangra, and Pymovie");
			((Control)cmdCloseAllAsteroidObservations).set_Location(new Point(203, 600));
			((Control)cmdCloseAllAsteroidObservations).set_Name("cmdCloseAllAsteroidObservations");
			((Control)cmdCloseAllAsteroidObservations).set_Size(new Size(194, 25));
			((Control)cmdCloseAllAsteroidObservations).set_TabIndex(7);
			((Control)cmdCloseAllAsteroidObservations).set_Text("Close all Asteroid observation forms");
			((ButtonBase)cmdCloseAllAsteroidObservations).set_UseVisualStyleBackColor(true);
			((Control)cmdCloseAllAsteroidObservations).add_Click((EventHandler)cmdCloseAllAsteroidObservations_Click);
			((Control)cmdAsteroidObservations).set_Font(new Font("Microsoft Sans Serif", 11f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdAsteroidObservations).set_Location(new Point(206, 44));
			((Control)cmdAsteroidObservations).set_Name("cmdAsteroidObservations");
			((Control)cmdAsteroidObservations).set_Size(new Size(189, 88));
			((Control)cmdAsteroidObservations).set_TabIndex(1);
			((Control)cmdAsteroidObservations).set_Text("Add / Edit / Plot\r\nobserved\r\nasteroidal occultations");
			toolTip1.SetToolTip((Control)(object)cmdAsteroidObservations, "The observations editor. The \r\nprimary tool for analying\r\nobservations");
			((ButtonBase)cmdAsteroidObservations).set_UseVisualStyleBackColor(true);
			((Control)cmdAsteroidObservations).add_Click((EventHandler)cmdAsteroidObservations_Click);
			((Control)grpMaintainObsFile).set_BackColor(Color.PaleTurquoise);
			((Control)grpMaintainObsFile).get_Controls().Add((Control)(object)cmdSearchFreeText);
			((Control)grpMaintainObsFile).get_Controls().Add((Control)(object)cmdMergeEditedWithMain);
			((Control)grpMaintainObsFile).get_Controls().Add((Control)(object)cmdNASA_PDS);
			((Control)grpMaintainObsFile).get_Controls().Add((Control)(object)cmdUpdateKeplerLinks);
			((Control)grpMaintainObsFile).get_Controls().Add((Control)(object)cmdCheckAsteroidalMidTimes);
			((Control)grpMaintainObsFile).get_Controls().Add((Control)(object)cmdValidate);
			((Control)grpMaintainObsFile).get_Controls().Add((Control)(object)cmdListPoorStars);
			((Control)grpMaintainObsFile).get_Controls().Add((Control)(object)cmdListDuplicates);
			((Control)grpMaintainObsFile).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpMaintainObsFile).set_Location(new Point(53, 469));
			((Control)grpMaintainObsFile).set_Name("grpMaintainObsFile");
			((Control)grpMaintainObsFile).set_Size(new Size(494, 123));
			((Control)grpMaintainObsFile).set_TabIndex(5);
			grpMaintainObsFile.set_TabStop(false);
			((Control)grpMaintainObsFile).set_Text("Maintenance    [for administrator use]");
			((Control)cmdSearchFreeText).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSearchFreeText).set_Location(new Point(215, 16));
			((Control)cmdSearchFreeText).set_Name("cmdSearchFreeText");
			((Control)cmdSearchFreeText).set_Size(new Size(77, 47));
			((Control)cmdSearchFreeText).set_TabIndex(7);
			((Control)cmdSearchFreeText).set_Text("Search free- text field");
			((ButtonBase)cmdSearchFreeText).set_UseVisualStyleBackColor(true);
			((Control)cmdSearchFreeText).add_Click((EventHandler)cmdSearchFreeText_Click);
			((Control)cmdMergeEditedWithMain).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdMergeEditedWithMain).set_Location(new Point(338, 71));
			((Control)cmdMergeEditedWithMain).set_Name("cmdMergeEditedWithMain");
			((Control)cmdMergeEditedWithMain).set_Size(new Size(137, 42));
			((Control)cmdMergeEditedWithMain).set_TabIndex(6);
			((Control)cmdMergeEditedWithMain).set_Text("Observations  file:  Merge\r\nmain file with edited ");
			((ButtonBase)cmdMergeEditedWithMain).set_UseVisualStyleBackColor(true);
			((Control)cmdMergeEditedWithMain).add_Click((EventHandler)cmdMergeEditedWithMain_Click);
			((Control)cmdNASA_PDS).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdNASA_PDS).set_Location(new Point(338, 12));
			((Control)cmdNASA_PDS).set_Name("cmdNASA_PDS");
			((Control)cmdNASA_PDS).set_Size(new Size(137, 54));
			((Control)cmdNASA_PDS).set_TabIndex(5);
			((Control)cmdNASA_PDS).set_Text("Process reports for:\r\n  * Minor Planet Center\r\n  * NASA PDS");
			((ButtonBase)cmdNASA_PDS).set_TextAlign(ContentAlignment.MiddleLeft);
			toolTip1.SetToolTip((Control)(object)cmdNASA_PDS, "Tool to create the files required\r\nfor archiving the observations at\r\nNASA's Planetary Data ystem");
			((ButtonBase)cmdNASA_PDS).set_UseVisualStyleBackColor(true);
			((Control)cmdNASA_PDS).add_Click((EventHandler)cmdNASA_PDS_Click);
			((Control)cmdUpdateKeplerLinks).set_Enabled(false);
			((Control)cmdUpdateKeplerLinks).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdUpdateKeplerLinks).set_Location(new Point(211, 95));
			((Control)cmdUpdateKeplerLinks).set_Name("cmdUpdateKeplerLinks");
			((Control)cmdUpdateKeplerLinks).set_Size(new Size(77, 22));
			((Control)cmdUpdateKeplerLinks).set_TabIndex(4);
			((Control)cmdUpdateKeplerLinks).set_Text("Add Kepler2 references");
			toolTip1.SetToolTip((Control)(object)cmdUpdateKeplerLinks, "Tool to add Kepler2 star\r\nreferences");
			((ButtonBase)cmdUpdateKeplerLinks).set_UseVisualStyleBackColor(true);
			((Control)cmdUpdateKeplerLinks).set_Visible(false);
			((Control)cmdUpdateKeplerLinks).add_Click((EventHandler)cmdUpdateKeplerLinks_Click);
			((Control)cmdCheckAsteroidalMidTimes).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdCheckAsteroidalMidTimes).set_Location(new Point(128, 69));
			((Control)cmdCheckAsteroidalMidTimes).set_Name("cmdCheckAsteroidalMidTimes");
			((Control)cmdCheckAsteroidalMidTimes).set_Size(new Size(77, 47));
			((Control)cmdCheckAsteroidalMidTimes).set_TabIndex(3);
			((Control)cmdCheckAsteroidalMidTimes).set_Text("Check specified \r\nMidT values");
			toolTip1.SetToolTip((Control)(object)cmdCheckAsteroidalMidTimes, "Tool to identify events where the\r\ntime set to determine the asteroid's\r\nmotion differs from the average\r\nobserved time");
			((ButtonBase)cmdCheckAsteroidalMidTimes).set_UseVisualStyleBackColor(true);
			((Control)cmdCheckAsteroidalMidTimes).add_Click((EventHandler)cmdCheckAsteroidalMidTimes_Click);
			((Control)cmdValidate).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdValidate).set_Location(new Point(6, 69));
			((Control)cmdValidate).set_Name("cmdValidate");
			((Control)cmdValidate).set_Size(new Size(114, 47));
			((Control)cmdValidate).set_TabIndex(1);
			((Control)cmdValidate).set_Text("Run data validity check on Historical file");
			toolTip1.SetToolTip((Control)(object)cmdValidate, "Tool that runs a data validity check\r\non the data in the observations file.");
			((ButtonBase)cmdValidate).set_UseVisualStyleBackColor(true);
			((Control)cmdValidate).add_Click((EventHandler)cmdValidate_Click);
			((Control)cmdListPoorStars).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdListPoorStars).set_Location(new Point(128, 16));
			((Control)cmdListPoorStars).set_Name("cmdListPoorStars");
			((Control)cmdListPoorStars).set_Size(new Size(77, 47));
			((Control)cmdListPoorStars).set_TabIndex(2);
			((Control)cmdListPoorStars).set_Text("List poor star positions");
			toolTip1.SetToolTip((Control)(object)cmdListPoorStars, "In right-hand panel -  Lists events \r\nwhere the star position was\r\nnot sourced from GAIA DR2");
			((ButtonBase)cmdListPoorStars).set_UseVisualStyleBackColor(true);
			((Control)cmdListPoorStars).add_Click((EventHandler)cmdListPoorStars_Click);
			((Control)cmdListDuplicates).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdListDuplicates).set_Location(new Point(6, 16));
			((Control)cmdListDuplicates).set_Name("cmdListDuplicates");
			((Control)cmdListDuplicates).set_Size(new Size(114, 47));
			((Control)cmdListDuplicates).set_TabIndex(0);
			((Control)cmdListDuplicates).set_Text("List events on same-\r\nday, by same asteroid \r\n");
			toolTip1.SetToolTip((Control)(object)cmdListDuplicates, "Lists events on the same day by \r\nthe ame asteroid. Used to identify \r\nduplicate event entries.");
			((ButtonBase)cmdListDuplicates).set_UseVisualStyleBackColor(true);
			((Control)cmdListDuplicates).add_Click((EventHandler)cmdListDuplicates_Click);
			((Control)grpAsteroidLists).set_BackColor(Color.LightYellow);
			((Control)grpAsteroidLists).get_Controls().Add((Control)(object)cmdListEventsWithNoMainBody);
			((Control)grpAsteroidLists).get_Controls().Add((Control)(object)cmdRingedAsteroids);
			((Control)grpAsteroidLists).get_Controls().Add((Control)(object)cmdForReview);
			((Control)grpAsteroidLists).get_Controls().Add((Control)(object)cmdListWrongUncertainties);
			((Control)grpAsteroidLists).get_Controls().Add((Control)(object)cmdUnfittedEvents);
			((Control)grpAsteroidLists).get_Controls().Add((Control)(object)cmdBinaryAsteroids);
			((Control)grpAsteroidLists).get_Controls().Add((Control)(object)cmdShapeModelFits);
			((Control)grpAsteroidLists).get_Controls().Add((Control)(object)cmdAsteroidDiameter);
			((Control)grpAsteroidLists).get_Controls().Add((Control)(object)cmdListAsteroidObserver);
			((Control)grpAsteroidLists).get_Controls().Add((Control)(object)cmdFindOccultedStars_Asteroidal);
			((Control)grpAsteroidLists).get_Controls().Add((Control)(object)cmdK2Events);
			((Control)grpAsteroidLists).get_Controls().Add((Control)(object)cmdGenerateDistantAsteroidList);
			((Control)grpAsteroidLists).get_Controls().Add((Control)(object)cmdGenerateAsteroidGraphicFiles);
			((Control)grpAsteroidLists).get_Controls().Add((Control)(object)cmdListDiameters);
			((Control)grpAsteroidLists).get_Controls().Add((Control)(object)cmdListPositions);
			((Control)grpAsteroidLists).get_Controls().Add((Control)(object)cmdListDoubleStars);
			((Control)grpAsteroidLists).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpAsteroidLists).set_Location(new Point(53, 270));
			((Control)grpAsteroidLists).set_Name("grpAsteroidLists");
			((Control)grpAsteroidLists).set_Size(new Size(494, 191));
			((Control)grpAsteroidLists).set_TabIndex(4);
			grpAsteroidLists.set_TabStop(false);
			((Control)grpAsteroidLists).set_Text("Summary lists");
			((Control)cmdListEventsWithNoMainBody).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdListEventsWithNoMainBody).set_Location(new Point(170, 145));
			((Control)cmdListEventsWithNoMainBody).set_Name("cmdListEventsWithNoMainBody");
			((Control)cmdListEventsWithNoMainBody).set_Size(new Size(69, 35));
			((Control)cmdListEventsWithNoMainBody).set_TabIndex(7);
			((Control)cmdListEventsWithNoMainBody).set_Text("(b) or (r), no\r\nmain body");
			toolTip1.SetToolTip((Control)(object)cmdListEventsWithNoMainBody, "List all events where the Main Body was\r\nnot detected. This will involve either a \r\nBinary asteroid, or an asteroid with rings.");
			((ButtonBase)cmdListEventsWithNoMainBody).set_UseVisualStyleBackColor(true);
			((Control)cmdListEventsWithNoMainBody).add_Click((EventHandler)cmdListEventsWithNoMainBody_Click);
			((Control)cmdRingedAsteroids).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdRingedAsteroids).set_Location(new Point(170, 103));
			((Control)cmdRingedAsteroids).set_Name("cmdRingedAsteroids");
			((Control)cmdRingedAsteroids).set_Size(new Size(69, 35));
			((Control)cmdRingedAsteroids).set_TabIndex(6);
			((Control)cmdRingedAsteroids).set_Text("(r) Ringed\r\nasteroids");
			toolTip1.SetToolTip((Control)(object)cmdRingedAsteroids, "List all asteroid events where occultation\r\nby a ring has been observed\r\n(including 'suspected' rings).");
			((ButtonBase)cmdRingedAsteroids).set_UseVisualStyleBackColor(true);
			((Control)cmdRingedAsteroids).add_Click((EventHandler)cmdRingedAsteroids_Click);
			((Control)cmdForReview).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdForReview).set_Location(new Point(364, 145));
			((Control)cmdForReview).set_Name("cmdForReview");
			((Control)cmdForReview).set_Size(new Size(113, 35));
			((Control)cmdForReview).set_TabIndex(14);
			((Control)cmdForReview).set_Text("Events marked\r\n'For review'");
			toolTip1.SetToolTip((Control)(object)cmdForReview, componentResourceManager.GetString("cmdForReview.ToolTip"));
			((ButtonBase)cmdForReview).set_UseVisualStyleBackColor(true);
			((Control)cmdForReview).add_Click((EventHandler)cmdForReview_Click);
			((Control)cmdListWrongUncertainties).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdListWrongUncertainties).set_Location(new Point(267, 103));
			((Control)cmdListWrongUncertainties).set_Name("cmdListWrongUncertainties");
			((Control)cmdListWrongUncertainties).set_Size(new Size(69, 35));
			((Control)cmdListWrongUncertainties).set_TabIndex(10);
			((Control)cmdListWrongUncertainties).set_Text("Large uncerts");
			toolTip1.SetToolTip((Control)(object)cmdListWrongUncertainties, "Tool to identify events where the \r\nastrometry has large uncertainties \r\nin RA or Dec. The tool allows such \r\nevents to be identified and fixed.");
			((ButtonBase)cmdListWrongUncertainties).set_UseVisualStyleBackColor(true);
			((Control)cmdListWrongUncertainties).add_Click((EventHandler)CmdListWrongUncertainties_Click);
			((Control)cmdUnfittedEvents).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdUnfittedEvents).set_Location(new Point(267, 61));
			((Control)cmdUnfittedEvents).set_Name("cmdUnfittedEvents");
			((Control)cmdUnfittedEvents).set_Size(new Size(69, 35));
			((Control)cmdUnfittedEvents).set_TabIndex(9);
			((Control)cmdUnfittedEvents).set_Text("Unfitted events");
			toolTip1.SetToolTip((Control)(object)cmdUnfittedEvents, "Maintenance tool - to list events where\r\nthe observation has not been 'fitted'\r\n\r\nSuch events have been deemed unreliable.\r\nThis tool allows them to be identified\r\nand reviewed");
			((ButtonBase)cmdUnfittedEvents).set_UseVisualStyleBackColor(true);
			((Control)cmdUnfittedEvents).add_Click((EventHandler)cmdUnfittedEvents_Click);
			((Control)cmdBinaryAsteroids).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdBinaryAsteroids).set_Location(new Point(170, 61));
			((Control)cmdBinaryAsteroids).set_Name("cmdBinaryAsteroids");
			((Control)cmdBinaryAsteroids).set_Size(new Size(69, 35));
			((Control)cmdBinaryAsteroids).set_TabIndex(5);
			((Control)cmdBinaryAsteroids).set_Text("(b) Binary\r\nasteroids");
			toolTip1.SetToolTip((Control)(object)cmdBinaryAsteroids, "List all binary asteroid occultation\r\nobservations (including 'suspected'\r\nbinary asteroids.");
			((ButtonBase)cmdBinaryAsteroids).set_UseVisualStyleBackColor(true);
			((Control)cmdBinaryAsteroids).add_Click((EventHandler)cmdBinaryAsteroids_Click);
			((Control)cmdShapeModelFits).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdShapeModelFits).set_Location(new Point(17, 145));
			((Control)cmdShapeModelFits).set_Name("cmdShapeModelFits");
			((Control)cmdShapeModelFits).set_Size(new Size(125, 35));
			((Control)cmdShapeModelFits).set_TabIndex(2);
			((Control)cmdShapeModelFits).set_Text("Shape model fits\r\nDetails");
			toolTip1.SetToolTip((Control)(object)cmdShapeModelFits, "List the fitting of asteroids to shape models.\r\n\r\nThe fits are listed by selecting the asterod.");
			((ButtonBase)cmdShapeModelFits).set_UseVisualStyleBackColor(true);
			((Control)cmdShapeModelFits).add_Click((EventHandler)cmdShapeModelFits_Click);
			((Control)cmdListAsteroidObserver).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdListAsteroidObserver).set_Location(new Point(364, 19));
			((Control)cmdListAsteroidObserver).set_Name("cmdListAsteroidObserver");
			((Control)cmdListAsteroidObserver).set_Size(new Size(113, 35));
			((Control)cmdListAsteroidObserver).set_TabIndex(4);
			((Control)cmdListAsteroidObserver).set_Text("Observations by \r\nan observer");
			toolTip1.SetToolTip((Control)(object)cmdListAsteroidObserver, "Tool to identify all observations\r\nmade by an observer");
			((ButtonBase)cmdListAsteroidObserver).set_UseVisualStyleBackColor(true);
			((Control)cmdListAsteroidObserver).add_Click((EventHandler)cmdListAsteroidObserver_Click);
			((Control)cmdFindOccultedStars_Asteroidal).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdFindOccultedStars_Asteroidal).set_Location(new Point(364, 61));
			((Control)cmdFindOccultedStars_Asteroidal).set_Name("cmdFindOccultedStars_Asteroidal");
			((Control)cmdFindOccultedStars_Asteroidal).set_Size(new Size(113, 35));
			((Control)cmdFindOccultedStars_Asteroidal).set_TabIndex(13);
			((Control)cmdFindOccultedStars_Asteroidal).set_Text("Statistics     +\r\nFind occulted stars");
			toolTip1.SetToolTip((Control)(object)cmdFindOccultedStars_Asteroidal, componentResourceManager.GetString("cmdFindOccultedStars_Asteroidal.ToolTip"));
			((ButtonBase)cmdFindOccultedStars_Asteroidal).set_UseVisualStyleBackColor(true);
			((Control)cmdFindOccultedStars_Asteroidal).add_Click((EventHandler)cmdFindOccultedStars_Asteroidal_Click);
			((Control)cmdK2Events).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdK2Events).set_Location(new Point(267, 145));
			((Control)cmdK2Events).set_Name("cmdK2Events");
			((Control)cmdK2Events).set_Size(new Size(69, 35));
			((Control)cmdK2Events).set_TabIndex(11);
			((Control)cmdK2Events).set_Text("Kepler2 stars");
			toolTip1.SetToolTip((Control)(object)cmdK2Events, "List events involving Kepler2 stars");
			((ButtonBase)cmdK2Events).set_UseVisualStyleBackColor(true);
			((Control)cmdK2Events).add_Click((EventHandler)cmdK2Events_Click);
			((Control)cmdAsteroidDiameter).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdAsteroidDiameter).set_Location(new Point(17, 61));
			((Control)cmdAsteroidDiameter).set_Name("cmdAsteroidDiameter");
			((Control)cmdAsteroidDiameter).set_Size(new Size(125, 35));
			((Control)cmdAsteroidDiameter).set_TabIndex(3);
			((Control)cmdAsteroidDiameter).set_Text("Observed chords +\r\nShapeModel Diameters");
			toolTip1.SetToolTip((Control)(object)cmdAsteroidDiameter, "List the length of all observed chords \r\nfor all occultations by an asteroid\r\n\r\nIncludes an approximate diameter \r\ndetermination using a method developed\r\nby John Broughton.");
			((ButtonBase)cmdAsteroidDiameter).set_UseVisualStyleBackColor(true);
			((Control)cmdAsteroidDiameter).add_Click((EventHandler)cmdAsteroidDiameter_Click);
			((Control)cmdGenerateDistantAsteroidList).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdGenerateDistantAsteroidList).set_Location(new Point(267, 19));
			((Control)cmdGenerateDistantAsteroidList).set_Name("cmdGenerateDistantAsteroidList");
			((Control)cmdGenerateDistantAsteroidList).set_Size(new Size(69, 35));
			((Control)cmdGenerateDistantAsteroidList).set_TabIndex(8);
			((Control)cmdGenerateDistantAsteroidList).set_Text("Events by\r\nclass");
			toolTip1.SetToolTip((Control)(object)cmdGenerateDistantAsteroidList, "List events by asteroid taxonomic class\r\n- other than the 'main belt' class ");
			((ButtonBase)cmdGenerateDistantAsteroidList).set_UseVisualStyleBackColor(true);
			((Control)cmdGenerateDistantAsteroidList).add_Click((EventHandler)cmdGenerateDistantAsteroidList_Click);
			((Control)cmdGenerateAsteroidGraphicFiles).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdGenerateAsteroidGraphicFiles).set_Location(new Point(364, 103));
			((Control)cmdGenerateAsteroidGraphicFiles).set_Name("cmdGenerateAsteroidGraphicFiles");
			((Control)cmdGenerateAsteroidGraphicFiles).set_Size(new Size(113, 35));
			((Control)cmdGenerateAsteroidGraphicFiles).set_TabIndex(12);
			((Control)cmdGenerateAsteroidGraphicFiles).set_Text("Generate Graphics files for Many events");
			toolTip1.SetToolTip((Control)(object)cmdGenerateAsteroidGraphicFiles, "Generate multiple image files \r\nshowing the results of occultations. \r\nThe tool has options to specify \r\nwhat is output");
			((ButtonBase)cmdGenerateAsteroidGraphicFiles).set_UseVisualStyleBackColor(true);
			((Control)cmdGenerateAsteroidGraphicFiles).add_Click((EventHandler)cmdGenerateAsteroidGraphicFiles_Click);
			((Control)cmdListDiameters).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdListDiameters).set_Location(new Point(17, 103));
			((Control)cmdListDiameters).set_Name("cmdListDiameters");
			((Control)cmdListDiameters).set_Size(new Size(125, 35));
			((Control)cmdListDiameters).set_TabIndex(1);
			((Control)cmdListDiameters).set_Text("Observed diameters\r\nEllipse,  Shape Models");
			toolTip1.SetToolTip((Control)(object)cmdListDiameters, "List the observed diameters of asteroids\r\nderived from ocultations");
			((ButtonBase)cmdListDiameters).set_UseVisualStyleBackColor(true);
			((Control)cmdListDiameters).add_Click((EventHandler)cmdListDiameters_Click);
			((Control)cmdListPositions).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdListPositions).set_Location(new Point(17, 19));
			((Control)cmdListPositions).set_Name("cmdListPositions");
			((Control)cmdListPositions).set_Size(new Size(125, 35));
			((Control)cmdListPositions).set_TabIndex(0);
			((Control)cmdListPositions).set_Text("Astrometric results");
			toolTip1.SetToolTip((Control)(object)cmdListPositions, "List the astrometric positions of \r\nasteroids derived from occultations");
			((ButtonBase)cmdListPositions).set_UseVisualStyleBackColor(true);
			((Control)cmdListPositions).add_Click((EventHandler)cmdListPositions_Click);
			((Control)cmdListDoubleStars).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdListDoubleStars).set_Location(new Point(170, 19));
			((Control)cmdListDoubleStars).set_Name("cmdListDoubleStars");
			((Control)cmdListDoubleStars).set_Size(new Size(69, 35));
			((Control)cmdListDoubleStars).set_TabIndex(4);
			((Control)cmdListDoubleStars).set_Text("Double stars");
			toolTip1.SetToolTip((Control)(object)cmdListDoubleStars, "List all double star discoveries/observations\r\nfrom asteroidal occultations");
			((ButtonBase)cmdListDoubleStars).set_UseVisualStyleBackColor(true);
			((Control)cmdListDoubleStars).add_Click((EventHandler)cmdListDoubleStars_Click);
			((Control)label27).set_AutoSize(true);
			((Control)label27).set_Font(new Font("Times New Roman", 18f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label27).set_Location(new Point(88, 12));
			((Control)label27).set_Name("label27");
			((Control)label27).set_Size(new Size(404, 26));
			((Control)label27).set_TabIndex(0);
			((Control)label27).set_Text("Minor Planet occultation observations");
			((Control)PanelEclipsesTransits).set_BackColor(SystemColors.Control);
			((Control)PanelEclipsesTransits).get_Controls().Add((Control)(object)label1);
			((Control)PanelEclipsesTransits).get_Controls().Add((Control)(object)cmdSelectDE_eclipses);
			((Control)PanelEclipsesTransits).get_Controls().Add((Control)(object)cmdCraterTimings);
			((Control)PanelEclipsesTransits).get_Controls().Add((Control)(object)label32);
			((Control)PanelEclipsesTransits).get_Controls().Add((Control)(object)cmdBailyBeads);
			((Control)PanelEclipsesTransits).get_Controls().Add((Control)(object)label20);
			((Control)PanelEclipsesTransits).get_Controls().Add((Control)(object)label19);
			((Control)PanelEclipsesTransits).get_Controls().Add((Control)(object)cmdSolarEclipse);
			((Control)PanelEclipsesTransits).get_Controls().Add((Control)(object)label11);
			((Control)PanelEclipsesTransits).get_Controls().Add((Control)(object)cmdTransits);
			((Control)PanelEclipsesTransits).get_Controls().Add((Control)(object)cmdLunarEclipse);
			((Control)PanelEclipsesTransits).set_Location(new Point(650, 51));
			((Control)PanelEclipsesTransits).set_Name("PanelEclipsesTransits");
			((Control)PanelEclipsesTransits).set_Size(new Size(600, 650));
			((Control)PanelEclipsesTransits).set_TabIndex(27);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Times New Roman", 18f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(82, 457));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(425, 26));
			((Control)label1).set_TabIndex(54);
			((Control)label1).set_Text("Analysis of Lunar Eclipse crater timings");
			((Control)cmdSelectDE_eclipses).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdSelectDE_eclipses).set_Location(new Point(223, 164));
			((Control)cmdSelectDE_eclipses).set_Name("cmdSelectDE_eclipses");
			((Control)cmdSelectDE_eclipses).set_Size(new Size(135, 24));
			((Control)cmdSelectDE_eclipses).set_TabIndex(53);
			((Control)cmdSelectDE_eclipses).set_Text("Select DE Ephemeris");
			((ButtonBase)cmdSelectDE_eclipses).set_UseVisualStyleBackColor(true);
			((Control)cmdSelectDE_eclipses).add_Click((EventHandler)cmdSelectDE_eclipses_Click);
			((Control)cmdCraterTimings).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdCraterTimings).set_Location(new Point(238, 490));
			((Control)cmdCraterTimings).set_Name("cmdCraterTimings");
			((Control)cmdCraterTimings).set_Size(new Size(112, 54));
			((Control)cmdCraterTimings).set_TabIndex(9);
			((Control)cmdCraterTimings).set_Text("Crater timing analysis");
			((ButtonBase)cmdCraterTimings).set_UseVisualStyleBackColor(true);
			((Control)cmdCraterTimings).add_Click((EventHandler)cmdCraterTimings_Click);
			((Control)label32).set_AutoSize(true);
			((Control)label32).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label32).set_Location(new Point(159, 288));
			((Control)label32).set_Name("label32");
			((Control)label32).set_Size(new Size(262, 17));
			((Control)label32).set_TabIndex(8);
			((Control)label32).set_Text("including animation of Baily Beads ");
			((Control)cmdBailyBeads).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdBailyBeads).set_Location(new Point(236, 346));
			((Control)cmdBailyBeads).set_Name("cmdBailyBeads");
			((Control)cmdBailyBeads).set_Size(new Size(115, 73));
			((Control)cmdBailyBeads).set_TabIndex(6);
			((Control)cmdBailyBeads).set_Text("Baily Bead \r\nAnalysis");
			((ButtonBase)cmdBailyBeads).set_UseVisualStyleBackColor(true);
			((Control)cmdBailyBeads).add_Click((EventHandler)cmdBailyBeads_Click);
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label20).set_Location(new Point(40, 312));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(501, 13));
			((Control)label20).set_TabIndex(5);
			((Control)label20).set_Text("Mr Baily's original description of beads [at the annular eclipse of 1836 May 15] is in 1836 MNRAS IV pg 15");
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Times New Roman", 18f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label19).set_Location(new Point(129, 259));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(322, 26));
			((Control)label19).set_TabIndex(4);
			((Control)label19).set_Text("Analysis of Baily Bead timings");
			((Control)cmdSolarEclipse).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdSolarEclipse).set_Location(new Point(64, 76));
			((Control)cmdSolarEclipse).set_Name("cmdSolarEclipse");
			((Control)cmdSolarEclipse).set_Size(new Size(124, 61));
			((Control)cmdSolarEclipse).set_TabIndex(1);
			((Control)cmdSolarEclipse).set_Text("Solar Eclipses");
			((ButtonBase)cmdSolarEclipse).set_UseVisualStyleBackColor(true);
			((Control)cmdSolarEclipse).add_Click((EventHandler)cmdSolarEclipse_Click);
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Times New Roman", 18f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label11).set_Location(new Point(128, 21));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(325, 26));
			((Control)label11).set_TabIndex(0);
			((Control)label11).set_Text("Eclipse and transit predictions");
			((Control)PanelLunarPredictions).set_BackColor(SystemColors.Control);
			((Control)PanelLunarPredictions).get_Controls().Add((Control)(object)cmdCloseAllLunarPredictions);
			((Control)PanelLunarPredictions).get_Controls().Add((Control)(object)cmdAutoGrazeOutput);
			((Control)PanelLunarPredictions).get_Controls().Add((Control)(object)cmdSingleLunar);
			((Control)PanelLunarPredictions).get_Controls().Add((Control)(object)cmdStarCatDetails);
			((Control)PanelLunarPredictions).get_Controls().Add((Control)(object)cmdDisplayMoonInStarField);
			((Control)PanelLunarPredictions).get_Controls().Add((Control)(object)cmdFindOccultations);
			((Control)PanelLunarPredictions).get_Controls().Add((Control)(object)cmdEditUserMinorPlanetsLunar);
			((Control)PanelLunarPredictions).get_Controls().Add((Control)(object)label21);
			((Control)PanelLunarPredictions).set_Location(new Point(760, 37));
			((Control)PanelLunarPredictions).set_Name("PanelLunarPredictions");
			((Control)PanelLunarPredictions).set_Size(new Size(600, 650));
			((Control)PanelLunarPredictions).set_TabIndex(28);
			((Control)cmdCloseAllLunarPredictions).set_Location(new Point(205, 556));
			((Control)cmdCloseAllLunarPredictions).set_Name("cmdCloseAllLunarPredictions");
			((Control)cmdCloseAllLunarPredictions).set_Size(new Size(170, 24));
			((Control)cmdCloseAllLunarPredictions).set_TabIndex(41);
			((Control)cmdCloseAllLunarPredictions).set_Text("Close all Lunar Prediction forms");
			((ButtonBase)cmdCloseAllLunarPredictions).set_UseVisualStyleBackColor(true);
			((Control)cmdCloseAllLunarPredictions).add_Click((EventHandler)cmdCloseAllLunarPredictions_Click);
			((Control)cmdAutoGrazeOutput).set_Location(new Point(400, 87));
			((Control)cmdAutoGrazeOutput).set_Name("cmdAutoGrazeOutput");
			((Control)cmdAutoGrazeOutput).set_Size(new Size(124, 83));
			((Control)cmdAutoGrazeOutput).set_TabIndex(39);
			((Control)cmdAutoGrazeOutput).set_Text("Predictions\r\nof\r\nMultiple objects\r\nfor Multiple sites");
			((ButtonBase)cmdAutoGrazeOutput).set_UseVisualStyleBackColor(true);
			((Control)cmdAutoGrazeOutput).add_Click((EventHandler)cmdAutoGrazeOutput_Click);
			((Control)cmdSingleLunar).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdSingleLunar).set_Location(new Point(224, 87));
			((Control)cmdSingleLunar).set_Name("cmdSingleLunar");
			((Control)cmdSingleLunar).set_Size(new Size(124, 83));
			((Control)cmdSingleLunar).set_TabIndex(1);
			((Control)cmdSingleLunar).set_Text("Predictions\r\nfor\r\nsingle sites\r\n");
			((ButtonBase)cmdSingleLunar).set_UseVisualStyleBackColor(true);
			((Control)cmdSingleLunar).add_Click((EventHandler)cmdSingleLunar_Click);
			((Control)cmdStarCatDetails).set_Location(new Point(233, 322));
			((Control)cmdStarCatDetails).set_Name("cmdStarCatDetails");
			((Control)cmdStarCatDetails).set_Size(new Size(115, 41));
			((Control)cmdStarCatDetails).set_TabIndex(38);
			((Control)cmdStarCatDetails).set_Text("View star \r\ncatalogue details");
			((ButtonBase)cmdStarCatDetails).set_UseVisualStyleBackColor(true);
			((Control)cmdStarCatDetails).add_Click((EventHandler)cmdStarCatDetails_Click);
			((Control)cmdDisplayMoonInStarField).set_Location(new Point(405, 322));
			((Control)cmdDisplayMoonInStarField).set_Name("cmdDisplayMoonInStarField");
			((Control)cmdDisplayMoonInStarField).set_Size(new Size(115, 41));
			((Control)cmdDisplayMoonInStarField).set_TabIndex(19);
			((Control)cmdDisplayMoonInStarField).set_Text("Display Moon in star field");
			((ButtonBase)cmdDisplayMoonInStarField).set_UseVisualStyleBackColor(true);
			((Control)cmdDisplayMoonInStarField).add_Click((EventHandler)cmdDisplayMoonInStarField_Click);
			((Control)cmdFindOccultations).set_Location(new Point(52, 87));
			((Control)cmdFindOccultations).set_Name("cmdFindOccultations");
			((Control)cmdFindOccultations).set_Size(new Size(124, 83));
			((Control)cmdFindOccultations).set_TabIndex(18);
			((Control)cmdFindOccultations).set_Text("Predictions\r\nof\r\nSingle objects");
			((ButtonBase)cmdFindOccultations).set_UseVisualStyleBackColor(true);
			((Control)cmdFindOccultations).add_Click((EventHandler)cmdFindOccultations_Click);
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Font(new Font("Times New Roman", 18f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label21).set_Location(new Point(128, 18));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(325, 26));
			((Control)label21).set_TabIndex(15);
			((Control)label21).set_Text("Lunar Occultation predictions");
			((Control)PanelLunarObservations).set_BackColor(SystemColors.Control);
			((Control)PanelLunarObservations).get_Controls().Add((Control)(object)cmdLunarEventFromStarAltitude);
			((Control)PanelLunarObservations).get_Controls().Add((Control)(object)cmdViewLightCurveLunar);
			((Control)PanelLunarObservations).get_Controls().Add((Control)(object)cmdListLunarsOfK2Stars);
			((Control)PanelLunarObservations).get_Controls().Add((Control)(object)cmdStarAnalysis);
			((Control)PanelLunarObservations).get_Controls().Add((Control)(object)cmdListMissingGrazes);
			((Control)PanelLunarObservations).get_Controls().Add((Control)(object)cmdSolveDouble);
			((Control)PanelLunarObservations).get_Controls().Add((Control)(object)cmdPlotLunarAgainstLimb);
			((Control)PanelLunarObservations).get_Controls().Add((Control)(object)grpLunarReduceAdmin);
			((Control)PanelLunarObservations).get_Controls().Add((Control)(object)button3);
			((Control)PanelLunarObservations).get_Controls().Add((Control)(object)cmdViewHistoricalOccults);
			((Control)PanelLunarObservations).get_Controls().Add((Control)(object)cmdViewHistoricalGrazes);
			((Control)PanelLunarObservations).get_Controls().Add((Control)(object)cmdAddEdit);
			((Control)PanelLunarObservations).get_Controls().Add((Control)(object)label25);
			((Control)PanelLunarObservations).set_Location(new Point(650, 70));
			((Control)PanelLunarObservations).set_Name("PanelLunarObservations");
			((Control)PanelLunarObservations).set_Size(new Size(600, 650));
			((Control)PanelLunarObservations).set_TabIndex(29);
			((Control)cmdLunarEventFromStarAltitude).set_Location(new Point(435, 371));
			((Control)cmdLunarEventFromStarAltitude).set_Name("cmdLunarEventFromStarAltitude");
			((Control)cmdLunarEventFromStarAltitude).set_Size(new Size(91, 48));
			((Control)cmdLunarEventFromStarAltitude).set_TabIndex(51);
			((Control)cmdLunarEventFromStarAltitude).set_Text("(Historical)\r\nEvent time from star altitude");
			((ButtonBase)cmdLunarEventFromStarAltitude).set_UseVisualStyleBackColor(true);
			((Control)cmdLunarEventFromStarAltitude).add_Click((EventHandler)cmdLunarEventFromStarAltitude_Click);
			((Control)cmdLunarEventFromStarAltitude).add_MouseHover((EventHandler)cmdLunarEventFromStarAltitude_MouseHover);
			((Control)cmdViewLightCurveLunar).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdViewLightCurveLunar).set_Location(new Point(218, 367));
			((Control)cmdViewLightCurveLunar).set_Name("cmdViewLightCurveLunar");
			((Control)cmdViewLightCurveLunar).set_Size(new Size(144, 56));
			((Control)cmdViewLightCurveLunar).set_TabIndex(50);
			((Control)cmdViewLightCurveLunar).set_Text("Light curve viewer");
			toolTip1.SetToolTip((Control)(object)cmdViewLightCurveLunar, "View reported occultation light curves");
			((ButtonBase)cmdViewLightCurveLunar).set_UseVisualStyleBackColor(true);
			((Control)cmdViewLightCurveLunar).add_Click((EventHandler)cmdViewLightCurveLunar_Click);
			((Control)cmdListLunarsOfK2Stars).set_Location(new Point(28, 282));
			((Control)cmdListLunarsOfK2Stars).set_Name("cmdListLunarsOfK2Stars");
			((Control)cmdListLunarsOfK2Stars).set_Size(new Size(144, 35));
			((Control)cmdListLunarsOfK2Stars).set_TabIndex(49);
			((Control)cmdListLunarsOfK2Stars).set_Text("List events involving Kepler2 stars");
			((ButtonBase)cmdListLunarsOfK2Stars).set_UseVisualStyleBackColor(true);
			((Control)cmdListLunarsOfK2Stars).add_Click((EventHandler)cmdListLunarsOfK2Stars_Click);
			((Control)cmdStarAnalysis).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdStarAnalysis).set_ForeColor(SystemColors.ControlText);
			((Control)cmdStarAnalysis).set_Location(new Point(408, 282));
			((Control)cmdStarAnalysis).set_Name("cmdStarAnalysis");
			((Control)cmdStarAnalysis).set_Size(new Size(144, 56));
			((Control)cmdStarAnalysis).set_TabIndex(48);
			((Control)cmdStarAnalysis).set_Text("Analyse for star position");
			toolTip1.SetToolTip((Control)(object)cmdStarAnalysis, "Tool to analyse corretions to a star's \r\nposition uning lunar occultations");
			((ButtonBase)cmdStarAnalysis).set_UseVisualStyleBackColor(true);
			((Control)cmdStarAnalysis).add_Click((EventHandler)cmdStarAnalysis_Click);
			((Control)cmdListMissingGrazes).set_Location(new Point(28, 326));
			((Control)cmdListMissingGrazes).set_Name("cmdListMissingGrazes");
			((Control)cmdListMissingGrazes).set_Size(new Size(144, 22));
			((Control)cmdListMissingGrazes).set_TabIndex(47);
			((Control)cmdListMissingGrazes).set_Text("List missing grazes");
			((ButtonBase)cmdListMissingGrazes).set_UseVisualStyleBackColor(true);
			((Control)cmdListMissingGrazes).add_Click((EventHandler)cmdListMissingGrazes_Click);
			((Control)cmdSolveDouble).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdSolveDouble).set_ForeColor(SystemColors.ControlText);
			((Control)cmdSolveDouble).set_Location(new Point(218, 282));
			((Control)cmdSolveDouble).set_Name("cmdSolveDouble");
			((Control)cmdSolveDouble).set_Size(new Size(144, 56));
			((Control)cmdSolveDouble).set_TabIndex(46);
			((Control)cmdSolveDouble).set_Text("Solve double star\r\nPA and separation");
			toolTip1.SetToolTip((Control)(object)cmdSolveDouble, "Tool to combine double star event \r\ntimes to determin the PA and separation\r\nof the components");
			((ButtonBase)cmdSolveDouble).set_UseVisualStyleBackColor(true);
			((Control)cmdSolveDouble).add_Click((EventHandler)cmdSolveDouble_Click);
			((Control)cmdPlotLunarAgainstLimb).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdPlotLunarAgainstLimb).set_ForeColor(SystemColors.ControlText);
			((Control)cmdPlotLunarAgainstLimb).set_Location(new Point(408, 197));
			((Control)cmdPlotLunarAgainstLimb).set_Name("cmdPlotLunarAgainstLimb");
			((Control)cmdPlotLunarAgainstLimb).set_Size(new Size(144, 56));
			((Control)cmdPlotLunarAgainstLimb).set_TabIndex(45);
			((Control)cmdPlotLunarAgainstLimb).set_Text("Plot observations\r\nagainst the\r\nLunar Limb");
			toolTip1.SetToolTip((Control)(object)cmdPlotLunarAgainstLimb, "Plot the lunar limb at a specified \r\nAxis Angle and librations");
			((ButtonBase)cmdPlotLunarAgainstLimb).set_UseVisualStyleBackColor(true);
			((Control)cmdPlotLunarAgainstLimb).add_Click((EventHandler)cmdPlotLunarAgainstLimb_Click);
			((Control)grpLunarReduceAdmin).get_Controls().Add((Control)(object)cmdVizierArchive);
			((Control)grpLunarReduceAdmin).get_Controls().Add((Control)(object)cmdReduceArchiveFile);
			((Control)grpLunarReduceAdmin).get_Controls().Add((Control)(object)cmdProcessLightCurves);
			((Control)grpLunarReduceAdmin).get_Controls().Add((Control)(object)cmdProcessRegionalFiles);
			((Control)grpLunarReduceAdmin).get_Controls().Add((Control)(object)cmdArchiveEditor);
			((Control)grpLunarReduceAdmin).set_Location(new Point(28, 472));
			((Control)grpLunarReduceAdmin).set_Name("grpLunarReduceAdmin");
			((Control)grpLunarReduceAdmin).set_Size(new Size(528, 99));
			((Control)grpLunarReduceAdmin).set_TabIndex(44);
			grpLunarReduceAdmin.set_TabStop(false);
			((Control)grpLunarReduceAdmin).set_Text("Administrator function");
			((Control)cmdVizierArchive).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdVizierArchive).set_ForeColor(SystemColors.ControlText);
			((Control)cmdVizierArchive).set_Location(new Point(312, 57));
			((Control)cmdVizierArchive).set_Name("cmdVizierArchive");
			((Control)cmdVizierArchive).set_Size(new Size(110, 34));
			((Control)cmdVizierArchive).set_TabIndex(30);
			((Control)cmdVizierArchive).set_Text("Create Archive for VizieR");
			((ButtonBase)cmdVizierArchive).set_UseVisualStyleBackColor(true);
			((Control)cmdVizierArchive).add_Click((EventHandler)cmdVizierArchive_Click);
			((Control)cmdReduceArchiveFile).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdReduceArchiveFile).set_ForeColor(SystemColors.ControlText);
			((Control)cmdReduceArchiveFile).set_Location(new Point(312, 14));
			((Control)cmdReduceArchiveFile).set_Name("cmdReduceArchiveFile");
			((Control)cmdReduceArchiveFile).set_Size(new Size(110, 34));
			((Control)cmdReduceArchiveFile).set_TabIndex(29);
			((Control)cmdReduceArchiveFile).set_Text("Reduce an Archive file");
			((ButtonBase)cmdReduceArchiveFile).set_UseVisualStyleBackColor(true);
			((Control)cmdReduceArchiveFile).add_Click((EventHandler)cmdReduceArchiveFile_Click);
			((Control)cmdProcessLightCurves).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdProcessLightCurves).set_ForeColor(SystemColors.ControlText);
			((Control)cmdProcessLightCurves).set_Location(new Point(183, 26));
			((Control)cmdProcessLightCurves).set_Name("cmdProcessLightCurves");
			((Control)cmdProcessLightCurves).set_Size(new Size(110, 55));
			((Control)cmdProcessLightCurves).set_TabIndex(28);
			((Control)cmdProcessLightCurves).set_Text("Process new light curves");
			((ButtonBase)cmdProcessLightCurves).set_UseVisualStyleBackColor(true);
			((Control)cmdProcessLightCurves).add_Click((EventHandler)cmdProcessLightCurves_Click);
			((Control)cmdProcessRegionalFiles).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdProcessRegionalFiles).set_ForeColor(SystemColors.ControlText);
			((Control)cmdProcessRegionalFiles).set_Location(new Point(20, 26));
			((Control)cmdProcessRegionalFiles).set_Name("cmdProcessRegionalFiles");
			((Control)cmdProcessRegionalFiles).set_Size(new Size(144, 56));
			((Control)cmdProcessRegionalFiles).set_TabIndex(27);
			((Control)cmdProcessRegionalFiles).set_Text("Process files from regional coordinators");
			((ButtonBase)cmdProcessRegionalFiles).set_UseVisualStyleBackColor(true);
			((Control)cmdProcessRegionalFiles).add_Click((EventHandler)cmdProcessRegionalFiles_Click);
			((Control)cmdArchiveEditor).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdArchiveEditor).set_ForeColor(SystemColors.ControlText);
			((Control)cmdArchiveEditor).set_Location(new Point(441, 26));
			((Control)cmdArchiveEditor).set_Name("cmdArchiveEditor");
			((Control)cmdArchiveEditor).set_Size(new Size(79, 56));
			((Control)cmdArchiveEditor).set_TabIndex(26);
			((Control)cmdArchiveEditor).set_Text("Archive Editor");
			((ButtonBase)cmdArchiveEditor).set_UseVisualStyleBackColor(true);
			((Control)cmdArchiveEditor).add_Click((EventHandler)cmdArchiveEditor_Click);
			((Control)button3).set_Location(new Point(188, 438));
			((Control)button3).set_Name("button3");
			((Control)button3).set_Size(new Size(205, 24));
			((Control)button3).set_TabIndex(42);
			((Control)button3).set_Text("Close all Lunar Observation forms");
			((ButtonBase)button3).set_UseVisualStyleBackColor(true);
			((Control)button3).add_Click((EventHandler)button3_Click);
			((Control)cmdViewHistoricalOccults).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdViewHistoricalOccults).set_ForeColor(SystemColors.ControlText);
			((Control)cmdViewHistoricalOccults).set_Location(new Point(28, 197));
			((Control)cmdViewHistoricalOccults).set_Name("cmdViewHistoricalOccults");
			((Control)cmdViewHistoricalOccults).set_Size(new Size(144, 56));
			((Control)cmdViewHistoricalOccults).set_TabIndex(21);
			((Control)cmdViewHistoricalOccults).set_Text("View / analyse\r\nhistorical Occultations\r\n1623 - now");
			toolTip1.SetToolTip((Control)(object)cmdViewHistoricalOccults, "Tool for analysing/extracting data \r\nfor lunar occultations");
			((ButtonBase)cmdViewHistoricalOccults).set_UseVisualStyleBackColor(true);
			((Control)cmdViewHistoricalOccults).add_Click((EventHandler)cmdViewHistoricalOccults_Click);
			((Control)cmdViewHistoricalGrazes).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdViewHistoricalGrazes).set_ForeColor(SystemColors.ControlText);
			((Control)cmdViewHistoricalGrazes).set_Location(new Point(218, 197));
			((Control)cmdViewHistoricalGrazes).set_Name("cmdViewHistoricalGrazes");
			((Control)cmdViewHistoricalGrazes).set_Size(new Size(144, 56));
			((Control)cmdViewHistoricalGrazes).set_TabIndex(20);
			((Control)cmdViewHistoricalGrazes).set_Text("View / analyse\r\nhistorical Grazes\r\n1706 - now");
			toolTip1.SetToolTip((Control)(object)cmdViewHistoricalGrazes, "Tool for viewing the results of \r\ngrazing ocultations, including \r\nplots against the lunar limb");
			((ButtonBase)cmdViewHistoricalGrazes).set_UseVisualStyleBackColor(true);
			((Control)cmdViewHistoricalGrazes).add_Click((EventHandler)cmdViewHistoricalGrazes_Click);
			((Control)cmdAddEdit).set_Font(new Font("Microsoft Sans Serif", 11f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdAddEdit).set_Location(new Point(198, 72));
			((Control)cmdAddEdit).set_Name("cmdAddEdit");
			((Control)cmdAddEdit).set_Size(new Size(185, 96));
			((Control)cmdAddEdit).set_TabIndex(18);
			((Control)cmdAddEdit).set_Text("Add / Edit / Plot\r\nobserved\r\nLunar Occultations");
			((ButtonBase)cmdAddEdit).set_UseVisualStyleBackColor(true);
			((Control)cmdAddEdit).add_Click((EventHandler)cmdAddEdit_Click);
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Font(new Font("Times New Roman", 18f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label25).set_Location(new Point(118, 18));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(345, 26));
			((Control)label25).set_TabIndex(16);
			((Control)label25).set_Text("Lunar Occultation Observations");
			((Control)toolStrip1).set_AutoSize(false);
			toolStrip1.set_ImageScalingSize(new Size(20, 20));
			((Control)toolStrip1).set_Location(new Point(0, 0));
			((Control)toolStrip1).set_Name("toolStrip1");
			((Control)toolStrip1).set_Size(new Size(100, 25));
			((Control)toolStrip1).set_TabIndex(0);
			((ToolStripItem)StripAsteroids).set_Name("StripAsteroids");
			((ToolStripItem)StripAsteroids).set_Size(new Size(23, 23));
			((ToolStripItem)StripMPReduce).set_Name("StripMPReduce");
			((ToolStripItem)StripMPReduce).set_Size(new Size(23, 23));
			((ToolStripItem)StripEclipses).set_Name("StripEclipses");
			((ToolStripItem)StripEclipses).set_Size(new Size(23, 23));
			((ToolStripItem)StripEphem).set_Name("StripEphem");
			((ToolStripItem)StripEphem).set_Size(new Size(23, 23));
			((ToolStripItem)StripLunar).set_Name("StripLunar");
			((ToolStripItem)StripLunar).set_Size(new Size(23, 23));
			((ToolStripItem)StripLunarObservations).set_Name("StripLunarObservations");
			((ToolStripItem)StripLunarObservations).set_Size(new Size(23, 23));
			((ToolStripItem)StripSatellites).set_Name("StripSatellites");
			((ToolStripItem)StripSatellites).set_Size(new Size(23, 23));
			((ToolStripItem)StripShared).set_Name("StripShared");
			((ToolStripItem)StripShared).set_Size(new Size(23, 23));
			((ToolStripItem)toolStripButton7).set_Name("toolStripButton7");
			((ToolStripItem)toolStripButton7).set_Size(new Size(23, 23));
			((Control)lblEphemerisSource).set_BackColor(SystemColors.Control);
			((Control)lblEphemerisSource).set_Font(new Font("Times New Roman", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblEphemerisSource).set_Location(new Point(5, 644));
			((Control)lblEphemerisSource).set_Name("lblEphemerisSource");
			((Control)lblEphemerisSource).set_Size(new Size(210, 13));
			((Control)lblEphemerisSource).set_TabIndex(31);
			((Control)lblEphemerisSource).set_Text("VSOP87A");
			lblEphemerisSource.set_TextAlign(ContentAlignment.MiddleLeft);
			toolTip1.set_AutomaticDelay(100);
			toolTip1.set_AutoPopDelay(15000);
			toolTip1.set_InitialDelay(40);
			toolTip1.set_IsBalloon(true);
			toolTip1.set_ReshowDelay(20);
			((Control)chkHighPriority).set_AutoSize(true);
			((Control)chkHighPriority).set_BackColor(SystemColors.Control);
			chkHighPriority.set_Checked(Occult.Properties.Settings.Default.RunWithHighPriority);
			chkHighPriority.set_CheckState((CheckState)1);
			((Control)chkHighPriority).get_DataBindings().Add(new Binding("Checked", (object)Occult.Properties.Settings.Default, "RunWithHighPriority", true, (DataSourceUpdateMode)1));
			((Control)chkHighPriority).set_Font(new Font("Times New Roman", 8f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkHighPriority).set_ForeColor(Color.Maroon);
			((Control)chkHighPriority).set_Location(new Point(3, 578));
			((Control)chkHighPriority).set_Name("chkHighPriority");
			((Control)chkHighPriority).set_Size(new Size(147, 17));
			((Control)chkHighPriority).set_TabIndex(32);
			((Control)chkHighPriority).set_Text("Run with High Priority");
			toolTip1.SetToolTip((Control)(object)chkHighPriority, "Changes to Priority only have effect when Occult is next run.");
			((ButtonBase)chkHighPriority).set_UseVisualStyleBackColor(false);
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)0);
			((Form)this).set_ClientSize(new Size(750, 659));
			((Control)this).get_Controls().Add((Control)(object)PanelAsteroidObservations);
			((Control)this).get_Controls().Add((Control)(object)chkHighPriority);
			((Control)this).get_Controls().Add((Control)(object)PanelAsteroidPredictions);
			((Control)this).get_Controls().Add((Control)(object)lblEphemerisSource);
			((Control)this).get_Controls().Add((Control)(object)PanelLunarObservations);
			((Control)this).get_Controls().Add((Control)(object)PanelMaintenance);
			((Control)this).get_Controls().Add((Control)(object)PanelEphemeris);
			((Control)this).get_Controls().Add((Control)(object)PanelLunarPredictions);
			((Control)this).get_Controls().Add((Control)(object)PanelSatellites);
			((Control)this).get_Controls().Add((Control)(object)PanelEclipsesTransits);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)MainToolStrip);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Occult.Properties.Settings.Default, "LocationMainForm", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_HelpButton(true);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Occult.Properties.Settings.Default.LocationMainForm);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Control)this).set_Name("frmOccult");
			((Control)this).set_Text("\\");
			((Form)this).add_FormClosing(new FormClosingEventHandler(frmOccult_FormClosing));
			((Form)this).add_FormClosed(new FormClosedEventHandler(frmOccult_FormClosed));
			((Form)this).add_Load((EventHandler)frmOccult_Load);
			((Control)grpEphemeris).ResumeLayout(false);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)MainToolStrip).ResumeLayout(false);
			((Control)MainToolStrip).PerformLayout();
			((Control)PanelAsteroidPredictions).ResumeLayout(false);
			((Control)PanelAsteroidPredictions).PerformLayout();
			((Control)groupBox8).ResumeLayout(false);
			((Control)groupBox11).ResumeLayout(false);
			((Control)groupBox11).PerformLayout();
			((Control)groupBox10).ResumeLayout(false);
			((Control)groupBox10).PerformLayout();
			((Control)groupBox7).ResumeLayout(false);
			((Control)groupBox7).PerformLayout();
			((Control)PanelEphemeris).ResumeLayout(false);
			((Control)PanelEphemeris).PerformLayout();
			((Control)groupBox12).ResumeLayout(false);
			((Control)grpShapeModels).ResumeLayout(false);
			((Control)groupBox6).ResumeLayout(false);
			((Control)groupBox4).ResumeLayout(false);
			((Control)groupBox3).ResumeLayout(false);
			((Control)groupBox2).ResumeLayout(false);
			((Control)groupBox1).ResumeLayout(false);
			((Control)PanelSatellites).ResumeLayout(false);
			((Control)PanelSatellites).PerformLayout();
			((Control)PanelMaintenance).ResumeLayout(false);
			((Control)PanelMaintenance).PerformLayout();
			((Control)grpNomad).ResumeLayout(false);
			((Control)grpNomad).PerformLayout();
			((Control)PanelAsteroidObservations).ResumeLayout(false);
			((Control)PanelAsteroidObservations).PerformLayout();
			((Control)groupBox9).ResumeLayout(false);
			((Control)groupBox5).ResumeLayout(false);
			((Control)groupBox5).PerformLayout();
			((Control)grpMaintainObsFile).ResumeLayout(false);
			((Control)grpAsteroidLists).ResumeLayout(false);
			((Control)PanelEclipsesTransits).ResumeLayout(false);
			((Control)PanelEclipsesTransits).PerformLayout();
			((Control)PanelLunarPredictions).ResumeLayout(false);
			((Control)PanelLunarPredictions).PerformLayout();
			((Control)PanelLunarObservations).ResumeLayout(false);
			((Control)PanelLunarObservations).PerformLayout();
			((Control)grpLunarReduceAdmin).ResumeLayout(false);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
