using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.IO.Compression;
using System.Net;
using System.Text;
using System.Windows.Forms;
using Occult.Asteroid_Observations;
using Occult.Lunar_Observations;
using Occult.Properties;
using Occult.Star_Catalogues;
using Shapes;

namespace Occult.File_Actions
{
	public class Downloads : Form
	{
		public static string DownloadContolFile = Utilities.AppPath + "\\Resource Files\\DownloadControl.txt";

		public static string EOPpresentFile = Utilities.AppPath + "\\Resource Files\\EOP_present.dat";

		public static string CometFile = Utilities.AppPath + "\\Downloaded Files\\Comet.dat";

		public static string DeltaTAFile = Utilities.AppPath + "\\Resource Files\\DeltaTA.dat";

		public static string AsteroidObsFile = Utilities.AppPath + "\\Resource Files\\" + Utilities.AsteroidObservationsFile;

		public static string LunarRecentFile = Utilities.AppPath + "\\Resource Files\\Archive Observations recent.dat";

		public static string BinaryAsteroidFile = Utilities.AppPath + "\\Resource Files\\BinaryAsteroids.csv";

		public static string AsteroidRingFile = Utilities.AppPath + "\\Resource Files\\AsteroidRings.csv";

		public static string AsteroidDiametersFile = Utilities.AppPath + "\\Resource Files\\AsteroidDiameters.bin";

		public static string AsteroidClassFile = Utilities.AppPath + "\\Resource Files\\AsteroidClasses.csv";

		public static string FutureFile = Utilities.AppPath + "\\Generated Files\\future.xml";

		public static string ReportingAddressFile = Utilities.AppPath + "\\Resource Files\\addresses.txt";

		public static string CameraDelayFile = Utilities.AppPath + "\\Resource Files\\CameraDelays.dat";

		public static string WDSfile = Utilities.AppPath + "\\DownLoaded Files\\wds.dat";

		public static string WDScodesFile = Utilities.AppPath + "\\Resource Files\\WDS Discovery Codes.dat";

		public static string Int4File = Utilities.AppPath + "\\DownLoaded Files\\InterferometricCat.dat";

		public static string AAVSOindexFile = Utilities.AppPath + "\\DownLoaded Files\\AAVSOindex.dat";

		public static string AAVSOFullFile = Utilities.AppPath + "\\DownLoaded Files\\vsx_csv_All.dat";

		public static string CALLfile = Utilities.AppPath + "\\Downloaded Files\\LC_Summary_Pub.txt";

		public static string DoubleOrbitFile = Utilities.AppPath + "\\DownLoaded Files\\SixthOrbitCatalogue.txt";

		public static string Kepler2File = Utilities.AppPath + "\\Resource Files\\Kepler2.dat";

		public static string FutureAllFile = Utilities.AppPath + "\\Generated Files\\FutureAll.xml";

		public static string TNO_RIOFile = Utilities.AppPath + "\\Generated Files\\TNOs_RIO.dat";

		public static string AstorbFile = Utilities.AppPath + "\\DownLoaded Files\\astorb.dat";

		public static string MPCorbFile = Utilities.AppPath + "\\DownLoaded Files\\MPCORB.DAT";

		public static string AstDysFile = Utilities.AppPath + "\\DownLoaded Files\\AstDys2.dat";

		public static string ISAMFile = Utilities.AppPath + "\\ShapeModels\\ISAM_AvailableAsteroids.csv";

		public static string ShapeFile = Utilities.AppPath + "\\ShapeModels\\DamitModels.csv";

		public static string LunarOldFiles = Utilities.AppPath + "\\Resource Files\\RArchive Observations 2016_2025.dat";

		public static string XZfile = Utilities.AppPath + "\\Resource Files\\xz80.dat";

		public static string LightCurveFile = Utilities.AppPath + "\\LightCurves\\LightCurves.txt";

		public static string Lola128File = Utilities.AppPath + "\\Resource Files\\Lola128.bin";

		public static string StarDiaFile = Utilities.AppPath + "\\Resource Files\\StarDia.bin";

		public static string Tycho2File = Utilities.AppPath + "\\Resource Files\\Tycho2.bin";

		public static string JPL_DEfile = Utilities.AppPath + "\\Resource Files\\DE_Ephemeris.bin";

		public static string JPL6000File = Utilities.AppPath + "\\Resource Files\\DE_LongEphemeris.bin";

		public static string EOPoldFile = Utilities.AppPath + "\\Resource Files\\EOP_old.dat";

		public static string CraterCoordsFile = Utilities.AppPath + "\\Resource Files\\Crater.bin";

		public static string CraterTimingsFile = Utilities.AppPath + "\\Resource Files\\CraterTimings.bin";

		public static string PhoebeFile = Utilities.AppPath + "\\Resource Files\\SaturnIX.bin";

		public static string GMTemplatesFile = Utilities.AppPath + "\\Resource Files\\GoogleMap_FileHeader.txt";

		public static string Gaia9File = Utilities.AppPath + "\\Resource Files\\Gaia\\Gaia9_DR3.bin";

		public static string Gaia12File = Utilities.AppPath + "\\Resource Files\\Gaia\\Gaia12_DR3.bin";

		public static string Gaia14File = Utilities.AppPath + "\\Resource Files\\Gaia\\Gaia14_DR3.bin";

		public static string Gaia16File = Utilities.AppPath + "\\Resource Files\\Gaia\\Gaia16_DR3.bin";

		public static string UBSCfile = Utilities.AppPath + "\\Resource Files\\Gaia\\UBSC.txt";

		public static string SAOFile = Utilities.AppPath + "\\Resource Files\\sao1950.bin";

		public static string Earth2014File = Utilities.AppPath + "\\Resource Files\\Earth2014.SUR2014.1min.geod.bin";

		public static string GaiaDoublesFile = Utilities.AppPath + "\\Resource Files\\Gaia\\Gaia_Doubles.csv";

		private ConvertAstorb_etc Astorb;

		private int DownloadAgeSelectedIndex;

		private int[] DownloadAgeMonth = new int[5] { 1, 3, 6, 12, 60 };

		private IContainer components;

		private Button cmdComet;

		private Button cmdFuture;

		private Label label1;

		private Label label2;

		private Label label3;

		private Button cmdFuture540;

		private Label label4;

		private Button cmdAstorb;

		private Button cmdAstorbVizier;

		private Label label6;

		private Button cmdMPCorbCzech;

		private Label label7;

		private Button cmdMPCorb;

		private Label label8;

		private Button cmdCancel;

		private Button cmdDowload_EOP_new;

		private Button cmdDowload_EOP_Old;

		private Label label9;

		private Label label10;

		private Label label11;

		private Button cmdLunar;

		private Label label12;

		private Button cmdTycho2;

		private Label label13;

		private Button cmdJPL_DE;

		private Label label14;

		private Button cmdAsteroidObservations;

		private GroupBox grpLargeButSeldom;

		private Label label17;

		private Label label16;

		private Label label15;

		private GroupBox grpAsteroids;

		private Label label19;

		private Label label18;

		private Label label20;

		private Label label21;

		private GroupBox grpGeneral;

		private Label label24;

		private Label label23;

		private Label label22;

		private Label label25;

		private Label label26;

		private Label label27;

		private Button cmdAddresses;

		private Button cmdHelp;

		private Label label28;

		private Label label29;

		private Button cmdDeltaT;

		private Label label30;

		private Label label31;

		private Button cmdLatestLunar;

		private Label label33;

		private Label label34;

		private Button cmdXZ;

		private ImageList imageList1;

		private Button DownloadFlagLunarOld;

		private Button DownloadFlagJPLDE;

		private Button DownloadFlagTycho2;

		private Button DownloadFlagXZ;

		private Button cmdExit;

		private Panel panel1;

		private Label label35;

		private Label label36;

		private Button cmdBinary;

		private Label label37;

		private Label label38;

		private Button cmdAsteroidDiameters;

		private Button DownloadFlagAsteroidDiameters;

		private Button DownloadFlagBinary;

		private Button DownloadFlagdeltaT;

		private Label label39;

		private Label label41;

		private Label label40;

		private Button cmdDownloadAll;

		private Label lblRetainFuture;

		private Label lblRetainFutureAll;

		private Label label55;

		private Button cmdDownloadAstDyS;

		private Label label58;

		private Button DownloadFlagAsteroids;

		private Button DownloadFlagRecentLunar;

		private Button DownloadFlagAddresses;

		private Button DownloadFlagAAVSO;

		private Label label49;

		private Label label48;

		private Button cmdDownloadAAVSO;

		private Button DownloadFlagWDS;

		private Button DownloadFlagIF;

		private Label label44;

		private Label label45;

		private Button cmdDownloadWDS;

		private Label label42;

		private Label label43;

		private Button cmdDownloadInterferometric;

		private Label label59;

		private ComboBox cmbFileAge;

		private Button DownloadFlagStarDias;

		private Label label61;

		private Label label62;

		private Button cmdStarDiameters;

		private Button cmdDownloadAsteroidLightCurveData;

		private Button downloadFlag_CALL;

		private Label label64;

		private Label label63;

		private Label lblUpdateCount;

		private Panel panel2;

		private Button DownloadFlagVisualBinaries;

		private Label label66;

		private Label label67;

		private Button cmdDownloadVisualBinaries;

		private Label label68;

		private Label label69;

		private Button cmdDE422;

		private Label label70;

		private Label label73;

		private Button cmdCreateISAM;

		private Label label75;

		private Button DownLoadFlagCraterTimings;

		private Label label76;

		private Label label77;

		private Button cmdDownloadCraterTimings;

		private Button DownloadFlagPhoebe;

		private Label label71;

		private Label label78;

		private Button cmdDownloadePhoebeEphemeris;

		private Label label86;

		private Label label85;

		private Label label84;

		private Label label83;

		private Label label82;

		private Label label81;

		private Label label80;

		private Label label79;

		private Label label89;

		private Label label91;

		private Label label90;

		private Label label94;

		private Label label93;

		private Label label92;

		private Label label97;

		private Label label96;

		private Label label95;

		private Label label103;

		private Label label105;

		private Label label104;

		private Label label102;

		private Label label101;

		private Label label100;

		private Label label99;

		private Label label98;

		private Button DownloadFlagCameraDelay;

		private Label label107;

		private Label label108;

		private Button cmdCameraDelays;

		private Label label106;

		private Label label109;

		private Label label110;

		private Button cmdDownloadGoogleMapFiles;

		private Label label117;

		private Label label116;

		private Label label115;

		private Button cmdRings;

		private Button DownloadFlagRings;

		private Label label118;

		private Label label119;

		private Button cmdDownloadWDSCodes;

		private Label label120;

		private Button DownloadFlagWDScodes;

		private Label lblCometFile;

		private Label lblWDSFile;

		private Label lblWDSCodesFile;

		private Label lblINT4File;

		private Label lblAAVSOfile;

		private Label lblCALLfile;

		private Label lbl6thOrbitFile;

		private Label lblDeltaTAfile;

		private Label lblAsteroidObsFile;

		private Label lblLatestLunarFile;

		private Label lblBinaryAsteroidFile;

		private Label lblAsteroidRingFile;

		private Label lblAsteroidDiameterFile;

		private Label lblFutureFile;

		private Label lblEOPPresentFile;

		private Label lblGoogleMapTemplatesFile;

		private Label lblPhoebeFile;

		private Label lblEOP_OldFile;

		private Label lblCraterFile;

		private Label lblJPL_DEfile;

		private Label lblDE6000file;

		private Label lblTycho2File;

		private Label lblStarDiaFile;

		private Label lblXZcatFile;

		private Label lblLunarOldFiles;

		private Label lblFutureAllFile;

		private Label lblAstorbFile;

		private Label lblMPCfile;

		private Label lblAstDysFile;

		private Label lblISAMfile;

		private Label lblCameraDelayFile;

		private Label lblReportAddressFile;

		private Panel panel3;

		private Label lblKepler2File;

		private Label label46;

		private Button DownloadFlagKepler2;

		private Label label47;

		private Label label54;

		private Button cmdDownloadKepler2;

		private Label lblLOLA128File;

		private Label label56;

		private Button DownloadFlagLOLA128;

		private Label label60;

		private Label label65;

		private Button cmdLOLA;

		private Button cmdCreateLOLA128;

		private Label lblLola;

		private Label label121;

		private Label label32;

		private Label label74;

		private Button cmdDownloadInstallationFiles;

		private Label lblLightCurves;

		private Label label123;

		private Button DownloadFlagLightCurves;

		private Label label124;

		private Label label125;

		private Button cmdDowloadLightCurves;

		private Label label126;

		private Label label127;

		private Label label128;

		private Button cmdDownloadISAM;

		private Label label5;

		private Panel panel4;

		private Button DownLoadFlagISAM;

		private Label label136;

		private Label lblGaia16;

		private Label lblGaia12;

		private Button DownloadFlagGaia12;

		private Label label131;

		private Label label132;

		private Label label133;

		private Button cmdDownloadGaia16;

		private Label label72;

		private Label label129;

		private Label label130;

		private Button cmdDownloadGaia12;

		private Button DownLoadFlagAstDyS;

		private Label lblAstDySUpdateDate;

		private Label label134;

		private Label label57;

		private Label label135;

		private Label lblSAO;

		private Label label138;

		private Label label139;

		private Label label140;

		private Button cmdSAO;

		private Label lblAsteroidClassFile;

		private Label label141;

		private Button DownloadFlagAsteroidClass;

		private Label label142;

		private Label label143;

		private Button cmdAsteroidClass;

		private Label label137;

		private Button cmdDownloadGaia;

		private Label lblGaiaFile;

		private Label label145;

		private Button DownloadFlagGaia16;

		private Label label144;

		private Button DownLoadFlagShapes;

		private Label lblShapeModels;

		private Label label147;

		private Label label148;

		private Button cmdDownloadShapes;

		private Label label149;

		private Button DownloadFlagGaia14;

		private Label label146;

		private Label lblGaia14;

		private Label label151;

		private Label label152;

		private Label label153;

		private Button cmdDownloadGaia14;

		private Label lblUBSC;

		private Label label52;

		private Label label53;

		private Label label87;

		private Button cmdUBSC;

		private Label label113;

		private Label lblEarth2014;

		private Label label88;

		private Label label111;

		private Label label112;

		private Button cmdEarth2014;

		private Label lblGaiaDoubles;

		private Label label51;

		private Label label114;

		private Label label122;

		private Button cmdGaiaDoubles;

		private Label lblGaia9;

		private Button DownloadFlagGaia9;

		private Label label150;

		private Label label154;

		private Label label155;

		private Button cmdDownloadGaia9;

		private Label label50;

		private Label label156;

		private NumericUpDown updnMagLimitAAVSO;

		private Label label157;

		private NumericUpDown updnMaxMagLimit;

		private Label lblAAVSOfile_Occult;

		private Button DownloadFlagAAVSO_Occult;

		private Label label159;

		private Label label160;

		private Button cmdDownloadAAVSO_Occult;

		private Label label161;

		private Button cmdUpdateDAMIT;

		private Label label162;

		private Label label158;

		private Label lblUpdateDamitPreferred;

		private Label label163;

		private Panel panel5;

		private Label label165;

		public Downloads()
		{
			InitializeComponent();
			Settings.Default.UpdatePageLastDisplayed = DateTime.Now;
		}

		private void Downloads_Load(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("The Downloads functionality requires internet access. You are not connected to the internet.\r\n\r\nYou must be connect to the internet before the Downloads page\r\ncan be accessed.", "No internet");
				((Form)this).Close();
			}
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			((Control)this).set_Height((int)((double)Screen.GetWorkingArea((Control)(object)this).Height * 0.85));
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			((Control)grpAsteroids).set_Top(((Control)grpGeneral).get_Top() + ((Control)grpGeneral).get_Height() + 30);
			((Control)grpLargeButSeldom).set_Top(((Control)grpAsteroids).get_Top() + ((Control)grpAsteroids).get_Height() + 30);
			DownloadAgeSelectedIndex = Settings.Default.DownloadFileAge;
			((ListControl)cmbFileAge).set_SelectedIndex(DownloadAgeSelectedIndex);
			((Control)cmdCreateLOLA128).set_Visible(Settings.Default.Administrator);
			http.DownloadHTTP(Settings.Default.OccultServer, "DownloadControl.txt", DownloadContolFile, unzip: false, gunzip: false, ShowMessages: false);
			SetFileDates();
			SetDownloadFlags();
			Label obj = lblRetainFuture;
			bool preserveFuture_dat;
			((Control)lblRetainFutureAll).set_Visible(preserveFuture_dat = Settings.Default.PreserveFuture_dat);
			((Control)obj).set_Visible(preserveFuture_dat);
		}

		private void SetDAMITdownload()
		{
			int num = new DirectoryInfo(Utilities.AppPath + "\\ShapeModels\\").GetFiles().Length;
			((Control)cmdUpdateDAMIT).set_Enabled(num > 15000);
		}

		private void SetDownloadFlags()
		{
			UpdatesAvailable.UpdateAvailable(out var Aster, out var Lunar, out var Address, out var dT, out var BinaryAst, out var AsterRings, out var AsterDia, out var AsterClass, out var ISAM, out var Shapes, out var LunarArchive, out var XZcat, out var LightCurve, out var _, out var DE, out var Lola, out var StarDias, out var CameraDelays, out var Kepler, out var Gaia, out var Gaia2, out var Gaia3, out var Gaia4, out var AstDyS, out var AAVSO_Occult);
			((Control)DownloadFlagAsteroids).set_Visible(Aster);
			((Control)DownloadFlagdeltaT).set_Visible(dT);
			((Control)DownloadFlagRecentLunar).set_Visible(Lunar);
			((Control)DownloadFlagBinary).set_Visible(BinaryAst);
			((Control)DownloadFlagRings).set_Visible(AsterRings);
			((Control)DownloadFlagAsteroidClass).set_Visible(AsterClass);
			((Control)DownloadFlagAsteroidDiameters).set_Visible(AsterDia);
			((Control)DownLoadFlagISAM).set_Visible(ISAM);
			((Control)DownLoadFlagShapes).set_Visible(Shapes);
			((Control)DownLoadFlagAstDyS).set_Visible(AstDyS);
			if (UpdatesAvailable.AstDyS2Date < DateTime.Now.AddDays(-2.0))
			{
				((Control)lblAstDySUpdateDate).set_Text("Date of last update at AstDyS-2: " + UpdatesAvailable.AstDyS2Date.Year + " " + Utilities.ShortMonths[UpdatesAvailable.AstDyS2Date.Month] + " " + UpdatesAvailable.AstDyS2Date.Day);
			}
			else
			{
				((Control)lblAstDySUpdateDate).set_Text("");
			}
			((Control)DownloadFlagAddresses).set_Visible(Address);
			((Control)DownloadFlagLunarOld).set_Visible(LunarArchive);
			((Control)DownloadFlagXZ).set_Visible(XZcat);
			((Control)DownloadFlagLightCurves).set_Visible(LightCurve);
			((Control)DownloadFlagTycho2).set_Visible(false);
			((Control)DownloadFlagGaia16).set_Visible(Gaia4 && Gaia3 && Gaia2 && Gaia);
			((Control)DownloadFlagGaia14).set_Visible(Gaia4 && Gaia3 && Gaia2 && Gaia);
			((Control)DownloadFlagGaia12).set_Visible(Gaia4 && Gaia3 && Gaia2 && Gaia);
			((Control)DownloadFlagGaia9).set_Visible(Gaia4 && Gaia3 && Gaia2 && Gaia);
			((Control)DownloadFlagAAVSO_Occult).set_Visible(AAVSO_Occult);
			((Control)DownloadFlagJPLDE).set_Visible(DE);
			((Control)DownloadFlagLOLA128).set_Visible(Lola);
			((Control)DownloadFlagStarDias).set_Visible(StarDias);
			((Control)DownloadFlagCameraDelay).set_Visible(CameraDelays);
			((Control)DownLoadFlagCraterTimings).set_Visible(!File.Exists(CraterTimingsFile));
			((Control)DownloadFlagPhoebe).set_Visible(!File.Exists(PhoebeFile));
			if (File.Exists(Kepler2File))
			{
				((Control)DownloadFlagKepler2).set_Visible(Kepler);
			}
			else
			{
				((Control)DownloadFlagKepler2).set_Visible(true);
			}
			int num = -DownloadAgeMonth[DownloadAgeSelectedIndex];
			DateTime dateTime = DateTime.Now.AddMonths(num);
			DateTime value = dateTime;
			if (num > -15)
			{
				value = DateTime.Now.AddMonths(-15);
			}
			if (File.Exists(WDSfile))
			{
				((Control)DownloadFlagWDS).set_Visible(File.GetLastWriteTime(WDSfile).CompareTo(dateTime) < 0);
			}
			else
			{
				((Control)DownloadFlagWDS).set_Visible(true);
			}
			if (File.Exists(WDScodesFile))
			{
				((Control)DownloadFlagWDScodes).set_Visible(File.GetLastWriteTime(WDScodesFile).CompareTo(dateTime) < 0);
			}
			else
			{
				((Control)DownloadFlagWDScodes).set_Visible(true);
			}
			if (File.Exists(AAVSOindexFile))
			{
				((Control)DownloadFlagAAVSO_Occult).set_Visible(File.GetLastWriteTime(AAVSOindexFile).CompareTo(DateTime.Now.AddMonths(-12)) < 0);
			}
			else
			{
				((Control)DownloadFlagAAVSO).set_Visible(true);
			}
			if (File.Exists(AAVSOFullFile))
			{
				((Control)DownloadFlagAAVSO).set_Visible(File.GetLastWriteTime(AAVSOFullFile).CompareTo(DateTime.Now.AddMonths(-12)) < 0);
			}
			else
			{
				((Control)DownloadFlagAAVSO).set_Visible(true);
			}
			if (File.Exists(CALLfile))
			{
				((Control)downloadFlag_CALL).set_Visible(File.GetLastWriteTime(CALLfile).CompareTo(value) < 0);
			}
			else
			{
				((Control)downloadFlag_CALL).set_Visible(true);
			}
			if (File.Exists(DoubleOrbitFile))
			{
				((Control)DownloadFlagVisualBinaries).set_Visible(File.GetLastWriteTime(DoubleOrbitFile).CompareTo(dateTime) < 0);
			}
			else
			{
				((Control)DownloadFlagVisualBinaries).set_Visible(true);
			}
			CountUpdateTags();
			SetDAMITdownload();
		}

		private void CountUpdateTags()
		{
			int num = 0;
			if (((Control)DownloadFlagAsteroids).get_Visible())
			{
				num++;
			}
			if (((Control)DownloadFlagdeltaT).get_Visible())
			{
				num++;
			}
			if (((Control)DownloadFlagRecentLunar).get_Visible())
			{
				num++;
			}
			if (((Control)DownloadFlagBinary).get_Visible())
			{
				num++;
			}
			if (((Control)DownloadFlagAsteroidDiameters).get_Visible())
			{
				num++;
			}
			if (((Control)DownloadFlagAsteroidClass).get_Visible())
			{
				num++;
			}
			if (((Control)DownLoadFlagISAM).get_Visible())
			{
				num++;
			}
			if (((Control)DownLoadFlagShapes).get_Visible())
			{
				num++;
			}
			if (((Control)DownloadFlagAddresses).get_Visible())
			{
				num++;
			}
			if (((Control)DownloadFlagLunarOld).get_Visible())
			{
				num++;
			}
			if (((Control)DownloadFlagXZ).get_Visible())
			{
				num++;
			}
			if (((Control)DownloadFlagLightCurves).get_Visible())
			{
				num++;
			}
			if (((Control)DownloadFlagTycho2).get_Visible())
			{
				num++;
			}
			if (((Control)DownloadFlagGaia12).get_Visible())
			{
				num++;
			}
			if (((Control)DownloadFlagGaia14).get_Visible())
			{
				num++;
			}
			if (((Control)DownloadFlagGaia16).get_Visible())
			{
				num++;
			}
			if (((Control)DownloadFlagJPLDE).get_Visible())
			{
				num++;
			}
			if (((Control)DownloadFlagLOLA128).get_Visible())
			{
				num++;
			}
			if (((Control)DownloadFlagStarDias).get_Visible())
			{
				num++;
			}
			if (((Control)DownloadFlagAAVSO).get_Visible())
			{
				num++;
			}
			if (((Control)DownloadFlagWDS).get_Visible())
			{
				num++;
			}
			if (((Control)DownloadFlagWDScodes).get_Visible())
			{
				num++;
			}
			if (((Control)DownloadFlagAAVSO).get_Visible())
			{
				num++;
			}
			if (((Control)downloadFlag_CALL).get_Visible())
			{
				num++;
			}
			if (((Control)DownloadFlagVisualBinaries).get_Visible())
			{
				num++;
			}
			if (((Control)DownLoadFlagCraterTimings).get_Visible())
			{
				num++;
			}
			if (((Control)DownloadFlagPhoebe).get_Visible())
			{
				num++;
			}
			if (((Control)DownloadFlagKepler2).get_Visible())
			{
				num++;
			}
			if (((Control)DownLoadFlagAstDyS).get_Visible())
			{
				num++;
			}
			((Control)lblUpdateCount).set_Text(string.Format("{0,1:f0} items tagged for downloading", num));
		}

		private void SetFileDates()
		{
			((Control)lblEOPPresentFile).set_Text(DownloadDate(EOPpresentFile));
			((Control)lblCometFile).set_Text(DownloadDate(CometFile));
			((Control)lblAsteroidObsFile).set_Text(DownloadDate(AsteroidObsFile));
			((Control)lblDeltaTAfile).set_Text(DownloadDate(DeltaTAFile));
			((Control)lblLatestLunarFile).set_Text(DownloadDate(LunarRecentFile));
			((Control)lblBinaryAsteroidFile).set_Text(DownloadDate(BinaryAsteroidFile));
			((Control)lblAsteroidRingFile).set_Text(DownloadDate(AsteroidRingFile));
			((Control)lblAsteroidDiameterFile).set_Text(DownloadDate(AsteroidDiametersFile));
			((Control)lblAsteroidClassFile).set_Text(DownloadDate(AsteroidClassFile));
			((Control)lblFutureFile).set_Text(DownloadDate(FutureFile));
			((Control)lblReportAddressFile).set_Text(DownloadDate(ReportingAddressFile));
			((Control)lblCameraDelayFile).set_Text(DownloadDate(CameraDelayFile));
			((Control)lblWDSFile).set_Text(DownloadDate(WDSfile));
			((Control)lblWDSCodesFile).set_Text(DownloadDate(WDScodesFile));
			((Control)lblINT4File).set_Text(DownloadDate(Int4File));
			((Control)lblAAVSOfile_Occult).set_Text(DownloadDate(AAVSOindexFile));
			((Control)lblAAVSOfile).set_Text(DownloadDate(AAVSOFullFile));
			((Control)lblCALLfile).set_Text(DownloadDate(CALLfile));
			((Control)lbl6thOrbitFile).set_Text(DownloadDate(DoubleOrbitFile));
			((Control)lblKepler2File).set_Text(DownloadDate(Kepler2File));
			((Control)lblFutureAllFile).set_Text(DownloadDate(FutureAllFile));
			((Control)lblAstorbFile).set_Text(DownloadDate(AstorbFile));
			((Control)lblMPCfile).set_Text(DownloadDate(MPCorbFile));
			((Control)lblAstDysFile).set_Text(DownloadDate(AstDysFile));
			((Control)lblISAMfile).set_Text(DownloadDate(ISAMFile));
			((Control)lblShapeModels).set_Text(DownloadDate(ShapeFile));
			((Control)lblLunarOldFiles).set_Text(DownloadDate(LunarOldFiles));
			((Control)lblXZcatFile).set_Text(DownloadDate(XZfile));
			((Control)lblLightCurves).set_Text(DownloadDate(LightCurveFile));
			((Control)lblLOLA128File).set_Text(DownloadDate(Lola128File));
			((Control)lblStarDiaFile).set_Text(DownloadDate(StarDiaFile));
			((Control)lblTycho2File).set_Text(DownloadDate(Tycho2File));
			((Control)lblJPL_DEfile).set_Text(DownloadDate(JPL_DEfile));
			((Control)lblDE6000file).set_Text(DownloadDate(JPL6000File));
			((Control)lblEOP_OldFile).set_Text(DownloadDate(EOPoldFile));
			((Control)lblCraterFile).set_Text(DownloadDate(CraterCoordsFile));
			((Control)lblPhoebeFile).set_Text(DownloadDate(PhoebeFile));
			((Control)lblGoogleMapTemplatesFile).set_Text(DownloadDate(GMTemplatesFile));
			((Control)lblGaia9).set_Text(DownloadDate(Gaia9File));
			((Control)lblGaia12).set_Text(DownloadDate(Gaia12File));
			((Control)lblGaia14).set_Text(DownloadDate(Gaia14File));
			((Control)lblGaia16).set_Text(DownloadDate(Gaia16File));
			((Control)lblUBSC).set_Text(DownloadDate(UBSCfile));
			((Control)lblSAO).set_Text(DownloadDate(SAOFile));
			((Control)lblEarth2014).set_Text(DownloadDate(Earth2014File));
			((Control)lblGaiaDoubles).set_Text(DownloadDate(GaiaDoublesFile));
		}

		internal static string DownloadDate(string Fname)
		{
			if (File.Exists(Fname))
			{
				DateTime lastWriteTime = File.GetLastWriteTime(Fname);
				return lastWriteTime.Day.ToString().PadLeft(2) + " " + Utilities.ShortMonths[lastWriteTime.Month] + " " + lastWriteTime.Year;
			}
			return "-- --- ----";
		}

		private void ShowConvert(int FileNumber)
		{
			//IL_0012: Unknown result type (might be due to invalid IL or missing references)
			Astorb = new ConvertAstorb_etc(FileNumber);
			((Form)Astorb).ShowDialog();
		}

		private void Downloads_Resize(object sender, EventArgs e)
		{
			((Control)panel1).set_Height(((Control)this).get_Height() - 80);
		}

		private void EnableCancelButton()
		{
			((Control)cmdCancel).set_Enabled(true);
			((Control)cmdCancel).set_ForeColor(Color.Red);
			((Control)cmdCancel).set_BackColor(Color.Yellow);
		}

		private void DisableCancelButton()
		{
			((Control)cmdCancel).set_Enabled(false);
			((Control)cmdCancel).set_ForeColor(((Control)cmdHelp).get_ForeColor());
			((Control)cmdCancel).set_BackColor(((Control)cmdHelp).get_BackColor());
		}

		private void cmdExit_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void Downloads_FormClosed(object sender, FormClosedEventArgs e)
		{
			((Component)this).Dispose();
		}

		private void cmdComet_Click(object sender, EventArgs e)
		{
			Download_Comet();
			SetFileDates();
		}

		internal void Download_Comet()
		{
			http.Download_Comet(SupressMessages: false);
		}

		private void cmdDeltaT_Click(object sender, EventArgs e)
		{
			Download_DeltaT(SupressMessages: false);
		}

		private void Download_DeltaT(bool SupressMessages)
		{
			string occultServer = Settings.Default.OccultServer;
			string text = "DeltaT.zip";
			string finalDestination = Utilities.AppPath + "\\Resource Files\\";
			if ((occultServer.Trim().Length == 0) | (text.Trim().Length == 0))
			{
				MissingDownloadDetails(occultServer, text);
				return;
			}
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, !SupressMessages);
			((Control)this).set_Cursor(Cursors.get_Default());
			SetFileDates();
			SetDownloadFlags();
		}

		private void cmdFuture_Click(object sender, EventArgs e)
		{
			Download_Future(SupressMessages: false);
			SetFileDates();
		}

		private void Download_Future(bool SupressMessages)
		{
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.Download_Future(SupressMessages);
			((Control)this).set_Cursor(Cursors.get_Default());
		}

		private void cmdFuture540_Click(object sender, EventArgs e)
		{
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.Download_Future_ALL(SupressMessages: false);
			SetFileDates();
			((Control)this).set_Cursor(Cursors.get_Default());
		}

		private static void MissingDownloadDetails(string Server, string File)
		{
			//IL_0082: Unknown result type (might be due to invalid IL or missing references)
			string text = "";
			if (Server.Length == 0)
			{
				text += "Server";
			}
			if (File.Length == 0)
			{
				if (text.Length > 0)
				{
					text += " and ";
				}
				text += " File ";
			}
			MessageBox.Show("The User Settings specify the download address as:\r\n>  Server = " + Server + "\r\n>  File = " + File + "\r\n\r\nThe entry for " + text + " is missing.\r\n\r\nTo fix this:\r\n* Close this form;\r\n* Open the User Settings form;\r\n* locate the group '4. Email settings; Download addresses..\r\n* Click the blue button at the end of that group; and\r\n* 'Save and Exit' from that form\r\n\r\nThen retry the download.", "Missing server details", (MessageBoxButtons)0, (MessageBoxIcon)16);
		}

		private void cmdAstorb_Click(object sender, EventArgs e)
		{
			string aSTORB_Server = Settings.Default.ASTORB_Server;
			string aSTORB_file = Settings.Default.ASTORB_file;
			if ((aSTORB_Server.Trim().Length == 0) | (aSTORB_file.Trim().Length == 0))
			{
				MissingDownloadDetails(aSTORB_Server, aSTORB_file);
				return;
			}
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			if (http.DownloadHTTP(aSTORB_Server, aSTORB_file, Utilities.AppPath + "\\DownLoaded Files\\", unzip: false, gunzip: true, ShowMessages: false))
			{
				((Control)this).set_Cursor(Cursors.get_Default());
				ShowConvert(0);
			}
			((Control)this).set_Cursor(Cursors.get_Default());
			DisableCancelButton();
			SetFileDates();
		}

		private void cmdAstorbVizier_Click(object sender, EventArgs e)
		{
			string aSTORB_Mirror = Settings.Default.ASTORB_Mirror;
			string aSTORB_Mirror_file = Settings.Default.ASTORB_Mirror_file;
			if ((aSTORB_Mirror.Trim().Length == 0) | (aSTORB_Mirror_file.Trim().Length == 0))
			{
				MissingDownloadDetails(aSTORB_Mirror, aSTORB_Mirror_file);
				return;
			}
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			if (ftp.DownloadFTP(aSTORB_Mirror, aSTORB_Mirror_file, Utilities.AppPath + "\\DownLoaded Files\\", unzip: false, gunzip: true, SupressMessages: false))
			{
				((Control)this).set_Cursor(Cursors.get_Default());
				ShowConvert(0);
			}
			((Control)this).set_Cursor(Cursors.get_Default());
			SetFileDates();
			DisableCancelButton();
		}

		private void cmdMPCorb_Click(object sender, EventArgs e)
		{
			string mPCOrb_Server = Settings.Default.MPCOrb_Server;
			string mPCOrb_file = Settings.Default.MPCOrb_file;
			string finalDestination = Utilities.AppPath + "\\Downloaded Files\\";
			if ((mPCOrb_Server.Trim().Length == 0) | (mPCOrb_file.Trim().Length == 0))
			{
				MissingDownloadDetails(mPCOrb_Server, mPCOrb_file);
				return;
			}
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			if (http.DownloadHTTP(mPCOrb_Server, mPCOrb_file, finalDestination, unzip: false, gunzip: true, ShowMessages: false))
			{
				((Control)this).set_Cursor(Cursors.get_Default());
				ShowConvert(1);
			}
			((Control)this).set_Cursor(Cursors.get_Default());
			SetFileDates();
			DisableCancelButton();
		}

		private void cmdMPCorbCzech_Click(object sender, EventArgs e)
		{
			string mPCORB_Mirror_Server = Settings.Default.MPCORB_Mirror_Server;
			string mPCOrb_Mirror_file = Settings.Default.MPCOrb_Mirror_file;
			if ((mPCORB_Mirror_Server.Trim().Length == 0) | (mPCOrb_Mirror_file.Trim().Length == 0))
			{
				MissingDownloadDetails(mPCORB_Mirror_Server, mPCOrb_Mirror_file);
				return;
			}
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			if (ftp.DownloadFTP(mPCORB_Mirror_Server, mPCOrb_Mirror_file, Utilities.AppPath + "\\DownLoaded Files\\", unzip: true, gunzip: false, SupressMessages: false))
			{
				((Control)this).set_Cursor(Cursors.get_Default());
				ShowConvert(1);
			}
			((Control)this).set_Cursor(Cursors.get_Default());
			SetFileDates();
			DisableCancelButton();
		}

		private void cmdDownloadAstDyS_Click(object sender, EventArgs e)
		{
			//IL_00dd: Unknown result type (might be due to invalid IL or missing references)
			string astDys2_Server = Settings.Default.AstDys2_Server;
			string astDys2_AllNumFile = Settings.Default.AstDys2_AllNumFile;
			string finalDestination = Utilities.AppPath + "\\DownLoaded Files\\AstDyS2.dat";
			if ((astDys2_Server.Trim().Length == 0) | (astDys2_AllNumFile.Trim().Length == 0))
			{
				MissingDownloadDetails(astDys2_Server, astDys2_AllNumFile);
				return;
			}
			try
			{
				HttpWebRequest obj = (HttpWebRequest)WebRequest.Create(astDys2_Server);
				obj.Method = "HEAD";
				_ = (HttpWebResponse)obj.GetResponse();
				EnableCancelButton();
				((Control)this).set_Cursor(Cursors.get_WaitCursor());
				if (http.DownloadHTTP(astDys2_Server, astDys2_AllNumFile, finalDestination, unzip: false, gunzip: false, ShowMessages: true))
				{
					((Control)this).set_Cursor(Cursors.get_Default());
					ShowConvert(2);
				}
				((Control)this).set_Cursor(Cursors.get_Default());
				SetFileDates();
				SetDownloadFlags();
				DisableCancelButton();
			}
			catch (Exception ex)
			{
				MessageBox.Show("AstDys-2 server\r\n   " + astDys2_Server + "\r\n is not available\r\n\r\n" + ex.Message, "Failed download", (MessageBoxButtons)0, (MessageBoxIcon)16);
			}
		}

		private void cmdDowload_EOP_new_Click(object sender, EventArgs e)
		{
			Download_EOP_new(SupressMessages: false);
		}

		private void Download_EOP_new(bool SupressMessages)
		{
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			ftp.Download_EOP_current(SupressMessages);
			SetFileDates();
			((Control)this).set_Cursor(Cursors.get_Default());
		}

		private void cmdDowload_EOP_Old_Click(object sender, EventArgs e)
		{
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			ftp.Download_EOP_Old(SupressMessages: false);
			SetFileDates();
			((Control)this).set_Cursor(Cursors.get_Default());
		}

		private void cmdAsteroidObservations_Click(object sender, EventArgs e)
		{
			Download_AsteroidObs(SupressMessages: false);
		}

		public void Download_AsteroidObs(bool SupressMessages)
		{
			string occultServer = Settings.Default.OccultServer;
			string text = "Asteroid_Observations.zip";
			string finalDestination = Utilities.AppPath + "\\Resource Files\\";
			if ((occultServer.Trim().Length == 0) | (text.Trim().Length == 0))
			{
				MissingDownloadDetails(occultServer, text);
				return;
			}
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, !SupressMessages);
			((Control)this).set_Cursor(Cursors.get_Default());
			DisableCancelButton();
			SetFileDates();
			SetDownloadFlags();
		}

		private void cmdLunar_Click(object sender, EventArgs e)
		{
			string occultServer = Settings.Default.OccultServer;
			string text = "Archive_Observations.zip";
			string finalDestination = Utilities.AppPath + "\\Resource Files\\";
			if ((occultServer.Trim().Length == 0) | (text.Trim().Length == 0))
			{
				MissingDownloadDetails(occultServer, text);
				return;
			}
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, ShowMessages: true);
			((Control)this).set_Cursor(Cursors.get_Default());
			DisableCancelButton();
			SetFileDates();
			SetDownloadFlags();
		}

		private void cmdLatestLunar_Click(object sender, EventArgs e)
		{
			Download_LunarRecent(SupressMessages: false);
		}

		private void Download_LunarRecent(bool SupressMessages)
		{
			string occultServer = Settings.Default.OccultServer;
			string text = "Latest_Observations.zip";
			string finalDestination = Utilities.AppPath + "\\Resource Files\\";
			if ((occultServer.Trim().Length == 0) | (text.Trim().Length == 0))
			{
				MissingDownloadDetails(occultServer, text);
				return;
			}
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, !SupressMessages);
			((Control)this).set_Cursor(Cursors.get_Default());
			DisableCancelButton();
			SetFileDates();
			SetDownloadFlags();
		}

		private void cmdBinary_Click(object sender, EventArgs e)
		{
			Download_BinaryAsteroids(SupressMessages: false);
		}

		private void Download_BinaryAsteroids(bool SupressMessages)
		{
			string occultServer = Settings.Default.OccultServer;
			string text = "BinaryAsteroids.zip";
			string finalDestination = Utilities.AppPath + "\\Resource Files\\";
			if ((occultServer.Trim().Length == 0) | (text.Trim().Length == 0))
			{
				MissingDownloadDetails(occultServer, text);
				return;
			}
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, !SupressMessages);
			((Control)this).set_Cursor(Cursors.get_Default());
			DisableCancelButton();
			SetFileDates();
			SetDownloadFlags();
		}

		private void cmdRings_Click(object sender, EventArgs e)
		{
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			DownloadAsteroidRings();
			((Control)this).set_Cursor(Cursors.get_Default());
			DisableCancelButton();
			SetFileDates();
			SetDownloadFlags();
		}

		public static void DownloadAsteroidRings()
		{
			if (Utilities.InternetIsAvailable())
			{
				string occultServer = Settings.Default.OccultServer;
				string text = "AsteroidRings.zip";
				string finalDestination = Utilities.AppPath + "\\Resource Files\\";
				if ((occultServer.Trim().Length == 0) | (text.Trim().Length == 0))
				{
					MissingDownloadDetails(occultServer, text);
				}
				else
				{
					http.DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, ShowMessages: false);
				}
			}
		}

		private void cmdAsteroidDiameters_Click(object sender, EventArgs e)
		{
			Download_AsteroidDias(SupressMessages: false);
			SetFileDates();
			SetDownloadFlags();
		}

		private void Download_AsteroidDias(bool SupressMessages)
		{
			http.Download_AsteroidDias(SupressMessages);
			SetFileDates();
			SetDownloadFlags();
			ShowConvert(0);
		}

		private void cmdAsteroidClass_Click(object sender, EventArgs e)
		{
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.DownloadAsteroidClassFile();
			((Control)this).set_Cursor(Cursors.get_Default());
			DisableCancelButton();
			SetFileDates();
			SetDownloadFlags();
		}

		private void cmdTycho2_Click(object sender, EventArgs e)
		{
			string occultServer = Settings.Default.OccultServer;
			string text = "tycho2.zip";
			string finalDestination = Utilities.AppPath + "\\Resource Files\\";
			if ((occultServer.Trim().Length == 0) | (text.Trim().Length == 0))
			{
				MissingDownloadDetails(occultServer, text);
				return;
			}
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, ShowMessages: true);
			((Control)this).set_Cursor(Cursors.get_Default());
			DisableCancelButton();
			SetFileDates();
			SetDownloadFlags();
		}

		private void cmdXZ_Click(object sender, EventArgs e)
		{
			string occultServer = Settings.Default.OccultServer;
			string text = "XZ.zip";
			string finalDestination = Utilities.AppPath + "\\Resource Files\\";
			if ((occultServer.Trim().Length == 0) | (text.Trim().Length == 0))
			{
				MissingDownloadDetails(occultServer, text);
				return;
			}
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, ShowMessages: true);
			((Control)this).set_Cursor(Cursors.get_Default());
			DisableCancelButton();
			SetFileDates();
			SetDownloadFlags();
		}

		private void cmdJPL_DE_Click(object sender, EventArgs e)
		{
			string occultServer = Settings.Default.OccultServer;
			string text = "DE_Ephemeris.zip";
			string finalDestination = Utilities.AppPath + "\\Resource Files\\";
			if ((occultServer.Trim().Length == 0) | (text.Trim().Length == 0))
			{
				MissingDownloadDetails(occultServer, text);
				return;
			}
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, ShowMessages: true);
			((Control)this).set_Cursor(Cursors.get_Default());
			DisableCancelButton();
			SetFileDates();
			SetDownloadFlags();
			JPL_DE.InitialiseDE_Ephemeris();
		}

		private void cmdAddresses_Click(object sender, EventArgs e)
		{
			Emails.GetCurrentAddresses();
			SetFileDates();
			SetDownloadFlags();
		}

		private void cmdCameraDelays_Click(object sender, EventArgs e)
		{
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			Download_CameraDelays(SupressMessages: false);
			((Control)this).set_Cursor(Cursors.get_Default());
			DisableCancelButton();
			SetFileDates();
			SetDownloadFlags();
		}

		internal static void Download_CameraDelays(bool SupressMessages)
		{
			string occultServer = Settings.Default.OccultServer;
			string text = "CameraDelays.zip";
			string finalDestination = Utilities.AppPath + "\\Resource Files\\";
			if ((occultServer.Trim().Length == 0) | (text.Trim().Length == 0))
			{
				MissingDownloadDetails(occultServer, text);
			}
			else
			{
				http.DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, !SupressMessages);
			}
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			ftp.CancelFlag = true;
			http.CancelFlag = true;
		}

		private void cmdHelp_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Downloads");
		}

		private void cmdDownloadAll_Click(object sender, EventArgs e)
		{
			Download_EOP_new(SupressMessages: true);
			Download_Comet();
			Download_AsteroidObs(SupressMessages: true);
			Download_DeltaT(SupressMessages: true);
			Download_LunarRecent(SupressMessages: true);
			Download_BinaryAsteroids(SupressMessages: true);
			DownloadAsteroidRings();
			Download_AsteroidDias(SupressMessages: true);
			Download_Future(SupressMessages: true);
			SetFileDates();
			SetDownloadFlags();
		}

		private void cmdDownloadWDS_Click(object sender, EventArgs e)
		{
			DownloadWDS();
		}

		private void DownloadWDS()
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_005c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0062: Invalid comparison between Unknown and I4
			//IL_00e1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e7: Invalid comparison between Unknown and I4
			//IL_011e: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("Cannot download the Washington Double Star catalogue - the Internet is not available", "Can't download");
			}
			else
			{
				if (((DateTime.UtcNow.DayOfWeek == DayOfWeek.Sunday) | ((DateTime.UtcNow.DayOfWeek == DayOfWeek.Monday) & (DateTime.UtcNow.Hour < 12))) && (int)MessageBox.Show("The WDS file might not be available for download.\r\n{It is not available on Sundays and Monday mornings}", "WDS may not be available", (MessageBoxButtons)1) == 2)
				{
					return;
				}
				string wDSDownloadServer = Settings.Default.WDSDownloadServer;
				string wDSDownload_File = Settings.Default.WDSDownload_File;
				if ((wDSDownloadServer.Trim().Length == 0) | (wDSDownload_File.Trim().Length == 0))
				{
					MissingDownloadDetails(wDSDownloadServer, wDSDownload_File);
					return;
				}
				EnableCancelButton();
				((Control)this).set_Cursor(Cursors.get_WaitCursor());
				if (!ftp.DownloadFTP(wDSDownloadServer, wDSDownload_File, Utilities.AppPath + "\\DownLoaded Files\\", unzip: false, gunzip: true, SupressMessages: false))
				{
					if ((int)MessageBox.Show("GZipped version of the WDS catalog is not available.\r\n\r\nDo you want to download the unzipped version?\r\n[The file size is about 15MB]", "WDS.dat.gz not found", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 7)
					{
						return;
					}
					if (!ftp.DownloadFTP(wDSDownloadServer, wDSDownload_File.Replace(".gz", ""), Utilities.AppPath + "\\DownLoaded Files\\", unzip: false, gunzip: false, SupressMessages: true))
					{
						MessageBox.Show("Download of the WDS catalogue failed.", "Failed download");
						((Control)this).set_Cursor(Cursors.get_Default());
						DisableCancelButton();
						return;
					}
				}
				File.Move(WDSfile, Utilities.AppPath + "\\DownLoaded Files\\wds.dat0");
				using (StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\DownLoaded Files\\wds.dat0"))
				{
					using StreamWriter streamWriter = new StreamWriter(WDSfile);
					do
					{
						streamWriter.WriteLine(streamReader.ReadLine()!.PadRight(130));
					}
					while (!streamReader.EndOfStream);
				}
				File.Delete(Utilities.AppPath + "\\DownLoaded Files\\wds.dat0");
			}
			((Control)this).set_Cursor(Cursors.get_Default());
			DisableCancelButton();
			SetFileDates();
			SetDownloadFlags();
		}

		private void cmdDownloadWDSCodes_Click(object sender, EventArgs e)
		{
			Download_WDSCodes();
		}

		internal void Download_WDSCodes()
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a7: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("Cannot download the WDS discovery codes - the Internet is not available", "Can't download");
			}
			else
			{
				string wDSCodesServer = Settings.Default.WDSCodesServer;
				string wDSCodesFile = Settings.Default.WDSCodesFile;
				_ = Utilities.AppPath + "\\DownLoaded Files\\WDSreferences.dat";
				if ((wDSCodesServer.Trim().Length == 0) | (wDSCodesFile.Trim().Length == 0))
				{
					MissingDownloadDetails(wDSCodesServer, wDSCodesFile);
					return;
				}
				EnableCancelButton();
				((Control)this).set_Cursor(Cursors.get_WaitCursor());
				if (!ftp.DownloadFTP(wDSCodesServer, wDSCodesFile, Utilities.AppPath + "\\DownLoaded Files\\", unzip: false, gunzip: true, SupressMessages: false))
				{
					MessageBox.Show("Download of the WDS discovery codes (references) failed.", "Failed download");
					((Control)this).set_Cursor(Cursors.get_Default());
					DisableCancelButton();
					return;
				}
			}
			string text = "";
			string text2 = "";
			string text3 = "";
			bool flag = false;
			bool flag2 = true;
			string wDScodesFile = WDScodesFile;
			string text4 = Utilities.AppPath + "\\Resource Files\\WDS Discovery Codes old.dat";
			if (File.Exists(text4))
			{
				File.Delete(text4);
			}
			if (File.Exists(wDScodesFile))
			{
				File.Move(wDScodesFile, text4);
			}
			string text5 = Utilities.AppPath + "\\DownLoaded Files\\WDSreferences.dat";
			if (File.Exists(text5))
			{
				File.Delete(text5);
			}
			string text6 = Utilities.AppPath + "\\DownLoaded Files\\refs.dat";
			if (File.Exists(text6))
			{
				File.Move(text6, text5);
			}
			using (StreamWriter streamWriter = new StreamWriter(wDScodesFile))
			{
				using StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\DownLoaded Files\\WDSreferences.dat", Encoding.ASCII);
				do
				{
					string text7 = streamReader.ReadLine()!.PadRight(110);
					if (flag2)
					{
						text2 = text2.Replace("(", "").Replace(")", "");
						if (flag)
						{
							text2 += " et al";
						}
						if (text.Trim() != "")
						{
							streamWriter.WriteLine(text + " " + text2.PadRight(36).Substring(0, 36));
						}
						text = text7.Substring(0, 3);
						text2 = (text3 = WDSName(text7.Substring(20)));
						if (text == "ENG")
						{
							text2 = WDSName("Engelmann, R.".PadRight(90));
						}
						if (text == "ENH")
						{
							text2 = WDSName("Engelhardt, B. von".PadRight(90));
						}
						flag = false;
						flag2 = false;
						if (text7.Trim().Length == 0)
						{
							flag2 = true;
						}
					}
					else if (text7.Trim().Length == 0)
					{
						flag2 = true;
					}
					else
					{
						if (!(text7.Substring(5, 4).Trim() != ""))
						{
							continue;
						}
						text3 = WDSName(text7.Substring(20));
						if ((text2 != text3) & (text2.Substring(0, 1) != "("))
						{
							flag = true;
							if (text3.Length < text2.Length)
							{
								text2 = text3;
							}
						}
					}
				}
				while (!streamReader.EndOfStream);
				if (flag)
				{
					text2 += " et al";
				}
				if (text != " ")
				{
					streamWriter.WriteLine(text + " " + text2.PadRight(36).Substring(0, 36));
				}
			}
			((Control)this).set_Cursor(Cursors.get_Default());
			DisableCancelButton();
			SetFileDates();
			SetDownloadFlags();
		}

		private static string WDSName(string X)
		{
			string text = "";
			text = X.Substring(0, 85).Trim();
			if (text == "")
			{
				text = "unspecified name";
			}
			int num = text.IndexOf("(");
			if (num > 0)
			{
				text = text.Substring(0, num);
			}
			return text;
		}

		private void cmdDownloadInterferometric_Click(object sender, EventArgs e)
		{
			DownloadInterferometric();
		}

		private void DownloadInterferometric()
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_0048: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b0: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("Cannot download the Interferometric catalogue - the Internet is not available", "Can't download");
				return;
			}
			string iFdownloadServer = Settings.Default.IFdownloadServer;
			string iFdownloadFile = Settings.Default.IFdownloadFile;
			if (iFdownloadFile.Contains("all.txt"))
			{
				MessageBox.Show("You are downloading an old version of the Interferometric Catalogue. Please go to the Maintenance Tab, User settings, Group 5, and click 'Reset Download addresses to Default values' to specify the correct file name", "Old file name", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return;
			}
			string finalDestination = Utilities.AppPath + "\\DownLoaded Files\\InterferometricCat.dat";
			if ((iFdownloadServer.Trim().Length == 0) | (iFdownloadFile.Trim().Length == 0))
			{
				MissingDownloadDetails(iFdownloadServer, iFdownloadFile);
				return;
			}
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			if (!http.DownloadHTTP(iFdownloadServer, iFdownloadFile, finalDestination, unzip: false, gunzip: true, ShowMessages: false))
			{
				MessageBox.Show("Download of the Interferometric catalogue failed.", "Failed download");
				((Control)this).set_Cursor(Cursors.get_Default());
				DisableCancelButton();
				return;
			}
			string text = Utilities.AppPath + "\\DownLoaded Files\\InterferometricCat.dat0";
			if (File.Exists(text))
			{
				File.Delete(text);
			}
			string text2 = Utilities.AppPath + "\\DownLoaded Files\\int4_all2.txt";
			if (File.Exists(text2))
			{
				File.Move(text2, text);
			}
			using (StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\DownLoaded Files\\InterferometricCat.dat0"))
			{
				string text3 = streamReader.ReadLine();
				using StreamWriter streamWriter = new StreamWriter(Int4File);
				do
				{
					text3 = streamReader.ReadLine();
					if (!text3.Contains("Catalog") && text3.Length > 2)
					{
						streamWriter.WriteLine(text3.PadRight(118));
					}
				}
				while (!streamReader.EndOfStream);
			}
			File.Delete(Utilities.AppPath + "\\DownLoaded Files\\InterferometricCat.dat0");
			((Control)this).set_Cursor(Cursors.get_Default());
			DisableCancelButton();
			SetFileDates();
			SetDownloadFlags();
		}

		private void cmdDownloadAAVSO_Click(object sender, EventArgs e)
		{
			//IL_00d3: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d9: Invalid comparison between Unknown and I4
			//IL_00f8: Unknown result type (might be due to invalid IL or missing references)
			//IL_00fe: Invalid comparison between Unknown and I4
			//IL_071e: Unknown result type (might be due to invalid IL or missing references)
			List<string> ftpFolderNames = ftp.GetFtpFolderNames("ftp://cdsarc.cds.unistra.fr/0/cats/B/vsx/versions/", "anonymous", "");
			string text = ftpFolderNames[ftpFolderNames.Count - 1];
			double num = (double)updnMagLimitAAVSO.get_Value();
			double num2 = (double)updnMaxMagLimit.get_Value();
			string text2 = Utilities.AppPath + "\\DownLoaded Files\\vsx_csv_All.dat";
			string sourceFileName = Utilities.AppPath + "\\DownLoaded Files\\vsx_csv.dat";
			string text3 = Utilities.AppPath + "\\DownLoaded Files\\vsx_csv_All_Old.dat";
			string text4 = "ftp://cdsarc.cds.unistra.fr/0/cats/B/vsx/versions/" + text + "/";
			string text5 = "vsx_csv.dat";
			bool flag = true;
			if (File.Exists(text2))
			{
				flag = (int)MessageBox.Show("You have an existing Download of the AAVSO file, which\r\ncan be used to create the subset used by Occult using \r\nthe " + string.Format("{0,1:f2}", updnMagLimitAAVSO.get_Value()) + " minimum magnitude variation you have set.\r\n\r\nDo you want to continue with the download?\r\n\r\nClick  Yes  to download and convert\r\nClick  No  to convert the existing downloaded file", "Download &/or Convert", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 6;
			}
			if (flag && (int)MessageBox.Show("The Download will take many minutes\r\n\r\nDo you want to continue with the download?", "Confirm Download", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			if ((text4.Trim().Length == 0) | (text5.Trim().Length == 0))
			{
				MissingDownloadDetails(text4, text5);
				return;
			}
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			if (flag)
			{
				if (File.Exists(text3))
				{
					File.Delete(text3);
				}
				if (File.Exists(text2))
				{
					File.Move(text2, text3);
				}
				if (!ftp.DownloadFTP(text4, text5, Utilities.AppPath + "\\DownLoaded Files\\", unzip: false, gunzip: false, SupressMessages: false))
				{
					return;
				}
				try
				{
					File.Move(sourceFileName, text2);
				}
				catch
				{
				}
			}
			using (StreamReader streamReader = new StreamReader(text2))
			{
				int num3 = 0;
				int num4 = 0;
				using (StreamWriter streamWriter = new StreamWriter(AAVSOindexFile))
				{
					streamReader.ReadLine()!.Split(new char[1] { ',' });
					do
					{
						string[] array = streamReader.ReadLine()!.Split(new char[1] { ',' });
						if (array.Length < 21)
						{
							continue;
						}
						double.TryParse(array[7], out var result);
						if (result > num2)
						{
							continue;
						}
						double.TryParse(array[12], out var result2);
						if ((array[10].Contains("Y") && result2 < 0.2) || result2 - result < num)
						{
							continue;
						}
						StringBuilder stringBuilder = new StringBuilder();
						stringBuilder.Append(array[0].Trim().PadRight(8).Substring(0, 8));
						stringBuilder.Append(array[1].Trim().PadRight(30).Substring(0, 30) + " ");
						stringBuilder.Append(array[2].Trim().PadRight(1) + " ");
						stringBuilder.Append(array[3].Trim().PadLeft(9) + " ");
						stringBuilder.Append(array[4].Trim().PadLeft(9) + " ");
						stringBuilder.Append(array[5].Trim().PadRight(30).Substring(0, 30) + " ");
						stringBuilder.Append(array[6].Trim().PadRight(1) + " ");
						int num5 = array[7].Trim().IndexOf(".");
						if (num5 == 1)
						{
							array[7] = " " + array[7];
						}
						stringBuilder.Append(" " + array[7].PadRight(7).Substring(0, 7));
						stringBuilder.Append(array[8].Trim().PadRight(1) + " ");
						stringBuilder.Append(array[9].Trim().PadRight(7) + " ");
						stringBuilder.Append(array[10].Trim().PadRight(1) + " ");
						stringBuilder.Append(array[11].Trim().PadRight(1) + " ");
						if (array[10].Contains("Y"))
						{
							stringBuilder.Append(string.Format("{0,6:f2} ", result + result2));
						}
						else
						{
							num5 = array[12].Trim().IndexOf(".");
							if (num5 == 1)
							{
								array[12] = " " + array[12];
							}
							stringBuilder.Append("  " + array[12].PadRight(7).Substring(0, 7));
						}
						stringBuilder.Append(array[13].Trim().PadRight(1) + " ");
						stringBuilder.Append(array[14].Trim().PadRight(8) + " ");
						stringBuilder.Append(" " + array[15].Trim().PadRight(14) + " ");
						stringBuilder.Append(array[16].Trim().PadRight(1) + " ");
						stringBuilder.Append(array[17].Trim().PadRight(1) + " ");
						if ((array[18].Trim().IndexOf('.') < 0) & (array[18].Trim().Length > 0))
						{
							array[18] += ".";
						}
						num5 = array[18].IndexOf(".");
						if (num5 > 0)
						{
							array[18] = "".PadRight(5 - num5) + array[18].Trim();
						}
						stringBuilder.Append(array[18].PadRight(18).Substring(0, 18) + " ");
						stringBuilder.Append(array[19].Trim().PadRight(1));
						string s = array[20].Trim().Replace('"', ' ');
						stringBuilder.Append(Encoding.GetEncoding(0).GetString(Encoding.GetEncoding(0).GetBytes(s)));
						streamWriter.WriteLine(stringBuilder.ToString().PadRight(202).Substring(0, 202));
						num4++;
					}
					while (!streamReader.EndOfStream);
				}
				Interferometric_Plus_WDS.AAVSOLength = num3 + 2;
			}
			((Control)this).set_Cursor(Cursors.get_Default());
			DisableCancelButton();
			SetFileDates();
			SetDownloadFlags();
			MessageBox.Show("The AAVSO index file having " + (double)new FileInfo(AAVSOindexFile).Length / 204.0 + "  records \r\n* with magnitude variations greater than" + string.Format(" {0,1:f2} ", num) + ", and\r\n* maximum Mag being brighter than " + string.Format(" {0,1:f2} ", num2) + "\r\nhas been created", "Successful file creation", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
		}

		private void cmdDownloadAsteroidLightCurveData_Click(object sender, EventArgs e)
		{
			//IL_0063: Unknown result type (might be due to invalid IL or missing references)
			string asteroidLightCurveData_url = Settings.Default.AsteroidLightCurveData_url;
			int num = asteroidLightCurveData_url.IndexOf("ps/LC");
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.DownloadHTTP(asteroidLightCurveData_url.Substring(0, num + 2), asteroidLightCurveData_url.Substring(num + 3), Utilities.AppPath + "\\Downloaded files\\", unzip: true, gunzip: false, ShowMessages: false, ShowProgressbar: true, "LC_summary_pub.TXT", "readme.pdf");
			MessageBox.Show("Asteroid Light Curve data has been downloaded");
			((Control)this).set_Cursor(Cursors.get_Default());
			DisableCancelButton();
			SetFileDates();
			SetDownloadFlags();
		}

		private void cmdDownloadVisualBinaries_Click(object sender, EventArgs e)
		{
			//IL_006b: Unknown result type (might be due to invalid IL or missing references)
			string orb6DownloadServer = Settings.Default.Orb6DownloadServer;
			string orb6DownloadFile = Settings.Default.Orb6DownloadFile;
			string doubleOrbitFile = DoubleOrbitFile;
			if ((orb6DownloadServer.Trim().Length == 0) | (orb6DownloadFile.Trim().Length == 0))
			{
				MissingDownloadDetails(orb6DownloadServer, orb6DownloadFile);
				return;
			}
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			if (!http.DownloadHTTP(orb6DownloadServer, orb6DownloadFile, doubleOrbitFile, unzip: false, gunzip: false, ShowMessages: false))
			{
				MessageBox.Show("Download of the 6th catalogue of Orbits of Visual Binary Stars failed.", "Failed download");
			}
			((Control)this).set_Cursor(Cursors.get_Default());
			DisableCancelButton();
			SetFileDates();
			SetDownloadFlags();
		}

		private void cmdLOLA_Click(object sender, EventArgs e)
		{
			string occultServer = Settings.Default.OccultServer;
			string text = "LOLA128.zip";
			string finalDestination = Utilities.AppPath + "\\Resource Files\\";
			if ((occultServer.Trim().Length == 0) | (text.Trim().Length == 0))
			{
				MissingDownloadDetails(occultServer, text);
				return;
			}
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, ShowMessages: true);
			Utilities.SetLunarLimbAvailability();
			((Control)this).set_Cursor(Cursors.get_Default());
			DisableCancelButton();
			SetFileDates();
			SetDownloadFlags();
		}

		private void cmdStarDiameters_Click(object sender, EventArgs e)
		{
			string occultServer = Settings.Default.OccultServer;
			string text = "StarDia.zip";
			string finalDestination = Utilities.AppPath + "\\Resource Files\\";
			if ((occultServer.Trim().Length == 0) | (text.Trim().Length == 0))
			{
				MissingDownloadDetails(occultServer, text);
				return;
			}
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, ShowMessages: true);
			((Control)this).set_Cursor(Cursors.get_Default());
			DisableCancelButton();
			SetFileDates();
			SetDownloadFlags();
		}

		private void cmbFileAge_SelectedIndexChanged(object sender, EventArgs e)
		{
			DownloadAgeSelectedIndex = ((ListControl)cmbFileAge).get_SelectedIndex();
			Settings.Default.DownloadFileAge = DownloadAgeSelectedIndex;
			SetDownloadFlags();
		}

		private void cmdDE422_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			//IL_004d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0053: Invalid comparison between Unknown and I4
			//IL_0103: Unknown result type (might be due to invalid IL or missing references)
			//IL_0109: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("WARNING\r\n\r\nThis download is LARGE - about 2.55GB. Do not attempt\r\nthis download unless you have a fast internet connection.\r\n\r\nDo you want to continue with the download?", "Confirm download", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7 || (File.Exists(Utilities.AppPath + "\\Resource Files\\DE_LongEphemeris.bin") && (int)MessageBox.Show("WARNING\r\n\r\nYou already have this file on your computer.\r\n\r\n Are you sure you want to download this large (540MB) file?", "Confirm download", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7))
			{
				return;
			}
			string occultServer = Settings.Default.OccultServer;
			string text = "DE_6000yrs.zip";
			string finalDestination = Utilities.AppPath + "\\Resource Files\\";
			if ((occultServer.Trim().Length == 0) | (text.Trim().Length == 0))
			{
				MissingDownloadDetails(occultServer, text);
				return;
			}
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			if (http.DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, ShowMessages: false))
			{
				string sourceFileName = Utilities.AppPath + "\\Resource Files\\DE_6000yrs.bin";
				string text2 = Utilities.AppPath + "\\Resource Files\\DE_LongEphemeris.bin";
				if (File.Exists(text2))
				{
					JPL_DE.ReNameEphemerisToDEVersion(IsLongEphemFile: true);
				}
				File.Move(sourceFileName, text2);
				if ((int)MessageBox.Show("The file has been successfully downloaded and unzipped.\r\n\r\nDo you want to save disk space, and delete the downloaded ZIP file", "Delete downloaded ZIP file", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6)
				{
					try
					{
						File.Delete(Utilities.AppPath + "\\Downloaded Files\\de_6000yrs.zip");
					}
					catch
					{
					}
				}
			}
			((Control)this).set_Cursor(Cursors.get_Default());
			DisableCancelButton();
			SetFileDates();
			SetDownloadFlags();
			JPL_DE.InitialiseDE_Ephemeris();
		}

		private void cmdDownloadShapes_Click(object sender, EventArgs e)
		{
			//IL_0055: Unknown result type (might be due to invalid IL or missing references)
			string occultServer = Settings.Default.OccultServer;
			string text = "ShapeModels.zip";
			string finalDestination = Utilities.AppPath + "\\ShapeModels\\";
			if ((occultServer.Trim().Length == 0) | (text.Trim().Length == 0))
			{
				MissingDownloadDetails(occultServer, text);
				return;
			}
			MessageBox.Show("After the file has downloaded, there will be a delay of several 10's of seconds while more than 6000 files are extracted from the .zip file", "Download delay", (MessageBoxButtons)0, (MessageBoxIcon)64);
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, ShowMessages: true);
			((Control)this).set_Cursor(Cursors.get_Default());
			DisableCancelButton();
			SetFileDates();
			SetDownloadFlags();
		}

		private void cmdCreateISAM_Click(object sender, EventArgs e)
		{
			//IL_0012: Unknown result type (might be due to invalid IL or missing references)
			if (http.Get_ISAM_Asteroids(ForceUpdate: true))
			{
				MessageBox.Show("ISAM file creation successful", "ISAM Download");
			}
			SetFileDates();
		}

		private void cmdDownloadISAM_Click(object sender, EventArgs e)
		{
			DownloadISAM();
		}

		public void DownloadISAM()
		{
			string occultServer = Settings.Default.OccultServer;
			string text = "isam_availableasteroids.zip";
			string finalDestination = Utilities.AppPath + "\\ShapeModels\\";
			if ((occultServer.Trim().Length == 0) | (text.Trim().Length == 0))
			{
				MissingDownloadDetails(occultServer, text);
				return;
			}
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, ShowMessages: true);
			((Control)this).set_Cursor(Cursors.get_Default());
			DisableCancelButton();
			SetFileDates();
			SetDownloadFlags();
		}

		private void cmdDownloadCraterTimings_Click(object sender, EventArgs e)
		{
			string occultServer = Settings.Default.OccultServer;
			string text = "CraterTimings.zip";
			string finalDestination = Utilities.AppPath + "\\Resource Files\\";
			if ((occultServer.Trim().Length == 0) | (text.Trim().Length == 0))
			{
				MissingDownloadDetails(occultServer, text);
				return;
			}
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, ShowMessages: true);
			text = "crater.zip";
			http.DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, ShowMessages: true);
			((Control)this).set_Cursor(Cursors.get_Default());
			DisableCancelButton();
			SetDownloadFlags();
		}

		private void cmdDownloadePhoebeEphemeris_Click(object sender, EventArgs e)
		{
			string occultServer = Settings.Default.OccultServer;
			string text = "SaturnIX.zip";
			string finalDestination = Utilities.AppPath + "\\Resource Files\\";
			if ((occultServer.Trim().Length == 0) | (text.Trim().Length == 0))
			{
				MissingDownloadDetails(occultServer, text);
				return;
			}
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, ShowMessages: true);
			((Control)this).set_Cursor(Cursors.get_Default());
			DisableCancelButton();
			SetDownloadFlags();
		}

		private void cmdDownloadGoogleMapFiles_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("This download will overwrite your existing GoogleMap template files, losing any customised changes you have made to those files. You might want to back-up those files before downloading the new files.\r\n\r\nThe files, located in the 'Resource Files' directory, are:\r\n*  GoogleMap_FileHeader.txt\r\n*  GoogleMap_FileFooter.txt\r\n*  GoogleMap_ServerDriver_2.js\r\n\r\nNOTE: the file  GoogleMap_ServerDriver.js , which was distributed before November 2013, is no longer functional and can be deleted.\r\n\r\nDo you want to continue with the download?", "Confirm download", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				string occultServer = Settings.Default.OccultServer;
				string text = "GoogleMap_Files.zip";
				string finalDestination = Utilities.AppPath + "\\Resource Files\\";
				if ((occultServer.Trim().Length == 0) | (text.Trim().Length == 0))
				{
					MissingDownloadDetails(occultServer, text);
					return;
				}
				EnableCancelButton();
				((Control)this).set_Cursor(Cursors.get_WaitCursor());
				http.DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, ShowMessages: true);
				((Control)this).set_Cursor(Cursors.get_Default());
				DisableCancelButton();
			}
		}

		private void cmdDownloadKepler2_Click(object sender, EventArgs e)
		{
			string occultServer = Settings.Default.OccultServer;
			string text = "kepler2.zip";
			string finalDestination = Utilities.AppPath + "\\Resource Files\\";
			if ((occultServer.Trim().Length == 0) | (text.Trim().Length == 0))
			{
				MissingDownloadDetails(occultServer, text);
				return;
			}
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, ShowMessages: true);
			((Control)this).set_Cursor(Cursors.get_Default());
			DisableCancelButton();
			SetFileDates();
			SetDownloadFlags();
		}

		private void cmdDownloadInstallationFiles_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("This will download two data installation files, and install them.\r\n\r\nTHIS FUNCTION SHOULD NOT BE REQUIRED.\r\n\r\nOnly use this function if you have reason to believe the data files have been corrupted or lost.\r\n\r\nDo you wish to continue?", "Confirm download of install files", (MessageBoxButtons)4, (MessageBoxIcon)64, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 6 && http.GetInstallationDataFiles())
			{
				using (ZipArchive source = ZipFile.Open(Utilities.AppPath + "\\Downloaded Files\\InstallResources.zip", ZipArchiveMode.Read))
				{
					source.ExtractToDirectory(Utilities.AppPath + "\\Resource Files");
				}
				using ZipArchive source2 = ZipFile.Open(Utilities.AppPath + "\\Downloaded Files\\InstallSites.zip", ZipArchiveMode.Read);
				source2.ExtractToDirectory(Utilities.AppPath + "\\Sites");
			}
		}

		private void cmdCreateLOLA128_Click(object sender, EventArgs e)
		{
			//IL_0128: Unknown result type (might be due to invalid IL or missing references)
			//IL_012e: Invalid comparison between Unknown and I4
			//IL_0131: Unknown result type (might be due to invalid IL or missing references)
			//IL_0138: Expected O, but got Unknown
			//IL_0168: Unknown result type (might be due to invalid IL or missing references)
			//IL_016e: Invalid comparison between Unknown and I4
			//IL_01f4: Unknown result type (might be due to invalid IL or missing references)
			string[] array = new string[16]
			{
				"ldem_512_45n_90n_000_090.img", "ldem_512_45n_90n_090_180.img", "ldem_512_45n_90n_180_270.img", "ldem_512_45n_90n_270_360.img", "ldem_512_00n_45n_000_090.img", "ldem_512_00n_45n_090_180.img", "ldem_512_00n_45n_180_270.img", "ldem_512_00n_45n_270_360.img", "ldem_512_45s_00s_000_090.img", "ldem_512_45s_00s_090_180.img",
				"ldem_512_45s_00s_180_270.img", "ldem_512_45s_00s_270_360.img", "ldem_512_90s_45s_000_090.img", "ldem_512_90s_45s_090_180.img", "ldem_512_90s_45s_180_270.img", "ldem_512_90s_45s_270_360.img"
			};
			double[] array2 = new double[16]
			{
				0.0, 90.0, 180.0, 270.0, 0.0, 90.0, 180.0, 270.0, 0.0, 90.0,
				180.0, 270.0, 0.0, 90.0, 180.0, 270.0
			};
			double[] array3 = new double[16]
			{
				67.5, 67.5, 67.5, 67.5, 22.5, 22.5, 22.5, 22.5, -22.5, -22.5,
				-22.5, -22.5, -67.5, -67.5, -67.5, -67.5
			};
			string text = "";
			string text2 = "";
			string text3 = "";
			bool flag = false;
			int num = 1;
			int num2 = 100;
			int num3 = 360 * num2;
			int num4 = 50;
			int num5 = 40 * num4 + 1;
			long num6 = num3 * num5;
			long[] array4 = new long[num6];
			long[] array5 = new long[num6];
			double num7 = 0.0;
			long num8 = 0L;
			if ((int)MessageBox.Show("This option converts the LOLA GDR Cylindrical 1/128, or 1/512, dataset to the binary file LOLA128.bin in the Occult4/Resource Files directory. LOLA128.bin has the data re-arranged to facilitate the generation of lunar limb profiles. The source dataset is updated on a regular basis (until the end of the LRO mission). The first Occult distribution includes measurements to Sept 2014. \r\n\r\nTo convert the file, you need to downloaded the latest version of the relevant LRO_LOLA_LDEM_128 GDR '.img' file (2GB),\r\nor LRO_LOLA_LDEM_512 GDR '.img' files (32GB). The files can be downloaded from:\r\n\r\nhttp://pds-geosciences.wustl.edu/lro/lro-l-lola-3-rdr-v1/lrolol_1xxx/browse/lola_gdr/CYLINDRICAL.html\r\n\r\nYou need the 2025MB 'IMG' file(s) found under '(1/128...) or (1/512...) resolution', 'DEM'\r\n\r\nThe conversion will take many minutes [hours for the 512 data], and the existing file will be overwritten. During the conversion, the current selenocentric latitude being processed is displayed.\r\n\r\nDo you want to continue?", "Convert LOLA LDEM file", (MessageBoxButtons)4, (MessageBoxIcon)64, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			OpenFileDialog val = new OpenFileDialog();
			try
			{
				((FileDialog)val).set_FileName("LDEM_128.img");
				((FileDialog)val).set_Title("Convert 'LDEM_128.img' or 'LDEM_512.img' to 'LOLA128.bin'");
				((FileDialog)val).set_InitialDirectory(Utilities.AppPath + "\\Downloaded Files");
				if ((int)((CommonDialog)val).ShowDialog() == 1)
				{
					text2 = Path.GetDirectoryName(((FileDialog)val).get_FileName());
					if (((FileDialog)val).get_FileName().Contains("_512"))
					{
						flag = true;
						num = 16;
						for (int i = 0; i < num; i++)
						{
							if (!File.Exists(text2 + "/" + array[i]))
							{
								text = text + array[i] + "\r\n";
							}
						}
						if (text.Length > 0)
						{
							MessageBox.Show("The following ldem_512 files are missing:\r\n" + text + "The conversion can't procede without these files", "Missing 512 files", (MessageBoxButtons)0, (MessageBoxIcon)48);
							return;
						}
					}
					((Control)this).set_Cursor(Cursors.get_WaitCursor());
					PBar pBar = new PBar();
					pBar.pBarFTP.set_Minimum(0);
					pBar.pBarFTP.set_Maximum(23040);
					((Control)pBar).Show();
					for (int j = 0; j < num; j++)
					{
						if (num == 1)
						{
							text3 = text2 + "/LDEM_128.IMG";
							((Control)pBar).set_Text("LDEM_128.IMG");
						}
						else
						{
							text3 = text2 + "/" + array[j];
							((Control)pBar).set_Text("File #" + j + " :" + array[j]);
						}
						using FileStream input = new FileStream(text3, FileMode.Open, FileAccess.Read);
						using BinaryReader binaryReader = new BinaryReader(input);
						for (int k = 0; k < 23040; k++)
						{
							double num9 = ((!flag) ? (((double)(11520 - k) + 0.5) / 128.0) : (array3[j] + ((double)(11520 - k) + 0.5) / 512.0));
							((Control)lblLola).set_Text(string.Format("Long={1,0:f0}, Lat={0,1:f2}", num9, array2[j]));
							pBar.pBarFTP.set_Value(k);
							Application.DoEvents();
							for (int l = 0; l < 46080; l++)
							{
								num7 = ((!flag) ? (((double)l + 0.5) / 128.0) : (array2[j] + ((double)l + 0.5) / 512.0));
								int num10 = binaryReader.ReadInt16() / 2;
								double y = Math.Cos(num9 / (180.0 / Math.PI)) * Math.Cos(num7 / (180.0 / Math.PI));
								double num11 = Math.Cos(num9 / (180.0 / Math.PI)) * Math.Sin(num7 / (180.0 / Math.PI));
								double num12 = Math.Sin(num9 / (180.0 / Math.PI));
								double num13 = Math.Atan2(0.0 - num11, num12) * (180.0 / Math.PI);
								double num14 = Math.Atan2(y, Math.Sqrt(num11 * num11 + num12 * num12)) * (180.0 / Math.PI);
								if (num13 < 0.0)
								{
									num13 += 360.0;
								}
								if (Math.Abs(num14) <= 20.0)
								{
									int num15 = Convert.ToInt32(num13 * (double)num2);
									if (num15 >= 36000)
									{
										num15 = 0;
									}
									num8 = Convert.ToInt32((20.0 + num14) * (double)num4) + num5 * num15;
									if (array5[num8] == 0L)
									{
										array4[num8] = num10;
									}
									else if (num10 > array4[num8])
									{
										array4[num8] = num10;
									}
									array5[num8]++;
								}
							}
						}
					}
					((Form)pBar).Close();
					if (File.Exists(Utilities.AppPath + "\\Resource Files\\LOLA128.old"))
					{
						File.Delete(Utilities.AppPath + "\\Resource Files\\LOLA128.old");
					}
					if (File.Exists(Utilities.AppPath + "\\Resource Files\\LOLA128.bin"))
					{
						File.Move(Utilities.AppPath + "\\Resource Files\\LOLA128.bin", Utilities.AppPath + "\\Resource Files\\LOLA128.old");
					}
					using (FileStream output = new FileStream(Utilities.AppPath + "\\Resource Files\\LOLA128.bin", FileMode.Create, FileAccess.Write))
					{
						using BinaryWriter binaryWriter = new BinaryWriter(output);
						for (long num16 = 0L; num16 < num6; num16++)
						{
							binaryWriter.Write(Convert.ToInt16(array4[num16]));
						}
					}
					((Control)this).set_Cursor(Cursors.get_Default());
				}
			}
			finally
			{
				((IDisposable)val)?.Dispose();
			}
			((Component)this).Dispose();
		}

		private void cmdDowloadLightCurves_Click(object sender, EventArgs e)
		{
			string occultServer = Settings.Default.OccultServer;
			string text = "LightCurves.zip";
			string finalDestination = Utilities.AppPath + "\\LightCurves\\";
			if ((occultServer.Trim().Length == 0) | (text.Trim().Length == 0))
			{
				MissingDownloadDetails(occultServer, text);
				return;
			}
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, ShowMessages: true);
			((Control)this).set_Cursor(Cursors.get_Default());
			DisableCancelButton();
			SetFileDates();
			SetDownloadFlags();
		}

		private void cmdDownloadGaia9_Click(object sender, EventArgs e)
		{
			string occultServer = Settings.Default.OccultServer;
			string text = "Gaia9_DR3.zip";
			string finalDestination = Utilities.AppPath + "\\Resource Files\\Gaia\\";
			if ((occultServer.Trim().Length == 0) | (text.Trim().Length == 0))
			{
				MissingDownloadDetails(occultServer, text);
				return;
			}
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, ShowMessages: true);
			((Control)this).set_Cursor(Cursors.get_Default());
			DisableCancelButton();
			SetFileDates();
			SetDownloadFlags();
			if (!File.Exists(Utilities.AppPath + "\\Resource Files\\Gaia\\U4_Gaia14.inx"))
			{
				http.DownloadUCAC4IndexFile();
			}
		}

		private void cmdDownloadGaia12_Click(object sender, EventArgs e)
		{
			string occultServer = Settings.Default.OccultServer;
			string text = "Gaia12_DR3.zip";
			string finalDestination = Utilities.AppPath + "\\Resource Files\\Gaia\\";
			if ((occultServer.Trim().Length == 0) | (text.Trim().Length == 0))
			{
				MissingDownloadDetails(occultServer, text);
				return;
			}
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, ShowMessages: true);
			((Control)this).set_Cursor(Cursors.get_Default());
			DisableCancelButton();
			SetFileDates();
			SetDownloadFlags();
			if (!File.Exists(Utilities.AppPath + "\\Resource Files\\Gaia\\U4_Gaia14.inx"))
			{
				http.DownloadUCAC4IndexFile();
			}
		}

		private void cmdDownloadGaia14_Click(object sender, EventArgs e)
		{
			string occultServer = Settings.Default.OccultServer;
			string text = "Gaia14_DR3.zip";
			string finalDestination = Utilities.AppPath + "\\Resource Files\\Gaia\\";
			if ((occultServer.Trim().Length == 0) | (text.Trim().Length == 0))
			{
				MissingDownloadDetails(occultServer, text);
				return;
			}
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, ShowMessages: true);
			((Control)this).set_Cursor(Cursors.get_Default());
			DisableCancelButton();
			SetFileDates();
			SetDownloadFlags();
			if (!File.Exists(Utilities.AppPath + "\\Resource Files\\Gaia\\U4_Gaia14.inx"))
			{
				http.DownloadUCAC4IndexFile();
			}
		}

		private void cmdDownloadGaia16_Click(object sender, EventArgs e)
		{
			string occultServer = Settings.Default.OccultServer;
			string text = "Gaia16_DR3.zip";
			string finalDestination = Utilities.AppPath + "\\Resource Files\\Gaia\\";
			if ((occultServer.Trim().Length == 0) | (text.Trim().Length == 0))
			{
				MissingDownloadDetails(occultServer, text);
				return;
			}
			EnableCancelButton();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, ShowMessages: true);
			((Control)this).set_Cursor(Cursors.get_Default());
			DisableCancelButton();
			SetFileDates();
			SetDownloadFlags();
			if (!File.Exists(Utilities.AppPath + "\\Resource Files\\Gaia\\U4_Gaia14.inx"))
			{
				http.DownloadUCAC4IndexFile();
			}
		}

		private void cmdUBSC_Click(object sender, EventArgs e)
		{
			http.GetUBSCcatalogue();
			SetFileDates();
		}

		private void cmdSAO_Click(object sender, EventArgs e)
		{
			http.GetSAOcatalogue();
			SetFileDates();
		}

		private void cmdEarth2014_Click(object sender, EventArgs e)
		{
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.GetEarth2014();
			((Control)this).set_Cursor(Cursors.get_Default());
			SetFileDates();
		}

		private void cmdGaiaDoubles_Click(object sender, EventArgs e)
		{
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.GetGaiaDoublesFile();
			((Control)this).set_Cursor(Cursors.get_Default());
			SetFileDates();
		}

		private void cmdDownloadAAVSO_Occult_Click(object sender, EventArgs e)
		{
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.GetAAVSOFile();
			((Control)this).set_Cursor(Cursors.get_Default());
			SetFileDates();
		}

		private void cmdUpdateDAMIT_Click(object sender, EventArgs e)
		{
			if (Asteroid_Observations_Reports.Display_ShapeModels == null)
			{
				Asteroid_Observations_Reports.Display_ShapeModels = new DisplayShapeModels();
				((Control)Asteroid_Observations_Reports.Display_ShapeModels).set_Top(10);
				((Control)Asteroid_Observations_Reports.Display_ShapeModels).set_Left(50);
			}
			GetShapeModelData.DownloadAllDAMITModels();
			DisplayShapeModels.InitialiseShapeModels();
			SetFileDates();
			SetDownloadFlags();
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
			//IL_016c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0176: Expected O, but got Unknown
			//IL_0177: Unknown result type (might be due to invalid IL or missing references)
			//IL_0181: Expected O, but got Unknown
			//IL_0182: Unknown result type (might be due to invalid IL or missing references)
			//IL_018c: Expected O, but got Unknown
			//IL_018d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0197: Expected O, but got Unknown
			//IL_0198: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a2: Expected O, but got Unknown
			//IL_01a3: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ad: Expected O, but got Unknown
			//IL_01ae: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b8: Expected O, but got Unknown
			//IL_01b9: Unknown result type (might be due to invalid IL or missing references)
			//IL_01c3: Expected O, but got Unknown
			//IL_01c4: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ce: Expected O, but got Unknown
			//IL_01cf: Unknown result type (might be due to invalid IL or missing references)
			//IL_01d9: Expected O, but got Unknown
			//IL_01da: Unknown result type (might be due to invalid IL or missing references)
			//IL_01e4: Expected O, but got Unknown
			//IL_01e5: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ef: Expected O, but got Unknown
			//IL_01f0: Unknown result type (might be due to invalid IL or missing references)
			//IL_01fa: Expected O, but got Unknown
			//IL_01fb: Unknown result type (might be due to invalid IL or missing references)
			//IL_0205: Expected O, but got Unknown
			//IL_0206: Unknown result type (might be due to invalid IL or missing references)
			//IL_0210: Expected O, but got Unknown
			//IL_0211: Unknown result type (might be due to invalid IL or missing references)
			//IL_021b: Expected O, but got Unknown
			//IL_021c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0226: Expected O, but got Unknown
			//IL_0227: Unknown result type (might be due to invalid IL or missing references)
			//IL_0231: Expected O, but got Unknown
			//IL_0232: Unknown result type (might be due to invalid IL or missing references)
			//IL_023c: Expected O, but got Unknown
			//IL_023d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0247: Expected O, but got Unknown
			//IL_0248: Unknown result type (might be due to invalid IL or missing references)
			//IL_0252: Expected O, but got Unknown
			//IL_0253: Unknown result type (might be due to invalid IL or missing references)
			//IL_025d: Expected O, but got Unknown
			//IL_025e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0268: Expected O, but got Unknown
			//IL_0269: Unknown result type (might be due to invalid IL or missing references)
			//IL_0273: Expected O, but got Unknown
			//IL_0274: Unknown result type (might be due to invalid IL or missing references)
			//IL_027e: Expected O, but got Unknown
			//IL_027f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0289: Expected O, but got Unknown
			//IL_028a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0294: Expected O, but got Unknown
			//IL_0295: Unknown result type (might be due to invalid IL or missing references)
			//IL_029f: Expected O, but got Unknown
			//IL_02a0: Unknown result type (might be due to invalid IL or missing references)
			//IL_02aa: Expected O, but got Unknown
			//IL_02ab: Unknown result type (might be due to invalid IL or missing references)
			//IL_02b5: Expected O, but got Unknown
			//IL_02b6: Unknown result type (might be due to invalid IL or missing references)
			//IL_02c0: Expected O, but got Unknown
			//IL_02c1: Unknown result type (might be due to invalid IL or missing references)
			//IL_02cb: Expected O, but got Unknown
			//IL_02cc: Unknown result type (might be due to invalid IL or missing references)
			//IL_02d6: Expected O, but got Unknown
			//IL_02d7: Unknown result type (might be due to invalid IL or missing references)
			//IL_02e1: Expected O, but got Unknown
			//IL_02e2: Unknown result type (might be due to invalid IL or missing references)
			//IL_02ec: Expected O, but got Unknown
			//IL_02ed: Unknown result type (might be due to invalid IL or missing references)
			//IL_02f7: Expected O, but got Unknown
			//IL_02f8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0302: Expected O, but got Unknown
			//IL_0303: Unknown result type (might be due to invalid IL or missing references)
			//IL_030d: Expected O, but got Unknown
			//IL_030e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0318: Expected O, but got Unknown
			//IL_0319: Unknown result type (might be due to invalid IL or missing references)
			//IL_0323: Expected O, but got Unknown
			//IL_0324: Unknown result type (might be due to invalid IL or missing references)
			//IL_032e: Expected O, but got Unknown
			//IL_032f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0339: Expected O, but got Unknown
			//IL_033a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0344: Expected O, but got Unknown
			//IL_0345: Unknown result type (might be due to invalid IL or missing references)
			//IL_034f: Expected O, but got Unknown
			//IL_0350: Unknown result type (might be due to invalid IL or missing references)
			//IL_035a: Expected O, but got Unknown
			//IL_035b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0365: Expected O, but got Unknown
			//IL_0366: Unknown result type (might be due to invalid IL or missing references)
			//IL_0370: Expected O, but got Unknown
			//IL_0371: Unknown result type (might be due to invalid IL or missing references)
			//IL_037b: Expected O, but got Unknown
			//IL_037c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0386: Expected O, but got Unknown
			//IL_0387: Unknown result type (might be due to invalid IL or missing references)
			//IL_0391: Expected O, but got Unknown
			//IL_0392: Unknown result type (might be due to invalid IL or missing references)
			//IL_039c: Expected O, but got Unknown
			//IL_039d: Unknown result type (might be due to invalid IL or missing references)
			//IL_03a7: Expected O, but got Unknown
			//IL_03a8: Unknown result type (might be due to invalid IL or missing references)
			//IL_03b2: Expected O, but got Unknown
			//IL_03b3: Unknown result type (might be due to invalid IL or missing references)
			//IL_03bd: Expected O, but got Unknown
			//IL_03be: Unknown result type (might be due to invalid IL or missing references)
			//IL_03c8: Expected O, but got Unknown
			//IL_03c9: Unknown result type (might be due to invalid IL or missing references)
			//IL_03d3: Expected O, but got Unknown
			//IL_03d4: Unknown result type (might be due to invalid IL or missing references)
			//IL_03de: Expected O, but got Unknown
			//IL_03df: Unknown result type (might be due to invalid IL or missing references)
			//IL_03e9: Expected O, but got Unknown
			//IL_03ea: Unknown result type (might be due to invalid IL or missing references)
			//IL_03f4: Expected O, but got Unknown
			//IL_03f5: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ff: Expected O, but got Unknown
			//IL_0400: Unknown result type (might be due to invalid IL or missing references)
			//IL_040a: Expected O, but got Unknown
			//IL_040b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0415: Expected O, but got Unknown
			//IL_0416: Unknown result type (might be due to invalid IL or missing references)
			//IL_0420: Expected O, but got Unknown
			//IL_0421: Unknown result type (might be due to invalid IL or missing references)
			//IL_042b: Expected O, but got Unknown
			//IL_042c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0436: Expected O, but got Unknown
			//IL_0437: Unknown result type (might be due to invalid IL or missing references)
			//IL_0441: Expected O, but got Unknown
			//IL_0442: Unknown result type (might be due to invalid IL or missing references)
			//IL_044c: Expected O, but got Unknown
			//IL_044d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0457: Expected O, but got Unknown
			//IL_0458: Unknown result type (might be due to invalid IL or missing references)
			//IL_0462: Expected O, but got Unknown
			//IL_0463: Unknown result type (might be due to invalid IL or missing references)
			//IL_046d: Expected O, but got Unknown
			//IL_046e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0478: Expected O, but got Unknown
			//IL_0479: Unknown result type (might be due to invalid IL or missing references)
			//IL_0483: Expected O, but got Unknown
			//IL_0484: Unknown result type (might be due to invalid IL or missing references)
			//IL_048e: Expected O, but got Unknown
			//IL_048f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0499: Expected O, but got Unknown
			//IL_049a: Unknown result type (might be due to invalid IL or missing references)
			//IL_04a4: Expected O, but got Unknown
			//IL_04a5: Unknown result type (might be due to invalid IL or missing references)
			//IL_04af: Expected O, but got Unknown
			//IL_04b0: Unknown result type (might be due to invalid IL or missing references)
			//IL_04ba: Expected O, but got Unknown
			//IL_04bb: Unknown result type (might be due to invalid IL or missing references)
			//IL_04c5: Expected O, but got Unknown
			//IL_04c6: Unknown result type (might be due to invalid IL or missing references)
			//IL_04d0: Expected O, but got Unknown
			//IL_04d1: Unknown result type (might be due to invalid IL or missing references)
			//IL_04db: Expected O, but got Unknown
			//IL_04dc: Unknown result type (might be due to invalid IL or missing references)
			//IL_04e6: Expected O, but got Unknown
			//IL_04e7: Unknown result type (might be due to invalid IL or missing references)
			//IL_04f1: Expected O, but got Unknown
			//IL_04f2: Unknown result type (might be due to invalid IL or missing references)
			//IL_04fc: Expected O, but got Unknown
			//IL_04fd: Unknown result type (might be due to invalid IL or missing references)
			//IL_0507: Expected O, but got Unknown
			//IL_0508: Unknown result type (might be due to invalid IL or missing references)
			//IL_0512: Expected O, but got Unknown
			//IL_0513: Unknown result type (might be due to invalid IL or missing references)
			//IL_051d: Expected O, but got Unknown
			//IL_051e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0528: Expected O, but got Unknown
			//IL_0529: Unknown result type (might be due to invalid IL or missing references)
			//IL_0533: Expected O, but got Unknown
			//IL_0534: Unknown result type (might be due to invalid IL or missing references)
			//IL_053e: Expected O, but got Unknown
			//IL_053f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0549: Expected O, but got Unknown
			//IL_054a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0554: Expected O, but got Unknown
			//IL_0555: Unknown result type (might be due to invalid IL or missing references)
			//IL_055f: Expected O, but got Unknown
			//IL_0560: Unknown result type (might be due to invalid IL or missing references)
			//IL_056a: Expected O, but got Unknown
			//IL_056b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0575: Expected O, but got Unknown
			//IL_0576: Unknown result type (might be due to invalid IL or missing references)
			//IL_0580: Expected O, but got Unknown
			//IL_0581: Unknown result type (might be due to invalid IL or missing references)
			//IL_058b: Expected O, but got Unknown
			//IL_058c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0596: Expected O, but got Unknown
			//IL_0597: Unknown result type (might be due to invalid IL or missing references)
			//IL_05a1: Expected O, but got Unknown
			//IL_05a2: Unknown result type (might be due to invalid IL or missing references)
			//IL_05ac: Expected O, but got Unknown
			//IL_05ad: Unknown result type (might be due to invalid IL or missing references)
			//IL_05b7: Expected O, but got Unknown
			//IL_05b8: Unknown result type (might be due to invalid IL or missing references)
			//IL_05c2: Expected O, but got Unknown
			//IL_05c3: Unknown result type (might be due to invalid IL or missing references)
			//IL_05cd: Expected O, but got Unknown
			//IL_05ce: Unknown result type (might be due to invalid IL or missing references)
			//IL_05d8: Expected O, but got Unknown
			//IL_05d9: Unknown result type (might be due to invalid IL or missing references)
			//IL_05e3: Expected O, but got Unknown
			//IL_05e4: Unknown result type (might be due to invalid IL or missing references)
			//IL_05ee: Expected O, but got Unknown
			//IL_05ef: Unknown result type (might be due to invalid IL or missing references)
			//IL_05f9: Expected O, but got Unknown
			//IL_05fa: Unknown result type (might be due to invalid IL or missing references)
			//IL_0604: Expected O, but got Unknown
			//IL_0605: Unknown result type (might be due to invalid IL or missing references)
			//IL_060f: Expected O, but got Unknown
			//IL_0610: Unknown result type (might be due to invalid IL or missing references)
			//IL_061a: Expected O, but got Unknown
			//IL_061b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0625: Expected O, but got Unknown
			//IL_0626: Unknown result type (might be due to invalid IL or missing references)
			//IL_0630: Expected O, but got Unknown
			//IL_0631: Unknown result type (might be due to invalid IL or missing references)
			//IL_063b: Expected O, but got Unknown
			//IL_063c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0646: Expected O, but got Unknown
			//IL_0647: Unknown result type (might be due to invalid IL or missing references)
			//IL_0651: Expected O, but got Unknown
			//IL_0652: Unknown result type (might be due to invalid IL or missing references)
			//IL_065c: Expected O, but got Unknown
			//IL_065d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0667: Expected O, but got Unknown
			//IL_0668: Unknown result type (might be due to invalid IL or missing references)
			//IL_0672: Expected O, but got Unknown
			//IL_0673: Unknown result type (might be due to invalid IL or missing references)
			//IL_067d: Expected O, but got Unknown
			//IL_067e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0688: Expected O, but got Unknown
			//IL_0689: Unknown result type (might be due to invalid IL or missing references)
			//IL_0693: Expected O, but got Unknown
			//IL_0694: Unknown result type (might be due to invalid IL or missing references)
			//IL_069e: Expected O, but got Unknown
			//IL_069f: Unknown result type (might be due to invalid IL or missing references)
			//IL_06a9: Expected O, but got Unknown
			//IL_06aa: Unknown result type (might be due to invalid IL or missing references)
			//IL_06b4: Expected O, but got Unknown
			//IL_06b5: Unknown result type (might be due to invalid IL or missing references)
			//IL_06bf: Expected O, but got Unknown
			//IL_06c0: Unknown result type (might be due to invalid IL or missing references)
			//IL_06ca: Expected O, but got Unknown
			//IL_06cb: Unknown result type (might be due to invalid IL or missing references)
			//IL_06d5: Expected O, but got Unknown
			//IL_06d6: Unknown result type (might be due to invalid IL or missing references)
			//IL_06e0: Expected O, but got Unknown
			//IL_06e1: Unknown result type (might be due to invalid IL or missing references)
			//IL_06eb: Expected O, but got Unknown
			//IL_06ec: Unknown result type (might be due to invalid IL or missing references)
			//IL_06f6: Expected O, but got Unknown
			//IL_06f7: Unknown result type (might be due to invalid IL or missing references)
			//IL_0701: Expected O, but got Unknown
			//IL_0702: Unknown result type (might be due to invalid IL or missing references)
			//IL_070c: Expected O, but got Unknown
			//IL_070d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0717: Expected O, but got Unknown
			//IL_0718: Unknown result type (might be due to invalid IL or missing references)
			//IL_0722: Expected O, but got Unknown
			//IL_0723: Unknown result type (might be due to invalid IL or missing references)
			//IL_072d: Expected O, but got Unknown
			//IL_072e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0738: Expected O, but got Unknown
			//IL_0739: Unknown result type (might be due to invalid IL or missing references)
			//IL_0743: Expected O, but got Unknown
			//IL_0744: Unknown result type (might be due to invalid IL or missing references)
			//IL_074e: Expected O, but got Unknown
			//IL_074f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0759: Expected O, but got Unknown
			//IL_075a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0764: Expected O, but got Unknown
			//IL_0765: Unknown result type (might be due to invalid IL or missing references)
			//IL_076f: Expected O, but got Unknown
			//IL_0770: Unknown result type (might be due to invalid IL or missing references)
			//IL_077a: Expected O, but got Unknown
			//IL_077b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0785: Expected O, but got Unknown
			//IL_0786: Unknown result type (might be due to invalid IL or missing references)
			//IL_0790: Expected O, but got Unknown
			//IL_0791: Unknown result type (might be due to invalid IL or missing references)
			//IL_079b: Expected O, but got Unknown
			//IL_079c: Unknown result type (might be due to invalid IL or missing references)
			//IL_07a6: Expected O, but got Unknown
			//IL_07a7: Unknown result type (might be due to invalid IL or missing references)
			//IL_07b1: Expected O, but got Unknown
			//IL_07b2: Unknown result type (might be due to invalid IL or missing references)
			//IL_07bc: Expected O, but got Unknown
			//IL_07bd: Unknown result type (might be due to invalid IL or missing references)
			//IL_07c7: Expected O, but got Unknown
			//IL_07c8: Unknown result type (might be due to invalid IL or missing references)
			//IL_07d2: Expected O, but got Unknown
			//IL_07d3: Unknown result type (might be due to invalid IL or missing references)
			//IL_07dd: Expected O, but got Unknown
			//IL_07de: Unknown result type (might be due to invalid IL or missing references)
			//IL_07e8: Expected O, but got Unknown
			//IL_07e9: Unknown result type (might be due to invalid IL or missing references)
			//IL_07f3: Expected O, but got Unknown
			//IL_07f4: Unknown result type (might be due to invalid IL or missing references)
			//IL_07fe: Expected O, but got Unknown
			//IL_07ff: Unknown result type (might be due to invalid IL or missing references)
			//IL_0809: Expected O, but got Unknown
			//IL_080a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0814: Expected O, but got Unknown
			//IL_0815: Unknown result type (might be due to invalid IL or missing references)
			//IL_081f: Expected O, but got Unknown
			//IL_0820: Unknown result type (might be due to invalid IL or missing references)
			//IL_082a: Expected O, but got Unknown
			//IL_082b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0835: Expected O, but got Unknown
			//IL_0836: Unknown result type (might be due to invalid IL or missing references)
			//IL_0840: Expected O, but got Unknown
			//IL_0841: Unknown result type (might be due to invalid IL or missing references)
			//IL_084b: Expected O, but got Unknown
			//IL_084c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0856: Expected O, but got Unknown
			//IL_0857: Unknown result type (might be due to invalid IL or missing references)
			//IL_0861: Expected O, but got Unknown
			//IL_0862: Unknown result type (might be due to invalid IL or missing references)
			//IL_086c: Expected O, but got Unknown
			//IL_086d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0877: Expected O, but got Unknown
			//IL_0878: Unknown result type (might be due to invalid IL or missing references)
			//IL_0882: Expected O, but got Unknown
			//IL_0883: Unknown result type (might be due to invalid IL or missing references)
			//IL_088d: Expected O, but got Unknown
			//IL_088e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0898: Expected O, but got Unknown
			//IL_0899: Unknown result type (might be due to invalid IL or missing references)
			//IL_08a3: Expected O, but got Unknown
			//IL_08a4: Unknown result type (might be due to invalid IL or missing references)
			//IL_08ae: Expected O, but got Unknown
			//IL_08af: Unknown result type (might be due to invalid IL or missing references)
			//IL_08b9: Expected O, but got Unknown
			//IL_08ba: Unknown result type (might be due to invalid IL or missing references)
			//IL_08c4: Expected O, but got Unknown
			//IL_08c5: Unknown result type (might be due to invalid IL or missing references)
			//IL_08cf: Expected O, but got Unknown
			//IL_08d0: Unknown result type (might be due to invalid IL or missing references)
			//IL_08da: Expected O, but got Unknown
			//IL_08db: Unknown result type (might be due to invalid IL or missing references)
			//IL_08e5: Expected O, but got Unknown
			//IL_08e6: Unknown result type (might be due to invalid IL or missing references)
			//IL_08f0: Expected O, but got Unknown
			//IL_08f1: Unknown result type (might be due to invalid IL or missing references)
			//IL_08fb: Expected O, but got Unknown
			//IL_08fc: Unknown result type (might be due to invalid IL or missing references)
			//IL_0906: Expected O, but got Unknown
			//IL_0907: Unknown result type (might be due to invalid IL or missing references)
			//IL_0911: Expected O, but got Unknown
			//IL_0912: Unknown result type (might be due to invalid IL or missing references)
			//IL_091c: Expected O, but got Unknown
			//IL_091d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0927: Expected O, but got Unknown
			//IL_0928: Unknown result type (might be due to invalid IL or missing references)
			//IL_0932: Expected O, but got Unknown
			//IL_0933: Unknown result type (might be due to invalid IL or missing references)
			//IL_093d: Expected O, but got Unknown
			//IL_093e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0948: Expected O, but got Unknown
			//IL_0949: Unknown result type (might be due to invalid IL or missing references)
			//IL_0953: Expected O, but got Unknown
			//IL_0954: Unknown result type (might be due to invalid IL or missing references)
			//IL_095e: Expected O, but got Unknown
			//IL_095f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0969: Expected O, but got Unknown
			//IL_096a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0974: Expected O, but got Unknown
			//IL_0975: Unknown result type (might be due to invalid IL or missing references)
			//IL_097f: Expected O, but got Unknown
			//IL_0980: Unknown result type (might be due to invalid IL or missing references)
			//IL_098a: Expected O, but got Unknown
			//IL_098b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0995: Expected O, but got Unknown
			//IL_0996: Unknown result type (might be due to invalid IL or missing references)
			//IL_09a0: Expected O, but got Unknown
			//IL_09a1: Unknown result type (might be due to invalid IL or missing references)
			//IL_09ab: Expected O, but got Unknown
			//IL_09ac: Unknown result type (might be due to invalid IL or missing references)
			//IL_09b6: Expected O, but got Unknown
			//IL_09b7: Unknown result type (might be due to invalid IL or missing references)
			//IL_09c1: Expected O, but got Unknown
			//IL_09c2: Unknown result type (might be due to invalid IL or missing references)
			//IL_09cc: Expected O, but got Unknown
			//IL_09cd: Unknown result type (might be due to invalid IL or missing references)
			//IL_09d7: Expected O, but got Unknown
			//IL_09d8: Unknown result type (might be due to invalid IL or missing references)
			//IL_09e2: Expected O, but got Unknown
			//IL_09e3: Unknown result type (might be due to invalid IL or missing references)
			//IL_09ed: Expected O, but got Unknown
			//IL_09ee: Unknown result type (might be due to invalid IL or missing references)
			//IL_09f8: Expected O, but got Unknown
			//IL_09f9: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a03: Expected O, but got Unknown
			//IL_0a04: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a0e: Expected O, but got Unknown
			//IL_0a0f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a19: Expected O, but got Unknown
			//IL_0a1a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a24: Expected O, but got Unknown
			//IL_0a25: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a2f: Expected O, but got Unknown
			//IL_0a30: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a3a: Expected O, but got Unknown
			//IL_0a3b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a45: Expected O, but got Unknown
			//IL_0a46: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a50: Expected O, but got Unknown
			//IL_0a51: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a5b: Expected O, but got Unknown
			//IL_0a5c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a66: Expected O, but got Unknown
			//IL_0a67: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a71: Expected O, but got Unknown
			//IL_0a72: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a7c: Expected O, but got Unknown
			//IL_0a7d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a87: Expected O, but got Unknown
			//IL_0a88: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a92: Expected O, but got Unknown
			//IL_0a93: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a9d: Expected O, but got Unknown
			//IL_0a9e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0aa8: Expected O, but got Unknown
			//IL_0aa9: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ab3: Expected O, but got Unknown
			//IL_0ab4: Unknown result type (might be due to invalid IL or missing references)
			//IL_0abe: Expected O, but got Unknown
			//IL_0abf: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ac9: Expected O, but got Unknown
			//IL_0aca: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ad4: Expected O, but got Unknown
			//IL_0ad5: Unknown result type (might be due to invalid IL or missing references)
			//IL_0adf: Expected O, but got Unknown
			//IL_0ae0: Unknown result type (might be due to invalid IL or missing references)
			//IL_0aea: Expected O, but got Unknown
			//IL_0aeb: Unknown result type (might be due to invalid IL or missing references)
			//IL_0af5: Expected O, but got Unknown
			//IL_0af6: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b00: Expected O, but got Unknown
			//IL_0b01: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b0b: Expected O, but got Unknown
			//IL_0b0c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b16: Expected O, but got Unknown
			//IL_0b17: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b21: Expected O, but got Unknown
			//IL_0b22: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b2c: Expected O, but got Unknown
			//IL_0b2d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b37: Expected O, but got Unknown
			//IL_0b38: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b42: Expected O, but got Unknown
			//IL_0b43: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b4d: Expected O, but got Unknown
			//IL_0b4e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b58: Expected O, but got Unknown
			//IL_0b59: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b63: Expected O, but got Unknown
			//IL_0b64: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b6e: Expected O, but got Unknown
			//IL_0b6f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b79: Expected O, but got Unknown
			//IL_0b7a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b84: Expected O, but got Unknown
			//IL_0b85: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b8f: Expected O, but got Unknown
			//IL_0b90: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b9a: Expected O, but got Unknown
			//IL_0b9b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ba5: Expected O, but got Unknown
			//IL_0ba6: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bb0: Expected O, but got Unknown
			//IL_0bb1: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bbb: Expected O, but got Unknown
			//IL_0bbc: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bc6: Expected O, but got Unknown
			//IL_0bc7: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bd1: Expected O, but got Unknown
			//IL_0bd2: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bdc: Expected O, but got Unknown
			//IL_0bdd: Unknown result type (might be due to invalid IL or missing references)
			//IL_0be7: Expected O, but got Unknown
			//IL_0be8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bf2: Expected O, but got Unknown
			//IL_0bf3: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bfd: Expected O, but got Unknown
			//IL_0bfe: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c08: Expected O, but got Unknown
			//IL_0c09: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c13: Expected O, but got Unknown
			//IL_0c14: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c1e: Expected O, but got Unknown
			//IL_0c1f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c29: Expected O, but got Unknown
			//IL_0c2a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c34: Expected O, but got Unknown
			//IL_0c35: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c3f: Expected O, but got Unknown
			//IL_0c40: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c4a: Expected O, but got Unknown
			//IL_0c4b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c55: Expected O, but got Unknown
			//IL_0c56: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c60: Expected O, but got Unknown
			//IL_0c61: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c6b: Expected O, but got Unknown
			//IL_0c6c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c76: Expected O, but got Unknown
			//IL_0c77: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c81: Expected O, but got Unknown
			//IL_0c82: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c8c: Expected O, but got Unknown
			//IL_0c8d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c97: Expected O, but got Unknown
			//IL_0c98: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ca2: Expected O, but got Unknown
			//IL_0ca3: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cad: Expected O, but got Unknown
			//IL_0cae: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cb8: Expected O, but got Unknown
			//IL_0cb9: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cc3: Expected O, but got Unknown
			//IL_0cc4: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cce: Expected O, but got Unknown
			//IL_0ccf: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cd9: Expected O, but got Unknown
			//IL_0cda: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ce4: Expected O, but got Unknown
			//IL_0ce5: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cef: Expected O, but got Unknown
			//IL_0cf0: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cfa: Expected O, but got Unknown
			//IL_0cfb: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d05: Expected O, but got Unknown
			//IL_0d06: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d10: Expected O, but got Unknown
			//IL_0d11: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d1b: Expected O, but got Unknown
			//IL_0d1c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d26: Expected O, but got Unknown
			//IL_0d27: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d31: Expected O, but got Unknown
			//IL_0d32: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d3c: Expected O, but got Unknown
			//IL_0d3d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d47: Expected O, but got Unknown
			//IL_0d48: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d52: Expected O, but got Unknown
			//IL_0d53: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d5d: Expected O, but got Unknown
			//IL_0d5e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d68: Expected O, but got Unknown
			//IL_0d69: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d73: Expected O, but got Unknown
			//IL_0d74: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d7e: Expected O, but got Unknown
			//IL_0d7f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d89: Expected O, but got Unknown
			//IL_0d8a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d94: Expected O, but got Unknown
			//IL_0d95: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d9f: Expected O, but got Unknown
			//IL_0da0: Unknown result type (might be due to invalid IL or missing references)
			//IL_0daa: Expected O, but got Unknown
			//IL_0dab: Unknown result type (might be due to invalid IL or missing references)
			//IL_0db5: Expected O, but got Unknown
			//IL_0db6: Unknown result type (might be due to invalid IL or missing references)
			//IL_0dc0: Expected O, but got Unknown
			//IL_0dc1: Unknown result type (might be due to invalid IL or missing references)
			//IL_0dcb: Expected O, but got Unknown
			//IL_0dcc: Unknown result type (might be due to invalid IL or missing references)
			//IL_0dd6: Expected O, but got Unknown
			//IL_2999: Unknown result type (might be due to invalid IL or missing references)
			//IL_29a3: Expected O, but got Unknown
			//IL_de78: Unknown result type (might be due to invalid IL or missing references)
			//IL_de82: Expected O, but got Unknown
			//IL_ded6: Unknown result type (might be due to invalid IL or missing references)
			//IL_dee0: Expected O, but got Unknown
			components = new Container();
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(Downloads));
			cmdComet = new Button();
			cmdFuture = new Button();
			label1 = new Label();
			label2 = new Label();
			label3 = new Label();
			cmdFuture540 = new Button();
			label4 = new Label();
			cmdAstorb = new Button();
			cmdAstorbVizier = new Button();
			label6 = new Label();
			cmdMPCorbCzech = new Button();
			label7 = new Label();
			cmdMPCorb = new Button();
			label8 = new Label();
			cmdCancel = new Button();
			cmdDowload_EOP_new = new Button();
			cmdDowload_EOP_Old = new Button();
			label9 = new Label();
			label10 = new Label();
			label11 = new Label();
			cmdLunar = new Button();
			label12 = new Label();
			cmdTycho2 = new Button();
			label13 = new Label();
			cmdJPL_DE = new Button();
			label14 = new Label();
			cmdAsteroidObservations = new Button();
			grpLargeButSeldom = new GroupBox();
			lblGaia9 = new Label();
			DownloadFlagGaia9 = new Button();
			imageList1 = new ImageList(components);
			label150 = new Label();
			label154 = new Label();
			label155 = new Label();
			cmdDownloadGaia9 = new Button();
			lblGaiaDoubles = new Label();
			label51 = new Label();
			label114 = new Label();
			label122 = new Label();
			cmdGaiaDoubles = new Button();
			label113 = new Label();
			lblEarth2014 = new Label();
			label88 = new Label();
			label111 = new Label();
			label112 = new Label();
			cmdEarth2014 = new Button();
			lblUBSC = new Label();
			label52 = new Label();
			label53 = new Label();
			label87 = new Label();
			cmdUBSC = new Button();
			DownloadFlagGaia14 = new Button();
			label146 = new Label();
			lblGaia14 = new Label();
			label151 = new Label();
			label152 = new Label();
			label153 = new Label();
			cmdDownloadGaia14 = new Button();
			DownloadFlagGaia16 = new Button();
			lblSAO = new Label();
			label138 = new Label();
			label139 = new Label();
			label140 = new Label();
			cmdSAO = new Button();
			label136 = new Label();
			lblGaia16 = new Label();
			lblGaia12 = new Label();
			DownloadFlagGaia12 = new Button();
			label131 = new Label();
			label132 = new Label();
			label133 = new Label();
			cmdDownloadGaia16 = new Button();
			label72 = new Label();
			label129 = new Label();
			label130 = new Label();
			cmdDownloadGaia12 = new Button();
			label128 = new Label();
			label121 = new Label();
			label32 = new Label();
			label74 = new Label();
			cmdDownloadInstallationFiles = new Button();
			label17 = new Label();
			lblLola = new Label();
			cmdCreateLOLA128 = new Button();
			lblLOLA128File = new Label();
			label56 = new Label();
			DownloadFlagLOLA128 = new Button();
			label60 = new Label();
			label65 = new Label();
			cmdLOLA = new Button();
			lblKepler2File = new Label();
			label46 = new Label();
			DownloadFlagKepler2 = new Button();
			label47 = new Label();
			label54 = new Label();
			cmdDownloadKepler2 = new Button();
			lblGoogleMapTemplatesFile = new Label();
			lblPhoebeFile = new Label();
			lblEOP_OldFile = new Label();
			lblCraterFile = new Label();
			lblJPL_DEfile = new Label();
			lblDE6000file = new Label();
			lblTycho2File = new Label();
			lblStarDiaFile = new Label();
			lblXZcatFile = new Label();
			lblLunarOldFiles = new Label();
			label106 = new Label();
			label109 = new Label();
			label110 = new Label();
			cmdDownloadGoogleMapFiles = new Button();
			label89 = new Label();
			label86 = new Label();
			label85 = new Label();
			label84 = new Label();
			label83 = new Label();
			label82 = new Label();
			label81 = new Label();
			label79 = new Label();
			DownloadFlagPhoebe = new Button();
			label71 = new Label();
			label78 = new Label();
			cmdDownloadePhoebeEphemeris = new Button();
			DownLoadFlagCraterTimings = new Button();
			label76 = new Label();
			label77 = new Label();
			cmdDownloadCraterTimings = new Button();
			label70 = new Label();
			label68 = new Label();
			label69 = new Label();
			cmdDE422 = new Button();
			DownloadFlagStarDias = new Button();
			label61 = new Label();
			label62 = new Label();
			cmdStarDiameters = new Button();
			DownloadFlagJPLDE = new Button();
			DownloadFlagTycho2 = new Button();
			DownloadFlagXZ = new Button();
			DownloadFlagLunarOld = new Button();
			label33 = new Label();
			label34 = new Label();
			cmdXZ = new Button();
			label21 = new Label();
			label16 = new Label();
			label15 = new Label();
			label30 = new Label();
			label31 = new Label();
			cmdLatestLunar = new Button();
			grpAsteroids = new GroupBox();
			label162 = new Label();
			label158 = new Label();
			cmdUpdateDAMIT = new Button();
			label144 = new Label();
			DownLoadFlagShapes = new Button();
			lblShapeModels = new Label();
			label147 = new Label();
			label148 = new Label();
			cmdDownloadShapes = new Button();
			label149 = new Label();
			label137 = new Label();
			cmdDownloadGaia = new Button();
			lblGaiaFile = new Label();
			label145 = new Label();
			label135 = new Label();
			label134 = new Label();
			label96 = new Label();
			label57 = new Label();
			lblAstDySUpdateDate = new Label();
			DownLoadFlagAstDyS = new Button();
			DownLoadFlagISAM = new Button();
			panel4 = new Panel();
			cmdCreateISAM = new Button();
			label5 = new Label();
			cmdDownloadISAM = new Button();
			label126 = new Label();
			lblFutureAllFile = new Label();
			lblAstorbFile = new Label();
			label19 = new Label();
			lblMPCfile = new Label();
			lblAstDysFile = new Label();
			lblISAMfile = new Label();
			label97 = new Label();
			label103 = new Label();
			label95 = new Label();
			label73 = new Label();
			label75 = new Label();
			label58 = new Label();
			cmdDownloadAstDyS = new Button();
			label55 = new Label();
			lblRetainFutureAll = new Label();
			label41 = new Label();
			label39 = new Label();
			label18 = new Label();
			label20 = new Label();
			grpGeneral = new GroupBox();
			label161 = new Label();
			lblAAVSOfile_Occult = new Label();
			DownloadFlagAAVSO_Occult = new Button();
			label159 = new Label();
			label160 = new Label();
			cmdDownloadAAVSO_Occult = new Button();
			label157 = new Label();
			updnMaxMagLimit = new NumericUpDown();
			label156 = new Label();
			updnMagLimitAAVSO = new NumericUpDown();
			label50 = new Label();
			lblAsteroidClassFile = new Label();
			label141 = new Label();
			DownloadFlagAsteroidClass = new Button();
			label142 = new Label();
			label143 = new Label();
			cmdAsteroidClass = new Button();
			label127 = new Label();
			lblLightCurves = new Label();
			label123 = new Label();
			DownloadFlagLightCurves = new Button();
			label124 = new Label();
			label125 = new Label();
			cmdDowloadLightCurves = new Button();
			panel3 = new Panel();
			cmbFileAge = new ComboBox();
			label59 = new Label();
			lblCameraDelayFile = new Label();
			lblReportAddressFile = new Label();
			lblLatestLunarFile = new Label();
			lblBinaryAsteroidFile = new Label();
			lblAsteroidRingFile = new Label();
			lblAsteroidDiameterFile = new Label();
			lblFutureFile = new Label();
			lblEOPPresentFile = new Label();
			lblAsteroidObsFile = new Label();
			lblDeltaTAfile = new Label();
			lbl6thOrbitFile = new Label();
			lblWDSFile = new Label();
			lblWDSCodesFile = new Label();
			lblINT4File = new Label();
			lblAAVSOfile = new Label();
			lblCALLfile = new Label();
			lblCometFile = new Label();
			DownloadFlagWDScodes = new Button();
			label120 = new Label();
			label118 = new Label();
			label119 = new Label();
			cmdDownloadWDSCodes = new Button();
			DownloadFlagRings = new Button();
			label117 = new Label();
			label116 = new Label();
			label115 = new Label();
			cmdRings = new Button();
			DownloadFlagCameraDelay = new Button();
			label107 = new Label();
			label108 = new Label();
			cmdCameraDelays = new Button();
			label105 = new Label();
			label104 = new Label();
			label102 = new Label();
			label101 = new Label();
			label100 = new Label();
			label99 = new Label();
			label98 = new Label();
			label94 = new Label();
			label93 = new Label();
			label92 = new Label();
			label91 = new Label();
			label90 = new Label();
			DownloadFlagVisualBinaries = new Button();
			label66 = new Label();
			label67 = new Label();
			cmdDownloadVisualBinaries = new Button();
			downloadFlag_CALL = new Button();
			label64 = new Label();
			label63 = new Label();
			label80 = new Label();
			cmdDownloadAsteroidLightCurveData = new Button();
			DownloadFlagAAVSO = new Button();
			label49 = new Label();
			label48 = new Label();
			cmdDownloadAAVSO = new Button();
			DownloadFlagWDS = new Button();
			DownloadFlagIF = new Button();
			label44 = new Label();
			label45 = new Label();
			cmdDownloadWDS = new Button();
			label42 = new Label();
			label43 = new Label();
			cmdDownloadInterferometric = new Button();
			DownloadFlagAddresses = new Button();
			DownloadFlagRecentLunar = new Button();
			DownloadFlagAsteroids = new Button();
			lblRetainFuture = new Label();
			label40 = new Label();
			cmdDownloadAll = new Button();
			DownloadFlagAsteroidDiameters = new Button();
			DownloadFlagBinary = new Button();
			DownloadFlagdeltaT = new Button();
			label37 = new Label();
			label38 = new Label();
			cmdAsteroidDiameters = new Button();
			label35 = new Label();
			label36 = new Label();
			cmdBinary = new Button();
			label28 = new Label();
			label29 = new Label();
			cmdDeltaT = new Button();
			label26 = new Label();
			label27 = new Label();
			cmdAddresses = new Button();
			label25 = new Label();
			label24 = new Label();
			label23 = new Label();
			label22 = new Label();
			cmdHelp = new Button();
			cmdExit = new Button();
			panel1 = new Panel();
			lblUpdateCount = new Label();
			panel2 = new Panel();
			lblUpdateDamitPreferred = new Label();
			panel5 = new Panel();
			label163 = new Label();
			label165 = new Label();
			((Control)grpLargeButSeldom).SuspendLayout();
			((Control)grpAsteroids).SuspendLayout();
			((Control)panel4).SuspendLayout();
			((Control)grpGeneral).SuspendLayout();
			((ISupportInitialize)updnMaxMagLimit).BeginInit();
			((ISupportInitialize)updnMagLimitAAVSO).BeginInit();
			((Control)panel3).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((Control)panel2).SuspendLayout();
			((Control)panel5).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)cmdComet).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdComet).set_Location(new Point(75, 62));
			((Control)cmdComet).set_Name("cmdComet");
			((Control)cmdComet).set_Size(new Size(64, 22));
			((Control)cmdComet).set_TabIndex(0);
			((Control)cmdComet).set_Text("Download");
			((ButtonBase)cmdComet).set_UseVisualStyleBackColor(true);
			((Control)cmdComet).add_Click((EventHandler)cmdComet_Click);
			((Control)cmdFuture).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdFuture).set_Location(new Point(75, 364));
			((Control)cmdFuture).set_Name("cmdFuture");
			((Control)cmdFuture).set_Size(new Size(64, 22));
			((Control)cmdFuture).set_TabIndex(1);
			((Control)cmdFuture).set_Text("Download");
			((ButtonBase)cmdFuture).set_UseVisualStyleBackColor(true);
			((Control)cmdFuture).add_Click((EventHandler)cmdFuture_Click);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(140, 67));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(107, 13));
			((Control)label1).set_TabIndex(2);
			((Control)label1).set_Text("2 Comet elements");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(304, 369));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(409, 13));
			((Control)label2).set_TabIndex(3);
			((Control)label2).set_Text("Steve Preston's files of asteroid occultation elements - covering the next 4 to 6 weeks");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(315, 38));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(113, 13));
			((Control)label3).set_TabIndex(4);
			((Control)label3).set_Text("20 FutureALL.xml  ");
			((Control)cmdFuture540).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdFuture540).set_Location(new Point(251, 33));
			((Control)cmdFuture540).set_Name("cmdFuture540");
			((Control)cmdFuture540).set_Size(new Size(64, 22));
			((Control)cmdFuture540).set_TabIndex(5);
			((Control)cmdFuture540).set_Text("Download");
			((ButtonBase)cmdFuture540).set_UseVisualStyleBackColor(true);
			((Control)cmdFuture540).add_Click((EventHandler)cmdFuture540_Click);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(75, 169));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(141, 26));
			((Control)label4).set_TabIndex(6);
			((Control)label4).set_Text("Astorb - Lowell observatory's\r\nfile of asteroid elements");
			((Control)cmdAstorb).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdAstorb).set_Location(new Point(252, 171));
			((Control)cmdAstorb).set_Name("cmdAstorb");
			((Control)cmdAstorb).set_Size(new Size(64, 22));
			((Control)cmdAstorb).set_TabIndex(7);
			((Control)cmdAstorb).set_Text("Download");
			((ButtonBase)cmdAstorb).set_UseVisualStyleBackColor(true);
			((Control)cmdAstorb).add_Click((EventHandler)cmdAstorb_Click);
			((Control)cmdAstorbVizier).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdAstorbVizier).set_Location(new Point(432, 171));
			((Control)cmdAstorbVizier).set_Name("cmdAstorbVizier");
			((Control)cmdAstorbVizier).set_Size(new Size(64, 22));
			((Control)cmdAstorbVizier).set_TabIndex(11);
			((Control)cmdAstorbVizier).set_Text("Download");
			((ButtonBase)cmdAstorbVizier).set_UseVisualStyleBackColor(true);
			((Control)cmdAstorbVizier).add_Click((EventHandler)cmdAstorbVizier_Click);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(497, 176));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(86, 13));
			((Control)label6).set_TabIndex(10);
			((Control)label6).set_Text("23a VizieR mirror");
			((Control)cmdMPCorbCzech).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdMPCorbCzech).set_Location(new Point(456, 73));
			((Control)cmdMPCorbCzech).set_Name("cmdMPCorbCzech");
			((Control)cmdMPCorbCzech).set_Size(new Size(64, 22));
			((Control)cmdMPCorbCzech).set_TabIndex(15);
			((Control)cmdMPCorbCzech).set_Text("Download");
			((ButtonBase)cmdMPCorbCzech).set_UseVisualStyleBackColor(true);
			((Control)cmdMPCorbCzech).add_Click((EventHandler)cmdMPCorbCzech_Click);
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(521, 78));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(113, 13));
			((Control)label7).set_TabIndex(14);
			((Control)label7).set_Text("22a Klet mirror (slow...)");
			((Control)cmdMPCorb).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdMPCorb).set_Location(new Point(251, 73));
			((Control)cmdMPCorb).set_Name("cmdMPCorb");
			((Control)cmdMPCorb).set_Size(new Size(64, 22));
			((Control)cmdMPCorb).set_TabIndex(13);
			((Control)cmdMPCorb).set_Text("Download");
			((ButtonBase)cmdMPCorb).set_UseVisualStyleBackColor(true);
			((Control)cmdMPCorb).add_Click((EventHandler)cmdMPCorb_Click);
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(75, 71));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(155, 26));
			((Control)label8).set_TabIndex(12);
			((Control)label8).set_Text("MPCORB - Minor Planet Center\r\nfile of asteroid elements");
			((Control)cmdCancel).set_Enabled(false);
			((Control)cmdCancel).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdCancel).set_Location(new Point(397, 7));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(124, 22));
			((Control)cmdCancel).set_TabIndex(16);
			((Control)cmdCancel).set_Text("Cancel download");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)cmdDowload_EOP_new).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDowload_EOP_new).set_Location(new Point(75, 24));
			((Control)cmdDowload_EOP_new).set_Name("cmdDowload_EOP_new");
			((Control)cmdDowload_EOP_new).set_Size(new Size(64, 22));
			((Control)cmdDowload_EOP_new).set_TabIndex(21);
			((Control)cmdDowload_EOP_new).set_Text("Download");
			((ButtonBase)cmdDowload_EOP_new).set_UseVisualStyleBackColor(true);
			((Control)cmdDowload_EOP_new).add_Click((EventHandler)cmdDowload_EOP_new_Click);
			((Control)cmdDowload_EOP_Old).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDowload_EOP_Old).set_Location(new Point(75, 325));
			((Control)cmdDowload_EOP_Old).set_Name("cmdDowload_EOP_Old");
			((Control)cmdDowload_EOP_Old).set_Size(new Size(64, 22));
			((Control)cmdDowload_EOP_Old).set_TabIndex(60);
			((Control)cmdDowload_EOP_Old).set_Text("Download");
			((ButtonBase)cmdDowload_EOP_Old).set_UseVisualStyleBackColor(true);
			((Control)cmdDowload_EOP_Old).add_Click((EventHandler)cmdDowload_EOP_Old_Click);
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(140, 29));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(117, 13));
			((Control)label9).set_TabIndex(22);
			((Control)label9).set_Text("1 EOP 1962 to now");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label10).set_Location(new Point(304, 323));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(400, 26));
			((Control)label10).set_TabIndex(62);
			((Control)label10).set_Text("Earth Orientation Parameters before 1962. This file should not require updating. Only\r\nupdate if advised to do so.");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label11).set_Location(new Point(141, 23));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(132, 26));
			((Control)label11).set_TabIndex(3);
			((Control)label11).set_Text("27 Historical files of \r\n     Lunar occultations");
			((Control)cmdLunar).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdLunar).set_Location(new Point(75, 25));
			((Control)cmdLunar).set_Name("cmdLunar");
			((Control)cmdLunar).set_Size(new Size(64, 22));
			((Control)cmdLunar).set_TabIndex(2);
			((Control)cmdLunar).set_Text("Download");
			((ButtonBase)cmdLunar).set_UseVisualStyleBackColor(true);
			((Control)cmdLunar).add_Click((EventHandler)cmdLunar_Click);
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold | FontStyle.Strikeout, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(141, 182));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(127, 13));
			((Control)label12).set_TabIndex(39);
			((Control)label12).set_Text("31 Tycho2 catalogue");
			((Control)cmdTycho2).set_Enabled(false);
			((Control)cmdTycho2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdTycho2).set_Location(new Point(75, 177));
			((Control)cmdTycho2).set_Name("cmdTycho2");
			((Control)cmdTycho2).set_Size(new Size(64, 22));
			((Control)cmdTycho2).set_TabIndex(38);
			((Control)cmdTycho2).set_Text("Download");
			((ButtonBase)cmdTycho2).set_UseVisualStyleBackColor(true);
			((Control)cmdTycho2).add_Click((EventHandler)cmdTycho2_Click);
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label13).set_Location(new Point(140, 256));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(163, 13));
			((Control)label13).set_TabIndex(51);
			((Control)label13).set_Text("33 JPL planetary ephemeris");
			((Control)cmdJPL_DE).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdJPL_DE).set_Location(new Point(75, 251));
			((Control)cmdJPL_DE).set_Name("cmdJPL_DE");
			((Control)cmdJPL_DE).set_Size(new Size(64, 22));
			((Control)cmdJPL_DE).set_TabIndex(50);
			((Control)cmdJPL_DE).set_Text("Download");
			((ButtonBase)cmdJPL_DE).set_UseVisualStyleBackColor(true);
			((Control)cmdJPL_DE).add_Click((EventHandler)cmdJPL_DE_Click);
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label14).set_Location(new Point(140, 105));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(144, 13));
			((Control)label14).set_TabIndex(31);
			((Control)label14).set_Text("3 Asteroid observations \r\n");
			((Control)cmdAsteroidObservations).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdAsteroidObservations).set_Location(new Point(75, 100));
			((Control)cmdAsteroidObservations).set_Name("cmdAsteroidObservations");
			((Control)cmdAsteroidObservations).set_Size(new Size(64, 22));
			((Control)cmdAsteroidObservations).set_TabIndex(30);
			((Control)cmdAsteroidObservations).set_Text("Download");
			((ButtonBase)cmdAsteroidObservations).set_UseVisualStyleBackColor(true);
			((Control)cmdAsteroidObservations).add_Click((EventHandler)cmdAsteroidObservations_Click);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)lblGaia9);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)DownloadFlagGaia9);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label150);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label154);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label155);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)cmdDownloadGaia9);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)lblGaiaDoubles);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label51);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label114);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label122);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)cmdGaiaDoubles);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label113);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)lblEarth2014);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label88);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label111);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label112);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)cmdEarth2014);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)lblUBSC);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label52);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label53);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label87);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)cmdUBSC);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)DownloadFlagGaia14);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label146);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)lblGaia14);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label151);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label152);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label153);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)cmdDownloadGaia14);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)DownloadFlagGaia16);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)lblSAO);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label138);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label139);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label140);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)cmdSAO);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label136);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)lblGaia16);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)lblGaia12);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)DownloadFlagGaia12);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label131);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label132);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label133);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)cmdDownloadGaia16);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label72);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label129);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label130);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)cmdDownloadGaia12);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label128);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label121);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label32);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label74);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)cmdDownloadInstallationFiles);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label17);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)lblLola);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)cmdCreateLOLA128);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)lblLOLA128File);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label56);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)DownloadFlagLOLA128);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label60);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label65);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)cmdLOLA);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)lblKepler2File);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label46);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)DownloadFlagKepler2);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label47);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label54);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)cmdDownloadKepler2);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)lblGoogleMapTemplatesFile);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)lblPhoebeFile);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)lblEOP_OldFile);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)lblCraterFile);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)lblJPL_DEfile);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)lblDE6000file);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)lblTycho2File);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)lblStarDiaFile);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)lblXZcatFile);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)lblLunarOldFiles);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label106);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label109);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label110);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)cmdDownloadGoogleMapFiles);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label89);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label86);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label85);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label84);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label83);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label82);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label81);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label79);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)DownloadFlagPhoebe);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label71);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label78);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)cmdDownloadePhoebeEphemeris);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)DownLoadFlagCraterTimings);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label76);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label77);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)cmdDownloadCraterTimings);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label70);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label68);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label69);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)cmdDE422);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)DownloadFlagStarDias);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label61);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label62);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)cmdStarDiameters);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)DownloadFlagJPLDE);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)DownloadFlagTycho2);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)DownloadFlagXZ);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)DownloadFlagLunarOld);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label33);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label34);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)cmdXZ);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label21);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label16);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label15);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label13);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label10);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)cmdJPL_DE);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label12);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)cmdTycho2);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)cmdDowload_EOP_Old);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)label11);
			((Control)grpLargeButSeldom).get_Controls().Add((Control)(object)cmdLunar);
			((Control)grpLargeButSeldom).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpLargeButSeldom).set_Location(new Point(12, 1279));
			((Control)grpLargeButSeldom).set_Name("grpLargeButSeldom");
			((Control)grpLargeButSeldom).set_Size(new Size(793, 798));
			((Control)grpLargeButSeldom).set_TabIndex(0);
			grpLargeButSeldom.set_TabStop(false);
			((Control)grpLargeButSeldom).set_Text("Static data files");
			((Control)lblGaia9).set_AutoSize(true);
			((Control)lblGaia9).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblGaia9).set_Location(new Point(5, 586));
			((Control)lblGaia9).set_Name("lblGaia9");
			((Control)lblGaia9).set_Size(new Size(36, 12));
			((Control)lblGaia9).set_TabIndex(137);
			((Control)lblGaia9).set_Text("-- --- ----");
			((ButtonBase)DownloadFlagGaia9).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownloadFlagGaia9).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownloadFlagGaia9).set_ImageIndex(0);
			((ButtonBase)DownloadFlagGaia9).set_ImageList(imageList1);
			((Control)DownloadFlagGaia9).set_Location(new Point(59, 581));
			((Control)DownloadFlagGaia9).set_Name("DownloadFlagGaia9");
			((Control)DownloadFlagGaia9).set_Size(new Size(14, 18));
			((Control)DownloadFlagGaia9).set_TabIndex(138);
			((ButtonBase)DownloadFlagGaia9).set_UseVisualStyleBackColor(true);
			((Control)DownloadFlagGaia9).set_Visible(false);
			imageList1.set_ImageStream((ImageListStreamer)componentResourceManager.GetObject("imageList1.ImageStream"));
			imageList1.set_TransparentColor(Color.Fuchsia);
			imageList1.get_Images().SetKeyName(0, "BuilderDialog_AddAll.bmp");
			((Control)label150).set_AutoSize(true);
			((Control)label150).set_BackColor(SystemColors.Control);
			((Control)label150).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label150).set_ForeColor(Color.DarkBlue);
			((Control)label150).set_Location(new Point(753, 586));
			((Control)label150).set_Name("label150");
			((Control)label150).set_Size(new Size(32, 13));
			((Control)label150).set_TabIndex(136);
			((Control)label150).set_Text("8MB");
			((Control)label154).set_AutoSize(true);
			((Control)label154).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label154).set_Location(new Point(301, 579));
			((Control)label154).set_Name("label154");
			((Control)label154).set_Size(new Size(424, 26));
			((Control)label154).set_TabIndex(135);
			((Control)label154).set_Text("The Gaia catalogue to mag 9.0v, used for Asteroid Occultations. [ This is a subset of #40 ]\r\nDiameters using the DR3 solar radii values are included");
			((Control)label155).set_AutoSize(true);
			((Control)label155).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label155).set_Location(new Point(138, 586));
			((Control)label155).set_Name("label155");
			((Control)label155).set_Size(new Size(150, 13));
			((Control)label155).set_TabIndex(134);
			((Control)label155).set_Text("43 Gaia9_DR3 catalogue");
			((Control)cmdDownloadGaia9).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDownloadGaia9).set_Location(new Point(72, 581));
			((Control)cmdDownloadGaia9).set_Name("cmdDownloadGaia9");
			((Control)cmdDownloadGaia9).set_Size(new Size(64, 22));
			((Control)cmdDownloadGaia9).set_TabIndex(133);
			((Control)cmdDownloadGaia9).set_Text("Download");
			((ButtonBase)cmdDownloadGaia9).set_UseVisualStyleBackColor(true);
			((Control)cmdDownloadGaia9).add_Click((EventHandler)cmdDownloadGaia9_Click);
			((Control)lblGaiaDoubles).set_AutoSize(true);
			((Control)lblGaiaDoubles).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblGaiaDoubles).set_Location(new Point(8, 726));
			((Control)lblGaiaDoubles).set_Name("lblGaiaDoubles");
			((Control)lblGaiaDoubles).set_Size(new Size(36, 12));
			((Control)lblGaiaDoubles).set_TabIndex(132);
			((Control)lblGaiaDoubles).set_Text("-- --- ----");
			((Control)label51).set_AutoSize(true);
			((Control)label51).set_BackColor(SystemColors.Control);
			((Control)label51).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label51).set_ForeColor(Color.DarkBlue);
			((Control)label51).set_Location(new Point(746, 723));
			((Control)label51).set_Name("label51");
			((Control)label51).set_Size(new Size(39, 13));
			((Control)label51).set_TabIndex(131);
			((Control)label51).set_Text("27MB");
			((Control)label114).set_AutoSize(true);
			((Control)label114).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label114).set_Location(new Point(304, 726));
			((Control)label114).set_Name("label114");
			((Control)label114).set_Size(new Size(163, 13));
			((Control)label114).set_TabIndex(130);
			((Control)label114).set_Text("Gaia double star solutions in DR3");
			((Control)label122).set_AutoSize(true);
			((Control)label122).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label122).set_Location(new Point(141, 726));
			((Control)label122).set_Name("label122");
			((Control)label122).set_Size(new Size(124, 13));
			((Control)label122).set_TabIndex(129);
			((Control)label122).set_Text("47 Gaia double stars");
			((Control)cmdGaiaDoubles).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdGaiaDoubles).set_Location(new Point(75, 721));
			((Control)cmdGaiaDoubles).set_Name("cmdGaiaDoubles");
			((Control)cmdGaiaDoubles).set_Size(new Size(64, 22));
			((Control)cmdGaiaDoubles).set_TabIndex(128);
			((Control)cmdGaiaDoubles).set_Text("Download");
			((ButtonBase)cmdGaiaDoubles).set_UseVisualStyleBackColor(true);
			((Control)cmdGaiaDoubles).add_Click((EventHandler)cmdGaiaDoubles_Click);
			((Control)label113).set_AutoSize(true);
			((Control)label113).set_BackColor(Color.Yellow);
			label113.set_BorderStyle((BorderStyle)1);
			((Control)label113).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label113).set_ForeColor(Color.Red);
			((Control)label113).set_Location(new Point(577, 686));
			((Control)label113).set_Name("label113");
			((Control)label113).set_Size(new Size(95, 26));
			((Control)label113).set_TabIndex(127);
			((Control)label113).set_Text("This download is \r\nlarge. ~ 0.45 GB. ");
			((Control)lblEarth2014).set_AutoSize(true);
			((Control)lblEarth2014).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblEarth2014).set_Location(new Point(8, 691));
			((Control)lblEarth2014).set_Name("lblEarth2014");
			((Control)lblEarth2014).set_Size(new Size(36, 12));
			((Control)lblEarth2014).set_TabIndex(126);
			((Control)lblEarth2014).set_Text("-- --- ----");
			((Control)label88).set_AutoSize(true);
			((Control)label88).set_BackColor(SystemColors.Control);
			((Control)label88).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label88).set_ForeColor(Color.DarkBlue);
			((Control)label88).set_Location(new Point(739, 691));
			((Control)label88).set_Name("label88");
			((Control)label88).set_Size(new Size(46, 13));
			((Control)label88).set_TabIndex(125);
			((Control)label88).set_Text("455MB");
			((Control)label111).set_AutoSize(true);
			((Control)label111).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label111).set_Location(new Point(304, 691));
			((Control)label111).set_Name("label111");
			((Control)label111).set_Size(new Size(267, 13));
			((Control)label111).set_TabIndex(124);
			((Control)label111).set_Text("Earth2014 1 arcmin global topography and relief models");
			((Control)label112).set_AutoSize(true);
			((Control)label112).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label112).set_Location(new Point(141, 691));
			((Control)label112).set_Name("label112");
			((Control)label112).set_Size(new Size(87, 13));
			((Control)label112).set_TabIndex(123);
			((Control)label112).set_Text("46 Earth 2014");
			((Control)cmdEarth2014).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdEarth2014).set_Location(new Point(75, 686));
			((Control)cmdEarth2014).set_Name("cmdEarth2014");
			((Control)cmdEarth2014).set_Size(new Size(64, 22));
			((Control)cmdEarth2014).set_TabIndex(122);
			((Control)cmdEarth2014).set_Text("Download");
			((ButtonBase)cmdEarth2014).set_UseVisualStyleBackColor(true);
			((Control)cmdEarth2014).add_Click((EventHandler)cmdEarth2014_Click);
			((Control)lblUBSC).set_AutoSize(true);
			((Control)lblUBSC).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblUBSC).set_Location(new Point(7, 655));
			((Control)lblUBSC).set_Name("lblUBSC");
			((Control)lblUBSC).set_Size(new Size(36, 12));
			((Control)lblUBSC).set_TabIndex(121);
			((Control)lblUBSC).set_Text("-- --- ----");
			((Control)label52).set_AutoSize(true);
			((Control)label52).set_BackColor(SystemColors.Control);
			((Control)label52).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label52).set_ForeColor(Color.DarkBlue);
			((Control)label52).set_Location(new Point(735, 655));
			((Control)label52).set_Name("label52");
			((Control)label52).set_Size(new Size(50, 13));
			((Control)label52).set_TabIndex(120);
			((Control)label52).set_Text("0.02MB");
			((Control)label53).set_AutoSize(true);
			((Control)label53).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label53).set_Location(new Point(303, 655));
			((Control)label53).set_Name("label53");
			((Control)label53).set_Size(new Size(191, 13));
			((Control)label53).set_TabIndex(119);
			((Control)label53).set_Text("USNO Bright-Star Astrometric Database");
			((Control)label87).set_AutoSize(true);
			((Control)label87).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label87).set_Location(new Point(140, 655));
			((Control)label87).set_Name("label87");
			((Control)label87).set_Size(new Size(118, 13));
			((Control)label87).set_TabIndex(118);
			((Control)label87).set_Text("45 UBSC catalogue");
			((Control)cmdUBSC).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdUBSC).set_Location(new Point(74, 650));
			((Control)cmdUBSC).set_Name("cmdUBSC");
			((Control)cmdUBSC).set_Size(new Size(64, 22));
			((Control)cmdUBSC).set_TabIndex(117);
			((Control)cmdUBSC).set_Text("Download");
			((ButtonBase)cmdUBSC).set_UseVisualStyleBackColor(true);
			((Control)cmdUBSC).add_Click((EventHandler)cmdUBSC_Click);
			((ButtonBase)DownloadFlagGaia14).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownloadFlagGaia14).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownloadFlagGaia14).set_ImageIndex(0);
			((ButtonBase)DownloadFlagGaia14).set_ImageList(imageList1);
			((Control)DownloadFlagGaia14).set_Location(new Point(59, 512));
			((Control)DownloadFlagGaia14).set_Name("DownloadFlagGaia14");
			((Control)DownloadFlagGaia14).set_Size(new Size(14, 18));
			((Control)DownloadFlagGaia14).set_TabIndex(115);
			((ButtonBase)DownloadFlagGaia14).set_UseVisualStyleBackColor(true);
			((Control)DownloadFlagGaia14).set_Visible(false);
			((Control)label146).set_AutoSize(true);
			((Control)label146).set_BackColor(Color.Yellow);
			label146.set_BorderStyle((BorderStyle)1);
			((Control)label146).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label146).set_ForeColor(Color.Red);
			((Control)label146).set_Location(new Point(642, 508));
			((Control)label146).set_Name("label146");
			((Control)label146).set_Size(new Size(95, 26));
			((Control)label146).set_TabIndex(114);
			((Control)label146).set_Text("This download is \r\nlarge. ~ 0.61 GB. ");
			((Control)lblGaia14).set_AutoSize(true);
			((Control)lblGaia14).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblGaia14).set_Location(new Point(6, 515));
			((Control)lblGaia14).set_Name("lblGaia14");
			((Control)lblGaia14).set_Size(new Size(36, 12));
			((Control)lblGaia14).set_TabIndex(113);
			((Control)lblGaia14).set_Text("-- --- ----");
			((Control)label151).set_AutoSize(true);
			((Control)label151).set_BackColor(SystemColors.Control);
			((Control)label151).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label151).set_ForeColor(Color.DarkBlue);
			((Control)label151).set_Location(new Point(740, 515));
			((Control)label151).set_Name("label151");
			((Control)label151).set_Size(new Size(46, 13));
			((Control)label151).set_TabIndex(112);
			((Control)label151).set_Text("610MB");
			((Control)label152).set_AutoSize(true);
			((Control)label152).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label152).set_Location(new Point(302, 508));
			((Control)label152).set_Name("label152");
			((Control)label152).set_Size(new Size(342, 26));
			((Control)label152).set_TabIndex(111);
			((Control)label152).set_Text("The Gaia catalogue to mag 14.0v, used for Asteroid Occultations. [This\r\n is a subset of #40 ] Diameters using the DR3 solar radii values included");
			((Control)label153).set_AutoSize(true);
			((Control)label153).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label153).set_Location(new Point(139, 515));
			((Control)label153).set_Name("label153");
			((Control)label153).set_Size(new Size(157, 13));
			((Control)label153).set_TabIndex(110);
			((Control)label153).set_Text("41 Gaia14_DR3 catalogue");
			((Control)cmdDownloadGaia14).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDownloadGaia14).set_Location(new Point(73, 510));
			((Control)cmdDownloadGaia14).set_Name("cmdDownloadGaia14");
			((Control)cmdDownloadGaia14).set_Size(new Size(64, 22));
			((Control)cmdDownloadGaia14).set_TabIndex(109);
			((Control)cmdDownloadGaia14).set_Text("Download");
			((ButtonBase)cmdDownloadGaia14).set_UseVisualStyleBackColor(true);
			((Control)cmdDownloadGaia14).add_Click((EventHandler)cmdDownloadGaia14_Click);
			((ButtonBase)DownloadFlagGaia16).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownloadFlagGaia16).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownloadFlagGaia16).set_ImageIndex(0);
			((ButtonBase)DownloadFlagGaia16).set_ImageList(imageList1);
			((Control)DownloadFlagGaia16).set_Location(new Point(59, 475));
			((Control)DownloadFlagGaia16).set_Name("DownloadFlagGaia16");
			((Control)DownloadFlagGaia16).set_Size(new Size(14, 18));
			((Control)DownloadFlagGaia16).set_TabIndex(108);
			((ButtonBase)DownloadFlagGaia16).set_UseVisualStyleBackColor(true);
			((Control)DownloadFlagGaia16).set_Visible(false);
			((Control)lblSAO).set_AutoSize(true);
			((Control)lblSAO).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblSAO).set_Location(new Point(7, 618));
			((Control)lblSAO).set_Name("lblSAO");
			((Control)lblSAO).set_Size(new Size(36, 12));
			((Control)lblSAO).set_TabIndex(107);
			((Control)lblSAO).set_Text("-- --- ----");
			((Control)label138).set_AutoSize(true);
			((Control)label138).set_BackColor(SystemColors.Control);
			((Control)label138).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label138).set_ForeColor(Color.DarkBlue);
			((Control)label138).set_Location(new Point(753, 618));
			((Control)label138).set_Name("label138");
			((Control)label138).set_Size(new Size(32, 13));
			((Control)label138).set_TabIndex(106);
			((Control)label138).set_Text("2MB");
			((Control)label139).set_AutoSize(true);
			((Control)label139).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label139).set_Location(new Point(303, 618));
			((Control)label139).set_Name("label139");
			((Control)label139).set_Size(new Size(363, 13));
			((Control)label139).set_TabIndex(105);
			((Control)label139).set_Text("The SAO catalogue - used to provide SAO numbers in asteroid pre-point lists");
			((Control)label140).set_AutoSize(true);
			((Control)label140).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label140).set_Location(new Point(140, 618));
			((Control)label140).set_Name("label140");
			((Control)label140).set_Size(new Size(110, 13));
			((Control)label140).set_TabIndex(104);
			((Control)label140).set_Text("44 SAO catalogue");
			((Control)cmdSAO).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSAO).set_Location(new Point(74, 613));
			((Control)cmdSAO).set_Name("cmdSAO");
			((Control)cmdSAO).set_Size(new Size(64, 22));
			((Control)cmdSAO).set_TabIndex(103);
			((Control)cmdSAO).set_Text("Download");
			((ButtonBase)cmdSAO).set_UseVisualStyleBackColor(true);
			((Control)cmdSAO).add_Click((EventHandler)cmdSAO_Click);
			((Control)label136).set_AutoSize(true);
			((Control)label136).set_BackColor(Color.Yellow);
			label136.set_BorderStyle((BorderStyle)1);
			((Control)label136).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label136).set_ForeColor(Color.Red);
			((Control)label136).set_Location(new Point(611, 471));
			((Control)label136).set_Name("label136");
			((Control)label136).set_Size(new Size(120, 26));
			((Control)label136).set_TabIndex(101);
			((Control)label136).set_Text("This download is very \r\nlarge. ~ 2.64 GB. ");
			((Control)lblGaia16).set_AutoSize(true);
			((Control)lblGaia16).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblGaia16).set_Location(new Point(6, 478));
			((Control)lblGaia16).set_Name("lblGaia16");
			((Control)lblGaia16).set_Size(new Size(36, 12));
			((Control)lblGaia16).set_TabIndex(99);
			((Control)lblGaia16).set_Text("-- --- ----");
			((Control)lblGaia12).set_AutoSize(true);
			((Control)lblGaia12).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblGaia12).set_Location(new Point(6, 552));
			((Control)lblGaia12).set_Name("lblGaia12");
			((Control)lblGaia12).set_Size(new Size(36, 12));
			((Control)lblGaia12).set_TabIndex(97);
			((Control)lblGaia12).set_Text("-- --- ----");
			((ButtonBase)DownloadFlagGaia12).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownloadFlagGaia12).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownloadFlagGaia12).set_ImageIndex(0);
			((ButtonBase)DownloadFlagGaia12).set_ImageList(imageList1);
			((Control)DownloadFlagGaia12).set_Location(new Point(60, 547));
			((Control)DownloadFlagGaia12).set_Name("DownloadFlagGaia12");
			((Control)DownloadFlagGaia12).set_Size(new Size(14, 18));
			((Control)DownloadFlagGaia12).set_TabIndex(98);
			((ButtonBase)DownloadFlagGaia12).set_UseVisualStyleBackColor(true);
			((Control)DownloadFlagGaia12).set_Visible(false);
			((Control)label131).set_AutoSize(true);
			((Control)label131).set_BackColor(SystemColors.Control);
			((Control)label131).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label131).set_ForeColor(Color.DarkBlue);
			((Control)label131).set_Location(new Point(737, 478));
			((Control)label131).set_Name("label131");
			((Control)label131).set_Size(new Size(49, 13));
			((Control)label131).set_TabIndex(96);
			((Control)label131).set_Text("2.64GB");
			((Control)label132).set_AutoSize(true);
			((Control)label132).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label132).set_Location(new Point(302, 471));
			((Control)label132).set_Name("label132");
			((Control)label132).set_Size(new Size(311, 26));
			((Control)label132).set_TabIndex(95);
			((Control)label132).set_Text("The Gaia catalogue to mag 16.0v, used for Asteroid Occultations.\r\nDiameters using the DR3 solar radii values are included");
			((Control)label133).set_AutoSize(true);
			((Control)label133).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label133).set_Location(new Point(139, 478));
			((Control)label133).set_Name("label133");
			((Control)label133).set_Size(new Size(157, 13));
			((Control)label133).set_TabIndex(94);
			((Control)label133).set_Text("40 Gaia16_DR3 catalogue");
			((Control)cmdDownloadGaia16).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDownloadGaia16).set_Location(new Point(73, 473));
			((Control)cmdDownloadGaia16).set_Name("cmdDownloadGaia16");
			((Control)cmdDownloadGaia16).set_Size(new Size(64, 22));
			((Control)cmdDownloadGaia16).set_TabIndex(93);
			((Control)cmdDownloadGaia16).set_Text("Download");
			((ButtonBase)cmdDownloadGaia16).set_UseVisualStyleBackColor(true);
			((Control)cmdDownloadGaia16).add_Click((EventHandler)cmdDownloadGaia16_Click);
			((Control)label72).set_AutoSize(true);
			((Control)label72).set_BackColor(SystemColors.Control);
			((Control)label72).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label72).set_ForeColor(Color.DarkBlue);
			((Control)label72).set_Location(new Point(739, 552));
			((Control)label72).set_Name("label72");
			((Control)label72).set_Size(new Size(46, 13));
			((Control)label72).set_TabIndex(92);
			((Control)label72).set_Text("125MB");
			((Control)label129).set_AutoSize(true);
			((Control)label129).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label129).set_Location(new Point(302, 545));
			((Control)label129).set_Name("label129");
			((Control)label129).set_Size(new Size(430, 26));
			((Control)label129).set_TabIndex(91);
			((Control)label129).set_Text("The Gaia catalogue to mag 12.0v, used for Asteroid Occultations. [ This is a subset of #40 ]\r\nDiameters using the DR3 solar radii values are included");
			((Control)label130).set_AutoSize(true);
			((Control)label130).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label130).set_Location(new Point(139, 552));
			((Control)label130).set_Name("label130");
			((Control)label130).set_Size(new Size(157, 13));
			((Control)label130).set_TabIndex(90);
			((Control)label130).set_Text("42 Gaia12_DR3 catalogue");
			((Control)cmdDownloadGaia12).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDownloadGaia12).set_Location(new Point(73, 547));
			((Control)cmdDownloadGaia12).set_Name("cmdDownloadGaia12");
			((Control)cmdDownloadGaia12).set_Size(new Size(64, 22));
			((Control)cmdDownloadGaia12).set_TabIndex(89);
			((Control)cmdDownloadGaia12).set_Text("Download");
			((ButtonBase)cmdDownloadGaia12).set_UseVisualStyleBackColor(true);
			((Control)cmdDownloadGaia12).add_Click((EventHandler)cmdDownloadGaia12_Click);
			((Control)label128).set_AutoSize(true);
			((Control)label128).set_BackColor(SystemColors.Control);
			((Control)label128).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label128).set_ForeColor(Color.DarkBlue);
			((Control)label128).set_Location(new Point(737, 293));
			((Control)label128).set_Name("label128");
			((Control)label128).set_Size(new Size(49, 13));
			((Control)label128).set_TabIndex(88);
			((Control)label128).set_Text("2.55GB");
			((Control)label121).set_AutoSize(true);
			((Control)label121).set_BackColor(SystemColors.Control);
			((Control)label121).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label121).set_ForeColor(Color.DarkBlue);
			((Control)label121).set_Location(new Point(746, 764));
			((Control)label121).set_Name("label121");
			((Control)label121).set_Size(new Size(39, 13));
			((Control)label121).set_TabIndex(86);
			((Control)label121).set_Text("38MB");
			((Control)label32).set_AutoSize(true);
			((Control)label32).set_BackColor(Color.Transparent);
			label32.set_BorderStyle((BorderStyle)1);
			((Control)label32).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label32).set_ForeColor(SystemColors.ControlText);
			((Control)label32).set_Location(new Point(138, 763));
			((Control)label32).set_Name("label32");
			((Control)label32).set_Size(new Size(145, 15));
			((Control)label32).set_TabIndex(84);
			((Control)label32).set_Text("00 Installation data files");
			((Control)label74).set_AutoSize(true);
			((Control)label74).set_BackColor(Color.LightGreen);
			label74.set_BorderStyle((BorderStyle)1);
			((Control)label74).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label74).set_ForeColor(SystemColors.ControlText);
			((Control)label74).set_Location(new Point(303, 756));
			((Control)label74).set_Name("label74");
			((Control)label74).set_Size(new Size(424, 28));
			((Control)label74).set_TabIndex(85);
			((Control)label74).set_Text("2 data files (zipped) required for a new installation of Occult. Downloading is not normally \r\nrequired or appropriate.");
			((Control)cmdDownloadInstallationFiles).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDownloadInstallationFiles).set_Location(new Point(73, 754));
			((Control)cmdDownloadInstallationFiles).set_Name("cmdDownloadInstallationFiles");
			((Control)cmdDownloadInstallationFiles).set_Size(new Size(64, 32));
			((Control)cmdDownloadInstallationFiles).set_TabIndex(83);
			((Control)cmdDownloadInstallationFiles).set_Text("Download &&\r\nRe-install");
			((ButtonBase)cmdDownloadInstallationFiles).set_UseVisualStyleBackColor(true);
			((Control)cmdDownloadInstallationFiles).add_Click((EventHandler)cmdDownloadInstallationFiles_Click);
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label17).set_Location(new Point(304, 249));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(440, 26));
			((Control)label17).set_TabIndex(52);
			((Control)label17).set_Text("The JPL DE ephemeris is the basis of all accurate predictions. New versions of the ephemeris\r\nare released every year or two. Update only when advised there is a new version.");
			((Control)lblLola).set_AutoSize(true);
			((Control)lblLola).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblLola).set_Location(new Point(689, 116));
			((Control)lblLola).set_Name("lblLola");
			((Control)lblLola).set_Size(new Size(14, 12));
			((Control)lblLola).set_TabIndex(82);
			((Control)lblLola).set_Text("...");
			((Control)cmdCreateLOLA128).set_Enabled(false);
			((Control)cmdCreateLOLA128).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdCreateLOLA128).set_Location(new Point(566, 112));
			((Control)cmdCreateLOLA128).set_Name("cmdCreateLOLA128");
			((Control)cmdCreateLOLA128).set_Size(new Size(120, 20));
			((Control)cmdCreateLOLA128).set_TabIndex(81);
			((Control)cmdCreateLOLA128).set_Text("Convert LDEM  .img files");
			((ButtonBase)cmdCreateLOLA128).set_UseVisualStyleBackColor(true);
			((Control)cmdCreateLOLA128).add_Click((EventHandler)cmdCreateLOLA128_Click);
			((Control)lblLOLA128File).set_AutoSize(true);
			((Control)lblLOLA128File).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblLOLA128File).set_Location(new Point(7, 104));
			((Control)lblLOLA128File).set_Name("lblLOLA128File");
			((Control)lblLOLA128File).set_Size(new Size(36, 12));
			((Control)lblLOLA128File).set_TabIndex(12);
			((Control)lblLOLA128File).set_Text("-- --- ----");
			((Control)label56).set_AutoSize(true);
			((Control)label56).set_BackColor(SystemColors.Control);
			((Control)label56).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label56).set_ForeColor(Color.DarkBlue);
			((Control)label56).set_Location(new Point(740, 104));
			((Control)label56).set_Name("label56");
			((Control)label56).set_Size(new Size(46, 13));
			((Control)label56).set_TabIndex(17);
			((Control)label56).set_Text("111MB");
			((ButtonBase)DownloadFlagLOLA128).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownloadFlagLOLA128).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownloadFlagLOLA128).set_ImageIndex(0);
			((ButtonBase)DownloadFlagLOLA128).set_ImageList(imageList1);
			((Control)DownloadFlagLOLA128).set_Location(new Point(61, 101));
			((Control)DownloadFlagLOLA128).set_Name("DownloadFlagLOLA128");
			((Control)DownloadFlagLOLA128).set_Size(new Size(14, 18));
			((Control)DownloadFlagLOLA128).set_TabIndex(13);
			((ButtonBase)DownloadFlagLOLA128).set_UseVisualStyleBackColor(true);
			((Control)DownloadFlagLOLA128).set_Visible(false);
			((Control)label60).set_AutoSize(true);
			((Control)label60).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label60).set_Location(new Point(304, 97));
			((Control)label60).set_Name("label60");
			((Control)label60).set_Size(new Size(440, 26));
			((Control)label60).set_TabIndex(16);
			((Control)label60).set_Text("Altitude readings from the Lunar Reconnaisance Orbiter - Lunar Orbiter Laser Altimeter.  Used\r\nto generate detailed lunar limb profiles for occultations.");
			((Control)label65).set_AutoSize(true);
			((Control)label65).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label65).set_Location(new Point(140, 104));
			((Control)label65).set_Name("label65");
			((Control)label65).set_Size(new Size(114, 13));
			((Control)label65).set_TabIndex(15);
			((Control)label65).set_Text("29 LOLA lunar limb");
			((Control)cmdLOLA).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdLOLA).set_Location(new Point(74, 99));
			((Control)cmdLOLA).set_Name("cmdLOLA");
			((Control)cmdLOLA).set_Size(new Size(64, 22));
			((Control)cmdLOLA).set_TabIndex(14);
			((Control)cmdLOLA).set_Text("Download");
			((ButtonBase)cmdLOLA).set_UseVisualStyleBackColor(true);
			((Control)cmdLOLA).add_Click((EventHandler)cmdLOLA_Click);
			((Control)lblKepler2File).set_AutoSize(true);
			((Control)lblKepler2File).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblKepler2File).set_Location(new Point(7, 219));
			((Control)lblKepler2File).set_Name("lblKepler2File");
			((Control)lblKepler2File).set_Size(new Size(36, 12));
			((Control)lblKepler2File).set_TabIndex(42);
			((Control)lblKepler2File).set_Text("-- --- ----");
			((Control)label46).set_AutoSize(true);
			((Control)label46).set_BackColor(SystemColors.Control);
			((Control)label46).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label46).set_ForeColor(Color.DarkBlue);
			((Control)label46).set_Location(new Point(754, 219));
			((Control)label46).set_Name("label46");
			((Control)label46).set_Size(new Size(32, 13));
			((Control)label46).set_TabIndex(47);
			((Control)label46).set_Text("4MB");
			((ButtonBase)DownloadFlagKepler2).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownloadFlagKepler2).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownloadFlagKepler2).set_ImageIndex(0);
			((ButtonBase)DownloadFlagKepler2).set_ImageList(imageList1);
			((Control)DownloadFlagKepler2).set_Location(new Point(61, 214));
			((Control)DownloadFlagKepler2).set_Name("DownloadFlagKepler2");
			((Control)DownloadFlagKepler2).set_Size(new Size(14, 18));
			((Control)DownloadFlagKepler2).set_TabIndex(43);
			((ButtonBase)DownloadFlagKepler2).set_UseVisualStyleBackColor(true);
			((Control)DownloadFlagKepler2).set_Visible(false);
			((Control)label47).set_AutoSize(true);
			((Control)label47).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label47).set_Location(new Point(304, 219));
			((Control)label47).set_Name("label47");
			((Control)label47).set_Size(new Size(199, 13));
			((Control)label47).set_TabIndex(46);
			((Control)label47).set_Text("List of target stars for the Kepler2 mission");
			((Control)label54).set_AutoSize(true);
			((Control)label54).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label54).set_Location(new Point(141, 219));
			((Control)label54).set_Name("label54");
			((Control)label54).set_Size(new Size(136, 13));
			((Control)label54).set_TabIndex(45);
			((Control)label54).set_Text("32 Kepler2 target stars");
			((Control)cmdDownloadKepler2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDownloadKepler2).set_Location(new Point(74, 214));
			((Control)cmdDownloadKepler2).set_Name("cmdDownloadKepler2");
			((Control)cmdDownloadKepler2).set_Size(new Size(64, 22));
			((Control)cmdDownloadKepler2).set_TabIndex(44);
			((Control)cmdDownloadKepler2).set_Text("Download");
			((ButtonBase)cmdDownloadKepler2).set_UseVisualStyleBackColor(true);
			((Control)cmdDownloadKepler2).add_Click((EventHandler)cmdDownloadKepler2_Click);
			((Control)lblGoogleMapTemplatesFile).set_AutoSize(true);
			((Control)lblGoogleMapTemplatesFile).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblGoogleMapTemplatesFile).set_Location(new Point(7, 441));
			((Control)lblGoogleMapTemplatesFile).set_Name("lblGoogleMapTemplatesFile");
			((Control)lblGoogleMapTemplatesFile).set_Size(new Size(36, 12));
			((Control)lblGoogleMapTemplatesFile).set_TabIndex(76);
			((Control)lblGoogleMapTemplatesFile).set_Text("-- --- ----");
			((Control)lblPhoebeFile).set_AutoSize(true);
			((Control)lblPhoebeFile).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblPhoebeFile).set_Location(new Point(7, 404));
			((Control)lblPhoebeFile).set_Name("lblPhoebeFile");
			((Control)lblPhoebeFile).set_Size(new Size(36, 12));
			((Control)lblPhoebeFile).set_TabIndex(70);
			((Control)lblPhoebeFile).set_Text("-- --- ----");
			((Control)lblEOP_OldFile).set_AutoSize(true);
			((Control)lblEOP_OldFile).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblEOP_OldFile).set_Location(new Point(7, 330));
			((Control)lblEOP_OldFile).set_Name("lblEOP_OldFile");
			((Control)lblEOP_OldFile).set_Size(new Size(36, 12));
			((Control)lblEOP_OldFile).set_TabIndex(59);
			((Control)lblEOP_OldFile).set_Text("-- --- ----");
			((Control)lblCraterFile).set_AutoSize(true);
			((Control)lblCraterFile).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblCraterFile).set_Location(new Point(7, 367));
			((Control)lblCraterFile).set_Name("lblCraterFile");
			((Control)lblCraterFile).set_Size(new Size(36, 12));
			((Control)lblCraterFile).set_TabIndex(64);
			((Control)lblCraterFile).set_Text("-- --- ----");
			((Control)lblJPL_DEfile).set_AutoSize(true);
			((Control)lblJPL_DEfile).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblJPL_DEfile).set_Location(new Point(7, 256));
			((Control)lblJPL_DEfile).set_Name("lblJPL_DEfile");
			((Control)lblJPL_DEfile).set_Size(new Size(36, 12));
			((Control)lblJPL_DEfile).set_TabIndex(48);
			((Control)lblJPL_DEfile).set_Text("-- --- ----");
			((Control)lblDE6000file).set_AutoSize(true);
			((Control)lblDE6000file).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblDE6000file).set_Location(new Point(7, 293));
			((Control)lblDE6000file).set_Name("lblDE6000file");
			((Control)lblDE6000file).set_Size(new Size(36, 12));
			((Control)lblDE6000file).set_TabIndex(54);
			((Control)lblDE6000file).set_Text("-- --- ----");
			((Control)lblTycho2File).set_AutoSize(true);
			((Control)lblTycho2File).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblTycho2File).set_Location(new Point(7, 182));
			((Control)lblTycho2File).set_Name("lblTycho2File");
			((Control)lblTycho2File).set_Size(new Size(36, 12));
			((Control)lblTycho2File).set_TabIndex(36);
			((Control)lblTycho2File).set_Text("-- --- ----");
			((Control)lblStarDiaFile).set_AutoSize(true);
			((Control)lblStarDiaFile).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblStarDiaFile).set_Location(new Point(7, 145));
			((Control)lblStarDiaFile).set_Name("lblStarDiaFile");
			((Control)lblStarDiaFile).set_Size(new Size(36, 12));
			((Control)lblStarDiaFile).set_TabIndex(30);
			((Control)lblStarDiaFile).set_Text("-- --- ----");
			((Control)lblXZcatFile).set_AutoSize(true);
			((Control)lblXZcatFile).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblXZcatFile).set_Location(new Point(7, 67));
			((Control)lblXZcatFile).set_Name("lblXZcatFile");
			((Control)lblXZcatFile).set_Size(new Size(36, 12));
			((Control)lblXZcatFile).set_TabIndex(6);
			((Control)lblXZcatFile).set_Text("-- --- ----");
			((Control)lblLunarOldFiles).set_AutoSize(true);
			((Control)lblLunarOldFiles).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblLunarOldFiles).set_Location(new Point(7, 30));
			((Control)lblLunarOldFiles).set_Name("lblLunarOldFiles");
			((Control)lblLunarOldFiles).set_Size(new Size(36, 12));
			((Control)lblLunarOldFiles).set_TabIndex(0);
			((Control)lblLunarOldFiles).set_Text("-- --- ----");
			((Control)label106).set_AutoSize(true);
			((Control)label106).set_BackColor(SystemColors.Control);
			((Control)label106).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label106).set_ForeColor(Color.DarkBlue);
			((Control)label106).set_Location(new Point(751, 441));
			((Control)label106).set_Name("label106");
			((Control)label106).set_Size(new Size(35, 13));
			((Control)label106).set_TabIndex(80);
			((Control)label106).set_Text("10kB");
			((Control)label109).set_AutoSize(true);
			((Control)label109).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label109).set_Location(new Point(139, 441));
			((Control)label109).set_Name("label109");
			((Control)label109).set_Size(new Size(147, 13));
			((Control)label109).set_TabIndex(78);
			((Control)label109).set_Text("39 GoogleMap templates");
			((Control)label110).set_AutoSize(true);
			((Control)label110).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label110).set_Location(new Point(304, 434));
			((Control)label110).set_Name("label110");
			((Control)label110).set_Size(new Size(435, 26));
			((Control)label110).set_TabIndex(79);
			((Control)label110).set_Text("For those who create GoogleMap html files of predictions to mount on web pages. This \r\ndownloads the current header/footer templates, and the current JavaScript file for the server");
			((Control)cmdDownloadGoogleMapFiles).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDownloadGoogleMapFiles).set_Location(new Point(74, 436));
			((Control)cmdDownloadGoogleMapFiles).set_Name("cmdDownloadGoogleMapFiles");
			((Control)cmdDownloadGoogleMapFiles).set_Size(new Size(64, 22));
			((Control)cmdDownloadGoogleMapFiles).set_TabIndex(77);
			((Control)cmdDownloadGoogleMapFiles).set_Text("Download");
			((ButtonBase)cmdDownloadGoogleMapFiles).set_UseVisualStyleBackColor(true);
			((Control)cmdDownloadGoogleMapFiles).add_Click((EventHandler)cmdDownloadGoogleMapFiles_Click);
			((Control)label89).set_AutoSize(true);
			((Control)label89).set_BackColor(SystemColors.Control);
			((Control)label89).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label89).set_ForeColor(Color.DarkBlue);
			((Control)label89).set_Location(new Point(754, 67));
			((Control)label89).set_Name("label89");
			((Control)label89).set_Size(new Size(32, 13));
			((Control)label89).set_TabIndex(11);
			((Control)label89).set_Text("6MB");
			((Control)label86).set_AutoSize(true);
			((Control)label86).set_BackColor(SystemColors.Control);
			((Control)label86).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label86).set_ForeColor(Color.DarkBlue);
			((Control)label86).set_Location(new Point(751, 145));
			((Control)label86).set_Name("label86");
			((Control)label86).set_Size(new Size(35, 13));
			((Control)label86).set_TabIndex(35);
			((Control)label86).set_Text("60kB");
			((Control)label85).set_AutoSize(true);
			((Control)label85).set_BackColor(SystemColors.Control);
			((Control)label85).set_Enabled(false);
			((Control)label85).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label85).set_ForeColor(Color.DarkBlue);
			((Control)label85).set_Location(new Point(747, 182));
			((Control)label85).set_Name("label85");
			((Control)label85).set_Size(new Size(39, 13));
			((Control)label85).set_TabIndex(41);
			((Control)label85).set_Text("64MB");
			((Control)label84).set_AutoSize(true);
			((Control)label84).set_BackColor(SystemColors.Control);
			((Control)label84).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label84).set_ForeColor(Color.DarkBlue);
			((Control)label84).set_Location(new Point(747, 256));
			((Control)label84).set_Name("label84");
			((Control)label84).set_Size(new Size(39, 13));
			((Control)label84).set_TabIndex(53);
			((Control)label84).set_Text("96MB");
			((Control)label83).set_AutoSize(true);
			((Control)label83).set_BackColor(SystemColors.Control);
			((Control)label83).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label83).set_ForeColor(Color.DarkBlue);
			((Control)label83).set_Location(new Point(743, 329));
			((Control)label83).set_Name("label83");
			((Control)label83).set_Size(new Size(43, 13));
			((Control)label83).set_TabIndex(63);
			((Control)label83).set_Text("0.8MB");
			((Control)label82).set_AutoSize(true);
			((Control)label82).set_BackColor(SystemColors.Control);
			((Control)label82).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label82).set_ForeColor(Color.DarkBlue);
			((Control)label82).set_Location(new Point(751, 368));
			((Control)label82).set_Name("label82");
			((Control)label82).set_Size(new Size(35, 13));
			((Control)label82).set_TabIndex(69);
			((Control)label82).set_Text("13kB");
			((Control)label81).set_AutoSize(true);
			((Control)label81).set_BackColor(SystemColors.Control);
			((Control)label81).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label81).set_ForeColor(Color.DarkBlue);
			((Control)label81).set_Location(new Point(744, 404));
			((Control)label81).set_Name("label81");
			((Control)label81).set_Size(new Size(42, 13));
			((Control)label81).set_TabIndex(75);
			((Control)label81).set_Text("130kB");
			((Control)label79).set_AutoSize(true);
			((Control)label79).set_BackColor(SystemColors.Control);
			((Control)label79).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label79).set_ForeColor(Color.DarkBlue);
			((Control)label79).set_Location(new Point(747, 30));
			((Control)label79).set_Name("label79");
			((Control)label79).set_Size(new Size(39, 13));
			((Control)label79).set_TabIndex(5);
			((Control)label79).set_Text("31MB");
			((ButtonBase)DownloadFlagPhoebe).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownloadFlagPhoebe).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownloadFlagPhoebe).set_ImageIndex(0);
			((ButtonBase)DownloadFlagPhoebe).set_ImageList(imageList1);
			((Control)DownloadFlagPhoebe).set_Location(new Point(61, 400));
			((Control)DownloadFlagPhoebe).set_Name("DownloadFlagPhoebe");
			((Control)DownloadFlagPhoebe).set_Size(new Size(14, 18));
			((Control)DownloadFlagPhoebe).set_TabIndex(71);
			((ButtonBase)DownloadFlagPhoebe).set_UseVisualStyleBackColor(true);
			((Control)DownloadFlagPhoebe).set_Visible(false);
			((Control)label71).set_AutoSize(true);
			((Control)label71).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label71).set_Location(new Point(139, 404));
			((Control)label71).set_Name("label71");
			((Control)label71).set_Size(new Size(129, 13));
			((Control)label71).set_TabIndex(73);
			((Control)label71).set_Text("37 Phoebe ephemeris");
			((Control)label78).set_AutoSize(true);
			((Control)label78).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label78).set_Location(new Point(304, 404));
			((Control)label78).set_Name("label78");
			((Control)label78).set_Size(new Size(440, 13));
			((Control)label78).set_TabIndex(74);
			((Control)label78).set_Text("Downloads a file containing an accurate ephemeris of Saturn IX - Phoebe - for 1875 to 2025.");
			((Control)cmdDownloadePhoebeEphemeris).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDownloadePhoebeEphemeris).set_Location(new Point(74, 399));
			((Control)cmdDownloadePhoebeEphemeris).set_Name("cmdDownloadePhoebeEphemeris");
			((Control)cmdDownloadePhoebeEphemeris).set_Size(new Size(64, 22));
			((Control)cmdDownloadePhoebeEphemeris).set_TabIndex(72);
			((Control)cmdDownloadePhoebeEphemeris).set_Text("Download");
			((ButtonBase)cmdDownloadePhoebeEphemeris).set_UseVisualStyleBackColor(true);
			((Control)cmdDownloadePhoebeEphemeris).add_Click((EventHandler)cmdDownloadePhoebeEphemeris_Click);
			((ButtonBase)DownLoadFlagCraterTimings).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownLoadFlagCraterTimings).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownLoadFlagCraterTimings).set_ImageIndex(0);
			((ButtonBase)DownLoadFlagCraterTimings).set_ImageList(imageList1);
			((Control)DownLoadFlagCraterTimings).set_Location(new Point(61, 363));
			((Control)DownLoadFlagCraterTimings).set_Name("DownLoadFlagCraterTimings");
			((Control)DownLoadFlagCraterTimings).set_Size(new Size(14, 18));
			((Control)DownLoadFlagCraterTimings).set_TabIndex(65);
			((ButtonBase)DownLoadFlagCraterTimings).set_UseVisualStyleBackColor(true);
			((Control)DownLoadFlagCraterTimings).set_Visible(false);
			((Control)label76).set_AutoSize(true);
			((Control)label76).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label76).set_Location(new Point(139, 367));
			((Control)label76).set_Name("label76");
			((Control)label76).set_Size(new Size(129, 13));
			((Control)label76).set_TabIndex(67);
			((Control)label76).set_Text("36 Crater coordinates");
			((Control)label77).set_AutoSize(true);
			((Control)label77).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label77).set_Location(new Point(304, 360));
			((Control)label77).set_Name("label77");
			((Control)label77).set_Size(new Size(441, 26));
			((Control)label77).set_TabIndex(68);
			((Control)label77).set_Text("Downloads two files. The first contains the coordinates of craters and other features for use\r\nin reducing lunar eclipse crater timings. The second is an updated file for drawing moon maps.");
			((Control)cmdDownloadCraterTimings).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDownloadCraterTimings).set_Location(new Point(74, 362));
			((Control)cmdDownloadCraterTimings).set_Name("cmdDownloadCraterTimings");
			((Control)cmdDownloadCraterTimings).set_Size(new Size(64, 22));
			((Control)cmdDownloadCraterTimings).set_TabIndex(66);
			((Control)cmdDownloadCraterTimings).set_Text("Download");
			((ButtonBase)cmdDownloadCraterTimings).set_UseVisualStyleBackColor(true);
			((Control)cmdDownloadCraterTimings).add_Click((EventHandler)cmdDownloadCraterTimings_Click);
			((Control)label70).set_AutoSize(true);
			((Control)label70).set_BackColor(Color.Yellow);
			label70.set_BorderStyle((BorderStyle)1);
			((Control)label70).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label70).set_ForeColor(Color.Red);
			((Control)label70).set_Location(new Point(451, 299));
			((Control)label70).set_Name("label70");
			((Control)label70).set_Size(new Size(202, 14));
			((Control)label70).set_TabIndex(58);
			((Control)label70).set_Text("This download is very large - 2.55 GB. ");
			((Control)label68).set_AutoSize(true);
			((Control)label68).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label68).set_Location(new Point(304, 286));
			((Control)label68).set_Name("label68");
			((Control)label68).set_Size(new Size(325, 26));
			((Control)label68).set_TabIndex(57);
			((Control)label68).set_Text("This is the JPL-DE441 ephemeris. It provides accurate ephemerides \r\nfor the period -13000 to +17000. ");
			((Control)label69).set_AutoSize(true);
			((Control)label69).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label69).set_Location(new Point(141, 286));
			((Control)label69).set_Name("label69");
			((Control)label69).set_Size(new Size(140, 26));
			((Control)label69).set_TabIndex(56);
			((Control)label69).set_Text("34 JPL 6000-year \r\n     planetary ephemeris");
			((Control)cmdDE422).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDE422).set_Location(new Point(75, 288));
			((Control)cmdDE422).set_Name("cmdDE422");
			((Control)cmdDE422).set_Size(new Size(64, 22));
			((Control)cmdDE422).set_TabIndex(55);
			((Control)cmdDE422).set_Text("Download");
			((ButtonBase)cmdDE422).set_UseVisualStyleBackColor(true);
			((Control)cmdDE422).add_Click((EventHandler)cmdDE422_Click);
			((ButtonBase)DownloadFlagStarDias).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownloadFlagStarDias).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownloadFlagStarDias).set_ImageIndex(0);
			((ButtonBase)DownloadFlagStarDias).set_ImageList(imageList1);
			((Control)DownloadFlagStarDias).set_Location(new Point(61, 142));
			((Control)DownloadFlagStarDias).set_Name("DownloadFlagStarDias");
			((Control)DownloadFlagStarDias).set_Size(new Size(14, 18));
			((Control)DownloadFlagStarDias).set_TabIndex(31);
			((ButtonBase)DownloadFlagStarDias).set_UseVisualStyleBackColor(true);
			((Control)DownloadFlagStarDias).set_Visible(false);
			((Control)label61).set_AutoSize(true);
			((Control)label61).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label61).set_Location(new Point(304, 145));
			((Control)label61).set_Name("label61");
			((Control)label61).set_Size(new Size(424, 13));
			((Control)label61).set_TabIndex(34);
			((Control)label61).set_Text("Star angular diameters. [Derivd from a merging of the CHARM2 and CADARS catalogues.");
			((Control)label62).set_AutoSize(true);
			((Control)label62).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label62).set_Location(new Point(140, 145));
			((Control)label62).set_Name("label62");
			((Control)label62).set_Size(new Size(106, 13));
			((Control)label62).set_TabIndex(33);
			((Control)label62).set_Text("30 Star diameters");
			((Control)cmdStarDiameters).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdStarDiameters).set_Location(new Point(74, 140));
			((Control)cmdStarDiameters).set_Name("cmdStarDiameters");
			((Control)cmdStarDiameters).set_Size(new Size(64, 22));
			((Control)cmdStarDiameters).set_TabIndex(32);
			((Control)cmdStarDiameters).set_Text("Download");
			((ButtonBase)cmdStarDiameters).set_UseVisualStyleBackColor(true);
			((Control)cmdStarDiameters).add_Click((EventHandler)cmdStarDiameters_Click);
			((ButtonBase)DownloadFlagJPLDE).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownloadFlagJPLDE).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownloadFlagJPLDE).set_ImageIndex(0);
			((ButtonBase)DownloadFlagJPLDE).set_ImageList(imageList1);
			((Control)DownloadFlagJPLDE).set_Location(new Point(61, 253));
			((Control)DownloadFlagJPLDE).set_Name("DownloadFlagJPLDE");
			((Control)DownloadFlagJPLDE).set_Size(new Size(14, 18));
			((Control)DownloadFlagJPLDE).set_TabIndex(49);
			((ButtonBase)DownloadFlagJPLDE).set_UseVisualStyleBackColor(true);
			((Control)DownloadFlagJPLDE).set_Visible(false);
			((ButtonBase)DownloadFlagTycho2).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownloadFlagTycho2).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownloadFlagTycho2).set_ImageIndex(0);
			((ButtonBase)DownloadFlagTycho2).set_ImageList(imageList1);
			((Control)DownloadFlagTycho2).set_Location(new Point(61, 179));
			((Control)DownloadFlagTycho2).set_Name("DownloadFlagTycho2");
			((Control)DownloadFlagTycho2).set_Size(new Size(14, 18));
			((Control)DownloadFlagTycho2).set_TabIndex(37);
			((ButtonBase)DownloadFlagTycho2).set_UseVisualStyleBackColor(true);
			((Control)DownloadFlagTycho2).set_Visible(false);
			((ButtonBase)DownloadFlagXZ).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownloadFlagXZ).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownloadFlagXZ).set_ImageIndex(0);
			((ButtonBase)DownloadFlagXZ).set_ImageList(imageList1);
			((Control)DownloadFlagXZ).set_Location(new Point(61, 64));
			((Control)DownloadFlagXZ).set_Name("DownloadFlagXZ");
			((Control)DownloadFlagXZ).set_Size(new Size(14, 18));
			((Control)DownloadFlagXZ).set_TabIndex(7);
			((ButtonBase)DownloadFlagXZ).set_UseVisualStyleBackColor(true);
			((Control)DownloadFlagXZ).set_Visible(false);
			((ButtonBase)DownloadFlagLunarOld).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownloadFlagLunarOld).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownloadFlagLunarOld).set_ImageIndex(0);
			((ButtonBase)DownloadFlagLunarOld).set_ImageList(imageList1);
			((Control)DownloadFlagLunarOld).set_Location(new Point(61, 26));
			((Control)DownloadFlagLunarOld).set_Name("DownloadFlagLunarOld");
			((Control)DownloadFlagLunarOld).set_Size(new Size(14, 18));
			((Control)DownloadFlagLunarOld).set_TabIndex(1);
			((ButtonBase)DownloadFlagLunarOld).set_UseVisualStyleBackColor(true);
			((Control)DownloadFlagLunarOld).set_Visible(false);
			((Control)label33).set_AutoSize(true);
			((Control)label33).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label33).set_Location(new Point(304, 60));
			((Control)label33).set_Name("label33");
			((Control)label33).set_Size(new Size(401, 26));
			((Control)label33).set_TabIndex(10);
			((Control)label33).set_Text("The XZ catalogue is used for Lunar Occultations. Updates and revisions occur yearly,\r\nusually as a result of updates to double star information.");
			((Control)label34).set_AutoSize(true);
			((Control)label34).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label34).set_Location(new Point(141, 67));
			((Control)label34).set_Name("label34");
			((Control)label34).set_Size(new Size(101, 13));
			((Control)label34).set_TabIndex(9);
			((Control)label34).set_Text("28 XZ catalogue");
			((Control)cmdXZ).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdXZ).set_Location(new Point(75, 62));
			((Control)cmdXZ).set_Name("cmdXZ");
			((Control)cmdXZ).set_Size(new Size(64, 22));
			((Control)cmdXZ).set_TabIndex(8);
			((Control)cmdXZ).set_Text("Download");
			((ButtonBase)cmdXZ).set_UseVisualStyleBackColor(true);
			((Control)cmdXZ).add_Click((EventHandler)cmdXZ_Click);
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label21).set_Location(new Point(140, 330));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(159, 13));
			((Control)label21).set_TabIndex(61);
			((Control)label21).set_Text("35 EOP series before 1962");
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Strikeout, GraphicsUnit.Point, 0));
			((Control)label16).set_Location(new Point(304, 175));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(413, 26));
			((Control)label16).set_TabIndex(40);
			((Control)label16).set_Text("The Tycho2 catalogue is used for Asteroid Occultations. Updates and revisions are rare.\r\nSuperceded by #40  Gaia16_EDR3");
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label15).set_Location(new Point(304, 23));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(401, 26));
			((Control)label15).set_TabIndex(4);
			((Control)label15).set_Text("The historical files of lunar occultations are used for graze predictions and Baily bead \r\nreductions.  Update only when advised there is a new version.");
			((Control)label30).set_AutoSize(true);
			((Control)label30).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label30).set_Location(new Point(304, 175));
			((Control)label30).set_Name("label30");
			((Control)label30).set_Size(new Size(365, 26));
			((Control)label30).set_TabIndex(36);
			((Control)label30).set_Text("This adds the most recent lunar occultation observations to the historical files.\r\n[ No files available before 2009. ]");
			((Control)label31).set_AutoSize(true);
			((Control)label31).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label31).set_Location(new Point(141, 175));
			((Control)label31).set_Name("label31");
			((Control)label31).set_Size(new Size(93, 26));
			((Control)label31).set_TabIndex(35);
			((Control)label31).set_Text("5 Latest Lunar \r\n   observations");
			((Control)cmdLatestLunar).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdLatestLunar).set_Location(new Point(75, 177));
			((Control)cmdLatestLunar).set_Name("cmdLatestLunar");
			((Control)cmdLatestLunar).set_Size(new Size(64, 22));
			((Control)cmdLatestLunar).set_TabIndex(34);
			((Control)cmdLatestLunar).set_Text("Download");
			((ButtonBase)cmdLatestLunar).set_UseVisualStyleBackColor(true);
			((Control)cmdLatestLunar).add_Click((EventHandler)cmdLatestLunar_Click);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)lblUpdateDamitPreferred);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)label162);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)label158);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)cmdUpdateDAMIT);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)label144);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)DownLoadFlagShapes);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)lblShapeModels);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)label147);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)label148);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)cmdDownloadShapes);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)label149);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)label137);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)cmdDownloadGaia);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)lblGaiaFile);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)label145);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)label135);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)label134);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)label96);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)label57);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)lblAstDySUpdateDate);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)DownLoadFlagAstDyS);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)DownLoadFlagISAM);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)panel4);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)label4);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)cmdAstorb);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)cmdDownloadISAM);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)label6);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)label126);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)cmdAstorbVizier);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)lblFutureAllFile);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)lblAstorbFile);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)label19);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)lblMPCfile);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)lblAstDysFile);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)lblISAMfile);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)label97);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)label103);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)label95);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)label73);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)label75);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)label58);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)cmdDownloadAstDyS);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)label55);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)lblRetainFutureAll);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)label41);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)label39);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)label18);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)cmdMPCorbCzech);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)label7);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)cmdMPCorb);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)label8);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)cmdFuture540);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)label3);
			((Control)grpAsteroids).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpAsteroids).set_Location(new Point(12, 893));
			((Control)grpAsteroids).set_Name("grpAsteroids");
			((Control)grpAsteroids).set_Size(new Size(793, 364));
			((Control)grpAsteroids).set_TabIndex(33);
			grpAsteroids.set_TabStop(false);
			((Control)grpAsteroids).set_Text("Files for asteroid predictions / reductions");
			((Control)label162).set_AutoSize(true);
			((Control)label162).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label162).set_Location(new Point(479, 235));
			((Control)label162).set_Name("label162");
			((Control)label162).set_Size(new Size(231, 26));
			((Control)label162).set_TabIndex(163);
			((Control)label162).set_Text("Update your DAMIT shape models direct from \r\nthe DAMIT website. Update interval - 12 months");
			((Control)label158).set_AutoSize(true);
			((Control)label158).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label158).set_Location(new Point(316, 242));
			((Control)label158).set_Name("label158");
			((Control)label158).set_Size(new Size(159, 13));
			((Control)label158).set_TabIndex(162);
			((Control)label158).set_Text("24a Update DAMIT models");
			((Control)cmdUpdateDAMIT).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdUpdateDAMIT).set_Location(new Point(251, 238));
			((Control)cmdUpdateDAMIT).set_Name("cmdUpdateDAMIT");
			((Control)cmdUpdateDAMIT).set_Size(new Size(64, 22));
			((Control)cmdUpdateDAMIT).set_TabIndex(161);
			((Control)cmdUpdateDAMIT).set_Text("Update");
			((ButtonBase)cmdUpdateDAMIT).set_UseVisualStyleBackColor(true);
			((Control)cmdUpdateDAMIT).add_Click((EventHandler)cmdUpdateDAMIT_Click);
			((Control)label144).set_AutoSize(true);
			((Control)label144).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label144).set_Location(new Point(446, 215));
			((Control)label144).set_Name("label144");
			((Control)label144).set_Size(new Size(242, 13));
			((Control)label144).set_TabIndex(160);
			((Control)label144).set_Text("Includes all current shape models and support files");
			((ButtonBase)DownLoadFlagShapes).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownLoadFlagShapes).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownLoadFlagShapes).set_ImageIndex(0);
			((ButtonBase)DownLoadFlagShapes).set_ImageList(imageList1);
			((Control)DownLoadFlagShapes).set_Location(new Point(61, 213));
			((Control)DownLoadFlagShapes).set_Name("DownLoadFlagShapes");
			((Control)DownLoadFlagShapes).set_Size(new Size(14, 18));
			((Control)DownLoadFlagShapes).set_TabIndex(159);
			((ButtonBase)DownLoadFlagShapes).set_UseVisualStyleBackColor(true);
			((Control)DownLoadFlagShapes).set_Visible(false);
			((Control)lblShapeModels).set_AutoSize(true);
			((Control)lblShapeModels).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblShapeModels).set_Location(new Point(7, 215));
			((Control)lblShapeModels).set_Name("lblShapeModels");
			((Control)lblShapeModels).set_Size(new Size(36, 12));
			((Control)lblShapeModels).set_TabIndex(158);
			((Control)lblShapeModels).set_Text("-- --- ----");
			((Control)label147).set_AutoSize(true);
			((Control)label147).set_BackColor(SystemColors.Control);
			((Control)label147).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label147).set_ForeColor(Color.DarkBlue);
			((Control)label147).set_Location(new Point(740, 215));
			((Control)label147).set_Name("label147");
			((Control)label147).set_Size(new Size(46, 13));
			((Control)label147).set_TabIndex(157);
			((Control)label147).set_Text("194MB");
			((Control)label148).set_AutoSize(true);
			((Control)label148).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label148).set_Location(new Point(316, 215));
			((Control)label148).set_Name("label148");
			((Control)label148).set_Size(new Size(104, 13));
			((Control)label148).set_TabIndex(156);
			((Control)label148).set_Text("24 Shape models");
			((Control)cmdDownloadShapes).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDownloadShapes).set_Location(new Point(251, 211));
			((Control)cmdDownloadShapes).set_Name("cmdDownloadShapes");
			((Control)cmdDownloadShapes).set_Size(new Size(64, 22));
			((Control)cmdDownloadShapes).set_TabIndex(155);
			((Control)cmdDownloadShapes).set_Text("Download");
			((ButtonBase)cmdDownloadShapes).set_UseVisualStyleBackColor(true);
			((Control)cmdDownloadShapes).add_Click((EventHandler)cmdDownloadShapes_Click);
			((Control)label149).set_AutoSize(true);
			((Control)label149).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label149).set_Location(new Point(75, 209));
			((Control)label149).set_Name("label149");
			((Control)label149).set_Size(new Size(91, 26));
			((Control)label149).set_TabIndex(154);
			((Control)label149).set_Text("DAMIT and ISAM\r\nShape models \r\n");
			((Control)label137).set_AutoSize(true);
			((Control)label137).set_Enabled(false);
			((Control)label137).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label137).set_Location(new Point(75, 324));
			((Control)label137).set_Name("label137");
			((Control)label137).set_Size(new Size(142, 26));
			((Control)label137).set_TabIndex(150);
			((Control)label137).set_Text("Gaia file of asteroid elements\r\n[not yet derived from Gaia]");
			((Control)cmdDownloadGaia).set_Enabled(false);
			((Control)cmdDownloadGaia).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDownloadGaia).set_Location(new Point(252, 326));
			((Control)cmdDownloadGaia).set_Name("cmdDownloadGaia");
			((Control)cmdDownloadGaia).set_Size(new Size(64, 22));
			((Control)cmdDownloadGaia).set_TabIndex(151);
			((Control)cmdDownloadGaia).set_Text("Download");
			((ButtonBase)cmdDownloadGaia).set_UseVisualStyleBackColor(true);
			((Control)lblGaiaFile).set_AutoSize(true);
			((Control)lblGaiaFile).set_Enabled(false);
			((Control)lblGaiaFile).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblGaiaFile).set_Location(new Point(7, 331));
			((Control)lblGaiaFile).set_Name("lblGaiaFile");
			((Control)lblGaiaFile).set_Size(new Size(36, 12));
			((Control)lblGaiaFile).set_TabIndex(153);
			((Control)lblGaiaFile).set_Text("-- --- ----");
			((Control)label145).set_AutoSize(true);
			((Control)label145).set_Enabled(false);
			((Control)label145).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label145).set_Location(new Point(317, 331));
			((Control)label145).set_Name("label145");
			((Control)label145).set_Size(new Size(106, 13));
			((Control)label145).set_TabIndex(152);
			((Control)label145).set_Text("26 Gaia asteroids");
			((Control)label135).set_AutoSize(true);
			((Control)label135).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label135).set_Location(new Point(594, 169));
			((Control)label135).set_Name("label135");
			((Control)label135).set_Size(new Size(122, 26));
			((Control)label135).set_TabIndex(149);
			((Control)label135).set_Text("Note: The VizieR mirror is\r\nonly updated monthly");
			((Control)label134).set_AutoSize(true);
			((Control)label134).set_BackColor(Color.RoyalBlue);
			label134.set_BorderStyle((BorderStyle)2);
			((Control)label134).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label134).set_ForeColor(Color.Yellow);
			((Control)label134).set_Location(new Point(169, 98));
			((Control)label134).set_Name("label134");
			((Control)label134).set_Size(new Size(362, 15));
			((Control)label134).set_TabIndex(148);
			((Control)label134).set_Text("If downloading MPCorb, also download Astorb ( used to insert error values )");
			((Control)label96).set_AutoSize(true);
			((Control)label96).set_BackColor(SystemColors.Control);
			((Control)label96).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label96).set_ForeColor(Color.DarkBlue);
			((Control)label96).set_Location(new Point(747, 177));
			((Control)label96).set_Name("label96");
			((Control)label96).set_Size(new Size(39, 13));
			((Control)label96).set_TabIndex(104);
			((Control)label96).set_Text("53MB");
			((Control)label57).set_AutoSize(true);
			((Control)label57).set_BackColor(Color.RoyalBlue);
			label57.set_BorderStyle((BorderStyle)2);
			((Control)label57).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label57).set_ForeColor(Color.Yellow);
			((Control)label57).set_Location(new Point(166, 149));
			((Control)label57).set_Name("label57");
			((Control)label57).set_Size(new Size(458, 15));
			((Control)label57).set_TabIndex(147);
			((Control)label57).set_Text("If downloading AstDyS-2, also download Astorb ( used to insert object names, and error values )");
			((Control)lblAstDySUpdateDate).set_AutoSize(true);
			((Control)lblAstDySUpdateDate).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblAstDySUpdateDate).set_Location(new Point(424, 127));
			((Control)lblAstDySUpdateDate).set_Name("lblAstDySUpdateDate");
			((Control)lblAstDySUpdateDate).set_Size(new Size(159, 13));
			((Control)lblAstDySUpdateDate).set_TabIndex(146);
			((Control)lblAstDySUpdateDate).set_Text("Date of last update at AstDyS-2:");
			((ButtonBase)DownLoadFlagAstDyS).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownLoadFlagAstDyS).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownLoadFlagAstDyS).set_ImageIndex(0);
			((ButtonBase)DownLoadFlagAstDyS).set_ImageList(imageList1);
			((Control)DownLoadFlagAstDyS).set_Location(new Point(61, 125));
			((Control)DownLoadFlagAstDyS).set_Name("DownLoadFlagAstDyS");
			((Control)DownLoadFlagAstDyS).set_Size(new Size(14, 18));
			((Control)DownLoadFlagAstDyS).set_TabIndex(145);
			((ButtonBase)DownLoadFlagAstDyS).set_UseVisualStyleBackColor(true);
			((Control)DownLoadFlagAstDyS).set_Visible(false);
			((Control)DownLoadFlagISAM).set_Enabled(false);
			((ButtonBase)DownLoadFlagISAM).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownLoadFlagISAM).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownLoadFlagISAM).set_ImageIndex(0);
			((ButtonBase)DownLoadFlagISAM).set_ImageList(imageList1);
			((Control)DownLoadFlagISAM).set_Location(new Point(61, 286));
			((Control)DownLoadFlagISAM).set_Name("DownLoadFlagISAM");
			((Control)DownLoadFlagISAM).set_Size(new Size(14, 18));
			((Control)DownLoadFlagISAM).set_TabIndex(144);
			((ButtonBase)DownLoadFlagISAM).set_UseVisualStyleBackColor(true);
			((Control)DownLoadFlagISAM).set_Visible(false);
			((Control)panel4).set_BackColor(Color.PeachPuff);
			panel4.set_BorderStyle((BorderStyle)1);
			((Control)panel4).get_Controls().Add((Control)(object)cmdCreateISAM);
			((Control)panel4).get_Controls().Add((Control)(object)label5);
			((Control)panel4).set_Enabled(false);
			((Control)panel4).set_Location(new Point(439, 274));
			((Control)panel4).set_Name("panel4");
			((Control)panel4).set_Size(new Size(301, 57));
			((Control)panel4).set_TabIndex(143);
			((Control)cmdCreateISAM).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdCreateISAM).set_Location(new Point(5, 11));
			((Control)cmdCreateISAM).set_Name("cmdCreateISAM");
			((Control)cmdCreateISAM).set_Size(new Size(94, 22));
			((Control)cmdCreateISAM).set_TabIndex(69);
			((Control)cmdCreateISAM).set_Text("Create ISAM file");
			((ButtonBase)cmdCreateISAM).set_UseVisualStyleBackColor(true);
			((Control)cmdCreateISAM).add_Click((EventHandler)cmdCreateISAM_Click);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(104, 0));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(197, 52));
			((Control)label5).set_TabIndex(141);
			((Control)label5).set_Text("For ADMIN use.  Create a file which lists \r\nall asteroids in the ISAM data base, \r\ntogether with the number of shape \r\nmodels available ");
			((Control)cmdDownloadISAM).set_Enabled(false);
			((Control)cmdDownloadISAM).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDownloadISAM).set_Location(new Point(252, 286));
			((Control)cmdDownloadISAM).set_Name("cmdDownloadISAM");
			((Control)cmdDownloadISAM).set_Size(new Size(64, 22));
			((Control)cmdDownloadISAM).set_TabIndex(142);
			((Control)cmdDownloadISAM).set_Text("Download");
			((ButtonBase)cmdDownloadISAM).set_UseVisualStyleBackColor(true);
			((Control)cmdDownloadISAM).add_Click((EventHandler)cmdDownloadISAM_Click);
			((Control)label126).set_AutoSize(true);
			((Control)label126).set_BackColor(SystemColors.Control);
			((Control)label126).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label126).set_ForeColor(Color.DarkBlue);
			((Control)label126).set_Location(new Point(758, 290));
			((Control)label126).set_Name("label126");
			((Control)label126).set_Size(new Size(28, 13));
			((Control)label126).set_TabIndex(140);
			((Control)label126).set_Text("6kB");
			((Control)lblFutureAllFile).set_AutoSize(true);
			((Control)lblFutureAllFile).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblFutureAllFile).set_Location(new Point(7, 38));
			((Control)lblFutureAllFile).set_Name("lblFutureAllFile");
			((Control)lblFutureAllFile).set_Size(new Size(36, 12));
			((Control)lblFutureAllFile).set_TabIndex(138);
			((Control)lblFutureAllFile).set_Text("-- --- ----");
			((Control)lblAstorbFile).set_AutoSize(true);
			((Control)lblAstorbFile).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblAstorbFile).set_Location(new Point(7, 176));
			((Control)lblAstorbFile).set_Name("lblAstorbFile");
			((Control)lblAstorbFile).set_Size(new Size(36, 12));
			((Control)lblAstorbFile).set_TabIndex(136);
			((Control)lblAstorbFile).set_Text("-- --- ----");
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label19).set_Location(new Point(317, 176));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(61, 13));
			((Control)label19).set_TabIndex(17);
			((Control)label19).set_Text("23 Astorb");
			((Control)lblMPCfile).set_AutoSize(true);
			((Control)lblMPCfile).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblMPCfile).set_Location(new Point(7, 78));
			((Control)lblMPCfile).set_Name("lblMPCfile");
			((Control)lblMPCfile).set_Size(new Size(36, 12));
			((Control)lblMPCfile).set_TabIndex(135);
			((Control)lblMPCfile).set_Text("-- --- ----");
			((Control)lblAstDysFile).set_AutoSize(true);
			((Control)lblAstDysFile).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblAstDysFile).set_Location(new Point(7, 127));
			((Control)lblAstDysFile).set_Name("lblAstDysFile");
			((Control)lblAstDysFile).set_Size(new Size(36, 12));
			((Control)lblAstDysFile).set_TabIndex(134);
			((Control)lblAstDysFile).set_Text("-- --- ----");
			((Control)lblISAMfile).set_AutoSize(true);
			((Control)lblISAMfile).set_Enabled(false);
			((Control)lblISAMfile).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblISAMfile).set_Location(new Point(7, 291));
			((Control)lblISAMfile).set_Name("lblISAMfile");
			((Control)lblISAMfile).set_Size(new Size(36, 12));
			((Control)lblISAMfile).set_TabIndex(133);
			((Control)lblISAMfile).set_Text("-- --- ----");
			((Control)label97).set_AutoSize(true);
			((Control)label97).set_BackColor(SystemColors.Control);
			((Control)label97).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label97).set_ForeColor(Color.DarkBlue);
			((Control)label97).set_Location(new Point(747, 79));
			((Control)label97).set_Name("label97");
			((Control)label97).set_Size(new Size(39, 13));
			((Control)label97).set_TabIndex(105);
			((Control)label97).set_Text("42MB");
			((Control)label103).set_AutoSize(true);
			((Control)label103).set_BackColor(SystemColors.Control);
			((Control)label103).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label103).set_ForeColor(Color.DarkBlue);
			((Control)label103).set_Location(new Point(747, 37));
			((Control)label103).set_Name("label103");
			((Control)label103).set_Size(new Size(43, 13));
			((Control)label103).set_TabIndex(112);
			((Control)label103).set_Text("0.5MB");
			((Control)label95).set_AutoSize(true);
			((Control)label95).set_BackColor(SystemColors.Control);
			((Control)label95).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label95).set_ForeColor(Color.DarkBlue);
			((Control)label95).set_Location(new Point(740, 127));
			((Control)label95).set_Name("label95");
			((Control)label95).set_Size(new Size(46, 13));
			((Control)label95).set_TabIndex(103);
			((Control)label95).set_Text("100MB");
			((Control)label73).set_AutoSize(true);
			((Control)label73).set_Enabled(false);
			((Control)label73).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label73).set_Location(new Point(75, 284));
			((Control)label73).set_Name("label73");
			((Control)label73).set_Size(new Size(161, 26));
			((Control)label73).set_TabIndex(71);
			((Control)label73).set_Text("Asteroids && #models included in \r\nthe ISAM shape-model database");
			((Control)label75).set_AutoSize(true);
			((Control)label75).set_Enabled(false);
			((Control)label75).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label75).set_Location(new Point(318, 291));
			((Control)label75).set_Name("label75");
			((Control)label75).set_Size(new Size(110, 13));
			((Control)label75).set_TabIndex(68);
			((Control)label75).set_Text("25 ISAM asteroids");
			((Control)label58).set_AutoSize(true);
			((Control)label58).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label58).set_Location(new Point(316, 127));
			((Control)label58).set_Name("label58");
			((Control)label58).set_Size(new Size(77, 13));
			((Control)label58).set_TabIndex(67);
			((Control)label58).set_Text("22 AstDyS-2");
			((Control)cmdDownloadAstDyS).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDownloadAstDyS).set_Location(new Point(251, 123));
			((Control)cmdDownloadAstDyS).set_Name("cmdDownloadAstDyS");
			((Control)cmdDownloadAstDyS).set_Size(new Size(64, 22));
			((Control)cmdDownloadAstDyS).set_TabIndex(64);
			((Control)cmdDownloadAstDyS).set_Text("Download");
			((ButtonBase)cmdDownloadAstDyS).set_UseVisualStyleBackColor(true);
			((Control)cmdDownloadAstDyS).add_Click((EventHandler)cmdDownloadAstDyS_Click);
			((Control)label55).set_AutoSize(true);
			((Control)label55).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label55).set_Location(new Point(75, 121));
			((Control)label55).set_Name("label55");
			((Control)label55).set_Size(new Size(134, 26));
			((Control)label55).set_TabIndex(63);
			((Control)label55).set_Text("AstDyS-2      from the \r\n'Asteroids - Dynamic Site 2'");
			((Control)lblRetainFutureAll).set_AutoSize(true);
			((Control)lblRetainFutureAll).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblRetainFutureAll).set_Location(new Point(440, 44));
			((Control)lblRetainFutureAll).set_Name("lblRetainFutureAll");
			((Control)lblRetainFutureAll).set_Size(new Size(271, 12));
			((Control)lblRetainFutureAll).set_TabIndex(62);
			((Control)lblRetainFutureAll).set_Text("[The existing  FutureAll.xml  file will be retained with today's date]");
			((Control)label41).set_AutoSize(true);
			((Control)label41).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label41).set_Location(new Point(440, 33));
			((Control)label41).set_Name("label41");
			((Control)label41).set_Size(new Size(293, 13));
			((Control)label41).set_TabIndex(48);
			((Control)label41).set_Text("This file covers a limited number of asteroids for the next year");
			((Control)label39).set_AutoSize(true);
			((Control)label39).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label39).set_Location(new Point(75, 31));
			((Control)label39).set_Name("label39");
			((Control)label39).set_Size(new Size(157, 26));
			((Control)label39).set_TabIndex(47);
			((Control)label39).set_Text("Steve Preston's files of asteroid \r\noccultation elements #2");
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label18).set_Location(new Point(316, 78));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(69, 13));
			((Control)label18).set_TabIndex(16);
			((Control)label18).set_Text("21 MPCorb");
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label20).set_Location(new Point(141, 369));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(83, 13));
			((Control)label20).set_TabIndex(18);
			((Control)label20).set_Text("10 Future.xml");
			((Control)grpGeneral).get_Controls().Add((Control)(object)label165);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label163);
			((Control)grpGeneral).get_Controls().Add((Control)(object)panel5);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label161);
			((Control)grpGeneral).get_Controls().Add((Control)(object)lblAAVSOfile_Occult);
			((Control)grpGeneral).get_Controls().Add((Control)(object)DownloadFlagAAVSO_Occult);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label159);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label160);
			((Control)grpGeneral).get_Controls().Add((Control)(object)cmdDownloadAAVSO_Occult);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label50);
			((Control)grpGeneral).get_Controls().Add((Control)(object)lblAsteroidClassFile);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label141);
			((Control)grpGeneral).get_Controls().Add((Control)(object)DownloadFlagAsteroidClass);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label142);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label143);
			((Control)grpGeneral).get_Controls().Add((Control)(object)cmdAsteroidClass);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label127);
			((Control)grpGeneral).get_Controls().Add((Control)(object)lblLightCurves);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label123);
			((Control)grpGeneral).get_Controls().Add((Control)(object)DownloadFlagLightCurves);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label124);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label125);
			((Control)grpGeneral).get_Controls().Add((Control)(object)cmdDowloadLightCurves);
			((Control)grpGeneral).get_Controls().Add((Control)(object)panel3);
			((Control)grpGeneral).get_Controls().Add((Control)(object)lblCameraDelayFile);
			((Control)grpGeneral).get_Controls().Add((Control)(object)lblReportAddressFile);
			((Control)grpGeneral).get_Controls().Add((Control)(object)lblLatestLunarFile);
			((Control)grpGeneral).get_Controls().Add((Control)(object)lblBinaryAsteroidFile);
			((Control)grpGeneral).get_Controls().Add((Control)(object)lblAsteroidRingFile);
			((Control)grpGeneral).get_Controls().Add((Control)(object)lblAsteroidDiameterFile);
			((Control)grpGeneral).get_Controls().Add((Control)(object)lblFutureFile);
			((Control)grpGeneral).get_Controls().Add((Control)(object)lblEOPPresentFile);
			((Control)grpGeneral).get_Controls().Add((Control)(object)lblAsteroidObsFile);
			((Control)grpGeneral).get_Controls().Add((Control)(object)lblDeltaTAfile);
			((Control)grpGeneral).get_Controls().Add((Control)(object)lbl6thOrbitFile);
			((Control)grpGeneral).get_Controls().Add((Control)(object)lblWDSFile);
			((Control)grpGeneral).get_Controls().Add((Control)(object)lblWDSCodesFile);
			((Control)grpGeneral).get_Controls().Add((Control)(object)lblINT4File);
			((Control)grpGeneral).get_Controls().Add((Control)(object)lblAAVSOfile);
			((Control)grpGeneral).get_Controls().Add((Control)(object)lblCALLfile);
			((Control)grpGeneral).get_Controls().Add((Control)(object)lblCometFile);
			((Control)grpGeneral).get_Controls().Add((Control)(object)DownloadFlagWDScodes);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label120);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label118);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label119);
			((Control)grpGeneral).get_Controls().Add((Control)(object)cmdDownloadWDSCodes);
			((Control)grpGeneral).get_Controls().Add((Control)(object)DownloadFlagRings);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label117);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label116);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label115);
			((Control)grpGeneral).get_Controls().Add((Control)(object)cmdRings);
			((Control)grpGeneral).get_Controls().Add((Control)(object)DownloadFlagCameraDelay);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label107);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label108);
			((Control)grpGeneral).get_Controls().Add((Control)(object)cmdCameraDelays);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label105);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label104);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label102);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label101);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label100);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label99);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label98);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label94);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label93);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label92);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label91);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label90);
			((Control)grpGeneral).get_Controls().Add((Control)(object)DownloadFlagVisualBinaries);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label66);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label67);
			((Control)grpGeneral).get_Controls().Add((Control)(object)cmdDownloadVisualBinaries);
			((Control)grpGeneral).get_Controls().Add((Control)(object)downloadFlag_CALL);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label64);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label63);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label80);
			((Control)grpGeneral).get_Controls().Add((Control)(object)cmdDownloadAsteroidLightCurveData);
			((Control)grpGeneral).get_Controls().Add((Control)(object)DownloadFlagAAVSO);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label48);
			((Control)grpGeneral).get_Controls().Add((Control)(object)cmdDownloadAAVSO);
			((Control)grpGeneral).get_Controls().Add((Control)(object)DownloadFlagWDS);
			((Control)grpGeneral).get_Controls().Add((Control)(object)DownloadFlagIF);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label44);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label45);
			((Control)grpGeneral).get_Controls().Add((Control)(object)cmdDownloadWDS);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label42);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label43);
			((Control)grpGeneral).get_Controls().Add((Control)(object)cmdDownloadInterferometric);
			((Control)grpGeneral).get_Controls().Add((Control)(object)DownloadFlagAddresses);
			((Control)grpGeneral).get_Controls().Add((Control)(object)DownloadFlagRecentLunar);
			((Control)grpGeneral).get_Controls().Add((Control)(object)DownloadFlagAsteroids);
			((Control)grpGeneral).get_Controls().Add((Control)(object)lblRetainFuture);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label40);
			((Control)grpGeneral).get_Controls().Add((Control)(object)cmdDownloadAll);
			((Control)grpGeneral).get_Controls().Add((Control)(object)DownloadFlagAsteroidDiameters);
			((Control)grpGeneral).get_Controls().Add((Control)(object)DownloadFlagBinary);
			((Control)grpGeneral).get_Controls().Add((Control)(object)DownloadFlagdeltaT);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label20);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label37);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label38);
			((Control)grpGeneral).get_Controls().Add((Control)(object)cmdAsteroidDiameters);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label35);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label36);
			((Control)grpGeneral).get_Controls().Add((Control)(object)cmdBinary);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label30);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label28);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label31);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label2);
			((Control)grpGeneral).get_Controls().Add((Control)(object)cmdFuture);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label29);
			((Control)grpGeneral).get_Controls().Add((Control)(object)cmdLatestLunar);
			((Control)grpGeneral).get_Controls().Add((Control)(object)cmdDeltaT);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label26);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label27);
			((Control)grpGeneral).get_Controls().Add((Control)(object)cmdAddresses);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label25);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label24);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label23);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label22);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label14);
			((Control)grpGeneral).get_Controls().Add((Control)(object)cmdAsteroidObservations);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label9);
			((Control)grpGeneral).get_Controls().Add((Control)(object)cmdDowload_EOP_new);
			((Control)grpGeneral).get_Controls().Add((Control)(object)label1);
			((Control)grpGeneral).get_Controls().Add((Control)(object)cmdComet);
			((Control)grpGeneral).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpGeneral).set_Location(new Point(10, 8));
			((Control)grpGeneral).set_Name("grpGeneral");
			((Control)grpGeneral).set_Size(new Size(793, 869));
			((Control)grpGeneral).set_TabIndex(34);
			grpGeneral.set_TabStop(false);
			((Control)grpGeneral).set_Text("General downloads");
			((Control)label161).set_AutoSize(true);
			((Control)label161).set_BackColor(SystemColors.Control);
			((Control)label161).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label161).set_ForeColor(Color.DarkBlue);
			((Control)label161).set_Location(new Point(740, 717));
			((Control)label161).set_Name("label161");
			((Control)label161).set_Size(new Size(46, 13));
			((Control)label161).set_TabIndex(174);
			((Control)label161).set_Text("135MB");
			((Control)lblAAVSOfile_Occult).set_AutoSize(true);
			((Control)lblAAVSOfile_Occult).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblAAVSOfile_Occult).set_Location(new Point(7, 718));
			((Control)lblAAVSOfile_Occult).set_Name("lblAAVSOfile_Occult");
			((Control)lblAAVSOfile_Occult).set_Size(new Size(36, 12));
			((Control)lblAAVSOfile_Occult).set_TabIndex(173);
			((Control)lblAAVSOfile_Occult).set_Text("-- --- ----");
			((ButtonBase)DownloadFlagAAVSO_Occult).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownloadFlagAAVSO_Occult).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownloadFlagAAVSO_Occult).set_ImageIndex(0);
			((ButtonBase)DownloadFlagAAVSO_Occult).set_ImageList(imageList1);
			((Control)DownloadFlagAAVSO_Occult).set_Location(new Point(61, 713));
			((Control)DownloadFlagAAVSO_Occult).set_Name("DownloadFlagAAVSO_Occult");
			((Control)DownloadFlagAAVSO_Occult).set_Size(new Size(14, 18));
			((Control)DownloadFlagAAVSO_Occult).set_TabIndex(172);
			((ButtonBase)DownloadFlagAAVSO_Occult).set_UseVisualStyleBackColor(true);
			((Control)DownloadFlagAAVSO_Occult).set_Visible(false);
			((Control)label159).set_AutoSize(true);
			((Control)label159).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label159).set_Location(new Point(304, 711));
			((Control)label159).set_Name("label159");
			((Control)label159).set_Size(new Size(301, 26));
			((Control)label159).set_TabIndex(171);
			((Control)label159).set_Text("From Occult server. The converted AAVSO Variable Star Index.\r\nUpdate interval : 12 months");
			((Control)label160).set_AutoSize(true);
			((Control)label160).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label160).set_Location(new Point(139, 718));
			((Control)label160).set_Name("label160");
			((Control)label160).set_Size(new Size(121, 13));
			((Control)label160).set_TabIndex(170);
			((Control)label160).set_Text("17 AAVSO variables");
			((Control)cmdDownloadAAVSO_Occult).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDownloadAAVSO_Occult).set_Location(new Point(74, 713));
			((Control)cmdDownloadAAVSO_Occult).set_Name("cmdDownloadAAVSO_Occult");
			((Control)cmdDownloadAAVSO_Occult).set_Size(new Size(64, 22));
			((Control)cmdDownloadAAVSO_Occult).set_TabIndex(169);
			((Control)cmdDownloadAAVSO_Occult).set_Text("Download\r\n");
			((ButtonBase)cmdDownloadAAVSO_Occult).set_UseVisualStyleBackColor(true);
			((Control)cmdDownloadAAVSO_Occult).add_Click((EventHandler)cmdDownloadAAVSO_Occult_Click);
			((Control)label157).set_AutoSize(true);
			((Control)label157).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Bold));
			((Control)label157).set_ForeColor(Color.Maroon);
			((Control)label157).set_Location(new Point(213, 25));
			((Control)label157).set_Name("label157");
			((Control)label157).set_Size(new Size(172, 13));
			((Control)label157).set_TabIndex(168);
			((Control)label157).set_Text("Max. magnitude brighter than");
			updnMaxMagLimit.set_DecimalPlaces(1);
			((Control)updnMaxMagLimit).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnMaxMagLimit.set_Increment(new decimal(new int[4] { 5, 0, 0, 131072 }));
			((Control)updnMaxMagLimit).set_Location(new Point(387, 23));
			updnMaxMagLimit.set_Maximum(new decimal(new int[4] { 20, 0, 0, 0 }));
			updnMaxMagLimit.set_Minimum(new decimal(new int[4] { 10, 0, 0, 0 }));
			((Control)updnMaxMagLimit).set_Name("updnMaxMagLimit");
			((Control)updnMaxMagLimit).set_Size(new Size(44, 20));
			((Control)updnMaxMagLimit).set_TabIndex(167);
			((UpDownBase)updnMaxMagLimit).set_TextAlign((HorizontalAlignment)2);
			updnMaxMagLimit.set_Value(new decimal(new int[4] { 165, 0, 0, 65536 }));
			((Control)label156).set_AutoSize(true);
			((Control)label156).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Bold));
			((Control)label156).set_ForeColor(Color.Maroon);
			((Control)label156).set_Location(new Point(213, 4));
			((Control)label156).set_Name("label156");
			((Control)label156).set_Size(new Size(172, 13));
			((Control)label156).set_TabIndex(166);
			((Control)label156).set_Text("Minimum amplitude to include");
			updnMagLimitAAVSO.set_DecimalPlaces(2);
			((Control)updnMagLimitAAVSO).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnMagLimitAAVSO.set_Increment(new decimal(new int[4] { 5, 0, 0, 131072 }));
			((Control)updnMagLimitAAVSO).set_Location(new Point(387, 2));
			updnMagLimitAAVSO.set_Maximum(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnMagLimitAAVSO).set_Name("updnMagLimitAAVSO");
			((Control)updnMagLimitAAVSO).set_Size(new Size(44, 20));
			((Control)updnMagLimitAAVSO).set_TabIndex(165);
			((UpDownBase)updnMagLimitAAVSO).set_TextAlign((HorizontalAlignment)2);
			updnMagLimitAAVSO.set_Value(new decimal(new int[4] { 2, 0, 0, 65536 }));
			((Control)label50).set_AutoSize(true);
			((Control)label50).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label50).set_ForeColor(Color.Maroon);
			((Control)label50).set_Location(new Point(558, 686));
			((Control)label50).set_Name("label50");
			((Control)label50).set_Size(new Size(180, 13));
			((Control)label50).set_TabIndex(164);
			((Control)label50).set_Text("No longer maintained by USNO");
			((Control)lblAsteroidClassFile).set_AutoSize(true);
			((Control)lblAsteroidClassFile).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblAsteroidClassFile).set_Location(new Point(6, 332));
			((Control)lblAsteroidClassFile).set_Name("lblAsteroidClassFile");
			((Control)lblAsteroidClassFile).set_Size(new Size(36, 12));
			((Control)lblAsteroidClassFile).set_TabIndex(163);
			((Control)lblAsteroidClassFile).set_Text("-- --- ----");
			((Control)label141).set_AutoSize(true);
			((Control)label141).set_BackColor(SystemColors.Control);
			((Control)label141).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label141).set_ForeColor(Color.DarkBlue);
			((Control)label141).set_Location(new Point(751, 332));
			((Control)label141).set_Name("label141");
			((Control)label141).set_Size(new Size(35, 13));
			((Control)label141).set_TabIndex(162);
			((Control)label141).set_Text("24kB");
			((ButtonBase)DownloadFlagAsteroidClass).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownloadFlagAsteroidClass).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownloadFlagAsteroidClass).set_ImageIndex(0);
			((ButtonBase)DownloadFlagAsteroidClass).set_ImageList(imageList1);
			((Control)DownloadFlagAsteroidClass).set_Location(new Point(61, 329));
			((Control)DownloadFlagAsteroidClass).set_Name("DownloadFlagAsteroidClass");
			((Control)DownloadFlagAsteroidClass).set_Size(new Size(14, 18));
			((Control)DownloadFlagAsteroidClass).set_TabIndex(161);
			((ButtonBase)DownloadFlagAsteroidClass).set_UseVisualStyleBackColor(true);
			((Control)DownloadFlagAsteroidClass).set_Visible(false);
			((Control)label142).set_AutoSize(true);
			((Control)label142).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label142).set_Location(new Point(304, 332));
			((Control)label142).set_Name("label142");
			((Control)label142).set_Size(new Size(349, 13));
			((Control)label142).set_TabIndex(160);
			((Control)label142).set_Text("Taxonomic classes of asteroids (Amor, Apollo, Aten, Centaur, TNO, Trojan)");
			((Control)label143).set_AutoSize(true);
			((Control)label143).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label143).set_Location(new Point(140, 332));
			((Control)label143).set_Name("label143");
			((Control)label143).set_Size(new Size(110, 13));
			((Control)label143).set_TabIndex(159);
			((Control)label143).set_Text("9 Asteroid classes");
			((Control)cmdAsteroidClass).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdAsteroidClass).set_Location(new Point(75, 327));
			((Control)cmdAsteroidClass).set_Name("cmdAsteroidClass");
			((Control)cmdAsteroidClass).set_Size(new Size(64, 22));
			((Control)cmdAsteroidClass).set_TabIndex(158);
			((Control)cmdAsteroidClass).set_Text("Download");
			((ButtonBase)cmdAsteroidClass).set_UseVisualStyleBackColor(true);
			((Control)cmdAsteroidClass).add_Click((EventHandler)cmdAsteroidClass_Click);
			((Control)label127).set_AutoSize(true);
			((Control)label127).set_BackColor(Color.Gold);
			label127.set_BorderStyle((BorderStyle)1);
			((Control)label127).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label127).set_ForeColor(SystemColors.ControlText);
			((Control)label127).set_Location(new Point(304, 614));
			((Control)label127).set_Name("label127");
			((Control)label127).set_Size(new Size(356, 14));
			((Control)label127).set_TabIndex(157);
			((Control)label127).set_Text("The WDS file may not be available on Sundays and Monday mornings (European time)");
			((Control)lblLightCurves).set_AutoSize(true);
			((Control)lblLightCurves).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblLightCurves).set_Location(new Point(9, 527));
			((Control)lblLightCurves).set_Name("lblLightCurves");
			((Control)lblLightCurves).set_Size(new Size(36, 12));
			((Control)lblLightCurves).set_TabIndex(156);
			((Control)lblLightCurves).set_Text("-- --- ----");
			((Control)label123).set_AutoSize(true);
			((Control)label123).set_BackColor(SystemColors.Control);
			((Control)label123).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label123).set_ForeColor(Color.DarkBlue);
			((Control)label123).set_Location(new Point(718, 527));
			((Control)label123).set_Name("label123");
			((Control)label123).set_Size(new Size(70, 13));
			((Control)label123).set_TabIndex(155);
			((Control)label123).set_Text("several MB");
			((ButtonBase)DownloadFlagLightCurves).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownloadFlagLightCurves).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownloadFlagLightCurves).set_ImageIndex(0);
			((ButtonBase)DownloadFlagLightCurves).set_ImageList(imageList1);
			((Control)DownloadFlagLightCurves).set_Location(new Point(63, 522));
			((Control)DownloadFlagLightCurves).set_Name("DownloadFlagLightCurves");
			((Control)DownloadFlagLightCurves).set_Size(new Size(14, 18));
			((Control)DownloadFlagLightCurves).set_TabIndex(154);
			((ButtonBase)DownloadFlagLightCurves).set_UseVisualStyleBackColor(true);
			((Control)DownloadFlagLightCurves).set_Visible(false);
			((Control)label124).set_AutoSize(true);
			((Control)label124).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label124).set_Location(new Point(304, 527));
			((Control)label124).set_Name("label124");
			((Control)label124).set_Size(new Size(145, 13));
			((Control)label124).set_TabIndex(153);
			((Control)label124).set_Text("Light curves from occultations");
			((Control)label125).set_AutoSize(true);
			((Control)label125).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label125).set_Location(new Point(141, 527));
			((Control)label125).set_Name("label125");
			((Control)label125).set_Size(new Size(149, 13));
			((Control)label125).set_TabIndex(152);
			((Control)label125).set_Text("13 Observed light curves");
			((Control)cmdDowloadLightCurves).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDowloadLightCurves).set_Location(new Point(76, 522));
			((Control)cmdDowloadLightCurves).set_Name("cmdDowloadLightCurves");
			((Control)cmdDowloadLightCurves).set_Size(new Size(64, 22));
			((Control)cmdDowloadLightCurves).set_TabIndex(151);
			((Control)cmdDowloadLightCurves).set_Text("Download");
			((ButtonBase)cmdDowloadLightCurves).set_UseVisualStyleBackColor(true);
			((Control)cmdDowloadLightCurves).add_Click((EventHandler)cmdDowloadLightCurves_Click);
			((Control)panel3).set_BackColor(Color.LavenderBlush);
			panel3.set_BorderStyle((BorderStyle)1);
			((Control)panel3).get_Controls().Add((Control)(object)cmbFileAge);
			((Control)panel3).get_Controls().Add((Control)(object)label59);
			((Control)panel3).set_Location(new Point(61, 566));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(603, 29));
			((Control)panel3).set_TabIndex(150);
			((Control)cmbFileAge).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbFileAge).set_FormattingEnabled(true);
			cmbFileAge.get_Items().AddRange(new object[5] { "1 month", "3 months", "6 months", "1 year", "5 years" });
			((Control)cmbFileAge).set_Location(new Point(504, 3));
			((Control)cmbFileAge).set_Name("cmbFileAge");
			((Control)cmbFileAge).set_Size(new Size(73, 21));
			((Control)cmbFileAge).set_TabIndex(0);
			cmbFileAge.add_SelectedIndexChanged((EventHandler)cmbFileAge_SelectedIndexChanged);
			((Control)label59).set_AutoSize(true);
			((Control)label59).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label59).set_Location(new Point(19, 6));
			((Control)label59).set_Name("label59");
			((Control)label59).set_Size(new Size(482, 13));
			((Control)label59).set_TabIndex(1);
			((Control)label59).set_Text("For the next group of files, the indicator to download the file is given when the file is 'old' by more than");
			((Control)lblCameraDelayFile).set_AutoSize(true);
			((Control)lblCameraDelayFile).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblCameraDelayFile).set_Location(new Point(6, 494));
			((Control)lblCameraDelayFile).set_Name("lblCameraDelayFile");
			((Control)lblCameraDelayFile).set_Size(new Size(36, 12));
			((Control)lblCameraDelayFile).set_TabIndex(149);
			((Control)lblCameraDelayFile).set_Text("-- --- ----");
			((Control)lblReportAddressFile).set_AutoSize(true);
			((Control)lblReportAddressFile).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblReportAddressFile).set_Location(new Point(6, 460));
			((Control)lblReportAddressFile).set_Name("lblReportAddressFile");
			((Control)lblReportAddressFile).set_Size(new Size(36, 12));
			((Control)lblReportAddressFile).set_TabIndex(148);
			((Control)lblReportAddressFile).set_Text("-- --- ----");
			((Control)lblLatestLunarFile).set_AutoSize(true);
			((Control)lblLatestLunarFile).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblLatestLunarFile).set_Location(new Point(6, 182));
			((Control)lblLatestLunarFile).set_Name("lblLatestLunarFile");
			((Control)lblLatestLunarFile).set_Size(new Size(36, 12));
			((Control)lblLatestLunarFile).set_TabIndex(147);
			((Control)lblLatestLunarFile).set_Text("-- --- ----");
			((Control)lblBinaryAsteroidFile).set_AutoSize(true);
			((Control)lblBinaryAsteroidFile).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblBinaryAsteroidFile).set_Location(new Point(6, 220));
			((Control)lblBinaryAsteroidFile).set_Name("lblBinaryAsteroidFile");
			((Control)lblBinaryAsteroidFile).set_Size(new Size(36, 12));
			((Control)lblBinaryAsteroidFile).set_TabIndex(146);
			((Control)lblBinaryAsteroidFile).set_Text("-- --- ----");
			((Control)lblAsteroidRingFile).set_AutoSize(true);
			((Control)lblAsteroidRingFile).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblAsteroidRingFile).set_Location(new Point(6, 258));
			((Control)lblAsteroidRingFile).set_Name("lblAsteroidRingFile");
			((Control)lblAsteroidRingFile).set_Size(new Size(36, 12));
			((Control)lblAsteroidRingFile).set_TabIndex(145);
			((Control)lblAsteroidRingFile).set_Text("-- --- ----");
			((Control)lblAsteroidDiameterFile).set_AutoSize(true);
			((Control)lblAsteroidDiameterFile).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblAsteroidDiameterFile).set_Location(new Point(6, 296));
			((Control)lblAsteroidDiameterFile).set_Name("lblAsteroidDiameterFile");
			((Control)lblAsteroidDiameterFile).set_Size(new Size(36, 12));
			((Control)lblAsteroidDiameterFile).set_TabIndex(144);
			((Control)lblAsteroidDiameterFile).set_Text("-- --- ----");
			((Control)lblFutureFile).set_AutoSize(true);
			((Control)lblFutureFile).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblFutureFile).set_Location(new Point(6, 369));
			((Control)lblFutureFile).set_Name("lblFutureFile");
			((Control)lblFutureFile).set_Size(new Size(36, 12));
			((Control)lblFutureFile).set_TabIndex(143);
			((Control)lblFutureFile).set_Text("-- --- ----");
			((Control)lblEOPPresentFile).set_AutoSize(true);
			((Control)lblEOPPresentFile).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblEOPPresentFile).set_Location(new Point(6, 29));
			((Control)lblEOPPresentFile).set_Name("lblEOPPresentFile");
			((Control)lblEOPPresentFile).set_Size(new Size(36, 12));
			((Control)lblEOPPresentFile).set_TabIndex(142);
			((Control)lblEOPPresentFile).set_Text("-- --- ----");
			((Control)lblAsteroidObsFile).set_AutoSize(true);
			((Control)lblAsteroidObsFile).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblAsteroidObsFile).set_Location(new Point(6, 105));
			((Control)lblAsteroidObsFile).set_Name("lblAsteroidObsFile");
			((Control)lblAsteroidObsFile).set_Size(new Size(36, 12));
			((Control)lblAsteroidObsFile).set_TabIndex(141);
			((Control)lblAsteroidObsFile).set_Text("-- --- ----");
			((Control)lblDeltaTAfile).set_AutoSize(true);
			((Control)lblDeltaTAfile).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblDeltaTAfile).set_Location(new Point(6, 144));
			((Control)lblDeltaTAfile).set_Name("lblDeltaTAfile");
			((Control)lblDeltaTAfile).set_Size(new Size(36, 12));
			((Control)lblDeltaTAfile).set_TabIndex(139);
			((Control)lblDeltaTAfile).set_Text("-- --- ----");
			((Control)lbl6thOrbitFile).set_AutoSize(true);
			((Control)lbl6thOrbitFile).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lbl6thOrbitFile).set_Location(new Point(7, 840));
			((Control)lbl6thOrbitFile).set_Name("lbl6thOrbitFile");
			((Control)lbl6thOrbitFile).set_Size(new Size(36, 12));
			((Control)lbl6thOrbitFile).set_TabIndex(138);
			((Control)lbl6thOrbitFile).set_Text("-- --- ----");
			((Control)lblWDSFile).set_AutoSize(true);
			((Control)lblWDSFile).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblWDSFile).set_Location(new Point(7, 606));
			((Control)lblWDSFile).set_Name("lblWDSFile");
			((Control)lblWDSFile).set_Size(new Size(36, 12));
			((Control)lblWDSFile).set_TabIndex(137);
			((Control)lblWDSFile).set_Text("-- --- ----");
			((Control)lblWDSCodesFile).set_AutoSize(true);
			((Control)lblWDSCodesFile).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblWDSCodesFile).set_Location(new Point(7, 643));
			((Control)lblWDSCodesFile).set_Name("lblWDSCodesFile");
			((Control)lblWDSCodesFile).set_Size(new Size(36, 12));
			((Control)lblWDSCodesFile).set_TabIndex(136);
			((Control)lblWDSCodesFile).set_Text("-- --- ----");
			((Control)lblINT4File).set_AutoSize(true);
			((Control)lblINT4File).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblINT4File).set_Location(new Point(7, 680));
			((Control)lblINT4File).set_Name("lblINT4File");
			((Control)lblINT4File).set_Size(new Size(36, 12));
			((Control)lblINT4File).set_TabIndex(135);
			((Control)lblINT4File).set_Text("-- --- ----");
			((Control)lblAAVSOfile).set_AutoSize(true);
			((Control)lblAAVSOfile).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblAAVSOfile).set_Location(new Point(7, 759));
			((Control)lblAAVSOfile).set_Name("lblAAVSOfile");
			((Control)lblAAVSOfile).set_Size(new Size(36, 12));
			((Control)lblAAVSOfile).set_TabIndex(134);
			((Control)lblAAVSOfile).set_Text("-- --- ----");
			((Control)lblCALLfile).set_AutoSize(true);
			((Control)lblCALLfile).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblCALLfile).set_Location(new Point(7, 803));
			((Control)lblCALLfile).set_Name("lblCALLfile");
			((Control)lblCALLfile).set_Size(new Size(36, 12));
			((Control)lblCALLfile).set_TabIndex(133);
			((Control)lblCALLfile).set_Text("-- --- ----");
			((Control)lblCometFile).set_AutoSize(true);
			((Control)lblCometFile).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblCometFile).set_Location(new Point(6, 67));
			((Control)lblCometFile).set_Name("lblCometFile");
			((Control)lblCometFile).set_Size(new Size(36, 12));
			((Control)lblCometFile).set_TabIndex(132);
			((Control)lblCometFile).set_Text("-- --- ----");
			((ButtonBase)DownloadFlagWDScodes).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownloadFlagWDScodes).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownloadFlagWDScodes).set_ImageIndex(0);
			((ButtonBase)DownloadFlagWDScodes).set_ImageList(imageList1);
			((Control)DownloadFlagWDScodes).set_Location(new Point(61, 639));
			((Control)DownloadFlagWDScodes).set_Name("DownloadFlagWDScodes");
			((Control)DownloadFlagWDScodes).set_Size(new Size(14, 18));
			((Control)DownloadFlagWDScodes).set_TabIndex(131);
			((ButtonBase)DownloadFlagWDScodes).set_UseVisualStyleBackColor(true);
			((Control)DownloadFlagWDScodes).set_Visible(false);
			((Control)label120).set_AutoSize(true);
			((Control)label120).set_BackColor(SystemColors.Control);
			((Control)label120).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label120).set_ForeColor(Color.DarkBlue);
			((Control)label120).set_Location(new Point(743, 643));
			((Control)label120).set_Name("label120");
			((Control)label120).set_Size(new Size(43, 13));
			((Control)label120).set_TabIndex(129);
			((Control)label120).set_Text("2.4MB");
			((Control)label118).set_AutoSize(true);
			((Control)label118).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label118).set_Location(new Point(139, 643));
			((Control)label118).set_Name("label118");
			((Control)label118).set_Size(new Size(155, 13));
			((Control)label118).set_TabIndex(128);
			((Control)label118).set_Text("15 WDS discoverer codes");
			((Control)label119).set_AutoSize(true);
			((Control)label119).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label119).set_Location(new Point(304, 643));
			((Control)label119).set_Name("label119");
			((Control)label119).set_Size(new Size(364, 13));
			((Control)label119).set_TabIndex(127);
			((Control)label119).set_Text("Discoverer codes for the WDS catalogue. Update only required occassionally");
			((Control)cmdDownloadWDSCodes).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDownloadWDSCodes).set_Location(new Point(74, 638));
			((Control)cmdDownloadWDSCodes).set_Name("cmdDownloadWDSCodes");
			((Control)cmdDownloadWDSCodes).set_Size(new Size(64, 22));
			((Control)cmdDownloadWDSCodes).set_TabIndex(126);
			((Control)cmdDownloadWDSCodes).set_Text("Download");
			((ButtonBase)cmdDownloadWDSCodes).set_UseVisualStyleBackColor(true);
			((Control)cmdDownloadWDSCodes).add_Click((EventHandler)cmdDownloadWDSCodes_Click);
			((ButtonBase)DownloadFlagRings).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownloadFlagRings).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownloadFlagRings).set_ImageIndex(0);
			((ButtonBase)DownloadFlagRings).set_ImageList(imageList1);
			((Control)DownloadFlagRings).set_Location(new Point(62, 255));
			((Control)DownloadFlagRings).set_Name("DownloadFlagRings");
			((Control)DownloadFlagRings).set_Size(new Size(14, 18));
			((Control)DownloadFlagRings).set_TabIndex(124);
			((ButtonBase)DownloadFlagRings).set_UseVisualStyleBackColor(true);
			((Control)DownloadFlagRings).set_Visible(false);
			((Control)label117).set_AutoSize(true);
			((Control)label117).set_BackColor(SystemColors.Control);
			((Control)label117).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label117).set_ForeColor(Color.DarkBlue);
			((Control)label117).set_Location(new Point(747, 258));
			((Control)label117).set_Name("label117");
			((Control)label117).set_Size(new Size(39, 13));
			((Control)label117).set_TabIndex(123);
			((Control)label117).set_Text("0.2kB");
			((Control)label116).set_AutoSize(true);
			((Control)label116).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label116).set_Location(new Point(304, 258));
			((Control)label116).set_Name("label116");
			((Control)label116).set_Size(new Size(221, 13));
			((Control)label116).set_TabIndex(122);
			((Control)label116).set_Text("Details of asteroid ring systems. Update yearly");
			((Control)label115).set_AutoSize(true);
			((Control)label115).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label115).set_Location(new Point(141, 258));
			((Control)label115).set_Name("label115");
			((Control)label115).set_Size(new Size(95, 13));
			((Control)label115).set_TabIndex(121);
			((Control)label115).set_Text("7 Asteroid rings");
			((Control)cmdRings).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdRings).set_Location(new Point(76, 253));
			((Control)cmdRings).set_Name("cmdRings");
			((Control)cmdRings).set_Size(new Size(64, 22));
			((Control)cmdRings).set_TabIndex(120);
			((Control)cmdRings).set_Text("Download");
			((ButtonBase)cmdRings).set_UseVisualStyleBackColor(true);
			((Control)cmdRings).add_Click((EventHandler)cmdRings_Click);
			((ButtonBase)DownloadFlagCameraDelay).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownloadFlagCameraDelay).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownloadFlagCameraDelay).set_ImageIndex(0);
			((ButtonBase)DownloadFlagCameraDelay).set_ImageList(imageList1);
			((Control)DownloadFlagCameraDelay).set_Location(new Point(63, 489));
			((Control)DownloadFlagCameraDelay).set_Name("DownloadFlagCameraDelay");
			((Control)DownloadFlagCameraDelay).set_Size(new Size(14, 18));
			((Control)DownloadFlagCameraDelay).set_TabIndex(119);
			((ButtonBase)DownloadFlagCameraDelay).set_UseVisualStyleBackColor(true);
			((Control)DownloadFlagCameraDelay).set_Visible(false);
			((Control)label107).set_AutoSize(true);
			((Control)label107).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label107).set_Location(new Point(304, 494));
			((Control)label107).set_Name("label107");
			((Control)label107).set_Size(new Size(371, 13));
			((Control)label107).set_TabIndex(117);
			((Control)label107).set_Text("A list of video cameras and their corrections for internal delays.   Used in AOTA");
			((Control)label108).set_AutoSize(true);
			((Control)label108).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label108).set_Location(new Point(141, 494));
			((Control)label108).set_Name("label108");
			((Control)label108).set_Size(new Size(107, 13));
			((Control)label108).set_TabIndex(116);
			((Control)label108).set_Text("12 Camera delays");
			((Control)cmdCameraDelays).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdCameraDelays).set_Location(new Point(76, 489));
			((Control)cmdCameraDelays).set_Name("cmdCameraDelays");
			((Control)cmdCameraDelays).set_Size(new Size(64, 22));
			((Control)cmdCameraDelays).set_TabIndex(115);
			((Control)cmdCameraDelays).set_Text("Download");
			((ButtonBase)cmdCameraDelays).set_UseVisualStyleBackColor(true);
			((Control)cmdCameraDelays).add_Click((EventHandler)cmdCameraDelays_Click);
			((Control)label105).set_AutoSize(true);
			((Control)label105).set_BackColor(SystemColors.Control);
			((Control)label105).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label105).set_ForeColor(Color.DarkBlue);
			((Control)label105).set_Location(new Point(758, 220));
			((Control)label105).set_Name("label105");
			((Control)label105).set_Size(new Size(28, 13));
			((Control)label105).set_TabIndex(114);
			((Control)label105).set_Text("4kB");
			((Control)label104).set_AutoSize(true);
			((Control)label104).set_BackColor(SystemColors.Control);
			((Control)label104).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label104).set_ForeColor(Color.DarkBlue);
			((Control)label104).set_Location(new Point(744, 369));
			((Control)label104).set_Name("label104");
			((Control)label104).set_Size(new Size(42, 13));
			((Control)label104).set_TabIndex(113);
			((Control)label104).set_Text("200kB");
			((Control)label102).set_AutoSize(true);
			((Control)label102).set_BackColor(SystemColors.Control);
			((Control)label102).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label102).set_ForeColor(Color.DarkBlue);
			((Control)label102).set_Location(new Point(754, 29));
			((Control)label102).set_Name("label102");
			((Control)label102).set_Size(new Size(32, 13));
			((Control)label102).set_TabIndex(111);
			((Control)label102).set_Text("3MB");
			((Control)label101).set_AutoSize(true);
			((Control)label101).set_BackColor(SystemColors.Control);
			((Control)label101).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label101).set_ForeColor(Color.DarkBlue);
			((Control)label101).set_Location(new Point(751, 67));
			((Control)label101).set_Name("label101");
			((Control)label101).set_Size(new Size(35, 13));
			((Control)label101).set_TabIndex(110);
			((Control)label101).set_Text("39kB");
			((Control)label100).set_AutoSize(true);
			((Control)label100).set_BackColor(SystemColors.Control);
			((Control)label100).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label100).set_ForeColor(Color.DarkBlue);
			((Control)label100).set_Location(new Point(744, 105));
			((Control)label100).set_Name("label100");
			((Control)label100).set_Size(new Size(42, 13));
			((Control)label100).set_TabIndex(109);
			((Control)label100).set_Text("400kB");
			((Control)label99).set_AutoSize(true);
			((Control)label99).set_BackColor(SystemColors.Control);
			((Control)label99).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label99).set_ForeColor(Color.DarkBlue);
			((Control)label99).set_Location(new Point(758, 144));
			((Control)label99).set_Name("label99");
			((Control)label99).set_Size(new Size(28, 13));
			((Control)label99).set_TabIndex(108);
			((Control)label99).set_Text("2kB");
			((Control)label98).set_AutoSize(true);
			((Control)label98).set_BackColor(SystemColors.Control);
			((Control)label98).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label98).set_ForeColor(Color.DarkBlue);
			((Control)label98).set_Location(new Point(735, 182));
			((Control)label98).set_Name("label98");
			((Control)label98).set_Size(new Size(51, 13));
			((Control)label98).set_TabIndex(107);
			((Control)label98).set_Text("~100KB");
			((Control)label94).set_AutoSize(true);
			((Control)label94).set_BackColor(SystemColors.Control);
			((Control)label94).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label94).set_ForeColor(Color.DarkBlue);
			((Control)label94).set_Location(new Point(754, 680));
			((Control)label94).set_Name("label94");
			((Control)label94).set_Size(new Size(32, 13));
			((Control)label94).set_TabIndex(106);
			((Control)label94).set_Text("7MB");
			((Control)label93).set_AutoSize(true);
			((Control)label93).set_BackColor(SystemColors.Control);
			((Control)label93).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label93).set_ForeColor(Color.DarkBlue);
			((Control)label93).set_Location(new Point(740, 759));
			((Control)label93).set_Name("label93");
			((Control)label93).set_Size(new Size(46, 13));
			((Control)label93).set_TabIndex(105);
			((Control)label93).set_Text("950MB");
			((Control)label92).set_AutoSize(true);
			((Control)label92).set_BackColor(SystemColors.Control);
			((Control)label92).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label92).set_ForeColor(Color.DarkBlue);
			((Control)label92).set_Location(new Point(754, 803));
			((Control)label92).set_Name("label92");
			((Control)label92).set_Size(new Size(32, 13));
			((Control)label92).set_TabIndex(104);
			((Control)label92).set_Text("1MB");
			((Control)label91).set_AutoSize(true);
			((Control)label91).set_BackColor(SystemColors.Control);
			((Control)label91).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label91).set_ForeColor(Color.DarkBlue);
			((Control)label91).set_Location(new Point(743, 840));
			((Control)label91).set_Name("label91");
			((Control)label91).set_Size(new Size(43, 13));
			((Control)label91).set_TabIndex(103);
			((Control)label91).set_Text("0.6MB");
			((Control)label90).set_AutoSize(true);
			((Control)label90).set_BackColor(SystemColors.Control);
			((Control)label90).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label90).set_ForeColor(Color.DarkBlue);
			((Control)label90).set_Location(new Point(744, 296));
			((Control)label90).set_Name("label90");
			((Control)label90).set_Size(new Size(42, 13));
			((Control)label90).set_TabIndex(102);
			((Control)label90).set_Text("351kB");
			((ButtonBase)DownloadFlagVisualBinaries).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownloadFlagVisualBinaries).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownloadFlagVisualBinaries).set_ImageIndex(0);
			((ButtonBase)DownloadFlagVisualBinaries).set_ImageList(imageList1);
			((Control)DownloadFlagVisualBinaries).set_Location(new Point(61, 835));
			((Control)DownloadFlagVisualBinaries).set_Name("DownloadFlagVisualBinaries");
			((Control)DownloadFlagVisualBinaries).set_Size(new Size(14, 18));
			((Control)DownloadFlagVisualBinaries).set_TabIndex(101);
			((ButtonBase)DownloadFlagVisualBinaries).set_UseVisualStyleBackColor(true);
			((Control)DownloadFlagVisualBinaries).set_Visible(false);
			((Control)label66).set_AutoSize(true);
			((Control)label66).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label66).set_Location(new Point(304, 840));
			((Control)label66).set_Name("label66");
			((Control)label66).set_Size(new Size(213, 13));
			((Control)label66).set_TabIndex(99);
			((Control)label66).set_Text("Sixth Catalog of Orbits of Visual Binary Stars");
			((Control)label67).set_AutoSize(true);
			((Control)label67).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label67).set_Location(new Point(139, 840));
			((Control)label67).set_Name("label67");
			((Control)label67).set_Size(new Size(108, 13));
			((Control)label67).set_TabIndex(98);
			((Control)label67).set_Text("19 Visual Binaries");
			((Control)cmdDownloadVisualBinaries).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDownloadVisualBinaries).set_Location(new Point(74, 835));
			((Control)cmdDownloadVisualBinaries).set_Name("cmdDownloadVisualBinaries");
			((Control)cmdDownloadVisualBinaries).set_Size(new Size(64, 22));
			((Control)cmdDownloadVisualBinaries).set_TabIndex(97);
			((Control)cmdDownloadVisualBinaries).set_Text("Download");
			((ButtonBase)cmdDownloadVisualBinaries).set_UseVisualStyleBackColor(true);
			((Control)cmdDownloadVisualBinaries).add_Click((EventHandler)cmdDownloadVisualBinaries_Click);
			((ButtonBase)downloadFlag_CALL).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)downloadFlag_CALL).set_FlatStyle((FlatStyle)0);
			((ButtonBase)downloadFlag_CALL).set_ImageIndex(0);
			((ButtonBase)downloadFlag_CALL).set_ImageList(imageList1);
			((Control)downloadFlag_CALL).set_Location(new Point(61, 800));
			((Control)downloadFlag_CALL).set_Name("downloadFlag_CALL");
			((Control)downloadFlag_CALL).set_Size(new Size(14, 18));
			((Control)downloadFlag_CALL).set_TabIndex(96);
			((ButtonBase)downloadFlag_CALL).set_UseVisualStyleBackColor(true);
			((Control)downloadFlag_CALL).set_Visible(false);
			((Control)label64).set_AutoSize(true);
			((Control)label64).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label64).set_Location(new Point(304, 796));
			((Control)label64).set_Name("label64");
			((Control)label64).set_Size(new Size(400, 26));
			((Control)label64).set_TabIndex(95);
			((Control)label64).set_Text("Asteroid light curve database [LCDB] - summary light curve measurements, updated \r\n annually.   {Download indicator shows only if the file is older than 15 months.}");
			((Control)label63).set_AutoSize(true);
			((Control)label63).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label63).set_Location(new Point(139, 803));
			((Control)label63).set_Name("label63");
			((Control)label63).set_Size(new Size(164, 13));
			((Control)label63).set_TabIndex(94);
			((Control)label63).set_Text("18 Asteroid light curve data");
			((Control)label80).set_AutoSize(true);
			((Control)label80).set_BackColor(SystemColors.Control);
			((Control)label80).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label80).set_ForeColor(Color.DarkBlue);
			((Control)label80).set_Location(new Point(754, 606));
			((Control)label80).set_Name("label80");
			((Control)label80).set_Size(new Size(32, 13));
			((Control)label80).set_TabIndex(95);
			((Control)label80).set_Text("5MB");
			((Control)cmdDownloadAsteroidLightCurveData).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDownloadAsteroidLightCurveData).set_Location(new Point(74, 798));
			((Control)cmdDownloadAsteroidLightCurveData).set_Name("cmdDownloadAsteroidLightCurveData");
			((Control)cmdDownloadAsteroidLightCurveData).set_Size(new Size(64, 22));
			((Control)cmdDownloadAsteroidLightCurveData).set_TabIndex(92);
			((Control)cmdDownloadAsteroidLightCurveData).set_Text("Download");
			((ButtonBase)cmdDownloadAsteroidLightCurveData).set_UseVisualStyleBackColor(true);
			((Control)cmdDownloadAsteroidLightCurveData).add_Click((EventHandler)cmdDownloadAsteroidLightCurveData_Click);
			((ButtonBase)DownloadFlagAAVSO).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownloadFlagAAVSO).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownloadFlagAAVSO).set_ImageIndex(0);
			((ButtonBase)DownloadFlagAAVSO).set_ImageList(imageList1);
			((Control)DownloadFlagAAVSO).set_Location(new Point(61, 756));
			((Control)DownloadFlagAAVSO).set_Name("DownloadFlagAAVSO");
			((Control)DownloadFlagAAVSO).set_Size(new Size(14, 18));
			((Control)DownloadFlagAAVSO).set_TabIndex(90);
			((ButtonBase)DownloadFlagAAVSO).set_UseVisualStyleBackColor(true);
			((Control)DownloadFlagAAVSO).set_Visible(false);
			((Control)label49).set_AutoSize(true);
			((Control)label49).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label49).set_Location(new Point(3, 8));
			((Control)label49).set_Name("label49");
			((Control)label49).set_Size(new Size(190, 26));
			((Control)label49).set_TabIndex(88);
			((Control)label49).set_Text("From VizieR. The AAVSO Variable \r\nStar Index. Update interval : 12 months");
			((Control)label48).set_AutoSize(true);
			((Control)label48).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label48).set_Location(new Point(139, 759));
			((Control)label48).set_Name("label48");
			((Control)label48).set_Size(new Size(132, 13));
			((Control)label48).set_TabIndex(87);
			((Control)label48).set_Text("17a  AAVSO variables");
			((Control)cmdDownloadAAVSO).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDownloadAAVSO).set_Location(new Point(74, 748));
			((Control)cmdDownloadAAVSO).set_Name("cmdDownloadAAVSO");
			((Control)cmdDownloadAAVSO).set_Size(new Size(64, 35));
			((Control)cmdDownloadAAVSO).set_TabIndex(86);
			((Control)cmdDownloadAAVSO).set_Text("Download\r\n+ Convert");
			((ButtonBase)cmdDownloadAAVSO).set_UseVisualStyleBackColor(true);
			((Control)cmdDownloadAAVSO).add_Click((EventHandler)cmdDownloadAAVSO_Click);
			((ButtonBase)DownloadFlagWDS).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownloadFlagWDS).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownloadFlagWDS).set_ImageIndex(0);
			((ButtonBase)DownloadFlagWDS).set_ImageList(imageList1);
			((Control)DownloadFlagWDS).set_Location(new Point(61, 603));
			((Control)DownloadFlagWDS).set_Name("DownloadFlagWDS");
			((Control)DownloadFlagWDS).set_Size(new Size(14, 18));
			((Control)DownloadFlagWDS).set_TabIndex(85);
			((ButtonBase)DownloadFlagWDS).set_UseVisualStyleBackColor(true);
			((Control)DownloadFlagWDS).set_Visible(false);
			((ButtonBase)DownloadFlagIF).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownloadFlagIF).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownloadFlagIF).set_ImageIndex(0);
			((ButtonBase)DownloadFlagIF).set_ImageList(imageList1);
			((Control)DownloadFlagIF).set_Location(new Point(61, 677));
			((Control)DownloadFlagIF).set_Name("DownloadFlagIF");
			((Control)DownloadFlagIF).set_Size(new Size(14, 18));
			((Control)DownloadFlagIF).set_TabIndex(84);
			((ButtonBase)DownloadFlagIF).set_UseVisualStyleBackColor(true);
			((Control)DownloadFlagIF).set_Visible(false);
			((Control)label44).set_AutoSize(true);
			((Control)label44).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label44).set_Location(new Point(139, 606));
			((Control)label44).set_Name("label44");
			((Control)label44).set_Size(new Size(100, 13));
			((Control)label44).set_TabIndex(81);
			((Control)label44).set_Text("14 WDS catalog");
			((Control)label45).set_AutoSize(true);
			((Control)label45).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label45).set_Location(new Point(304, 601));
			((Control)label45).set_Name("label45");
			((Control)label45).set_Size(new Size(370, 13));
			((Control)label45).set_TabIndex(80);
			((Control)label45).set_Text("The Washington Double Star catalogue. The file is updated monthly on VizieR.");
			((Control)cmdDownloadWDS).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDownloadWDS).set_Location(new Point(74, 601));
			((Control)cmdDownloadWDS).set_Name("cmdDownloadWDS");
			((Control)cmdDownloadWDS).set_Size(new Size(64, 22));
			((Control)cmdDownloadWDS).set_TabIndex(79);
			((Control)cmdDownloadWDS).set_Text("Download");
			((ButtonBase)cmdDownloadWDS).set_UseVisualStyleBackColor(true);
			((Control)cmdDownloadWDS).add_Click((EventHandler)cmdDownloadWDS_Click);
			((Control)label42).set_AutoSize(true);
			((Control)label42).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label42).set_Location(new Point(139, 680));
			((Control)label42).set_Name("label42");
			((Control)label42).set_Size(new Size(153, 13));
			((Control)label42).set_TabIndex(78);
			((Control)label42).set_Text("16 Interferometric catalog");
			((Control)label43).set_AutoSize(true);
			((Control)label43).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Strikeout, GraphicsUnit.Point, 0));
			((Control)label43).set_Location(new Point(304, 673));
			((Control)label43).set_Name("label43");
			((Control)label43).set_Size(new Size(442, 26));
			((Control)label43).set_TabIndex(77);
			((Control)label43).set_Text("The Interferometric catalog of double stars. The file is updated daily at US Naval Observatory, \r\nbut only needs to be downloaded every few months.\r\n");
			((Control)cmdDownloadInterferometric).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDownloadInterferometric).set_Location(new Point(74, 675));
			((Control)cmdDownloadInterferometric).set_Name("cmdDownloadInterferometric");
			((Control)cmdDownloadInterferometric).set_Size(new Size(64, 22));
			((Control)cmdDownloadInterferometric).set_TabIndex(76);
			((Control)cmdDownloadInterferometric).set_Text("Download");
			((ButtonBase)cmdDownloadInterferometric).set_UseVisualStyleBackColor(true);
			((Control)cmdDownloadInterferometric).add_Click((EventHandler)cmdDownloadInterferometric_Click);
			((ButtonBase)DownloadFlagAddresses).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownloadFlagAddresses).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownloadFlagAddresses).set_ImageIndex(0);
			((ButtonBase)DownloadFlagAddresses).set_ImageList(imageList1);
			((Control)DownloadFlagAddresses).set_Location(new Point(62, 455));
			((Control)DownloadFlagAddresses).set_Name("DownloadFlagAddresses");
			((Control)DownloadFlagAddresses).set_Size(new Size(14, 18));
			((Control)DownloadFlagAddresses).set_TabIndex(64);
			((ButtonBase)DownloadFlagAddresses).set_UseVisualStyleBackColor(true);
			((Control)DownloadFlagAddresses).set_Visible(false);
			((ButtonBase)DownloadFlagRecentLunar).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownloadFlagRecentLunar).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownloadFlagRecentLunar).set_ImageIndex(0);
			((ButtonBase)DownloadFlagRecentLunar).set_ImageList(imageList1);
			((Control)DownloadFlagRecentLunar).set_Location(new Point(61, 175));
			((Control)DownloadFlagRecentLunar).set_Name("DownloadFlagRecentLunar");
			((Control)DownloadFlagRecentLunar).set_Size(new Size(14, 18));
			((Control)DownloadFlagRecentLunar).set_TabIndex(63);
			((ButtonBase)DownloadFlagRecentLunar).set_UseVisualStyleBackColor(true);
			((Control)DownloadFlagRecentLunar).set_Visible(false);
			((ButtonBase)DownloadFlagAsteroids).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownloadFlagAsteroids).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownloadFlagAsteroids).set_ImageIndex(0);
			((ButtonBase)DownloadFlagAsteroids).set_ImageList(imageList1);
			((Control)DownloadFlagAsteroids).set_Location(new Point(61, 102));
			((Control)DownloadFlagAsteroids).set_Name("DownloadFlagAsteroids");
			((Control)DownloadFlagAsteroids).set_Size(new Size(14, 18));
			((Control)DownloadFlagAsteroids).set_TabIndex(62);
			((ButtonBase)DownloadFlagAsteroids).set_UseVisualStyleBackColor(true);
			((Control)DownloadFlagAsteroids).set_Visible(false);
			((Control)lblRetainFuture).set_AutoSize(true);
			((Control)lblRetainFuture).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblRetainFuture).set_Location(new Point(304, 381));
			((Control)lblRetainFuture).set_Name("lblRetainFuture");
			((Control)lblRetainFuture).set_Size(new Size(256, 12));
			((Control)lblRetainFuture).set_TabIndex(61);
			((Control)lblRetainFuture).set_Text("[The existing Future.xml file will be retained with today's date]");
			((Control)label40).set_AutoSize(true);
			((Control)label40).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label40).set_Location(new Point(304, 410));
			((Control)label40).set_Name("label40");
			((Control)label40).set_Size(new Size(427, 13));
			((Control)label40).set_TabIndex(60);
			((Control)label40).set_Text("Download all the above files in this group (Reporting addresses must be done seperately).");
			((Control)cmdDownloadAll).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdDownloadAll).set_Location(new Point(31, 400));
			((Control)cmdDownloadAll).set_Name("cmdDownloadAll");
			((Control)cmdDownloadAll).set_Size(new Size(153, 33));
			((Control)cmdDownloadAll).set_TabIndex(59);
			((Control)cmdDownloadAll).set_Text("Download All the above");
			((ButtonBase)cmdDownloadAll).set_UseVisualStyleBackColor(true);
			((Control)cmdDownloadAll).add_Click((EventHandler)cmdDownloadAll_Click);
			((ButtonBase)DownloadFlagAsteroidDiameters).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownloadFlagAsteroidDiameters).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownloadFlagAsteroidDiameters).set_ImageIndex(0);
			((ButtonBase)DownloadFlagAsteroidDiameters).set_ImageList(imageList1);
			((Control)DownloadFlagAsteroidDiameters).set_Location(new Point(61, 293));
			((Control)DownloadFlagAsteroidDiameters).set_Name("DownloadFlagAsteroidDiameters");
			((Control)DownloadFlagAsteroidDiameters).set_Size(new Size(14, 18));
			((Control)DownloadFlagAsteroidDiameters).set_TabIndex(58);
			((ButtonBase)DownloadFlagAsteroidDiameters).set_UseVisualStyleBackColor(true);
			((Control)DownloadFlagAsteroidDiameters).set_Visible(false);
			((ButtonBase)DownloadFlagBinary).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownloadFlagBinary).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownloadFlagBinary).set_ImageIndex(0);
			((ButtonBase)DownloadFlagBinary).set_ImageList(imageList1);
			((Control)DownloadFlagBinary).set_Location(new Point(62, 217));
			((Control)DownloadFlagBinary).set_Name("DownloadFlagBinary");
			((Control)DownloadFlagBinary).set_Size(new Size(14, 18));
			((Control)DownloadFlagBinary).set_TabIndex(57);
			((ButtonBase)DownloadFlagBinary).set_UseVisualStyleBackColor(true);
			((Control)DownloadFlagBinary).set_Visible(false);
			((ButtonBase)DownloadFlagdeltaT).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)DownloadFlagdeltaT).set_FlatStyle((FlatStyle)0);
			((ButtonBase)DownloadFlagdeltaT).set_ImageIndex(0);
			((ButtonBase)DownloadFlagdeltaT).set_ImageList(imageList1);
			((Control)DownloadFlagdeltaT).set_Location(new Point(61, 141));
			((Control)DownloadFlagdeltaT).set_Name("DownloadFlagdeltaT");
			((Control)DownloadFlagdeltaT).set_Size(new Size(14, 18));
			((Control)DownloadFlagdeltaT).set_TabIndex(56);
			((ButtonBase)DownloadFlagdeltaT).set_UseVisualStyleBackColor(true);
			((Control)DownloadFlagdeltaT).set_Visible(false);
			((Control)label37).set_AutoSize(true);
			((Control)label37).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label37).set_Location(new Point(304, 296));
			((Control)label37).set_Name("label37");
			((Control)label37).set_Size(new Size(295, 13));
			((Control)label37).set_TabIndex(54);
			((Control)label37).set_Text("Diameters of asteroids - for asteroid predictions. Update yearly");
			((Control)label38).set_AutoSize(true);
			((Control)label38).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label38).set_Location(new Point(140, 296));
			((Control)label38).set_Name("label38");
			((Control)label38).set_Size(new Size(122, 13));
			((Control)label38).set_TabIndex(53);
			((Control)label38).set_Text("8 Asteroid diameters");
			((Control)cmdAsteroidDiameters).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdAsteroidDiameters).set_Location(new Point(75, 291));
			((Control)cmdAsteroidDiameters).set_Name("cmdAsteroidDiameters");
			((Control)cmdAsteroidDiameters).set_Size(new Size(64, 22));
			((Control)cmdAsteroidDiameters).set_TabIndex(52);
			((Control)cmdAsteroidDiameters).set_Text("Download");
			((ButtonBase)cmdAsteroidDiameters).set_UseVisualStyleBackColor(true);
			((Control)cmdAsteroidDiameters).add_Click((EventHandler)cmdAsteroidDiameters_Click);
			((Control)label35).set_AutoSize(true);
			((Control)label35).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label35).set_Location(new Point(304, 220));
			((Control)label35).set_Name("label35");
			((Control)label35).set_Size(new Size(362, 13));
			((Control)label35).set_TabIndex(50);
			((Control)label35).set_Text("Details of binary asteroids - for asteroid predictions. Update every six months");
			((Control)label36).set_AutoSize(true);
			((Control)label36).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label36).set_Location(new Point(140, 220));
			((Control)label36).set_Name("label36");
			((Control)label36).set_Size(new Size(108, 13));
			((Control)label36).set_TabIndex(49);
			((Control)label36).set_Text("6 Binary asteroids");
			((Control)cmdBinary).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdBinary).set_Location(new Point(75, 215));
			((Control)cmdBinary).set_Name("cmdBinary");
			((Control)cmdBinary).set_Size(new Size(64, 22));
			((Control)cmdBinary).set_TabIndex(48);
			((Control)cmdBinary).set_Text("Download");
			((ButtonBase)cmdBinary).set_UseVisualStyleBackColor(true);
			((Control)cmdBinary).add_Click((EventHandler)cmdBinary_Click);
			((Control)label28).set_AutoSize(true);
			((Control)label28).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label28).set_Location(new Point(304, 137));
			((Control)label28).set_Name("label28");
			((Control)label28).set_Size(new Size(448, 26));
			((Control)label28).set_TabIndex(41);
			((Control)label28).set_Text("File containing the difference between UTC and terrestrial time (generally referred to as deltaT). \r\nUpdates are irregular. Download each year around 1 March and 1 September.");
			((Control)label29).set_AutoSize(true);
			((Control)label29).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label29).set_Location(new Point(140, 144));
			((Control)label29).set_Name("label29");
			((Control)label29).set_Size(new Size(92, 13));
			((Control)label29).set_TabIndex(40);
			((Control)label29).set_Text("4 deltaT tables");
			((Control)cmdDeltaT).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDeltaT).set_Location(new Point(75, 139));
			((Control)cmdDeltaT).set_Name("cmdDeltaT");
			((Control)cmdDeltaT).set_Size(new Size(64, 22));
			((Control)cmdDeltaT).set_TabIndex(39);
			((Control)cmdDeltaT).set_Text("Download");
			((ButtonBase)cmdDeltaT).set_UseVisualStyleBackColor(true);
			((Control)cmdDeltaT).add_Click((EventHandler)cmdDeltaT_Click);
			((Control)label26).set_AutoSize(true);
			((Control)label26).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label26).set_Location(new Point(304, 453));
			((Control)label26).set_Name("label26");
			((Control)label26).set_Size(new Size(437, 26));
			((Control)label26).set_TabIndex(38);
			((Control)label26).set_Text("Lunar occultation reports, and Light Curve reports, need to be emailed to certain addresses. \r\nThis file is automatically updated at start-up, if you are connected to the internet.");
			((Control)label27).set_AutoSize(true);
			((Control)label27).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label27).set_Location(new Point(140, 453));
			((Control)label27).set_Name("label27");
			((Control)label27).set_Size(new Size(145, 26));
			((Control)label27).set_TabIndex(37);
			((Control)label27).set_Text("11 Reporting addresses \r\n     for occultations\r\n");
			((Control)cmdAddresses).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdAddresses).set_Location(new Point(75, 455));
			((Control)cmdAddresses).set_Name("cmdAddresses");
			((Control)cmdAddresses).set_Size(new Size(64, 22));
			((Control)cmdAddresses).set_TabIndex(36);
			((Control)cmdAddresses).set_Text("Download");
			((ButtonBase)cmdAddresses).set_UseVisualStyleBackColor(true);
			((Control)cmdAddresses).add_Click((EventHandler)cmdAddresses_Click);
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_BackColor(Color.Gold);
			label25.set_BorderStyle((BorderStyle)1);
			((Control)label25).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label25).set_Location(new Point(304, 35));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(295, 15));
			((Control)label25).set_TabIndex(35);
			((Control)label25).set_Text("For accurate lunar reductions, this should be updated weekly.");
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label24).set_Location(new Point(304, 22));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(379, 13));
			((Control)label24).set_TabIndex(34);
			((Control)label24).set_Text("Earth Orientation Parameters, giving daily values of UT1-UTC, and polar motion. ");
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label23).set_Location(new Point(304, 67));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(299, 13));
			((Control)label23).set_TabIndex(33);
			((Control)label23).set_Text("Orbital elements of currently visible comets. Update as required");
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label22).set_Location(new Point(304, 98));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(406, 26));
			((Control)label22).set_TabIndex(32);
			((Control)label22).set_Text("File containing the observations of asteroid occultations. It is usually updated monthly.\r\nDownload to access the latest observations.");
			((Control)cmdHelp).set_Location(new Point(633, 7));
			((Control)cmdHelp).set_Name("cmdHelp");
			((Control)cmdHelp).set_Size(new Size(45, 22));
			((Control)cmdHelp).set_TabIndex(35);
			((Control)cmdHelp).set_Text("Help");
			((ButtonBase)cmdHelp).set_UseVisualStyleBackColor(true);
			((Control)cmdHelp).add_Click((EventHandler)cmdHelp_Click);
			((Control)cmdExit).set_Location(new Point(730, 7));
			((Control)cmdExit).set_Name("cmdExit");
			((Control)cmdExit).set_Size(new Size(45, 22));
			((Control)cmdExit).set_TabIndex(36);
			((Control)cmdExit).set_Text("Exit");
			((ButtonBase)cmdExit).set_UseVisualStyleBackColor(true);
			((Control)cmdExit).add_Click((EventHandler)cmdExit_Click);
			((ScrollableControl)panel1).set_AutoScroll(true);
			((ScrollableControl)panel1).set_AutoScrollMargin(new Size(0, 10));
			((ScrollableControl)panel1).set_AutoScrollMinSize(new Size(600, 500));
			panel1.set_AutoSizeMode((AutoSizeMode)0);
			((Control)panel1).get_Controls().Add((Control)(object)grpAsteroids);
			((Control)panel1).get_Controls().Add((Control)(object)grpGeneral);
			((Control)panel1).get_Controls().Add((Control)(object)grpLargeButSeldom);
			((Control)panel1).set_Location(new Point(0, 32));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(830, 686));
			((Control)panel1).set_TabIndex(37);
			((Control)lblUpdateCount).set_AutoSize(true);
			((Control)lblUpdateCount).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblUpdateCount).set_ForeColor(Color.Gold);
			((Control)lblUpdateCount).set_Location(new Point(13, 2));
			((Control)lblUpdateCount).set_Name("lblUpdateCount");
			((Control)lblUpdateCount).set_Size(new Size(235, 17));
			((Control)lblUpdateCount).set_TabIndex(38);
			((Control)lblUpdateCount).set_Text("0 items tagged for downloading");
			((Control)panel2).set_BackColor(Color.Navy);
			((Control)panel2).get_Controls().Add((Control)(object)lblUpdateCount);
			((Control)panel2).set_Location(new Point(14, 7));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(271, 22));
			((Control)panel2).set_TabIndex(39);
			((Control)lblUpdateDamitPreferred).set_AutoSize(true);
			((Control)lblUpdateDamitPreferred).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblUpdateDamitPreferred).set_ForeColor(Color.DarkGreen);
			((Control)lblUpdateDamitPreferred).set_Location(new Point(114, 236));
			((Control)lblUpdateDamitPreferred).set_Name("lblUpdateDamitPreferred");
			((Control)lblUpdateDamitPreferred).set_Size(new Size(138, 26));
			((Control)lblUpdateDamitPreferred).set_TabIndex(164);
			((Control)lblUpdateDamitPreferred).set_Text("Use this to update your\r\nshape model files");
			((Control)panel5).set_BackColor(Color.SeaShell);
			panel5.set_BorderStyle((BorderStyle)2);
			((Control)panel5).get_Controls().Add((Control)(object)label157);
			((Control)panel5).get_Controls().Add((Control)(object)updnMaxMagLimit);
			((Control)panel5).get_Controls().Add((Control)(object)label156);
			((Control)panel5).get_Controls().Add((Control)(object)updnMagLimitAAVSO);
			((Control)panel5).get_Controls().Add((Control)(object)label49);
			((Control)panel5).set_Location(new Point(300, 745));
			((Control)panel5).set_Name("panel5");
			((Control)panel5).set_Size(new Size(439, 47));
			((Control)panel5).set_TabIndex(175);
			((Control)label163).set_AutoSize(true);
			((Control)label163).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label163).set_ForeColor(Color.DarkGreen);
			((Control)label163).set_Location(new Point(445, 724));
			((Control)label163).set_Name("label163");
			((Control)label163).set_Size(new Size(229, 13));
			((Control)label163).set_TabIndex(176);
			((Control)label163).set_Text("Recommended download for most users");
			((Control)label165).set_AutoSize(true);
			((Control)label165).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label165).set_ForeColor(Color.DarkGreen);
			((Control)label165).set_Location(new Point(168, 771));
			((Control)label165).set_Name("label165");
			((Control)label165).set_Size(new Size(113, 13));
			((Control)label165).set_TabIndex(178);
			((Control)label165).set_Text("Set your own limits");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(829, 722));
			((Control)this).get_Controls().Add((Control)(object)panel2);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)cmdExit);
			((Control)this).get_Controls().Add((Control)(object)cmdHelp);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationFilesDownloads", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationFilesDownloads);
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("Downloads");
			((Control)this).set_Text("Downloads ::         General downloads,  Files for asteroid predictions,  Static data files");
			((Form)this).add_FormClosed(new FormClosedEventHandler(Downloads_FormClosed));
			((Form)this).add_Load((EventHandler)Downloads_Load);
			((Control)this).add_Resize((EventHandler)Downloads_Resize);
			((Control)grpLargeButSeldom).ResumeLayout(false);
			((Control)grpLargeButSeldom).PerformLayout();
			((Control)grpAsteroids).ResumeLayout(false);
			((Control)grpAsteroids).PerformLayout();
			((Control)panel4).ResumeLayout(false);
			((Control)panel4).PerformLayout();
			((Control)grpGeneral).ResumeLayout(false);
			((Control)grpGeneral).PerformLayout();
			((ISupportInitialize)updnMaxMagLimit).EndInit();
			((ISupportInitialize)updnMagLimitAAVSO).EndInit();
			((Control)panel3).ResumeLayout(false);
			((Control)panel3).PerformLayout();
			((Control)panel1).ResumeLayout(false);
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((Control)panel5).ResumeLayout(false);
			((Control)panel5).PerformLayout();
			((Control)this).ResumeLayout(false);
		}
	}
}
