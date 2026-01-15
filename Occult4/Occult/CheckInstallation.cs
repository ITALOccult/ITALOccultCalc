using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class CheckInstallation : Form
	{
		private static string[] ResourceFiles_Essential = new string[81]
		{
			"addresses.txt", "AsteroidDiameters.bin", "AsteroidDiametersAll.bin", "AsteroidElements.csv", "AsteroidRings.csv", "AsteroidsAlwaysRead.dat", "Bayer.bin", "BinaryAsteroids.csv", "CameraDelays.dat", "Constellation Abbrevs.dat",
			"Constellation id.dat", "Constellation Lines EW.dat", "Constellation Lines NS.dat", "Constellation Names.dat", "Crater.bin", "CRATER.DAT", "CraterTimings.bin", "DeltaT.dat", "DeltaTA.dat", "Earth.bin",
			"EOP_old.dat", "EOP_present.dat", "GeoidHeights.bin", "GoogleMap_FileFooter.txt", "GoogleMap_FileHeader.txt", "GoogleMap_ServerDriver_2.js", "GSC Fields.dat", "Hipparcos Index.dat", "J6.bin", "J7.bin",
			"J8.bin", "J9.bin", "J10.bin", "J11.bin", "J12.bin", "J13.bin", "Jupiter.bin", "Lunar Reduction Email body.txt", "MARIA.DAT", "Mars.bin",
			"Mercury.bin", "MoonSeries.bin", "Neptune.bin", "Nutation.bin", "Pluto.bin", "sao_xz.dat", "Saturn.bin", "SaturnIX.bin", "StarDia.bin", "Tycho2.bin",
			"tycho2.inx", "U1.bin", "U2.bin", "U3.bin", "U4.bin", "U5.bin", "UCAC_BSS.bin", "UCAC_BSS.inx", "UCAC2 Index.dat", "Uranus.bin",
			"UserMinorPlanetElements.csv", "UserMinorPlanetElements_Lunar.csv", "UserStar_MinorPlanetSearch.dat", "Venus.bin", "WDS Discovery Codes.dat", "World.bin", "WORLD.INX", "xz_zc.dat", "xz80.dat", "xz80.inx",
			"XZ80index.dat", "XZConfirmations.DAT", "XZDoubles Discoveries.dat", "XZDoubles.dat", "XZinKepler2.dat", "XZNegatives.dat", "XZVariables.dat", "ZC.dat", "ZC.inx", "ZCNames.dat",
			"zc-xz.dat"
		};

		private static string[] ResourceFiles_XZ = new string[6] { "XZ80Mag4.dat", "XZ80Mag4.inx", "XZ80Mag7.dat", "XZ80Mag7.inx", "XZ80Mag9.dat", "XZ80Mag9.inx" };

		private static string[] ResourceFiles_DE = new string[2] { "DE_Ephemeris.bin", "DE_LongEphemeris.bin" };

		private static string[] ResourceFiles_Lunar = new string[13]
		{
			"LOLA128.bin", "RArchive Observations 1600_1949.dat", "RArchive Observations 1950_1969.dat", "RArchive Observations 1970_1975.dat", "RArchive Observations 1976_1980.dat", "RArchive Observations 1981_1985.dat", "RArchive Observations 1986_1989.dat", "RArchive Observations 1990_1994.dat", "RArchive Observations 1995_1999.dat", "RArchive Observations 2000_2005.dat",
			"RArchive Observations 2006_2015.dat", "RArchive Observations 2016_2025.dat", "RArchive Observations recent.dat"
		};

		private static string[] ResourceFiles_Observations = new string[18]
		{
			"Archive Observations 1600_1949.dat", "Archive Observations 1950_1969.dat", "Archive Observations 1970_1975.dat", "Archive Observations 1976_1980.dat", "Archive Observations 1981_1985.dat", "Archive Observations 1986_1989.dat", "Archive Observations 1990_1994.dat", "Archive Observations 1995_1999.dat", "Archive Observations 2000_2005.dat", "Archive Observations 2006_2015.dat",
			"Archive Observations 2016_2025.dat", "Archive Observations recent.dat", "Archive Graze Index.dat", "Archive ILOC Sites.dat", "Archive ILOC Observers.dat", "Archive RGO Sites.dat", "ILOC Site Statistics.dat", "RGO Site Statistics.dat"
		};

		private static string[] ResourceFiles_XYZ = new string[1] { "2450000_XYZ.bin" };

		private static string[] ResourceFiles_User = new string[6] { "UserA.bin", "UserB.bin", "UserC.bin", "UserD.bin", "UserE.bin", "UserF.bin" };

		private static string[] ShapeFiles = new string[1] { "ISAM_AvailableAsteroids.csv" };

		private IContainer components;

		private ListBox lstEssential;

		private Label label1;

		private ListBox lstLunar;

		private Label label10;

		private Label lblXZ;

		private Label lblDE;

		private Label label2;

		private ListBox lstObserved;

		private Label lblXYZ;

		private Label lblHistory;

		private Label label4;

		private ListBox lstUser;

		private Label label5;

		private Button cmdHelp;

		private Label label3;

		private ListBox lstSites;

		private Label label6;

		private Label txtAAVSO;

		private Label txt4Int;

		private Label txtWDS;

		private Label lbl1a;

		private Label lbl1c;

		private Label lbl1b;

		private Panel pnlEssential;

		public CheckInstallation()
		{
			InitializeComponent();
		}

		private void CheckInstallation_Load(object sender, EventArgs e)
		{
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			bool flag = true;
			lstEssential.get_Items().Clear();
			int upperBound = ResourceFiles_Essential.GetUpperBound(0);
			for (int i = 0; i <= upperBound; i++)
			{
				if (!File.Exists(Utilities.AppPath + "\\Resource Files\\" + ResourceFiles_Essential[i]))
				{
					lstEssential.get_Items().Add((object)ResourceFiles_Essential[i]);
				}
			}
			for (int j = 0; j < ShapeFiles.Length; j++)
			{
				if (!File.Exists(Utilities.AppPath + "\\ShapeModels\\" + ShapeFiles[j]))
				{
					lstEssential.get_Items().Add((object)("Shape file" + ShapeFiles[j]));
				}
			}
			if (lstEssential.get_Items().get_Count() < 1)
			{
				lstEssential.get_Items().Add((object)"All essential files are present");
				((Control)pnlEssential).set_Visible(false);
			}
			else
			{
				((Control)pnlEssential).set_Visible(true);
			}
			lstLunar.get_Items().Clear();
			upperBound = ResourceFiles_Lunar.GetUpperBound(0);
			for (int k = 0; k <= upperBound; k++)
			{
				if (!File.Exists(Utilities.AppPath + "\\Resource Files\\" + ResourceFiles_Lunar[k]))
				{
					lstLunar.get_Items().Add((object)ResourceFiles_Lunar[k]);
				}
			}
			if (lstLunar.get_Items().get_Count() < 1)
			{
				lstLunar.get_Items().Add((object)"All optional files of lunar occultation residuals are present");
			}
			lstObserved.get_Items().Clear();
			upperBound = ResourceFiles_Observations.GetUpperBound(0);
			for (int l = 0; l <= upperBound; l++)
			{
				if (!File.Exists(Utilities.AppPath + "\\Resource Files\\" + ResourceFiles_Observations[l]))
				{
					lstObserved.get_Items().Add((object)ResourceFiles_Observations[l]);
				}
			}
			if (lstObserved.get_Items().get_Count() < 1)
			{
				lstObserved.get_Items().Add((object)"All optional files of lunar occultation observations are present");
			}
			lstUser.get_Items().Clear();
			upperBound = ResourceFiles_User.GetUpperBound(0);
			for (int m = 0; m <= upperBound; m++)
			{
				if (File.Exists(Utilities.AppPath + "\\Resource Files\\" + ResourceFiles_User[m]))
				{
					lstUser.get_Items().Add((object)ResourceFiles_User[m]);
				}
			}
			if (lstUser.get_Items().get_Count() < 1)
			{
				lstUser.get_Items().Add((object)"No optional USER files are present");
			}
			upperBound = ResourceFiles_XZ.GetUpperBound(0);
			for (int n = 0; n <= upperBound; n++)
			{
				flag &= File.Exists(Utilities.AppPath + "\\Resource Files\\" + ResourceFiles_XZ[n]);
			}
			if (flag)
			{
				((Control)lblXZ).set_Text("XZ catalogue subsets are OK");
			}
			else
			{
				((Control)lblXZ).set_Text("XZ catalogue subsets have not been created. To do this, select the Maintain XZ catalogue button");
			}
			if (File.Exists(Utilities.AppPath + "\\Resource Files\\" + ResourceFiles_DE[0]))
			{
				((Control)lblDE).set_Text("Planetary ephemeris (DE405_413) present, version " + Utilities.EphemerisBasis());
			}
			else if (File.Exists(Utilities.AppPath + "\\Resource Files\\" + ResourceFiles_DE[1]))
			{
				((Control)lblDE).set_Text("You have the  JPL DE405  planetary ephemeris. For improved precision you should download the  DE405_413.bin  file");
			}
			else
			{
				((Control)lblDE).set_Text("The planetary ephemeris is the basic version (VSOP87). For improved precision you should download the  DE405_413.bin  file");
			}
			((Control)lblHistory).set_Visible(!File.Exists(Utilities.AppPath + "\\Resource Files\\Asteroid_Observations.dat"));
			((Control)lblXYZ).set_Visible(!File.Exists(Utilities.AppPath + "\\Resource Files\\" + ResourceFiles_XYZ[0]));
			lstSites.get_Items().Clear();
			string[] files = Directory.GetFiles(Utilities.AppPath + "\\Sites", "*.site");
			foreach (string path in files)
			{
				lstSites.get_Items().Add((object)Path.GetFileName(path));
			}
			if (lstSites.get_Items().get_Count() < 0)
			{
				lstSites.get_Items().Add((object)"No site files are available");
			}
			((Control)txtWDS).set_Visible(!File.Exists(Utilities.AppPath + "\\Downloaded files\\wds.dat"));
			((Control)txt4Int).set_Visible(!File.Exists(Utilities.AppPath + "\\Downloaded files\\InterferometricCat.dat"));
			((Control)txtAAVSO).set_Visible(!File.Exists(Utilities.AppPath + "\\Downloaded files\\AAVSOindex.dat"));
		}

		private void CheckInstallation_FormClosed(object sender, FormClosedEventArgs e)
		{
			((Component)this).Dispose();
		}

		private void cmdHelp_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Check installation");
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
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_001b: Expected O, but got Unknown
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
			//IL_0dc5: Unknown result type (might be due to invalid IL or missing references)
			//IL_0dcf: Expected O, but got Unknown
			//IL_0e15: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e1f: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(CheckInstallation));
			lstEssential = new ListBox();
			label1 = new Label();
			lstLunar = new ListBox();
			label10 = new Label();
			lblXZ = new Label();
			lblDE = new Label();
			label2 = new Label();
			lstObserved = new ListBox();
			lblXYZ = new Label();
			lblHistory = new Label();
			label4 = new Label();
			lstUser = new ListBox();
			label5 = new Label();
			cmdHelp = new Button();
			label3 = new Label();
			lstSites = new ListBox();
			label6 = new Label();
			txtAAVSO = new Label();
			txt4Int = new Label();
			txtWDS = new Label();
			lbl1a = new Label();
			lbl1c = new Label();
			lbl1b = new Label();
			pnlEssential = new Panel();
			((Control)pnlEssential).SuspendLayout();
			((Control)this).SuspendLayout();
			((ListControl)lstEssential).set_FormattingEnabled(true);
			((Control)lstEssential).set_Location(new Point(10, 36));
			((Control)lstEssential).set_Name("lstEssential");
			((Control)lstEssential).set_Size(new Size(454, 56));
			((Control)lstEssential).set_TabIndex(0);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(10, 20));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(223, 13));
			((Control)label1).set_TabIndex(1);
			((Control)label1).set_Text("Any files listed here are essential for OCCULT.");
			((ListControl)lstLunar).set_FormattingEnabled(true);
			((Control)lstLunar).set_Location(new Point(10, 127));
			((Control)lstLunar).set_Name("lstLunar");
			((Control)lstLunar).set_Size(new Size(454, 69));
			((Control)lstLunar).set_TabIndex(2);
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(7, 111));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(653, 13));
			((Control)label10).set_TabIndex(3);
			((Control)label10).set_Text("The non-essential files listed here are used to plot graze profiles. If files are listed here, graze profiles will not have all available data plotted");
			((Control)lblXZ).set_AutoSize(true);
			((Control)lblXZ).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblXZ).set_Location(new Point(7, 408));
			((Control)lblXZ).set_Name("lblXZ");
			((Control)lblXZ).set_Size(new Size(173, 13));
			((Control)lblXZ).set_TabIndex(4);
			((Control)lblXZ).set_Text("XZ catalogue subsets are OK");
			((Control)lblDE).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblDE).set_Location(new Point(7, 434));
			((Control)lblDE).set_Name("lblDE");
			((Control)lblDE).set_Size(new Size(457, 29));
			((Control)lblDE).set_TabIndex(5);
			((Control)lblDE).set_Text("Optimal planetary ephemeris (DE413) present");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(7, 210));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(751, 13));
			((Control)label2).set_TabIndex(7);
			((Control)label2).set_Text("The optional files listed here contain observed data for lunar grazes and occultations since 1623. They are needed if you want to investigate past observations");
			((ListControl)lstObserved).set_FormattingEnabled(true);
			((Control)lstObserved).set_Location(new Point(10, 226));
			((Control)lstObserved).set_Name("lstObserved");
			((Control)lstObserved).set_Size(new Size(454, 69));
			((Control)lstObserved).set_TabIndex(6);
			((Control)lblXYZ).set_AutoSize(true);
			((Control)lblXYZ).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblXYZ).set_Location(new Point(7, 492));
			((Control)lblXYZ).set_Name("lblXYZ");
			((Control)lblXYZ).set_Size(new Size(406, 52));
			((Control)lblXYZ).set_TabIndex(8);
			((Control)lblXYZ).set_Text(componentResourceManager.GetString("lblXYZ.Text"));
			((Control)lblHistory).set_AutoSize(true);
			((Control)lblHistory).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblHistory).set_Location(new Point(7, 463));
			((Control)lblHistory).set_Name("lblHistory");
			((Control)lblHistory).set_Size(new Size(276, 13));
			((Control)lblHistory).set_TabIndex(10);
			((Control)lblHistory).set_Text("Historical file of asteroid occultations is missing");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(7, 312));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(460, 13));
			((Control)label4).set_TabIndex(12);
			((Control)label4).set_Text("The following optional USER star catalogues are available for searching for asteroid occultations");
			((ListControl)lstUser).set_FormattingEnabled(true);
			((Control)lstUser).set_Location(new Point(10, 328));
			((Control)lstUser).set_Name("lstUser");
			((Control)lstUser).set_Size(new Size(454, 69));
			((Control)lstUser).set_TabIndex(11);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(501, 328));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(236, 26));
			((Control)label5).set_TabIndex(13);
			((Control)label5).set_Text("The possible catalogues are:\r\n* UserA, UserB, UserC, UserD, UserE and UserF");
			label5.set_TextAlign(ContentAlignment.BottomLeft);
			((Control)cmdHelp).set_Location(new Point(360, 4));
			((Control)cmdHelp).set_Name("cmdHelp");
			((Control)cmdHelp).set_Size(new Size(64, 26));
			((Control)cmdHelp).set_TabIndex(14);
			((Control)cmdHelp).set_Text("Help");
			((ButtonBase)cmdHelp).set_UseVisualStyleBackColor(true);
			((Control)cmdHelp).add_Click((EventHandler)cmdHelp_Click);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(501, 408));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(262, 13));
			((Control)label3).set_TabIndex(15);
			((Control)label3).set_Text("The following Site files are present [ in ...Occult\\Sites ]");
			label3.set_TextAlign(ContentAlignment.BottomLeft);
			((ListControl)lstSites).set_FormattingEnabled(true);
			((Control)lstSites).set_Location(new Point(501, 424));
			((Control)lstSites).set_Name("lstSites");
			((Control)lstSites).set_Size(new Size(206, 186));
			((Control)lstSites).set_TabIndex(16);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(480, 127));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(240, 52));
			((Control)label6).set_TabIndex(17);
			((Control)label6).set_Text("Note: These files may be downloaded from the \r\nIOTA site. But if you have the corresponding files \r\nof observed data, you can generate these files \r\nfrom the Lunar Observations tab.");
			label6.set_TextAlign(ContentAlignment.BottomLeft);
			((Control)txtAAVSO).set_AutoSize(true);
			((Control)txtAAVSO).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)txtAAVSO).set_Location(new Point(7, 585));
			((Control)txtAAVSO).set_Name("txtAAVSO");
			((Control)txtAAVSO).set_Size(new Size(370, 13));
			((Control)txtAAVSO).set_TabIndex(18);
			((Control)txtAAVSO).set_Text("- optional AAVSO list of variable stars has not been downloaded");
			((Control)txt4Int).set_AutoSize(true);
			((Control)txt4Int).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)txt4Int).set_Location(new Point(7, 572));
			((Control)txt4Int).set_Name("txt4Int");
			((Control)txt4Int).set_Size(new Size(370, 13));
			((Control)txt4Int).set_TabIndex(19);
			((Control)txt4Int).set_Text("- optional '4th Interferometric catalog' has not been downloaded");
			((Control)txtWDS).set_AutoSize(true);
			((Control)txtWDS).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)txtWDS).set_Location(new Point(7, 559));
			((Control)txtWDS).set_Name("txtWDS");
			((Control)txtWDS).set_Size(new Size(412, 13));
			((Control)txtWDS).set_TabIndex(20);
			((Control)txtWDS).set_Text("- optional Washington Double Star catalogue has not been downloaded");
			((Control)lbl1a).set_AutoSize(true);
			((Control)lbl1a).set_ForeColor(Color.Red);
			((Control)lbl1a).set_Location(new Point(1, 4));
			((Control)lbl1a).set_Name("lbl1a");
			((Control)lbl1a).set_Size(new Size(242, 26));
			((Control)lbl1a).set_TabIndex(21);
			((Control)lbl1a).set_Text("You can re-install ALL essential data files by going\r\nto the Downloads page, last item :");
			lbl1a.set_TextAlign(ContentAlignment.BottomLeft);
			((Control)lbl1c).set_AutoSize(true);
			((Control)lbl1c).set_ForeColor(Color.Red);
			((Control)lbl1c).set_Location(new Point(1, 43));
			((Control)lbl1c).set_Name("lbl1c");
			((Control)lbl1c).set_Size(new Size(211, 13));
			((Control)lbl1c).set_TabIndex(22);
			((Control)lbl1c).set_Text("and clicking the 'Download && install' button.");
			lbl1c.set_TextAlign(ContentAlignment.BottomLeft);
			((Control)lbl1b).set_AutoSize(true);
			((Control)lbl1b).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lbl1b).set_ForeColor(Color.Red);
			((Control)lbl1b).set_Location(new Point(1, 30));
			((Control)lbl1b).set_Name("lbl1b");
			((Control)lbl1b).set_Size(new Size(143, 13));
			((Control)lbl1b).set_TabIndex(23);
			((Control)lbl1b).set_Text("00 Installation data files");
			lbl1b.set_TextAlign(ContentAlignment.BottomLeft);
			((Control)pnlEssential).get_Controls().Add((Control)(object)lbl1b);
			((Control)pnlEssential).get_Controls().Add((Control)(object)lbl1c);
			((Control)pnlEssential).get_Controls().Add((Control)(object)lbl1a);
			((Control)pnlEssential).set_Location(new Point(483, 33));
			((Control)pnlEssential).set_Name("pnlEssential");
			((Control)pnlEssential).set_Size(new Size(266, 59));
			((Control)pnlEssential).set_TabIndex(24);
			((Control)pnlEssential).set_Visible(false);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(784, 615));
			((Control)this).get_Controls().Add((Control)(object)pnlEssential);
			((Control)this).get_Controls().Add((Control)(object)txtWDS);
			((Control)this).get_Controls().Add((Control)(object)txt4Int);
			((Control)this).get_Controls().Add((Control)(object)txtAAVSO);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)lstSites);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)cmdHelp);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)lstUser);
			((Control)this).get_Controls().Add((Control)(object)lblHistory);
			((Control)this).get_Controls().Add((Control)(object)lblXYZ);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)lstObserved);
			((Control)this).get_Controls().Add((Control)(object)lblDE);
			((Control)this).get_Controls().Add((Control)(object)lblXZ);
			((Control)this).get_Controls().Add((Control)(object)label10);
			((Control)this).get_Controls().Add((Control)(object)lstLunar);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)lstEssential);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationDefaultCheck", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationDefaultCheck);
			((Control)this).set_Name("CheckInstallation");
			((Control)this).set_Text("Check Installation - report of any missing files");
			((Form)this).add_FormClosed(new FormClosedEventHandler(CheckInstallation_FormClosed));
			((Form)this).add_Load((EventHandler)CheckInstallation_Load);
			((Control)pnlEssential).ResumeLayout(false);
			((Control)pnlEssential).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
