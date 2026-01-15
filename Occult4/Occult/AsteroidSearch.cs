using System;
using System.ComponentModel;
using System.Drawing;
using System.Globalization;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Occult.Asteroids;
using Occult.Defaults___Settings;
using Occult.File_Actions;
using Occult.Properties;
using Occult.Star_Catalogues;

namespace Occult
{
	public class AsteroidSearch : Form
	{
		private readonly string AppPath;

		private bool GaiaPresent;

		private bool Tycho2Present;

		private bool UCAC4Present;

		private bool PPMXLPresent;

		private bool UserStarCataloguePresent;

		internal bool SearchCancelled;

		private bool Booting;

		private string UCAC4_Path;

		private string PPMXL_Path;

		private string XYZFileVersion;

		internal int FirstRecord;

		internal int LastRecord;

		internal int FirstNumber;

		internal int LastNumber;

		internal string FirstName = "";

		internal string LastName = "";

		private const int ElementFileLength = 145;

		private const double Radian = 180.0 / Math.PI;

		private bool ChangingMonth;

		private bool HaveGotHorizons;

		private bool AutoHorizonsJustChanged = true;

		private IContainer components;

		private Panel panel3;

		private Label label2;

		private Label label1;

		public Button cmdCompute;

		private Button cmdCancel;

		public ListBox lstResults;

		public ProgressBar pbarSearch;

		private Button cmdThisYear;

		private Button cmdNextYear;

		private Button cmdWholeYear;

		private Button cmdWholeMonth;

		private Button cmd2yrs;

		public Label lblAsteroidName;

		private GroupBox grpSetStarCat;

		private RadioButton optUserCats;

		private RadioButton optTycho2;

		private TextBox txtOutputFile;

		private Button cmdSetOutputFile;

		private GroupBox grpSetDateRange;

		private GroupBox grpSetSaveFile;

		private GroupBox groupBox4;

		private Button cmdJD239;

		private Button cmdJD248;

		private Button cmdJD247;

		private Button cmdJD246;

		private Button cmdJD245;

		private Button cmdJD244;

		private Button cmdJD243;

		private Button cmdJD242;

		private Button cmdJD241;

		private Button cmdJD240;

		private NumericUpDown updnMag;

		private NumericUpDown updnDiameter;

		private Label label5;

		private Button cmdSelectAsteroid;

		private Label lblEphemerisSource;

		private CheckBox chkIncludeSmallMoons;

		private CheckBox chkIncludeMainMoons;

		private ComboBox cmbPlanets;

		private GroupBox grpSetAsteroidsPlanets;

		private RadioButton optPlanets;

		private RadioButton optMinorPlanets;

		private GroupBox grpSearch;

		public Label lblSearchRange;

		private RadioButton optUserStar;

		private CheckBox chkApplyMagLimit;

		private Label lblMiss1;

		private Label lblMiss2;

		private NumericUpDown updnMissDistance;

		private Label lblTotalObjects;

		private Button cmdEditUserStar;

		private Label lblMoons;

		private Label lblUserStar;

		private TextBox txtPlanetMagLimit;

		internal CheckBox chkPlanetMagLimit;

		public Label label6;

		private Panel panelAdjust;

		private Label lblAdjust;

		private Label label9;

		private ComboBox cmbUserCat;

		private Label lblElementsDate;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private RadioButton optUCAC4;

		private RadioButton optPPMXL;

		private NumericUpDown updnDecCorrn;

		private NumericUpDown updnRACorrn;

		private ToolStripMenuItem setDataForSearchIntervalToolStripMenuItem;

		private ToolStripMenuItem mnuJD242;

		private ToolStripMenuItem mnuJD243;

		private ToolStripMenuItem mnuJD244;

		private ToolStripMenuItem mnuJD245;

		private ToolStripMenuItem mnuJD246;

		private ToolStripMenuItem mnuJD247;

		private ToolStripMenuItem mnuJD248;

		private ToolTip toolTip1;

		private ToolStripMenuItem mnuJD239;

		private ToolStripMenuItem mnuJD240;

		private ToolStripMenuItem mnuJD241;

		private RadioButton optGaia;

		private Panel panelGaiaFiles;

		private Label label20;

		private ListBox lstGaiaFiles;

		private Button cmdNow;

		private NumericUpDown updnMagDrop;

		private CheckBox chkMagDiff;

		private CheckBox chkAutoSave;

		private CheckBox chkClearPrevious;

		private Label label21;

		private Label label25;

		private ComboBox cmbAsteroidClasses;

		private Panel pnlSelectAsteroids;

		private Panel pnlSelectPlanets;

		private Label label8;

		private ListBox lstMiriadeAsteroids;

		private Label label4;

		private CheckBox chkOneOrbitOnly;

		private CheckBox chkNoMiriadeOrbits;

		private Label lblInternetWarning;

		private CheckBox chkSaveLast;

		private Label label10;

		private Label lblUpdateDate;

		private Label label11;

		private Button cmdMiriadeUpdateNow;

		private Panel pnlMiriade;

		private Button cmdGetMiriadeSolutions;

		private Label lblCatalogue;

		private Label label12;

		private Label label7;

		private Button cmdAdd1;

		internal CheckBox chkHorizons;

		internal CheckBox chkGaiaPlot;

		private NumericUpDown updnSearchLatitude;

		private Label label13;

		private NumericUpDown updnSearchLongitude;

		private CheckBox chkSiteLimit;

		private Label label15;

		private ComboBox cmbDistance;

		private Button cmdSetSite;

		private CheckBox chkHorizons_Satellites;

		private Label label16;

		private Label lblXYZversion;

		private Button cmdRegenerateXYZ;

		private Label label3;

		internal CheckBox chkUserElements;

		internal CheckBox chkDiameterLimit;

		internal CheckBox chkMinDuration;

		protected internal CheckBox chkAutoSetHorizons;

		internal NumericUpDown updnEndMonth;

		internal NumericUpDown updnEndYear;

		internal NumericUpDown updnStartMonth;

		internal NumericUpDown updnStartYear;

		internal NumericUpDown updnEndDay;

		internal NumericUpDown updnStartDay;

		private CheckBox chkNoListDisplay;

		private CheckBox chkSortByDate;

		internal CheckBox chkUseHorizonsEphemeris;

		internal NumericUpDown updnMinimumDuration;

		private Label label17;

		private CheckBox chkAperture;

		private NumericUpDown updnAperture;

		private Label label18;

		private CheckBox chkVarExp;

		private GroupBox grpFilters;

		private Panel panel1;

		private Label label19;

		private Label label14;

		private Label label22;

		private Label label23;

		private Panel panel2;

		private Label label24;

		private ToolStripMenuItem useoldHorizonsOrbitToolStripMenuItem;

		private ToolStripMenuItem selectOldOrbitToolStripMenuItem;

		private ToolStripMenuItem useOldOrbitToolStripMenuItem;

		private ToolStripMenuItem thisFunctionIsNOTForGenralUseToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator2;

		private ToolStripTextBox SelectedOrbitID;

		private ToolStripSeparator toolStripSeparator1;

		private Label lblMagDropWarning;

		public AsteroidSearch()
		{
			//IL_0049: Unknown result type (might be due to invalid IL or missing references)
			//IL_0053: Expected O, but got Unknown
			InitializeComponent();
			Booting = true;
			lstGaiaFiles.set_DrawMode((DrawMode)1);
			lstGaiaFiles.add_DrawItem(new DrawItemEventHandler(lstGaiaFiles_DrawItem));
			MinorPlanetOccultationElements.CallingForm = this;
			PlanetOccultationElements.CallingForm = this;
			AppPath = Utilities.AppPath;
			((ListControl)cmbPlanets).set_SelectedIndex(0);
			((ListControl)cmbAsteroidClasses).set_SelectedIndex(0);
			updnStartYear.set_Value((decimal)DateTime.Now.ToUniversalTime().Year);
			updnStartMonth.set_Value((decimal)DateTime.Now.ToUniversalTime().Month);
			updnStartDay.set_Value((decimal)DateTime.Now.ToUniversalTime().Day);
			updnEndYear.set_Value((decimal)DateTime.Now.ToUniversalTime().AddMonths(1).Year);
			updnEndMonth.set_Value((decimal)DateTime.Now.ToUniversalTime().AddMonths(1).Month);
			updnEndDay.set_Value((decimal)DateTime.Now.ToUniversalTime().AddMonths(1).Day);
			((Control)cmdThisYear).set_Text(Convert.ToString(DateTime.Now.ToUniversalTime().Year));
			((Control)cmdNextYear).set_Text(Convert.ToString(DateTime.Now.ToUniversalTime().Year + 1));
			((Control)cmd2yrs).set_Text(Convert.ToString(DateTime.Now.ToUniversalTime().Year + 2));
			Panel obj = pnlSelectAsteroids;
			Panel obj2 = pnlSelectPlanets;
			Point location = new Point(2, 42);
			((Control)obj2).set_Location(location);
			((Control)obj).set_Location(location);
			((Control)pnlSelectAsteroids).set_Visible(true);
			((Control)pnlSelectPlanets).set_Visible(false);
			CheckBox obj3 = chkHorizons;
			CheckBox obj4 = chkHorizons_Satellites;
			bool flag;
			chkHorizons_Satellites.set_Checked(flag = Utilities.InternetIsAvailable());
			bool enabled;
			((Control)obj4).set_Enabled(enabled = flag);
			((Control)obj3).set_Enabled(enabled);
			MoonLabel();
			SetUserStarLabel();
			toolTip1.set_InitialDelay(100);
			toolTip1.set_AutoPopDelay(5000);
			updnRACorrn.set_Value(0m);
			updnDecCorrn.set_Value(0m);
			Set_DontShowListAndDisplay_Status();
			Booting = false;
		}

		private void lstGaiaFiles_DrawItem(object sender, DrawItemEventArgs e)
		{
			Color color = Color.FromArgb(255, 128, 255, 0);
			Color color2 = Color.FromArgb(255, 255, 238, 225);
			if (e.get_Index() > -1)
			{
				if (e.get_Index() == ((ListControl)lstGaiaFiles).get_SelectedIndex())
				{
					e.get_Graphics().FillRectangle(Brushes.Blue, e.get_Bounds());
				}
				else if (e.get_Index() < Gaia.GaiaPrimaryFiles.Count + 1)
				{
					e.get_Graphics().FillRectangle(new SolidBrush(color), e.get_Bounds());
				}
				else if (e.get_Index() < Gaia.GaiaAdditionalFiles.Count + Gaia.GaiaPrimaryFiles.Count + 1)
				{
					e.get_Graphics().FillRectangle(new SolidBrush(color2), e.get_Bounds());
				}
				else
				{
					e.DrawBackground();
				}
				if (e.get_Index() == ((ListControl)lstGaiaFiles).get_SelectedIndex())
				{
					e.get_Graphics().FillRectangle(Brushes.Blue, e.get_Bounds());
				}
				using Brush brush = new SolidBrush(e.get_ForeColor());
				e.get_Graphics().DrawString(lstGaiaFiles.get_Items().get_Item(e.get_Index()).ToString(), e.get_Font(), brush, e.get_Bounds().Location);
			}
		}

		private void AsteroidSearch_Load(object sender, EventArgs e)
		{
			//IL_047c: Unknown result type (might be due to invalid IL or missing references)
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			if (File.Exists(AppPath + "\\Resource Files\\UserA.bin"))
			{
				cmbUserCat.get_Items().Add((object)"UserA");
			}
			if (File.Exists(AppPath + "\\Resource Files\\UserB.bin"))
			{
				cmbUserCat.get_Items().Add((object)"UserB");
			}
			if (File.Exists(AppPath + "\\Resource Files\\UserC.bin"))
			{
				cmbUserCat.get_Items().Add((object)"UserC");
			}
			if (File.Exists(AppPath + "\\Resource Files\\UserD.bin"))
			{
				cmbUserCat.get_Items().Add((object)"UserD");
			}
			if (File.Exists(AppPath + "\\Resource Files\\UserE.bin"))
			{
				cmbUserCat.get_Items().Add((object)"UserE");
			}
			if (File.Exists(AppPath + "\\Resource Files\\UserF.bin"))
			{
				cmbUserCat.get_Items().Add((object)"UserF");
			}
			bool @checked;
			if (cmbUserCat.get_Items().get_Count() > 0)
			{
				((ListControl)cmbUserCat).set_SelectedIndex(0);
				RadioButton obj = optUserCats;
				((Control)optUserCats).set_Enabled(@checked = true);
				obj.set_Checked(@checked);
			}
			if (!File.Exists(AppPath + "\\Resource Files\\AsteroidClasses.csv"))
			{
				http.DownloadAsteroidClassFile();
			}
			XYZFileVersion = Settings.Default.XYZfileVersion;
			RadioButton obj2 = optUserStar;
			((Control)optUserStar).set_Enabled(@checked = (UserStarCataloguePresent = File.Exists(AppPath + "\\Resource Files\\UserStar_MinorPlanetSearch.csv")));
			obj2.set_Checked(@checked);
			((Control)optTycho2).set_Enabled(Tycho2Present = File.Exists(AppPath + "\\Resource Files\\Tycho2.bin"));
			UCAC4_Path = Settings.Default.UCAC4_Path;
			((Control)optUCAC4).set_Enabled(UCAC4Present = File.Exists(UCAC4_Path + "\\u4b\\z900"));
			PPMXL_Path = Settings.Default.PPMXL_Path;
			((Control)optPPMXL).set_Enabled(PPMXLPresent = File.Exists(PPMXL_Path + "\\n89d.dat"));
			RadioButton obj3 = optTycho2;
			RadioButton obj4 = optUCAC4;
			bool flag;
			optPPMXL.set_Checked(flag = false);
			obj4.set_Checked(@checked = flag);
			obj3.set_Checked(@checked);
			chkGaiaPlot.set_Checked(Settings.Default.GaiaStarPlotInSearch);
			lstGaiaFiles.get_Items().Clear();
			((ListControl)cmbDistance).set_SelectedIndex(Settings.Default.SearchDistance);
			Gaia.GetAvailableGaiaCatalogues();
			for (int i = 0; i < Gaia.GaiaPrimaryFiles.Count; i++)
			{
				lstGaiaFiles.get_Items().Add((object)Gaia.GaiaPrimaryFiles[i]);
			}
			lstGaiaFiles.get_Items().Add((object)"________________________");
			for (int j = 0; j < Gaia.GaiaAdditionalFiles.Count; j++)
			{
				lstGaiaFiles.get_Items().Add((object)Gaia.GaiaAdditionalFiles[j]);
			}
			if (lstGaiaFiles.get_Items().get_Count() > 0)
			{
				((ListControl)lstGaiaFiles).set_SelectedIndex(0);
			}
			optGaia.set_Checked(GaiaPresent = lstGaiaFiles.get_Items().get_Count() > 0);
			((Control)lblEphemerisSource).set_Text(Utilities.EphemerisBasis());
			((Control)lblXYZversion).set_Text("Current version : " + Settings.Default.XYZfileVersion);
			if (Utilities.EphemerisBasis().Contains("DE4") && Utilities.EphemerisBasis().PadRight(5).Substring(0, 5)
				.ToUpper() != Settings.Default.XYZfileVersion.PadRight(5).Substring(0, 5).ToUpper())
			{
				string text = "The files of elements used to integrate asteroid orbits are out of date. The effect will be minimal, but you should Regenerate the files using the button at the bottom of this form";
				if (!Utilities.DE_EphemerisFileExists)
				{
					text += "\r\n\r\nTo update these files, you first need to download the JPL-DE Ephemeris. This is download #33";
				}
				MessageBox.Show(text, "Update files for integrating", (MessageBoxButtons)0, (MessageBoxIcon)64);
			}
			((Control)txtOutputFile).set_Text(AppPath + "\\Generated Files\\Occelmnt" + DateTime.Now.Year + DateTime.Now.Month.ToString().PadLeft(2, '0') + DateTime.Now.Day.ToString().PadLeft(2, '0') + ".xml");
			Set_DontShowListAndDisplay_Status();
			SelectAll();
			SetEphemerisSource(IsUserFile: false);
			http.UpdateListOfMiriadeBinaries(MustUpdateNow: false);
			ShowListOfMiriadeAsteroids();
			((Control)pnlMiriade).set_Enabled(Utilities.InternetIsAvailable());
			Label obj5 = lblInternetWarning;
			chkNoMiriadeOrbits.set_Checked(@checked = !((Control)pnlMiriade).get_Enabled());
			((Control)obj5).set_Visible(@checked);
			Check_XYZ_Files();
			if (chkMagDiff.get_Checked())
			{
				MinorPlanetOccultationElements.MagDropLimitForAsteroidSearches = (double)updnMagDrop.get_Value();
			}
			else
			{
				MinorPlanetOccultationElements.MagDropLimitForAsteroidSearches = -1.0;
			}
		}

		private void SetEphemerisSource(bool IsUserFile)
		{
			if (IsUserFile)
			{
				Elements.UserAsteroids.Fill_AllAsteroids();
				if (Elements.UserAsteroids.AstElements.Count >= 1)
				{
					Elements.UserAsteroids.AstElements[0].OrbitDate.Split(new char[1] { '-' });
					((Control)lblElementsDate).set_Text("User file\r\n" + Elements.UserAsteroids.AstElements[0].OrbitDate);
				}
			}
			else if (LastRecord >= 0)
			{
				((Control)lblElementsDate).set_Text(Elements.MainAsteroids.AstElements[0].OrbitSource + "\r\n" + Elements.MainAsteroids.AstElements[0].OrbitDate);
			}
			else
			{
				((Control)lblEphemerisSource).set_Text("No elements");
			}
		}

		private void Set_DontShowListAndDisplay_Status()
		{
			if (!chkAutoSave.get_Checked() | !optGaia.get_Checked() | !optMinorPlanets.get_Checked())
			{
				chkNoListDisplay.set_Checked(false);
				((Control)chkNoListDisplay).set_Enabled(false);
			}
			else
			{
				((Control)chkNoListDisplay).set_Enabled(true);
			}
		}

		private void ShowListOfMiriadeAsteroids()
		{
			string[] array = Settings.Default.MiriadeAsteroids.Split(new char[1] { ' ' });
			lstMiriadeAsteroids.get_Items().Clear();
			for (int i = 0; i < array.Length; i++)
			{
				if (array[i].Length > 0)
				{
					lstMiriadeAsteroids.get_Items().Add((object)array[i]);
				}
			}
			DateTime miriadeBinaryUpdateDate = Settings.Default.MiriadeBinaryUpdateDate;
			((Control)lblUpdateDate).set_Text(miriadeBinaryUpdateDate.Year + " " + CultureInfo.CurrentCulture.DateTimeFormat.GetMonthName(miriadeBinaryUpdateDate.Month) + " " + miriadeBinaryUpdateDate.Day);
		}

		private void Check_XYZ_Files()
		{
			DateTime dateTime = new DateTime(2010, 1, 1);
			if (Utilities.DE_EphemerisFileExists | Utilities.DE_LongEphemerisFileExists)
			{
				dateTime = new DateTime(2010, 8, 17);
			}
			Button obj = cmdJD239;
			Button obj2 = cmdJD240;
			Button obj3 = cmdJD241;
			Button obj4 = cmdJD242;
			Button obj5 = cmdJD243;
			Button obj6 = cmdJD244;
			Button obj7 = cmdJD245;
			Button obj8 = cmdJD246;
			Button obj9 = cmdJD247;
			bool flag;
			((Control)cmdJD248).set_Enabled(flag = true);
			bool flag2;
			((Control)obj9).set_Enabled(flag2 = flag);
			bool flag3;
			((Control)obj8).set_Enabled(flag3 = flag2);
			bool flag4;
			((Control)obj7).set_Enabled(flag4 = flag3);
			bool flag5;
			((Control)obj6).set_Enabled(flag5 = flag4);
			bool flag6;
			((Control)obj5).set_Enabled(flag6 = flag5);
			bool flag7;
			((Control)obj4).set_Enabled(flag7 = flag6);
			bool flag8;
			((Control)obj3).set_Enabled(flag8 = flag7);
			bool enabled;
			((Control)obj2).set_Enabled(enabled = flag8);
			((Control)obj).set_Enabled(enabled);
			ToolStripMenuItem obj10 = mnuJD239;
			ToolStripMenuItem obj11 = mnuJD240;
			ToolStripMenuItem obj12 = mnuJD241;
			ToolStripMenuItem obj13 = mnuJD242;
			ToolStripMenuItem obj14 = mnuJD243;
			ToolStripMenuItem obj15 = mnuJD244;
			ToolStripMenuItem obj16 = mnuJD245;
			ToolStripMenuItem obj17 = mnuJD246;
			ToolStripMenuItem obj18 = mnuJD247;
			((ToolStripItem)mnuJD248).set_Enabled(flag = true);
			((ToolStripItem)obj18).set_Enabled(flag2 = flag);
			((ToolStripItem)obj17).set_Enabled(flag3 = flag2);
			((ToolStripItem)obj16).set_Enabled(flag4 = flag3);
			((ToolStripItem)obj15).set_Enabled(flag5 = flag4);
			((ToolStripItem)obj14).set_Enabled(flag6 = flag5);
			((ToolStripItem)obj13).set_Enabled(flag7 = flag6);
			((ToolStripItem)obj12).set_Enabled(flag8 = flag7);
			((ToolStripItem)obj11).set_Enabled(enabled = flag8);
			((ToolStripItem)obj10).set_Enabled(enabled);
			if (File.Exists(AppPath + "\\Resource Files\\2390000_XYZ.bin") && ((new FileInfo(AppPath + "\\Resource Files\\2390000_XYZ.bin").Length == 1200000) & (new FileInfo(AppPath + "\\Resource Files\\2390000_XYZ.bin").LastWriteTime > dateTime)))
			{
				ToolStripMenuItem obj19 = mnuJD239;
				((Control)cmdJD239).set_Enabled(enabled = false);
				((ToolStripItem)obj19).set_Enabled(enabled);
			}
			if (File.Exists(AppPath + "\\Resource Files\\2400000_XYZ.bin") && ((new FileInfo(AppPath + "\\Resource Files\\2400000_XYZ.bin").Length == 1200000) & (new FileInfo(AppPath + "\\Resource Files\\2400000_XYZ.bin").LastWriteTime > dateTime)))
			{
				ToolStripMenuItem obj20 = mnuJD240;
				((Control)cmdJD240).set_Enabled(enabled = false);
				((ToolStripItem)obj20).set_Enabled(enabled);
			}
			if (File.Exists(AppPath + "\\Resource Files\\2410000_XYZ.bin") && ((new FileInfo(AppPath + "\\Resource Files\\2410000_XYZ.bin").Length == 1200000) & (new FileInfo(AppPath + "\\Resource Files\\2410000_XYZ.bin").LastWriteTime > dateTime)))
			{
				ToolStripMenuItem obj21 = mnuJD241;
				((Control)cmdJD241).set_Enabled(enabled = false);
				((ToolStripItem)obj21).set_Enabled(enabled);
			}
			if (File.Exists(AppPath + "\\Resource Files\\2420000_XYZ.bin") && ((new FileInfo(AppPath + "\\Resource Files\\2420000_XYZ.bin").Length == 1200000) & (new FileInfo(AppPath + "\\Resource Files\\2420000_XYZ.bin").LastWriteTime > dateTime)))
			{
				ToolStripMenuItem obj22 = mnuJD242;
				((Control)cmdJD242).set_Enabled(enabled = false);
				((ToolStripItem)obj22).set_Enabled(enabled);
			}
			if (File.Exists(AppPath + "\\Resource Files\\2430000_XYZ.bin") && ((new FileInfo(AppPath + "\\Resource Files\\2430000_XYZ.bin").Length == 1200000) & (new FileInfo(AppPath + "\\Resource Files\\2430000_XYZ.bin").LastWriteTime > dateTime)))
			{
				ToolStripMenuItem obj23 = mnuJD243;
				((Control)cmdJD243).set_Enabled(enabled = false);
				((ToolStripItem)obj23).set_Enabled(enabled);
			}
			if (File.Exists(AppPath + "\\Resource Files\\2440000_XYZ.bin") && ((new FileInfo(AppPath + "\\Resource Files\\2440000_XYZ.bin").Length == 1200000) & (new FileInfo(AppPath + "\\Resource Files\\2440000_XYZ.bin").LastWriteTime > dateTime)))
			{
				ToolStripMenuItem obj24 = mnuJD244;
				((Control)cmdJD244).set_Enabled(enabled = false);
				((ToolStripItem)obj24).set_Enabled(enabled);
			}
			if (File.Exists(AppPath + "\\Resource Files\\2450000_XYZ.bin") && ((new FileInfo(AppPath + "\\Resource Files\\2450000_XYZ.bin").Length == 1200000) & (new FileInfo(AppPath + "\\Resource Files\\2450000_XYZ.bin").LastWriteTime > dateTime)))
			{
				ToolStripMenuItem obj25 = mnuJD245;
				((Control)cmdJD245).set_Enabled(enabled = false);
				((ToolStripItem)obj25).set_Enabled(enabled);
			}
			if (File.Exists(AppPath + "\\Resource Files\\2460000_XYZ.bin") && ((new FileInfo(AppPath + "\\Resource Files\\2460000_XYZ.bin").Length == 1200000) & (new FileInfo(AppPath + "\\Resource Files\\2460000_XYZ.bin").LastWriteTime > dateTime)))
			{
				ToolStripMenuItem obj26 = mnuJD246;
				((Control)cmdJD246).set_Enabled(enabled = false);
				((ToolStripItem)obj26).set_Enabled(enabled);
			}
			if (File.Exists(AppPath + "\\Resource Files\\2470000_XYZ.bin") && ((new FileInfo(AppPath + "\\Resource Files\\2470000_XYZ.bin").Length == 1200000) & (new FileInfo(AppPath + "\\Resource Files\\2470000_XYZ.bin").LastWriteTime > dateTime)))
			{
				ToolStripMenuItem obj27 = mnuJD247;
				((Control)cmdJD247).set_Enabled(enabled = false);
				((ToolStripItem)obj27).set_Enabled(enabled);
			}
			if (File.Exists(AppPath + "\\Resource Files\\2480000_XYZ.bin") && ((new FileInfo(AppPath + "\\Resource Files\\2480000_XYZ.bin").Length == 1200000) & (new FileInfo(AppPath + "\\Resource Files\\2480000_XYZ.bin").LastWriteTime > dateTime)))
			{
				ToolStripMenuItem obj28 = mnuJD248;
				((Control)cmdJD248).set_Enabled(enabled = false);
				((ToolStripItem)obj28).set_Enabled(enabled);
			}
		}

		private bool Check_XYZ_For_Search_Dates(double JDStart, double JDEnd)
		{
			bool flag = true;
			int num = (int)Math.Floor(JDStart / 10000.0);
			int num2 = (int)Math.Floor(JDEnd / 10000.0);
			if (num > 245)
			{
				num = 245;
			}
			if (num2 < 245)
			{
				num2 = 245;
			}
			for (int i = num; i <= num2; i++)
			{
				string text = AppPath + "\\Resource Files\\" + Convert.ToString(i) + "0000_XYZ.bin";
				bool flag2 = false;
				if (File.Exists(text) && new FileInfo(text).Length == 1200000)
				{
					flag2 = true;
				}
				flag = flag && flag2;
			}
			return flag;
		}

		private void cmdCompute_Click(object sender, EventArgs e)
		{
			try
			{
				DisplayMPOccultations.ListAndDisplay.chkStarID.set_Checked(false);
				((Control)DisplayMPOccultations.ListAndDisplay.lstSummary).set_BackColor(SystemColors.Window);
			}
			catch
			{
			}
			Search();
		}

		internal void Search()
		{
			//IL_004e: Unknown result type (might be due to invalid IL or missing references)
			//IL_007e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0158: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ad: Unknown result type (might be due to invalid IL or missing references)
			//IL_04e0: Unknown result type (might be due to invalid IL or missing references)
			//IL_04e6: Invalid comparison between Unknown and I4
			//IL_0f16: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f7e: Unknown result type (might be due to invalid IL or missing references)
			string text = ((Control)txtOutputFile).get_Text();
			bool @checked = chkAutoSave.get_Checked();
			bool flag = !chkNoListDisplay.get_Checked();
			int num = FirstRecord;
			int lastAsteroidRecord = LastRecord;
			if (text.Length < 2)
			{
				MessageBox.Show("An output file must be set", "No output file", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			if (optGaia.get_Checked() & (((ListControl)lstGaiaFiles).get_SelectedIndex() < 0))
			{
				MessageBox.Show("A Gaia file has not been selected - so no search can occur", "No GAIA catalogue file", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return;
			}
			double num2 = Utilities.JD_from_Date((int)updnStartYear.get_Value(), (int)updnStartMonth.get_Value(), (int)updnStartDay.get_Value());
			double num3 = Utilities.JD_from_Date((int)updnEndYear.get_Value(), (int)updnEndMonth.get_Value(), (int)updnEndDay.get_Value());
			if (optMinorPlanets.get_Checked())
			{
				num2 = Math.Floor(num2 - 0.5) + 0.5;
				num3 = Math.Floor(num3 - 0.5) + 1.5;
				if (!Check_XYZ_For_Search_Dates(num2, num3))
				{
					MessageBox.Show("Planet elements for the search period are not present.\r\n\r\nMake sure the buttons at the bottom of the form are depressed\r\nfor the period from 2000 to the period of the search, inclusive. \r\n\r\nAlternatively, use the menu item 'numeric integration data'.", "Error - Planetary elements for integration", (MessageBoxButtons)0, (MessageBoxIcon)16);
					return;
				}
				if ((FirstRecord == LastRecord) & (FirstRecord >= 0))
				{
					Utilities.JD_from_Date((int)updnStartYear.get_Value(), (int)updnStartMonth.get_Value(), (double)updnStartDay.get_Value());
					Utilities.JD_from_Date((int)updnEndYear.get_Value(), (int)updnEndMonth.get_Value(), (double)updnEndDay.get_Value());
					if (num3 - num2 > 4.0)
					{
						bool flag2 = false;
						if (chkUserElements.get_Checked() | ((Elements.UserAsteroids.AstElements.Count == 1) & (FirstRecord < 0)))
						{
							if (Elements.UserAsteroids.AstElements.Count == 1)
							{
								if ((Elements.UserAsteroids.AstElements[0].A * (1.0 - Elements.UserAsteroids.AstElements[0].E) < 0.7) | (Elements.UserAsteroids.AstElements[0].E > 0.5))
								{
									flag2 = true;
								}
							}
							else if ((Elements.UserAsteroids.AstElements[FirstRecord].A * (1.0 - Elements.UserAsteroids.AstElements[FirstRecord].E) < 0.7) | (Elements.UserAsteroids.AstElements[FirstRecord].E > 0.5))
							{
								flag2 = true;
							}
						}
						else if ((Elements.MainAsteroids.AstElements[FirstRecord].A * (1.0 - Elements.MainAsteroids.AstElements[FirstRecord].E) < 0.7) | (Elements.MainAsteroids.AstElements[FirstRecord].E > 0.5))
						{
							flag2 = true;
						}
						if (flag2)
						{
							MessageBox.Show("The orbital elements of this asteroid are such that\r\nthe integration scheme in Occult may be inaccurate.\r\n\r\nYou can continue, and generate a list of possible occultations.\r\nHowever they may be inaccurate.\r\n\r\nTo ensure you have accurate predictions, you need \r\nto limit the search to particular days of interest.\r\n\r\nPlease use the 'Add 1 day' button below the Start Year... \r\nline at the top of the form to conduct a search for a\r\nspecific day of interest\r\n\r\nMake sure the 'use elements from Horizons' box is \r\nchecked, and re-select the asteroid for each new date - to\r\nensure the correct Horizons elements are downloaded.", "Integration issue", (MessageBoxButtons)0, (MessageBoxIcon)48);
						}
					}
				}
			}
			double limitingStarMagForSearch = (double)updnMag.get_Value();
			if (!chkApplyMagLimit.get_Checked())
			{
				limitingStarMagForSearch = 22.0;
			}
			double telescopeAperture_cm = 0.0;
			if (chkAperture.get_Checked())
			{
				telescopeAperture_cm = (double)updnAperture.get_Value();
			}
			double num4 = (double)updnMinimumDuration.get_Value();
			if (!chkMinDuration.get_Checked())
			{
				num4 = 0.0;
			}
			double expandedSearchDistInEarthRadii = (double)(int)(updnMissDistance.get_Value() * 10m) / 10.0;
			double num5 = (double)updnRACorrn.get_Value();
			double num6 = (double)updnDecCorrn.get_Value();
			SearchCancelled = false;
			MinorPlanetOccultationElements.IncludeWDSSearchInSearch = (PlanetOccultationElements.IncludeWDSSearchInSearch = true);
			OccultationElements.SiteNeedsUpdating = (OccultationElements.RegionNeedsUpdating = true);
			if (chkClearPrevious.get_Checked())
			{
				DisplayMPOccultations.OccElements.Clear();
			}
			if (@checked)
			{
				if (File.Exists(text) && (int)MessageBox.Show("The file for saving the output\r\n\r\n   " + text + "\r\n\r\nexists. Do you want to overwrite that file?", "Output file exists", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 7)
				{
					return;
				}
				using StreamWriter streamWriter = new StreamWriter(text);
				streamWriter.WriteLine("<Occultations>");
			}
			if (optMinorPlanets.get_Checked())
			{
				MinorPlanetOccultationElements.UseHorizonsEphemeris = chkUseHorizonsEphemeris.get_Checked();
				MinorPlanetOccultationElements.ApplySiteRestriction = chkSiteLimit.get_Checked();
				MinorPlanetOccultationElements.SiteLongitudeTest = (double)updnSearchLongitude.get_Value();
				MinorPlanetOccultationElements.SiteLatitudeTest = (double)updnSearchLatitude.get_Value();
				MinorPlanetOccultationElements.SearchDistance = (double)(300 + 300 * ((ListControl)cmbDistance).get_SelectedIndex()) / 6378.137;
				double minimumAsteroidDiameter = (double)updnDiameter.get_Value();
				if (!chkDiameterLimit.get_Checked())
				{
					minimumAsteroidDiameter = 0.0;
				}
				if (FirstRecord == LastRecord)
				{
					minimumAsteroidDiameter = 0.0;
				}
				bool useUserElementFile = chkUserElements.get_Checked() | chkHorizons.get_Checked();
				string text2 = AppPath + "\\Resource Files\\UserMinorPlanetElements.csv";
				double result = 0.0;
				double result2 = 0.0;
				double result3 = 0.0;
				bool useKnownErrors = false;
				if (chkHorizons.get_Checked())
				{
					if (!GetHorizonsElements())
					{
						string text3 = FirstNumber.ToString();
						if (FirstNumber < 1)
						{
							text3 = FirstName;
						}
						lstResults.get_Items().Add((object)"");
						lstResults.get_Items().Add((object)("   " + text3 + " is not present in Horizons"));
						return;
					}
					text2 = AppPath + "\\Resource Files\\HorizonsElements.csv";
					num = (lastAsteroidRecord = 0);
					using StreamReader streamReader = new StreamReader(text2);
					streamReader.ReadLine();
					string[] array = streamReader.ReadLine()!.Split(new char[1] { ',' });
					double.TryParse(array[22], out result);
					double.TryParse(array[23], out result2);
					double.TryParse(array[24], out result3);
					useKnownErrors = true;
				}
				string userStarCatFile = AppPath + "\\Resource Files\\UserStar_MinorPlanetSearch.csv";
				string asteroidClass = cmbAsteroidClasses.get_Items().get_Item(((ListControl)cmbAsteroidClasses).get_SelectedIndex()).ToString()!.Replace("All", "");
				if (AsteroidClassList.AClass.Count < 1)
				{
					AsteroidClassList.Fill_AllAsteroids();
				}
				if (BinaryAsteroids.BinElements.Count < 1)
				{
					MinorPlanetOccultationElements.CreateBinaryElementList();
				}
				if (MinorPlanetOccultationElements.BinaryAsteroidsInMiriade.Count < 1)
				{
					MinorPlanetOccultationElements.CreateBinaryAstroidsInMiriade();
				}
				MinorPlanetOccultationElements.QueryMiriade = !chkNoMiriadeOrbits.get_Checked();
				MinorPlanetOccultationElements.MiriadeFirstOnly = chkOneOrbitOnly.get_Checked();
				MinorPlanetOccultationElements.SaveMiriadeResponse = chkSaveLast.get_Checked();
				XMLprocess.ErrorCount = 0;
				MinorPlanetOccultationElements.SearchWDS = Interferometric_Plus_WDS.CanSearchWDS();
				MinorPlanetOccultationElements.SearchIF = Interferometric_Plus_WDS.CanSearchInterferometric();
				MinorPlanetOccultationElements.OffsetRA = num5 / 3600000.0 / (180.0 / Math.PI);
				MinorPlanetOccultationElements.OffsetDec = num6 / 3600000.0 / (180.0 / Math.PI);
				for (int i = 0; i < lstGaiaFiles.get_Items().get_Count(); i++)
				{
					if (lstGaiaFiles.get_Items().get_Item(i).ToString()!.Contains("Gaia"))
					{
						MinorPlanetOccultationElements.GaiaFileForNearby = lstGaiaFiles.get_Items().get_Item(i).ToString();
						break;
					}
				}
				if (num < 0)
				{
					return;
				}
				if (optGaia.get_Checked())
				{
					MinorPlanetOccultationElements.MinorPlanetSearch_Gaia(lstGaiaFiles.get_Items().get_Item(((ListControl)lstGaiaFiles).get_SelectedIndex()).ToString(), num2, num3, num, lastAsteroidRecord, minimumAsteroidDiameter, asteroidClass, limitingStarMagForSearch, num4, telescopeAperture_cm, chkVarExp.get_Checked(), expandedSearchDistInEarthRadii, useUserElementFile, UseDefaultErrorSettings: true, AppendFlag: true, text2, @checked, text, ShowProgressBar: true, GetNearbyStars: true, flag);
				}
				else if (optUCAC4.get_Checked())
				{
					MinorPlanetOccultationElements.MinorPlanetSearch_UCAC4(num2, num3, num, lastAsteroidRecord, minimumAsteroidDiameter, asteroidClass, limitingStarMagForSearch, num4, expandedSearchDistInEarthRadii, useUserElementFile, UseDefaultErrorSettings: true, AppendFlag: true, text2, @checked, text, ShowProgressBar: true);
				}
				else if (optTycho2.get_Checked())
				{
					MinorPlanetOccultationElements.MinorPlanetSearch_Tycho2(num2, num3, num, lastAsteroidRecord, minimumAsteroidDiameter, asteroidClass, limitingStarMagForSearch, num4, expandedSearchDistInEarthRadii, useUserElementFile, UseDefaultErrorSettings: true, AppendFlag: true, UseTycho2: true, text2, @checked, text, ShowProgressBar: true, "");
				}
				else if (optUserCats.get_Checked())
				{
					if (((ListControl)cmbUserCat).get_SelectedIndex() < 0)
					{
						return;
					}
					MinorPlanetOccultationElements.MinorPlanetSearch_Tycho2(num2, num3, num, lastAsteroidRecord, minimumAsteroidDiameter, asteroidClass, limitingStarMagForSearch, num4, expandedSearchDistInEarthRadii, useUserElementFile, UseDefaultErrorSettings: true, AppendFlag: true, UseTycho2: false, text2, @checked, text, ShowProgressBar: true, cmbUserCat.get_Items().get_Item(((ListControl)cmbUserCat).get_SelectedIndex()).ToString());
				}
				else if (optUserStar.get_Checked())
				{
					MinorPlanetOccultationElements.MinorPlanetSearch_UserStars(num2, num3, num, lastAsteroidRecord, minimumAsteroidDiameter, asteroidClass, limitingStarMagForSearch, num4, expandedSearchDistInEarthRadii, useUserElementFile, UseDefaultErrorSettings: true, AppendFlag: true, text2, userStarCatFile, @checked, text, ShowProgressBar: true, useKnownErrors, result, result2, result3, 0.0, 0.0);
				}
				else if (optPPMXL.get_Checked())
				{
					MinorPlanetOccultationElements.MinorPlanetSearch_PPMXL(num2, num3, num, lastAsteroidRecord, minimumAsteroidDiameter, asteroidClass, limitingStarMagForSearch, num4, expandedSearchDistInEarthRadii, useUserElementFile, UseDefaultErrorSettings: true, AppendFlag: true, text2, @checked, text, ShowProgressBar: true);
				}
				AsteroidClassList.AClass.Clear();
			}
			else
			{
				PlanetOccultationElements.ApplySiteRestriction = chkSiteLimit.get_Checked();
				PlanetOccultationElements.SiteLongitudeTest = (double)updnSearchLongitude.get_Value();
				PlanetOccultationElements.SiteLatitudeTest = (double)updnSearchLatitude.get_Value();
				PlanetOccultationElements.SearchDistance = (double)(300 + 300 * ((ListControl)cmbDistance).get_SelectedIndex()) / 6378.137;
				PlanetOccultationElements.HorizonsIsDown = false;
				string userStarCatFile2 = AppPath + "\\Resource Files\\UserStar_MinorPlanetSearch.csv";
				int num7 = ((ListControl)cmbPlanets).get_SelectedIndex();
				int num8 = ((ListControl)cmbPlanets).get_SelectedIndex();
				bool flag3 = false;
				if (((ListControl)cmbPlanets).get_SelectedIndex() == 0)
				{
					num7 = 1;
					num8 = 8;
					flag3 = true;
				}
				for (int j = num7; j <= num8; j++)
				{
					((ListControl)cmbPlanets).set_SelectedIndex(j);
					DoEvents();
					int num9 = j;
					if (num9 > 2)
					{
						num9++;
					}
					PlanetOccultationElements.RACorrection = num5 / 3600000.0 / (180.0 / Math.PI);
					PlanetOccultationElements.DecCorrection = num6 / 3600000.0 / (180.0 / Math.PI);
					PlanetOccultationElements.SearchWDS = Interferometric_Plus_WDS.CanSearchWDS();
					PlanetOccultationElements.SearchIF = Interferometric_Plus_WDS.CanSearchInterferometric();
					if (optGaia.get_Checked())
					{
						PlanetOccultationElements.PlanetSearch_Gaia(lstGaiaFiles.get_Items().get_Item(((ListControl)lstGaiaFiles).get_SelectedIndex()).ToString(), num2, num3, num9, chkIncludeMainMoons.get_Checked(), chkIncludeSmallMoons.get_Checked(), chkHorizons_Satellites.get_Checked(), limitingStarMagForSearch, num4, telescopeAperture_cm, chkVarExp.get_Checked(), expandedSearchDistInEarthRadii, AppendFlag: true, @checked, text, ShowProgressBar: true);
					}
					else if (optTycho2.get_Checked())
					{
						PlanetOccultationElements.PlanetSearch_Tycho2(num2, num3, num9, chkIncludeMainMoons.get_Checked(), chkIncludeSmallMoons.get_Checked(), chkHorizons_Satellites.get_Checked(), limitingStarMagForSearch, expandedSearchDistInEarthRadii, AppendFlag: true, UseTycho2: true, @checked, text, ShowProgressBar: true, "");
					}
					else if (optUserCats.get_Checked())
					{
						if (((ListControl)cmbUserCat).get_SelectedIndex() < 0)
						{
							return;
						}
						PlanetOccultationElements.PlanetSearch_Tycho2(num2, num3, num9, chkIncludeMainMoons.get_Checked(), chkIncludeSmallMoons.get_Checked(), chkHorizons_Satellites.get_Checked(), limitingStarMagForSearch, expandedSearchDistInEarthRadii, AppendFlag: true, UseTycho2: false, @checked, text, ShowProgressBar: true, cmbUserCat.get_Items().get_Item(((ListControl)cmbUserCat).get_SelectedIndex()).ToString());
					}
					else if (optUCAC4.get_Checked())
					{
						PlanetOccultationElements.PlanetSearch_UCAC4(num2, num3, num9, chkIncludeMainMoons.get_Checked(), chkIncludeSmallMoons.get_Checked(), chkHorizons_Satellites.get_Checked(), limitingStarMagForSearch, expandedSearchDistInEarthRadii, AppendFlag: true, @checked, text, ShowProgressBar: true);
					}
					else if (optPPMXL.get_Checked())
					{
						PlanetOccultationElements.PlanetSearch_PPMXL(num2, num3, num9, chkIncludeMainMoons.get_Checked(), chkIncludeSmallMoons.get_Checked(), chkHorizons_Satellites.get_Checked(), limitingStarMagForSearch, expandedSearchDistInEarthRadii, AppendFlag: true, @checked, text, ShowProgressBar: true);
					}
					else if (optUserStar.get_Checked())
					{
						PlanetOccultationElements.PlanetSearch_UserStar(num2, num3, num9, chkIncludeMainMoons.get_Checked(), chkIncludeSmallMoons.get_Checked(), chkHorizons_Satellites.get_Checked(), limitingStarMagForSearch, expandedSearchDistInEarthRadii, AppendFlag: true, userStarCatFile2, @checked, text, ShowProgressBar: true);
					}
				}
				if (flag3)
				{
					((ListControl)cmbPlanets).set_SelectedIndex(0);
				}
			}
			if (@checked)
			{
				using StreamWriter streamWriter2 = new StreamWriter(text, append: true);
				streamWriter2.WriteLine("</Occultations>");
			}
			if (flag)
			{
				lstResults.get_Items().Add((object)"");
				lstResults.get_Items().Add((object)"Search of the period");
				lstResults.get_Items().Add((object)(Utilities.Date_from_JD(num2, 0) + " to " + Utilities.Date_from_JD(num3, 0)));
				if (optMinorPlanets.get_Checked())
				{
					lstResults.get_Items().Add((object)("Covering : " + ((Control)lblSearchRange).get_Text()));
				}
				else
				{
					lstResults.get_Items().Add((object)("Covering : " + cmbPlanets.get_Items().get_Item(((ListControl)cmbPlanets).get_SelectedIndex())?.ToString() + ", " + ((Control)lblMoons).get_Text()));
				}
				if (SearchCancelled)
				{
					lstResults.get_Items().Add((object)"   Cancelled");
				}
				else
				{
					lstResults.get_Items().Add((object)"   Completed");
				}
				((ListControl)lstResults).set_SelectedIndex(lstResults.get_Items().get_Count() - 1);
				if (DisplayMPOccultations.OccElements.Count > MinorPlanetOccultationElements.MaximumNumEvents)
				{
					MessageBox.Show("The search has generated " + DisplayMPOccultations.OccElements.Count + " events, which is greater than\r\nthe " + MinorPlanetOccultationElements.MaximumNumEvents + " events that are allowed. This restriction has been \r\nimposed to ensure the program does not run out of memory, \r\nand has good performance.\r\n\r\nTo conduct the search you have specified, you should consider the following possibilities\r\n * incease (or apply) the limit on event duration\r\n * increase (or apply) the limit on star magnitudes\r\n * increase (or apply) the limit on asteroid diameter\r\n * reduce the time period covered by the search\r\n * reduce the range of asteroids covered by the search.\r\n\r\nPress OK to continue with displaying the currently found events\r\n\r\nYou can change the limit under Maintenance, User Settings, Defaults... under 11. Asteroid occultations\r\nHowever if the limit is too large, you may experience a fatal error. The recommended value is 20,000", "Too many events", (MessageBoxButtons)0, (MessageBoxIcon)48);
				}
				DisplayMPOccultations.ShowListAndDisplay();
				Cursor.set_Current(Cursors.get_WaitCursor());
				DisplayMPOccultations.ListAndDisplay.SetSite();
				DisplayMPOccultations.ListAndDisplay.SetSiteTests();
				DisplayMPOccultations.ListAndDisplay.RemoveDuplicates();
				DisplayMPOccultations.ListAndDisplay.SortAndDisplay(0);
				((Control)DisplayMPOccultations.ListAndDisplay).Focus();
			}
			else
			{
				MessageBox.Show("The search has been completed. The results are in\r\n\r\n" + ((Control)txtOutputFile).get_Text(), "Search finished", (MessageBoxButtons)0, (MessageBoxIcon)64);
			}
			Cursor.set_Current(((Control)this).get_DefaultCursor());
		}

		private void cmdThisYear_Click(object sender, EventArgs e)
		{
			NumericUpDown obj = updnEndYear;
			decimal value;
			updnStartYear.set_Value(value = DateTime.Now.ToUniversalTime().Year);
			obj.set_Value(value);
			updnStartMonth.set_Value(1m);
			updnStartDay.set_Value(1m);
			updnEndMonth.set_Value(12m);
			updnEndDay.set_Value(31m);
		}

		private void cmdNextYear_Click(object sender, EventArgs e)
		{
			updnStartYear.set_Value((decimal)(DateTime.Now.ToUniversalTime().Year + 1));
			updnStartMonth.set_Value(1m);
			updnStartDay.set_Value(1m);
			updnEndMonth.set_Value(12m);
			updnEndDay.set_Value(31m);
			updnEndYear.set_Value(updnStartYear.get_Value());
		}

		private void cmd2yrs_Click(object sender, EventArgs e)
		{
			NumericUpDown obj = updnEndYear;
			decimal value;
			updnStartYear.set_Value(value = DateTime.Now.ToUniversalTime().Year + 2);
			obj.set_Value(value);
			updnStartMonth.set_Value(1m);
			updnStartDay.set_Value(1m);
			updnEndMonth.set_Value(12m);
			updnEndDay.set_Value(31m);
		}

		private void cmdWholeYear_Click(object sender, EventArgs e)
		{
			int num = (int)updnStartYear.get_Value();
			int num2 = (int)(updnStartMonth.get_Value() + 11m);
			if (num2 > 12)
			{
				num++;
				num2 -= 12;
			}
			updnEndYear.set_Value((decimal)num);
			updnEndMonth.set_Value((decimal)num2);
			updnStartDay.set_Value(1m);
			updnEndDay.set_Value(updnEndDay.get_Maximum() - 1m);
		}

		private void cmdWholeMonth_Click(object sender, EventArgs e)
		{
			updnEndYear.set_Value(updnStartYear.get_Value());
			updnEndMonth.set_Value(updnStartMonth.get_Value());
			updnStartDay.set_Value(1m);
			updnEndDay.set_Value(updnEndDay.get_Maximum() - 1m);
		}

		private void cmdAdd1_Click(object sender, EventArgs e)
		{
			updnEndYear.set_Value(updnStartYear.get_Value());
			updnEndMonth.set_Value(updnStartMonth.get_Value());
			updnEndDay.set_Value(updnStartDay.get_Value() + 1m);
			optMinorPlanets.set_Checked(true);
			((Control)cmdSelectAsteroid).Focus();
			SelectAsteroid("");
			try
			{
				DisplayMPOccultations.ListAndDisplay.chkStarID.set_Checked(false);
				((Control)DisplayMPOccultations.ListAndDisplay.lstSummary).set_BackColor(SystemColors.Window);
			}
			catch
			{
			}
			if (((Control)chkHorizons).get_Enabled())
			{
				Search();
				return;
			}
			lstResults.get_Items().Clear();
			lstResults.get_Items().Add((object)"To search for ");
			lstResults.get_Items().Add((object)("   " + ((Control)lblSearchRange).get_Text()));
			lstResults.get_Items().Add((object)"click Search");
		}

		private void cmdNow_Click(object sender, EventArgs e)
		{
			updnStartYear.set_Value((decimal)DateTime.Now.ToUniversalTime().Year);
			updnStartMonth.set_Value((decimal)DateTime.Now.ToUniversalTime().Month);
			updnStartDay.set_Value((decimal)DateTime.Now.ToUniversalTime().Day);
			updnEndYear.set_Value((decimal)DateTime.Now.ToUniversalTime().AddDays(10.0).Year);
			updnEndMonth.set_Value((decimal)DateTime.Now.ToUniversalTime().AddDays(10.0).Month);
			updnEndDay.set_Value((decimal)DateTime.Now.ToUniversalTime().AddDays(10.0).Day);
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			MinorPlanetOccultationElements.AbortFlag = true;
			PlanetOccultationElements.AbortFlag = true;
		}

		public void DoEvents()
		{
			Application.DoEvents();
		}

		private void cmdSetOutputFile_Click(object sender, EventArgs e)
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_0048: Unknown result type (might be due to invalid IL or missing references)
			//IL_004e: Invalid comparison between Unknown and I4
			SaveFileDialog val = new SaveFileDialog();
			((FileDialog)val).set_CheckPathExists(true);
			((FileDialog)val).set_Title("Set file for output of elements");
			val.set_OverwritePrompt(false);
			((FileDialog)val).set_InitialDirectory(AppPath + "\\Generated Files\\");
			((FileDialog)val).set_Filter("XML files *.xml|*.xml|(all files)|*.*");
			((FileDialog)val).set_FilterIndex(1);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				((Control)txtOutputFile).set_Text(((FileDialog)val).get_FileName());
			}
		}

		private void cmdJD239_Click(object sender, EventArgs e)
		{
			GenerateDailyXYZephemeris("239");
		}

		private void cmdJD240_Click(object sender, EventArgs e)
		{
			GenerateDailyXYZephemeris("240");
		}

		private void cmdJD241_Click(object sender, EventArgs e)
		{
			GenerateDailyXYZephemeris("241");
		}

		private void cmdJD242_Click(object sender, EventArgs e)
		{
			GenerateDailyXYZephemeris("242");
		}

		private void cmdJD243_Click(object sender, EventArgs e)
		{
			GenerateDailyXYZephemeris("243");
		}

		private void cmdJD244_Click(object sender, EventArgs e)
		{
			GenerateDailyXYZephemeris("244");
		}

		private void cmdJD245_Click(object sender, EventArgs e)
		{
			GenerateDailyXYZephemeris("245");
		}

		private void cmdJD246_Click(object sender, EventArgs e)
		{
			GenerateDailyXYZephemeris("246");
		}

		private void cmdJD247_Click(object sender, EventArgs e)
		{
			GenerateDailyXYZephemeris("247");
		}

		private void cmdJD248_Click(object sender, EventArgs e)
		{
			GenerateDailyXYZephemeris("248");
		}

		private void mnuJD239_Click(object sender, EventArgs e)
		{
			GenerateDailyXYZephemeris("239");
		}

		private void mnuJD240_Click(object sender, EventArgs e)
		{
			GenerateDailyXYZephemeris("240");
		}

		private void mnuJD241_Click(object sender, EventArgs e)
		{
			GenerateDailyXYZephemeris("241");
		}

		private void mnuJD242_Click(object sender, EventArgs e)
		{
			GenerateDailyXYZephemeris("242");
		}

		private void mnuJD243_Click(object sender, EventArgs e)
		{
			GenerateDailyXYZephemeris("243");
		}

		private void mnuJD244_Click(object sender, EventArgs e)
		{
			GenerateDailyXYZephemeris("244");
		}

		private void mnuJD245_Click(object sender, EventArgs e)
		{
			GenerateDailyXYZephemeris("245");
		}

		private void mnuJD246_Click(object sender, EventArgs e)
		{
			GenerateDailyXYZephemeris("246");
		}

		private void mnuJD247_Click(object sender, EventArgs e)
		{
			GenerateDailyXYZephemeris("247");
		}

		private void mnuJD248_Click(object sender, EventArgs e)
		{
			GenerateDailyXYZephemeris("248");
		}

		private void GenerateDailyXYZephemeris(string JDbase)
		{
			Application.set_UseWaitCursor(true);
			Application.DoEvents();
			Utilities.AllPlanetsXYZ_Daily(JDbase);
			Application.set_UseWaitCursor(false);
			Check_XYZ_Files();
		}

		private void SelectAll()
		{
			string text2;
			string text;
			if (!chkUserElements.get_Checked())
			{
				if (Elements.MainAsteroids.AstElements.Count < 1)
				{
					Elements.MainAsteroids.Fill_AllAsteroids();
				}
				FirstRecord = 0;
				LastRecord = Elements.MainAsteroids.AstElements.Count - 1;
				text2 = (text = "");
				if (Elements.MainAsteroids.AstElements[FirstRecord].IDNumber > 0)
				{
					text2 = Elements.MainAsteroids.AstElements[FirstRecord].IDNumber + " ";
				}
				if (Elements.MainAsteroids.AstElements[LastRecord].IDNumber > 0)
				{
					text = Elements.MainAsteroids.AstElements[LastRecord].IDNumber + " ";
				}
				text2 += Elements.MainAsteroids.AstElements[0].IDName.Trim();
				text += Elements.MainAsteroids.AstElements[Elements.MainAsteroids.AstElements.Count - 1].IDName.Trim();
			}
			else
			{
				Elements.UserAsteroids.Fill_AllAsteroids();
				if (Elements.UserAsteroids.AstElements.Count > 0)
				{
					FirstRecord = 0;
					LastRecord = Elements.UserAsteroids.AstElements.Count - 1;
					text2 = (text = "");
					if (Elements.UserAsteroids.AstElements[FirstRecord].IDNumber > 0)
					{
						text2 = Elements.UserAsteroids.AstElements[FirstRecord].IDNumber + " ";
					}
					if (Elements.UserAsteroids.AstElements[LastRecord].IDNumber > 0)
					{
						text = Elements.UserAsteroids.AstElements[LastRecord].IDNumber + " ";
					}
					text2 += Elements.UserAsteroids.AstElements[0].IDName.Trim();
					text += Elements.UserAsteroids.AstElements[Elements.UserAsteroids.AstElements.Count - 1].IDName.Trim();
				}
				else
				{
					text2 = (text = "");
				}
			}
			((Control)lblSearchRange).set_Text(text2 + " <=> " + text);
			TotalAsteroidsToSearch();
			SetHorizonsAvailability();
		}

		private void TotalAsteroidsToSearch()
		{
			int num = 0;
			_ = new char[143];
			double num2 = (double)updnDiameter.get_Value();
			if (!chkDiameterLimit.get_Checked())
			{
				num2 = 0.0;
			}
			if (AsteroidClassList.AClass.Count < 1)
			{
				AsteroidClassList.Fill_AllAsteroids();
			}
			string value = cmbAsteroidClasses.get_Items().get_Item(((ListControl)cmbAsteroidClasses).get_SelectedIndex()).ToString()!.Replace("All", "");
			if (FirstRecord >= 0)
			{
				if (!chkUserElements.get_Checked())
				{
					if (Elements.MainAsteroids.AstElements.Count < 1)
					{
						Elements.MainAsteroids.Fill_AllAsteroids();
					}
					for (int i = FirstRecord; i <= LastRecord; i++)
					{
						if (Elements.MainAsteroids.AstElements[i].AsteroidClass.Contains(value) & (Elements.MainAsteroids.AstElements[i].Diameter_Mean >= num2))
						{
							num++;
						}
					}
				}
				else
				{
					if (Elements.UserAsteroids.AstElements.Count < 1)
					{
						Elements.UserAsteroids.Fill_AllAsteroids();
					}
					for (int j = FirstRecord; j <= LastRecord; j++)
					{
						if (Elements.UserAsteroids.AstElements[j].AsteroidClass.Contains(value) & (Elements.UserAsteroids.AstElements[j].Diameter_Mean >= num2))
						{
							num++;
						}
						else if (Elements.UserAsteroids.AstElements[j].Diameter_Mean >= num2)
						{
							num++;
						}
					}
				}
			}
			((Control)lblTotalObjects).set_Text("( " + num + " asteroids )");
			AsteroidClassList.AClass.Clear();
		}

		private void cmdSelectAsteroid_Click(object sender, EventArgs e)
		{
			SelectAsteroid("");
		}

		internal void SelectAsteroid(string PresetAsteroid)
		{
			//IL_004d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0053: Invalid comparison between Unknown and I4
			NumericUpDown obj = updnRACorrn;
			NumericUpDown obj2 = updnDecCorrn;
			decimal value = default(decimal);
			obj2.set_Value(value);
			obj.set_Value(value);
			Elements.UserAsteroids.SourceFile = AppPath + "\\Resource Files\\UserMinorPlanetElements.csv";
			AsteroidID_SearchString asteroidID_SearchString = new AsteroidID_SearchString(chkUserElements.get_Checked(), PresetAsteroid);
			if ((int)((Form)asteroidID_SearchString).ShowDialog() == 2)
			{
				return;
			}
			FirstRecord = asteroidID_SearchString.FirstRecord;
			LastRecord = asteroidID_SearchString.LastRecord;
			FirstNumber = asteroidID_SearchString.FirstAsteroidNumber;
			LastNumber = asteroidID_SearchString.LastAsteroidNumber;
			FirstName = asteroidID_SearchString.FirstAsteroidName;
			LastName = asteroidID_SearchString.LastAsteroidName;
			((Control)lblSearchRange).set_Text(asteroidID_SearchString.SearchLabel);
			useOldOrbitToolStripMenuItem.set_Checked(MinorPlanetOccultationElements.UseOldHorizonsOrbit = false);
			if (selectOldOrbitToolStripMenuItem.get_Checked())
			{
				((ToolStripItem)useoldHorizonsOrbitToolStripMenuItem).set_BackColor(SystemColors.Control);
				((ToolStripItem)useoldHorizonsOrbitToolStripMenuItem).set_ForeColor(Color.Red);
			}
			else
			{
				((ToolStripItem)useoldHorizonsOrbitToolStripMenuItem).set_BackColor(SystemColors.Control);
				((ToolStripItem)useoldHorizonsOrbitToolStripMenuItem).set_ForeColor(Color.Silver);
			}
			NumericUpDown obj3 = updnRACorrn;
			NumericUpDown obj4 = updnDecCorrn;
			value = default(decimal);
			obj4.set_Value(value);
			obj3.set_Value(value);
			if (FirstRecord < 0)
			{
				if (asteroidID_SearchString.NumberOfAsteroids == 1)
				{
					((Control)lblTotalObjects).set_Text("( 1, Horizons only )");
				}
				else
				{
					((Control)lblTotalObjects).set_Text("( no asteroids )");
				}
			}
			else if (FirstRecord == LastRecord)
			{
				((Control)panelAdjust).set_Enabled(true);
				((Control)lblTotalObjects).set_Text("( 1 asteroid )");
			}
			else
			{
				((Control)panelAdjust).set_Enabled(false);
				TotalAsteroidsToSearch();
			}
			if ((FirstRecord >= 0) | (asteroidID_SearchString.NumberOfAsteroids == 1))
			{
				HaveGotHorizons = false;
				SetHorizonsAvailability();
			}
			lstResults.get_Items().Clear();
			lstResults.get_Items().Add((object)"To search for ");
			lstResults.get_Items().Add((object)("   " + ((Control)lblSearchRange).get_Text()));
			lstResults.get_Items().Add((object)"click Search");
			((Control)lblAsteroidName).set_Text("");
		}

		private void optPlanets_CheckedChanged(object sender, EventArgs e)
		{
			((Control)lblMagDropWarning).set_Visible(optPlanets.get_Checked());
			NumericUpDown obj = updnRACorrn;
			NumericUpDown obj2 = updnDecCorrn;
			decimal value = default(decimal);
			obj2.set_Value(value);
			obj.set_Value(value);
			if (optPlanets.get_Checked())
			{
				((Control)lblAdjust).set_Text("Adjust planet position ( in mas ) by");
			}
			else
			{
				((Control)lblAdjust).set_Text("Adjust asteroid position ( in mas ) by");
				((Control)panelAdjust).set_Enabled(FirstRecord == LastRecord);
			}
			Set_DontShowListAndDisplay_Status();
		}

		private void optMinorPlanets_CheckedChanged(object sender, EventArgs e)
		{
			NumericUpDown obj = updnRACorrn;
			NumericUpDown obj2 = updnDecCorrn;
			decimal value = default(decimal);
			obj2.set_Value(value);
			obj.set_Value(value);
			((Control)lblMagDropWarning).set_Visible(optPlanets.get_Checked());
			if (optPlanets.get_Checked())
			{
				((Control)pnlSelectAsteroids).set_Visible(false);
				((Control)pnlSelectPlanets).set_Visible(true);
			}
			else
			{
				((Control)pnlSelectAsteroids).set_Visible(true);
				((Control)pnlSelectPlanets).set_Visible(false);
			}
			Set_DontShowListAndDisplay_Status();
		}

		private void chkDiameterLimit_Click(object sender, EventArgs e)
		{
			//IL_0082: Unknown result type (might be due to invalid IL or missing references)
			//IL_0088: Invalid comparison between Unknown and I4
			if (chkDiameterLimit.get_Checked())
			{
				string text = "";
				if (Elements.MainAsteroids.AstElements.Count > 1000)
				{
					text = " Failure to do this may result in too many events for Occult to process. ";
				}
				if ((int)MessageBox.Show("By unchecking this limitation on the asteroid diameter, all " + Elements.MainAsteroids.AstElements.Count + " asteroids in your file of asteroid elements will be considered in your search.\r\n\r\nTo avoid generating a HUGE number of potentially un-observable events, you need to set the option to exclude events where the duration is less than the specified value (for example, 0.4 secs), and/or set the Search filter to limit events to a specified site location." + text + "\r\n\r\nDo you want to proceed with unchecking this option?", "Warning - no diameter limit", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
				{
					return;
				}
			}
			chkDiameterLimit.set_Checked(!chkDiameterLimit.get_Checked());
			TotalAsteroidsToSearch();
		}

		private void chkUserElements_CheckedChanged(object sender, EventArgs e)
		{
			if (File.Exists(AppPath + "\\Resource Files\\UserMinorPlanetElements.csv"))
			{
				((Control)cmbAsteroidClasses).set_Enabled(!chkUserElements.get_Checked());
				if (chkUserElements.get_Checked())
				{
					((ListControl)cmbAsteroidClasses).set_SelectedIndex(0);
				}
			}
			else
			{
				Button obj = cmdSelectAsteroid;
				bool enabled;
				((Control)cmbAsteroidClasses).set_Enabled(enabled = true);
				((Control)obj).set_Enabled(enabled);
			}
			HaveGotHorizons = false;
			SetEphemerisSource(chkUserElements.get_Checked());
			SelectAll();
		}

		private void chkA_CheckedChanged(object sender, EventArgs e)
		{
			TotalAsteroidsToSearch();
		}

		private void updnDiameter_ValueChanged(object sender, EventArgs e)
		{
			TotalAsteroidsToSearch();
			if (updnDiameter.get_Value() < 30m)
			{
				updnDiameter.set_Increment(1m);
			}
			if (updnDiameter.get_Value() > 30m)
			{
				updnDiameter.set_Increment(2m);
			}
			if (updnDiameter.get_Value() > 50m)
			{
				updnDiameter.set_Increment(5m);
			}
			if (updnDiameter.get_Value() > 100m)
			{
				updnDiameter.set_Increment(10m);
			}
			if (updnDiameter.get_Value() > 200m)
			{
				updnDiameter.set_Increment(20m);
			}
			if (updnDiameter.get_Value() > 400m)
			{
				updnDiameter.set_Increment(50m);
			}
		}

		private void cmdEditUserStar_Click(object sender, EventArgs e)
		{
			//IL_0005: Unknown result type (might be due to invalid IL or missing references)
			((Form)new UserStar_Edit()).ShowDialog();
			SetUserStarLabel();
		}

		private void SetUserStarLabel()
		{
			if (!File.Exists(AppPath + "\\Resource Files\\UserStar_MinorPlanetSearch.csv"))
			{
				return;
			}
			using StreamReader streamReader = new StreamReader(AppPath + "\\Resource Files\\UserStar_MinorPlanetSearch.csv");
			string[] array = streamReader.ReadLine()!.Split(new char[1] { ',' });
			((Control)lblUserStar).set_Text("[ " + array[2].PadLeft(2, '0') + " " + array[3].PadLeft(2, '0') + " " + array[4] + "   " + array[6] + " " + array[7] + " " + array[8] + " ]");
		}

		private void MoonLabel()
		{
			int num = ((ListControl)cmbPlanets).get_SelectedIndex();
			if (num > 2)
			{
				num++;
			}
			((Control)lblMoons).set_Text("");
			if (!(chkIncludeMainMoons.get_Checked() | chkIncludeSmallMoons.get_Checked()))
			{
				return;
			}
			switch (num)
			{
			case 4:
				if (chkIncludeSmallMoons.get_Checked())
				{
					((Control)lblMoons).set_Text("Phobos, Deimos");
				}
				break;
			case 5:
				if (chkIncludeMainMoons.get_Checked())
				{
					Label obj3 = lblMoons;
					((Control)obj3).set_Text(((Control)obj3).get_Text() + "Io, Europa, Ganymede, Callisto");
				}
				if (chkIncludeSmallMoons.get_Checked())
				{
					Label obj4 = lblMoons;
					((Control)obj4).set_Text(((Control)obj4).get_Text() + " Himalia, Elara, Pasiphae, Sinope, Lysithea, Carme, Ananke, Leda");
				}
				break;
			case 6:
				if (chkIncludeMainMoons.get_Checked())
				{
					((Control)lblMoons).set_Text("Mimas, Enceladus, Tethys, Dione, Rhea, Titan, Hyperion, Iapetus");
				}
				if (chkIncludeSmallMoons.get_Checked())
				{
					Label obj7 = lblMoons;
					((Control)obj7).set_Text(((Control)obj7).get_Text() + " Phoebe, Helene, Telesto, Calypso");
				}
				break;
			case 7:
				if (chkIncludeMainMoons.get_Checked())
				{
					((Control)lblMoons).set_Text("Ariel, Umbriel, Titania, Oberon, Miranda");
				}
				if (chkIncludeSmallMoons.get_Checked())
				{
					Label obj5 = lblMoons;
					((Control)obj5).set_Text(((Control)obj5).get_Text() + " Cordelia, Ophelia, Bianca, Cressida, Desdemona, Juliet, Portia, Rosalind, Belinda, Puck");
				}
				break;
			case 8:
				if (chkIncludeMainMoons.get_Checked())
				{
					((Control)lblMoons).set_Text("Triton");
				}
				if (chkIncludeSmallMoons.get_Checked())
				{
					Label obj6 = lblMoons;
					((Control)obj6).set_Text(((Control)obj6).get_Text() + " Nereid");
				}
				break;
			case 9:
				if (chkIncludeMainMoons.get_Checked())
				{
					Label obj = lblMoons;
					((Control)obj).set_Text(((Control)obj).get_Text() + "Charon");
				}
				if (chkIncludeSmallMoons.get_Checked())
				{
					Label obj2 = lblMoons;
					((Control)obj2).set_Text(((Control)obj2).get_Text() + " Nix, Hydra, Kerberos, Styx");
				}
				break;
			default:
				((Control)lblMoons).set_Text("");
				break;
			}
		}

		private void chkIncludeMainMoons_CheckedChanged(object sender, EventArgs e)
		{
			MoonLabel();
		}

		private void chkIncludeSmallMoons_CheckedChanged(object sender, EventArgs e)
		{
			MoonLabel();
		}

		private void cmbPlanets_SelectedIndexChanged(object sender, EventArgs e)
		{
			MoonLabel();
			NumericUpDown obj = updnRACorrn;
			NumericUpDown obj2 = updnDecCorrn;
			decimal value = default(decimal);
			obj2.set_Value(value);
			obj.set_Value(value);
			((Control)panelAdjust).set_Enabled(((ListControl)cmbPlanets).get_SelectedIndex() > 0);
			switch (((ListControl)cmbPlanets).get_SelectedIndex())
			{
			case 0:
				((Control)txtPlanetMagLimit).set_Text("var");
				break;
			case 1:
				((Control)txtPlanetMagLimit).set_Text(Settings.Default.MagLimitMercury.ToString());
				break;
			case 2:
				((Control)txtPlanetMagLimit).set_Text(Settings.Default.MagLimitVenus.ToString());
				break;
			case 3:
				((Control)txtPlanetMagLimit).set_Text(Settings.Default.MagLimitMars.ToString());
				break;
			case 4:
				((Control)txtPlanetMagLimit).set_Text(Settings.Default.MagLimitJupiter.ToString());
				break;
			case 5:
				((Control)txtPlanetMagLimit).set_Text(Settings.Default.MagLimitSaturn.ToString());
				break;
			case 6:
				((Control)txtPlanetMagLimit).set_Text(Settings.Default.MagLimitUranus.ToString());
				break;
			case 7:
				((Control)txtPlanetMagLimit).set_Text(Settings.Default.MagLimitNeptune.ToString());
				break;
			case 8:
				((Control)txtPlanetMagLimit).set_Text(Settings.Default.MagLimitPluto.ToString());
				break;
			}
		}

		private void updnStartYear_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnStartYear).Select(0, 10);
		}

		private void updnStartYear_MouseDoubleClick(object sender, MouseEventArgs e)
		{
			((UpDownBase)updnStartYear).Select(0, 10);
		}

		private void updnStartMonth_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnStartMonth).Select(0, 10);
		}

		private void updnStartDay_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnStartDay).Select(0, 10);
		}

		private void updnEndYear_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnEndYear).Select(0, 10);
		}

		private void updnEndYear_MouseDoubleClick(object sender, MouseEventArgs e)
		{
			((UpDownBase)updnEndYear).Select(0, 10);
		}

		private void updnEndMonth_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnEndMonth).Select(0, 10);
		}

		private void updnEndDay_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnEndDay).Select(0, 10);
		}

		private void updnDiameter_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnDiameter).Select(0, 10);
		}

		private void updnMag_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnMag).Select(0, 10);
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Asteroid - Search");
		}

		private void updnStartDay_ValueChanged(object sender, EventArgs e)
		{
			if (ChangingMonth)
			{
				return;
			}
			if (updnStartDay.get_Value() == updnStartDay.get_Maximum())
			{
				NumericUpDown obj = updnStartMonth;
				obj.set_Value(obj.get_Value() + 1m);
				updnStartDay.set_Value(1m);
			}
			if (updnStartDay.get_Value() == 0m)
			{
				NumericUpDown obj2 = updnStartMonth;
				obj2.set_Value(obj2.get_Value() - 1m);
				updnStartDay.set_Maximum((decimal)(Utilities.MaximumDaysInMonth((int)updnStartYear.get_Value(), (int)updnStartMonth.get_Value()) + 1));
				if ((updnStartDay.get_Value() >= updnStartDay.get_Maximum()) | (updnStartDay.get_Value() == 0m))
				{
					updnStartDay.set_Value(updnStartDay.get_Maximum() - 1m);
				}
			}
			SetHorizonsAvailability();
		}

		private void updnEndDay_ValueChanged(object sender, EventArgs e)
		{
			if (ChangingMonth)
			{
				return;
			}
			if (updnEndDay.get_Value() == updnEndDay.get_Maximum())
			{
				NumericUpDown obj = updnEndMonth;
				obj.set_Value(obj.get_Value() + 1m);
				updnEndDay.set_Value(1m);
			}
			if (updnEndDay.get_Value() == 0m)
			{
				NumericUpDown obj2 = updnEndMonth;
				obj2.set_Value(obj2.get_Value() - 1m);
				updnEndDay.set_Maximum((decimal)(Utilities.MaximumDaysInMonth((int)updnEndYear.get_Value(), (int)updnEndMonth.get_Value()) + 1));
				if ((updnEndDay.get_Value() >= updnEndDay.get_Maximum()) | (updnEndDay.get_Value() == 0m))
				{
					updnEndDay.set_Value(updnEndDay.get_Maximum() - 1m);
				}
			}
			SetHorizonsAvailability();
		}

		private void updnStartMonth_ValueChanged(object sender, EventArgs e)
		{
			ChangingMonth = true;
			if (updnStartMonth.get_Value() == 13m)
			{
				NumericUpDown obj = updnStartYear;
				obj.set_Value(obj.get_Value() + 1m);
				updnStartMonth.set_Value(1m);
				return;
			}
			if (updnStartMonth.get_Value() == 0m)
			{
				NumericUpDown obj2 = updnStartYear;
				obj2.set_Value(obj2.get_Value() - 1m);
				updnStartMonth.set_Value(12m);
				return;
			}
			updnStartDay.set_Maximum((decimal)(Utilities.MaximumDaysInMonth((int)updnStartYear.get_Value(), (int)updnStartMonth.get_Value()) + 1));
			if ((updnStartDay.get_Value() >= updnStartDay.get_Maximum()) | (updnStartDay.get_Value() == 0m))
			{
				updnStartDay.set_Value(updnStartDay.get_Maximum() - 1m);
			}
			ChangingMonth = false;
			SetHorizonsAvailability();
		}

		private void updnEndMonth_ValueChanged(object sender, EventArgs e)
		{
			ChangingMonth = true;
			if (updnEndMonth.get_Value() == 13m)
			{
				NumericUpDown obj = updnEndYear;
				obj.set_Value(obj.get_Value() + 1m);
				updnEndMonth.set_Value(1m);
				return;
			}
			if (updnEndMonth.get_Value() == 0m)
			{
				NumericUpDown obj2 = updnEndYear;
				obj2.set_Value(obj2.get_Value() - 1m);
				updnEndMonth.set_Value(12m);
				return;
			}
			updnEndDay.set_Maximum((decimal)(Utilities.MaximumDaysInMonth((int)updnEndYear.get_Value(), (int)updnEndMonth.get_Value()) + 1));
			if ((updnEndDay.get_Value() >= updnEndDay.get_Maximum()) | (updnEndDay.get_Value() == 0m))
			{
				updnEndDay.set_Value(updnEndDay.get_Maximum() - 1m);
			}
			ChangingMonth = false;
			SetHorizonsAvailability();
		}

		private void updnStartYear_ValueChanged(object sender, EventArgs e)
		{
			updnStartDay.set_Maximum((decimal)(Utilities.MaximumDaysInMonth((int)updnStartYear.get_Value(), (int)updnStartMonth.get_Value()) + 1));
			if (((int)updnStartMonth.get_Value() == 2) & (updnStartDay.get_Value() >= 28m))
			{
				updnStartDay.set_Value(updnStartDay.get_Maximum() - 1m);
			}
			SetHorizonsAvailability();
		}

		private void updnStartYear_Leave(object sender, EventArgs e)
		{
			//IL_0044: Unknown result type (might be due to invalid IL or missing references)
			if ((updnStartYear.get_Value() > 2100m) | (updnStartYear.get_Value() < 1700m))
			{
				MessageBox.Show("Start year is outside the range 1700 to 2100\r\n\r\nReenter the year", "InvalidYear", (MessageBoxButtons)0, (MessageBoxIcon)16);
				((Control)updnStartYear).Focus();
			}
		}

		private void updnEndYear_ValueChanged(object sender, EventArgs e)
		{
			updnEndDay.set_Maximum((decimal)(Utilities.MaximumDaysInMonth((int)updnEndYear.get_Value(), (int)updnEndMonth.get_Value()) + 1));
			if (((int)updnEndMonth.get_Value() == 2) & (updnEndDay.get_Value() >= 28m))
			{
				updnEndDay.set_Value(updnEndDay.get_Maximum() - 1m);
			}
			SetHorizonsAvailability();
		}

		private void updnEndYear_Leave(object sender, EventArgs e)
		{
			//IL_0044: Unknown result type (might be due to invalid IL or missing references)
			if ((updnEndYear.get_Value() > 2100m) | (updnEndYear.get_Value() < 1700m))
			{
				MessageBox.Show("End year is outside the range 1700 to 2100\r\n\r\nReenter the year", "InvalidYear", (MessageBoxButtons)0, (MessageBoxIcon)16);
				((Control)updnEndYear).Focus();
			}
		}

		private void SetHorizonsAvailability()
		{
			double num = Utilities.JD_from_Date((int)updnStartYear.get_Value(), (int)updnStartMonth.get_Value(), (double)updnStartDay.get_Value());
			double num2 = Utilities.JD_from_Date((int)updnEndYear.get_Value(), (int)updnEndMonth.get_Value(), (double)updnEndDay.get_Value());
			bool flag = num2 - num >= 0.0 && num2 - num < 366.0;
			bool flag2 = FirstRecord == LastRecord;
			if (Utilities.InternetIsAvailable())
			{
				((Control)chkUseHorizonsEphemeris).set_Enabled(true);
				if (flag && flag2)
				{
					CheckBox obj = chkHorizons;
					bool enabled;
					chkUseHorizonsEphemeris.set_Checked(enabled = true);
					((Control)obj).set_Enabled(enabled);
					if (chkAutoSetHorizons.get_Checked())
					{
						CheckBox obj2 = chkHorizons;
						chkUseHorizonsEphemeris.set_Checked(enabled = true);
						obj2.set_Checked(enabled);
					}
				}
				else
				{
					((Control)chkHorizons).set_Enabled(false);
					chkHorizons.set_Checked(false);
				}
				if (((ListControl)cmbAsteroidClasses).get_SelectedIndex() > 0)
				{
					CheckBox obj3 = chkHorizons;
					bool enabled;
					((Control)chkAutoSetHorizons).set_Enabled(enabled = false);
					((Control)obj3).set_Enabled(enabled);
				}
			}
			else
			{
				CheckBox obj4 = chkHorizons;
				CheckBox obj5 = chkUseHorizonsEphemeris;
				bool flag3;
				((Control)chkAutoSetHorizons).set_Enabled(flag3 = false);
				bool enabled;
				((Control)obj5).set_Enabled(enabled = flag3);
				((Control)obj4).set_Enabled(enabled);
				CheckBox obj6 = chkHorizons;
				CheckBox obj7 = chkUseHorizonsEphemeris;
				chkAutoSetHorizons.set_Checked(flag3 = false);
				obj7.set_Checked(enabled = flag3);
				obj6.set_Checked(enabled);
			}
		}

		private void chkHorizons_CheckedChanged(object sender, EventArgs e)
		{
			if (!Booting)
			{
				if (chkHorizons.get_Checked())
				{
					chkAutoSetHorizons.set_Checked(true);
				}
				if (!HaveGotHorizons)
				{
					GetHorizonsElements();
				}
			}
		}

		private bool GetHorizonsElements()
		{
			//IL_0058: Unknown result type (might be due to invalid IL or missing references)
			string text = "3696";
			if (((Control)chkHorizons).get_Enabled() & chkHorizons.get_Checked())
			{
				text = FirstNumber.ToString();
				if (FirstNumber < 1)
				{
					text = FirstName;
				}
				if (text == "134340")
				{
					MessageBox.Show("Asteroid 134340 = Pluto. This function does not work with Pluto", "Pluto", (MessageBoxButtons)0, (MessageBoxIcon)64);
					return false;
				}
				AsteroidElements AE = new AsteroidElements();
				string horizonsElements_TDBtime = http.GetHorizonsElements_TDBtime(text, (int)updnStartYear.get_Value(), (int)updnStartMonth.get_Value(), (int)updnStartDay.get_Value(), 0.0, ref AE);
				if ((horizonsElements_TDBtime == null) | (horizonsElements_TDBtime.Length < 100))
				{
					HaveGotHorizons = false;
					return false;
				}
				using StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Resource Files\\HorizonsElements.csv");
				streamWriter.WriteLine(AsteroidElements.CSVHeader());
				streamWriter.WriteLine(horizonsElements_TDBtime);
			}
			HaveGotHorizons = true;
			return true;
		}

		private void optGaia_CheckedChanged(object sender, EventArgs e)
		{
			((Control)panelGaiaFiles).set_Enabled(optGaia.get_Checked());
			if (lstGaiaFiles.get_Items().get_Count() > 0 && optGaia.get_Checked())
			{
				((Control)lblCatalogue).set_Text(lstGaiaFiles.get_Items().get_Item(((ListControl)lstGaiaFiles).get_SelectedIndex()).ToString());
			}
			Set_DontShowListAndDisplay_Status();
		}

		private void lstGaiaFiles_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (lstGaiaFiles.get_Items().get_Count() > 0 && optGaia.get_Checked())
			{
				((Control)lblCatalogue).set_Text(lstGaiaFiles.get_Items().get_Item(((ListControl)lstGaiaFiles).get_SelectedIndex()).ToString());
			}
		}

		private void optTycho2_CheckedChanged(object sender, EventArgs e)
		{
			if (optTycho2.get_Checked())
			{
				((Control)lblCatalogue).set_Text("Tycho2 - pre gaia");
			}
			Set_DontShowListAndDisplay_Status();
		}

		private void optUCAC4_CheckedChanged(object sender, EventArgs e)
		{
			if (optUCAC4.get_Checked())
			{
				((Control)lblCatalogue).set_Text("UCAC4");
			}
			Set_DontShowListAndDisplay_Status();
		}

		private void optUserCats_CheckedChanged(object sender, EventArgs e)
		{
			if (optUserCats.get_Checked())
			{
				((Control)lblCatalogue).set_Text("User - Cat " + cmbUserCat.get_Items().get_Item(((ListControl)cmbUserCat).get_SelectedIndex()));
			}
			Set_DontShowListAndDisplay_Status();
		}

		private void cmdRegenerateXYZ_Click(object sender, EventArgs e)
		{
			for (int i = 239; i < 249; i++)
			{
				GenerateDailyXYZephemeris(i.ToString());
			}
			Settings.Default.XYZfileVersion = Utilities.EphemerisBasis();
			XYZFileVersion = Settings.Default.XYZfileVersion;
			((Control)lblXYZversion).set_Text("Current version : " + Settings.Default.XYZfileVersion);
		}

		private void chkAutoSetHorizons_MouseClick(object sender, MouseEventArgs e)
		{
			//IL_0024: Unknown result type (might be due to invalid IL or missing references)
			//IL_002a: Invalid comparison between Unknown and I4
			if (chkAutoSetHorizons.get_Checked())
			{
				if ((int)MessageBox.Show("This setting controls whether the 'use Horizons elements'\r\nwill be automatically set when searching\r\n\r\nAutomatic setting occurs when:\r\n*  there is only one asteroid selected, and\r\n*  the search period is less than 1 year\r\n\r\nDo you want to disable the automatic setting of Horizons?", "Disable auto-set of Horizons", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
				{
					return;
				}
				chkAutoSetHorizons.set_Checked(false);
			}
			else
			{
				chkAutoSetHorizons.set_Checked(true);
			}
			SetHorizonsAvailability();
		}

		private void updnMissDistance_Leave(object sender, EventArgs e)
		{
			updnMissDistance.set_Value((decimal)((double)(int)(updnMissDistance.get_Value() * 10m) / 10.0));
		}

		private void chkAutoSave_CheckedChanged(object sender, EventArgs e)
		{
			Set_DontShowListAndDisplay_Status();
		}

		private void chkUseHorizonsEphemeris_Click(object sender, EventArgs e)
		{
			//IL_0033: Unknown result type (might be due to invalid IL or missing references)
			chkUseHorizonsEphemeris.set_Checked(!chkUseHorizonsEphemeris.get_Checked());
			if (chkUseHorizonsEphemeris.get_Checked())
			{
				MessageBox.Show("You have checked 'Use Horizons ephemeris...'\r\n\r\nThis option provides the maximum accuracy of\r\npredictions. However it will increase the time to generate\r\nthe predictions, because an ephemeris is downloaded\r\nfor each event found.\r\n\r\nAs a result, this option should only be set for searches \r\nof a single asteroid, or of a small number of asteroids over\r\na limited period of time. You should also consider\r\nlimiting the search by setting the Search filter to exclude\r\nstars fainter than a specified value.", "'use Horizons ephemeris' limitations", (MessageBoxButtons)0, (MessageBoxIcon)64);
			}
		}

		private void selectOldOrbitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0029: Unknown result type (might be due to invalid IL or missing references)
			//IL_002f: Expected O, but got Unknown
			//IL_006c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0072: Invalid comparison between Unknown and I4
			selectOldOrbitToolStripMenuItem.set_Checked(!selectOldOrbitToolStripMenuItem.get_Checked());
			if (selectOldOrbitToolStripMenuItem.get_Checked())
			{
				OpenFileDialog val = new OpenFileDialog();
				((FileDialog)val).set_Title("Specify file Containing HORIZONS orbit parameters");
				((FileDialog)val).set_Filter("Text files (*.txt)|*.txt|All files (*.*)|*.*");
				val.set_Multiselect(false);
				try
				{
					((FileDialog)val).set_InitialDirectory(Path.GetDirectoryName(Utilities.AppPath + "\\DownLoaded Files\\"));
				}
				catch
				{
				}
				if ((int)((CommonDialog)val).ShowDialog() == 1)
				{
					string text;
					using (StreamReader streamReader = new StreamReader(((FileDialog)val).get_FileName()))
					{
						text = streamReader.ReadToEnd();
					}
					string text2 = Path.GetFileNameWithoutExtension(((FileDialog)val).get_FileName())!.Replace(" ", "").Replace("#", "~");
					string[] array = text.Replace("\r\n\r\n", "\r\n").Replace("\r\n", ",").Split(new char[1] { ',' });
					StringBuilder stringBuilder = new StringBuilder();
					stringBuilder.Append("COMMAND=%3B&MAKE_EPHEM='YES'&EPHEM_TYPE='OBSERVER'&" + array[0].Replace("= ", "=").Replace(" ", "") + "&");
					stringBuilder.Append("OBJECT='" + text2 + "'&ECLIP=J2000&");
					stringBuilder.Append(array[1].Trim().Replace(" = ", "=").Replace("= ", "=")
						.Replace("  ", "&") + "&");
					stringBuilder.Append(array[2].Trim().Replace(" = ", "=").Replace("= ", "=")
						.Replace("  ", "&") + "&");
					stringBuilder.Append(array[3].Trim().Replace(" = ", "=").Replace("= ", "=")
						.Insert(4, "'")
						.Replace(" ", ",") + ",");
					stringBuilder.Append(array[4].Trim().Replace(" ", ",") + ",");
					stringBuilder.Append(array[5].Trim().Replace(" ", ",") + ",");
					stringBuilder.Append(array[6].Trim().Replace(" ", ",") + ",");
					stringBuilder.Append(array[7].Trim().Replace(" ", ",") + ",");
					stringBuilder.Append(array[8].Trim().Replace(" ", ",") + ",");
					stringBuilder.Append(array[9].Trim().Replace(" ", ",") + "'&");
					MinorPlanetOccultationElements.OldHorizonsOrbitForQueryString = stringBuilder.ToString();
					((ToolStripItem)SelectedOrbitID).set_Text(text2);
					((ToolStripItem)useoldHorizonsOrbitToolStripMenuItem).set_ForeColor(Color.Red);
				}
			}
			else
			{
				((ToolStripItem)useoldHorizonsOrbitToolStripMenuItem).set_ForeColor(Color.Silver);
				((ToolStripItem)useoldHorizonsOrbitToolStripMenuItem).set_BackColor(SystemColors.Control);
				MinorPlanetOccultationElements.OldHorizonsOrbitForQueryString = "";
				useOldOrbitToolStripMenuItem.set_Checked(false);
				((ToolStripItem)SelectedOrbitID).set_Text("None");
			}
		}

		private void useOldOrbitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (selectOldOrbitToolStripMenuItem.get_Checked())
			{
				useOldOrbitToolStripMenuItem.set_Checked(!useOldOrbitToolStripMenuItem.get_Checked());
				MinorPlanetOccultationElements.UseOldHorizonsOrbit = useOldOrbitToolStripMenuItem.get_Checked();
				if (useOldOrbitToolStripMenuItem.get_Checked())
				{
					((ToolStripItem)useoldHorizonsOrbitToolStripMenuItem).set_BackColor(Color.Yellow);
					((ToolStripItem)useoldHorizonsOrbitToolStripMenuItem).set_ForeColor(Color.Green);
				}
				else
				{
					((ToolStripItem)useoldHorizonsOrbitToolStripMenuItem).set_BackColor(SystemColors.Control);
					((ToolStripItem)useoldHorizonsOrbitToolStripMenuItem).set_ForeColor(Color.Red);
				}
			}
		}

		private void optPPMXL_CheckedChanged(object sender, EventArgs e)
		{
			if (optPPMXL.get_Checked())
			{
				((Control)lblCatalogue).set_Text("PPMXL");
			}
			Set_DontShowListAndDisplay_Status();
		}

		private void optUserStar_CheckedChanged(object sender, EventArgs e)
		{
			if (optUserStar.get_Checked())
			{
				((Control)lblCatalogue).set_Text("User star");
			}
			Set_DontShowListAndDisplay_Status();
		}

		private void cmbDistance_SelectedIndexChanged(object sender, EventArgs e)
		{
			Settings.Default.SearchDistance = ((ListControl)cmbDistance).get_SelectedIndex();
		}

		private void cmdSetSite_Click(object sender, EventArgs e)
		{
			//IL_0007: Unknown result type (might be due to invalid IL or missing references)
			//IL_000e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0014: Invalid comparison between Unknown and I4
			SiteSelector siteSelector = new SiteSelector();
			((Form)siteSelector).ShowDialog();
			if ((int)((Form)siteSelector).get_DialogResult() == 1)
			{
				updnSearchLongitude.set_Value(decimal.Parse(((Control)siteSelector.txtLongitude).get_Text()));
				updnSearchLatitude.set_Value(decimal.Parse(((Control)siteSelector.txtLatitude).get_Text()));
			}
			((Component)(object)siteSelector).Dispose();
		}

		private void chkGaiaPlot_CheckedChanged(object sender, EventArgs e)
		{
			Settings.Default.GaiaStarPlotInSearch = chkGaiaPlot.get_Checked();
		}

		private void cmbUserCat_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (optUserCats.get_Checked())
			{
				((Control)lblCatalogue).set_Text("User - Cat " + cmbUserCat.get_Items().get_Item(((ListControl)cmbUserCat).get_SelectedIndex()));
			}
		}

		private void lstGaiaFiles_Click(object sender, EventArgs e)
		{
			if (((ListControl)lstGaiaFiles).get_SelectedIndex() == Gaia.GaiaPrimaryFiles.Count)
			{
				if (Gaia.GaiaPrimaryFiles.Count > 0)
				{
					ListBox obj = lstGaiaFiles;
					int selectedIndex = ((ListControl)obj).get_SelectedIndex();
					((ListControl)obj).set_SelectedIndex(selectedIndex + 1);
				}
				else if (((ListControl)lstGaiaFiles).get_SelectedIndex() > 2)
				{
					ListBox obj2 = lstGaiaFiles;
					int selectedIndex = ((ListControl)obj2).get_SelectedIndex();
					((ListControl)obj2).set_SelectedIndex(selectedIndex - 1);
				}
			}
			if ((((ListControl)lstGaiaFiles).get_SelectedIndex() > -1) & !lstGaiaFiles.get_Items().get_Item(((ListControl)lstGaiaFiles).get_SelectedIndex()).ToString()!.Contains("____"))
			{
				GaiaCatalogueMap.Plot = chkGaiaPlot.get_Checked();
				Gaia.ShowGaiaMap();
				((Control)Gaia.GaiaMap).set_Left(((Control)this).get_Left() + ((Control)grpSetStarCat).get_Left());
				((Control)Gaia.GaiaMap).set_Top(((Control)this).get_Top() + ((Control)grpSetStarCat).get_Bottom());
				Gaia.GaiaMap.MapCatalogueCoverage(lstGaiaFiles.get_Items().get_Item(((ListControl)lstGaiaFiles).get_SelectedIndex()).ToString());
			}
		}

		private void cmdMiriadeUpdateNow_Click(object sender, EventArgs e)
		{
			if (http.UpdateListOfMiriadeBinaries(MustUpdateNow: true))
			{
				ShowListOfMiriadeAsteroids();
				MinorPlanetOccultationElements.CreateBinaryAstroidsInMiriade();
			}
		}

		private void cmdGetMiriadeSolutions_Click(object sender, EventArgs e)
		{
			//IL_0005: Unknown result type (might be due to invalid IL or missing references)
			((Form)new MiriadeSolutions()).ShowDialog();
		}

		private void lstGaiaFiles_Leave(object sender, EventArgs e)
		{
			try
			{
				((Form)Gaia.GaiaMap).Close();
				((Component)(object)Gaia.GaiaMap).Dispose();
			}
			catch
			{
			}
		}

		private void chkMagDiff_CheckedChanged(object sender, EventArgs e)
		{
			if (chkMagDiff.get_Checked())
			{
				PlanetOccultationElements.MagDropLimitForSatelliteSearches = (MinorPlanetOccultationElements.MagDropLimitForAsteroidSearches = (double)updnMagDrop.get_Value());
			}
			else
			{
				PlanetOccultationElements.MagDropLimitForSatelliteSearches = (MinorPlanetOccultationElements.MagDropLimitForAsteroidSearches = -1.0);
			}
		}

		private void updnMagDrop_ValueChanged(object sender, EventArgs e)
		{
			if (chkMagDiff.get_Checked())
			{
				PlanetOccultationElements.MagDropLimitForSatelliteSearches = (MinorPlanetOccultationElements.MagDropLimitForAsteroidSearches = (double)updnMagDrop.get_Value());
			}
			else
			{
				PlanetOccultationElements.MagDropLimitForSatelliteSearches = (MinorPlanetOccultationElements.MagDropLimitForAsteroidSearches = -1.0);
			}
		}

		private void chkMinDuration_Click(object sender, EventArgs e)
		{
			//IL_0032: Unknown result type (might be due to invalid IL or missing references)
			//IL_0038: Invalid comparison between Unknown and I4
			if (!chkMinDuration.get_Checked())
			{
				chkMinDuration.set_Checked(true);
			}
			else if ((int)MessageBox.Show("Clearing the limitation on event duration can result \r\nin a huge number of events.\r\n\r\nAre you sure you want to clear the duration limitation?", "Confirm duration limitation", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 6)
			{
				chkMinDuration.set_Checked(false);
			}
			if (chkMinDuration.get_Checked())
			{
				chkMinDuration.set_Checked(true);
			}
		}

		private void cmbAsteroidClasses_SelectedIndexChanged(object sender, EventArgs e)
		{
			FirstRecord = 0;
			LastRecord = Elements.MainAsteroids.GetAsteroidRecord_fromNumber(999999, GetNextHigher: false, GetNextLower: true);
			FirstName = Elements.MainAsteroids.AstElements[FirstRecord].IDName.Trim();
			FirstNumber = Elements.MainAsteroids.AstElements[FirstRecord].IDNumber;
			LastName = Elements.MainAsteroids.AstElements[LastRecord].IDName.Trim();
			LastNumber = Elements.MainAsteroids.AstElements[LastRecord].IDNumber;
			((Control)lblSearchRange).set_Text("(" + FirstNumber + ") " + FirstName + " <=> (" + LastNumber + ") " + LastName);
			TotalAsteroidsToSearch();
			chkAutoSetHorizons.set_Checked(true);
			CheckBox obj = chkUserElements;
			CheckBox obj2 = chkHorizons;
			bool flag;
			((Control)chkAutoSetHorizons).set_Enabled(flag = ((ListControl)cmbAsteroidClasses).get_SelectedIndex() == 0);
			bool enabled;
			((Control)obj2).set_Enabled(enabled = flag);
			((Control)obj).set_Enabled(enabled);
			if (((ListControl)cmbAsteroidClasses).get_SelectedIndex() > 0)
			{
				CheckBox obj3 = chkUserElements;
				chkHorizons.set_Checked(enabled = false);
				obj3.set_Checked(enabled);
			}
			else
			{
				chkHorizons.set_Checked(true);
			}
		}

		private void cmdUpdateMiraideBinaryAsteroids_Click(object sender, EventArgs e)
		{
			http.UpdateListOfMiriadeBinaries(MustUpdateNow: false);
			ShowListOfMiriadeAsteroids();
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
			//IL_094a: Unknown result type (might be due to invalid IL or missing references)
			//IL_09af: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a1d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ada: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ba0: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c0e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ccc: Unknown result type (might be due to invalid IL or missing references)
			//IL_0dba: Unknown result type (might be due to invalid IL or missing references)
			//IL_0dc4: Expected O, but got Unknown
			//IL_0de4: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ea1: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f8f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f99: Expected O, but got Unknown
			//IL_0ff5: Unknown result type (might be due to invalid IL or missing references)
			//IL_10a0: Unknown result type (might be due to invalid IL or missing references)
			//IL_112e: Unknown result type (might be due to invalid IL or missing references)
			//IL_11f5: Unknown result type (might be due to invalid IL or missing references)
			//IL_125f: Unknown result type (might be due to invalid IL or missing references)
			//IL_12c9: Unknown result type (might be due to invalid IL or missing references)
			//IL_1357: Unknown result type (might be due to invalid IL or missing references)
			//IL_13e5: Unknown result type (might be due to invalid IL or missing references)
			//IL_1473: Unknown result type (might be due to invalid IL or missing references)
			//IL_1501: Unknown result type (might be due to invalid IL or missing references)
			//IL_159e: Unknown result type (might be due to invalid IL or missing references)
			//IL_1717: Unknown result type (might be due to invalid IL or missing references)
			//IL_173b: Unknown result type (might be due to invalid IL or missing references)
			//IL_17a5: Unknown result type (might be due to invalid IL or missing references)
			//IL_184b: Unknown result type (might be due to invalid IL or missing references)
			//IL_18e9: Unknown result type (might be due to invalid IL or missing references)
			//IL_1952: Unknown result type (might be due to invalid IL or missing references)
			//IL_1a33: Unknown result type (might be due to invalid IL or missing references)
			//IL_1ac7: Unknown result type (might be due to invalid IL or missing references)
			//IL_1b89: Unknown result type (might be due to invalid IL or missing references)
			//IL_1c27: Unknown result type (might be due to invalid IL or missing references)
			//IL_1cf6: Unknown result type (might be due to invalid IL or missing references)
			//IL_1db6: Unknown result type (might be due to invalid IL or missing references)
			//IL_1e82: Unknown result type (might be due to invalid IL or missing references)
			//IL_1f45: Unknown result type (might be due to invalid IL or missing references)
			//IL_2007: Unknown result type (might be due to invalid IL or missing references)
			//IL_2011: Expected O, but got Unknown
			//IL_204c: Unknown result type (might be due to invalid IL or missing references)
			//IL_20d5: Unknown result type (might be due to invalid IL or missing references)
			//IL_20df: Expected O, but got Unknown
			//IL_212a: Unknown result type (might be due to invalid IL or missing references)
			//IL_21f0: Unknown result type (might be due to invalid IL or missing references)
			//IL_21fa: Expected O, but got Unknown
			//IL_2227: Unknown result type (might be due to invalid IL or missing references)
			//IL_22c0: Unknown result type (might be due to invalid IL or missing references)
			//IL_22ca: Expected O, but got Unknown
			//IL_2319: Unknown result type (might be due to invalid IL or missing references)
			//IL_23fd: Unknown result type (might be due to invalid IL or missing references)
			//IL_2407: Expected O, but got Unknown
			//IL_2435: Unknown result type (might be due to invalid IL or missing references)
			//IL_24e9: Unknown result type (might be due to invalid IL or missing references)
			//IL_25a0: Unknown result type (might be due to invalid IL or missing references)
			//IL_266a: Unknown result type (might be due to invalid IL or missing references)
			//IL_26d7: Unknown result type (might be due to invalid IL or missing references)
			//IL_26e1: Expected O, but got Unknown
			//IL_2702: Unknown result type (might be due to invalid IL or missing references)
			//IL_27ea: Unknown result type (might be due to invalid IL or missing references)
			//IL_2851: Unknown result type (might be due to invalid IL or missing references)
			//IL_285b: Expected O, but got Unknown
			//IL_287c: Unknown result type (might be due to invalid IL or missing references)
			//IL_294c: Unknown result type (might be due to invalid IL or missing references)
			//IL_2956: Expected O, but got Unknown
			//IL_29a1: Unknown result type (might be due to invalid IL or missing references)
			//IL_2a21: Unknown result type (might be due to invalid IL or missing references)
			//IL_2a9b: Unknown result type (might be due to invalid IL or missing references)
			//IL_2b13: Unknown result type (might be due to invalid IL or missing references)
			//IL_2b8a: Unknown result type (might be due to invalid IL or missing references)
			//IL_2cc2: Unknown result type (might be due to invalid IL or missing references)
			//IL_2ce6: Unknown result type (might be due to invalid IL or missing references)
			//IL_2d50: Unknown result type (might be due to invalid IL or missing references)
			//IL_2e6b: Unknown result type (might be due to invalid IL or missing references)
			//IL_2e8f: Unknown result type (might be due to invalid IL or missing references)
			//IL_2f11: Unknown result type (might be due to invalid IL or missing references)
			//IL_2fc4: Unknown result type (might be due to invalid IL or missing references)
			//IL_2fce: Expected O, but got Unknown
			//IL_2fec: Unknown result type (might be due to invalid IL or missing references)
			//IL_309a: Unknown result type (might be due to invalid IL or missing references)
			//IL_30a4: Expected O, but got Unknown
			//IL_30d1: Unknown result type (might be due to invalid IL or missing references)
			//IL_326f: Unknown result type (might be due to invalid IL or missing references)
			//IL_3293: Unknown result type (might be due to invalid IL or missing references)
			//IL_3326: Unknown result type (might be due to invalid IL or missing references)
			//IL_33a1: Unknown result type (might be due to invalid IL or missing references)
			//IL_3449: Unknown result type (might be due to invalid IL or missing references)
			//IL_34db: Unknown result type (might be due to invalid IL or missing references)
			//IL_356c: Unknown result type (might be due to invalid IL or missing references)
			//IL_35fd: Unknown result type (might be due to invalid IL or missing references)
			//IL_368e: Unknown result type (might be due to invalid IL or missing references)
			//IL_371f: Unknown result type (might be due to invalid IL or missing references)
			//IL_37b0: Unknown result type (might be due to invalid IL or missing references)
			//IL_383e: Unknown result type (might be due to invalid IL or missing references)
			//IL_38cc: Unknown result type (might be due to invalid IL or missing references)
			//IL_395a: Unknown result type (might be due to invalid IL or missing references)
			//IL_3a52: Unknown result type (might be due to invalid IL or missing references)
			//IL_3ae1: Unknown result type (might be due to invalid IL or missing references)
			//IL_3b48: Unknown result type (might be due to invalid IL or missing references)
			//IL_3b52: Expected O, but got Unknown
			//IL_3ba1: Unknown result type (might be due to invalid IL or missing references)
			//IL_3c81: Unknown result type (might be due to invalid IL or missing references)
			//IL_3c8b: Expected O, but got Unknown
			//IL_3cbc: Unknown result type (might be due to invalid IL or missing references)
			//IL_3d4a: Unknown result type (might be due to invalid IL or missing references)
			//IL_3df8: Unknown result type (might be due to invalid IL or missing references)
			//IL_3e02: Expected O, but got Unknown
			//IL_3e32: Unknown result type (might be due to invalid IL or missing references)
			//IL_3ec8: Unknown result type (might be due to invalid IL or missing references)
			//IL_3f55: Unknown result type (might be due to invalid IL or missing references)
			//IL_3feb: Unknown result type (might be due to invalid IL or missing references)
			//IL_4094: Unknown result type (might be due to invalid IL or missing references)
			//IL_40fa: Unknown result type (might be due to invalid IL or missing references)
			//IL_4104: Expected O, but got Unknown
			//IL_4124: Unknown result type (might be due to invalid IL or missing references)
			//IL_420c: Unknown result type (might be due to invalid IL or missing references)
			//IL_4288: Unknown result type (might be due to invalid IL or missing references)
			//IL_431b: Unknown result type (might be due to invalid IL or missing references)
			//IL_4392: Unknown result type (might be due to invalid IL or missing references)
			//IL_4429: Unknown result type (might be due to invalid IL or missing references)
			//IL_4433: Expected O, but got Unknown
			//IL_4454: Unknown result type (might be due to invalid IL or missing references)
			//IL_450d: Unknown result type (might be due to invalid IL or missing references)
			//IL_4517: Expected O, but got Unknown
			//IL_4534: Unknown result type (might be due to invalid IL or missing references)
			//IL_462c: Unknown result type (might be due to invalid IL or missing references)
			//IL_4721: Unknown result type (might be due to invalid IL or missing references)
			//IL_4745: Unknown result type (might be due to invalid IL or missing references)
			//IL_4852: Unknown result type (might be due to invalid IL or missing references)
			//IL_4932: Unknown result type (might be due to invalid IL or missing references)
			//IL_49c5: Unknown result type (might be due to invalid IL or missing references)
			//IL_49cf: Expected O, but got Unknown
			//IL_49fc: Unknown result type (might be due to invalid IL or missing references)
			//IL_4aea: Unknown result type (might be due to invalid IL or missing references)
			//IL_4b64: Unknown result type (might be due to invalid IL or missing references)
			//IL_4b6e: Expected O, but got Unknown
			//IL_4ba2: Unknown result type (might be due to invalid IL or missing references)
			//IL_4c4e: Unknown result type (might be due to invalid IL or missing references)
			//IL_4dc1: Unknown result type (might be due to invalid IL or missing references)
			//IL_4e48: Unknown result type (might be due to invalid IL or missing references)
			//IL_4ed4: Unknown result type (might be due to invalid IL or missing references)
			//IL_4f7f: Unknown result type (might be due to invalid IL or missing references)
			//IL_5033: Unknown result type (might be due to invalid IL or missing references)
			//IL_50c4: Unknown result type (might be due to invalid IL or missing references)
			//IL_5158: Unknown result type (might be due to invalid IL or missing references)
			//IL_51f0: Unknown result type (might be due to invalid IL or missing references)
			//IL_5292: Unknown result type (might be due to invalid IL or missing references)
			//IL_529c: Expected O, but got Unknown
			//IL_52bd: Unknown result type (might be due to invalid IL or missing references)
			//IL_5343: Unknown result type (might be due to invalid IL or missing references)
			//IL_53e2: Unknown result type (might be due to invalid IL or missing references)
			//IL_5458: Unknown result type (might be due to invalid IL or missing references)
			//IL_54f4: Unknown result type (might be due to invalid IL or missing references)
			//IL_55b4: Unknown result type (might be due to invalid IL or missing references)
			//IL_5732: Unknown result type (might be due to invalid IL or missing references)
			//IL_57ca: Unknown result type (might be due to invalid IL or missing references)
			//IL_5874: Unknown result type (might be due to invalid IL or missing references)
			//IL_5902: Unknown result type (might be due to invalid IL or missing references)
			//IL_59b2: Unknown result type (might be due to invalid IL or missing references)
			//IL_5b45: Unknown result type (might be due to invalid IL or missing references)
			//IL_5b69: Unknown result type (might be due to invalid IL or missing references)
			//IL_5bff: Unknown result type (might be due to invalid IL or missing references)
			//IL_5c90: Unknown result type (might be due to invalid IL or missing references)
			//IL_5d21: Unknown result type (might be due to invalid IL or missing references)
			//IL_5d88: Unknown result type (might be due to invalid IL or missing references)
			//IL_5d92: Expected O, but got Unknown
			//IL_5dbf: Unknown result type (might be due to invalid IL or missing references)
			//IL_5eb3: Unknown result type (might be due to invalid IL or missing references)
			//IL_5fa5: Unknown result type (might be due to invalid IL or missing references)
			//IL_602b: Unknown result type (might be due to invalid IL or missing references)
			//IL_60be: Unknown result type (might be due to invalid IL or missing references)
			//IL_6191: Unknown result type (might be due to invalid IL or missing references)
			//IL_6264: Unknown result type (might be due to invalid IL or missing references)
			//IL_62e8: Unknown result type (might be due to invalid IL or missing references)
			//IL_638c: Unknown result type (might be due to invalid IL or missing references)
			//IL_6424: Unknown result type (might be due to invalid IL or missing references)
			//IL_64d4: Unknown result type (might be due to invalid IL or missing references)
			//IL_6c0e: Unknown result type (might be due to invalid IL or missing references)
			//IL_6c95: Unknown result type (might be due to invalid IL or missing references)
			//IL_6c9f: Expected O, but got Unknown
			//IL_6ccd: Unknown result type (might be due to invalid IL or missing references)
			//IL_6d3f: Unknown result type (might be due to invalid IL or missing references)
			//IL_6d49: Expected O, but got Unknown
			//IL_6d81: Unknown result type (might be due to invalid IL or missing references)
			//IL_6e3f: Unknown result type (might be due to invalid IL or missing references)
			//IL_6ec6: Unknown result type (might be due to invalid IL or missing references)
			//IL_6ed0: Expected O, but got Unknown
			//IL_6f01: Unknown result type (might be due to invalid IL or missing references)
			//IL_70eb: Unknown result type (might be due to invalid IL or missing references)
			//IL_710f: Unknown result type (might be due to invalid IL or missing references)
			//IL_7198: Unknown result type (might be due to invalid IL or missing references)
			//IL_723c: Unknown result type (might be due to invalid IL or missing references)
			//IL_72ed: Unknown result type (might be due to invalid IL or missing references)
			//IL_74b4: Unknown result type (might be due to invalid IL or missing references)
			//IL_773d: Unknown result type (might be due to invalid IL or missing references)
			//IL_7747: Expected O, but got Unknown
			//IL_7796: Unknown result type (might be due to invalid IL or missing references)
			components = new Container();
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(AsteroidSearch));
			panel3 = new Panel();
			label1 = new Label();
			updnEndDay = new NumericUpDown();
			updnStartDay = new NumericUpDown();
			label2 = new Label();
			updnEndMonth = new NumericUpDown();
			updnEndYear = new NumericUpDown();
			updnStartMonth = new NumericUpDown();
			updnStartYear = new NumericUpDown();
			cmdAdd1 = new Button();
			cmdCompute = new Button();
			cmdCancel = new Button();
			lstResults = new ListBox();
			pbarSearch = new ProgressBar();
			cmdThisYear = new Button();
			cmdNextYear = new Button();
			cmdWholeYear = new Button();
			cmdWholeMonth = new Button();
			cmd2yrs = new Button();
			lblAsteroidName = new Label();
			grpSetStarCat = new GroupBox();
			cmdEditUserStar = new Button();
			cmbUserCat = new ComboBox();
			panelGaiaFiles = new Panel();
			lstGaiaFiles = new ListBox();
			label20 = new Label();
			optUCAC4 = new RadioButton();
			lblUserStar = new Label();
			optUserStar = new RadioButton();
			optUserCats = new RadioButton();
			optPPMXL = new RadioButton();
			optTycho2 = new RadioButton();
			optGaia = new RadioButton();
			chkGaiaPlot = new CheckBox();
			updnMagDrop = new NumericUpDown();
			chkMagDiff = new CheckBox();
			updnMag = new NumericUpDown();
			chkApplyMagLimit = new CheckBox();
			cmdSetSite = new Button();
			label15 = new Label();
			cmbDistance = new ComboBox();
			updnSearchLatitude = new NumericUpDown();
			label13 = new Label();
			updnSearchLongitude = new NumericUpDown();
			chkSiteLimit = new CheckBox();
			lblMiss1 = new Label();
			lblMiss2 = new Label();
			txtOutputFile = new TextBox();
			cmdSetOutputFile = new Button();
			grpSetDateRange = new GroupBox();
			cmdNow = new Button();
			grpSetSaveFile = new GroupBox();
			chkNoListDisplay = new CheckBox();
			chkAutoSave = new CheckBox();
			chkSortByDate = new CheckBox();
			groupBox4 = new GroupBox();
			lblXYZversion = new Label();
			cmdRegenerateXYZ = new Button();
			cmdJD248 = new Button();
			cmdJD247 = new Button();
			cmdJD246 = new Button();
			cmdJD245 = new Button();
			cmdJD244 = new Button();
			cmdJD243 = new Button();
			cmdJD242 = new Button();
			cmdJD241 = new Button();
			cmdJD240 = new Button();
			cmdJD239 = new Button();
			cmbAsteroidClasses = new ComboBox();
			label25 = new Label();
			updnMinimumDuration = new NumericUpDown();
			chkMinDuration = new CheckBox();
			lblTotalObjects = new Label();
			chkDiameterLimit = new CheckBox();
			chkUserElements = new CheckBox();
			lblSearchRange = new Label();
			cmdSelectAsteroid = new Button();
			label5 = new Label();
			updnDiameter = new NumericUpDown();
			lblEphemerisSource = new Label();
			txtPlanetMagLimit = new TextBox();
			chkPlanetMagLimit = new CheckBox();
			lblMoons = new Label();
			chkIncludeSmallMoons = new CheckBox();
			chkIncludeMainMoons = new CheckBox();
			cmbPlanets = new ComboBox();
			grpSetAsteroidsPlanets = new GroupBox();
			pnlSelectAsteroids = new Panel();
			panel2 = new Panel();
			chkUseHorizonsEphemeris = new CheckBox();
			chkAutoSetHorizons = new CheckBox();
			chkHorizons = new CheckBox();
			label23 = new Label();
			pnlMiriade = new Panel();
			lblInternetWarning = new Label();
			cmdGetMiriadeSolutions = new Button();
			cmdMiriadeUpdateNow = new Button();
			lblUpdateDate = new Label();
			label11 = new Label();
			label10 = new Label();
			chkSaveLast = new CheckBox();
			chkOneOrbitOnly = new CheckBox();
			chkNoMiriadeOrbits = new CheckBox();
			label4 = new Label();
			lstMiriadeAsteroids = new ListBox();
			optPlanets = new RadioButton();
			optMinorPlanets = new RadioButton();
			pnlSelectPlanets = new Panel();
			label3 = new Label();
			label16 = new Label();
			chkHorizons_Satellites = new CheckBox();
			label8 = new Label();
			grpSearch = new GroupBox();
			label12 = new Label();
			label7 = new Label();
			lblCatalogue = new Label();
			updnMissDistance = new NumericUpDown();
			chkClearPrevious = new CheckBox();
			panelAdjust = new Panel();
			label21 = new Label();
			updnDecCorrn = new NumericUpDown();
			updnRACorrn = new NumericUpDown();
			label9 = new Label();
			lblAdjust = new Label();
			lblElementsDate = new Label();
			label6 = new Label();
			menuStrip1 = new MenuStrip();
			setDataForSearchIntervalToolStripMenuItem = new ToolStripMenuItem();
			mnuJD239 = new ToolStripMenuItem();
			mnuJD240 = new ToolStripMenuItem();
			mnuJD241 = new ToolStripMenuItem();
			mnuJD242 = new ToolStripMenuItem();
			mnuJD243 = new ToolStripMenuItem();
			mnuJD244 = new ToolStripMenuItem();
			mnuJD245 = new ToolStripMenuItem();
			mnuJD246 = new ToolStripMenuItem();
			mnuJD247 = new ToolStripMenuItem();
			mnuJD248 = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			useoldHorizonsOrbitToolStripMenuItem = new ToolStripMenuItem();
			thisFunctionIsNOTForGenralUseToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			selectOldOrbitToolStripMenuItem = new ToolStripMenuItem();
			SelectedOrbitID = new ToolStripTextBox();
			toolStripSeparator1 = new ToolStripSeparator();
			useOldOrbitToolStripMenuItem = new ToolStripMenuItem();
			toolTip1 = new ToolTip(components);
			label17 = new Label();
			chkAperture = new CheckBox();
			updnAperture = new NumericUpDown();
			label18 = new Label();
			chkVarExp = new CheckBox();
			grpFilters = new GroupBox();
			label22 = new Label();
			label19 = new Label();
			label14 = new Label();
			panel1 = new Panel();
			label24 = new Label();
			lblMagDropWarning = new Label();
			((Control)panel3).SuspendLayout();
			((ISupportInitialize)updnEndDay).BeginInit();
			((ISupportInitialize)updnStartDay).BeginInit();
			((ISupportInitialize)updnEndMonth).BeginInit();
			((ISupportInitialize)updnEndYear).BeginInit();
			((ISupportInitialize)updnStartMonth).BeginInit();
			((ISupportInitialize)updnStartYear).BeginInit();
			((Control)grpSetStarCat).SuspendLayout();
			((Control)panelGaiaFiles).SuspendLayout();
			((ISupportInitialize)updnMagDrop).BeginInit();
			((ISupportInitialize)updnMag).BeginInit();
			((ISupportInitialize)updnSearchLatitude).BeginInit();
			((ISupportInitialize)updnSearchLongitude).BeginInit();
			((Control)grpSetDateRange).SuspendLayout();
			((Control)grpSetSaveFile).SuspendLayout();
			((Control)groupBox4).SuspendLayout();
			((ISupportInitialize)updnMinimumDuration).BeginInit();
			((ISupportInitialize)updnDiameter).BeginInit();
			((Control)grpSetAsteroidsPlanets).SuspendLayout();
			((Control)pnlSelectAsteroids).SuspendLayout();
			((Control)panel2).SuspendLayout();
			((Control)pnlMiriade).SuspendLayout();
			((Control)pnlSelectPlanets).SuspendLayout();
			((Control)grpSearch).SuspendLayout();
			((ISupportInitialize)updnMissDistance).BeginInit();
			((Control)panelAdjust).SuspendLayout();
			((ISupportInitialize)updnDecCorrn).BeginInit();
			((ISupportInitialize)updnRACorrn).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((ISupportInitialize)updnAperture).BeginInit();
			((Control)grpFilters).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)panel3).get_Controls().Add((Control)(object)label1);
			((Control)panel3).get_Controls().Add((Control)(object)updnEndDay);
			((Control)panel3).get_Controls().Add((Control)(object)updnStartDay);
			((Control)panel3).get_Controls().Add((Control)(object)label2);
			((Control)panel3).get_Controls().Add((Control)(object)updnEndMonth);
			((Control)panel3).get_Controls().Add((Control)(object)updnEndYear);
			((Control)panel3).get_Controls().Add((Control)(object)updnStartMonth);
			((Control)panel3).get_Controls().Add((Control)(object)updnStartYear);
			((Control)panel3).get_Controls().Add((Control)(object)cmdAdd1);
			((Control)panel3).set_Location(new Point(5, 15));
			((Control)panel3).set_Margin(new Padding(4, 3, 4, 3));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(294, 59));
			((Control)panel3).set_TabIndex(0);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(1, 4));
			((Control)label1).set_Margin(new Padding(4, 0, 4, 0));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(145, 13));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text("Start Year, month  && day");
			((Control)updnEndDay).set_Location(new Point(248, 37));
			((Control)updnEndDay).set_Margin(new Padding(4, 3, 4, 3));
			updnEndDay.set_Maximum(new decimal(new int[4] { 32, 0, 0, 0 }));
			((Control)updnEndDay).set_Name("updnEndDay");
			((Control)updnEndDay).set_Size(new Size(40, 20));
			((Control)updnEndDay).set_TabIndex(8);
			updnEndDay.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnEndDay.add_ValueChanged((EventHandler)updnEndDay_ValueChanged);
			((Control)updnEndDay).add_Enter((EventHandler)updnEndDay_Enter);
			((Control)updnStartDay).set_Location(new Point(248, 2));
			((Control)updnStartDay).set_Margin(new Padding(4, 3, 4, 3));
			updnStartDay.set_Maximum(new decimal(new int[4] { 32, 0, 0, 0 }));
			((Control)updnStartDay).set_Name("updnStartDay");
			((Control)updnStartDay).set_Size(new Size(40, 20));
			((Control)updnStartDay).set_TabIndex(3);
			updnStartDay.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnStartDay.add_ValueChanged((EventHandler)updnStartDay_ValueChanged);
			((Control)updnStartDay).add_Enter((EventHandler)updnStartDay_Enter);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(7, 39));
			((Control)label2).set_Margin(new Padding(4, 0, 4, 0));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(136, 13));
			((Control)label2).set_TabIndex(5);
			((Control)label2).set_Text("End Year, month && day");
			((Control)updnEndMonth).set_Location(new Point(202, 37));
			((Control)updnEndMonth).set_Margin(new Padding(4, 3, 4, 3));
			updnEndMonth.set_Maximum(new decimal(new int[4] { 13, 0, 0, 0 }));
			((Control)updnEndMonth).set_Name("updnEndMonth");
			((Control)updnEndMonth).set_Size(new Size(40, 20));
			((Control)updnEndMonth).set_TabIndex(7);
			updnEndMonth.set_Value(new decimal(new int[4] { 2, 0, 0, 0 }));
			updnEndMonth.add_ValueChanged((EventHandler)updnEndMonth_ValueChanged);
			((Control)updnEndMonth).add_Enter((EventHandler)updnEndMonth_Enter);
			((Control)updnEndYear).set_Location(new Point(145, 37));
			((Control)updnEndYear).set_Margin(new Padding(4, 3, 4, 3));
			updnEndYear.set_Maximum(new decimal(new int[4] { 9000, 0, 0, 0 }));
			updnEndYear.set_Minimum(new decimal(new int[4] { 9000, 0, 0, -2147483648 }));
			((Control)updnEndYear).set_Name("updnEndYear");
			((Control)updnEndYear).set_Size(new Size(55, 20));
			((Control)updnEndYear).set_TabIndex(6);
			updnEndYear.set_Value(new decimal(new int[4] { 2018, 0, 0, 0 }));
			updnEndYear.add_ValueChanged((EventHandler)updnEndYear_ValueChanged);
			((Control)updnEndYear).add_Enter((EventHandler)updnEndYear_Enter);
			((Control)updnEndYear).add_Leave((EventHandler)updnEndYear_Leave);
			((Control)updnEndYear).add_MouseDoubleClick(new MouseEventHandler(updnEndYear_MouseDoubleClick));
			((Control)updnStartMonth).set_Location(new Point(202, 2));
			((Control)updnStartMonth).set_Margin(new Padding(4, 3, 4, 3));
			updnStartMonth.set_Maximum(new decimal(new int[4] { 13, 0, 0, 0 }));
			((Control)updnStartMonth).set_Name("updnStartMonth");
			((Control)updnStartMonth).set_Size(new Size(40, 20));
			((Control)updnStartMonth).set_TabIndex(2);
			updnStartMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnStartMonth.add_ValueChanged((EventHandler)updnStartMonth_ValueChanged);
			((Control)updnStartMonth).add_Enter((EventHandler)updnStartMonth_Enter);
			((Control)updnStartYear).set_Location(new Point(145, 2));
			((Control)updnStartYear).set_Margin(new Padding(4, 3, 4, 3));
			updnStartYear.set_Maximum(new decimal(new int[4] { 9000, 0, 0, 0 }));
			updnStartYear.set_Minimum(new decimal(new int[4] { 9000, 0, 0, -2147483648 }));
			((Control)updnStartYear).set_Name("updnStartYear");
			((Control)updnStartYear).set_Size(new Size(54, 20));
			((Control)updnStartYear).set_TabIndex(1);
			updnStartYear.set_Value(new decimal(new int[4] { 2018, 0, 0, 0 }));
			updnStartYear.add_ValueChanged((EventHandler)updnStartYear_ValueChanged);
			((Control)updnStartYear).add_Enter((EventHandler)updnStartYear_Enter);
			((Control)updnStartYear).add_Leave((EventHandler)updnStartYear_Leave);
			((Control)updnStartYear).add_MouseDoubleClick(new MouseEventHandler(updnStartYear_MouseDoubleClick));
			((Control)cmdAdd1).set_BackColor(Color.Gold);
			((ButtonBase)cmdAdd1).get_FlatAppearance().set_BorderSize(0);
			((Control)cmdAdd1).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdAdd1).set_Location(new Point(35, 19));
			((Control)cmdAdd1).set_Margin(new Padding(4, 3, 4, 3));
			((Control)cmdAdd1).set_Name("cmdAdd1");
			((Control)cmdAdd1).set_Size(new Size(98, 20));
			((Control)cmdAdd1).set_TabIndex(4);
			((Control)cmdAdd1).set_Text("Add 1 day =>");
			((ButtonBase)cmdAdd1).set_UseVisualStyleBackColor(false);
			((Control)cmdAdd1).add_Click((EventHandler)cmdAdd1_Click);
			((Control)cmdCompute).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdCompute).set_Location(new Point(150, 86));
			((Control)cmdCompute).set_Margin(new Padding(4, 3, 4, 3));
			((Control)cmdCompute).set_Name("cmdCompute");
			((Control)cmdCompute).set_Size(new Size(117, 32));
			((Control)cmdCompute).set_TabIndex(5);
			((Control)cmdCompute).set_Text("&Search");
			((ButtonBase)cmdCompute).set_UseVisualStyleBackColor(true);
			((Control)cmdCompute).add_Click((EventHandler)cmdCompute_Click);
			((Control)cmdCancel).set_Location(new Point(150, 86));
			((Control)cmdCancel).set_Margin(new Padding(4, 3, 4, 3));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(117, 32));
			((Control)cmdCancel).set_TabIndex(3);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)lstResults).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstResults).set_FormattingEnabled(true);
			lstResults.set_ItemHeight(14);
			((Control)lstResults).set_Location(new Point(477, 272));
			((Control)lstResults).set_Margin(new Padding(4, 3, 4, 3));
			((Control)lstResults).set_Name("lstResults");
			lstResults.set_ScrollAlwaysVisible(true);
			((Control)lstResults).set_Size(new Size(417, 396));
			((Control)lstResults).set_TabIndex(7);
			((Control)pbarSearch).set_Location(new Point(29, 124));
			((Control)pbarSearch).set_Margin(new Padding(4, 3, 4, 3));
			((Control)pbarSearch).set_Name("pbarSearch");
			((Control)pbarSearch).set_Size(new Size(359, 12));
			((Control)pbarSearch).set_TabIndex(7);
			((Control)pbarSearch).set_Visible(false);
			((Control)cmdThisYear).set_Location(new Point(365, 12));
			((Control)cmdThisYear).set_Margin(new Padding(4, 3, 4, 3));
			((Control)cmdThisYear).set_Name("cmdThisYear");
			((Control)cmdThisYear).set_Size(new Size(48, 21));
			((Control)cmdThisYear).set_TabIndex(4);
			((Control)cmdThisYear).set_Text("2000");
			((ButtonBase)cmdThisYear).set_UseVisualStyleBackColor(true);
			((Control)cmdThisYear).add_Click((EventHandler)cmdThisYear_Click);
			((Control)cmdNextYear).set_Location(new Point(365, 33));
			((Control)cmdNextYear).set_Margin(new Padding(4, 3, 4, 3));
			((Control)cmdNextYear).set_Name("cmdNextYear");
			((Control)cmdNextYear).set_Size(new Size(48, 21));
			((Control)cmdNextYear).set_TabIndex(5);
			((Control)cmdNextYear).set_Text("2000");
			((ButtonBase)cmdNextYear).set_UseVisualStyleBackColor(true);
			((Control)cmdNextYear).add_Click((EventHandler)cmdNextYear_Click);
			((Control)cmdWholeYear).set_Location(new Point(306, 12));
			((Control)cmdWholeYear).set_Margin(new Padding(4, 3, 4, 3));
			((Control)cmdWholeYear).set_Name("cmdWholeYear");
			((Control)cmdWholeYear).set_Size(new Size(55, 21));
			((Control)cmdWholeYear).set_TabIndex(1);
			((Control)cmdWholeYear).set_Text("&Year");
			((ButtonBase)cmdWholeYear).set_UseVisualStyleBackColor(true);
			((Control)cmdWholeYear).add_Click((EventHandler)cmdWholeYear_Click);
			((Control)cmdWholeMonth).set_Location(new Point(306, 33));
			((Control)cmdWholeMonth).set_Margin(new Padding(4, 3, 4, 3));
			((Control)cmdWholeMonth).set_Name("cmdWholeMonth");
			((Control)cmdWholeMonth).set_Size(new Size(55, 21));
			((Control)cmdWholeMonth).set_TabIndex(2);
			((Control)cmdWholeMonth).set_Text("&Month");
			((ButtonBase)cmdWholeMonth).set_UseVisualStyleBackColor(true);
			((Control)cmdWholeMonth).add_Click((EventHandler)cmdWholeMonth_Click);
			((Control)cmd2yrs).set_Location(new Point(365, 54));
			((Control)cmd2yrs).set_Margin(new Padding(4, 3, 4, 3));
			((Control)cmd2yrs).set_Name("cmd2yrs");
			((Control)cmd2yrs).set_Size(new Size(48, 21));
			((Control)cmd2yrs).set_TabIndex(6);
			((Control)cmd2yrs).set_Text("2000");
			((ButtonBase)cmd2yrs).set_UseVisualStyleBackColor(true);
			((Control)cmd2yrs).add_Click((EventHandler)cmd2yrs_Click);
			((Control)lblAsteroidName).set_AutoSize(true);
			((Control)lblAsteroidName).set_Location(new Point(479, 257));
			((Control)lblAsteroidName).set_Margin(new Padding(4, 0, 4, 0));
			((Control)lblAsteroidName).set_Name("lblAsteroidName");
			((Control)lblAsteroidName).set_Size(new Size(87, 13));
			((Control)lblAsteroidName).set_TabIndex(5);
			((Control)lblAsteroidName).set_Text("Object's name");
			((Control)grpSetStarCat).set_BackColor(Color.FromArgb(220, 255, 255));
			((Control)grpSetStarCat).get_Controls().Add((Control)(object)cmdEditUserStar);
			((Control)grpSetStarCat).get_Controls().Add((Control)(object)cmbUserCat);
			((Control)grpSetStarCat).get_Controls().Add((Control)(object)panelGaiaFiles);
			((Control)grpSetStarCat).get_Controls().Add((Control)(object)optUCAC4);
			((Control)grpSetStarCat).get_Controls().Add((Control)(object)lblUserStar);
			((Control)grpSetStarCat).get_Controls().Add((Control)(object)optUserStar);
			((Control)grpSetStarCat).get_Controls().Add((Control)(object)optUserCats);
			((Control)grpSetStarCat).get_Controls().Add((Control)(object)optPPMXL);
			((Control)grpSetStarCat).get_Controls().Add((Control)(object)optTycho2);
			((Control)grpSetStarCat).get_Controls().Add((Control)(object)optGaia);
			((Control)grpSetStarCat).get_Controls().Add((Control)(object)chkGaiaPlot);
			((Control)grpSetStarCat).set_Location(new Point(14, 113));
			((Control)grpSetStarCat).set_Margin(new Padding(4, 3, 4, 3));
			((Control)grpSetStarCat).set_Name("grpSetStarCat");
			((Control)grpSetStarCat).set_Padding(new Padding(4, 3, 4, 3));
			((Control)grpSetStarCat).set_Size(new Size(420, 169));
			((Control)grpSetStarCat).set_TabIndex(1);
			grpSetStarCat.set_TabStop(false);
			((Control)grpSetStarCat).set_Text("2.  Select the star catalogue for the search");
			((Control)cmdEditUserStar).set_Location(new Point(85, 107));
			((Control)cmdEditUserStar).set_Margin(new Padding(4, 3, 4, 3));
			((Control)cmdEditUserStar).set_Name("cmdEditUserStar");
			((Control)cmdEditUserStar).set_Size(new Size(88, 20));
			((Control)cmdEditUserStar).set_TabIndex(6);
			((Control)cmdEditUserStar).set_Text("Set user star");
			((ButtonBase)cmdEditUserStar).set_UseVisualStyleBackColor(true);
			((Control)cmdEditUserStar).add_Click((EventHandler)cmdEditUserStar_Click);
			cmbUserCat.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbUserCat).set_FormattingEnabled(true);
			((Control)cmbUserCat).set_Location(new Point(108, 130));
			((Control)cmbUserCat).set_Margin(new Padding(4, 3, 4, 3));
			((Control)cmbUserCat).set_Name("cmbUserCat");
			((Control)cmbUserCat).set_Size(new Size(62, 21));
			((Control)cmbUserCat).set_TabIndex(8);
			cmbUserCat.add_SelectedIndexChanged((EventHandler)cmbUserCat_SelectedIndexChanged);
			((Control)panelGaiaFiles).get_Controls().Add((Control)(object)lstGaiaFiles);
			((Control)panelGaiaFiles).get_Controls().Add((Control)(object)label20);
			((Control)panelGaiaFiles).set_Location(new Point(215, 14));
			((Control)panelGaiaFiles).set_Margin(new Padding(4, 3, 4, 3));
			((Control)panelGaiaFiles).set_Name("panelGaiaFiles");
			((Control)panelGaiaFiles).set_Size(new Size(201, 151));
			((Control)panelGaiaFiles).set_TabIndex(13);
			((ListControl)lstGaiaFiles).set_FormattingEnabled(true);
			((Control)lstGaiaFiles).set_Location(new Point(8, 0));
			((Control)lstGaiaFiles).set_Margin(new Padding(4, 3, 4, 3));
			((Control)lstGaiaFiles).set_Name("lstGaiaFiles");
			((Control)lstGaiaFiles).set_Size(new Size(181, 147));
			((Control)lstGaiaFiles).set_TabIndex(1);
			toolTip1.SetToolTip((Control)(object)lstGaiaFiles, "Selected file will be used. Click to show sky coverage\r\nGreen = Most recent mag 16, 14, 12 & 9 files\r\nTan = User-created Gaia files\r\n");
			lstGaiaFiles.add_Click((EventHandler)lstGaiaFiles_Click);
			lstGaiaFiles.add_SelectedIndexChanged((EventHandler)lstGaiaFiles_SelectedIndexChanged);
			((Control)lstGaiaFiles).add_Leave((EventHandler)lstGaiaFiles_Leave);
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label20).set_Location(new Point(9, 3));
			((Control)label20).set_Margin(new Padding(4, 0, 4, 0));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(155, 13));
			((Control)label20).set_TabIndex(0);
			((Control)label20).set_Text("Select the Gaia file to use");
			((Control)optUCAC4).set_AutoSize(true);
			((Control)optUCAC4).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optUCAC4).set_Location(new Point(10, 65));
			((Control)optUCAC4).set_Margin(new Padding(4, 3, 4, 3));
			((Control)optUCAC4).set_Name("optUCAC4");
			((Control)optUCAC4).set_Size(new Size(60, 17));
			((Control)optUCAC4).set_TabIndex(3);
			optUCAC4.set_TabStop(true);
			((Control)optUCAC4).set_Text("UCAC4");
			((ButtonBase)optUCAC4).set_UseVisualStyleBackColor(true);
			optUCAC4.add_CheckedChanged((EventHandler)optUCAC4_CheckedChanged);
			((Control)lblUserStar).set_AutoSize(true);
			((Control)lblUserStar).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblUserStar).set_Location(new Point(7, 153));
			((Control)lblUserStar).set_Margin(new Padding(4, 0, 4, 0));
			((Control)lblUserStar).set_Name("lblUserStar");
			((Control)lblUserStar).set_Size(new Size(61, 12));
			((Control)lblUserStar).set_TabIndex(10);
			((Control)lblUserStar).set_Text("[No user star]");
			((Control)optUserStar).set_AutoSize(true);
			((Control)optUserStar).set_Enabled(false);
			((Control)optUserStar).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optUserStar).set_Location(new Point(10, 109));
			((Control)optUserStar).set_Margin(new Padding(4, 3, 4, 3));
			((Control)optUserStar).set_Name("optUserStar");
			((Control)optUserStar).set_Size(new Size(67, 17));
			((Control)optUserStar).set_TabIndex(5);
			optUserStar.set_TabStop(true);
			((Control)optUserStar).set_Text("User star");
			((ButtonBase)optUserStar).set_UseVisualStyleBackColor(true);
			optUserStar.add_CheckedChanged((EventHandler)optUserStar_CheckedChanged);
			((Control)optUserCats).set_AutoSize(true);
			((Control)optUserCats).set_Enabled(false);
			((Control)optUserCats).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optUserCats).set_Location(new Point(10, 131));
			((Control)optUserCats).set_Margin(new Padding(4, 3, 4, 3));
			((Control)optUserCats).set_Name("optUserCats");
			((Control)optUserCats).set_Size(new Size(85, 17));
			((Control)optUserCats).set_TabIndex(7);
			optUserCats.set_TabStop(true);
			((Control)optUserCats).set_Text("User catalog");
			((ButtonBase)optUserCats).set_UseVisualStyleBackColor(true);
			optUserCats.add_CheckedChanged((EventHandler)optUserCats_CheckedChanged);
			((Control)optPPMXL).set_AutoSize(true);
			((Control)optPPMXL).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optPPMXL).set_Location(new Point(10, 87));
			((Control)optPPMXL).set_Margin(new Padding(4, 3, 4, 3));
			((Control)optPPMXL).set_Name("optPPMXL");
			((Control)optPPMXL).set_Size(new Size(61, 17));
			((Control)optPPMXL).set_TabIndex(4);
			optPPMXL.set_TabStop(true);
			((Control)optPPMXL).set_Text("PPMXL");
			((ButtonBase)optPPMXL).set_UseVisualStyleBackColor(true);
			optPPMXL.add_CheckedChanged((EventHandler)optPPMXL_CheckedChanged);
			((Control)optTycho2).set_AutoSize(true);
			((Control)optTycho2).set_Enabled(false);
			((Control)optTycho2).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optTycho2).set_Location(new Point(10, 43));
			((Control)optTycho2).set_Margin(new Padding(4, 3, 4, 3));
			((Control)optTycho2).set_Name("optTycho2");
			((Control)optTycho2).set_Size(new Size(159, 17));
			((Control)optTycho2).set_TabIndex(2);
			optTycho2.set_TabStop(true);
			((Control)optTycho2).set_Text("Tycho2 (pre Gaia - obsolete)");
			((ButtonBase)optTycho2).set_UseVisualStyleBackColor(true);
			optTycho2.add_CheckedChanged((EventHandler)optTycho2_CheckedChanged);
			((Control)optGaia).set_AutoSize(true);
			((Control)optGaia).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optGaia).set_Location(new Point(10, 14));
			((Control)optGaia).set_Margin(new Padding(4, 3, 4, 3));
			((Control)optGaia).set_Name("optGaia");
			((Control)optGaia).set_Size(new Size(65, 24));
			((Control)optGaia).set_TabIndex(0);
			optGaia.set_TabStop(true);
			((Control)optGaia).set_Text("Gaia");
			((ButtonBase)optGaia).set_UseVisualStyleBackColor(true);
			optGaia.add_CheckedChanged((EventHandler)optGaia_CheckedChanged);
			((Control)chkGaiaPlot).set_AutoSize(true);
			chkGaiaPlot.set_Checked(Settings.Default.GaiaStarPlotInSearch);
			chkGaiaPlot.set_CheckState((CheckState)1);
			((Control)chkGaiaPlot).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "GaiaStarPlotInSearch", true, (DataSourceUpdateMode)1));
			((Control)chkGaiaPlot).set_Font(new Font("Microsoft Sans Serif", 6.2f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkGaiaPlot).set_Location(new Point(93, 14));
			((Control)chkGaiaPlot).set_Margin(new Padding(4, 3, 4, 3));
			((Control)chkGaiaPlot).set_Name("chkGaiaPlot");
			((Control)chkGaiaPlot).set_Size(new Size(86, 28));
			((Control)chkGaiaPlot).set_TabIndex(1);
			((Control)chkGaiaPlot).set_Text("Plot stars \r\nwhen selecting");
			((ButtonBase)chkGaiaPlot).set_UseVisualStyleBackColor(true);
			chkGaiaPlot.add_CheckedChanged((EventHandler)chkGaiaPlot_CheckedChanged);
			((Control)updnMagDrop).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MagDropLimitInPredictions", true, (DataSourceUpdateMode)1));
			updnMagDrop.set_DecimalPlaces(2);
			updnMagDrop.set_Increment(new decimal(new int[4] { 5, 0, 0, 131072 }));
			((Control)updnMagDrop).set_Location(new Point(102, 2));
			((Control)updnMagDrop).set_Margin(new Padding(4, 3, 4, 3));
			updnMagDrop.set_Maximum(new decimal(new int[4] { 10, 0, 0, 65536 }));
			((Control)updnMagDrop).set_Name("updnMagDrop");
			((Control)updnMagDrop).set_Size(new Size(49, 20));
			((Control)updnMagDrop).set_TabIndex(1);
			updnMagDrop.set_Value(Settings.Default.MagDropLimitInPredictions);
			updnMagDrop.add_ValueChanged((EventHandler)updnMagDrop_ValueChanged);
			((Control)chkMagDiff).set_AutoSize(true);
			chkMagDiff.set_Checked(Settings.Default.MagDropLimitInPredictions_Checked);
			((Control)chkMagDiff).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "MagDropLimitInPredictions_Checked", true, (DataSourceUpdateMode)1));
			((Control)chkMagDiff).set_ForeColor(Color.DarkRed);
			((Control)chkMagDiff).set_Location(new Point(9, 4));
			((Control)chkMagDiff).set_Margin(new Padding(4, 3, 4, 3));
			((Control)chkMagDiff).set_Name("chkMagDiff");
			((Control)chkMagDiff).set_Size(new Size(90, 17));
			((Control)chkMagDiff).set_TabIndex(0);
			((Control)chkMagDiff).set_Text("Mag drop <");
			((ButtonBase)chkMagDiff).set_TextAlign(ContentAlignment.BottomLeft);
			((ButtonBase)chkMagDiff).set_UseVisualStyleBackColor(true);
			chkMagDiff.add_CheckedChanged((EventHandler)chkMagDiff_CheckedChanged);
			((Control)updnMag).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "AsteroidSearchFaintStarLimit", true, (DataSourceUpdateMode)1));
			updnMag.set_DecimalPlaces(1);
			updnMag.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnMag).set_Location(new Point(130, 23));
			((Control)updnMag).set_Margin(new Padding(4, 3, 4, 3));
			updnMag.set_Maximum(new decimal(new int[4] { 21, 0, 0, 0 }));
			updnMag.set_Minimum(new decimal(new int[4] { 6, 0, 0, 0 }));
			((Control)updnMag).set_Name("updnMag");
			((Control)updnMag).set_Size(new Size(50, 20));
			((Control)updnMag).set_TabIndex(6);
			updnMag.set_Value(Settings.Default.AsteroidSearchFaintStarLimit);
			((Control)updnMag).add_Enter((EventHandler)updnMag_Enter);
			((Control)chkApplyMagLimit).set_AutoSize(true);
			chkApplyMagLimit.set_Checked(Settings.Default.AsteroidSearchExcludeStars);
			chkApplyMagLimit.set_CheckState((CheckState)1);
			((Control)chkApplyMagLimit).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidSearchExcludeStars", true, (DataSourceUpdateMode)1));
			((Control)chkApplyMagLimit).set_ForeColor(Color.DarkRed);
			((Control)chkApplyMagLimit).set_Location(new Point(9, 26));
			((Control)chkApplyMagLimit).set_Margin(new Padding(4, 3, 4, 3));
			((Control)chkApplyMagLimit).set_Name("chkApplyMagLimit");
			((Control)chkApplyMagLimit).set_Size(new Size(124, 17));
			((Control)chkApplyMagLimit).set_TabIndex(5);
			((Control)chkApplyMagLimit).set_Text("Stars fainter than");
			((ButtonBase)chkApplyMagLimit).set_UseVisualStyleBackColor(true);
			((Control)cmdSetSite).set_BackColor(Color.PaleGreen);
			((Control)cmdSetSite).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSetSite).set_ForeColor(SystemColors.ControlText);
			((Control)cmdSetSite).set_Location(new Point(356, 106));
			((Control)cmdSetSite).set_Margin(new Padding(4, 3, 4, 3));
			((Control)cmdSetSite).set_Name("cmdSetSite");
			((Control)cmdSetSite).set_Size(new Size(58, 22));
			((Control)cmdSetSite).set_TabIndex(11);
			((Control)cmdSetSite).set_Text("Set site");
			((ButtonBase)cmdSetSite).set_TextAlign(ContentAlignment.TopCenter);
			((ButtonBase)cmdSetSite).set_UseVisualStyleBackColor(false);
			((Control)cmdSetSite).add_Click((EventHandler)cmdSetSite_Click);
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_ForeColor(Color.DarkGreen);
			((Control)label15).set_Location(new Point(173, 112));
			((Control)label15).set_Margin(new Padding(4, 0, 4, 0));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(38, 13));
			((Control)label15).set_TabIndex(6);
			((Control)label15).set_Text("km of");
			((ListControl)cmbDistance).set_FormattingEnabled(true);
			cmbDistance.get_Items().AddRange(new object[8] { "300", "600", "900", "1200", "1500", "1800", "2100", "2400" });
			((Control)cmbDistance).set_Location(new Point(114, 108));
			((Control)cmbDistance).set_Margin(new Padding(4, 3, 4, 3));
			((Control)cmbDistance).set_Name("cmbDistance");
			((Control)cmbDistance).set_Size(new Size(56, 21));
			((Control)cmbDistance).set_TabIndex(5);
			cmbDistance.add_SelectedIndexChanged((EventHandler)cmbDistance_SelectedIndexChanged);
			((Control)updnSearchLatitude).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "SearchLatitude", true, (DataSourceUpdateMode)1));
			((Control)updnSearchLatitude).set_Location(new Point(303, 107));
			((Control)updnSearchLatitude).set_Margin(new Padding(4, 3, 4, 3));
			updnSearchLatitude.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			updnSearchLatitude.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)updnSearchLatitude).set_Name("updnSearchLatitude");
			((Control)updnSearchLatitude).set_Size(new Size(46, 20));
			((Control)updnSearchLatitude).set_TabIndex(10);
			updnSearchLatitude.set_Value(Settings.Default.SearchLatitude);
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label13).set_ForeColor(Color.Brown);
			((Control)label13).set_Location(new Point(276, 107));
			((Control)label13).set_Margin(new Padding(4, 0, 4, 0));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(24, 20));
			((Control)label13).set_TabIndex(9);
			((Control)label13).set_Text(" ϕ");
			((Control)updnSearchLongitude).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "SearchLongitude", true, (DataSourceUpdateMode)1));
			((Control)updnSearchLongitude).set_Location(new Point(227, 107));
			((Control)updnSearchLongitude).set_Margin(new Padding(4, 3, 4, 3));
			updnSearchLongitude.set_Maximum(new decimal(new int[4] { 200, 0, 0, 0 }));
			updnSearchLongitude.set_Minimum(new decimal(new int[4] { 200, 0, 0, -2147483648 }));
			((Control)updnSearchLongitude).set_Name("updnSearchLongitude");
			((Control)updnSearchLongitude).set_Size(new Size(52, 20));
			((Control)updnSearchLongitude).set_TabIndex(8);
			updnSearchLongitude.set_Value(Settings.Default.SearchLongitude);
			((Control)chkSiteLimit).set_AutoSize(true);
			chkSiteLimit.set_Checked(Settings.Default.SearchLimit1200km);
			((Control)chkSiteLimit).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "SearchLimit1200km", true, (DataSourceUpdateMode)1));
			((Control)chkSiteLimit).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkSiteLimit).set_ForeColor(Color.DarkGreen);
			((Control)chkSiteLimit).set_Location(new Point(10, 111));
			((Control)chkSiteLimit).set_Margin(new Padding(4, 3, 4, 3));
			((Control)chkSiteLimit).set_Name("chkSiteLimit");
			((Control)chkSiteLimit).set_Size(new Size(93, 17));
			((Control)chkSiteLimit).set_TabIndex(4);
			((Control)chkSiteLimit).set_Text("Passing within");
			((ButtonBase)chkSiteLimit).set_UseVisualStyleBackColor(true);
			((Control)lblMiss1).set_AutoSize(true);
			((Control)lblMiss1).set_Location(new Point(28, 49));
			((Control)lblMiss1).set_Margin(new Padding(4, 0, 4, 0));
			((Control)lblMiss1).set_Name("lblMiss1");
			((Control)lblMiss1).set_Size(new Size(187, 13));
			((Control)lblMiss1).set_TabIndex(0);
			((Control)lblMiss1).set_Text("Increase Earth miss distance by");
			((Control)lblMiss2).set_AutoSize(true);
			((Control)lblMiss2).set_Location(new Point(266, 49));
			((Control)lblMiss2).set_Margin(new Padding(4, 0, 4, 0));
			((Control)lblMiss2).set_Name("lblMiss2");
			((Control)lblMiss2).set_Size(new Size(65, 13));
			((Control)lblMiss2).set_TabIndex(2);
			((Control)lblMiss2).set_Text("Earth radii");
			((Control)txtOutputFile).set_BackColor(Color.LavenderBlush);
			((Control)txtOutputFile).set_Location(new Point(12, 57));
			((Control)txtOutputFile).set_Margin(new Padding(4, 3, 4, 3));
			((Control)txtOutputFile).set_Name("txtOutputFile");
			((TextBoxBase)txtOutputFile).set_ReadOnly(true);
			((Control)txtOutputFile).set_Size(new Size(398, 20));
			((Control)txtOutputFile).set_TabIndex(1);
			((Control)cmdSetOutputFile).set_BackColor(Color.Pink);
			((Control)cmdSetOutputFile).set_Location(new Point(31, 33));
			((Control)cmdSetOutputFile).set_Margin(new Padding(4, 3, 4, 3));
			((Control)cmdSetOutputFile).set_Name("cmdSetOutputFile");
			((Control)cmdSetOutputFile).set_Size(new Size(139, 23));
			((Control)cmdSetOutputFile).set_TabIndex(0);
			((Control)cmdSetOutputFile).set_Text("Set the output file");
			((ButtonBase)cmdSetOutputFile).set_UseVisualStyleBackColor(false);
			((Control)cmdSetOutputFile).add_Click((EventHandler)cmdSetOutputFile_Click);
			((Control)grpSetDateRange).set_BackColor(Color.LightGoldenrodYellow);
			((Control)grpSetDateRange).get_Controls().Add((Control)(object)cmdNow);
			((Control)grpSetDateRange).get_Controls().Add((Control)(object)cmd2yrs);
			((Control)grpSetDateRange).get_Controls().Add((Control)(object)cmdWholeMonth);
			((Control)grpSetDateRange).get_Controls().Add((Control)(object)cmdWholeYear);
			((Control)grpSetDateRange).get_Controls().Add((Control)(object)cmdNextYear);
			((Control)grpSetDateRange).get_Controls().Add((Control)(object)cmdThisYear);
			((Control)grpSetDateRange).get_Controls().Add((Control)(object)panel3);
			((Control)grpSetDateRange).set_Location(new Point(14, 28));
			((Control)grpSetDateRange).set_Margin(new Padding(4, 3, 4, 3));
			((Control)grpSetDateRange).set_Name("grpSetDateRange");
			((Control)grpSetDateRange).set_Padding(new Padding(4, 3, 4, 3));
			((Control)grpSetDateRange).set_Size(new Size(420, 81));
			((Control)grpSetDateRange).set_TabIndex(0);
			grpSetDateRange.set_TabStop(false);
			((Control)grpSetDateRange).set_Text("1.  Set the date range for the search");
			((Control)cmdNow).set_Location(new Point(306, 54));
			((Control)cmdNow).set_Margin(new Padding(4, 3, 4, 3));
			((Control)cmdNow).set_Name("cmdNow");
			((Control)cmdNow).set_Size(new Size(55, 21));
			((Control)cmdNow).set_TabIndex(3);
			((Control)cmdNow).set_Text("&Now");
			((ButtonBase)cmdNow).set_UseVisualStyleBackColor(true);
			((Control)cmdNow).add_Click((EventHandler)cmdNow_Click);
			((Control)grpSetSaveFile).set_BackColor(Color.FromArgb(205, 217, 242));
			((Control)grpSetSaveFile).get_Controls().Add((Control)(object)chkNoListDisplay);
			((Control)grpSetSaveFile).get_Controls().Add((Control)(object)chkAutoSave);
			((Control)grpSetSaveFile).get_Controls().Add((Control)(object)chkSortByDate);
			((Control)grpSetSaveFile).get_Controls().Add((Control)(object)cmdSetOutputFile);
			((Control)grpSetSaveFile).get_Controls().Add((Control)(object)txtOutputFile);
			((Control)grpSetSaveFile).set_Location(new Point(477, 28));
			((Control)grpSetSaveFile).set_Margin(new Padding(4, 3, 4, 3));
			((Control)grpSetSaveFile).set_Name("grpSetSaveFile");
			((Control)grpSetSaveFile).set_Padding(new Padding(4, 3, 4, 3));
			((Control)grpSetSaveFile).set_Size(new Size(420, 81));
			((Control)grpSetSaveFile).set_TabIndex(3);
			grpSetSaveFile.set_TabStop(false);
			((Control)grpSetSaveFile).set_Text("5.   Set the file to save the occultation elements from the search");
			((Control)chkNoListDisplay).set_AutoSize(true);
			((Control)chkNoListDisplay).set_Enabled(false);
			((Control)chkNoListDisplay).set_Location(new Point(251, 19));
			((Control)chkNoListDisplay).set_Margin(new Padding(4, 3, 4, 3));
			((Control)chkNoListDisplay).set_Name("chkNoListDisplay");
			((Control)chkNoListDisplay).set_Size(new Size(161, 30));
			((Control)chkNoListDisplay).set_TabIndex(15);
			((Control)chkNoListDisplay).set_Text("Do not show the search\r\nresults in List && Display");
			toolTip1.SetToolTip((Control)(object)chkNoListDisplay, componentResourceManager.GetString("chkNoListDisplay.ToolTip"));
			((ButtonBase)chkNoListDisplay).set_UseVisualStyleBackColor(true);
			((Control)chkAutoSave).set_AutoSize(true);
			chkAutoSave.set_Checked(Settings.Default.AsteroidSearchAutoSave);
			((Control)chkAutoSave).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidSearchAutoSave", true, (DataSourceUpdateMode)1));
			((Control)chkAutoSave).set_Location(new Point(12, 17));
			((Control)chkAutoSave).set_Margin(new Padding(4, 3, 4, 3));
			((Control)chkAutoSave).set_Name("chkAutoSave");
			((Control)chkAutoSave).set_Size(new Size(237, 17));
			((Control)chkAutoSave).set_TabIndex(14);
			((Control)chkAutoSave).set_Text("Automatically save the search results");
			((ButtonBase)chkAutoSave).set_UseVisualStyleBackColor(true);
			chkAutoSave.add_CheckedChanged((EventHandler)chkAutoSave_CheckedChanged);
			((Control)chkSortByDate).set_AutoSize(true);
			chkSortByDate.set_Checked(Settings.Default.AsteroidSearchDateSort);
			((Control)chkSortByDate).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidSearchDateSort", true, (DataSourceUpdateMode)1));
			((Control)chkSortByDate).set_Enabled(false);
			((Control)chkSortByDate).set_Location(new Point(172, 40));
			((Control)chkSortByDate).set_Margin(new Padding(4, 3, 4, 3));
			((Control)chkSortByDate).set_Name("chkSortByDate");
			((Control)chkSortByDate).set_Size(new Size(138, 17));
			((Control)chkSortByDate).set_TabIndex(6);
			((Control)chkSortByDate).set_Text("Sort the file by date");
			((ButtonBase)chkSortByDate).set_UseVisualStyleBackColor(true);
			((Control)chkSortByDate).set_Visible(false);
			((Control)groupBox4).set_BackColor(Color.MistyRose);
			((Control)groupBox4).get_Controls().Add((Control)(object)lblXYZversion);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmdRegenerateXYZ);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmdJD248);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmdJD247);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmdJD246);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmdJD245);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmdJD244);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmdJD243);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmdJD242);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmdJD241);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmdJD240);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmdJD239);
			((Control)groupBox4).set_Location(new Point(14, 671));
			((Control)groupBox4).set_Margin(new Padding(4, 3, 4, 3));
			((Control)groupBox4).set_Name("groupBox4");
			((Control)groupBox4).set_Padding(new Padding(4, 3, 4, 3));
			((Control)groupBox4).set_Size(new Size(882, 93));
			((Control)groupBox4).set_TabIndex(8);
			groupBox4.set_TabStop(false);
			((Control)groupBox4).set_Text("Note:  Search must be within the interval covered by the depressed buttons. Press buttons as needed. ");
			((Control)lblXYZversion).set_AutoSize(true);
			((Control)lblXYZversion).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblXYZversion).set_Location(new Point(670, 45));
			((Control)lblXYZversion).set_Margin(new Padding(4, 0, 4, 0));
			((Control)lblXYZversion).set_Name("lblXYZversion");
			((Control)lblXYZversion).set_Size(new Size(62, 13));
			((Control)lblXYZversion).set_TabIndex(11);
			((Control)lblXYZversion).set_Text("Updated to ");
			((Control)cmdRegenerateXYZ).set_BackColor(Color.Turquoise);
			((Control)cmdRegenerateXYZ).set_Location(new Point(666, 8));
			((Control)cmdRegenerateXYZ).set_Margin(new Padding(4, 3, 4, 3));
			((Control)cmdRegenerateXYZ).set_Name("cmdRegenerateXYZ");
			((Control)cmdRegenerateXYZ).set_Size(new Size(187, 37));
			((Control)cmdRegenerateXYZ).set_TabIndex(10);
			((Control)cmdRegenerateXYZ).set_Text("Re-generate all using the current DE ephemeris");
			toolTip1.SetToolTip((Control)(object)cmdRegenerateXYZ, "The current DE ephemeris is given at the top right of this form, and the bottom left of the main Occult form");
			((ButtonBase)cmdRegenerateXYZ).set_UseVisualStyleBackColor(false);
			((Control)cmdRegenerateXYZ).add_Click((EventHandler)cmdRegenerateXYZ_Click);
			((Control)cmdJD248).set_Location(new Point(666, 65));
			((Control)cmdJD248).set_Margin(new Padding(4, 3, 4, 3));
			((Control)cmdJD248).set_Name("cmdJD248");
			((Control)cmdJD248).set_Size(new Size(187, 22));
			((Control)cmdJD248).set_TabIndex(9);
			((Control)cmdJD248).set_Text("2077 Nov 28 == 2105 Apr 16");
			((ButtonBase)cmdJD248).set_UseVisualStyleBackColor(true);
			((Control)cmdJD248).add_Click((EventHandler)cmdJD248_Click);
			((Control)cmdJD247).set_Location(new Point(454, 65));
			((Control)cmdJD247).set_Margin(new Padding(4, 3, 4, 3));
			((Control)cmdJD247).set_Name("cmdJD247");
			((Control)cmdJD247).set_Size(new Size(187, 22));
			((Control)cmdJD247).set_TabIndex(8);
			((Control)cmdJD247).set_Text("2050 Jul 13 == 2077 Nov 28");
			((ButtonBase)cmdJD247).set_UseVisualStyleBackColor(true);
			((Control)cmdJD247).add_Click((EventHandler)cmdJD247_Click);
			((Control)cmdJD246).set_Location(new Point(454, 43));
			((Control)cmdJD246).set_Margin(new Padding(4, 3, 4, 3));
			((Control)cmdJD246).set_Name("cmdJD246");
			((Control)cmdJD246).set_Size(new Size(187, 22));
			((Control)cmdJD246).set_TabIndex(7);
			((Control)cmdJD246).set_Text("2023 Feb 25 == 2050 Jul 13");
			((ButtonBase)cmdJD246).set_UseVisualStyleBackColor(true);
			((Control)cmdJD246).add_Click((EventHandler)cmdJD246_Click);
			((Control)cmdJD245).set_Location(new Point(454, 21));
			((Control)cmdJD245).set_Margin(new Padding(4, 3, 4, 3));
			((Control)cmdJD245).set_Name("cmdJD245");
			((Control)cmdJD245).set_Size(new Size(187, 22));
			((Control)cmdJD245).set_TabIndex(6);
			((Control)cmdJD245).set_Text("1995 Oct 10 == 2023 Feb 25");
			((ButtonBase)cmdJD245).set_UseVisualStyleBackColor(true);
			((Control)cmdJD245).add_Click((EventHandler)cmdJD245_Click);
			((Control)cmdJD244).set_Location(new Point(236, 65));
			((Control)cmdJD244).set_Margin(new Padding(4, 3, 4, 3));
			((Control)cmdJD244).set_Name("cmdJD244");
			((Control)cmdJD244).set_Size(new Size(187, 22));
			((Control)cmdJD244).set_TabIndex(5);
			((Control)cmdJD244).set_Text("1968 May 24 == 1995 Oct 10");
			((ButtonBase)cmdJD244).set_UseVisualStyleBackColor(true);
			((Control)cmdJD244).add_Click((EventHandler)cmdJD244_Click);
			((Control)cmdJD243).set_Location(new Point(236, 43));
			((Control)cmdJD243).set_Margin(new Padding(4, 3, 4, 3));
			((Control)cmdJD243).set_Name("cmdJD243");
			((Control)cmdJD243).set_Size(new Size(187, 22));
			((Control)cmdJD243).set_TabIndex(4);
			((Control)cmdJD243).set_Text("1941 Jan 06 == 1968 May 24");
			((ButtonBase)cmdJD243).set_UseVisualStyleBackColor(true);
			((Control)cmdJD243).add_Click((EventHandler)cmdJD243_Click);
			((Control)cmdJD242).set_Location(new Point(236, 21));
			((Control)cmdJD242).set_Margin(new Padding(4, 3, 4, 3));
			((Control)cmdJD242).set_Name("cmdJD242");
			((Control)cmdJD242).set_Size(new Size(187, 22));
			((Control)cmdJD242).set_TabIndex(3);
			((Control)cmdJD242).set_Text("1913 Aug 21 == 1941 Jan 06");
			((ButtonBase)cmdJD242).set_UseVisualStyleBackColor(true);
			((Control)cmdJD242).add_Click((EventHandler)cmdJD242_Click);
			((Control)cmdJD241).set_Location(new Point(18, 65));
			((Control)cmdJD241).set_Margin(new Padding(4, 3, 4, 3));
			((Control)cmdJD241).set_Name("cmdJD241");
			((Control)cmdJD241).set_Size(new Size(187, 22));
			((Control)cmdJD241).set_TabIndex(2);
			((Control)cmdJD241).set_Text("1886 Apr 04 == 1913 Aug 21");
			((ButtonBase)cmdJD241).set_UseVisualStyleBackColor(true);
			((Control)cmdJD241).add_Click((EventHandler)cmdJD241_Click);
			((Control)cmdJD240).set_Location(new Point(18, 43));
			((Control)cmdJD240).set_Margin(new Padding(4, 3, 4, 3));
			((Control)cmdJD240).set_Name("cmdJD240");
			((Control)cmdJD240).set_Size(new Size(187, 22));
			((Control)cmdJD240).set_TabIndex(1);
			((Control)cmdJD240).set_Text("1858 Nov 17 == 1886 Apr 04");
			((ButtonBase)cmdJD240).set_UseVisualStyleBackColor(true);
			((Control)cmdJD240).add_Click((EventHandler)cmdJD240_Click);
			((Control)cmdJD239).set_Location(new Point(18, 19));
			((Control)cmdJD239).set_Margin(new Padding(4, 3, 4, 3));
			((Control)cmdJD239).set_Name("cmdJD239");
			((Control)cmdJD239).set_Size(new Size(187, 22));
			((Control)cmdJD239).set_TabIndex(0);
			((Control)cmdJD239).set_Text("1831 Jul 02 == 1858 Nov 17");
			((ButtonBase)cmdJD239).set_UseVisualStyleBackColor(true);
			((Control)cmdJD239).add_Click((EventHandler)cmdJD239_Click);
			((ListControl)cmbAsteroidClasses).set_FormattingEnabled(true);
			cmbAsteroidClasses.get_Items().AddRange(new object[9] { "All", "Amor", "Apollo", "Aten", "Binary", "Centaur", "PHA", "TNO", "Trojan" });
			((Control)cmbAsteroidClasses).set_Location(new Point(8, 44));
			((Control)cmbAsteroidClasses).set_Margin(new Padding(4, 3, 4, 3));
			((Control)cmbAsteroidClasses).set_Name("cmbAsteroidClasses");
			((Control)cmbAsteroidClasses).set_Size(new Size(74, 21));
			((Control)cmbAsteroidClasses).set_TabIndex(10);
			cmbAsteroidClasses.add_SelectedIndexChanged((EventHandler)cmbAsteroidClasses_SelectedIndexChanged);
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_ForeColor(Color.DarkRed);
			((Control)label25).set_Location(new Point(364, 26));
			((Control)label25).set_Margin(new Padding(4, 0, 4, 0));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(33, 13));
			((Control)label25).set_TabIndex(9);
			((Control)label25).set_Text("secs");
			((Control)updnMinimumDuration).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "AsteroidSearchMinimumDuration", true, (DataSourceUpdateMode)1));
			updnMinimumDuration.set_DecimalPlaces(2);
			updnMinimumDuration.set_Increment(new decimal(new int[4] { 5, 0, 0, 131072 }));
			((Control)updnMinimumDuration).set_Location(new Point(313, 24));
			((Control)updnMinimumDuration).set_Margin(new Padding(4, 3, 4, 3));
			updnMinimumDuration.set_Maximum(new decimal(new int[4] { 4, 0, 0, 0 }));
			updnMinimumDuration.set_Minimum(new decimal(new int[4] { 1, 0, 0, 131072 }));
			((Control)updnMinimumDuration).set_Name("updnMinimumDuration");
			((Control)updnMinimumDuration).set_Size(new Size(49, 20));
			((Control)updnMinimumDuration).set_TabIndex(8);
			updnMinimumDuration.set_Value(Settings.Default.AsteroidSearchMinimumDuration);
			chkMinDuration.set_AutoCheck(false);
			((Control)chkMinDuration).set_AutoSize(true);
			chkMinDuration.set_Checked(Settings.Default.AsteroidSearch_Duration);
			chkMinDuration.set_CheckState((CheckState)1);
			((Control)chkMinDuration).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidSearch_Duration", true, (DataSourceUpdateMode)1));
			((Control)chkMinDuration).set_ForeColor(Color.DarkRed);
			((Control)chkMinDuration).set_Location(new Point(227, 26));
			((Control)chkMinDuration).set_Margin(new Padding(4, 3, 4, 3));
			((Control)chkMinDuration).set_Name("chkMinDuration");
			((Control)chkMinDuration).set_Size(new Size(85, 17));
			((Control)chkMinDuration).set_TabIndex(7);
			((Control)chkMinDuration).set_Text("Duration <");
			((ButtonBase)chkMinDuration).set_UseVisualStyleBackColor(true);
			((Control)chkMinDuration).add_Click((EventHandler)chkMinDuration_Click);
			((Control)lblTotalObjects).set_Location(new Point(194, 22));
			((Control)lblTotalObjects).set_Margin(new Padding(4, 0, 4, 0));
			((Control)lblTotalObjects).set_Name("lblTotalObjects");
			((Control)lblTotalObjects).set_Size(new Size(140, 13));
			((Control)lblTotalObjects).set_TabIndex(6);
			((Control)lblTotalObjects).set_Text("(  asteroids)");
			lblTotalObjects.set_TextAlign(ContentAlignment.TopCenter);
			chkDiameterLimit.set_AutoCheck(false);
			((Control)chkDiameterLimit).set_AutoSize(true);
			chkDiameterLimit.set_Checked(Settings.Default.AsteroidSearch_Diameter);
			chkDiameterLimit.set_CheckState((CheckState)1);
			((Control)chkDiameterLimit).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidSearch_Diameter", true, (DataSourceUpdateMode)1));
			((Control)chkDiameterLimit).set_ForeColor(Color.DarkRed);
			((Control)chkDiameterLimit).set_Location(new Point(227, 4));
			((Control)chkDiameterLimit).set_Margin(new Padding(4, 3, 4, 3));
			((Control)chkDiameterLimit).set_Name("chkDiameterLimit");
			((Control)chkDiameterLimit).set_Size(new Size(87, 17));
			((Control)chkDiameterLimit).set_TabIndex(2);
			((Control)chkDiameterLimit).set_Text("Diameter <");
			((ButtonBase)chkDiameterLimit).set_UseVisualStyleBackColor(true);
			((Control)chkDiameterLimit).add_Click((EventHandler)chkDiameterLimit_Click);
			((Control)chkUserElements).set_AutoSize(true);
			((Control)chkUserElements).set_Location(new Point(6, 75));
			((Control)chkUserElements).set_Margin(new Padding(4, 3, 4, 3));
			((Control)chkUserElements).set_Name("chkUserElements");
			((Control)chkUserElements).set_Size(new Size(178, 17));
			((Control)chkUserElements).set_TabIndex(3);
			((Control)chkUserElements).set_Text("use USER file of elements ");
			((ButtonBase)chkUserElements).set_UseVisualStyleBackColor(true);
			chkUserElements.add_CheckedChanged((EventHandler)chkUserElements_CheckedChanged);
			((Control)lblSearchRange).set_Location(new Point(124, 5));
			((Control)lblSearchRange).set_Margin(new Padding(4, 0, 4, 0));
			((Control)lblSearchRange).set_Name("lblSearchRange");
			((Control)lblSearchRange).set_Size(new Size(280, 13));
			((Control)lblSearchRange).set_TabIndex(1);
			((Control)lblSearchRange).set_Text("xxx to xxx");
			lblSearchRange.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)cmdSelectAsteroid).set_BackColor(Color.Blue);
			((Control)cmdSelectAsteroid).set_ForeColor(Color.Yellow);
			((Control)cmdSelectAsteroid).set_Location(new Point(5, 1));
			((Control)cmdSelectAsteroid).set_Margin(new Padding(4, 3, 4, 3));
			((Control)cmdSelectAsteroid).set_Name("cmdSelectAsteroid");
			((Control)cmdSelectAsteroid).set_Size(new Size(119, 37));
			((Control)cmdSelectAsteroid).set_TabIndex(0);
			((Control)cmdSelectAsteroid).set_Text("Select asteroid\r\nor Set a comet");
			((ButtonBase)cmdSelectAsteroid).set_UseVisualStyleBackColor(false);
			((Control)cmdSelectAsteroid).add_Click((EventHandler)cmdSelectAsteroid_Click);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_ForeColor(Color.DarkRed);
			((Control)label5).set_Location(new Point(364, 5));
			((Control)label5).set_Margin(new Padding(4, 0, 4, 0));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(23, 13));
			((Control)label5).set_TabIndex(4);
			((Control)label5).set_Text("km");
			((Control)updnDiameter).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "AsteroidSearchMinDiameter", true, (DataSourceUpdateMode)1));
			((Control)updnDiameter).set_Location(new Point(316, 2));
			((Control)updnDiameter).set_Margin(new Padding(4, 3, 4, 3));
			updnDiameter.set_Maximum(new decimal(new int[4] { 999, 0, 0, 0 }));
			((Control)updnDiameter).set_Name("updnDiameter");
			((Control)updnDiameter).set_Size(new Size(46, 20));
			((Control)updnDiameter).set_TabIndex(3);
			updnDiameter.set_Value(Settings.Default.AsteroidSearchMinDiameter);
			updnDiameter.add_ValueChanged((EventHandler)updnDiameter_ValueChanged);
			((Control)updnDiameter).add_Enter((EventHandler)updnDiameter_Enter);
			((Control)lblEphemerisSource).set_BackColor(Color.Transparent);
			((Control)lblEphemerisSource).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblEphemerisSource).set_Location(new Point(698, 5));
			((Control)lblEphemerisSource).set_Margin(new Padding(4, 0, 4, 0));
			((Control)lblEphemerisSource).set_Name("lblEphemerisSource");
			((Control)lblEphemerisSource).set_Size(new Size(198, 13));
			((Control)lblEphemerisSource).set_TabIndex(11);
			((Control)lblEphemerisSource).set_Text("VSOP87A");
			lblEphemerisSource.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)txtPlanetMagLimit).set_Location(new Point(345, 11));
			((Control)txtPlanetMagLimit).set_Margin(new Padding(4, 3, 4, 3));
			((Control)txtPlanetMagLimit).set_Name("txtPlanetMagLimit");
			((TextBoxBase)txtPlanetMagLimit).set_ReadOnly(true);
			((Control)txtPlanetMagLimit).set_Size(new Size(41, 20));
			((Control)txtPlanetMagLimit).set_TabIndex(5);
			((Control)chkPlanetMagLimit).set_AutoSize(true);
			chkPlanetMagLimit.set_Checked(Settings.Default.PlanetSearchLimitStarMag);
			chkPlanetMagLimit.set_CheckState((CheckState)1);
			((Control)chkPlanetMagLimit).set_Location(new Point(160, 6));
			((Control)chkPlanetMagLimit).set_Margin(new Padding(4, 3, 4, 3));
			((Control)chkPlanetMagLimit).set_Name("chkPlanetMagLimit");
			((Control)chkPlanetMagLimit).set_Size(new Size(184, 30));
			((Control)chkPlanetMagLimit).set_TabIndex(4);
			((Control)chkPlanetMagLimit).set_Text("For main planet, exclude \r\nstars fainter than magnitude");
			((ButtonBase)chkPlanetMagLimit).set_UseVisualStyleBackColor(true);
			((Control)lblMoons).set_Location(new Point(18, 84));
			((Control)lblMoons).set_Margin(new Padding(4, 0, 4, 0));
			((Control)lblMoons).set_Name("lblMoons");
			((Control)lblMoons).set_Size(new Size(352, 40));
			((Control)lblMoons).set_TabIndex(1);
			((Control)lblMoons).set_Text("label3");
			lblMoons.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)chkIncludeSmallMoons).set_AutoSize(true);
			chkIncludeSmallMoons.set_Checked(Settings.Default.PlanetSearchIncludeSmallMoons);
			((Control)chkIncludeSmallMoons).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "PlanetSearchIncludeSmallMoons", true, (DataSourceUpdateMode)1));
			((Control)chkIncludeSmallMoons).set_Location(new Point(167, 38));
			((Control)chkIncludeSmallMoons).set_Margin(new Padding(4, 3, 4, 3));
			((Control)chkIncludeSmallMoons).set_Name("chkIncludeSmallMoons");
			((Control)chkIncludeSmallMoons).set_Size(new Size(140, 17));
			((Control)chkIncludeSmallMoons).set_TabIndex(2);
			((Control)chkIncludeSmallMoons).set_Text("Include small moons");
			((ButtonBase)chkIncludeSmallMoons).set_UseVisualStyleBackColor(true);
			chkIncludeSmallMoons.add_CheckedChanged((EventHandler)chkIncludeSmallMoons_CheckedChanged);
			((Control)chkIncludeMainMoons).set_AutoSize(true);
			chkIncludeMainMoons.set_Checked(Settings.Default.PlanetSearchIncludeMainMoons);
			chkIncludeMainMoons.set_CheckState((CheckState)1);
			((Control)chkIncludeMainMoons).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "PlanetSearchIncludeMainMoons", true, (DataSourceUpdateMode)1));
			((Control)chkIncludeMainMoons).set_Location(new Point(5, 39));
			((Control)chkIncludeMainMoons).set_Margin(new Padding(4, 3, 4, 3));
			((Control)chkIncludeMainMoons).set_Name("chkIncludeMainMoons");
			((Control)chkIncludeMainMoons).set_Size(new Size(138, 17));
			((Control)chkIncludeMainMoons).set_TabIndex(0);
			((Control)chkIncludeMainMoons).set_Text("Include main moons");
			((ButtonBase)chkIncludeMainMoons).set_UseVisualStyleBackColor(true);
			chkIncludeMainMoons.add_CheckedChanged((EventHandler)chkIncludeMainMoons_CheckedChanged);
			((ListControl)cmbPlanets).set_FormattingEnabled(true);
			cmbPlanets.get_Items().AddRange(new object[9] { "Mercury -> Pluto", "Mercury", "Venus", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune", "Pluto" });
			((Control)cmbPlanets).set_Location(new Point(5, 11));
			((Control)cmbPlanets).set_Margin(new Padding(4, 3, 4, 3));
			((Control)cmbPlanets).set_Name("cmbPlanets");
			((Control)cmbPlanets).set_Size(new Size(125, 21));
			((Control)cmbPlanets).set_TabIndex(0);
			cmbPlanets.add_SelectedIndexChanged((EventHandler)cmbPlanets_SelectedIndexChanged);
			((Control)grpSetAsteroidsPlanets).set_BackColor(Color.FromArgb(255, 224, 202));
			((Control)grpSetAsteroidsPlanets).get_Controls().Add((Control)(object)pnlSelectAsteroids);
			((Control)grpSetAsteroidsPlanets).get_Controls().Add((Control)(object)optPlanets);
			((Control)grpSetAsteroidsPlanets).get_Controls().Add((Control)(object)optMinorPlanets);
			((Control)grpSetAsteroidsPlanets).get_Controls().Add((Control)(object)pnlSelectPlanets);
			grpSetAsteroidsPlanets.set_FlatStyle((FlatStyle)1);
			((Control)grpSetAsteroidsPlanets).set_Location(new Point(14, 422));
			((Control)grpSetAsteroidsPlanets).set_Margin(new Padding(4, 3, 4, 3));
			((Control)grpSetAsteroidsPlanets).set_Name("grpSetAsteroidsPlanets");
			((Control)grpSetAsteroidsPlanets).set_Padding(new Padding(4, 3, 4, 3));
			((Control)grpSetAsteroidsPlanets).set_Size(new Size(420, 243));
			((Control)grpSetAsteroidsPlanets).set_TabIndex(2);
			grpSetAsteroidsPlanets.set_TabStop(false);
			((Control)grpSetAsteroidsPlanets).set_Text("4.  Select asteroids to search, or planets [including Pluto] to search");
			((Control)pnlSelectAsteroids).set_BackColor(Color.FromArgb(255, 224, 202));
			((Control)pnlSelectAsteroids).get_Controls().Add((Control)(object)panel2);
			((Control)pnlSelectAsteroids).get_Controls().Add((Control)(object)pnlMiriade);
			((Control)pnlSelectAsteroids).get_Controls().Add((Control)(object)chkUserElements);
			((Control)pnlSelectAsteroids).get_Controls().Add((Control)(object)cmdSelectAsteroid);
			((Control)pnlSelectAsteroids).get_Controls().Add((Control)(object)lblSearchRange);
			((Control)pnlSelectAsteroids).get_Controls().Add((Control)(object)lblTotalObjects);
			((Control)pnlSelectAsteroids).set_Location(new Point(13, 42));
			((Control)pnlSelectAsteroids).set_Margin(new Padding(4, 3, 4, 3));
			((Control)pnlSelectAsteroids).set_Name("pnlSelectAsteroids");
			((Control)pnlSelectAsteroids).set_Size(new Size(408, 199));
			((Control)pnlSelectAsteroids).set_TabIndex(0);
			((Control)panel2).set_BackColor(Color.FromArgb(235, 204, 182));
			panel2.set_BorderStyle((BorderStyle)2);
			((Control)panel2).get_Controls().Add((Control)(object)chkUseHorizonsEphemeris);
			((Control)panel2).get_Controls().Add((Control)(object)chkAutoSetHorizons);
			((Control)panel2).get_Controls().Add((Control)(object)chkHorizons);
			((Control)panel2).get_Controls().Add((Control)(object)label23);
			((Control)panel2).set_Location(new Point(5, 38));
			((Control)panel2).set_Margin(new Padding(4, 3, 4, 3));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(400, 35));
			((Control)panel2).set_TabIndex(16);
			chkUseHorizonsEphemeris.set_AutoCheck(false);
			((Control)chkUseHorizonsEphemeris).set_AutoSize(true);
			chkUseHorizonsEphemeris.set_Checked(Settings.Default.AsteroidSearch_UseHorizonsEphem);
			chkUseHorizonsEphemeris.set_CheckState((CheckState)1);
			((Control)chkUseHorizonsEphemeris).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidSearch_UseHorizonsEphem", true, (DataSourceUpdateMode)1));
			((Control)chkUseHorizonsEphemeris).set_ForeColor(Color.Red);
			((Control)chkUseHorizonsEphemeris).set_Location(new Point(1, 16));
			((Control)chkUseHorizonsEphemeris).set_Margin(new Padding(4, 3, 4, 3));
			((Control)chkUseHorizonsEphemeris).set_Name("chkUseHorizonsEphemeris");
			((Control)chkUseHorizonsEphemeris).set_Size(new Size(303, 17));
			((Control)chkUseHorizonsEphemeris).set_TabIndex(14);
			((Control)chkUseHorizonsEphemeris).set_Text("use Horizons ephemeris (predictions will be slow)");
			((ButtonBase)chkUseHorizonsEphemeris).set_UseVisualStyleBackColor(true);
			((Control)chkUseHorizonsEphemeris).add_Click((EventHandler)chkUseHorizonsEphemeris_Click);
			chkAutoSetHorizons.set_AutoCheck(false);
			((Control)chkAutoSetHorizons).set_AutoSize(true);
			chkAutoSetHorizons.set_Checked(true);
			chkAutoSetHorizons.set_CheckState((CheckState)1);
			((Control)chkAutoSetHorizons).set_Font(new Font("Arial Narrow", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkAutoSetHorizons).set_ForeColor(Color.Indigo);
			((Control)chkAutoSetHorizons).set_Location(new Point(154, 0));
			((Control)chkAutoSetHorizons).set_Margin(new Padding(4, 3, 4, 3));
			((Control)chkAutoSetHorizons).set_Name("chkAutoSetHorizons");
			((Control)chkAutoSetHorizons).set_Size(new Size(104, 19));
			((Control)chkAutoSetHorizons).set_TabIndex(13);
			((Control)chkAutoSetHorizons).set_Text("Set automatically");
			toolTip1.SetToolTip((Control)(object)chkAutoSetHorizons, "This setting controls whether the 'use Horizons elements'\r\nsetting will be automatically set when searching.\r\n\r\nClear this setting when using the USER file of elements");
			((ButtonBase)chkAutoSetHorizons).set_UseVisualStyleBackColor(true);
			((Control)chkAutoSetHorizons).add_MouseClick(new MouseEventHandler(chkAutoSetHorizons_MouseClick));
			((Control)chkHorizons).set_AutoSize(true);
			((Control)chkHorizons).set_Enabled(false);
			((Control)chkHorizons).set_Location(new Point(1, 0));
			((Control)chkHorizons).set_Margin(new Padding(4, 3, 4, 3));
			((Control)chkHorizons).set_Name("chkHorizons");
			((Control)chkHorizons).set_Size(new Size(153, 17));
			((Control)chkHorizons).set_TabIndex(2);
			((Control)chkHorizons).set_Text("use Horizons elements");
			((ButtonBase)chkHorizons).set_UseVisualStyleBackColor(true);
			chkHorizons.add_CheckedChanged((EventHandler)chkHorizons_CheckedChanged);
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_ForeColor(Color.DarkSlateGray);
			((Control)label23).set_Location(new Point(273, 0));
			((Control)label23).set_Margin(new Padding(4, 0, 4, 0));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(120, 26));
			((Control)label23).set_TabIndex(15);
			((Control)label23).set_Text("For searches of one\r\nasteroid only");
			label23.set_TextAlign(ContentAlignment.MiddleRight);
			pnlMiriade.set_BorderStyle((BorderStyle)2);
			((Control)pnlMiriade).get_Controls().Add((Control)(object)lblInternetWarning);
			((Control)pnlMiriade).get_Controls().Add((Control)(object)cmdGetMiriadeSolutions);
			((Control)pnlMiriade).get_Controls().Add((Control)(object)cmdMiriadeUpdateNow);
			((Control)pnlMiriade).get_Controls().Add((Control)(object)lblUpdateDate);
			((Control)pnlMiriade).get_Controls().Add((Control)(object)label11);
			((Control)pnlMiriade).get_Controls().Add((Control)(object)label10);
			((Control)pnlMiriade).get_Controls().Add((Control)(object)chkSaveLast);
			((Control)pnlMiriade).get_Controls().Add((Control)(object)chkOneOrbitOnly);
			((Control)pnlMiriade).get_Controls().Add((Control)(object)chkNoMiriadeOrbits);
			((Control)pnlMiriade).get_Controls().Add((Control)(object)label4);
			((Control)pnlMiriade).get_Controls().Add((Control)(object)lstMiriadeAsteroids);
			((Control)pnlMiriade).set_Location(new Point(5, 93));
			((Control)pnlMiriade).set_Margin(new Padding(4, 3, 4, 3));
			((Control)pnlMiriade).set_Name("pnlMiriade");
			((Control)pnlMiriade).set_Size(new Size(400, 102));
			((Control)pnlMiriade).set_TabIndex(0);
			((Control)lblInternetWarning).set_AutoSize(true);
			((Control)lblInternetWarning).set_BackColor(Color.MediumSpringGreen);
			((Control)lblInternetWarning).set_ForeColor(Color.Red);
			((Control)lblInternetWarning).set_Location(new Point(9, 78));
			((Control)lblInternetWarning).set_Margin(new Padding(4, 0, 4, 0));
			((Control)lblInternetWarning).set_Name("lblInternetWarning");
			((Control)lblInternetWarning).set_Size(new Size(389, 13));
			((Control)lblInternetWarning).set_TabIndex(10);
			((Control)lblInternetWarning).set_Text("To get binary asteroid data from Miriade, you need Internet access!");
			((Control)cmdGetMiriadeSolutions).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdGetMiriadeSolutions).set_Location(new Point(211, 48));
			((Control)cmdGetMiriadeSolutions).set_Margin(new Padding(4, 3, 4, 3));
			((Control)cmdGetMiriadeSolutions).set_Name("cmdGetMiriadeSolutions");
			((Control)cmdGetMiriadeSolutions).set_Size(new Size(138, 20));
			((Control)cmdGetMiriadeSolutions).set_TabIndex(5);
			((Control)cmdGetMiriadeSolutions).set_Text("Get solution details");
			((ButtonBase)cmdGetMiriadeSolutions).set_UseVisualStyleBackColor(true);
			((Control)cmdGetMiriadeSolutions).add_Click((EventHandler)cmdGetMiriadeSolutions_Click);
			((Control)cmdMiriadeUpdateNow).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdMiriadeUpdateNow).set_Location(new Point(96, 48));
			((Control)cmdMiriadeUpdateNow).set_Margin(new Padding(4, 3, 4, 3));
			((Control)cmdMiriadeUpdateNow).set_Name("cmdMiriadeUpdateNow");
			((Control)cmdMiriadeUpdateNow).set_Size(new Size(92, 20));
			((Control)cmdMiriadeUpdateNow).set_TabIndex(4);
			((Control)cmdMiriadeUpdateNow).set_Text("Update list");
			((ButtonBase)cmdMiriadeUpdateNow).set_UseVisualStyleBackColor(true);
			((Control)cmdMiriadeUpdateNow).add_Click((EventHandler)cmdMiriadeUpdateNow_Click);
			((Control)lblUpdateDate).set_AutoSize(true);
			((Control)lblUpdateDate).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblUpdateDate).set_Location(new Point(102, 83));
			((Control)lblUpdateDate).set_Margin(new Padding(4, 0, 4, 0));
			((Control)lblUpdateDate).set_Name("lblUpdateDate");
			((Control)lblUpdateDate).set_Size(new Size(53, 12));
			((Control)lblUpdateDate).set_TabIndex(8);
			((Control)lblUpdateDate).set_Text("Last update");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label11).set_Location(new Point(102, 70));
			((Control)label11).set_Margin(new Padding(4, 0, 4, 0));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(53, 12));
			((Control)label11).set_TabIndex(6);
			((Control)label11).set_Text("Last update");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label10).set_Location(new Point(229, 85));
			((Control)label10).set_Margin(new Padding(4, 0, 4, 0));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(145, 12));
			((Control)label10).set_TabIndex(9);
			((Control)label10).set_Text("[For test and monitoring purposes]");
			((Control)chkSaveLast).set_AutoSize(true);
			((Control)chkSaveLast).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkSaveLast).set_Location(new Point(211, 70));
			((Control)chkSaveLast).set_Margin(new Padding(4, 3, 4, 3));
			((Control)chkSaveLast).set_Name("chkSaveLast");
			((Control)chkSaveLast).set_Size(new Size(153, 17));
			((Control)chkSaveLast).set_TabIndex(7);
			((Control)chkSaveLast).set_Text("Save last Miriade response");
			((ButtonBase)chkSaveLast).set_UseVisualStyleBackColor(true);
			((Control)chkOneOrbitOnly).set_AutoSize(true);
			chkOneOrbitOnly.set_Checked(Settings.Default.MiriadeBestOrbitOnly);
			chkOneOrbitOnly.set_CheckState((CheckState)1);
			((Control)chkOneOrbitOnly).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "MiriadeBestOrbitOnly", true, (DataSourceUpdateMode)1));
			((Control)chkOneOrbitOnly).set_Location(new Point(135, 32));
			((Control)chkOneOrbitOnly).set_Margin(new Padding(4, 3, 4, 3));
			((Control)chkOneOrbitOnly).set_Name("chkOneOrbitOnly");
			((Control)chkOneOrbitOnly).set_Size(new Size(208, 17));
			((Control)chkOneOrbitOnly).set_TabIndex(3);
			((Control)chkOneOrbitOnly).set_Text("Limit output to first (= best) orbit");
			((ButtonBase)chkOneOrbitOnly).set_UseVisualStyleBackColor(true);
			((Control)chkNoMiriadeOrbits).set_AutoSize(true);
			((Control)chkNoMiriadeOrbits).set_Location(new Point(135, 16));
			((Control)chkNoMiriadeOrbits).set_Margin(new Padding(4, 3, 4, 3));
			((Control)chkNoMiriadeOrbits).set_Name("chkNoMiriadeOrbits");
			((Control)chkNoMiriadeOrbits).set_Size(new Size(208, 17));
			((Control)chkNoMiriadeOrbits).set_TabIndex(2);
			((Control)chkNoMiriadeOrbits).set_Text("Do not use Miriade ephemerides");
			((ButtonBase)chkNoMiriadeOrbits).set_UseVisualStyleBackColor(true);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(26, 0));
			((Control)label4).set_Margin(new Padding(4, 0, 4, 0));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(294, 13));
			((Control)label4).set_TabIndex(0);
			((Control)label4).set_Text("Binary asteroid ephemerides available from Miriade");
			((ListControl)lstMiriadeAsteroids).set_FormattingEnabled(true);
			((Control)lstMiriadeAsteroids).set_Location(new Point(6, 16));
			((Control)lstMiriadeAsteroids).set_Margin(new Padding(4, 3, 4, 3));
			((Control)lstMiriadeAsteroids).set_Name("lstMiriadeAsteroids");
			((Control)lstMiriadeAsteroids).set_RightToLeft((RightToLeft)1);
			lstMiriadeAsteroids.set_ScrollAlwaysVisible(true);
			((Control)lstMiriadeAsteroids).set_Size(new Size(82, 82));
			((Control)lstMiriadeAsteroids).set_TabIndex(1);
			((Control)optPlanets).set_AutoSize(true);
			((Control)optPlanets).set_Font(new Font("Microsoft Sans Serif", 12.75f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optPlanets).set_Location(new Point(287, 19));
			((Control)optPlanets).set_Margin(new Padding(4, 3, 4, 3));
			((Control)optPlanets).set_Name("optPlanets");
			((Control)optPlanets).set_Size(new Size(90, 24));
			((Control)optPlanets).set_TabIndex(1);
			((Control)optPlanets).set_Text("Planets");
			((ButtonBase)optPlanets).set_UseVisualStyleBackColor(true);
			optPlanets.add_CheckedChanged((EventHandler)optPlanets_CheckedChanged);
			((Control)optMinorPlanets).set_AutoSize(true);
			optMinorPlanets.set_Checked(true);
			((Control)optMinorPlanets).set_Font(new Font("Microsoft Sans Serif", 12.75f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optMinorPlanets).set_Location(new Point(28, 19));
			((Control)optMinorPlanets).set_Margin(new Padding(4, 3, 4, 3));
			((Control)optMinorPlanets).set_Name("optMinorPlanets");
			((Control)optMinorPlanets).set_Size(new Size(195, 24));
			((Control)optMinorPlanets).set_TabIndex(0);
			optMinorPlanets.set_TabStop(true);
			((Control)optMinorPlanets).set_Text("Asteroids && Comets");
			((ButtonBase)optMinorPlanets).set_UseVisualStyleBackColor(true);
			optMinorPlanets.add_CheckedChanged((EventHandler)optMinorPlanets_CheckedChanged);
			((Control)pnlSelectPlanets).set_BackColor(Color.FromArgb(255, 224, 202));
			((Control)pnlSelectPlanets).get_Controls().Add((Control)(object)chkPlanetMagLimit);
			((Control)pnlSelectPlanets).get_Controls().Add((Control)(object)label3);
			((Control)pnlSelectPlanets).get_Controls().Add((Control)(object)label16);
			((Control)pnlSelectPlanets).get_Controls().Add((Control)(object)txtPlanetMagLimit);
			((Control)pnlSelectPlanets).get_Controls().Add((Control)(object)chkHorizons_Satellites);
			((Control)pnlSelectPlanets).get_Controls().Add((Control)(object)cmbPlanets);
			((Control)pnlSelectPlanets).get_Controls().Add((Control)(object)chkIncludeMainMoons);
			((Control)pnlSelectPlanets).get_Controls().Add((Control)(object)lblMoons);
			((Control)pnlSelectPlanets).get_Controls().Add((Control)(object)chkIncludeSmallMoons);
			((Control)pnlSelectPlanets).set_Location(new Point(6, 42));
			((Control)pnlSelectPlanets).set_Margin(new Padding(4, 3, 4, 3));
			((Control)pnlSelectPlanets).set_Name("pnlSelectPlanets");
			((Control)pnlSelectPlanets).set_Size(new Size(408, 201));
			((Control)pnlSelectPlanets).set_TabIndex(3);
			((Control)label3).set_BackColor(Color.FromArgb(255, 192, 128));
			label3.set_BorderStyle((BorderStyle)2);
			((Control)label3).set_ForeColor(Color.Maroon);
			((Control)label3).set_Location(new Point(5, 124));
			((Control)label3).set_Margin(new Padding(4, 0, 4, 0));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(400, 75));
			((Control)label3).set_TabIndex(8);
			((Control)label3).set_Text(componentResourceManager.GetString("label3.Text"));
			label3.set_TextAlign(ContentAlignment.MiddleLeft);
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label16).set_Location(new Point(181, 57));
			((Control)label16).set_Margin(new Padding(4, 0, 4, 0));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(189, 26));
			((Control)label16).set_TabIndex(7);
			((Control)label16).set_Text("Searches covering many weeks should\r\nbe avoided with Mars.");
			((Control)chkHorizons_Satellites).set_AutoSize(true);
			chkHorizons_Satellites.set_Checked(true);
			chkHorizons_Satellites.set_CheckState((CheckState)1);
			((Control)chkHorizons_Satellites).set_Location(new Point(5, 62));
			((Control)chkHorizons_Satellites).set_Margin(new Padding(4, 3, 4, 3));
			((Control)chkHorizons_Satellites).set_Name("chkHorizons_Satellites");
			((Control)chkHorizons_Satellites).set_Size(new Size(176, 17));
			((Control)chkHorizons_Satellites).set_TabIndex(4);
			((Control)chkHorizons_Satellites).set_Text("Use Horizons for Satellites");
			((ButtonBase)chkHorizons_Satellites).set_UseVisualStyleBackColor(true);
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label8).set_ForeColor(Color.DarkRed);
			((Control)label8).set_Location(new Point(81, 48));
			((Control)label8).set_Margin(new Padding(4, 0, 4, 0));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(56, 13));
			((Control)label8).set_TabIndex(11);
			((Control)label8).set_Text("Taxonomy");
			((Control)grpSearch).set_BackColor(Color.FromArgb(218, 240, 218));
			((Control)grpSearch).get_Controls().Add((Control)(object)label12);
			((Control)grpSearch).get_Controls().Add((Control)(object)label7);
			((Control)grpSearch).get_Controls().Add((Control)(object)lblCatalogue);
			((Control)grpSearch).get_Controls().Add((Control)(object)lblMiss1);
			((Control)grpSearch).get_Controls().Add((Control)(object)lblMiss2);
			((Control)grpSearch).get_Controls().Add((Control)(object)updnMissDistance);
			((Control)grpSearch).get_Controls().Add((Control)(object)chkClearPrevious);
			((Control)grpSearch).get_Controls().Add((Control)(object)panelAdjust);
			((Control)grpSearch).get_Controls().Add((Control)(object)cmdCompute);
			((Control)grpSearch).get_Controls().Add((Control)(object)pbarSearch);
			((Control)grpSearch).get_Controls().Add((Control)(object)cmdCancel);
			((Control)grpSearch).get_Controls().Add((Control)(object)lblElementsDate);
			((Control)grpSearch).set_Location(new Point(477, 113));
			((Control)grpSearch).set_Margin(new Padding(4, 3, 4, 3));
			((Control)grpSearch).set_Name("grpSearch");
			((Control)grpSearch).set_Padding(new Padding(4, 3, 4, 3));
			((Control)grpSearch).set_Size(new Size(418, 141));
			((Control)grpSearch).set_TabIndex(4);
			grpSearch.set_TabStop(false);
			((Control)grpSearch).set_Text("6.  Do the search");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(304, 83));
			((Control)label12).set_Margin(new Padding(4, 0, 4, 0));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(75, 13));
			((Control)label12).set_TabIndex(17);
			((Control)label12).set_Text("Orbit source");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(7, 83));
			((Control)label7).set_Margin(new Padding(4, 0, 4, 0));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(89, 13));
			((Control)label7).set_TabIndex(16);
			((Control)label7).set_Text("Star catalogue");
			((Control)lblCatalogue).set_AutoSize(true);
			((Control)lblCatalogue).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblCatalogue).set_Location(new Point(7, 95));
			((Control)lblCatalogue).set_Margin(new Padding(4, 0, 4, 0));
			((Control)lblCatalogue).set_Name("lblCatalogue");
			((Control)lblCatalogue).set_Size(new Size(55, 13));
			((Control)lblCatalogue).set_TabIndex(15);
			((Control)lblCatalogue).set_Text("Catalogue");
			((Control)updnMissDistance).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "AsteroidSearchExpandedMissDistance", true, (DataSourceUpdateMode)1));
			updnMissDistance.set_DecimalPlaces(1);
			((Control)updnMissDistance).set_Location(new Point(211, 47));
			((Control)updnMissDistance).set_Margin(new Padding(4, 3, 4, 3));
			updnMissDistance.set_Maximum(new decimal(new int[4] { 999, 0, 0, 0 }));
			updnMissDistance.set_Minimum(new decimal(new int[4] { 2, 0, 0, -2147418112 }));
			((Control)updnMissDistance).set_Name("updnMissDistance");
			((Control)updnMissDistance).set_Size(new Size(52, 20));
			((Control)updnMissDistance).set_TabIndex(1);
			((UpDownBase)updnMissDistance).set_TextAlign((HorizontalAlignment)1);
			updnMissDistance.set_Value(Settings.Default.AsteroidSearchExpandedMissDistance);
			((Control)updnMissDistance).add_Leave((EventHandler)updnMissDistance_Leave);
			((Control)chkClearPrevious).set_AutoSize(true);
			chkClearPrevious.set_Checked(true);
			chkClearPrevious.set_CheckState((CheckState)1);
			((Control)chkClearPrevious).set_Location(new Point(28, 67));
			((Control)chkClearPrevious).set_Margin(new Padding(4, 3, 4, 3));
			((Control)chkClearPrevious).set_Name("chkClearPrevious");
			((Control)chkClearPrevious).set_Size(new Size(244, 17));
			((Control)chkClearPrevious).set_TabIndex(14);
			((Control)chkClearPrevious).set_Text("Clear the previous results from memory");
			((ButtonBase)chkClearPrevious).set_UseVisualStyleBackColor(true);
			panelAdjust.set_BorderStyle((BorderStyle)1);
			((Control)panelAdjust).get_Controls().Add((Control)(object)label21);
			((Control)panelAdjust).get_Controls().Add((Control)(object)updnDecCorrn);
			((Control)panelAdjust).get_Controls().Add((Control)(object)updnRACorrn);
			((Control)panelAdjust).get_Controls().Add((Control)(object)label9);
			((Control)panelAdjust).get_Controls().Add((Control)(object)lblAdjust);
			((Control)panelAdjust).set_Location(new Point(12, 16));
			((Control)panelAdjust).set_Margin(new Padding(4, 3, 4, 3));
			((Control)panelAdjust).set_Name("panelAdjust");
			((Control)panelAdjust).set_Size(new Size(394, 24));
			((Control)panelAdjust).set_TabIndex(3);
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label21).set_Location(new Point(209, 5));
			((Control)label21).set_Margin(new Padding(4, 0, 4, 0));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(21, 13));
			((Control)label21).set_TabIndex(5);
			((Control)label21).set_Text("RA");
			updnDecCorrn.set_DecimalPlaces(1);
			((Control)updnDecCorrn).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnDecCorrn).set_Location(new Point(328, 2));
			((Control)updnDecCorrn).set_Margin(new Padding(4, 3, 4, 3));
			updnDecCorrn.set_Maximum(new decimal(new int[4] { 2000, 0, 0, 0 }));
			updnDecCorrn.set_Minimum(new decimal(new int[4] { 2000, 0, 0, -2147483648 }));
			((Control)updnDecCorrn).set_Name("updnDecCorrn");
			((Control)updnDecCorrn).set_Size(new Size(55, 18));
			((Control)updnDecCorrn).set_TabIndex(3);
			((UpDownBase)updnDecCorrn).set_TextAlign((HorizontalAlignment)1);
			updnRACorrn.set_DecimalPlaces(1);
			((Control)updnRACorrn).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnRACorrn).set_Location(new Point(234, 2));
			((Control)updnRACorrn).set_Margin(new Padding(4, 3, 4, 3));
			updnRACorrn.set_Maximum(new decimal(new int[4] { 2000, 0, 0, 0 }));
			updnRACorrn.set_Minimum(new decimal(new int[4] { 2000, 0, 0, -2147483648 }));
			((Control)updnRACorrn).set_Name("updnRACorrn");
			((Control)updnRACorrn).set_Size(new Size(55, 18));
			((Control)updnRACorrn).set_TabIndex(1);
			((UpDownBase)updnRACorrn).set_TextAlign((HorizontalAlignment)1);
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(296, 5));
			((Control)label9).set_Margin(new Padding(4, 0, 4, 0));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(26, 13));
			((Control)label9).set_TabIndex(2);
			((Control)label9).set_Text("Dec");
			((Control)lblAdjust).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblAdjust).set_Location(new Point(-22, 1));
			((Control)lblAdjust).set_Margin(new Padding(4, 0, 4, 0));
			((Control)lblAdjust).set_Name("lblAdjust");
			((Control)lblAdjust).set_Size(new Size(229, 19));
			((Control)lblAdjust).set_TabIndex(0);
			((Control)lblAdjust).set_Text("Adjust asteroid position ( in mas ) by");
			lblAdjust.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)lblElementsDate).set_AutoSize(true);
			((Control)lblElementsDate).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblElementsDate).set_Location(new Point(304, 95));
			((Control)lblElementsDate).set_Margin(new Padding(4, 0, 4, 0));
			((Control)lblElementsDate).set_Name("lblElementsDate");
			((Control)lblElementsDate).set_Size(new Size(50, 13));
			((Control)lblElementsDate).set_TabIndex(13);
			((Control)lblElementsDate).set_Text("Elements");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 10.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(630, 254));
			((Control)label6).set_Margin(new Padding(4, 0, 4, 0));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(99, 17));
			((Control)label6).set_TabIndex(6);
			((Control)label6).set_Text("Search results");
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)setDataForSearchIntervalToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem,
				(ToolStripItem)useoldHorizonsOrbitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Padding(new Padding(7, 2, 0, 2));
			((Control)menuStrip1).set_Size(new Size(910, 24));
			((Control)menuStrip1).set_TabIndex(13);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)setDataForSearchIntervalToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[10]
			{
				(ToolStripItem)mnuJD239,
				(ToolStripItem)mnuJD240,
				(ToolStripItem)mnuJD241,
				(ToolStripItem)mnuJD242,
				(ToolStripItem)mnuJD243,
				(ToolStripItem)mnuJD244,
				(ToolStripItem)mnuJD245,
				(ToolStripItem)mnuJD246,
				(ToolStripItem)mnuJD247,
				(ToolStripItem)mnuJD248
			});
			((ToolStripItem)setDataForSearchIntervalToolStripMenuItem).set_Name("setDataForSearchIntervalToolStripMenuItem");
			((ToolStripItem)setDataForSearchIntervalToolStripMenuItem).set_Size(new Size(177, 20));
			((ToolStripItem)setDataForSearchIntervalToolStripMenuItem).set_Text("numerical integration data...   ");
			((ToolStripItem)mnuJD239).set_Name("mnuJD239");
			((ToolStripItem)mnuJD239).set_Size(new Size(222, 22));
			((ToolStripItem)mnuJD239).set_Text("1831 Jul 02 <> 1858 Nov 17");
			((ToolStripItem)mnuJD239).add_Click((EventHandler)mnuJD239_Click);
			((ToolStripItem)mnuJD240).set_Name("mnuJD240");
			((ToolStripItem)mnuJD240).set_Size(new Size(222, 22));
			((ToolStripItem)mnuJD240).set_Text("1858 Nov 17 <> 1886 Apr 04");
			((ToolStripItem)mnuJD240).add_Click((EventHandler)mnuJD240_Click);
			((ToolStripItem)mnuJD241).set_Name("mnuJD241");
			((ToolStripItem)mnuJD241).set_Size(new Size(222, 22));
			((ToolStripItem)mnuJD241).set_Text("1886 Apr 04 <> 1913 Aug 21");
			((ToolStripItem)mnuJD241).add_Click((EventHandler)mnuJD241_Click);
			((ToolStripItem)mnuJD242).set_Name("mnuJD242");
			((ToolStripItem)mnuJD242).set_Size(new Size(222, 22));
			((ToolStripItem)mnuJD242).set_Text("1913 Aug 21 <> 1941 Jan 06");
			((ToolStripItem)mnuJD242).add_Click((EventHandler)mnuJD242_Click);
			((ToolStripItem)mnuJD243).set_Name("mnuJD243");
			((ToolStripItem)mnuJD243).set_Size(new Size(222, 22));
			((ToolStripItem)mnuJD243).set_Text("1941 Jan 06 <> 1968 May 24");
			((ToolStripItem)mnuJD243).add_Click((EventHandler)mnuJD243_Click);
			((ToolStripItem)mnuJD244).set_Name("mnuJD244");
			((ToolStripItem)mnuJD244).set_Size(new Size(222, 22));
			((ToolStripItem)mnuJD244).set_Text("1968 May 24 <> 1995 Oct 10");
			((ToolStripItem)mnuJD244).add_Click((EventHandler)mnuJD244_Click);
			((ToolStripItem)mnuJD245).set_Name("mnuJD245");
			((ToolStripItem)mnuJD245).set_Size(new Size(222, 22));
			((ToolStripItem)mnuJD245).set_Text("1995 Oct 10 <> 2023 Feb 25");
			((ToolStripItem)mnuJD245).add_Click((EventHandler)mnuJD245_Click);
			((ToolStripItem)mnuJD246).set_Name("mnuJD246");
			((ToolStripItem)mnuJD246).set_Size(new Size(222, 22));
			((ToolStripItem)mnuJD246).set_Text("2023 Feb 25 <> 2050 Jul 13");
			((ToolStripItem)mnuJD246).add_Click((EventHandler)mnuJD246_Click);
			((ToolStripItem)mnuJD247).set_Name("mnuJD247");
			((ToolStripItem)mnuJD247).set_Size(new Size(222, 22));
			((ToolStripItem)mnuJD247).set_Text("2050 Jul 13 <> 2077 Nov 28");
			((ToolStripItem)mnuJD247).add_Click((EventHandler)mnuJD247_Click);
			((ToolStripItem)mnuJD248).set_Name("mnuJD248");
			((ToolStripItem)mnuJD248).set_Size(new Size(222, 22));
			((ToolStripItem)mnuJD248).set_Text("2077 Nov 28 <> 2105 Apr 16");
			((ToolStripItem)mnuJD248).add_Click((EventHandler)mnuJD248_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(69, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help   ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(84, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit          ");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((ToolStripDropDownItem)useoldHorizonsOrbitToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)thisFunctionIsNOTForGenralUseToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)selectOldOrbitToolStripMenuItem,
				(ToolStripItem)SelectedOrbitID,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)useOldOrbitToolStripMenuItem
			});
			((ToolStripItem)useoldHorizonsOrbitToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)useoldHorizonsOrbitToolStripMenuItem).set_ForeColor(Color.Silver);
			((ToolStripItem)useoldHorizonsOrbitToolStripMenuItem).set_Name("useoldHorizonsOrbitToolStripMenuItem");
			((ToolStripItem)useoldHorizonsOrbitToolStripMenuItem).set_Size(new Size(133, 20));
			((ToolStripItem)useoldHorizonsOrbitToolStripMenuItem).set_Text("'Old' Horizons orbits");
			((ToolStripItem)thisFunctionIsNOTForGenralUseToolStripMenuItem).set_ForeColor(Color.Red);
			((ToolStripItem)thisFunctionIsNOTForGenralUseToolStripMenuItem).set_Name("thisFunctionIsNOTForGenralUseToolStripMenuItem");
			((ToolStripItem)thisFunctionIsNOTForGenralUseToolStripMenuItem).set_Size(new Size(272, 22));
			((ToolStripItem)thisFunctionIsNOTForGenralUseToolStripMenuItem).set_Text("This function is NOT for general use");
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(269, 6));
			((ToolStripItem)selectOldOrbitToolStripMenuItem).set_Name("selectOldOrbitToolStripMenuItem");
			((ToolStripItem)selectOldOrbitToolStripMenuItem).set_Size(new Size(272, 22));
			((ToolStripItem)selectOldOrbitToolStripMenuItem).set_Text("Select old orbit");
			((ToolStripItem)selectOldOrbitToolStripMenuItem).add_Click((EventHandler)selectOldOrbitToolStripMenuItem_Click);
			((ToolStripItem)SelectedOrbitID).set_Font(new Font("Segoe UI", 9f));
			((ToolStripItem)SelectedOrbitID).set_Name("SelectedOrbitID");
			((ToolStripItem)SelectedOrbitID).set_Size(new Size(100, 23));
			((ToolStripItem)SelectedOrbitID).set_Text("None");
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(269, 6));
			((ToolStripItem)useOldOrbitToolStripMenuItem).set_Name("useOldOrbitToolStripMenuItem");
			((ToolStripItem)useOldOrbitToolStripMenuItem).set_Size(new Size(272, 22));
			((ToolStripItem)useOldOrbitToolStripMenuItem).set_Text("Use old orbit");
			((ToolStripItem)useOldOrbitToolStripMenuItem).add_Click((EventHandler)useOldOrbitToolStripMenuItem_Click);
			toolTip1.set_IsBalloon(true);
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label17).set_ForeColor(Color.Brown);
			((Control)label17).set_Location(new Point(206, 108));
			((Control)label17).set_Margin(new Padding(4, 0, 4, 0));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(19, 20));
			((Control)label17).set_TabIndex(7);
			((Control)label17).set_Text("λ");
			((Control)chkAperture).set_AutoSize(true);
			chkAperture.set_Checked(Settings.Default.AsteroidSearch_Visibility);
			((Control)chkAperture).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidSearch_Visibility", true, (DataSourceUpdateMode)1));
			((Control)chkAperture).set_ForeColor(Color.DarkGreen);
			((Control)chkAperture).set_Location(new Point(10, 88));
			((Control)chkAperture).set_Margin(new Padding(4, 3, 4, 3));
			((Control)chkAperture).set_Name("chkAperture");
			((Control)chkAperture).set_Size(new Size(58, 17));
			((Control)chkAperture).set_TabIndex(0);
			((Control)chkAperture).set_Text("Using");
			((ButtonBase)chkAperture).set_UseVisualStyleBackColor(true);
			((Control)updnAperture).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "AsteroidSearch_Aperture", true, (DataSourceUpdateMode)1));
			updnAperture.set_Increment(new decimal(new int[4] { 5, 0, 0, 0 }));
			((Control)updnAperture).set_Location(new Point(71, 86));
			((Control)updnAperture).set_Margin(new Padding(4, 3, 4, 3));
			updnAperture.set_Maximum(new decimal(new int[4] { 95, 0, 0, 0 }));
			updnAperture.set_Minimum(new decimal(new int[4] { 5, 0, 0, 0 }));
			((Control)updnAperture).set_Name("updnAperture");
			((Control)updnAperture).set_Size(new Size(40, 20));
			((Control)updnAperture).set_TabIndex(1);
			updnAperture.set_Value(Settings.Default.AsteroidSearch_Aperture);
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_ForeColor(Color.DarkGreen);
			((Control)label18).set_Location(new Point(114, 89));
			((Control)label18).set_Margin(new Padding(4, 0, 4, 0));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(113, 13));
			((Control)label18).set_TabIndex(2);
			((Control)label18).set_Text("cm telescope, with");
			((Control)chkVarExp).set_AutoSize(true);
			chkVarExp.set_Checked(Settings.Default.AsteroidSearch_Integration);
			((Control)chkVarExp).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidSearch_Integration", true, (DataSourceUpdateMode)1));
			((Control)chkVarExp).set_ForeColor(Color.DarkGreen);
			((Control)chkVarExp).set_Location(new Point(227, 88));
			((Control)chkVarExp).set_Margin(new Padding(4, 3, 4, 3));
			((Control)chkVarExp).set_Name("chkVarExp");
			((Control)chkVarExp).set_Size(new Size(171, 17));
			((Control)chkVarExp).set_TabIndex(3);
			((Control)chkVarExp).set_Text("variable exposure camera");
			((ButtonBase)chkVarExp).set_UseVisualStyleBackColor(true);
			((Control)grpFilters).set_BackColor(Color.LightSkyBlue);
			((Control)grpFilters).get_Controls().Add((Control)(object)label22);
			((Control)grpFilters).get_Controls().Add((Control)(object)label19);
			((Control)grpFilters).get_Controls().Add((Control)(object)label14);
			((Control)grpFilters).get_Controls().Add((Control)(object)panel1);
			((Control)grpFilters).get_Controls().Add((Control)(object)updnSearchLatitude);
			((Control)grpFilters).get_Controls().Add((Control)(object)updnSearchLongitude);
			((Control)grpFilters).get_Controls().Add((Control)(object)label17);
			((Control)grpFilters).get_Controls().Add((Control)(object)label18);
			((Control)grpFilters).get_Controls().Add((Control)(object)updnAperture);
			((Control)grpFilters).get_Controls().Add((Control)(object)chkVarExp);
			((Control)grpFilters).get_Controls().Add((Control)(object)chkAperture);
			((Control)grpFilters).get_Controls().Add((Control)(object)label13);
			((Control)grpFilters).get_Controls().Add((Control)(object)cmbDistance);
			((Control)grpFilters).get_Controls().Add((Control)(object)label15);
			((Control)grpFilters).get_Controls().Add((Control)(object)cmdSetSite);
			((Control)grpFilters).get_Controls().Add((Control)(object)chkSiteLimit);
			((Control)grpFilters).set_Location(new Point(14, 286));
			((Control)grpFilters).set_Margin(new Padding(4, 3, 4, 3));
			((Control)grpFilters).set_Name("grpFilters");
			((Control)grpFilters).set_Padding(new Padding(4, 3, 4, 3));
			((Control)grpFilters).set_Size(new Size(420, 132));
			((Control)grpFilters).set_TabIndex(14);
			grpFilters.set_TabStop(false);
			((Control)grpFilters).set_Text("3.  Filters");
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_ForeColor(Color.DarkBlue);
			((Control)label22).set_Location(new Point(205, 0));
			((Control)label22).set_Margin(new Padding(4, 0, 4, 0));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(18, 13));
			((Control)label22).set_TabIndex(14);
			((Control)label22).set_Text("or");
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold | FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label19).set_ForeColor(Color.DarkGreen);
			((Control)label19).set_Location(new Point(227, 0));
			((Control)label19).set_Margin(new Padding(4, 0, 4, 0));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(92, 13));
			((Control)label19).set_TabIndex(15);
			((Control)label19).set_Text("L i m i t   t o ...");
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_BackColor(Color.LightSkyBlue);
			((Control)label14).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold | FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label14).set_ForeColor(Color.DarkRed);
			((Control)label14).set_Location(new Point(107, 0));
			((Control)label14).set_Margin(new Padding(4, 0, 4, 0));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(84, 13));
			((Control)label14).set_TabIndex(13);
			((Control)label14).set_Text("E x c l u d e  ");
			((Control)panel1).set_BackColor(Color.FromArgb(145, 220, 250));
			panel1.set_BorderStyle((BorderStyle)2);
			((Control)panel1).get_Controls().Add((Control)(object)label24);
			((Control)panel1).get_Controls().Add((Control)(object)updnMag);
			((Control)panel1).get_Controls().Add((Control)(object)updnMagDrop);
			((Control)panel1).get_Controls().Add((Control)(object)chkApplyMagLimit);
			((Control)panel1).get_Controls().Add((Control)(object)chkMagDiff);
			((Control)panel1).get_Controls().Add((Control)(object)label25);
			((Control)panel1).get_Controls().Add((Control)(object)updnDiameter);
			((Control)panel1).get_Controls().Add((Control)(object)updnMinimumDuration);
			((Control)panel1).get_Controls().Add((Control)(object)cmbAsteroidClasses);
			((Control)panel1).get_Controls().Add((Control)(object)label8);
			((Control)panel1).get_Controls().Add((Control)(object)label5);
			((Control)panel1).get_Controls().Add((Control)(object)chkMinDuration);
			((Control)panel1).get_Controls().Add((Control)(object)chkDiameterLimit);
			((Control)panel1).get_Controls().Add((Control)(object)lblMagDropWarning);
			((Control)panel1).set_Location(new Point(1, 13));
			((Control)panel1).set_Margin(new Padding(4, 3, 4, 3));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(416, 69));
			((Control)panel1).set_TabIndex(12);
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label24).set_ForeColor(Color.DarkRed);
			((Control)label24).set_Location(new Point(137, 48));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(274, 13));
			((Control)label24).set_TabIndex(12);
			((Control)label24).set_Text("(Can use Horizons Ephemeris, but not Horizons elements)");
			((Control)lblMagDropWarning).set_AutoSize(true);
			((Control)lblMagDropWarning).set_Font(new Font("Arial", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblMagDropWarning).set_ForeColor(Color.Red);
			((Control)lblMagDropWarning).set_Location(new Point(149, 5));
			((Control)lblMagDropWarning).set_Name("lblMagDropWarning");
			((Control)lblMagDropWarning).set_Size(new Size(67, 13));
			((Control)lblMagDropWarning).set_TabIndex(13);
			((Control)lblMagDropWarning).set_Text("See Group 4");
			((Control)lblMagDropWarning).set_Visible(false);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(7f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((ScrollableControl)this).set_AutoScroll(true);
			((Form)this).set_ClientSize(new Size(910, 765));
			((Control)this).get_Controls().Add((Control)(object)grpFilters);
			((Control)this).get_Controls().Add((Control)(object)grpSetAsteroidsPlanets);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)lblEphemerisSource);
			((Control)this).get_Controls().Add((Control)(object)grpSetSaveFile);
			((Control)this).get_Controls().Add((Control)(object)grpSetDateRange);
			((Control)this).get_Controls().Add((Control)(object)lblAsteroidName);
			((Control)this).get_Controls().Add((Control)(object)lstResults);
			((Control)this).get_Controls().Add((Control)(object)grpSearch);
			((Control)this).get_Controls().Add((Control)(object)grpSetStarCat);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)groupBox4);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationAsterSearch", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationAsterSearch);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_Margin(new Padding(4, 3, 4, 3));
			((Control)this).set_Name("AsteroidSearch");
			((Control)this).set_Text("Search for occultations by Asteroids, or by Planets [including Pluto]");
			((Form)this).add_Load((EventHandler)AsteroidSearch_Load);
			((Control)panel3).ResumeLayout(false);
			((Control)panel3).PerformLayout();
			((ISupportInitialize)updnEndDay).EndInit();
			((ISupportInitialize)updnStartDay).EndInit();
			((ISupportInitialize)updnEndMonth).EndInit();
			((ISupportInitialize)updnEndYear).EndInit();
			((ISupportInitialize)updnStartMonth).EndInit();
			((ISupportInitialize)updnStartYear).EndInit();
			((Control)grpSetStarCat).ResumeLayout(false);
			((Control)grpSetStarCat).PerformLayout();
			((Control)panelGaiaFiles).ResumeLayout(false);
			((Control)panelGaiaFiles).PerformLayout();
			((ISupportInitialize)updnMagDrop).EndInit();
			((ISupportInitialize)updnMag).EndInit();
			((ISupportInitialize)updnSearchLatitude).EndInit();
			((ISupportInitialize)updnSearchLongitude).EndInit();
			((Control)grpSetDateRange).ResumeLayout(false);
			((Control)grpSetSaveFile).ResumeLayout(false);
			((Control)grpSetSaveFile).PerformLayout();
			((Control)groupBox4).ResumeLayout(false);
			((Control)groupBox4).PerformLayout();
			((ISupportInitialize)updnMinimumDuration).EndInit();
			((ISupportInitialize)updnDiameter).EndInit();
			((Control)grpSetAsteroidsPlanets).ResumeLayout(false);
			((Control)grpSetAsteroidsPlanets).PerformLayout();
			((Control)pnlSelectAsteroids).ResumeLayout(false);
			((Control)pnlSelectAsteroids).PerformLayout();
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((Control)pnlMiriade).ResumeLayout(false);
			((Control)pnlMiriade).PerformLayout();
			((Control)pnlSelectPlanets).ResumeLayout(false);
			((Control)pnlSelectPlanets).PerformLayout();
			((Control)grpSearch).ResumeLayout(false);
			((Control)grpSearch).PerformLayout();
			((ISupportInitialize)updnMissDistance).EndInit();
			((Control)panelAdjust).ResumeLayout(false);
			((Control)panelAdjust).PerformLayout();
			((ISupportInitialize)updnDecCorrn).EndInit();
			((ISupportInitialize)updnRACorrn).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((ISupportInitialize)updnAperture).EndInit();
			((Control)grpFilters).ResumeLayout(false);
			((Control)grpFilters).PerformLayout();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
