using System;
using System.ComponentModel;
using System.Diagnostics;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using System.Windows.Forms.Layout;
using Microsoft.VisualBasic;
using Occult.Defaults___Settings;
using Occult.Properties;

namespace Occult.Asteroids
{
	public class ListAndDisplay : Form
	{
		internal bool ListStarsMoreThanOnce;

		private bool AutoGenerateCancelFlag;

		private bool IsLoading = true;

		private string FormName;

		private int CountOfDisplayedEvents;

		private int CountOfRegions;

		private double AutoGeneratePlotScale = 1.0;

		private string[] Regions = new string[100];

		private string[] sites = new string[100];

		private string[] SortFields = new string[15]
		{
			"Date", "Diameter (km)", "Apparent diameter", "Duration", "Star Magnitude", "Magnitude drop (visual)", "Elongation", "Starnumber", "Number", "Name",
			"Probability", "Distance", "Magnitude drop (Red)", "Combined magnitude", "Rate of motion km/sec"
		};

		private double[] PresetScales = new double[9] { 1.0, 1.5, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 };

		private double[] ProbabilityValues = new double[8] { 1.0, 2.0, 5.0, 10.0, 20.0, 40.0, 60.0, 80.0 };

		private int CurrentSortField;

		private string RegionFile = Utilities.AppPath + "\\Resource Files\\AsteroidRegions.txt";

		private bool CancelHorizons;

		private bool ChangingMonth;

		private bool AutoUpdatingStart;

		private bool AutoUpdatingEnd;

		private IContainer components;

		private NumericUpDown updnLocalAltitude;

		private CheckBox chkLocalAltitude;

		private CheckBox chkShapeModel;

		private Label label12;

		private CheckBox chkSolarElongation;

		private Label label11;

		private CheckBox chkBinary;

		private Button cmdReSetSites;

		private Label label6;

		private NumericUpDown updnEndYear;

		private NumericUpDown updnStartYear;

		private ComboBox cmbPlanet;

		private Label label5;

		private Label label4;

		private Label label3;

		private Label label2;

		private Label label1;

		private TextBox txtAsteroidName;

		private TextBox txtAsteroidNumber;

		private CheckBox chkMagDrop;

		private CheckBox chkDuration;

		private CheckBox chkDiameter;

		private CheckBox chkAsteroidNumber;

		private CheckBox chkAsteroidName;

		private CheckBox chkDate;

		private CheckBox chkPlanet;

		private CheckBox chkDistArcSec;

		private CheckBox chkSiteDistKM;

		private CheckBox chkSite;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem fileToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private Label label7;

		private Panel pnlFilters;

		private Panel pnlOtherFilter;

		private Panel pnlIDfilter;

		private Panel pnlVisibilityFilter;

		private Panel pnlDateFilter;

		private Panel pnlSiteFilter;

		private Label label8;

		public ListBox lstSummary;

		private ToolStripMenuItem readToolStripMenuItem;

		private ToolStripMenuItem sortToolStripMenuItem;

		private ToolStripMenuItem byDateToolStripMenuItem;

		private ToolStripMenuItem byDiameterkmToolStripMenuItem;

		private ToolStripMenuItem byDiameterapparentToolStripMenuItem;

		private ToolStripMenuItem byDurationToolStripMenuItem;

		private ToolStripMenuItem byStarMagnitudeToolStripMenuItem;

		private ToolStripMenuItem byCombinedMagnitudeToolStripMenuItem;

		private ToolStripMenuItem byMagnitudeDropViosualToolStripMenuItem;

		private ToolStripMenuItem byMagnitudeDropRedToolStripMenuItem;

		private ToolStripMenuItem byElongationToolStripMenuItem;

		private ToolStripMenuItem byDistanceToolStripMenuItem;

		private ToolStripMenuItem byProbabilityToolStripMenuItem;

		private ToolStripMenuItem byNameToolStripMenuItem;

		private ToolStripMenuItem byNumberToolStripMenuItem;

		private ToolStripMenuItem byStarNumberToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripMenuItem listStarsOccultedMoreThanOnceToolStripMenuItem;

		private Button cmdListEvents;

		private NumericUpDown updnMagDrop;

		private NumericUpDown updnMaxDurn;

		private NumericUpDown updnDiameter;

		private NumericUpDown updnDistArcSec;

		private NumericUpDown updnDistKM;

		private NumericUpDown updnSolarElong;

		private ToolStripMenuItem helpToolStripMenuItem;

		private Label lblFormHeader;

		private CheckBox chkRings;

		private ToolTip toolTip1;

		private Label label13;

		private Label label10;

		private Label label9;

		private Label label14;

		private RadioButton optCombined;

		private RadioButton optStarMag;

		private Button cmdSaveListed;

		private Panel pnlRegion;

		private Label label20;

		private Label label19;

		private CheckBox chkInRegion;

		private NumericUpDown updnLatSE;

		private NumericUpDown updnLongSE;

		private NumericUpDown updnLatSW;

		private NumericUpDown updnLongSW;

		private NumericUpDown updnLatNE;

		private NumericUpDown updnLongNE;

		private NumericUpDown updnLatNW;

		private NumericUpDown updnLongNW;

		private Label label21;

		private Label label22;

		private ToolStripMenuItem saveAllToolStripMenuItem;

		private Label label24;

		private Label label23;

		private ToolStripMenuItem saveListedEventsToolStripMenuItem;

		private NumericUpDown updnStarMag;

		private CheckBox chkStarMag;

		private ToolStripMenuItem openAndMergeToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator2;

		private ToolStripMenuItem copyListToolStripMenuItem;

		private ToolStripMenuItem autoFileGenerationForAllListedToolStripMenuItem;

		internal ProgressBar PBarSummary;

		private Button cmdAutoCancel;

		private ToolStripMenuItem toolStripMenuItem1;

		private CheckBox chkMinimumD;

		private ToolStripMenuItem saveListedEventsForOccultWatcherFeedToolStripMenuItem;

		private Panel pnlSiteOptions;

		private ToolStripMenuItem editListOfRegionsToolStripMenuItem1;

		private ToolStripSeparator toolStripSeparator3;

		internal TextBox txtLatitude;

		internal TextBox txtLongitude;

		private ToolStripMenuItem plotPathsOnAMapToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator4;

		private ToolStripMenuItem savePathsAsGoogleEarthKMZToolStripMenuItem;

		private ToolStripMenuItem plotAllPathsInGoogleEarthToolStripMenuItem;

		private ToolStripMenuItem generationToolStripMenuItem;

		private ToolStripMenuItem filesForOccultWatcherFeedToolStripMenuItem;

		private NumericUpDown updnEndDay;

		private NumericUpDown updnStartDay;

		private NumericUpDown updnEndMonth;

		private NumericUpDown updnStartMonth;

		private ToolStripSeparator toolStripSeparator6;

		private ToolStripMenuItem xMLSaveAllEventsToolStripMenuItem;

		private ToolStripMenuItem xMLSaveListedEventsToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator7;

		private Label lblEventCount;

		private Label label17;

		private Label label16;

		private Label label15;

		private CheckBox chkAsteroidClass;

		private ComboBox cmbAsteroidClasses;

		private NumericUpDown updnUncertainty;

		private Label label18;

		private CheckBox chkUncertainty;

		private ToolStripMenuItem setScaleForWorldMapsToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator5;

		private ToolStripSeparator toolStripSeparator8;

		private ToolStripSeparator toolStripSeparator9;

		private ToolStripMenuItem saveListedEventsAsAPersonalOWFeedToolStripMenuItem;

		private ToolStripMenuItem mapsSitePathCenteredSiteToolStripMenuItem;

		private ToolStripMenuItem createUserAsteroidFileForAllListedAsteroidsToolStripMenuItem;

		private Panel PanelCancel;

		private Button cmdCancelHorizons;

		private Label lblHeader1;

		private Label lblHeader2;

		private Label lblType;

		private CheckBox chkAperture;

		private Label label25;

		private NumericUpDown updnAperture;

		private RadioButton optIntegrate;

		private Label lblMagType;

		private ToolStripMenuItem setLocalHorizonAltitudesToolStripMenuItem;

		private CheckBox chkHorizon;

		private ComboBox cmbProbabilityValues;

		private CheckBox chkProbability;

		private ToolStripMenuItem openAndMergeDuplicatesRetainedToolStripMenuItem;

		private ToolStripMenuItem sunBelowHorizonToolStripMenuItem;

		private ToolStripMenuItem fullPathToolStripMenuItem;

		private ToolStripMenuItem GEplotlimitedByAltitudesOfSunStarByStarMagToolStripMenuItem;

		private ToolStripMenuItem GEplotfullPathToolStripMenuItem;

		private ToolStripMenuItem saveKMZlimitedByAltitudesOfStarSunsetByStarMagToolStripMenuItem;

		private ToolStripMenuItem saveKMZfullPathToolStripMenuItem1;

		private Label label26;

		private TextBox txtStarID;

		internal CheckBox chkStarID;

		private Label label27;

		private Label label29;

		private Label label28;

		private ToolStripSeparator toolStripSeparator10;

		private ToolStripMenuItem pasteOneOrMoreXMLEventPredictionToolStripMenuItem;

		private ToolStripMenuItem clearAllEventsFromMemoryToolStripMenuItem;

		private ToolStripMenuItem clearAllUndisplayedEventsFromMemoryToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator11;

		private ToolStripMenuItem rermoveSelectedEventToolStripMenuItem;

		private ToolStripMenuItem copyOccultationElementsOfSelectedEventToolStripMenuItem;

		private ToolStripMenuItem newToolStripMenuItem;

		private ComboBox cmbRegion;

		private Panel panel1;

		private NumericUpDown updnMinD;

		private Label label30;

		private ToolStripMenuItem byRateOfMotionkmsec;

		private Label label31;

		public ListAndDisplay()
		{
			InitializeComponent();
			Regions_PopulateDropDownList();
			AddScalesToMenu();
		}

		private void Regions_PopulateDropDownList()
		{
			for (int i = 0; i < 100; i++)
			{
				Regions[i] = "";
			}
			if (File.Exists(RegionFile) && new FileInfo(RegionFile).Length > 20)
			{
				using StreamReader streamReader = new StreamReader(RegionFile);
				CountOfRegions = 0;
				int num = 0;
				cmbRegion.get_Items().Clear();
				cmbRegion.get_Items().Add((object)"Last region");
				do
				{
					Regions[CountOfRegions] = streamReader.ReadLine();
					num = Regions[CountOfRegions].IndexOf(",");
					cmbRegion.get_Items().Add((object)Regions[CountOfRegions].Substring(0, num));
					CountOfRegions++;
				}
				while (!streamReader.EndOfStream);
			}
			if (cmbRegion.get_Items().get_Count() < 1)
			{
				cmbRegion.get_Items().Add((object)"None set");
				((ListControl)cmbRegion).set_SelectedIndex(0);
				((Control)cmbRegion).set_Enabled(false);
				return;
			}
			try
			{
				((ListControl)cmbRegion).set_SelectedIndex(Settings.Default.AsteroidListDispRegion);
			}
			catch
			{
				Settings @default = Settings.Default;
				int asteroidListDispRegion;
				((ListControl)cmbRegion).set_SelectedIndex(asteroidListDispRegion = 0);
				@default.AsteroidListDispRegion = asteroidListDispRegion;
			}
			((Control)cmbRegion).set_Enabled(true);
		}

		private void cmbRegion_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (((ListControl)cmbRegion).get_SelectedIndex() >= 1)
			{
				string[] array = Regions[((ListControl)cmbRegion).get_SelectedIndex() - 1].ToString().Split(new char[1] { ',' });
				int length = array.GetLength(0);
				if (length >= 2)
				{
					updnLongNW.set_Value(decimal.Parse(array[1]));
				}
				if (length >= 3)
				{
					updnLatNW.set_Value(decimal.Parse(array[2]));
				}
				if (length >= 4)
				{
					updnLongNE.set_Value(decimal.Parse(array[3]));
				}
				if (length >= 5)
				{
					updnLatNE.set_Value(decimal.Parse(array[4]));
				}
				if (length >= 6)
				{
					updnLongSW.set_Value(decimal.Parse(array[5]));
				}
				if (length >= 7)
				{
					updnLatSW.set_Value(decimal.Parse(array[6]));
				}
				if (length >= 8)
				{
					updnLongSE.set_Value(decimal.Parse(array[7]));
				}
				if (length >= 9)
				{
					updnLatSE.set_Value(decimal.Parse(array[8]));
				}
				Settings.Default.AsteroidListDispRegion = ((ListControl)cmbRegion).get_SelectedIndex();
				if (chkInRegion.get_Checked())
				{
					ListEvents();
				}
			}
		}

		private void AddScalesToMenu()
		{
			//IL_001b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0021: Expected O, but got Unknown
			for (int i = 0; i < PresetScales.Length; i++)
			{
				ToolStripItem val = (ToolStripItem)new ToolStripMenuItem(string.Format("{0,1:f1}", PresetScales[i]));
				val.add_Click((EventHandler)Scale_Click);
				((ToolStripDropDownItem)setScaleForWorldMapsToolStripMenuItem).get_DropDownItems().Add(val);
			}
		}

		private void Scale_Click(object sender, EventArgs e)
		{
			//IL_0051: Unknown result type (might be due to invalid IL or missing references)
			string text = sender.ToString();
			int num = 0;
			for (int i = 0; i < PresetScales.Length; i++)
			{
				if (((ToolStripDropDownItem)setScaleForWorldMapsToolStripMenuItem).get_DropDownItems().get_Item(i).get_Text() == text)
				{
					num = i;
				}
			}
			for (int j = 0; j < PresetScales.Length; j++)
			{
				((ToolStripMenuItem)((ToolStripDropDownItem)setScaleForWorldMapsToolStripMenuItem).get_DropDownItems().get_Item(j)).set_Checked(j == num);
			}
			Settings.Default.AsteroidListDisplayScaleIndex = num;
			AutoGeneratePlotScale = double.Parse(text);
			((ToolStripItem)setScaleForWorldMapsToolStripMenuItem).set_Text(string.Format("Set scale for world maps ( = {0,1:f1})", AutoGeneratePlotScale));
		}

		private void ListAndDisplay_Load(object sender, EventArgs e)
		{
			//IL_0039: Unknown result type (might be due to invalid IL or missing references)
			//IL_0263: Unknown result type (might be due to invalid IL or missing references)
			//IL_0268: Unknown result type (might be due to invalid IL or missing references)
			string formName;
			((Control)this).set_Text(formName = "Asteroid - List and Display : Occult v." + Utilities.OccultVersion_Short);
			FormName = formName;
			SetSize();
			for (int i = 0; i < PresetScales.Length; i++)
			{
				((ToolStripMenuItem)((ToolStripDropDownItem)setScaleForWorldMapsToolStripMenuItem).get_DropDownItems().get_Item(i)).set_Checked(i == Settings.Default.AsteroidListDisplayScaleIndex);
			}
			AutoGeneratePlotScale = PresetScales[Settings.Default.AsteroidListDisplayScaleIndex];
			((ToolStripItem)setScaleForWorldMapsToolStripMenuItem).set_Text(string.Format("Set scale for world maps ( = {0,1:f1})", AutoGeneratePlotScale));
			if (!File.Exists(Utilities.AppPath + "\\Resource Files\\AsteroidClasses.csv"))
			{
				http.DownloadAsteroidClassFile();
			}
			((ListControl)cmbAsteroidClasses).set_SelectedIndex(0);
			DateTime now = DateTime.Now;
			updnStartYear.set_Value((decimal)now.ToUniversalTime().Year);
			updnStartMonth.set_Value((decimal)now.ToUniversalTime().Month);
			updnStartDay.set_Value((decimal)now.ToUniversalTime().Day);
			now = DateTime.Now.AddDays(30.0);
			updnEndYear.set_Value((decimal)now.ToUniversalTime().Year);
			updnEndMonth.set_Value((decimal)now.ToUniversalTime().Month);
			updnEndDay.set_Value((decimal)now.ToUniversalTime().Day);
			((ListControl)cmbPlanet).set_SelectedIndex(3);
			for (int j = 0; j < ProbabilityValues.Length; j++)
			{
				cmbProbabilityValues.get_Items().Add((object)(">" + ProbabilityValues[j] + "%"));
			}
			((ListControl)cmbProbabilityValues).set_SelectedIndex(3);
			((Control)pnlSiteOptions).set_Enabled(chkSite.get_Checked());
			DisplayMPOccultations.LoadISAM_and_DAMIT_ids(UpdateISAM: true);
			DisplayMPOccultations.CreateListOfFutureFileEvents();
			OccultationElements.TestSite = chkSite.get_Checked();
			SetSite();
			SetSiteTests();
			DisplayMPOccultations.MultipathFileFromListAndDisplayIsCurrent = false;
			ToolStripItem obj = ((ToolStrip)((Form)this).get_MainMenuStrip()).get_Items().get_Item(0);
			foreach (ToolStripItem item in (ArrangedElementCollection)((ToolStripDropDownItem)((obj is ToolStripMenuItem) ? obj : null)).get_DropDownItems())
			{
				item.set_Text(item.get_Text().Replace("#", "\r\n"));
			}
			IsLoading = false;
		}

		private void lstSummary_SelectedIndexChanged(object sender, EventArgs e)
		{
			int result = -1;
			if (((ListControl)lstSummary).get_SelectedIndex() < 0)
			{
				return;
			}
			string text = lstSummary.get_Items().get_Item(((ListControl)lstSummary).get_SelectedIndex()).ToString()!.Trim();
			if (text.Length >= 50)
			{
				int startIndex = text.LastIndexOf(" ");
				if (!int.TryParse(text.Substring(startIndex), out result))
				{
					result = -1;
				}
				if (DisplayMPOccultations.PlotMagnification < 1.0)
				{
					DisplayMPOccultations.PlotMagnification = 1.0;
				}
				if (DisplayMPOccultations.Plot != null)
				{
					AsteroidPlotPath.SetScaleCheckmarks((int)DisplayMPOccultations.PlotMagnification + 1);
				}
				DisplayMPOccultations.Set_Data_From_OccultationElements_Record(result, PlotEvent: true);
			}
		}

		private void lstSummary_DoubleClick(object sender, EventArgs e)
		{
			int result = -1;
			string text = lstSummary.get_Items().get_Item(((ListControl)lstSummary).get_SelectedIndex()).ToString()!.Trim();
			if (text.Length >= 50)
			{
				int startIndex = text.LastIndexOf(" ");
				if (!int.TryParse(text.Substring(startIndex), out result))
				{
					result = -1;
				}
				DisplayMPOccultations.PlotMagnification = 1.0;
				if (DisplayMPOccultations.Plot != null)
				{
					((Control)DisplayMPOccultations.Plot.cmdx1).Focus();
					AsteroidPlotPath.SetScaleCheckmarks(2);
				}
				DisplayMPOccultations.Set_Data_From_OccultationElements_Record(result, PlotEvent: true);
			}
		}

		private void ListAndDisplay_Resize(object sender, EventArgs e)
		{
			SetSize();
		}

		private void SetSize()
		{
			if (((Control)this).get_Width() < 500)
			{
				((Control)this).set_Width(500);
			}
			if (((Control)this).get_Height() < 500)
			{
				((Control)this).set_Height(500);
			}
			Panel obj = pnlFilters;
			int width;
			((Control)lstSummary).set_Width(width = ((Control)this).get_Width() - 26);
			((Control)obj).set_Width(width);
			((Control)lstSummary).set_Height(((Control)this).get_Height() - 255);
			int num = (((Control)pnlFilters).get_Width() - 1128) / 5 - 1;
			if (num < 0)
			{
				num = 0;
			}
			((Control)pnlVisibilityFilter).set_Left(329 + num);
			((Control)pnlRegion).set_Left(505 + 2 * num);
			((Control)pnlOtherFilter).set_Left(674 + 3 * num);
			((Control)pnlIDfilter).set_Left(878 + 4 * num);
			((Control)pnlDateFilter).set_Left(985 + 5 * num);
		}

		private void readToolStripMenuItem_Click(object sender, EventArgs e)
		{
			string displayParameters = DisplayMPOccultations.ReadOccElementFile(Merge: false);
			RemoveDuplicates();
			OccultationElements.SiteNeedsUpdating = (OccultationElements.RegionNeedsUpdating = true);
			DisplayMPOccultations.MultipathFileFromListAndDisplayIsCurrent = false;
			SetDisplayParameters(displayParameters);
			((Control)lstSummary).set_BackColor(SystemColors.Window);
		}

		internal string SetDisplayParameters(string Read_File)
		{
			for (int num = Read_File.Length; num > 0; num--)
			{
				Read_File = Read_File.Insert(num, " ");
			}
			((Control)this).set_Text(FormName + "         " + Read_File);
			Cursor.set_Current(Cursors.get_No());
			SetSite();
			SetSiteTests();
			SetTests();
			SortAndDisplay(0);
			Cursor.set_Current(Cursors.get_Default());
			((Control)lstSummary).set_BackColor(SystemColors.Window);
			return Read_File;
		}

		private void openAndMergeToolStripMenuItem_Click(object sender, EventArgs e)
		{
			OpenAndMerge(Remove_Duplicates: true);
		}

		private void openAndMergeDuplicatesRetainedToolStripMenuItem_Click(object sender, EventArgs e)
		{
			OpenAndMerge(Remove_Duplicates: false);
		}

		private void OpenAndMerge(bool Remove_Duplicates)
		{
			string displayParameters = DisplayMPOccultations.ReadOccElementFile(Merge: true);
			if (Remove_Duplicates)
			{
				RemoveDuplicates();
			}
			SetDisplayParameters(displayParameters);
			OccultationElements.SiteNeedsUpdating = (OccultationElements.RegionNeedsUpdating = true);
			DisplayMPOccultations.MultipathFileFromListAndDisplayIsCurrent = false;
		}

		private void pasteOneOrMoreXMLEventPredictionToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_003d: Unknown result type (might be due to invalid IL or missing references)
			int num = DisplayMPOccultations.PasteOccultationElements();
			SetDisplayParameters("");
			OccultationElements.SiteNeedsUpdating = (OccultationElements.RegionNeedsUpdating = true);
			DisplayMPOccultations.MultipathFileFromListAndDisplayIsCurrent = false;
			MessageBox.Show(num + "events added from Paste", "Pasted events", (MessageBoxButtons)0, (MessageBoxIcon)64);
		}

		internal void SortAndDisplay(int SortField)
		{
			string text = "Star";
			if (optCombined.get_Checked())
			{
				text = "Comb";
			}
			SetSite();
			SetSiteTests();
			SetTests();
			if (SortField >= 0)
			{
				CurrentSortField = SortField;
				Application.DoEvents();
				OccultationElements.SortField = SortField;
				DisplayMPOccultations.OccElements.Sort();
				Application.DoEvents();
			}
			for (int i = 0; i < DisplayMPOccultations.OccElements.Count; i++)
			{
				DisplayMPOccultations.OccElements[i].RecordNumber = i;
			}
			int num = 0;
			lstSummary.get_Items().Clear();
			if (chkSite.get_Checked())
			{
				lstSummary.get_Items().Add((object)("     Event Summary for Longitude " + ((Control)txtLongitude).get_Text().Trim() + "°, Latitude " + ((Control)txtLatitude).get_Text().Trim() + "°  - sorted by " + SortFields[CurrentSortField]));
			}
			else if (chkInRegion.get_Checked())
			{
				lstSummary.get_Items().Add((object)("     Event Summary for " + cmbRegion.get_Items().get_Item(((ListControl)cmbRegion).get_SelectedIndex()).ToString() + " region - sorted by " + SortFields[CurrentSortField]));
			}
			else
			{
				lstSummary.get_Items().Add((object)("     Global summary of events - sorted by " + SortFields[CurrentSortField]));
			}
			((Control)lblType).set_Text(lstSummary.get_Items().get_Item(0).ToString()!.Replace("\\", ""));
			lstSummary.get_Items().Add((object)"");
			if (!chkSite.get_Checked())
			{
				((Control)lblHeader1).set_Text("     Date       U.T.    Diameter   Durn  Rate " + text + "  Mag-Drop Elon  %      Star           d Rely             Planet           Min        Moon     § ◉    R.A. (J2000)  Dec.            Rec");
				lstSummary.get_Items().Add((object)((Control)lblHeader1).get_Text());
				((Control)lblHeader2).set_Text("   y   m  d    h   m     km   \"   sec/m  km/s  mag   V    R *   o Ill     No.              <1.4          No Name              D   Error Dist ill  ☾    h  m   s      o  '   \"           #");
				lstSummary.get_Items().Add((object)((Control)lblHeader2).get_Text());
			}
			else
			{
				((Control)lblHeader1).set_Text("     Date       U.T.    Diameter   Durn  Rate " + text + "  Mag-Drop Elon  %      Star           d Rely             Planet           Alt  Az  Dist Sun Moon Proba-  Moon   § ◉    R.A. (J2000)  Dec.    ");
				lstSummary.get_Items().Add((object)((Control)lblHeader1).get_Text());
				string text2 = "";
				text2 = ((!chkSiteDistKM.get_Checked()) ? "   y   m  d    h   m     km   \"   sec/m  km/s  mag   V    R *   o Ill     No.              <1.4          No Name               o   o    \"  Alt Alt  bility Dist ill  ☾    h  m   s      o  '   \"  " : "   y   m  d    h   m     km   \"   sec/m  km/s  mag   V    R *   o Ill     No.              <1.4          No Name               o   o   km  Alt Alt  bility Elon ill  ☾    h  m   s      o  '   \"  ");
				lstSummary.get_Items().Add((object)text2);
				((Control)lblHeader2).set_Text(text2.Replace("\\", ""));
			}
			CountOfDisplayedEvents = 0;
			((Control)PBarSummary).set_Left(20);
			PBarSummary.set_Minimum(0);
			PBarSummary.set_Value(0);
			PBarSummary.set_Maximum(DisplayMPOccultations.OccElements.Count);
			((Control)PBarSummary).set_Visible(true);
			if (ListStarsMoreThanOnce)
			{
				bool[] array = new bool[DisplayMPOccultations.OccElements.Count];
				for (int j = 0; j < DisplayMPOccultations.OccElements.Count; j++)
				{
					array[j] = false;
				}
				for (int k = 0; k < DisplayMPOccultations.OccElements.Count - 1; k++)
				{
					if ((DisplayMPOccultations.OccElements[k].StarCatName == DisplayMPOccultations.OccElements[k + 1].StarCatName) & (DisplayMPOccultations.OccElements[k].ObjectName != DisplayMPOccultations.OccElements[k + 1].ObjectName))
					{
						array[k] = (array[k + 1] = true);
					}
				}
				for (int l = 0; l < DisplayMPOccultations.OccElements.Count; l++)
				{
					if (l % 100 == 0)
					{
						PBarSummary.set_Value(l);
						Application.DoEvents();
					}
					if (array[l])
					{
						lstSummary.get_Items().Add((object)DisplayMPOccultations.OccElements[l].Summary_Line);
						num++;
						CountOfDisplayedEvents++;
						if (num % 5 == 0)
						{
							lstSummary.get_Items().Add((object)"");
						}
					}
					if (l >= MinorPlanetOccultationElements.MaximumNumEvents)
					{
						break;
					}
				}
			}
			else
			{
				for (int m = 0; m < DisplayMPOccultations.OccElements.Count; m++)
				{
					if (m % 100 == 0)
					{
						PBarSummary.set_Value(m);
						Application.DoEvents();
					}
					if (DisplayMPOccultations.OccElements[m].IsEventForOutput)
					{
						lstSummary.get_Items().Add((object)DisplayMPOccultations.OccElements[m].Summary_Line);
						num++;
						CountOfDisplayedEvents++;
						if (num % 5 == 0)
						{
							lstSummary.get_Items().Add((object)"");
						}
					}
					if (m >= MinorPlanetOccultationElements.MaximumNumEvents)
					{
						break;
					}
				}
			}
			((Control)lblEventCount).set_Text(CountOfDisplayedEvents + " events");
			if (ListStarsMoreThanOnce)
			{
				ListStarsMoreThanOnce = false;
			}
			ToolStripMenuItem obj = plotAllPathsInGoogleEarthToolStripMenuItem;
			bool enabled;
			((ToolStripItem)savePathsAsGoogleEarthKMZToolStripMenuItem).set_Enabled(enabled = CountOfDisplayedEvents < 100);
			((ToolStripItem)obj).set_Enabled(enabled);
			((Control)PBarSummary).set_Visible(false);
		}

		private void byDateToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SortAndDisplay(0);
		}

		private void byDiameterkmToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SortAndDisplay(1);
		}

		private void byDiameterapparentToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SortAndDisplay(2);
		}

		private void byDurationToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SortAndDisplay(3);
		}

		private void byRateOfMotionkmsec_Click(object sender, EventArgs e)
		{
			SortAndDisplay(14);
		}

		private void byStarMagnitudeToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SortAndDisplay(4);
		}

		private void byCombinedMagnitudeToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SortAndDisplay(13);
		}

		private void byMagnitudeDropViosualToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SortAndDisplay(5);
		}

		private void byMagnitudeDropRedToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SortAndDisplay(12);
		}

		private void byElongationToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SortAndDisplay(6);
		}

		private void byDistanceToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SortAndDisplay(11);
		}

		private void byProbabilityToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SortAndDisplay(10);
		}

		private void byNameToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SortAndDisplay(9);
		}

		private void byNumberToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SortAndDisplay(8);
		}

		private void byStarNumberToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SortAndDisplay(7);
		}

		private void listStarsOccultedMoreThanOnceToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ListStarsMoreThanOnce = true;
			SortAndDisplay(7);
		}

		private void txtLongitude_Leave(object sender, EventArgs e)
		{
			SetSite();
		}

		private void txtLatitude_Leave(object sender, EventArgs e)
		{
			SetSite();
		}

		internal void SetSite()
		{
			if (!double.TryParse(((Control)txtLongitude).get_Text(), out var result))
			{
				result = 0.0;
			}
			if (!double.TryParse(((Control)txtLatitude).get_Text(), out var result2))
			{
				result2 = 0.0;
			}
			OccultationElements.SiteLongitudeTest = result / (180.0 / Math.PI);
			OccultationElements.SiteLatitudeTest = result2 / (180.0 / Math.PI);
			OccultationElements.RegionLongitude[0] = (double)updnLongNW.get_Value() / (180.0 / Math.PI);
			OccultationElements.RegionLongitude[1] = (double)updnLongNE.get_Value() / (180.0 / Math.PI);
			OccultationElements.RegionLongitude[2] = (double)updnLongSW.get_Value() / (180.0 / Math.PI);
			OccultationElements.RegionLongitude[3] = (double)updnLongSE.get_Value() / (180.0 / Math.PI);
			OccultationElements.RegionLatitude[0] = (double)updnLatNW.get_Value() / (180.0 / Math.PI);
			OccultationElements.RegionLatitude[1] = (double)updnLatNE.get_Value() / (180.0 / Math.PI);
			OccultationElements.RegionLatitude[2] = (double)updnLatSW.get_Value() / (180.0 / Math.PI);
			OccultationElements.RegionLatitude[3] = (double)updnLatSE.get_Value() / (180.0 / Math.PI);
		}

		internal void SetSiteTests()
		{
			OccultationElements.TestSite = chkSite.get_Checked();
			OccultationElements.TestInRegion = chkInRegion.get_Checked();
			if (chkSite.get_Checked() | chkInRegion.get_Checked())
			{
				for (int i = 0; i < DisplayMPOccultations.OccElements.Count; i++)
				{
					DisplayMPOccultations.OccElements[i].SetSiteDependantFields();
				}
				if (chkSite.get_Checked())
				{
					OccultationElements.SiteNeedsUpdating = false;
				}
				if (chkInRegion.get_Checked())
				{
					OccultationElements.RegionNeedsUpdating = false;
				}
			}
		}

		private void cmdListEvents_Click(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void ListEvents()
		{
			Cursor.set_Current(Cursors.get_WaitCursor());
			SortAndDisplay(0);
			Cursor.set_Current(((Control)this).get_DefaultCursor());
			DisplayMPOccultations.MultipathFileFromListAndDisplayIsCurrent = false;
		}

		private void SetTests()
		{
			OccultationElements.TestSite = chkSite.get_Checked();
			SetSite();
			OccultationElements.TestSiteDistanceKM = chkSiteDistKM.get_Checked();
			OccultationElements.PathDistanceKMTest = (double)updnDistKM.get_Value();
			OccultationElements.TestProbability = chkProbability.get_Checked();
			try
			{
				OccultationElements.ProbabilityTest = ProbabilityValues[((ListControl)cmbProbabilityValues).get_SelectedIndex()];
			}
			catch
			{
				OccultationElements.ProbabilityTest = 0.0;
			}
			OccultationElements.TestSiteDistanceArcSec = chkDistArcSec.get_Checked();
			OccultationElements.PathDistanceArcSecTest = (double)updnDistArcSec.get_Value();
			OccultationElements.TestInRegion = chkInRegion.get_Checked();
			OccultationElements.TestDateRange = chkDate.get_Checked();
			OccultationElements.JDStartTest = Utilities.JD_from_Date((int)updnStartYear.get_Value(), (int)updnStartMonth.get_Value(), (double)updnStartDay.get_Value());
			OccultationElements.JDEndTest = Utilities.JD_from_Date((int)updnEndYear.get_Value(), (int)updnEndMonth.get_Value(), (double)updnEndDay.get_Value());
			OccultationElements.TestMagDrop = chkMagDrop.get_Checked();
			OccultationElements.MagDropTestValue = (double)updnMagDrop.get_Value();
			OccultationElements.TestStarMag = chkStarMag.get_Checked();
			OccultationElements.MagnitudeTest = (double)updnStarMag.get_Value();
			OccultationElements.UseCombinedMag = optCombined.get_Checked();
			OccultationElements.TestMaxDuration = chkDuration.get_Checked();
			OccultationElements.MinimumDurationTest = (double)updnMaxDurn.get_Value();
			OccultationElements.TestDiameter = chkDiameter.get_Checked();
			OccultationElements.DiameterTest = (double)updnDiameter.get_Value();
			OccultationElements.TestLocalAlt = chkLocalAltitude.get_Checked();
			OccultationElements.AltitudeTest = (double)updnLocalAltitude.get_Value();
			OccultationElements.TestLocalHorizon = chkHorizon.get_Checked();
			if (chkHorizon.get_Checked())
			{
				OccultationElements.SetLocalHorizon();
			}
			OccultationElements.TestSolarElongation = chkSolarElongation.get_Checked();
			OccultationElements.SolarElongationTest = (double)updnSolarElong.get_Value();
			OccultationElements.TestAperture = chkAperture.get_Checked();
			OccultationElements.ApertureTest = (double)updnAperture.get_Value();
			OccultationElements.TestIntegratingCamera = optIntegrate.get_Checked();
			OccultationElements.TestAsteroidNumber = chkAsteroidNumber.get_Checked();
			OccultationElements.AsteroidNumberTest = ((Control)txtAsteroidNumber).get_Text().Trim().ToUpper();
			OccultationElements.TestAsteroidName = chkAsteroidName.get_Checked();
			OccultationElements.AsteroidNameTest = ((Control)txtAsteroidName).get_Text().Trim().ToUpper();
			OccultationElements.TestAsteroidClass = chkAsteroidClass.get_Checked();
			try
			{
				OccultationElements.AsteroidClassTest = cmbAsteroidClasses.get_Items().get_Item(((ListControl)cmbAsteroidClasses).get_SelectedIndex()).ToString()!.Replace("All", "");
			}
			catch
			{
				OccultationElements.AsteroidClassTest = "";
			}
			OccultationElements.TestStarID = chkStarID.get_Checked();
			OccultationElements.StarIDTest = ((Control)txtStarID).get_Text().Trim().ToUpper();
			OccultationElements.TestPlanet = chkPlanet.get_Checked();
			try
			{
				OccultationElements.PlanetTest = ((ListControl)cmbPlanet).get_SelectedIndex() + 1;
				if (OccultationElements.PlanetTest > 2)
				{
					OccultationElements.PlanetTest++;
				}
			}
			catch
			{
				OccultationElements.PlanetTest = 1;
			}
			OccultationElements.TestUncertainty = chkUncertainty.get_Checked();
			OccultationElements.UncertaintyTest = (double)updnUncertainty.get_Value();
			OccultationElements.TestBinaryAsteroid = chkBinary.get_Checked();
			OccultationElements.TestShapeModel = chkShapeModel.get_Checked();
			OccultationElements.TestRings = chkRings.get_Checked();
			OccultationElements.TestMinimumD = chkMinimumD.get_Checked();
			OccultationElements.MinimumDistanceTest = (double)updnMinD.get_Value();
		}

		private void cmdReSetSites_Click(object sender, EventArgs e)
		{
			//IL_0007: Unknown result type (might be due to invalid IL or missing references)
			//IL_000e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0014: Invalid comparison between Unknown and I4
			SiteSelector siteSelector = new SiteSelector();
			((Form)siteSelector).ShowDialog();
			if ((int)((Form)siteSelector).get_DialogResult() == 1)
			{
				((Control)txtLongitude).set_Text(((Control)siteSelector.txtLongitude).get_Text());
				((Control)txtLatitude).set_Text(((Control)siteSelector.txtLatitude).get_Text());
				updnAperture.set_Value(decimal.Parse(((Control)siteSelector.txtAperture).get_Text()));
				Settings.Default.AsteroidSiteLongitude = ((Control)txtLongitude).get_Text();
				Settings.Default.AsteroidSiteLatitude = ((Control)txtLatitude).get_Text();
			}
			((Component)(object)siteSelector).Dispose();
		}

		private void chkDistArcSec_Click(object sender, EventArgs e)
		{
			if (chkDistArcSec.get_Checked())
			{
				chkDistArcSec.set_Checked(false);
				return;
			}
			chkDistArcSec.set_Checked(true);
			chkSiteDistKM.set_Checked(false);
		}

		private void chkSiteDistKM_Click(object sender, EventArgs e)
		{
			if (chkSiteDistKM.get_Checked())
			{
				chkSiteDistKM.set_Checked(false);
				return;
			}
			chkSiteDistKM.set_Checked(true);
			chkDistArcSec.set_Checked(false);
		}

		private void updnMaxDurn_Click(object sender, EventArgs e)
		{
			double num = (double)updnMaxDurn.get_Value();
			if (num > 19.9)
			{
				updnMaxDurn.set_DecimalPlaces(0);
				updnMaxDurn.set_Increment(5m);
				updnMaxDurn.set_Value((decimal)(5 * (int)(num / 5.0)));
			}
			else if (num > 9.9)
			{
				updnMaxDurn.set_DecimalPlaces(0);
				updnMaxDurn.set_Increment(2m);
				updnMaxDurn.set_Value((decimal)(2 * (int)(num / 2.0)));
			}
			else if (num > 0.9)
			{
				updnMaxDurn.set_DecimalPlaces(1);
				updnMaxDurn.set_Increment(0.5m);
			}
			else
			{
				updnMaxDurn.set_DecimalPlaces(1);
				updnMaxDurn.set_Increment(0.2m);
			}
		}

		private void updnDiameter_Click(object sender, EventArgs e)
		{
			double num = (double)updnDiameter.get_Value();
			if (num >= 100.0)
			{
				updnDiameter.set_Increment(50m);
				updnDiameter.set_Value((decimal)(50 * (int)(num / 50.0)));
			}
			else if (num >= 50.0)
			{
				updnDiameter.set_Increment(10m);
				updnDiameter.set_Value((decimal)(10 * (int)(num / 10.0)));
			}
			else if (num >= 20.0)
			{
				updnDiameter.set_Increment(5m);
				updnDiameter.set_Value((decimal)(5 * (int)(num / 5.0)));
			}
			else
			{
				updnDiameter.set_Increment(1m);
			}
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Asteroid - List and Display");
		}

		private void optStarMag_CheckedChanged(object sender, EventArgs e)
		{
			AsteroidSummaryLine.UseCombinedMagnitude = optCombined.get_Checked();
			ListEvents();
		}

		private void optCombined_CheckedChanged(object sender, EventArgs e)
		{
			AsteroidSummaryLine.UseCombinedMagnitude = optCombined.get_Checked();
			if (optCombined.get_Checked())
			{
				((Control)lblMagType).set_Text("combined");
			}
			else
			{
				((Control)lblMagType).set_Text("visual");
			}
		}

		private void cmdSaveListed_Click(object sender, EventArgs e)
		{
			SaveXML(All: false, OWfeed: false);
		}

		private void saveAllToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SaveXML(All: true, OWfeed: false);
		}

		private void saveListedEventsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Save(All: false, OWfeed: false);
		}

		private void saveListedEventsForOccultWatcherFeedToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Save(All: false, OWfeed: true);
		}

		private void saveListedEventsAsAPersonalOWFeedToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Save(All: false, OWfeed: true);
		}

		private void Save(bool All, bool OWfeed)
		{
			//IL_019f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0217: Unknown result type (might be due to invalid IL or missing references)
			//IL_021e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0225: Expected O, but got Unknown
			//IL_02ef: Unknown result type (might be due to invalid IL or missing references)
			//IL_02f5: Invalid comparison between Unknown and I4
			string text = "";
			string text2 = "";
			int num = 0;
			int result = -1;
			if (All)
			{
				for (int i = 0; i < DisplayMPOccultations.OccElements.Count; i++)
				{
					text += DisplayMPOccultations.OccElements[i].ElementsIn540Format;
				}
			}
			else
			{
				for (int j = 0; j < lstSummary.get_Items().get_Count(); j++)
				{
					text2 = lstSummary.get_Items().get_Item(j).ToString()!.Trim();
					if (text2.Length > 50)
					{
						num = text2.LastIndexOf(" ");
						if (!int.TryParse(text2.Substring(num), out result))
						{
							result = -1;
						}
						if (result >= 0)
						{
							text += DisplayMPOccultations.OccElements[result].ElementsIn540Format;
						}
					}
				}
			}
			if (text.Length <= 0)
			{
				return;
			}
			string text3 = "";
			if (OWfeed)
			{
				if (File.Exists(text3))
				{
					File.Delete(text3);
				}
				((Control)this).set_Cursor(Cursors.get_WaitCursor());
				using (Process.GetCurrentProcess())
				{
					Process[] processes = Process.GetProcesses();
					foreach (Process process in processes)
					{
						try
						{
							if (!(Path.GetFileName(process.MainModule!.FileName) == "OccultWatcher.exe"))
							{
								continue;
							}
							string text4 = Path.GetDirectoryName(process.MainModule!.FileName) + "\\Personal";
							if (Directory.Exists(text4))
							{
								text3 = text4 + "\\OccelmntSubsetForOW.dat";
								using (StreamWriter streamWriter = new StreamWriter(text3, append: false))
								{
									streamWriter.Write(text);
								}
								((Control)this).set_Cursor(Cursors.get_Default());
								MessageBox.Show("Personal predictions have been saved in the OccultWatcher Personal directory", "Saved to OccultWatcher Personal directory", (MessageBoxButtons)0, (MessageBoxIcon)64);
								return;
							}
						}
						catch
						{
						}
					}
				}
				text3 = Utilities.AppPath + "\\Generated Files\\OccelmntSubsetForOW.dat";
				using (StreamWriter streamWriter2 = new StreamWriter(text3, append: false))
				{
					streamWriter2.Write(text);
				}
				((Control)this).set_Cursor(Cursors.get_Default());
				MessageBox.Show("Personal predictions have been saved in the Occult Generated_Files directory. It will need to be copied to the OccultWatcher Personal directory", "Saved to Occult Generated_Files directory", (MessageBoxButtons)0, (MessageBoxIcon)64);
				return;
			}
			SaveFileDialog val = new SaveFileDialog();
			((FileDialog)val).set_CheckPathExists(true);
			((FileDialog)val).set_FileName("OcElmnt" + DateTime.UtcNow.Year + DateTime.UtcNow.Month.ToString().PadLeft(2, '0') + DateTime.UtcNow.Day.ToString().PadLeft(2, '0') + ".txt");
			((FileDialog)val).set_Title(((FileDialog)val).get_FileName());
			val.set_OverwritePrompt(true);
			((FileDialog)val).set_InitialDirectory(Utilities.AppPath + "\\Generated files");
			((FileDialog)val).set_Filter("Text files (*.txt )|*.txt|All files (*.*)|*.*");
			((FileDialog)val).set_FilterIndex(1);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				text3 = ((FileDialog)val).get_FileName();
			}
			if (!(text3 != ""))
			{
				return;
			}
			using StreamWriter streamWriter3 = new StreamWriter(text3, append: false);
			streamWriter3.Write(text);
		}

		private void copyListToolStripMenuItem_Click(object sender, EventArgs e)
		{
			string text = "";
			for (int i = 0; i < lstSummary.get_Items().get_Count(); i++)
			{
				text = text + lstSummary.get_Items().get_Item(i).ToString() + "\r\n";
			}
			Clipboard.SetText(text);
		}

		private void filesForOccultWatcherFeedToolStripMenuItem_Click(object sender, EventArgs e)
		{
			string text = Interaction.InputBox("Specify the OccultWatcher FeedName", "OW Feed name", "", -1, -1);
			if (!(text == ""))
			{
				AutoFileGeneration(ForOWfeed: true, text, "");
			}
		}

		private void AutoFileGeneration(bool ForOWfeed, string OWFeedName, string SiteFileName)
		{
			//IL_0032: Unknown result type (might be due to invalid IL or missing references)
			//IL_0038: Invalid comparison between Unknown and I4
			//IL_010c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0112: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("The world maps will be generated with a plot scale of" + string.Format(" {0,1:f1} ", AutoGeneratePlotScale) + "\r\n\r\nDo you want to continue?", "Confirm plot scale", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			int count = lstSummary.get_Items().get_Count();
			int num = 4 * CountOfDisplayedEvents;
			if (!ForOWfeed)
			{
				num = DisplayMPOccultations.GetNumberOfAutogeneratedFiles() * CountOfDisplayedEvents;
			}
			if (num < 1)
			{
				return;
			}
			string text = "";
			text = (ForOWfeed ? ("There are " + CountOfDisplayedEvents + " events, which will result in " + num + " files being generated.\r\n\r\nAre you sure you want to proceed?") : ("You are going to generate files using the site file\r\n\r\n        " + SiteFileName + "\r\n\r\nThere are " + CountOfDisplayedEvents + " events. With your current User Settings, " + num + " files will be generated.\r\n\r\nAre you sure you want to proceed?"));
			if ((int)MessageBox.Show(text, "Confirm Autogeneration of files", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			((Control)PBarSummary).set_Left(186);
			PBarSummary.set_Minimum(0);
			PBarSummary.set_Value(0);
			PBarSummary.set_Maximum(count + 1);
			((Control)PBarSummary).set_Visible(true);
			AutoGenerateCancelFlag = false;
			((Control)cmdAutoCancel).set_Visible(true);
			int result;
			if (ForOWfeed)
			{
				DisplayMPOccultations.OWFeedGen_Open(OWFeedName);
				using (StreamWriter streamWriter = new StreamWriter(DisplayMPOccultations.AutogenerateOWpath + "\\" + OWFeedName.Replace(" ", "_") + ".txt"))
				{
					for (int i = 0; i < count && i <= lstSummary.get_Items().get_Count(); i++)
					{
						string text2 = lstSummary.get_Items().get_Item(i).ToString()!.Trim();
						int num2 = text2.LastIndexOf(" ");
						if (num2 >= 1)
						{
							if (!int.TryParse(text2.Substring(num2), out result))
							{
								result = -1;
							}
							if (result >= 0)
							{
								PBarSummary.set_Value(i);
								SetPlotCenter(result);
								DisplayMPOccultations.OWFeedGen_AddEvent(result, FromListAndDisplay: true, AutoGeneratePlotScale);
								streamWriter.WriteLine(lstSummary.get_Items().get_Item(i).ToString());
							}
							Application.DoEvents();
							if (AutoGenerateCancelFlag)
							{
								break;
							}
						}
					}
				}
				DisplayMPOccultations.OWFeedGen_Close();
			}
			else
			{
				using StreamWriter outIndexFile = new StreamWriter(Utilities.AppPath + "\\AutoGenerated Asteroids\\FileIndex.txt", append: false);
				for (int j = 0; j < count && j <= lstSummary.get_Items().get_Count(); j++)
				{
					string text3 = lstSummary.get_Items().get_Item(j).ToString()!.Trim();
					int num3 = text3.LastIndexOf(" ");
					if (num3 >= 1)
					{
						if (!int.TryParse(text3.Substring(num3), out result))
						{
							result = -1;
						}
						if (result >= 0)
						{
							PBarSummary.set_Value(j);
							SetPlotCenter(result);
							DisplayMPOccultations.GeneratePredictionOutputFilesFromSummaryPage(FromListAndDisplay: true, "", result, SiteFileName, AutoGeneratePlotScale, UpdatedMultiSiteFile: false, outIndexFile);
						}
						Application.DoEvents();
						if (AutoGenerateCancelFlag)
						{
							break;
						}
					}
				}
			}
			((Control)cmdAutoCancel).set_Visible(false);
			((Control)PBarSummary).set_Visible(false);
		}

		private void SetPlotCenter(int RecordNumber)
		{
			if (!double.TryParse(((Control)txtLongitude).get_Text(), out var result))
			{
				result = 0.0;
			}
			if (!double.TryParse(((Control)txtLatitude).get_Text(), out var result2))
			{
				result2 = 0.0;
			}
			DisplayMPOccultations.SiteLongitude_Rad = result / (180.0 / Math.PI);
			DisplayMPOccultations.SiteLatitude_Rad = result2 / (180.0 / Math.PI);
			DisplayMPOccultations.UseRedrawSites = false;
			DisplayMPOccultations.FilterOnSite = true;
			if ((AutoGeneratePlotScale > 1.0) & mapsSitePathCenteredSiteToolStripMenuItem.get_Checked())
			{
				DisplayMPOccultations.Set_Data_From_OccultationElements_Record(RecordNumber, PlotEvent: false);
				AsteroidPathCoords CalculatedPath = new AsteroidPathCoords();
				bool ValidPath = false;
				DisplayMPOccultations.PathByLongitude(result, result2, UseLocalTopography: false, 0.0, ref CalculatedPath, out ValidPath);
				if (!ValidPath)
				{
					DisplayMPOccultations.PathByLatitude(result2, result, UseLocalTopography: false, 0.0, ref CalculatedPath, out ValidPath);
				}
				if (ValidPath)
				{
					DisplayMPOccultations.SiteLongitude_Rad = CalculatedPath.LongitudeCentre / (180.0 / Math.PI);
					DisplayMPOccultations.SiteLatitude_Rad = CalculatedPath.LatitudeCentre / (180.0 / Math.PI);
				}
			}
		}

		private void cmdAutoCancel_Click(object sender, EventArgs e)
		{
			AutoGenerateCancelFlag = true;
		}

		private void chkSite_CheckedChanged(object sender, EventArgs e)
		{
			((Control)pnlSiteOptions).set_Enabled(chkSite.get_Checked());
			ListEvents();
		}

		private void editListOfRegionsToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			//IL_0005: Unknown result type (might be due to invalid IL or missing references)
			((Form)new EditRegions()).ShowDialog();
			Regions_PopulateDropDownList();
		}

		internal void RemoveDuplicates()
		{
			OccultationElements.SortField = 0;
			DisplayMPOccultations.OccElements.Sort();
			for (int num = DisplayMPOccultations.OccElements.Count - 1; num > 0; num--)
			{
				if (DisplayMPOccultations.OccElements[num].ObjectName == DisplayMPOccultations.OccElements[num - 1].ObjectName && DisplayMPOccultations.OccElements[num].EventDay == DisplayMPOccultations.OccElements[num - 1].EventDay && DisplayMPOccultations.OccElements[num].StarCatName == DisplayMPOccultations.OccElements[num - 1].StarCatName)
				{
					DisplayMPOccultations.OccElements.RemoveAt(num);
				}
			}
			for (int i = 0; i < DisplayMPOccultations.OccElements.Count; i++)
			{
				DisplayMPOccultations.OccElements[i].RecordNumber = i;
			}
		}

		private void plotPathsOnAMapToolStripMenuItem_Click(object sender, EventArgs e)
		{
		}

		private void sunBelowHorizonToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.MultipathFileFromListAndDisplayIsCurrent = false;
			DisplayMPOccultations.Plot_MultiPaths(FromListAndDisplay: true, OnlyWhereSunIsDown: true);
		}

		private void fullPathToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.MultipathFileFromListAndDisplayIsCurrent = false;
			DisplayMPOccultations.Plot_MultiPaths(FromListAndDisplay: true, OnlyWhereSunIsDown: false);
		}

		private void savePathsAsGoogleEarthKMZToolStripMenuItem_Click(object sender, EventArgs e)
		{
		}

		private void plotAllPathsInGoogleEarthToolStripMenuItem_Click(object sender, EventArgs e)
		{
		}

		private void saveKMZlimitedByAltitudesOfStarSunsetByStarMagToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.MultipathFileFromListAndDisplayIsCurrent = false;
			DisplayMPOccultations.MultipathKMZfile(OnlyWhereSunIsDown_StarIsUp: true, Show: false);
		}

		private void saveKMZfullPathToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.MultipathFileFromListAndDisplayIsCurrent = false;
			DisplayMPOccultations.MultipathKMZfile(OnlyWhereSunIsDown_StarIsUp: false, Show: false);
		}

		private void GEplotlimitedByAltitudesOfSunStarByStarMagToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.MultipathFileFromListAndDisplayIsCurrent = false;
			DisplayMPOccultations.MultipathKMZfile(OnlyWhereSunIsDown_StarIsUp: true, Show: true);
		}

		private void GEplotfullPathToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.MultipathFileFromListAndDisplayIsCurrent = false;
			DisplayMPOccultations.MultipathKMZfile(OnlyWhereSunIsDown_StarIsUp: false, Show: true);
		}

		private void updnStartYear_ValueChanged(object sender, EventArgs e)
		{
			if (!AutoUpdatingStart)
			{
				updnStartDay.set_Maximum((decimal)(Utilities.MaximumDaysInMonth((int)updnStartYear.get_Value(), (int)updnStartMonth.get_Value()) + 1));
				if (((int)updnStartMonth.get_Value() == 2) & (updnStartDay.get_Value() >= 28m))
				{
					updnStartDay.set_Value(updnStartDay.get_Maximum() - 1m);
				}
				AutoUpdateEndDate(IsStartDate: true);
				ListEvents();
			}
		}

		private void updnEndYear_ValueChanged(object sender, EventArgs e)
		{
			if (!AutoUpdatingEnd)
			{
				updnEndDay.set_Maximum((decimal)(Utilities.MaximumDaysInMonth((int)updnEndYear.get_Value(), (int)updnEndMonth.get_Value()) + 1));
				if (((int)updnEndMonth.get_Value() == 2) & (updnEndDay.get_Value() >= 28m))
				{
					updnEndDay.set_Value(updnEndDay.get_Maximum() - 1m);
				}
				AutoUpdateEndDate(IsStartDate: false);
				ListEvents();
			}
		}

		private void updnStartMonth_ValueChanged(object sender, EventArgs e)
		{
			if (AutoUpdatingStart)
			{
				return;
			}
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
			AutoUpdateEndDate(IsStartDate: true);
			ListEvents();
		}

		private void updnEndMonth_ValueChanged(object sender, EventArgs e)
		{
			if (AutoUpdatingEnd)
			{
				return;
			}
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
			AutoUpdateEndDate(IsStartDate: false);
			ListEvents();
		}

		private void updnStartDay_ValueChanged(object sender, EventArgs e)
		{
			if (AutoUpdatingStart || ChangingMonth)
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
			AutoUpdateEndDate(IsStartDate: true);
			ListEvents();
		}

		private void updnEndDay_ValueChanged(object sender, EventArgs e)
		{
			if (AutoUpdatingEnd || ChangingMonth)
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
			AutoUpdateEndDate(IsStartDate: false);
			ListEvents();
		}

		private void AutoUpdateEndDate(bool IsStartDate)
		{
			if (IsLoading)
			{
				return;
			}
			int num = (int)updnStartMonth.get_Value();
			if (num == 0)
			{
				num = 12;
			}
			if (num == 13)
			{
				num = 1;
			}
			double num2 = Utilities.JD_from_Date((int)updnStartYear.get_Value(), num, (double)updnStartDay.get_Value());
			num = (int)updnEndMonth.get_Value();
			if (num == 0)
			{
				num = 12;
			}
			if (num == 13)
			{
				num = 1;
			}
			double num3 = Utilities.JD_from_Date((int)updnEndYear.get_Value(), num, (double)updnEndDay.get_Value());
			if (!(num2 < num3))
			{
				int Year;
				int Month;
				double day;
				if (IsStartDate)
				{
					Utilities.Date_from_JD(num2, out Year, out Month, out day);
					AutoUpdatingEnd = true;
					updnEndYear.set_Value((decimal)Year);
					updnEndMonth.set_Value((decimal)Month);
					AutoUpdatingEnd = false;
					updnEndDay.set_Value((decimal)day);
				}
				else
				{
					Utilities.Date_from_JD(num3, out Year, out Month, out day);
					AutoUpdatingStart = true;
					updnStartYear.set_Value((decimal)Year);
					updnStartMonth.set_Value((decimal)Month);
					AutoUpdatingStart = false;
					updnStartDay.set_Value((decimal)day);
				}
			}
		}

		private void xMLSaveAllEventsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SaveXML(All: true, OWfeed: false);
		}

		private void xMLSaveListedEventsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SaveXML(All: false, OWfeed: false);
		}

		private void updnLongNW_ValueChanged(object sender, EventArgs e)
		{
			OccultationElements.RegionNeedsUpdating = true;
		}

		private void updnLatNW_ValueChanged(object sender, EventArgs e)
		{
			OccultationElements.RegionNeedsUpdating = true;
		}

		private void updnLongNE_ValueChanged(object sender, EventArgs e)
		{
			OccultationElements.RegionNeedsUpdating = true;
		}

		private void updnLatNE_ValueChanged(object sender, EventArgs e)
		{
			OccultationElements.RegionNeedsUpdating = true;
		}

		private void updnLongSW_ValueChanged(object sender, EventArgs e)
		{
			OccultationElements.RegionNeedsUpdating = true;
		}

		private void updnLatSW_ValueChanged(object sender, EventArgs e)
		{
			OccultationElements.RegionNeedsUpdating = true;
		}

		private void updnLongSE_ValueChanged(object sender, EventArgs e)
		{
			OccultationElements.RegionNeedsUpdating = true;
		}

		private void updnLatSE_ValueChanged(object sender, EventArgs e)
		{
			OccultationElements.RegionNeedsUpdating = true;
		}

		private void txtLongitude_TextChanged(object sender, EventArgs e)
		{
			OccultationElements.SiteNeedsUpdating = true;
		}

		private void txtLatitude_TextChanged(object sender, EventArgs e)
		{
			OccultationElements.SiteNeedsUpdating = true;
		}

		private void updnStartYear_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnStartYear).Select(0, 5);
		}

		private void updnStartMonth_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnStartMonth).Select(0, 5);
		}

		private void updnStartDay_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnStartDay).Select(0, 5);
		}

		private void updnEndYear_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnEndYear).Select(0, 5);
		}

		private void updnEndMonth_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnEndMonth).Select(0, 5);
		}

		private void updnEndDay_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnEndDay).Select(0, 5);
		}

		private void txtLatitude_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtLatitude).Select(0, 8);
		}

		private void txtLongitude_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtLongitude).Select(0, 8);
		}

		private void updnDistKM_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnDistKM).Select(0, 8);
		}

		private void updnDistArcSec_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnDistArcSec).Select(0, 8);
		}

		private void updnLocalAltitude_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnLocalAltitude).Select(0, 8);
		}

		private void updnMagDrop_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnMagDrop).Select(0, 8);
		}

		private void updnStarMag_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnStarMag).Select(0, 8);
		}

		private void updnMaxDurn_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnMaxDurn).Select(0, 8);
		}

		private void updnDiameter_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnDiameter).Select(0, 8);
		}

		private void updnSolarElong_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnSolarElong).Select(0, 8);
		}

		private void updnLongNW_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnLongNW).Select(0, 8);
		}

		private void updnLatNW_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnLatNW).Select(0, 8);
		}

		private void updnLongNE_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnLongNE).Select(0, 8);
		}

		private void updnLatNE_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnLatNE).Select(0, 8);
		}

		private void updnLongSW_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnLongSW).Select(0, 8);
		}

		private void updnLatSW_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnLatSW).Select(0, 8);
		}

		private void updnLongSE_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnLongSE).Select(0, 8);
		}

		private void updnLatSE_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnLatSE).Select(0, 8);
		}

		private void updnUncertainty_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnUncertainty).Select(0, 8);
		}

		private void txtAsteroidNumber_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtAsteroidNumber).Select(0, 8);
		}

		private void txtAsteroidName_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtAsteroidName).Select(0, 16);
		}

		private void mapsSitePathCenteredSiteToolStripMenuItem_Click(object sender, EventArgs e)
		{
			mapsSitePathCenteredSiteToolStripMenuItem.set_Checked(!mapsSitePathCenteredSiteToolStripMenuItem.get_Checked());
			if (mapsSitePathCenteredSiteToolStripMenuItem.get_Checked())
			{
				((ToolStripItem)mapsSitePathCenteredSiteToolStripMenuItem).set_Text("Plot the maps Path/Site-centered [Path]");
			}
			else
			{
				((ToolStripItem)mapsSitePathCenteredSiteToolStripMenuItem).set_Text("Plot the maps Path/Site-centered [Site]");
			}
		}

		private void createUserAsteroidFileForAllListedAsteroidsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_002f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0035: Invalid comparison between Unknown and I4
			//IL_004f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0055: Invalid comparison between Unknown and I4
			//IL_0154: Unknown result type (might be due to invalid IL or missing references)
			//IL_015a: Invalid comparison between Unknown and I4
			//IL_01b3: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b9: Invalid comparison between Unknown and I4
			//IL_01fe: Unknown result type (might be due to invalid IL or missing references)
			//IL_0204: Invalid comparison between Unknown and I4
			int num = 0;
			string text = "";
			string text2 = "";
			if ((int)MessageBox.Show("This will create a User file of the listed asteroids, downloading the orbital elements from JPL-Horizons. This User file can then be used to generate predictions based on the Horizons elements, for all events that may be of interest to you\r\n\r\nThe retrieval of elements is not fast (depending on your Internet acess). For example, it may take about an hour to download the elements for 3600 asteroids. Make sure you sensibly filter the list to limit events that are truly of potential interest" + "\r\n\r\nDo you want to continue?", "Need to filter events", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7 || (int)MessageBox.Show("This routine will delete your current file of asteroid User Elements. It will also sort the display by asteroid number. \r\n\r\nIs this OK", "Overwriting User Elements file", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			SortAndDisplay(0);
			int eventYear = DisplayMPOccultations.OccElements[0].EventYear;
			int eventMonth = DisplayMPOccultations.OccElements[0].EventMonth;
			int eventDay = DisplayMPOccultations.OccElements[0].EventDay;
			int eventYear2 = DisplayMPOccultations.OccElements[DisplayMPOccultations.OccElements.Count - 1].EventYear;
			int eventMonth2 = DisplayMPOccultations.OccElements[DisplayMPOccultations.OccElements.Count - 1].EventMonth;
			int eventDay2 = DisplayMPOccultations.OccElements[DisplayMPOccultations.OccElements.Count - 1].EventDay;
			double num2 = Utilities.JD_from_Date(eventYear, eventMonth, eventDay);
			double num3 = Utilities.JD_from_Date(eventYear2, eventMonth2, eventDay2);
			Utilities.Date_from_JD(Math.Floor((num2 + num3) / 2.0) + 0.5, out var Year, out var Month, out var day);
			if (num3 - num2 > 183.0 && (int)MessageBox.Show("The period covered by this download is more than 6 months. This reduces the advantages of using the Horizons elements.\r\n\r\nIt would be better if you filtered your events to a period of about a month or two.\r\n\r\nDo you want to continue?", "Search period", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			int num4 = ((lstSummary.get_Items().get_Count() - 4) / 6 + 1) * 5;
			if ((num4 > 3600 && (int)MessageBox.Show("The estimated download time is about " + string.Format("{0,1:f1} mins", (double)num4 / 60.0) + "\r\n\r\nThe greater the number of events, the greater the risk of \r\na download issue that might result in the loss of all\r\ndownloaded data. \r\n\r\nYou should consider changing the filter settings so that \r\nfewer events are listed.\r\n\r\nDo you want to continue with the download?", "Number of downloads", (MessageBoxButtons)4, (MessageBoxIcon)48) == 7) || (num4 > 500 && (int)MessageBox.Show("The estimated download time is about " + string.Format("{0,1:f1} mins", (double)num4 / 60.0) + "\r\n\r\nDo you want to change the filter settings so that fewer events are listed?", "Number of downloads", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6))
			{
				return;
			}
			SortAndDisplay(8);
			string path = Utilities.AppPath + "\\Resource Files\\UserMinorPlanetElements.csv";
			PBar pBar = new PBar();
			((Control)pBar).set_Text("Downloading Horizons");
			pBar.pBarFTP.set_Minimum(0);
			pBar.pBarFTP.set_Maximum(DisplayMPOccultations.OccElements.Count);
			((Control)pBar).Show();
			((Control)PanelCancel).set_Visible(true);
			CancelHorizons = false;
			using (StreamWriter streamWriter = new StreamWriter(path))
			{
				streamWriter.WriteLine(AsteroidElements.CSVHeader());
				string text3 = "";
				for (int i = 0; i < DisplayMPOccultations.OccElements.Count; i++)
				{
					if (!DisplayMPOccultations.OccElements[i].IsEventForOutput)
					{
						continue;
					}
					pBar.pBarFTP.set_Value(i);
					Application.DoEvents();
					if (CancelHorizons)
					{
						break;
					}
					int asteroidNumber = DisplayMPOccultations.OccElements[i].AsteroidNumber;
					text2 = DisplayMPOccultations.OccElements[i].ObjectName;
					if (text2.Contains("#") || (DisplayMPOccultations.OccElements[i].IsPlanet | DisplayMPOccultations.OccElements[i].IsPlanetaryMoon) || asteroidNumber == 134340)
					{
						continue;
					}
					if (num > 0)
					{
						if (asteroidNumber == num)
						{
							continue;
						}
						text3 = ((asteroidNumber != 0) ? asteroidNumber.ToString() : text2);
					}
					else
					{
						if (text2 == text)
						{
							continue;
						}
						text3 = text2;
					}
					num = asteroidNumber;
					text = text2;
					AsteroidElements AE = new AsteroidElements();
					http.GetHorizonsElements_TDBtime(text3, Year, Month, (int)day, 0.0, ref AE);
					streamWriter.WriteLine(AE.CSVString());
				}
			}
			((Control)PanelCancel).set_Visible(false);
			((Form)pBar).Close();
			SortAndDisplay(0);
		}

		private void cmdCancelHorizons_Click(object sender, EventArgs e)
		{
			CancelHorizons = true;
		}

		private void optIntegrate_Click(object sender, EventArgs e)
		{
			optIntegrate.set_Checked(!optIntegrate.get_Checked());
		}

		private void setLocalHorizonAltitudesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0005: Unknown result type (might be due to invalid IL or missing references)
			((Form)new SetHorizon()).ShowDialog();
		}

		private void lstSummary_MouseUp(object sender, MouseEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Invalid comparison between Unknown and I4
			//IL_0049: Unknown result type (might be due to invalid IL or missing references)
			//IL_004f: Invalid comparison between Unknown and I4
			//IL_017b: Unknown result type (might be due to invalid IL or missing references)
			if ((int)e.get_Button() != 2097152)
			{
				return;
			}
			int num = 0;
			if ((int)MessageBox.Show(string.Concat("IMPORTANT\r\nThe preferred way to generate the most accurate predictions\r\nis to set the option on the Search form:\r\n\r\n   'use Horizons ephemeris (predictions will be slow)'\r\n\r\n" + "".PadRight(35, '=') + "\r\nIf you continue with the present option:\r\nOrbital elements from Horizons for the time of the event will be downloaded\r\n\r\nThis will create a 'User Elements file', and a new prediction for that day will be generated.\r\nThis will provide a 'full accuracy' prediction for that event, from which reliable topographic path coordinates can be generated.\r\nHowever it does not allow for any very small changes in the\r\n orbital elements that occur over the duration of the event.", "\r\n\r\nDo you want to continue?"), "Elements for a single event", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			string text = ((Control)lstSummary).get_Text().ToString().Trim();
			if (text.Length < 50)
			{
				return;
			}
			int startIndex = text.LastIndexOf(" ");
			if (!int.TryParse(text.Substring(startIndex), out var result))
			{
				return;
			}
			int eventYear = DisplayMPOccultations.OccElements[result].EventYear;
			int eventMonth = DisplayMPOccultations.OccElements[result].EventMonth;
			int eventDay = DisplayMPOccultations.OccElements[result].EventDay;
			double epochHour = (double)DisplayMPOccultations.OccElements[result].EventHour + DisplayMPOccultations.OccElements[result].EventMin / 60.0;
			string path = Utilities.AppPath + "\\Resource Files\\UserMinorPlanetElements.csv";
			int asteroidNumber = DisplayMPOccultations.OccElements[result].AsteroidNumber;
			string text2 = DisplayMPOccultations.OccElements[result].ObjectName;
			if ((text2.Contains("#") | DisplayMPOccultations.OccElements[result].IsPlanet | DisplayMPOccultations.OccElements[result].IsPlanetaryMoon) || asteroidNumber == 134340)
			{
				MessageBox.Show("Cannot retrieve elements for the selected entry", "Can't use", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			int num2 = text2.IndexOf("P/");
			if (num2 > 0)
			{
				text2 = text2.Substring(0, num2 + 1);
			}
			if (text2.Contains("(P/") | text2.Contains("(C/"))
			{
				num2 = text2.IndexOf("(P/") + 3;
				if (num2 < 3)
				{
					num2 = text2.IndexOf("(C/") + 3;
				}
				int num3 = text2.IndexOf(")", num2);
				text2 = text2.Substring(num2, num3 - num2);
			}
			string asteroidNumber2 = ((num <= 0) ? text2 : ((asteroidNumber != 0) ? asteroidNumber.ToString() : text2));
			using (StreamWriter streamWriter = new StreamWriter(path))
			{
				AsteroidElements AE = new AsteroidElements();
				http.GetHorizonsElements_TDBtime(asteroidNumber2, eventYear, eventMonth, eventDay, epochHour, ref AE);
				streamWriter.WriteLine(AsteroidElements.CSVHeader());
				streamWriter.WriteLine(AE.CSVString());
			}
			((Control)PanelCancel).set_Visible(false);
			chkStarID.set_Checked(true);
			((Control)txtStarID).set_Text(DisplayMPOccultations.OccElements[result].StarCatName);
			if (DisplayMPOccultations.SearchForm == null)
			{
				DisplayMPOccultations.ShowSearch();
			}
			if (((Control)DisplayMPOccultations.SearchForm).get_IsDisposed())
			{
				DisplayMPOccultations.ShowSearch();
			}
			((Control)DisplayMPOccultations.SearchForm).Show();
			DisplayMPOccultations.SearchForm.updnStartYear.set_Value((decimal)eventYear);
			DisplayMPOccultations.SearchForm.updnEndYear.set_Value((decimal)eventYear);
			DisplayMPOccultations.SearchForm.updnStartMonth.set_Value((decimal)eventMonth);
			DisplayMPOccultations.SearchForm.updnEndMonth.set_Value((decimal)eventMonth);
			DisplayMPOccultations.SearchForm.updnStartDay.set_Value((decimal)eventDay);
			DisplayMPOccultations.SearchForm.updnEndDay.set_Value((decimal)eventDay);
			DisplayMPOccultations.SearchForm.chkHorizons.set_Checked(false);
			DisplayMPOccultations.SearchForm.chkAutoSetHorizons.set_Checked(false);
			DisplayMPOccultations.SearchForm.chkDiameterLimit.set_Checked(false);
			DisplayMPOccultations.SearchForm.chkMinDuration.set_Checked(false);
			DisplayMPOccultations.SearchForm.chkUserElements.set_Checked(true);
			DisplayMPOccultations.SearchForm.Search();
			DisplayMPOccultations.SearchForm.chkUserElements.set_Checked(false);
			DisplayMPOccultations.SearchForm.chkAutoSetHorizons.set_Checked(true);
			((Control)lstSummary).set_BackColor(Color.LightCyan);
			try
			{
				((Control)DisplayMPOccultations.Plot).Hide();
			}
			catch
			{
			}
			try
			{
				((Control)DisplayMPOccultations.PathCoordsForm).Hide();
			}
			catch
			{
			}
		}

		private void chkStarID_MouseClick(object sender, MouseEventArgs e)
		{
			chkStarID.set_Checked(!chkStarID.get_Checked());
			if (!chkStarID.get_Checked())
			{
				((Control)lstSummary).set_BackColor(SystemColors.Window);
			}
		}

		private void clearAllEventsFromMemoryToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.OccElements.Clear();
			DisplayMPOccultations.Current_OccElements_Record = -1;
			SortAndDisplay(0);
		}

		private void clearAllUndisplayedEventsFromMemoryToolStripMenuItem_Click(object sender, EventArgs e)
		{
			int num = 0;
			string text = lstSummary.get_Items().get_Item(lstSummary.get_Items().get_Count() - 1).ToString()!.Trim();
			if (text == "")
			{
				num = 1;
				text = lstSummary.get_Items().get_Item(lstSummary.get_Items().get_Count() - 1 - num).ToString()!.Trim();
			}
			int startIndex = text.LastIndexOf(" ");
			if (!int.TryParse(text.Substring(startIndex), out var result))
			{
				return;
			}
			int num2 = result;
			int num3 = DisplayMPOccultations.OccElements.Count - 1 - num2;
			if (num3 > 0)
			{
				DisplayMPOccultations.OccElements.RemoveRange(num2 + 1, num3);
			}
			for (int num4 = lstSummary.get_Items().get_Count() - 2 - num; num4 >= 0; num4--)
			{
				text = lstSummary.get_Items().get_Item(num4).ToString()!.Trim();
				if (text.Length >= 50)
				{
					startIndex = text.LastIndexOf(" ");
					if (startIndex >= 0 && int.TryParse(text.Substring(startIndex), out result))
					{
						int num5 = num2;
						num2 = result;
						num3 = num5 - num2 - 1;
						if (num3 > 0)
						{
							DisplayMPOccultations.OccElements.RemoveRange(num2 + 1, num3);
						}
					}
				}
			}
			int num6 = num2;
			num2 = 0;
			num3 = num6;
			if (num3 > 0)
			{
				DisplayMPOccultations.OccElements.RemoveRange(num2, num3);
			}
			SortAndDisplay(CurrentSortField);
		}

		private void rermoveSelectedEventToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_007d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0083: Invalid comparison between Unknown and I4
			string text = lstSummary.get_Items().get_Item(((ListControl)lstSummary).get_SelectedIndex()).ToString()!.Trim();
			if (text.Length >= 50)
			{
				int num = text.LastIndexOf(" ");
				if (num >= 0 && int.TryParse(text.Substring(num), out var result) && (int)MessageBox.Show(" You are about to delete the entry starting with\r\n\r\n" + text.Substring(0, 115) + ".....\r\n\r\nDo you want to delete this entry?", "Delete an entry", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
				{
					DisplayMPOccultations.OccElements.RemoveAt(result);
					SortAndDisplay(CurrentSortField);
				}
			}
		}

		private void copyOccultationElementsOfSelectedEventToolStripMenuItem_Click(object sender, EventArgs e)
		{
			string text = lstSummary.get_Items().get_Item(((ListControl)lstSummary).get_SelectedIndex()).ToString()!.Trim();
			if (text.Length >= 50)
			{
				int num = text.LastIndexOf(" ");
				if (num >= 0 && int.TryParse(text.Substring(num), out var result))
				{
					Clipboard.SetText(DisplayMPOccultations.OccElements[result].XML_Elements().ToString());
				}
			}
		}

		private void newToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMPOccultations.OccElements.Clear();
			DisplayMPOccultations.Current_OccElements_Record = -1;
			SortAndDisplay(0);
		}

		private void chkInRegion_CheckedChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void chkMagDrop_CheckedChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void chkStarMag_CheckedChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void chkDuration_CheckedChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void chkDiameter_CheckedChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void chkSolarElongation_CheckedChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void chkAperture_CheckedChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void chkPlanet_CheckedChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void chkShapeModel_CheckedChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void chkBinary_CheckedChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void chkRings_CheckedChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void chkUncertainty_CheckedChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void chkFuture_CheckedChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void chkDate_CheckedChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void chkAsteroidNumber_CheckedChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void chkAsteroidName_CheckedChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void chkAsteroidClass_CheckedChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void chkStarID_CheckedChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void chkProbability_CheckedChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void chkSiteDistKM_CheckedChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void chkDistArcSec_CheckedChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void chkLocalAltitude_CheckedChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void chkHorizon_CheckedChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void optIntegrate_CheckedChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void updnMagDrop_ValueChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void updnStarMag_ValueChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void updnMaxDurn_ValueChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void updnDiameter_ValueChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void updnSolarElong_ValueChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void updnAperture_ValueChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void cmbPlanet_SelectedIndexChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void updnUncertainty_ValueChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void cmbProbabilityValues_SelectedIndexChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void updnDistKM_ValueChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void updnDistArcSec_ValueChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void updnLocalAltitude_ValueChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void cmbAsteroidClasses_SelectedIndexChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void updnMinD_ValueChanged(object sender, EventArgs e)
		{
			ListEvents();
		}

		private void txtAsteroidNumber_KeyDown(object sender, KeyEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_0008: Invalid comparison between Unknown and I4
			if ((int)e.get_KeyCode() == 13)
			{
				ListEvents();
			}
		}

		private void txtAsteroidName_KeyDown(object sender, KeyEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_0008: Invalid comparison between Unknown and I4
			if ((int)e.get_KeyCode() == 13)
			{
				ListEvents();
			}
		}

		private void txtStarID_KeyDown(object sender, KeyEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_0008: Invalid comparison between Unknown and I4
			if ((int)e.get_KeyCode() == 13)
			{
				ListEvents();
			}
		}

		private void txtLongitude_KeyDown(object sender, KeyEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_0008: Invalid comparison between Unknown and I4
			if ((int)e.get_KeyCode() == 13)
			{
				ListEvents();
			}
		}

		private void txtLatitude_KeyDown(object sender, KeyEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_0008: Invalid comparison between Unknown and I4
			if ((int)e.get_KeyCode() == 13)
			{
				ListEvents();
			}
		}

		private void SaveXML(bool All, bool OWfeed)
		{
			//IL_0124: Unknown result type (might be due to invalid IL or missing references)
			//IL_012b: Expected O, but got Unknown
			//IL_01f5: Unknown result type (might be due to invalid IL or missing references)
			//IL_01fb: Invalid comparison between Unknown and I4
			string text = "<Occultations>\r\n";
			string text2 = "";
			int num = 0;
			int result = -1;
			Cursor.set_Current(Cursors.get_WaitCursor());
			if (All)
			{
				for (int i = 0; i < DisplayMPOccultations.OccElements.Count; i++)
				{
					text += DisplayMPOccultations.OccElements[i].XML_Elements();
				}
			}
			else
			{
				for (int j = 0; j < lstSummary.get_Items().get_Count(); j++)
				{
					text2 = lstSummary.get_Items().get_Item(j).ToString()!.Trim();
					if (text2.Length > 50)
					{
						num = text2.LastIndexOf(" ");
						if (!int.TryParse(text2.Substring(num), out result))
						{
							result = -1;
						}
						if (result >= 0)
						{
							text += DisplayMPOccultations.OccElements[result].XML_Elements();
						}
					}
				}
			}
			Cursor.set_Current(Cursors.get_Default());
			text += "</Occultations>\r\n";
			if (text.Length <= 0)
			{
				return;
			}
			string text3 = "";
			if (OWfeed)
			{
				text3 = Utilities.AppPath + "\\Generated Files\\OccelmntSubsetForOW.dat";
				if (File.Exists(text3))
				{
					File.Delete(text3);
				}
			}
			else
			{
				SaveFileDialog val = new SaveFileDialog();
				((FileDialog)val).set_CheckPathExists(true);
				((FileDialog)val).set_FileName("OcElmnt" + DateTime.UtcNow.Year + DateTime.UtcNow.Month.ToString().PadLeft(2, '0') + DateTime.UtcNow.Day.ToString().PadLeft(2, '0') + ".xml");
				((FileDialog)val).set_Title(((FileDialog)val).get_FileName());
				val.set_OverwritePrompt(true);
				((FileDialog)val).set_InitialDirectory(Utilities.AppPath + "\\Generated files");
				((FileDialog)val).set_Filter("XML files (*.xml )|*.xml|All files (*.*)|*.*");
				((FileDialog)val).set_FilterIndex(0);
				if ((int)((CommonDialog)val).ShowDialog() == 1)
				{
					text3 = ((FileDialog)val).get_FileName();
				}
			}
			if (text3 != "")
			{
				using (StreamWriter streamWriter = new StreamWriter(text3, append: false))
				{
					streamWriter.Write(text);
				}
			}
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
			//IL_000c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0016: Expected O, but got Unknown
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_0021: Expected O, but got Unknown
			//IL_0022: Unknown result type (might be due to invalid IL or missing references)
			//IL_002c: Expected O, but got Unknown
			//IL_002d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0037: Expected O, but got Unknown
			//IL_0038: Unknown result type (might be due to invalid IL or missing references)
			//IL_0042: Expected O, but got Unknown
			//IL_0043: Unknown result type (might be due to invalid IL or missing references)
			//IL_004d: Expected O, but got Unknown
			//IL_004e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0058: Expected O, but got Unknown
			//IL_0059: Unknown result type (might be due to invalid IL or missing references)
			//IL_0063: Expected O, but got Unknown
			//IL_0064: Unknown result type (might be due to invalid IL or missing references)
			//IL_006e: Expected O, but got Unknown
			//IL_006f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0079: Expected O, but got Unknown
			//IL_007a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0084: Expected O, but got Unknown
			//IL_0085: Unknown result type (might be due to invalid IL or missing references)
			//IL_008f: Expected O, but got Unknown
			//IL_0090: Unknown result type (might be due to invalid IL or missing references)
			//IL_009a: Expected O, but got Unknown
			//IL_009b: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a5: Expected O, but got Unknown
			//IL_00a6: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b0: Expected O, but got Unknown
			//IL_00b1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00bb: Expected O, but got Unknown
			//IL_00bc: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c6: Expected O, but got Unknown
			//IL_00c7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d1: Expected O, but got Unknown
			//IL_00d2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00dc: Expected O, but got Unknown
			//IL_00dd: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e7: Expected O, but got Unknown
			//IL_00e8: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f2: Expected O, but got Unknown
			//IL_00f3: Unknown result type (might be due to invalid IL or missing references)
			//IL_00fd: Expected O, but got Unknown
			//IL_00fe: Unknown result type (might be due to invalid IL or missing references)
			//IL_0108: Expected O, but got Unknown
			//IL_0109: Unknown result type (might be due to invalid IL or missing references)
			//IL_0113: Expected O, but got Unknown
			//IL_0114: Unknown result type (might be due to invalid IL or missing references)
			//IL_011e: Expected O, but got Unknown
			//IL_011f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0129: Expected O, but got Unknown
			//IL_012a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0134: Expected O, but got Unknown
			//IL_0135: Unknown result type (might be due to invalid IL or missing references)
			//IL_013f: Expected O, but got Unknown
			//IL_0140: Unknown result type (might be due to invalid IL or missing references)
			//IL_014a: Expected O, but got Unknown
			//IL_014b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0155: Expected O, but got Unknown
			//IL_0156: Unknown result type (might be due to invalid IL or missing references)
			//IL_0160: Expected O, but got Unknown
			//IL_0161: Unknown result type (might be due to invalid IL or missing references)
			//IL_016b: Expected O, but got Unknown
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
			//IL_0776: Unknown result type (might be due to invalid IL or missing references)
			//IL_0780: Expected O, but got Unknown
			//IL_0781: Unknown result type (might be due to invalid IL or missing references)
			//IL_078b: Expected O, but got Unknown
			//IL_078c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0796: Expected O, but got Unknown
			//IL_0797: Unknown result type (might be due to invalid IL or missing references)
			//IL_07a1: Expected O, but got Unknown
			//IL_07a2: Unknown result type (might be due to invalid IL or missing references)
			//IL_07ac: Expected O, but got Unknown
			//IL_07ad: Unknown result type (might be due to invalid IL or missing references)
			//IL_07b7: Expected O, but got Unknown
			//IL_07b8: Unknown result type (might be due to invalid IL or missing references)
			//IL_07c2: Expected O, but got Unknown
			//IL_07c3: Unknown result type (might be due to invalid IL or missing references)
			//IL_07cd: Expected O, but got Unknown
			//IL_07ce: Unknown result type (might be due to invalid IL or missing references)
			//IL_07d8: Expected O, but got Unknown
			//IL_07d9: Unknown result type (might be due to invalid IL or missing references)
			//IL_07e3: Expected O, but got Unknown
			//IL_1156: Unknown result type (might be due to invalid IL or missing references)
			//IL_1160: Expected O, but got Unknown
			//IL_11e5: Unknown result type (might be due to invalid IL or missing references)
			//IL_11ef: Expected O, but got Unknown
			//IL_340b: Unknown result type (might be due to invalid IL or missing references)
			//IL_3415: Expected O, but got Unknown
			//IL_34d0: Unknown result type (might be due to invalid IL or missing references)
			//IL_34da: Expected O, but got Unknown
			//IL_35bf: Unknown result type (might be due to invalid IL or missing references)
			//IL_35c9: Expected O, but got Unknown
			//IL_36b1: Unknown result type (might be due to invalid IL or missing references)
			//IL_36bb: Expected O, but got Unknown
			//IL_379d: Unknown result type (might be due to invalid IL or missing references)
			//IL_37a7: Expected O, but got Unknown
			//IL_388e: Unknown result type (might be due to invalid IL or missing references)
			//IL_3898: Expected O, but got Unknown
			//IL_397d: Unknown result type (might be due to invalid IL or missing references)
			//IL_3987: Expected O, but got Unknown
			//IL_3a6f: Unknown result type (might be due to invalid IL or missing references)
			//IL_3a79: Expected O, but got Unknown
			//IL_3b5b: Unknown result type (might be due to invalid IL or missing references)
			//IL_3b65: Expected O, but got Unknown
			//IL_3ef0: Unknown result type (might be due to invalid IL or missing references)
			//IL_3efa: Expected O, but got Unknown
			//IL_4074: Unknown result type (might be due to invalid IL or missing references)
			//IL_407e: Expected O, but got Unknown
			//IL_423a: Unknown result type (might be due to invalid IL or missing references)
			//IL_4244: Expected O, but got Unknown
			//IL_4314: Unknown result type (might be due to invalid IL or missing references)
			//IL_431e: Expected O, but got Unknown
			//IL_4582: Unknown result type (might be due to invalid IL or missing references)
			//IL_458c: Expected O, but got Unknown
			//IL_465c: Unknown result type (might be due to invalid IL or missing references)
			//IL_4666: Expected O, but got Unknown
			//IL_4736: Unknown result type (might be due to invalid IL or missing references)
			//IL_4740: Expected O, but got Unknown
			//IL_4813: Unknown result type (might be due to invalid IL or missing references)
			//IL_481d: Expected O, but got Unknown
			//IL_5186: Unknown result type (might be due to invalid IL or missing references)
			//IL_5190: Expected O, but got Unknown
			//IL_521c: Unknown result type (might be due to invalid IL or missing references)
			//IL_5226: Expected O, but got Unknown
			//IL_57d8: Unknown result type (might be due to invalid IL or missing references)
			//IL_57e2: Expected O, but got Unknown
			//IL_58df: Unknown result type (might be due to invalid IL or missing references)
			//IL_58e9: Expected O, but got Unknown
			//IL_597c: Unknown result type (might be due to invalid IL or missing references)
			//IL_5986: Expected O, but got Unknown
			//IL_5a8b: Unknown result type (might be due to invalid IL or missing references)
			//IL_5a95: Expected O, but got Unknown
			//IL_5b27: Unknown result type (might be due to invalid IL or missing references)
			//IL_5b31: Expected O, but got Unknown
			//IL_5c0a: Unknown result type (might be due to invalid IL or missing references)
			//IL_5c14: Expected O, but got Unknown
			//IL_5d06: Unknown result type (might be due to invalid IL or missing references)
			//IL_5d10: Expected O, but got Unknown
			//IL_5e13: Unknown result type (might be due to invalid IL or missing references)
			//IL_5e1d: Expected O, but got Unknown
			//IL_5f28: Unknown result type (might be due to invalid IL or missing references)
			//IL_5f32: Expected O, but got Unknown
			//IL_6003: Unknown result type (might be due to invalid IL or missing references)
			//IL_600d: Expected O, but got Unknown
			//IL_60dc: Unknown result type (might be due to invalid IL or missing references)
			//IL_60e6: Expected O, but got Unknown
			//IL_61b6: Unknown result type (might be due to invalid IL or missing references)
			//IL_61c0: Expected O, but got Unknown
			//IL_629c: Unknown result type (might be due to invalid IL or missing references)
			//IL_62a6: Expected O, but got Unknown
			//IL_6767: Unknown result type (might be due to invalid IL or missing references)
			//IL_6771: Expected O, but got Unknown
			//IL_6808: Unknown result type (might be due to invalid IL or missing references)
			//IL_6812: Expected O, but got Unknown
			//IL_6907: Unknown result type (might be due to invalid IL or missing references)
			//IL_6911: Expected O, but got Unknown
			//IL_69ff: Unknown result type (might be due to invalid IL or missing references)
			//IL_6a09: Expected O, but got Unknown
			//IL_6b2c: Unknown result type (might be due to invalid IL or missing references)
			//IL_6b36: Expected O, but got Unknown
			//IL_6c39: Unknown result type (might be due to invalid IL or missing references)
			//IL_6c43: Expected O, but got Unknown
			//IL_6d44: Unknown result type (might be due to invalid IL or missing references)
			//IL_6d4e: Expected O, but got Unknown
			//IL_6f13: Unknown result type (might be due to invalid IL or missing references)
			//IL_6f1d: Expected O, but got Unknown
			//IL_6fe5: Unknown result type (might be due to invalid IL or missing references)
			//IL_6fef: Expected O, but got Unknown
			//IL_704f: Unknown result type (might be due to invalid IL or missing references)
			//IL_7059: Expected O, but got Unknown
			//IL_71c5: Unknown result type (might be due to invalid IL or missing references)
			//IL_71cf: Expected O, but got Unknown
			components = new Container();
			label12 = new Label();
			label11 = new Label();
			cmdReSetSites = new Button();
			label6 = new Label();
			updnEndYear = new NumericUpDown();
			updnStartYear = new NumericUpDown();
			cmbPlanet = new ComboBox();
			label5 = new Label();
			label4 = new Label();
			label3 = new Label();
			label2 = new Label();
			label1 = new Label();
			txtAsteroidName = new TextBox();
			txtAsteroidNumber = new TextBox();
			chkAsteroidNumber = new CheckBox();
			chkAsteroidName = new CheckBox();
			menuStrip1 = new MenuStrip();
			fileToolStripMenuItem = new ToolStripMenuItem();
			newToolStripMenuItem = new ToolStripMenuItem();
			readToolStripMenuItem = new ToolStripMenuItem();
			openAndMergeToolStripMenuItem = new ToolStripMenuItem();
			openAndMergeDuplicatesRetainedToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator10 = new ToolStripSeparator();
			pasteOneOrMoreXMLEventPredictionToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator11 = new ToolStripSeparator();
			rermoveSelectedEventToolStripMenuItem = new ToolStripMenuItem();
			clearAllUndisplayedEventsFromMemoryToolStripMenuItem = new ToolStripMenuItem();
			clearAllEventsFromMemoryToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			copyOccultationElementsOfSelectedEventToolStripMenuItem = new ToolStripMenuItem();
			copyListToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator3 = new ToolStripSeparator();
			xMLSaveListedEventsToolStripMenuItem = new ToolStripMenuItem();
			xMLSaveAllEventsToolStripMenuItem = new ToolStripMenuItem();
			saveListedEventsForOccultWatcherFeedToolStripMenuItem = new ToolStripMenuItem();
			createUserAsteroidFileForAllListedAsteroidsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator7 = new ToolStripSeparator();
			editListOfRegionsToolStripMenuItem1 = new ToolStripMenuItem();
			setLocalHorizonAltitudesToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator4 = new ToolStripSeparator();
			plotPathsOnAMapToolStripMenuItem = new ToolStripMenuItem();
			sunBelowHorizonToolStripMenuItem = new ToolStripMenuItem();
			fullPathToolStripMenuItem = new ToolStripMenuItem();
			plotAllPathsInGoogleEarthToolStripMenuItem = new ToolStripMenuItem();
			GEplotlimitedByAltitudesOfSunStarByStarMagToolStripMenuItem = new ToolStripMenuItem();
			GEplotfullPathToolStripMenuItem = new ToolStripMenuItem();
			savePathsAsGoogleEarthKMZToolStripMenuItem = new ToolStripMenuItem();
			saveKMZlimitedByAltitudesOfStarSunsetByStarMagToolStripMenuItem = new ToolStripMenuItem();
			saveKMZfullPathToolStripMenuItem1 = new ToolStripMenuItem();
			toolStripSeparator6 = new ToolStripSeparator();
			saveListedEventsToolStripMenuItem = new ToolStripMenuItem();
			saveAllToolStripMenuItem = new ToolStripMenuItem();
			sortToolStripMenuItem = new ToolStripMenuItem();
			byDateToolStripMenuItem = new ToolStripMenuItem();
			byDiameterkmToolStripMenuItem = new ToolStripMenuItem();
			byDiameterapparentToolStripMenuItem = new ToolStripMenuItem();
			byDurationToolStripMenuItem = new ToolStripMenuItem();
			byRateOfMotionkmsec = new ToolStripMenuItem();
			byStarMagnitudeToolStripMenuItem = new ToolStripMenuItem();
			byCombinedMagnitudeToolStripMenuItem = new ToolStripMenuItem();
			byMagnitudeDropViosualToolStripMenuItem = new ToolStripMenuItem();
			byMagnitudeDropRedToolStripMenuItem = new ToolStripMenuItem();
			byElongationToolStripMenuItem = new ToolStripMenuItem();
			byDistanceToolStripMenuItem = new ToolStripMenuItem();
			byProbabilityToolStripMenuItem = new ToolStripMenuItem();
			byNameToolStripMenuItem = new ToolStripMenuItem();
			byNumberToolStripMenuItem = new ToolStripMenuItem();
			byStarNumberToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			listStarsOccultedMoreThanOnceToolStripMenuItem = new ToolStripMenuItem();
			autoFileGenerationForAllListedToolStripMenuItem = new ToolStripMenuItem();
			setScaleForWorldMapsToolStripMenuItem = new ToolStripMenuItem();
			mapsSitePathCenteredSiteToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator5 = new ToolStripSeparator();
			generationToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator8 = new ToolStripSeparator();
			filesForOccultWatcherFeedToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator9 = new ToolStripSeparator();
			saveListedEventsAsAPersonalOWFeedToolStripMenuItem = new ToolStripMenuItem();
			toolStripMenuItem1 = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			label7 = new Label();
			pnlFilters = new Panel();
			lblEventCount = new Label();
			pnlRegion = new Panel();
			cmbRegion = new ComboBox();
			label29 = new Label();
			label28 = new Label();
			label24 = new Label();
			label23 = new Label();
			label21 = new Label();
			label22 = new Label();
			label20 = new Label();
			label19 = new Label();
			chkInRegion = new CheckBox();
			updnLatSE = new NumericUpDown();
			updnLongSE = new NumericUpDown();
			updnLatSW = new NumericUpDown();
			updnLongSW = new NumericUpDown();
			updnLatNE = new NumericUpDown();
			updnLongNE = new NumericUpDown();
			updnLatNW = new NumericUpDown();
			updnLongNW = new NumericUpDown();
			cmdSaveListed = new Button();
			cmdListEvents = new Button();
			pnlOtherFilter = new Panel();
			updnMinD = new NumericUpDown();
			label30 = new Label();
			updnUncertainty = new NumericUpDown();
			label18 = new Label();
			chkUncertainty = new CheckBox();
			chkMinimumD = new CheckBox();
			label13 = new Label();
			label10 = new Label();
			label9 = new Label();
			chkRings = new CheckBox();
			chkPlanet = new CheckBox();
			chkShapeModel = new CheckBox();
			chkBinary = new CheckBox();
			pnlDateFilter = new Panel();
			panel1 = new Panel();
			label14 = new Label();
			optCombined = new RadioButton();
			optStarMag = new RadioButton();
			updnEndDay = new NumericUpDown();
			updnStartDay = new NumericUpDown();
			updnEndMonth = new NumericUpDown();
			updnStartMonth = new NumericUpDown();
			chkDate = new CheckBox();
			pnlIDfilter = new Panel();
			txtStarID = new TextBox();
			chkStarID = new CheckBox();
			label17 = new Label();
			label16 = new Label();
			label15 = new Label();
			chkAsteroidClass = new CheckBox();
			cmbAsteroidClasses = new ComboBox();
			label27 = new Label();
			pnlVisibilityFilter = new Panel();
			lblMagType = new Label();
			updnAperture = new NumericUpDown();
			chkAperture = new CheckBox();
			updnStarMag = new NumericUpDown();
			chkStarMag = new CheckBox();
			updnSolarElong = new NumericUpDown();
			updnDiameter = new NumericUpDown();
			updnMaxDurn = new NumericUpDown();
			updnMagDrop = new NumericUpDown();
			chkSolarElongation = new CheckBox();
			chkMagDrop = new CheckBox();
			chkDuration = new CheckBox();
			chkDiameter = new CheckBox();
			optIntegrate = new RadioButton();
			label25 = new Label();
			pnlSiteFilter = new Panel();
			pnlSiteOptions = new Panel();
			cmbProbabilityValues = new ComboBox();
			chkProbability = new CheckBox();
			chkHorizon = new CheckBox();
			updnDistKM = new NumericUpDown();
			updnDistArcSec = new NumericUpDown();
			updnLocalAltitude = new NumericUpDown();
			chkDistArcSec = new CheckBox();
			chkSiteDistKM = new CheckBox();
			chkLocalAltitude = new CheckBox();
			label8 = new Label();
			txtLatitude = new TextBox();
			txtLongitude = new TextBox();
			chkSite = new CheckBox();
			lstSummary = new ListBox();
			lblFormHeader = new Label();
			toolTip1 = new ToolTip(components);
			PBarSummary = new ProgressBar();
			cmdAutoCancel = new Button();
			PanelCancel = new Panel();
			cmdCancelHorizons = new Button();
			lblHeader1 = new Label();
			lblHeader2 = new Label();
			lblType = new Label();
			label26 = new Label();
			label31 = new Label();
			((ISupportInitialize)updnEndYear).BeginInit();
			((ISupportInitialize)updnStartYear).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((Control)pnlFilters).SuspendLayout();
			((Control)pnlRegion).SuspendLayout();
			((ISupportInitialize)updnLatSE).BeginInit();
			((ISupportInitialize)updnLongSE).BeginInit();
			((ISupportInitialize)updnLatSW).BeginInit();
			((ISupportInitialize)updnLongSW).BeginInit();
			((ISupportInitialize)updnLatNE).BeginInit();
			((ISupportInitialize)updnLongNE).BeginInit();
			((ISupportInitialize)updnLatNW).BeginInit();
			((ISupportInitialize)updnLongNW).BeginInit();
			((Control)pnlOtherFilter).SuspendLayout();
			((ISupportInitialize)updnMinD).BeginInit();
			((ISupportInitialize)updnUncertainty).BeginInit();
			((Control)pnlDateFilter).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((ISupportInitialize)updnEndDay).BeginInit();
			((ISupportInitialize)updnStartDay).BeginInit();
			((ISupportInitialize)updnEndMonth).BeginInit();
			((ISupportInitialize)updnStartMonth).BeginInit();
			((Control)pnlIDfilter).SuspendLayout();
			((Control)pnlVisibilityFilter).SuspendLayout();
			((ISupportInitialize)updnAperture).BeginInit();
			((ISupportInitialize)updnStarMag).BeginInit();
			((ISupportInitialize)updnSolarElong).BeginInit();
			((ISupportInitialize)updnDiameter).BeginInit();
			((ISupportInitialize)updnMaxDurn).BeginInit();
			((ISupportInitialize)updnMagDrop).BeginInit();
			((Control)pnlSiteFilter).SuspendLayout();
			((Control)pnlSiteOptions).SuspendLayout();
			((ISupportInitialize)updnDistKM).BeginInit();
			((ISupportInitialize)updnDistArcSec).BeginInit();
			((ISupportInitialize)updnLocalAltitude).BeginInit();
			((Control)PanelCancel).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(144, 85));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(14, 17));
			((Control)label12).set_TabIndex(12);
			((Control)label12).set_Text("°");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label11).set_ForeColor(Color.DeepPink);
			((Control)label11).set_Location(new Point(0, 33));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(20, 15));
			((Control)label11).set_TabIndex(2);
			((Control)label11).set_Text("or");
			((Control)cmdReSetSites).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdReSetSites).set_Location(new Point(197, 2));
			((Control)cmdReSetSites).set_Name("cmdReSetSites");
			((Control)cmdReSetSites).set_Size(new Size(52, 36));
			((Control)cmdReSetSites).set_TabIndex(5);
			((Control)cmdReSetSites).set_Text("Select site");
			((ButtonBase)cmdReSetSites).set_UseVisualStyleBackColor(true);
			((Control)cmdReSetSites).add_Click((EventHandler)cmdReSetSites_Click);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(11, 41));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(25, 13));
			((Control)label6).set_TabIndex(4);
			((Control)label6).set_Text("and");
			((Control)updnEndYear).set_Font(new Font("Microsoft Sans Serif", 7.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnEndYear).set_Location(new Point(5, 56));
			updnEndYear.set_Maximum(new decimal(new int[4] { 2200, 0, 0, 0 }));
			updnEndYear.set_Minimum(new decimal(new int[4] { 1800, 0, 0, 0 }));
			((Control)updnEndYear).set_Name("updnEndYear");
			((Control)updnEndYear).set_Size(new Size(51, 19));
			((Control)updnEndYear).set_TabIndex(5);
			updnEndYear.set_Value(new decimal(new int[4] { 1800, 0, 0, 0 }));
			updnEndYear.add_ValueChanged((EventHandler)updnEndYear_ValueChanged);
			((Control)updnEndYear).add_Enter((EventHandler)updnEndYear_Enter);
			((Control)updnStartYear).set_Font(new Font("Microsoft Sans Serif", 7.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnStartYear).set_Location(new Point(5, 21));
			updnStartYear.set_Maximum(new decimal(new int[4] { 2200, 0, 0, 0 }));
			updnStartYear.set_Minimum(new decimal(new int[4] { 1800, 0, 0, 0 }));
			((Control)updnStartYear).set_Name("updnStartYear");
			((Control)updnStartYear).set_Size(new Size(51, 19));
			((Control)updnStartYear).set_TabIndex(1);
			updnStartYear.set_Value(new decimal(new int[4] { 1800, 0, 0, 0 }));
			updnStartYear.add_ValueChanged((EventHandler)updnStartYear_ValueChanged);
			((Control)updnStartYear).add_Enter((EventHandler)updnStartYear_Enter);
			((Control)cmbPlanet).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbPlanet).set_FormattingEnabled(true);
			cmbPlanet.get_Items().AddRange(new object[8] { "Mercury", "Venus", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune", "Pluto" });
			((Control)cmbPlanet).set_Location(new Point(113, 2));
			((Control)cmbPlanet).set_Name("cmbPlanet");
			((Control)cmbPlanet).set_Size(new Size(69, 21));
			((Control)cmbPlanet).set_TabIndex(1);
			cmbPlanet.add_SelectedIndexChanged((EventHandler)cmbPlanet_SelectedIndexChanged);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(123, 68));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(21, 13));
			((Control)label5).set_TabIndex(9);
			((Control)label5).set_Text("km");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(139, 47));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(24, 13));
			((Control)label4).set_TabIndex(6);
			((Control)label4).set_Text("sec");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(224, 42));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(13, 17));
			((Control)label3).set_TabIndex(8);
			((Control)label3).set_Text("\"");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(216, 24));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(21, 13));
			((Control)label2).set_TabIndex(5);
			((Control)label2).set_Text("km");
			toolTip1.SetToolTip((Control)(object)label2, "Distances >500km are approximate");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(145, 1));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(45, 13));
			((Control)label1).set_TabIndex(3);
			((Control)label1).set_Text("Latitude");
			((Control)txtAsteroidName).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAsteroidName).set_Location(new Point(20, 46));
			((Control)txtAsteroidName).set_Name("txtAsteroidName");
			((Control)txtAsteroidName).set_Size(new Size(76, 18));
			((Control)txtAsteroidName).set_TabIndex(3);
			((Control)txtAsteroidName).add_Enter((EventHandler)txtAsteroidName_Enter);
			((Control)txtAsteroidName).add_KeyDown(new KeyEventHandler(txtAsteroidName_KeyDown));
			((Control)txtAsteroidNumber).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAsteroidNumber).set_Location(new Point(20, 12));
			((Control)txtAsteroidNumber).set_Name("txtAsteroidNumber");
			((Control)txtAsteroidNumber).set_Size(new Size(51, 18));
			((Control)txtAsteroidNumber).set_TabIndex(1);
			((Control)txtAsteroidNumber).add_Enter((EventHandler)txtAsteroidNumber_Enter);
			((Control)txtAsteroidNumber).add_KeyDown(new KeyEventHandler(txtAsteroidNumber_KeyDown));
			((Control)chkAsteroidNumber).set_AutoSize(true);
			((Control)chkAsteroidNumber).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkAsteroidNumber).set_Location(new Point(3, 15));
			((Control)chkAsteroidNumber).set_Name("chkAsteroidNumber");
			((Control)chkAsteroidNumber).set_Size(new Size(15, 14));
			((Control)chkAsteroidNumber).set_TabIndex(0);
			((ButtonBase)chkAsteroidNumber).set_TextAlign(ContentAlignment.TopCenter);
			((ButtonBase)chkAsteroidNumber).set_UseVisualStyleBackColor(true);
			chkAsteroidNumber.add_CheckedChanged((EventHandler)chkAsteroidNumber_CheckedChanged);
			((Control)chkAsteroidName).set_AutoSize(true);
			((Control)chkAsteroidName).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkAsteroidName).set_Location(new Point(3, 48));
			((Control)chkAsteroidName).set_Name("chkAsteroidName");
			((Control)chkAsteroidName).set_Size(new Size(15, 14));
			((Control)chkAsteroidName).set_TabIndex(2);
			((ButtonBase)chkAsteroidName).set_UseVisualStyleBackColor(true);
			chkAsteroidName.add_CheckedChanged((EventHandler)chkAsteroidName_CheckedChanged);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)fileToolStripMenuItem,
				(ToolStripItem)sortToolStripMenuItem,
				(ToolStripItem)autoFileGenerationForAllListedToolStripMenuItem,
				(ToolStripItem)toolStripMenuItem1,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(1225, 24));
			((Control)menuStrip1).set_TabIndex(1);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripItem)fileToolStripMenuItem).set_AutoSize(false);
			((ToolStripDropDownItem)fileToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[28]
			{
				(ToolStripItem)newToolStripMenuItem,
				(ToolStripItem)readToolStripMenuItem,
				(ToolStripItem)openAndMergeToolStripMenuItem,
				(ToolStripItem)openAndMergeDuplicatesRetainedToolStripMenuItem,
				(ToolStripItem)toolStripSeparator10,
				(ToolStripItem)pasteOneOrMoreXMLEventPredictionToolStripMenuItem,
				(ToolStripItem)toolStripSeparator11,
				(ToolStripItem)rermoveSelectedEventToolStripMenuItem,
				(ToolStripItem)clearAllUndisplayedEventsFromMemoryToolStripMenuItem,
				(ToolStripItem)clearAllEventsFromMemoryToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)copyOccultationElementsOfSelectedEventToolStripMenuItem,
				(ToolStripItem)copyListToolStripMenuItem,
				(ToolStripItem)toolStripSeparator3,
				(ToolStripItem)xMLSaveListedEventsToolStripMenuItem,
				(ToolStripItem)xMLSaveAllEventsToolStripMenuItem,
				(ToolStripItem)saveListedEventsForOccultWatcherFeedToolStripMenuItem,
				(ToolStripItem)createUserAsteroidFileForAllListedAsteroidsToolStripMenuItem,
				(ToolStripItem)toolStripSeparator7,
				(ToolStripItem)editListOfRegionsToolStripMenuItem1,
				(ToolStripItem)setLocalHorizonAltitudesToolStripMenuItem,
				(ToolStripItem)toolStripSeparator4,
				(ToolStripItem)plotPathsOnAMapToolStripMenuItem,
				(ToolStripItem)plotAllPathsInGoogleEarthToolStripMenuItem,
				(ToolStripItem)savePathsAsGoogleEarthKMZToolStripMenuItem,
				(ToolStripItem)toolStripSeparator6,
				(ToolStripItem)saveListedEventsToolStripMenuItem,
				(ToolStripItem)saveAllToolStripMenuItem
			});
			((ToolStripItem)fileToolStripMenuItem).set_Name("fileToolStripMenuItem");
			((ToolStripItem)fileToolStripMenuItem).set_Size(new Size(122, 20));
			((ToolStripItem)fileToolStripMenuItem).set_Text("File...    ");
			((ToolStripItem)fileToolStripMenuItem).set_TextAlign(ContentAlignment.MiddleLeft);
			((ToolStripItem)newToolStripMenuItem).set_Image((Image)Resources.NewDocumentHS);
			((ToolStripItem)newToolStripMenuItem).set_Name("newToolStripMenuItem");
			((ToolStripItem)newToolStripMenuItem).set_Size(new Size(441, 22));
			((ToolStripItem)newToolStripMenuItem).set_Text("New");
			((ToolStripItem)newToolStripMenuItem).add_Click((EventHandler)newToolStripMenuItem_Click);
			((ToolStripItem)readToolStripMenuItem).set_Image((Image)Resources.openfolderHS);
			((ToolStripItem)readToolStripMenuItem).set_Name("readToolStripMenuItem");
			((ToolStripItem)readToolStripMenuItem).set_Size(new Size(441, 22));
			((ToolStripItem)readToolStripMenuItem).set_Text("Open");
			((ToolStripItem)readToolStripMenuItem).add_Click((EventHandler)readToolStripMenuItem_Click);
			((ToolStripItem)openAndMergeToolStripMenuItem).set_Image((Image)Resources.icon_branch);
			((ToolStripItem)openAndMergeToolStripMenuItem).set_Name("openAndMergeToolStripMenuItem");
			((ToolStripItem)openAndMergeToolStripMenuItem).set_Size(new Size(441, 22));
			((ToolStripItem)openAndMergeToolStripMenuItem).set_Text("Open and Merge - duplicates removed");
			((ToolStripItem)openAndMergeToolStripMenuItem).set_ToolTipText("Use this when you want\r\nduplicate entries from the\r\nmerged files to be  removed.");
			((ToolStripItem)openAndMergeToolStripMenuItem).add_Click((EventHandler)openAndMergeToolStripMenuItem_Click);
			((ToolStripItem)openAndMergeDuplicatesRetainedToolStripMenuItem).set_Image((Image)Resources.Merge_16x);
			((ToolStripItem)openAndMergeDuplicatesRetainedToolStripMenuItem).set_Name("openAndMergeDuplicatesRetainedToolStripMenuItem");
			((ToolStripItem)openAndMergeDuplicatesRetainedToolStripMenuItem).set_Size(new Size(441, 22));
			((ToolStripItem)openAndMergeDuplicatesRetainedToolStripMenuItem).set_Text("Open and Merge - duplicates retained");
			((ToolStripItem)openAndMergeDuplicatesRetainedToolStripMenuItem).set_ToolTipText("Use this when you want\r\nduplicate entries from the\r\nmerged files to be retained. \r\n\r\nUseful for comparing predictions\r\nfrom different sources or elements.");
			((ToolStripItem)openAndMergeDuplicatesRetainedToolStripMenuItem).add_Click((EventHandler)openAndMergeDuplicatesRetainedToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator10).set_Name("toolStripSeparator10");
			((ToolStripItem)toolStripSeparator10).set_Size(new Size(438, 6));
			((ToolStripItem)pasteOneOrMoreXMLEventPredictionToolStripMenuItem).set_Image((Image)Resources.PasteHS);
			((ToolStripItem)pasteOneOrMoreXMLEventPredictionToolStripMenuItem).set_Name("pasteOneOrMoreXMLEventPredictionToolStripMenuItem");
			((ToolStripItem)pasteOneOrMoreXMLEventPredictionToolStripMenuItem).set_Size(new Size(441, 22));
			((ToolStripItem)pasteOneOrMoreXMLEventPredictionToolStripMenuItem).set_Text("Paste one or more XML event predictions");
			((ToolStripItem)pasteOneOrMoreXMLEventPredictionToolStripMenuItem).add_Click((EventHandler)pasteOneOrMoreXMLEventPredictionToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator11).set_Name("toolStripSeparator11");
			((ToolStripItem)toolStripSeparator11).set_Size(new Size(438, 6));
			((ToolStripItem)rermoveSelectedEventToolStripMenuItem).set_Image((Image)Resources.DeleteTablefromDatabase_270_32);
			((ToolStripItem)rermoveSelectedEventToolStripMenuItem).set_Name("rermoveSelectedEventToolStripMenuItem");
			((ToolStripItem)rermoveSelectedEventToolStripMenuItem).set_Size(new Size(441, 22));
			((ToolStripItem)rermoveSelectedEventToolStripMenuItem).set_Text("Remove selected event");
			((ToolStripItem)rermoveSelectedEventToolStripMenuItem).add_Click((EventHandler)rermoveSelectedEventToolStripMenuItem_Click);
			((ToolStripItem)clearAllUndisplayedEventsFromMemoryToolStripMenuItem).set_Image((Image)Resources.ClearMessageQueue_16x);
			((ToolStripItem)clearAllUndisplayedEventsFromMemoryToolStripMenuItem).set_Name("clearAllUndisplayedEventsFromMemoryToolStripMenuItem");
			((ToolStripItem)clearAllUndisplayedEventsFromMemoryToolStripMenuItem).set_Size(new Size(441, 22));
			((ToolStripItem)clearAllUndisplayedEventsFromMemoryToolStripMenuItem).set_Text("Remove all unlisted events");
			((ToolStripItem)clearAllUndisplayedEventsFromMemoryToolStripMenuItem).add_Click((EventHandler)clearAllUndisplayedEventsFromMemoryToolStripMenuItem_Click);
			((ToolStripItem)clearAllEventsFromMemoryToolStripMenuItem).set_Image((Image)Resources.ClearWindowContent_16x_32);
			((ToolStripItem)clearAllEventsFromMemoryToolStripMenuItem).set_Name("clearAllEventsFromMemoryToolStripMenuItem");
			((ToolStripItem)clearAllEventsFromMemoryToolStripMenuItem).set_Size(new Size(441, 22));
			((ToolStripItem)clearAllEventsFromMemoryToolStripMenuItem).set_Text("Remove all events (listed + unlisted)");
			((ToolStripItem)clearAllEventsFromMemoryToolStripMenuItem).add_Click((EventHandler)clearAllEventsFromMemoryToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(438, 6));
			((ToolStripItem)copyOccultationElementsOfSelectedEventToolStripMenuItem).set_BackColor(Color.LightCyan);
			((ToolStripItem)copyOccultationElementsOfSelectedEventToolStripMenuItem).set_Image((Image)Resources.CopyItem_32x);
			((ToolStripItem)copyOccultationElementsOfSelectedEventToolStripMenuItem).set_Name("copyOccultationElementsOfSelectedEventToolStripMenuItem");
			((ToolStripItem)copyOccultationElementsOfSelectedEventToolStripMenuItem).set_Size(new Size(441, 22));
			((ToolStripItem)copyOccultationElementsOfSelectedEventToolStripMenuItem).set_Text("Copy occultation elements of selected event");
			((ToolStripItem)copyOccultationElementsOfSelectedEventToolStripMenuItem).add_Click((EventHandler)copyOccultationElementsOfSelectedEventToolStripMenuItem_Click);
			((ToolStripItem)copyListToolStripMenuItem).set_BackColor(Color.LightCyan);
			((ToolStripItem)copyListToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyListToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyListToolStripMenuItem).set_Name("copyListToolStripMenuItem");
			((ToolStripItem)copyListToolStripMenuItem).set_Size(new Size(441, 22));
			((ToolStripItem)copyListToolStripMenuItem).set_Text("Copy list");
			((ToolStripItem)copyListToolStripMenuItem).add_Click((EventHandler)copyListToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator3).set_Name("toolStripSeparator3");
			((ToolStripItem)toolStripSeparator3).set_Size(new Size(438, 6));
			((ToolStripItem)xMLSaveListedEventsToolStripMenuItem).set_Image((Image)Resources.SaveAs_16x_32);
			((ToolStripItem)xMLSaveListedEventsToolStripMenuItem).set_Name("xMLSaveListedEventsToolStripMenuItem");
			((ToolStripItem)xMLSaveListedEventsToolStripMenuItem).set_Size(new Size(441, 22));
			((ToolStripItem)xMLSaveListedEventsToolStripMenuItem).set_Text("Save listed events");
			((ToolStripItem)xMLSaveListedEventsToolStripMenuItem).add_Click((EventHandler)xMLSaveListedEventsToolStripMenuItem_Click);
			((ToolStripItem)xMLSaveAllEventsToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)xMLSaveAllEventsToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)xMLSaveAllEventsToolStripMenuItem).set_Name("xMLSaveAllEventsToolStripMenuItem");
			((ToolStripItem)xMLSaveAllEventsToolStripMenuItem).set_Size(new Size(441, 22));
			((ToolStripItem)xMLSaveAllEventsToolStripMenuItem).set_Text("Save all events");
			((ToolStripItem)xMLSaveAllEventsToolStripMenuItem).add_Click((EventHandler)xMLSaveAllEventsToolStripMenuItem_Click);
			((ToolStripItem)saveListedEventsForOccultWatcherFeedToolStripMenuItem).set_Image((Image)Resources.OccultWatcher);
			((ToolStripItem)saveListedEventsForOccultWatcherFeedToolStripMenuItem).set_ImageTransparentColor(Color.FromArgb(255, 135, 40));
			((ToolStripItem)saveListedEventsForOccultWatcherFeedToolStripMenuItem).set_Name("saveListedEventsForOccultWatcherFeedToolStripMenuItem");
			((ToolStripItem)saveListedEventsForOccultWatcherFeedToolStripMenuItem).set_Size(new Size(441, 22));
			((ToolStripItem)saveListedEventsForOccultWatcherFeedToolStripMenuItem).set_Text("Save listed events as a 'Personal' OW feed");
			((ToolStripItem)saveListedEventsForOccultWatcherFeedToolStripMenuItem).add_Click((EventHandler)saveListedEventsForOccultWatcherFeedToolStripMenuItem_Click);
			((ToolStripItem)createUserAsteroidFileForAllListedAsteroidsToolStripMenuItem).set_Image((Image)Resources.nasa2);
			((ToolStripItem)createUserAsteroidFileForAllListedAsteroidsToolStripMenuItem).set_Name("createUserAsteroidFileForAllListedAsteroidsToolStripMenuItem");
			((ToolStripItem)createUserAsteroidFileForAllListedAsteroidsToolStripMenuItem).set_Size(new Size(441, 22));
			((ToolStripItem)createUserAsteroidFileForAllListedAsteroidsToolStripMenuItem).set_Text("Create 'UserAsteroid.csv' file from JPL-Horizons, for all listed asteroids");
			((ToolStripItem)createUserAsteroidFileForAllListedAsteroidsToolStripMenuItem).add_Click((EventHandler)createUserAsteroidFileForAllListedAsteroidsToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator7).set_Name("toolStripSeparator7");
			((ToolStripItem)toolStripSeparator7).set_Size(new Size(438, 6));
			((ToolStripItem)editListOfRegionsToolStripMenuItem1).set_BackColor(Color.Lavender);
			((ToolStripItem)editListOfRegionsToolStripMenuItem1).set_Image((Image)Resources.EditorZone_6025_24);
			((ToolStripItem)editListOfRegionsToolStripMenuItem1).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)editListOfRegionsToolStripMenuItem1).set_Name("editListOfRegionsToolStripMenuItem1");
			((ToolStripItem)editListOfRegionsToolStripMenuItem1).set_Size(new Size(441, 22));
			((ToolStripItem)editListOfRegionsToolStripMenuItem1).set_Text("Edit list of regions");
			((ToolStripItem)editListOfRegionsToolStripMenuItem1).add_Click((EventHandler)editListOfRegionsToolStripMenuItem1_Click);
			((ToolStripItem)setLocalHorizonAltitudesToolStripMenuItem).set_Image((Image)Resources.sun_horizon);
			((ToolStripItem)setLocalHorizonAltitudesToolStripMenuItem).set_Name("setLocalHorizonAltitudesToolStripMenuItem");
			((ToolStripItem)setLocalHorizonAltitudesToolStripMenuItem).set_Size(new Size(441, 22));
			((ToolStripItem)setLocalHorizonAltitudesToolStripMenuItem).set_Text("Set the elevation of the local horizon");
			((ToolStripItem)setLocalHorizonAltitudesToolStripMenuItem).add_Click((EventHandler)setLocalHorizonAltitudesToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator4).set_Name("toolStripSeparator4");
			((ToolStripItem)toolStripSeparator4).set_Size(new Size(438, 6));
			((ToolStripItem)plotPathsOnAMapToolStripMenuItem).set_BackColor(Color.Honeydew);
			((ToolStripDropDownItem)plotPathsOnAMapToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)sunBelowHorizonToolStripMenuItem,
				(ToolStripItem)fullPathToolStripMenuItem
			});
			((ToolStripItem)plotPathsOnAMapToolStripMenuItem).set_Image((Image)Resources.Paths);
			((ToolStripItem)plotPathsOnAMapToolStripMenuItem).set_Name("plotPathsOnAMapToolStripMenuItem");
			((ToolStripItem)plotPathsOnAMapToolStripMenuItem).set_Size(new Size(441, 22));
			((ToolStripItem)plotPathsOnAMapToolStripMenuItem).set_Text("Plot paths on a map");
			((ToolStripItem)plotPathsOnAMapToolStripMenuItem).add_Click((EventHandler)plotPathsOnAMapToolStripMenuItem_Click);
			((ToolStripItem)sunBelowHorizonToolStripMenuItem).set_Name("sunBelowHorizonToolStripMenuItem");
			((ToolStripItem)sunBelowHorizonToolStripMenuItem).set_Size(new Size(342, 22));
			((ToolStripItem)sunBelowHorizonToolStripMenuItem).set_Text("Limited by altitudes of Star && Sun (set by star mag)");
			((ToolStripItem)sunBelowHorizonToolStripMenuItem).add_Click((EventHandler)sunBelowHorizonToolStripMenuItem_Click);
			((ToolStripItem)fullPathToolStripMenuItem).set_Name("fullPathToolStripMenuItem");
			((ToolStripItem)fullPathToolStripMenuItem).set_Size(new Size(342, 22));
			((ToolStripItem)fullPathToolStripMenuItem).set_Text("Full path");
			((ToolStripItem)fullPathToolStripMenuItem).add_Click((EventHandler)fullPathToolStripMenuItem_Click);
			((ToolStripItem)plotAllPathsInGoogleEarthToolStripMenuItem).set_BackColor(Color.Honeydew);
			((ToolStripDropDownItem)plotAllPathsInGoogleEarthToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)GEplotlimitedByAltitudesOfSunStarByStarMagToolStripMenuItem,
				(ToolStripItem)GEplotfullPathToolStripMenuItem
			});
			((ToolStripItem)plotAllPathsInGoogleEarthToolStripMenuItem).set_Image((Image)Resources.google_earth);
			((ToolStripItem)plotAllPathsInGoogleEarthToolStripMenuItem).set_Name("plotAllPathsInGoogleEarthToolStripMenuItem");
			((ToolStripItem)plotAllPathsInGoogleEarthToolStripMenuItem).set_Size(new Size(441, 22));
			((ToolStripItem)plotAllPathsInGoogleEarthToolStripMenuItem).set_Text("Plot all paths in GoogleEarth");
			((ToolStripItem)plotAllPathsInGoogleEarthToolStripMenuItem).add_Click((EventHandler)plotAllPathsInGoogleEarthToolStripMenuItem_Click);
			((ToolStripItem)GEplotlimitedByAltitudesOfSunStarByStarMagToolStripMenuItem).set_Name("GEplotlimitedByAltitudesOfSunStarByStarMagToolStripMenuItem");
			((ToolStripItem)GEplotlimitedByAltitudesOfSunStarByStarMagToolStripMenuItem).set_Size(new Size(342, 22));
			((ToolStripItem)GEplotlimitedByAltitudesOfSunStarByStarMagToolStripMenuItem).set_Text("Limited by altitudes of Star && Sun (set by star mag)");
			((ToolStripItem)GEplotlimitedByAltitudesOfSunStarByStarMagToolStripMenuItem).add_Click((EventHandler)GEplotlimitedByAltitudesOfSunStarByStarMagToolStripMenuItem_Click);
			((ToolStripItem)GEplotfullPathToolStripMenuItem).set_Name("GEplotfullPathToolStripMenuItem");
			((ToolStripItem)GEplotfullPathToolStripMenuItem).set_Size(new Size(342, 22));
			((ToolStripItem)GEplotfullPathToolStripMenuItem).set_Text("Full path");
			((ToolStripItem)GEplotfullPathToolStripMenuItem).add_Click((EventHandler)GEplotfullPathToolStripMenuItem_Click);
			((ToolStripItem)savePathsAsGoogleEarthKMZToolStripMenuItem).set_BackColor(Color.Honeydew);
			((ToolStripDropDownItem)savePathsAsGoogleEarthKMZToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)saveKMZlimitedByAltitudesOfStarSunsetByStarMagToolStripMenuItem,
				(ToolStripItem)saveKMZfullPathToolStripMenuItem1
			});
			((ToolStripItem)savePathsAsGoogleEarthKMZToolStripMenuItem).set_Image((Image)Resources.google_earth);
			((ToolStripItem)savePathsAsGoogleEarthKMZToolStripMenuItem).set_Name("savePathsAsGoogleEarthKMZToolStripMenuItem");
			((ToolStripItem)savePathsAsGoogleEarthKMZToolStripMenuItem).set_Size(new Size(441, 22));
			((ToolStripItem)savePathsAsGoogleEarthKMZToolStripMenuItem).set_Text("Save all paths as GoogleEarth KMZ");
			((ToolStripItem)savePathsAsGoogleEarthKMZToolStripMenuItem).add_Click((EventHandler)savePathsAsGoogleEarthKMZToolStripMenuItem_Click);
			((ToolStripItem)saveKMZlimitedByAltitudesOfStarSunsetByStarMagToolStripMenuItem).set_Name("saveKMZlimitedByAltitudesOfStarSunsetByStarMagToolStripMenuItem");
			((ToolStripItem)saveKMZlimitedByAltitudesOfStarSunsetByStarMagToolStripMenuItem).set_Size(new Size(342, 22));
			((ToolStripItem)saveKMZlimitedByAltitudesOfStarSunsetByStarMagToolStripMenuItem).set_Text("Limited by altitudes of Star && Sun (set by star mag)");
			((ToolStripItem)saveKMZlimitedByAltitudesOfStarSunsetByStarMagToolStripMenuItem).add_Click((EventHandler)saveKMZlimitedByAltitudesOfStarSunsetByStarMagToolStripMenuItem_Click);
			((ToolStripItem)saveKMZfullPathToolStripMenuItem1).set_Name("saveKMZfullPathToolStripMenuItem1");
			((ToolStripItem)saveKMZfullPathToolStripMenuItem1).set_Size(new Size(342, 22));
			((ToolStripItem)saveKMZfullPathToolStripMenuItem1).set_Text("Full path");
			((ToolStripItem)saveKMZfullPathToolStripMenuItem1).add_Click((EventHandler)saveKMZfullPathToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator6).set_Name("toolStripSeparator6");
			((ToolStripItem)toolStripSeparator6).set_Size(new Size(438, 6));
			((ToolStripItem)saveListedEventsToolStripMenuItem).set_Font(new Font("Microsoft Sans Serif", 7.5f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((ToolStripItem)saveListedEventsToolStripMenuItem).set_Image((Image)Resources.SaveAs_16x_32);
			((ToolStripItem)saveListedEventsToolStripMenuItem).set_ImageTransparentColor(Color.Black);
			((ToolStripItem)saveListedEventsToolStripMenuItem).set_Name("saveListedEventsToolStripMenuItem");
			((ToolStripItem)saveListedEventsToolStripMenuItem).set_Size(new Size(441, 22));
			((ToolStripItem)saveListedEventsToolStripMenuItem).set_Text("Save listed events (old format)");
			((ToolStripItem)saveListedEventsToolStripMenuItem).add_Click((EventHandler)saveListedEventsToolStripMenuItem_Click);
			((ToolStripItem)saveAllToolStripMenuItem).set_Font(new Font("Microsoft Sans Serif", 7.5f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((ToolStripItem)saveAllToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveAllToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveAllToolStripMenuItem).set_Name("saveAllToolStripMenuItem");
			((ToolStripItem)saveAllToolStripMenuItem).set_Size(new Size(441, 22));
			((ToolStripItem)saveAllToolStripMenuItem).set_Text("Save all events (old format)");
			((ToolStripItem)saveAllToolStripMenuItem).add_Click((EventHandler)saveAllToolStripMenuItem_Click);
			((ToolStripDropDownItem)sortToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[17]
			{
				(ToolStripItem)byDateToolStripMenuItem,
				(ToolStripItem)byDiameterkmToolStripMenuItem,
				(ToolStripItem)byDiameterapparentToolStripMenuItem,
				(ToolStripItem)byDurationToolStripMenuItem,
				(ToolStripItem)byRateOfMotionkmsec,
				(ToolStripItem)byStarMagnitudeToolStripMenuItem,
				(ToolStripItem)byCombinedMagnitudeToolStripMenuItem,
				(ToolStripItem)byMagnitudeDropViosualToolStripMenuItem,
				(ToolStripItem)byMagnitudeDropRedToolStripMenuItem,
				(ToolStripItem)byElongationToolStripMenuItem,
				(ToolStripItem)byDistanceToolStripMenuItem,
				(ToolStripItem)byProbabilityToolStripMenuItem,
				(ToolStripItem)byNameToolStripMenuItem,
				(ToolStripItem)byNumberToolStripMenuItem,
				(ToolStripItem)byStarNumberToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)listStarsOccultedMoreThanOnceToolStripMenuItem
			});
			((ToolStripItem)sortToolStripMenuItem).set_Image((Image)Resources.Sort);
			((ToolStripItem)sortToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)sortToolStripMenuItem).set_Name("sortToolStripMenuItem");
			((ToolStripItem)sortToolStripMenuItem).set_Size(new Size(111, 20));
			((ToolStripItem)sortToolStripMenuItem).set_Text("Sort events...   ");
			((ToolStripItem)byDateToolStripMenuItem).set_Name("byDateToolStripMenuItem");
			((ToolStripItem)byDateToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)byDateToolStripMenuItem).set_Text("by Date");
			((ToolStripItem)byDateToolStripMenuItem).add_Click((EventHandler)byDateToolStripMenuItem_Click);
			((ToolStripItem)byDiameterkmToolStripMenuItem).set_Name("byDiameterkmToolStripMenuItem");
			((ToolStripItem)byDiameterkmToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)byDiameterkmToolStripMenuItem).set_Text("by Diameter (km)");
			((ToolStripItem)byDiameterkmToolStripMenuItem).add_Click((EventHandler)byDiameterkmToolStripMenuItem_Click);
			((ToolStripItem)byDiameterapparentToolStripMenuItem).set_Name("byDiameterapparentToolStripMenuItem");
			((ToolStripItem)byDiameterapparentToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)byDiameterapparentToolStripMenuItem).set_Text("by Diameter (apparent)");
			((ToolStripItem)byDiameterapparentToolStripMenuItem).add_Click((EventHandler)byDiameterapparentToolStripMenuItem_Click);
			((ToolStripItem)byDurationToolStripMenuItem).set_Name("byDurationToolStripMenuItem");
			((ToolStripItem)byDurationToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)byDurationToolStripMenuItem).set_Text("by Duration");
			((ToolStripItem)byDurationToolStripMenuItem).add_Click((EventHandler)byDurationToolStripMenuItem_Click);
			((ToolStripItem)byRateOfMotionkmsec).set_Name("byRateOfMotionkmsec");
			((ToolStripItem)byRateOfMotionkmsec).set_Size(new Size(255, 22));
			((ToolStripItem)byRateOfMotionkmsec).set_Text("by Rate of motion (km/sec)");
			((ToolStripItem)byRateOfMotionkmsec).add_Click((EventHandler)byRateOfMotionkmsec_Click);
			((ToolStripItem)byStarMagnitudeToolStripMenuItem).set_Name("byStarMagnitudeToolStripMenuItem");
			((ToolStripItem)byStarMagnitudeToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)byStarMagnitudeToolStripMenuItem).set_Text("by STAR magnitude");
			((ToolStripItem)byStarMagnitudeToolStripMenuItem).add_Click((EventHandler)byStarMagnitudeToolStripMenuItem_Click);
			((ToolStripItem)byCombinedMagnitudeToolStripMenuItem).set_Name("byCombinedMagnitudeToolStripMenuItem");
			((ToolStripItem)byCombinedMagnitudeToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)byCombinedMagnitudeToolStripMenuItem).set_Text("by COMBINED magnitude");
			((ToolStripItem)byCombinedMagnitudeToolStripMenuItem).add_Click((EventHandler)byCombinedMagnitudeToolStripMenuItem_Click);
			((ToolStripItem)byMagnitudeDropViosualToolStripMenuItem).set_Name("byMagnitudeDropViosualToolStripMenuItem");
			((ToolStripItem)byMagnitudeDropViosualToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)byMagnitudeDropViosualToolStripMenuItem).set_Text("by Magnitude drop (Visual)");
			((ToolStripItem)byMagnitudeDropViosualToolStripMenuItem).add_Click((EventHandler)byMagnitudeDropViosualToolStripMenuItem_Click);
			((ToolStripItem)byMagnitudeDropRedToolStripMenuItem).set_Name("byMagnitudeDropRedToolStripMenuItem");
			((ToolStripItem)byMagnitudeDropRedToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)byMagnitudeDropRedToolStripMenuItem).set_Text("by Magnitude drop (Red)");
			((ToolStripItem)byMagnitudeDropRedToolStripMenuItem).add_Click((EventHandler)byMagnitudeDropRedToolStripMenuItem_Click);
			((ToolStripItem)byElongationToolStripMenuItem).set_Name("byElongationToolStripMenuItem");
			((ToolStripItem)byElongationToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)byElongationToolStripMenuItem).set_Text("by Elongation");
			((ToolStripItem)byElongationToolStripMenuItem).add_Click((EventHandler)byElongationToolStripMenuItem_Click);
			((ToolStripItem)byDistanceToolStripMenuItem).set_Name("byDistanceToolStripMenuItem");
			((ToolStripItem)byDistanceToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)byDistanceToolStripMenuItem).set_Text("by Distance");
			((ToolStripItem)byDistanceToolStripMenuItem).add_Click((EventHandler)byDistanceToolStripMenuItem_Click);
			((ToolStripItem)byProbabilityToolStripMenuItem).set_Name("byProbabilityToolStripMenuItem");
			((ToolStripItem)byProbabilityToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)byProbabilityToolStripMenuItem).set_Text("by Probability");
			((ToolStripItem)byProbabilityToolStripMenuItem).add_Click((EventHandler)byProbabilityToolStripMenuItem_Click);
			((ToolStripItem)byNameToolStripMenuItem).set_Name("byNameToolStripMenuItem");
			((ToolStripItem)byNameToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)byNameToolStripMenuItem).set_Text("by Name");
			((ToolStripItem)byNameToolStripMenuItem).add_Click((EventHandler)byNameToolStripMenuItem_Click);
			((ToolStripItem)byNumberToolStripMenuItem).set_Name("byNumberToolStripMenuItem");
			((ToolStripItem)byNumberToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)byNumberToolStripMenuItem).set_Text("by Number");
			((ToolStripItem)byNumberToolStripMenuItem).add_Click((EventHandler)byNumberToolStripMenuItem_Click);
			((ToolStripItem)byStarNumberToolStripMenuItem).set_Name("byStarNumberToolStripMenuItem");
			((ToolStripItem)byStarNumberToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)byStarNumberToolStripMenuItem).set_Text("by Star number");
			((ToolStripItem)byStarNumberToolStripMenuItem).add_Click((EventHandler)byStarNumberToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(252, 6));
			((ToolStripItem)listStarsOccultedMoreThanOnceToolStripMenuItem).set_Name("listStarsOccultedMoreThanOnceToolStripMenuItem");
			((ToolStripItem)listStarsOccultedMoreThanOnceToolStripMenuItem).set_Size(new Size(255, 22));
			((ToolStripItem)listStarsOccultedMoreThanOnceToolStripMenuItem).set_Text("List stars occulted more than once");
			((ToolStripItem)listStarsOccultedMoreThanOnceToolStripMenuItem).add_Click((EventHandler)listStarsOccultedMoreThanOnceToolStripMenuItem_Click);
			((ToolStripDropDownItem)autoFileGenerationForAllListedToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[8]
			{
				(ToolStripItem)setScaleForWorldMapsToolStripMenuItem,
				(ToolStripItem)mapsSitePathCenteredSiteToolStripMenuItem,
				(ToolStripItem)toolStripSeparator5,
				(ToolStripItem)generationToolStripMenuItem,
				(ToolStripItem)toolStripSeparator8,
				(ToolStripItem)filesForOccultWatcherFeedToolStripMenuItem,
				(ToolStripItem)toolStripSeparator9,
				(ToolStripItem)saveListedEventsAsAPersonalOWFeedToolStripMenuItem
			});
			((ToolStripItem)autoFileGenerationForAllListedToolStripMenuItem).set_Name("autoFileGenerationForAllListedToolStripMenuItem");
			((ToolStripItem)autoFileGenerationForAllListedToolStripMenuItem).set_Size(new Size(229, 20));
			((ToolStripItem)autoFileGenerationForAllListedToolStripMenuItem).set_Text("Generate predictions for all listed events");
			((ToolStripItem)setScaleForWorldMapsToolStripMenuItem).set_Name("setScaleForWorldMapsToolStripMenuItem");
			((ToolStripItem)setScaleForWorldMapsToolStripMenuItem).set_Size(new Size(292, 22));
			((ToolStripItem)setScaleForWorldMapsToolStripMenuItem).set_Text("Set scale for world maps  ( = 1.0)");
			mapsSitePathCenteredSiteToolStripMenuItem.set_Checked(Settings.Default.PlotPathCentered);
			((ToolStripItem)mapsSitePathCenteredSiteToolStripMenuItem).set_Name("mapsSitePathCenteredSiteToolStripMenuItem");
			((ToolStripItem)mapsSitePathCenteredSiteToolStripMenuItem).set_Size(new Size(292, 22));
			((ToolStripItem)mapsSitePathCenteredSiteToolStripMenuItem).set_Text("Plot the maps Path/Site-centered [Site]");
			((ToolStripItem)mapsSitePathCenteredSiteToolStripMenuItem).add_Click((EventHandler)mapsSitePathCenteredSiteToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator5).set_Name("toolStripSeparator5");
			((ToolStripItem)toolStripSeparator5).set_Size(new Size(289, 6));
			((ToolStripItem)generationToolStripMenuItem).set_Name("generationToolStripMenuItem");
			((ToolStripItem)generationToolStripMenuItem).set_Size(new Size(292, 22));
			((ToolStripItem)generationToolStripMenuItem).set_Text("Create files   [StationSort using site file ....");
			((ToolStripItem)toolStripSeparator8).set_Name("toolStripSeparator8");
			((ToolStripItem)toolStripSeparator8).set_Size(new Size(289, 6));
			((ToolStripItem)filesForOccultWatcherFeedToolStripMenuItem).set_Name("filesForOccultWatcherFeedToolStripMenuItem");
			((ToolStripItem)filesForOccultWatcherFeedToolStripMenuItem).set_Size(new Size(292, 22));
			((ToolStripItem)filesForOccultWatcherFeedToolStripMenuItem).set_Text("Create files for Occult Watcher feed");
			((ToolStripItem)filesForOccultWatcherFeedToolStripMenuItem).add_Click((EventHandler)filesForOccultWatcherFeedToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator9).set_Name("toolStripSeparator9");
			((ToolStripItem)toolStripSeparator9).set_Size(new Size(289, 6));
			((ToolStripItem)saveListedEventsAsAPersonalOWFeedToolStripMenuItem).set_Image((Image)Resources.OccultWatcher);
			((ToolStripItem)saveListedEventsAsAPersonalOWFeedToolStripMenuItem).set_ImageTransparentColor(Color.FromArgb(255, 135, 40));
			((ToolStripItem)saveListedEventsAsAPersonalOWFeedToolStripMenuItem).set_Name("saveListedEventsAsAPersonalOWFeedToolStripMenuItem");
			((ToolStripItem)saveListedEventsAsAPersonalOWFeedToolStripMenuItem).set_Size(new Size(292, 22));
			((ToolStripItem)saveListedEventsAsAPersonalOWFeedToolStripMenuItem).set_Text("Save listed events as a 'Personal' OW feed");
			((ToolStripItem)saveListedEventsAsAPersonalOWFeedToolStripMenuItem).add_Click((EventHandler)saveListedEventsAsAPersonalOWFeedToolStripMenuItem_Click);
			((ToolStripItem)toolStripMenuItem1).set_AutoSize(false);
			((ToolStripItem)toolStripMenuItem1).set_Name("toolStripMenuItem1");
			((ToolStripItem)toolStripMenuItem1).set_Size(new Size(340, 20));
			((ToolStripItem)toolStripMenuItem1).set_Text("                                    .                                              .");
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(66, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit    ");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(149, 63));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(14, 17));
			((Control)label7).set_TabIndex(11);
			((Control)label7).set_Text("°");
			((Control)pnlFilters).set_BackColor(Color.Ivory);
			pnlFilters.set_BorderStyle((BorderStyle)2);
			((Control)pnlFilters).get_Controls().Add((Control)(object)lblEventCount);
			((Control)pnlFilters).get_Controls().Add((Control)(object)pnlRegion);
			((Control)pnlFilters).get_Controls().Add((Control)(object)cmdSaveListed);
			((Control)pnlFilters).get_Controls().Add((Control)(object)cmdListEvents);
			((Control)pnlFilters).get_Controls().Add((Control)(object)pnlOtherFilter);
			((Control)pnlFilters).get_Controls().Add((Control)(object)pnlDateFilter);
			((Control)pnlFilters).get_Controls().Add((Control)(object)pnlIDfilter);
			((Control)pnlFilters).get_Controls().Add((Control)(object)pnlVisibilityFilter);
			((Control)pnlFilters).get_Controls().Add((Control)(object)pnlSiteFilter);
			((Control)pnlFilters).set_Location(new Point(6, 32));
			((Control)pnlFilters).set_Name("pnlFilters");
			((Control)pnlFilters).set_Size(new Size(1215, 140));
			((Control)pnlFilters).set_TabIndex(0);
			((Control)lblEventCount).set_AutoSize(true);
			((Control)lblEventCount).set_Font(new Font("Microsoft Sans Serif", 7.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblEventCount).set_Location(new Point(3, 51));
			((Control)lblEventCount).set_Name("lblEventCount");
			((Control)lblEventCount).set_Size(new Size(48, 13));
			((Control)lblEventCount).set_TabIndex(7);
			((Control)lblEventCount).set_Text("0 events");
			((Control)pnlRegion).set_BackColor(Color.Lavender);
			pnlRegion.set_BorderStyle((BorderStyle)1);
			((Control)pnlRegion).get_Controls().Add((Control)(object)cmbRegion);
			((Control)pnlRegion).get_Controls().Add((Control)(object)label29);
			((Control)pnlRegion).get_Controls().Add((Control)(object)label28);
			((Control)pnlRegion).get_Controls().Add((Control)(object)label24);
			((Control)pnlRegion).get_Controls().Add((Control)(object)label23);
			((Control)pnlRegion).get_Controls().Add((Control)(object)label21);
			((Control)pnlRegion).get_Controls().Add((Control)(object)label22);
			((Control)pnlRegion).get_Controls().Add((Control)(object)label20);
			((Control)pnlRegion).get_Controls().Add((Control)(object)label19);
			((Control)pnlRegion).get_Controls().Add((Control)(object)chkInRegion);
			((Control)pnlRegion).get_Controls().Add((Control)(object)updnLatSE);
			((Control)pnlRegion).get_Controls().Add((Control)(object)updnLongSE);
			((Control)pnlRegion).get_Controls().Add((Control)(object)updnLatSW);
			((Control)pnlRegion).get_Controls().Add((Control)(object)updnLongSW);
			((Control)pnlRegion).get_Controls().Add((Control)(object)updnLatNE);
			((Control)pnlRegion).get_Controls().Add((Control)(object)updnLongNE);
			((Control)pnlRegion).get_Controls().Add((Control)(object)updnLatNW);
			((Control)pnlRegion).get_Controls().Add((Control)(object)updnLongNW);
			((Control)pnlRegion).set_Location(new Point(505, 2));
			((Control)pnlRegion).set_Name("pnlRegion");
			((Control)pnlRegion).set_Size(new Size(168, 132));
			((Control)pnlRegion).set_TabIndex(2);
			((ListControl)cmbRegion).set_FormattingEnabled(true);
			((Control)cmbRegion).set_Location(new Point(60, 6));
			((Control)cmbRegion).set_Name("cmbRegion");
			((Control)cmbRegion).set_Size(new Size(104, 21));
			((Control)cmbRegion).set_TabIndex(10);
			toolTip1.SetToolTip((Control)(object)cmbRegion, "Edit regions using \r\nFile... Edit list of Regions");
			cmbRegion.add_SelectedIndexChanged((EventHandler)cmbRegion_SelectedIndexChanged);
			((Control)label29).set_AutoSize(true);
			((Control)label29).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label29).set_ForeColor(Color.MediumBlue);
			((Control)label29).set_Location(new Point(94, 90));
			((Control)label29).set_Name("label29");
			((Control)label29).set_Size(new Size(56, 13));
			((Control)label29).set_TabIndex(17);
			((Control)label29).set_Text("SouthEast");
			((Control)label28).set_AutoSize(true);
			((Control)label28).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label28).set_ForeColor(Color.MediumBlue);
			((Control)label28).set_Location(new Point(8, 90));
			((Control)label28).set_Name("label28");
			((Control)label28).set_Size(new Size(58, 13));
			((Control)label28).set_TabIndex(16);
			((Control)label28).set_Text("SouthWest");
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label24).set_ForeColor(Color.MediumBlue);
			((Control)label24).set_Location(new Point(96, 53));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(53, 13));
			((Control)label24).set_TabIndex(3);
			((Control)label24).set_Text("NorthEast");
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label23).set_ForeColor(Color.MediumBlue);
			((Control)label23).set_Location(new Point(10, 53));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(55, 13));
			((Control)label23).set_TabIndex(2);
			((Control)label23).set_Text("NorthWest");
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label21).set_Location(new Point(84, 36));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(34, 13));
			((Control)label21).set_TabIndex(6);
			((Control)label21).set_Text("Long.");
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label22).set_Location(new Point(128, 36));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(25, 13));
			((Control)label22).set_TabIndex(7);
			((Control)label22).set_Text("Lat.");
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label20).set_Location(new Point(-1, 36));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(34, 13));
			((Control)label20).set_TabIndex(4);
			((Control)label20).set_Text("Long.");
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label19).set_Location(new Point(43, 36));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(25, 13));
			((Control)label19).set_TabIndex(5);
			((Control)label19).set_Text("Lat.");
			((Control)chkInRegion).set_AutoSize(true);
			chkInRegion.set_Checked(Settings.Default.PathCrossingRegion);
			((Control)chkInRegion).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "PathCrossingRegion", true, (DataSourceUpdateMode)1));
			((Control)chkInRegion).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkInRegion).set_Location(new Point(3, 2));
			((Control)chkInRegion).set_Name("chkInRegion");
			((Control)chkInRegion).set_Size(new Size(61, 30));
			((Control)chkInRegion).set_TabIndex(0);
			((Control)chkInRegion).set_Text("Path \r\ncrosses");
			((ButtonBase)chkInRegion).set_TextAlign(ContentAlignment.MiddleCenter);
			((ButtonBase)chkInRegion).set_UseVisualStyleBackColor(true);
			chkInRegion.add_CheckedChanged((EventHandler)chkInRegion_CheckedChanged);
			((Control)updnLatSE).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "SELat", true, (DataSourceUpdateMode)1));
			((Control)updnLatSE).set_Location(new Point(129, 105));
			updnLatSE.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			updnLatSE.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)updnLatSE).set_Name("updnLatSE");
			((Control)updnLatSE).set_Size(new Size(35, 20));
			((Control)updnLatSE).set_TabIndex(15);
			updnLatSE.set_Value(Settings.Default.SELat);
			updnLatSE.add_ValueChanged((EventHandler)updnLatSE_ValueChanged);
			((Control)updnLatSE).add_Enter((EventHandler)updnLatSE_Enter);
			((Control)updnLongSE).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "SELong", true, (DataSourceUpdateMode)1));
			((Control)updnLongSE).set_Location(new Point(86, 105));
			updnLongSE.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			updnLongSE.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)updnLongSE).set_Name("updnLongSE");
			((Control)updnLongSE).set_Size(new Size(42, 20));
			((Control)updnLongSE).set_TabIndex(14);
			updnLongSE.set_Value(Settings.Default.SELong);
			updnLongSE.add_ValueChanged((EventHandler)updnLongSE_ValueChanged);
			((Control)updnLongSE).add_Enter((EventHandler)updnLongSE_Enter);
			((Control)updnLatSW).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "SWLat", true, (DataSourceUpdateMode)1));
			((Control)updnLatSW).set_Location(new Point(44, 105));
			updnLatSW.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			updnLatSW.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)updnLatSW).set_Name("updnLatSW");
			((Control)updnLatSW).set_Size(new Size(35, 20));
			((Control)updnLatSW).set_TabIndex(13);
			updnLatSW.set_Value(Settings.Default.SWLat);
			updnLatSW.add_ValueChanged((EventHandler)updnLatSW_ValueChanged);
			((Control)updnLatSW).add_Enter((EventHandler)updnLatSW_Enter);
			((Control)updnLongSW).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "SWLong", true, (DataSourceUpdateMode)1));
			((Control)updnLongSW).set_Location(new Point(1, 105));
			updnLongSW.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			updnLongSW.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)updnLongSW).set_Name("updnLongSW");
			((Control)updnLongSW).set_Size(new Size(42, 20));
			((Control)updnLongSW).set_TabIndex(12);
			updnLongSW.set_Value(Settings.Default.SWLong);
			updnLongSW.add_ValueChanged((EventHandler)updnLongSW_ValueChanged);
			((Control)updnLongSW).add_Enter((EventHandler)updnLongSW_Enter);
			((Control)updnLatNE).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "NELat", true, (DataSourceUpdateMode)1));
			((Control)updnLatNE).set_Location(new Point(129, 68));
			updnLatNE.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			updnLatNE.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)updnLatNE).set_Name("updnLatNE");
			((Control)updnLatNE).set_Size(new Size(35, 20));
			((Control)updnLatNE).set_TabIndex(11);
			updnLatNE.set_Value(Settings.Default.NELat);
			updnLatNE.add_ValueChanged((EventHandler)updnLatNE_ValueChanged);
			((Control)updnLatNE).add_Enter((EventHandler)updnLatNE_Enter);
			((Control)updnLongNE).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "NELong", true, (DataSourceUpdateMode)1));
			((Control)updnLongNE).set_Location(new Point(86, 68));
			updnLongNE.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			updnLongNE.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)updnLongNE).set_Name("updnLongNE");
			((Control)updnLongNE).set_Size(new Size(42, 20));
			((Control)updnLongNE).set_TabIndex(10);
			updnLongNE.set_Value(Settings.Default.NELong);
			updnLongNE.add_ValueChanged((EventHandler)updnLongNE_ValueChanged);
			((Control)updnLongNE).add_Enter((EventHandler)updnLongNE_Enter);
			((Control)updnLatNW).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "NWLat", true, (DataSourceUpdateMode)1));
			((Control)updnLatNW).set_Location(new Point(44, 68));
			updnLatNW.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			updnLatNW.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)updnLatNW).set_Name("updnLatNW");
			((Control)updnLatNW).set_Size(new Size(35, 20));
			((Control)updnLatNW).set_TabIndex(9);
			updnLatNW.set_Value(Settings.Default.NWLat);
			updnLatNW.add_ValueChanged((EventHandler)updnLatNW_ValueChanged);
			((Control)updnLatNW).add_Enter((EventHandler)updnLatNW_Enter);
			((Control)updnLongNW).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "NWLong", true, (DataSourceUpdateMode)1));
			((Control)updnLongNW).set_Location(new Point(1, 68));
			updnLongNW.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			updnLongNW.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)updnLongNW).set_Name("updnLongNW");
			((Control)updnLongNW).set_Size(new Size(42, 20));
			((Control)updnLongNW).set_TabIndex(8);
			updnLongNW.set_Value(Settings.Default.NWLong);
			updnLongNW.add_ValueChanged((EventHandler)updnLongNW_ValueChanged);
			((Control)updnLongNW).add_Enter((EventHandler)updnLongNW_Enter);
			((Control)cmdSaveListed).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSaveListed).set_Location(new Point(3, 83));
			((Control)cmdSaveListed).set_Name("cmdSaveListed");
			((Control)cmdSaveListed).set_Size(new Size(68, 35));
			((Control)cmdSaveListed).set_TabIndex(8);
			((Control)cmdSaveListed).set_Text("Save listed events");
			((ButtonBase)cmdSaveListed).set_UseVisualStyleBackColor(true);
			((Control)cmdSaveListed).add_Click((EventHandler)cmdSaveListed_Click);
			((Control)cmdListEvents).set_Location(new Point(3, 12));
			((Control)cmdListEvents).set_Name("cmdListEvents");
			((Control)cmdListEvents).set_Size(new Size(68, 35));
			((Control)cmdListEvents).set_TabIndex(6);
			((Control)cmdListEvents).set_Text("List events");
			((ButtonBase)cmdListEvents).set_UseVisualStyleBackColor(true);
			((Control)cmdListEvents).add_Click((EventHandler)cmdListEvents_Click);
			((Control)pnlOtherFilter).set_BackColor(Color.LightCyan);
			pnlOtherFilter.set_BorderStyle((BorderStyle)1);
			((Control)pnlOtherFilter).get_Controls().Add((Control)(object)updnMinD);
			((Control)pnlOtherFilter).get_Controls().Add((Control)(object)label30);
			((Control)pnlOtherFilter).get_Controls().Add((Control)(object)updnUncertainty);
			((Control)pnlOtherFilter).get_Controls().Add((Control)(object)label18);
			((Control)pnlOtherFilter).get_Controls().Add((Control)(object)chkUncertainty);
			((Control)pnlOtherFilter).get_Controls().Add((Control)(object)chkMinimumD);
			((Control)pnlOtherFilter).get_Controls().Add((Control)(object)label13);
			((Control)pnlOtherFilter).get_Controls().Add((Control)(object)label10);
			((Control)pnlOtherFilter).get_Controls().Add((Control)(object)label9);
			((Control)pnlOtherFilter).get_Controls().Add((Control)(object)chkRings);
			((Control)pnlOtherFilter).get_Controls().Add((Control)(object)chkPlanet);
			((Control)pnlOtherFilter).get_Controls().Add((Control)(object)cmbPlanet);
			((Control)pnlOtherFilter).get_Controls().Add((Control)(object)chkShapeModel);
			((Control)pnlOtherFilter).get_Controls().Add((Control)(object)chkBinary);
			((Control)pnlOtherFilter).set_Location(new Point(674, 2));
			((Control)pnlOtherFilter).set_Name("pnlOtherFilter");
			((Control)pnlOtherFilter).set_Size(new Size(203, 132));
			((Control)pnlOtherFilter).set_TabIndex(3);
			((Control)updnMinD).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "AsteroidSearchMinimumDuration", true, (DataSourceUpdateMode)1));
			updnMinD.set_DecimalPlaces(1);
			updnMinD.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnMinD).set_Location(new Point(90, 107));
			updnMinD.set_Maximum(new decimal(new int[4] { 10, 0, 0, 0 }));
			updnMinD.set_Minimum(new decimal(new int[4] { 10, 0, 0, 65536 }));
			((Control)updnMinD).set_Name("updnMinD");
			((Control)updnMinD).set_Size(new Size(41, 20));
			((Control)updnMinD).set_TabIndex(12);
			updnMinD.set_Value(Settings.Default.AsteroidSearchMinimumDuration);
			updnMinD.add_ValueChanged((EventHandler)updnMinD_ValueChanged);
			((Control)label30).set_AutoSize(true);
			((Control)label30).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label30).set_Location(new Point(134, 111));
			((Control)label30).set_Name("label30");
			((Control)label30).set_Size(new Size(54, 13));
			((Control)label30).set_TabIndex(13);
			((Control)label30).set_Text("Earth radii");
			((Control)updnUncertainty).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "AsteroidPathUncertEarthRadii", true, (DataSourceUpdateMode)1));
			updnUncertainty.set_DecimalPlaces(1);
			updnUncertainty.set_Increment(new decimal(new int[4] { 2, 0, 0, 65536 }));
			((Control)updnUncertainty).set_Location(new Point(90, 85));
			updnUncertainty.set_Maximum(new decimal(new int[4] { 4, 0, 0, 0 }));
			updnUncertainty.set_Minimum(new decimal(new int[4] { 2, 0, 0, 65536 }));
			((Control)updnUncertainty).set_Name("updnUncertainty");
			((Control)updnUncertainty).set_Size(new Size(41, 20));
			((Control)updnUncertainty).set_TabIndex(9);
			updnUncertainty.set_Value(Settings.Default.AsteroidPathUncertEarthRadii);
			updnUncertainty.add_ValueChanged((EventHandler)updnUncertainty_ValueChanged);
			((Control)updnUncertainty).add_Enter((EventHandler)updnUncertainty_Enter);
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label18).set_Location(new Point(134, 89));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(54, 13));
			((Control)label18).set_TabIndex(10);
			((Control)label18).set_Text("Earth radii");
			((Control)chkUncertainty).set_AutoSize(true);
			chkUncertainty.set_Checked(Settings.Default.PathUncertaintyChecked);
			chkUncertainty.set_CheckState((CheckState)1);
			((Control)chkUncertainty).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "PathUncertaintyChecked", true, (DataSourceUpdateMode)1));
			((Control)chkUncertainty).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkUncertainty).set_Location(new Point(5, 87));
			((Control)chkUncertainty).set_Name("chkUncertainty");
			((Control)chkUncertainty).set_Size(new Size(90, 17));
			((Control)chkUncertainty).set_TabIndex(8);
			((Control)chkUncertainty).set_Text("Uncertainty < ");
			((ButtonBase)chkUncertainty).set_UseVisualStyleBackColor(true);
			chkUncertainty.add_CheckedChanged((EventHandler)chkUncertainty_CheckedChanged);
			((Control)chkMinimumD).set_AutoSize(true);
			chkMinimumD.set_Checked(Settings.Default.ExcludeIfInFutureChecked);
			((Control)chkMinimumD).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "ExcludeIfInFutureChecked", true, (DataSourceUpdateMode)1));
			((Control)chkMinimumD).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkMinimumD).set_Location(new Point(5, 109));
			((Control)chkMinimumD).set_Name("chkMinimumD");
			((Control)chkMinimumD).set_Size(new Size(87, 17));
			((Control)chkMinimumD).set_TabIndex(11);
			((Control)chkMinimumD).set_Text("Minimum D <");
			toolTip1.SetToolTip((Control)(object)chkMinimumD, "Minimum distance from the \r\nGeocenter, in Earth radii");
			((ButtonBase)chkMinimumD).set_UseVisualStyleBackColor(true);
			chkMinimumD.add_CheckedChanged((EventHandler)chkFuture_CheckedChanged);
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label13).set_Location(new Point(129, 49));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(15, 17));
			((Control)label13).set_TabIndex(5);
			((Control)label13).set_Text("☾");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label10).set_Location(new Point(121, 68));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(18, 17));
			((Control)label10).set_TabIndex(7);
			((Control)label10).set_Text("◉");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(168, 30));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(13, 13));
			((Control)label9).set_TabIndex(3);
			((Control)label9).set_Text("§");
			((Control)chkRings).set_AutoSize(true);
			chkRings.set_Checked(Settings.Default.HasRingsChecked);
			((Control)chkRings).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "HasRingsChecked", true, (DataSourceUpdateMode)1));
			((Control)chkRings).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkRings).set_Location(new Point(5, 68));
			((Control)chkRings).set_Name("chkRings");
			((Control)chkRings).set_Size(new Size(122, 17));
			((Control)chkRings).set_TabIndex(6);
			((Control)chkRings).set_Text("Asteroids with rings -");
			((ButtonBase)chkRings).set_UseVisualStyleBackColor(true);
			chkRings.add_CheckedChanged((EventHandler)chkRings_CheckedChanged);
			((Control)chkPlanet).set_AutoSize(true);
			chkPlanet.set_Checked(Settings.Default.AsteroidPlanetChecked);
			((Control)chkPlanet).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidPlanetChecked", true, (DataSourceUpdateMode)1));
			((Control)chkPlanet).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkPlanet).set_Location(new Point(5, -2));
			((Control)chkPlanet).set_Name("chkPlanet");
			((Control)chkPlanet).set_Size(new Size(96, 30));
			((Control)chkPlanet).set_TabIndex(0);
			((Control)chkPlanet).set_Text("Major planets \r\n[&& their moons]");
			((ButtonBase)chkPlanet).set_UseVisualStyleBackColor(true);
			chkPlanet.add_CheckedChanged((EventHandler)chkPlanet_CheckedChanged);
			((Control)chkShapeModel).set_AutoSize(true);
			chkShapeModel.set_Checked(Settings.Default.HasShapeModelChecked);
			((Control)chkShapeModel).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "HasShapeModelChecked", true, (DataSourceUpdateMode)1));
			((Control)chkShapeModel).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkShapeModel).set_Location(new Point(5, 30));
			((Control)chkShapeModel).set_Name("chkShapeModel");
			((Control)chkShapeModel).set_Size(new Size(169, 17));
			((Control)chkShapeModel).set_TabIndex(2);
			((Control)chkShapeModel).set_Text("Asteroids with a shape model -");
			((ButtonBase)chkShapeModel).set_UseVisualStyleBackColor(true);
			chkShapeModel.add_CheckedChanged((EventHandler)chkShapeModel_CheckedChanged);
			((Control)chkBinary).set_AutoSize(true);
			chkBinary.set_Checked(Settings.Default.HasMoonsChecked);
			((Control)chkBinary).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "HasMoonsChecked", true, (DataSourceUpdateMode)1));
			((Control)chkBinary).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkBinary).set_Location(new Point(5, 49));
			((Control)chkBinary).set_Name("chkBinary");
			((Control)chkBinary).set_Size(new Size(131, 17));
			((Control)chkBinary).set_TabIndex(4);
			((Control)chkBinary).set_Text("Asteroids with moons -");
			((ButtonBase)chkBinary).set_UseVisualStyleBackColor(true);
			chkBinary.add_CheckedChanged((EventHandler)chkBinary_CheckedChanged);
			((Control)pnlDateFilter).set_BackColor(Color.MistyRose);
			pnlDateFilter.set_BorderStyle((BorderStyle)1);
			((Control)pnlDateFilter).get_Controls().Add((Control)(object)panel1);
			((Control)pnlDateFilter).get_Controls().Add((Control)(object)updnEndDay);
			((Control)pnlDateFilter).get_Controls().Add((Control)(object)updnStartDay);
			((Control)pnlDateFilter).get_Controls().Add((Control)(object)updnEndMonth);
			((Control)pnlDateFilter).get_Controls().Add((Control)(object)updnStartMonth);
			((Control)pnlDateFilter).get_Controls().Add((Control)(object)label6);
			((Control)pnlDateFilter).get_Controls().Add((Control)(object)updnEndYear);
			((Control)pnlDateFilter).get_Controls().Add((Control)(object)updnStartYear);
			((Control)pnlDateFilter).get_Controls().Add((Control)(object)chkDate);
			((Control)pnlDateFilter).set_Location(new Point(985, 2));
			((Control)pnlDateFilter).set_Name("pnlDateFilter");
			((Control)pnlDateFilter).set_Size(new Size(142, 132));
			((Control)pnlDateFilter).set_TabIndex(5);
			((Control)panel1).set_BackColor(Color.BurlyWood);
			panel1.set_BorderStyle((BorderStyle)1);
			((Control)panel1).get_Controls().Add((Control)(object)label14);
			((Control)panel1).get_Controls().Add((Control)(object)optCombined);
			((Control)panel1).get_Controls().Add((Control)(object)optStarMag);
			((Control)panel1).set_Location(new Point(-1, 81));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(142, 50));
			((Control)panel1).set_TabIndex(11);
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label14).set_Location(new Point(6, 0));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(124, 13));
			((Control)label14).set_TabIndex(8);
			((Control)label14).set_Text("Displayed magnitude");
			((Control)optCombined).set_AutoSize(true);
			((Control)optCombined).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optCombined).set_Location(new Point(32, 31));
			((Control)optCombined).set_Name("optCombined");
			((Control)optCombined).set_Size(new Size(72, 17));
			((Control)optCombined).set_TabIndex(10);
			((Control)optCombined).set_Text("Combined");
			((ButtonBase)optCombined).set_UseVisualStyleBackColor(true);
			optCombined.add_CheckedChanged((EventHandler)optCombined_CheckedChanged);
			((Control)optStarMag).set_AutoSize(true);
			optStarMag.set_Checked(true);
			((Control)optStarMag).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optStarMag).set_Location(new Point(32, 14));
			((Control)optStarMag).set_Name("optStarMag");
			((Control)optStarMag).set_Size(new Size(66, 17));
			((Control)optStarMag).set_TabIndex(9);
			optStarMag.set_TabStop(true);
			((Control)optStarMag).set_Text("Star only");
			((ButtonBase)optStarMag).set_UseVisualStyleBackColor(true);
			optStarMag.add_CheckedChanged((EventHandler)optStarMag_CheckedChanged);
			((Control)updnEndDay).set_Location(new Point(101, 56));
			updnEndDay.set_Maximum(new decimal(new int[4] { 32, 0, 0, 0 }));
			((Control)updnEndDay).set_Name("updnEndDay");
			((Control)updnEndDay).set_Size(new Size(34, 20));
			((Control)updnEndDay).set_TabIndex(7);
			updnEndDay.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnEndDay.add_ValueChanged((EventHandler)updnEndDay_ValueChanged);
			((Control)updnEndDay).add_Enter((EventHandler)updnEndDay_Enter);
			((Control)updnStartDay).set_Location(new Point(101, 21));
			updnStartDay.set_Maximum(new decimal(new int[4] { 32, 0, 0, 0 }));
			((Control)updnStartDay).set_Name("updnStartDay");
			((Control)updnStartDay).set_Size(new Size(34, 20));
			((Control)updnStartDay).set_TabIndex(3);
			updnStartDay.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnStartDay.add_ValueChanged((EventHandler)updnStartDay_ValueChanged);
			((Control)updnStartDay).add_Enter((EventHandler)updnStartDay_Enter);
			((Control)updnEndMonth).set_Location(new Point(61, 56));
			updnEndMonth.set_Maximum(new decimal(new int[4] { 13, 0, 0, 0 }));
			((Control)updnEndMonth).set_Name("updnEndMonth");
			((Control)updnEndMonth).set_Size(new Size(34, 20));
			((Control)updnEndMonth).set_TabIndex(6);
			updnEndMonth.set_Value(new decimal(new int[4] { 2, 0, 0, 0 }));
			updnEndMonth.add_ValueChanged((EventHandler)updnEndMonth_ValueChanged);
			((Control)updnEndMonth).add_Enter((EventHandler)updnEndMonth_Enter);
			((Control)updnStartMonth).set_Location(new Point(61, 21));
			updnStartMonth.set_Maximum(new decimal(new int[4] { 13, 0, 0, 0 }));
			((Control)updnStartMonth).set_Name("updnStartMonth");
			((Control)updnStartMonth).set_Size(new Size(34, 20));
			((Control)updnStartMonth).set_TabIndex(2);
			updnStartMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnStartMonth.add_ValueChanged((EventHandler)updnStartMonth_ValueChanged);
			((Control)updnStartMonth).add_Enter((EventHandler)updnStartMonth_Enter);
			((Control)chkDate).set_AutoSize(true);
			chkDate.set_Checked(Settings.Default.AsteroidDateRangeChecked);
			((Control)chkDate).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkDate).set_Location(new Point(5, 3));
			((Control)chkDate).set_Name("chkDate");
			((Control)chkDate).set_Size(new Size(103, 17));
			((Control)chkDate).set_TabIndex(0);
			((Control)chkDate).set_Text("Date is between");
			((ButtonBase)chkDate).set_UseVisualStyleBackColor(true);
			chkDate.add_CheckedChanged((EventHandler)chkDate_CheckedChanged);
			((Control)pnlIDfilter).set_BackColor(Color.Bisque);
			pnlIDfilter.set_BorderStyle((BorderStyle)1);
			((Control)pnlIDfilter).get_Controls().Add((Control)(object)txtStarID);
			((Control)pnlIDfilter).get_Controls().Add((Control)(object)chkStarID);
			((Control)pnlIDfilter).get_Controls().Add((Control)(object)label17);
			((Control)pnlIDfilter).get_Controls().Add((Control)(object)label16);
			((Control)pnlIDfilter).get_Controls().Add((Control)(object)label15);
			((Control)pnlIDfilter).get_Controls().Add((Control)(object)chkAsteroidClass);
			((Control)pnlIDfilter).get_Controls().Add((Control)(object)cmbAsteroidClasses);
			((Control)pnlIDfilter).get_Controls().Add((Control)(object)txtAsteroidNumber);
			((Control)pnlIDfilter).get_Controls().Add((Control)(object)txtAsteroidName);
			((Control)pnlIDfilter).get_Controls().Add((Control)(object)chkAsteroidName);
			((Control)pnlIDfilter).get_Controls().Add((Control)(object)chkAsteroidNumber);
			((Control)pnlIDfilter).get_Controls().Add((Control)(object)label27);
			((Control)pnlIDfilter).set_Location(new Point(878, 2));
			((Control)pnlIDfilter).set_Name("pnlIDfilter");
			((Control)pnlIDfilter).set_Size(new Size(106, 132));
			((Control)pnlIDfilter).set_TabIndex(4);
			((Control)txtStarID).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtStarID).set_Location(new Point(20, 112));
			((Control)txtStarID).set_Name("txtStarID");
			((Control)txtStarID).set_Size(new Size(84, 18));
			((Control)txtStarID).set_TabIndex(11);
			((Control)txtStarID).add_KeyDown(new KeyEventHandler(txtStarID_KeyDown));
			chkStarID.set_AutoCheck(false);
			((Control)chkStarID).set_AutoSize(true);
			((Control)chkStarID).set_Location(new Point(3, 114));
			((Control)chkStarID).set_Name("chkStarID");
			((Control)chkStarID).set_Size(new Size(15, 14));
			((Control)chkStarID).set_TabIndex(10);
			((ButtonBase)chkStarID).set_UseVisualStyleBackColor(true);
			chkStarID.add_CheckedChanged((EventHandler)chkStarID_CheckedChanged);
			((Control)chkStarID).add_MouseClick(new MouseEventHandler(chkStarID_MouseClick));
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label17).set_Location(new Point(20, 65));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(82, 13));
			((Control)label17).set_TabIndex(9);
			((Control)label17).set_Text("Taxonomic class");
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label16).set_Location(new Point(20, -1));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(86, 13));
			((Control)label16).set_TabIndex(8);
			((Control)label16).set_Text("Asteroid number ");
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label15).set_Location(new Point(20, 32));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(76, 13));
			((Control)label15).set_TabIndex(7);
			((Control)label15).set_Text("Name contains");
			((Control)chkAsteroidClass).set_AutoSize(true);
			((Control)chkAsteroidClass).set_Location(new Point(3, 81));
			((Control)chkAsteroidClass).set_Name("chkAsteroidClass");
			((Control)chkAsteroidClass).set_Size(new Size(15, 14));
			((Control)chkAsteroidClass).set_TabIndex(6);
			((ButtonBase)chkAsteroidClass).set_UseVisualStyleBackColor(true);
			chkAsteroidClass.add_CheckedChanged((EventHandler)chkAsteroidClass_CheckedChanged);
			((Control)cmbAsteroidClasses).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbAsteroidClasses).set_FormattingEnabled(true);
			cmbAsteroidClasses.get_Items().AddRange(new object[8] { "All", "Amor", "Apollo", "Aten", "Centaur", "PHA", "TNO", "Trojan" });
			((Control)cmbAsteroidClasses).set_Location(new Point(20, 78));
			((Control)cmbAsteroidClasses).set_Name("cmbAsteroidClasses");
			((Control)cmbAsteroidClasses).set_Size(new Size(76, 20));
			((Control)cmbAsteroidClasses).set_TabIndex(5);
			cmbAsteroidClasses.add_SelectedIndexChanged((EventHandler)cmbAsteroidClasses_SelectedIndexChanged);
			((Control)label27).set_AutoSize(true);
			((Control)label27).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label27).set_Location(new Point(20, 99));
			((Control)label27).set_Name("label27");
			((Control)label27).set_Size(new Size(40, 13));
			((Control)label27).set_TabIndex(10);
			((Control)label27).set_Text("Star ID");
			((Control)pnlVisibilityFilter).set_BackColor(Color.BurlyWood);
			pnlVisibilityFilter.set_BorderStyle((BorderStyle)1);
			((Control)pnlVisibilityFilter).get_Controls().Add((Control)(object)lblMagType);
			((Control)pnlVisibilityFilter).get_Controls().Add((Control)(object)updnAperture);
			((Control)pnlVisibilityFilter).get_Controls().Add((Control)(object)chkAperture);
			((Control)pnlVisibilityFilter).get_Controls().Add((Control)(object)updnStarMag);
			((Control)pnlVisibilityFilter).get_Controls().Add((Control)(object)chkStarMag);
			((Control)pnlVisibilityFilter).get_Controls().Add((Control)(object)updnSolarElong);
			((Control)pnlVisibilityFilter).get_Controls().Add((Control)(object)updnDiameter);
			((Control)pnlVisibilityFilter).get_Controls().Add((Control)(object)updnMaxDurn);
			((Control)pnlVisibilityFilter).get_Controls().Add((Control)(object)updnMagDrop);
			((Control)pnlVisibilityFilter).get_Controls().Add((Control)(object)chkSolarElongation);
			((Control)pnlVisibilityFilter).get_Controls().Add((Control)(object)chkMagDrop);
			((Control)pnlVisibilityFilter).get_Controls().Add((Control)(object)chkDuration);
			((Control)pnlVisibilityFilter).get_Controls().Add((Control)(object)label4);
			((Control)pnlVisibilityFilter).get_Controls().Add((Control)(object)chkDiameter);
			((Control)pnlVisibilityFilter).get_Controls().Add((Control)(object)label5);
			((Control)pnlVisibilityFilter).get_Controls().Add((Control)(object)label12);
			((Control)pnlVisibilityFilter).get_Controls().Add((Control)(object)optIntegrate);
			((Control)pnlVisibilityFilter).get_Controls().Add((Control)(object)label25);
			((Control)pnlVisibilityFilter).set_Location(new Point(329, 2));
			((Control)pnlVisibilityFilter).set_Name("pnlVisibilityFilter");
			((Control)pnlVisibilityFilter).set_Size(new Size(175, 132));
			((Control)pnlVisibilityFilter).set_TabIndex(1);
			((Control)lblMagType).set_AutoSize(true);
			((Control)lblMagType).set_Location(new Point(126, 26));
			((Control)lblMagType).set_Name("lblMagType");
			((Control)lblMagType).set_Size(new Size(34, 13));
			((Control)lblMagType).set_TabIndex(17);
			((Control)lblMagType).set_Text("visual");
			((Control)updnAperture).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "AsteroidAperture_cm", true, (DataSourceUpdateMode)1));
			updnAperture.set_Increment(new decimal(new int[4] { 5, 0, 0, 0 }));
			((Control)updnAperture).set_Location(new Point(54, 106));
			updnAperture.set_Maximum(new decimal(new int[4] { 95, 0, 0, 0 }));
			updnAperture.set_Minimum(new decimal(new int[4] { 5, 0, 0, 0 }));
			((Control)updnAperture).set_Name("updnAperture");
			((Control)updnAperture).set_Size(new Size(34, 20));
			((Control)updnAperture).set_TabIndex(14);
			updnAperture.set_Value(Settings.Default.AsteroidAperture_cm);
			updnAperture.add_ValueChanged((EventHandler)updnAperture_ValueChanged);
			((Control)chkAperture).set_AutoSize(true);
			chkAperture.set_Checked(Settings.Default.AsteroidApertureSet);
			((Control)chkAperture).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidApertureSet", true, (DataSourceUpdateMode)1));
			((Control)chkAperture).set_Location(new Point(5, 108));
			((Control)chkAperture).set_Name("chkAperture");
			((Control)chkAperture).set_Size(new Size(54, 17));
			((Control)chkAperture).set_TabIndex(13);
			((Control)chkAperture).set_Text("Apert.");
			((ButtonBase)chkAperture).set_UseVisualStyleBackColor(true);
			chkAperture.add_CheckedChanged((EventHandler)chkAperture_CheckedChanged);
			((Control)updnStarMag).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "StarMagListDisplay", true, (DataSourceUpdateMode)1));
			updnStarMag.set_DecimalPlaces(1);
			((Control)updnStarMag).set_Location(new Point(79, 22));
			updnStarMag.set_Maximum(new decimal(new int[4] { 25, 0, 0, 0 }));
			updnStarMag.set_Minimum(new decimal(new int[4] { 4, 0, 0, 0 }));
			((Control)updnStarMag).set_Name("updnStarMag");
			((Control)updnStarMag).set_Size(new Size(44, 20));
			((Control)updnStarMag).set_TabIndex(3);
			updnStarMag.set_Value(Settings.Default.StarMagListDisplay);
			updnStarMag.add_ValueChanged((EventHandler)updnStarMag_ValueChanged);
			((Control)updnStarMag).add_Enter((EventHandler)updnStarMag_Enter);
			((Control)chkStarMag).set_AutoSize(true);
			chkStarMag.set_Checked(Settings.Default.AsteoidStarMagChecked);
			((Control)chkStarMag).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteoidStarMagChecked", true, (DataSourceUpdateMode)1));
			((Control)chkStarMag).set_Location(new Point(5, 24));
			((Control)chkStarMag).set_Name("chkStarMag");
			((Control)chkStarMag).set_Size(new Size(77, 17));
			((Control)chkStarMag).set_TabIndex(2);
			((Control)chkStarMag).set_Text("Star mag <");
			((ButtonBase)chkStarMag).set_UseVisualStyleBackColor(true);
			chkStarMag.add_CheckedChanged((EventHandler)chkStarMag_CheckedChanged);
			((Control)updnSolarElong).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "SolarElongListDisplay", true, (DataSourceUpdateMode)1));
			updnSolarElong.set_Increment(new decimal(new int[4] { 5, 0, 0, 0 }));
			((Control)updnSolarElong).set_Location(new Point(110, 85));
			updnSolarElong.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			((Control)updnSolarElong).set_Name("updnSolarElong");
			((Control)updnSolarElong).set_Size(new Size(35, 20));
			((Control)updnSolarElong).set_TabIndex(11);
			updnSolarElong.set_Value(Settings.Default.SolarElongListDisplay);
			updnSolarElong.add_ValueChanged((EventHandler)updnSolarElong_ValueChanged);
			((Control)updnSolarElong).add_Enter((EventHandler)updnSolarElong_Enter);
			((Control)updnDiameter).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "DiameterListDisplay", true, (DataSourceUpdateMode)1));
			((Control)updnDiameter).set_Location(new Point(78, 64));
			updnDiameter.set_Maximum(new decimal(new int[4] { 500, 0, 0, 0 }));
			updnDiameter.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnDiameter).set_Name("updnDiameter");
			((Control)updnDiameter).set_Size(new Size(43, 20));
			((Control)updnDiameter).set_TabIndex(8);
			updnDiameter.set_Value(Settings.Default.DiameterListDisplay);
			updnDiameter.add_ValueChanged((EventHandler)updnDiameter_ValueChanged);
			((Control)updnDiameter).add_Click((EventHandler)updnDiameter_Click);
			((Control)updnDiameter).add_Enter((EventHandler)updnDiameter_Enter);
			((Control)updnMaxDurn).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MaxDurnListDisplay", true, (DataSourceUpdateMode)1));
			updnMaxDurn.set_DecimalPlaces(1);
			updnMaxDurn.set_Increment(new decimal(new int[4] { 2, 0, 0, 65536 }));
			((Control)updnMaxDurn).set_Location(new Point(103, 43));
			updnMaxDurn.set_Maximum(new decimal(new int[4] { 99, 0, 0, 0 }));
			((Control)updnMaxDurn).set_Name("updnMaxDurn");
			((Control)updnMaxDurn).set_Size(new Size(36, 20));
			((Control)updnMaxDurn).set_TabIndex(5);
			updnMaxDurn.set_Value(Settings.Default.MaxDurnListDisplay);
			updnMaxDurn.add_ValueChanged((EventHandler)updnMaxDurn_ValueChanged);
			((Control)updnMaxDurn).add_Click((EventHandler)updnMaxDurn_Click);
			((Control)updnMaxDurn).add_Enter((EventHandler)updnMaxDurn_Enter);
			((Control)updnMagDrop).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MagDropInListDisplay", true, (DataSourceUpdateMode)1));
			updnMagDrop.set_DecimalPlaces(1);
			updnMagDrop.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnMagDrop).set_Location(new Point(110, 1));
			updnMagDrop.set_Maximum(new decimal(new int[4] { 2, 0, 0, 0 }));
			((Control)updnMagDrop).set_Name("updnMagDrop");
			((Control)updnMagDrop).set_Size(new Size(39, 20));
			((Control)updnMagDrop).set_TabIndex(1);
			updnMagDrop.set_Value(Settings.Default.MagDropInListDisplay);
			updnMagDrop.add_ValueChanged((EventHandler)updnMagDrop_ValueChanged);
			((Control)updnMagDrop).add_Enter((EventHandler)updnMagDrop_Enter);
			((Control)chkSolarElongation).set_AutoSize(true);
			chkSolarElongation.set_Checked(Settings.Default.AsteroidSolarElongationChecked);
			((Control)chkSolarElongation).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidSolarElongationChecked", true, (DataSourceUpdateMode)1));
			((Control)chkSolarElongation).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkSolarElongation).set_Location(new Point(5, 87));
			((Control)chkSolarElongation).set_Name("chkSolarElongation");
			((Control)chkSolarElongation).set_Size(new Size(111, 17));
			((Control)chkSolarElongation).set_TabIndex(10);
			((Control)chkSolarElongation).set_Text("Solar elongation >");
			((ButtonBase)chkSolarElongation).set_UseVisualStyleBackColor(true);
			chkSolarElongation.add_CheckedChanged((EventHandler)chkSolarElongation_CheckedChanged);
			((Control)chkMagDrop).set_AutoSize(true);
			chkMagDrop.set_Checked(Settings.Default.AsteroidMagDropChecked);
			((Control)chkMagDrop).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidMagDropChecked", true, (DataSourceUpdateMode)1));
			((Control)chkMagDrop).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkMagDrop).set_Location(new Point(5, 3));
			((Control)chkMagDrop).set_Name("chkMagDrop");
			((Control)chkMagDrop).set_Size(new Size(109, 17));
			((Control)chkMagDrop).set_TabIndex(0);
			((Control)chkMagDrop).set_Text("Magnitude drop >");
			((ButtonBase)chkMagDrop).set_UseVisualStyleBackColor(true);
			chkMagDrop.add_CheckedChanged((EventHandler)chkMagDrop_CheckedChanged);
			((Control)chkDuration).set_AutoSize(true);
			chkDuration.set_Checked(Settings.Default.AsteroidDurationChecked);
			((Control)chkDuration).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidDurationChecked", true, (DataSourceUpdateMode)1));
			((Control)chkDuration).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkDuration).set_Location(new Point(5, 45));
			((Control)chkDuration).set_Name("chkDuration");
			((Control)chkDuration).set_Size(new Size(103, 17));
			((Control)chkDuration).set_TabIndex(4);
			((Control)chkDuration).set_Text("Maximum durn >");
			((ButtonBase)chkDuration).set_UseVisualStyleBackColor(true);
			chkDuration.add_CheckedChanged((EventHandler)chkDuration_CheckedChanged);
			((Control)chkDiameter).set_AutoSize(true);
			chkDiameter.set_Checked(Settings.Default.AsteroidDiameterChecked);
			((Control)chkDiameter).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidDiameterChecked", true, (DataSourceUpdateMode)1));
			((Control)chkDiameter).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkDiameter).set_Location(new Point(5, 66));
			((Control)chkDiameter).set_Name("chkDiameter");
			((Control)chkDiameter).set_Size(new Size(77, 17));
			((Control)chkDiameter).set_TabIndex(7);
			((Control)chkDiameter).set_Text("Diameter >");
			((ButtonBase)chkDiameter).set_UseVisualStyleBackColor(true);
			chkDiameter.add_CheckedChanged((EventHandler)chkDiameter_CheckedChanged);
			optIntegrate.set_AutoCheck(false);
			((Control)optIntegrate).set_AutoSize(true);
			optIntegrate.set_Checked(Settings.Default.AsteroidIntegrateCamera);
			((Control)optIntegrate).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidIntegrateCamera", true, (DataSourceUpdateMode)1));
			((Control)optIntegrate).set_Location(new Point(108, 108));
			((Control)optIntegrate).set_Name("optIntegrate");
			((Control)optIntegrate).set_Size(new Size(67, 17));
			((Control)optIntegrate).set_TabIndex(16);
			optIntegrate.set_TabStop(true);
			((Control)optIntegrate).set_Text("Integrate");
			((ButtonBase)optIntegrate).set_UseVisualStyleBackColor(true);
			optIntegrate.add_CheckedChanged((EventHandler)optIntegrate_CheckedChanged);
			((Control)optIntegrate).add_Click((EventHandler)optIntegrate_Click);
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Location(new Point(87, 110));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(21, 13));
			((Control)label25).set_TabIndex(15);
			((Control)label25).set_Text("cm");
			((Control)pnlSiteFilter).set_BackColor(Color.LightGoldenrodYellow);
			pnlSiteFilter.set_BorderStyle((BorderStyle)1);
			((Control)pnlSiteFilter).get_Controls().Add((Control)(object)pnlSiteOptions);
			((Control)pnlSiteFilter).get_Controls().Add((Control)(object)label8);
			((Control)pnlSiteFilter).get_Controls().Add((Control)(object)cmdReSetSites);
			((Control)pnlSiteFilter).get_Controls().Add((Control)(object)label1);
			((Control)pnlSiteFilter).get_Controls().Add((Control)(object)txtLatitude);
			((Control)pnlSiteFilter).get_Controls().Add((Control)(object)txtLongitude);
			((Control)pnlSiteFilter).get_Controls().Add((Control)(object)chkSite);
			((Control)pnlSiteFilter).set_Location(new Point(74, 2));
			((Control)pnlSiteFilter).set_Name("pnlSiteFilter");
			((Control)pnlSiteFilter).set_Size(new Size(254, 132));
			((Control)pnlSiteFilter).set_TabIndex(0);
			((Control)pnlSiteOptions).set_BackColor(Color.LightYellow);
			((Control)pnlSiteOptions).get_Controls().Add((Control)(object)cmbProbabilityValues);
			((Control)pnlSiteOptions).get_Controls().Add((Control)(object)chkProbability);
			((Control)pnlSiteOptions).get_Controls().Add((Control)(object)chkHorizon);
			((Control)pnlSiteOptions).get_Controls().Add((Control)(object)updnDistKM);
			((Control)pnlSiteOptions).get_Controls().Add((Control)(object)updnDistArcSec);
			((Control)pnlSiteOptions).get_Controls().Add((Control)(object)updnLocalAltitude);
			((Control)pnlSiteOptions).get_Controls().Add((Control)(object)label3);
			((Control)pnlSiteOptions).get_Controls().Add((Control)(object)label2);
			((Control)pnlSiteOptions).get_Controls().Add((Control)(object)label7);
			((Control)pnlSiteOptions).get_Controls().Add((Control)(object)chkDistArcSec);
			((Control)pnlSiteOptions).get_Controls().Add((Control)(object)chkSiteDistKM);
			((Control)pnlSiteOptions).get_Controls().Add((Control)(object)label11);
			((Control)pnlSiteOptions).get_Controls().Add((Control)(object)chkLocalAltitude);
			((Control)pnlSiteOptions).set_Location(new Point(2, 39));
			((Control)pnlSiteOptions).set_Name("pnlSiteOptions");
			((Control)pnlSiteOptions).set_Size(new Size(246, 90));
			((Control)pnlSiteOptions).set_TabIndex(6);
			((Control)cmbProbabilityValues).set_Font(new Font("Consolas", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbProbabilityValues).set_FormattingEnabled(true);
			((Control)cmbProbabilityValues).set_Location(new Point(84, 0));
			((Control)cmbProbabilityValues).set_Name("cmbProbabilityValues");
			((Control)cmbProbabilityValues).set_Size(new Size(59, 22));
			((Control)cmbProbabilityValues).set_TabIndex(1);
			((Control)cmbProbabilityValues).set_Text(" >10%");
			cmbProbabilityValues.add_SelectedIndexChanged((EventHandler)cmbProbabilityValues_SelectedIndexChanged);
			((Control)chkProbability).set_AutoSize(true);
			((Control)chkProbability).set_Location(new Point(17, 3));
			((Control)chkProbability).set_Name("chkProbability");
			((Control)chkProbability).set_Size(new Size(72, 17));
			((Control)chkProbability).set_TabIndex(0);
			((Control)chkProbability).set_Text("Probabilty");
			((ButtonBase)chkProbability).set_UseVisualStyleBackColor(true);
			chkProbability.add_CheckedChanged((EventHandler)chkProbability_CheckedChanged);
			((Control)chkHorizon).set_AutoSize(true);
			chkHorizon.set_Checked(Settings.Default.AsteroidHorizon);
			((Control)chkHorizon).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidHorizon", true, (DataSourceUpdateMode)1));
			((Control)chkHorizon).set_Location(new Point(170, 69));
			((Control)chkHorizon).set_Name("chkHorizon");
			((Control)chkHorizon).set_Size(new Size(71, 17));
			((Control)chkHorizon).set_TabIndex(12);
			((Control)chkHorizon).set_Text("> Horizon");
			((ButtonBase)chkHorizon).set_UseVisualStyleBackColor(true);
			chkHorizon.add_CheckedChanged((EventHandler)chkHorizon_CheckedChanged);
			((Control)updnDistKM).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "DistKMListDisplay", true, (DataSourceUpdateMode)1));
			updnDistKM.set_Increment(new decimal(new int[4] { 20, 0, 0, 0 }));
			((Control)updnDistKM).set_Location(new Point(169, 21));
			updnDistKM.set_Maximum(new decimal(new int[4] { 2000, 0, 0, 0 }));
			((Control)updnDistKM).set_Name("updnDistKM");
			((Control)updnDistKM).set_Size(new Size(47, 20));
			((Control)updnDistKM).set_TabIndex(4);
			toolTip1.SetToolTip((Control)(object)updnDistKM, "Distances >500km are approximate");
			updnDistKM.set_Value(Settings.Default.DistKMListDisplay);
			updnDistKM.add_ValueChanged((EventHandler)updnDistKM_ValueChanged);
			((Control)updnDistKM).add_Enter((EventHandler)updnDistKM_Enter);
			((Control)updnDistArcSec).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "DistArcSecListDisplay", true, (DataSourceUpdateMode)1));
			updnDistArcSec.set_DecimalPlaces(2);
			updnDistArcSec.set_Increment(new decimal(new int[4] { 5, 0, 0, 131072 }));
			((Control)updnDistArcSec).set_Location(new Point(184, 45));
			updnDistArcSec.set_Maximum(new decimal(new int[4] { 2, 0, 0, 0 }));
			((Control)updnDistArcSec).set_Name("updnDistArcSec");
			((Control)updnDistArcSec).set_Size(new Size(42, 20));
			((Control)updnDistArcSec).set_TabIndex(7);
			updnDistArcSec.set_Value(Settings.Default.DistArcSecListDisplay);
			updnDistArcSec.add_ValueChanged((EventHandler)updnDistArcSec_ValueChanged);
			((Control)updnDistArcSec).add_Enter((EventHandler)updnDistArcSec_Enter);
			((Control)updnLocalAltitude).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "AsteroidLocalAltitudeValue", true, (DataSourceUpdateMode)1));
			((Control)updnLocalAltitude).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnLocalAltitude).set_Location(new Point(117, 66));
			updnLocalAltitude.set_Maximum(new decimal(new int[4] { 30, 0, 0, 0 }));
			updnLocalAltitude.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnLocalAltitude).set_Name("updnLocalAltitude");
			((Control)updnLocalAltitude).set_Size(new Size(34, 20));
			((Control)updnLocalAltitude).set_TabIndex(10);
			updnLocalAltitude.set_Value(Settings.Default.AsteroidLocalAltitudeValue);
			updnLocalAltitude.add_ValueChanged((EventHandler)updnLocalAltitude_ValueChanged);
			((Control)updnLocalAltitude).add_Enter((EventHandler)updnLocalAltitude_Enter);
			chkDistArcSec.set_AutoCheck(false);
			((Control)chkDistArcSec).set_AutoSize(true);
			chkDistArcSec.set_Checked(Settings.Default.AsteroidDistanceSecChecked);
			((Control)chkDistArcSec).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidDistanceSecChecked", true, (DataSourceUpdateMode)1));
			((Control)chkDistArcSec).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkDistArcSec).set_Location(new Point(17, 47));
			((Control)chkDistArcSec).set_Name("chkDistArcSec");
			((Control)chkDistArcSec).set_Size(new Size(172, 17));
			((Control)chkDistArcSec).set_TabIndex(6);
			((Control)chkDistArcSec).set_Text("Distance of asteroid from star <");
			((ButtonBase)chkDistArcSec).set_UseVisualStyleBackColor(true);
			chkDistArcSec.add_CheckedChanged((EventHandler)chkDistArcSec_CheckedChanged);
			((Control)chkDistArcSec).add_Click((EventHandler)chkDistArcSec_Click);
			chkSiteDistKM.set_AutoCheck(false);
			((Control)chkSiteDistKM).set_AutoSize(true);
			chkSiteDistKM.set_Checked(Settings.Default.AsteroidDistanceKMChecked);
			chkSiteDistKM.set_CheckState((CheckState)1);
			((Control)chkSiteDistKM).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidDistanceKMChecked", true, (DataSourceUpdateMode)1));
			((Control)chkSiteDistKM).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkSiteDistKM).set_Location(new Point(17, 23));
			((Control)chkSiteDistKM).set_Name("chkSiteDistKM");
			((Control)chkSiteDistKM).set_Size(new Size(155, 17));
			((Control)chkSiteDistKM).set_TabIndex(3);
			((Control)chkSiteDistKM).set_Text("Distance of site from path <");
			toolTip1.SetToolTip((Control)(object)chkSiteDistKM, "Distances >500km are approximate");
			((ButtonBase)chkSiteDistKM).set_UseVisualStyleBackColor(true);
			chkSiteDistKM.add_CheckedChanged((EventHandler)chkSiteDistKM_CheckedChanged);
			((Control)chkSiteDistKM).add_Click((EventHandler)chkSiteDistKM_Click);
			((Control)chkLocalAltitude).set_AutoSize(true);
			chkLocalAltitude.set_Checked(Settings.Default.AsteroidLocalAltitude);
			((Control)chkLocalAltitude).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidLocalAltitude", true, (DataSourceUpdateMode)1));
			((Control)chkLocalAltitude).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkLocalAltitude).set_Location(new Point(17, 68));
			((Control)chkLocalAltitude).set_Name("chkLocalAltitude");
			((Control)chkLocalAltitude).set_Size(new Size(101, 17));
			((Control)chkLocalAltitude).set_TabIndex(9);
			((Control)chkLocalAltitude).set_Text("Local altitude  >");
			((ButtonBase)chkLocalAltitude).set_UseVisualStyleBackColor(true);
			chkLocalAltitude.add_CheckedChanged((EventHandler)chkLocalAltitude_CheckedChanged);
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(86, 1));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(47, 13));
			((Control)label8).set_TabIndex(1);
			((Control)label8).set_Text("E. Long.");
			((Control)txtLatitude).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatitude).set_Location(new Point(145, 15));
			((Control)txtLatitude).set_Name("txtLatitude");
			((Control)txtLatitude).set_Size(new Size(47, 20));
			((Control)txtLatitude).set_TabIndex(4);
			((Control)txtLatitude).set_Text(Settings.Default.AsteroidSiteLatitude);
			((Control)txtLatitude).add_TextChanged((EventHandler)txtLatitude_TextChanged);
			((Control)txtLatitude).add_Enter((EventHandler)txtLatitude_Enter);
			((Control)txtLatitude).add_KeyDown(new KeyEventHandler(txtLatitude_KeyDown));
			((Control)txtLatitude).add_Leave((EventHandler)txtLatitude_Leave);
			((Control)txtLongitude).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongitude).set_Location(new Point(86, 15));
			((Control)txtLongitude).set_Name("txtLongitude");
			((Control)txtLongitude).set_Size(new Size(55, 20));
			((Control)txtLongitude).set_TabIndex(2);
			((Control)txtLongitude).set_Text(Settings.Default.AsteroidSiteLongitude);
			((Control)txtLongitude).add_TextChanged((EventHandler)txtLongitude_TextChanged);
			((Control)txtLongitude).add_Enter((EventHandler)txtLongitude_Enter);
			((Control)txtLongitude).add_KeyDown(new KeyEventHandler(txtLongitude_KeyDown));
			((Control)txtLongitude).add_Leave((EventHandler)txtLongitude_Leave);
			((Control)chkSite).set_AutoSize(true);
			chkSite.set_Checked(Settings.Default.AsteroidSiteChecked);
			chkSite.set_CheckState((CheckState)1);
			((Control)chkSite).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidSiteChecked", true, (DataSourceUpdateMode)1));
			((Control)chkSite).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkSite).set_Location(new Point(5, 16));
			((Control)chkSite).set_Name("chkSite");
			((Control)chkSite).set_Size(new Size(82, 17));
			((Control)chkSite).set_TabIndex(0);
			((Control)chkSite).set_Text("Visible from:");
			((ButtonBase)chkSite).set_UseVisualStyleBackColor(true);
			chkSite.add_CheckedChanged((EventHandler)chkSite_CheckedChanged);
			((Control)lstSummary).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstSummary).set_FormattingEnabled(true);
			lstSummary.set_HorizontalExtent(1330);
			lstSummary.set_HorizontalScrollbar(true);
			lstSummary.set_ItemHeight(14);
			((Control)lstSummary).set_Location(new Point(6, 211));
			((Control)lstSummary).set_Name("lstSummary");
			((Control)lstSummary).set_Size(new Size(1215, 424));
			((Control)lstSummary).set_TabIndex(2);
			lstSummary.add_SelectedIndexChanged((EventHandler)lstSummary_SelectedIndexChanged);
			((Control)lstSummary).add_DoubleClick((EventHandler)lstSummary_DoubleClick);
			((Control)lstSummary).add_MouseUp(new MouseEventHandler(lstSummary_MouseUp));
			((Control)lblFormHeader).set_AutoSize(true);
			((Control)lblFormHeader).set_Font(new Font("Microsoft Sans Serif", 14f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblFormHeader).set_ForeColor(Color.Blue);
			((Control)lblFormHeader).set_Location(new Point(467, -2));
			((Control)lblFormHeader).set_Name("lblFormHeader");
			((Control)lblFormHeader).set_Size(new Size(248, 24));
			((Control)lblFormHeader).set_TabIndex(3);
			((Control)lblFormHeader).set_Text("S e l e c t i o n   f i l t e r s");
			toolTip1.set_AutomaticDelay(100);
			toolTip1.set_AutoPopDelay(4000);
			toolTip1.set_BackColor(Color.PaleGreen);
			toolTip1.set_InitialDelay(100);
			toolTip1.set_IsBalloon(true);
			toolTip1.set_ReshowDelay(20);
			((Control)PBarSummary).set_Location(new Point(290, 23));
			((Control)PBarSummary).set_Name("PBarSummary");
			((Control)PBarSummary).set_Size(new Size(208, 9));
			((Control)PBarSummary).set_TabIndex(4);
			((Control)PBarSummary).set_Visible(false);
			((Control)cmdAutoCancel).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdAutoCancel).set_Location(new Point(925, 5));
			((Control)cmdAutoCancel).set_Name("cmdAutoCancel");
			((Control)cmdAutoCancel).set_Size(new Size(85, 21));
			((Control)cmdAutoCancel).set_TabIndex(4);
			((Control)cmdAutoCancel).set_Text("Cancel");
			((ButtonBase)cmdAutoCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdAutoCancel).set_Visible(false);
			((Control)cmdAutoCancel).add_Click((EventHandler)cmdAutoCancel_Click);
			((Control)PanelCancel).set_BackColor(Color.LightGreen);
			PanelCancel.set_BorderStyle((BorderStyle)1);
			((Control)PanelCancel).get_Controls().Add((Control)(object)cmdCancelHorizons);
			((Control)PanelCancel).set_Location(new Point(463, 190));
			((Control)PanelCancel).set_Name("PanelCancel");
			((Control)PanelCancel).set_Size(new Size(189, 126));
			((Control)PanelCancel).set_TabIndex(5);
			((Control)PanelCancel).set_Visible(false);
			((Control)cmdCancelHorizons).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdCancelHorizons).set_Location(new Point(52, 32));
			((Control)cmdCancelHorizons).set_Name("cmdCancelHorizons");
			((Control)cmdCancelHorizons).set_Size(new Size(83, 60));
			((Control)cmdCancelHorizons).set_TabIndex(0);
			((Control)cmdCancelHorizons).set_Text("Cancel \r\nHorizons \r\nDownload");
			((ButtonBase)cmdCancelHorizons).set_UseVisualStyleBackColor(true);
			((Control)cmdCancelHorizons).add_Click((EventHandler)cmdCancelHorizons_Click);
			((Control)lblHeader1).set_AutoSize(true);
			((Control)lblHeader1).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblHeader1).set_Location(new Point(7, 184));
			((Control)lblHeader1).set_Name("lblHeader1");
			((Control)lblHeader1).set_Size(new Size(294, 14));
			((Control)lblHeader1).set_TabIndex(6);
			((Control)lblHeader1).set_Text("     Date       U.T.    Diameter   Durn  ");
			((Control)lblHeader2).set_AutoSize(true);
			((Control)lblHeader2).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblHeader2).set_Location(new Point(7, 197));
			((Control)lblHeader2).set_Name("lblHeader2");
			((Control)lblHeader2).set_Size(new Size(294, 14));
			((Control)lblHeader2).set_TabIndex(7);
			((Control)lblHeader2).set_Text("   y   m  d    h   m     km   \"   sec/m  ");
			((Control)lblType).set_AutoSize(true);
			((Control)lblType).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblType).set_Location(new Point(7, 171));
			((Control)lblType).set_Name("lblType");
			((Control)lblType).set_Size(new Size(210, 14));
			((Control)lblType).set_TabIndex(8);
			((Control)lblType).set_Text("     Global summary of events");
			((Control)label26).set_AutoSize(true);
			((Control)label26).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label26).set_ForeColor(Color.Red);
			((Control)label26).set_Location(new Point(568, 20));
			((Control)label26).set_Name("label26");
			((Control)label26).set_Size(new Size(326, 13));
			((Control)label26).set_TabIndex(9);
			((Control)label26).set_Text("For the highest accuracy prediction  -  Select the event  && Right click ");
			((Control)label31).set_AutoSize(true);
			((Control)label31).set_BackColor(Color.Aqua);
			((Control)label31).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label31).set_ForeColor(Color.Firebrick);
			((Control)label31).set_Location(new Point(6, 20));
			((Control)label31).set_Name("label31");
			((Control)label31).set_Size(new Size(248, 13));
			((Control)label31).set_TabIndex(10);
			((Control)label31).set_Text("Output limited to the checked requirements");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(1225, 641));
			((Control)this).get_Controls().Add((Control)(object)label31);
			((Control)this).get_Controls().Add((Control)(object)label26);
			((Control)this).get_Controls().Add((Control)(object)lblType);
			((Control)this).get_Controls().Add((Control)(object)lblHeader1);
			((Control)this).get_Controls().Add((Control)(object)PanelCancel);
			((Control)this).get_Controls().Add((Control)(object)PBarSummary);
			((Control)this).get_Controls().Add((Control)(object)lblFormHeader);
			((Control)this).get_Controls().Add((Control)(object)cmdAutoCancel);
			((Control)this).get_Controls().Add((Control)(object)lstSummary);
			((Control)this).get_Controls().Add((Control)(object)pnlFilters);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)lblHeader2);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("ListAndDisplay");
			((Control)this).set_Text("`");
			((Form)this).add_Load((EventHandler)ListAndDisplay_Load);
			((Control)this).add_Resize((EventHandler)ListAndDisplay_Resize);
			((ISupportInitialize)updnEndYear).EndInit();
			((ISupportInitialize)updnStartYear).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)pnlFilters).ResumeLayout(false);
			((Control)pnlFilters).PerformLayout();
			((Control)pnlRegion).ResumeLayout(false);
			((Control)pnlRegion).PerformLayout();
			((ISupportInitialize)updnLatSE).EndInit();
			((ISupportInitialize)updnLongSE).EndInit();
			((ISupportInitialize)updnLatSW).EndInit();
			((ISupportInitialize)updnLongSW).EndInit();
			((ISupportInitialize)updnLatNE).EndInit();
			((ISupportInitialize)updnLongNE).EndInit();
			((ISupportInitialize)updnLatNW).EndInit();
			((ISupportInitialize)updnLongNW).EndInit();
			((Control)pnlOtherFilter).ResumeLayout(false);
			((Control)pnlOtherFilter).PerformLayout();
			((ISupportInitialize)updnMinD).EndInit();
			((ISupportInitialize)updnUncertainty).EndInit();
			((Control)pnlDateFilter).ResumeLayout(false);
			((Control)pnlDateFilter).PerformLayout();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((ISupportInitialize)updnEndDay).EndInit();
			((ISupportInitialize)updnStartDay).EndInit();
			((ISupportInitialize)updnEndMonth).EndInit();
			((ISupportInitialize)updnStartMonth).EndInit();
			((Control)pnlIDfilter).ResumeLayout(false);
			((Control)pnlIDfilter).PerformLayout();
			((Control)pnlVisibilityFilter).ResumeLayout(false);
			((Control)pnlVisibilityFilter).PerformLayout();
			((ISupportInitialize)updnAperture).EndInit();
			((ISupportInitialize)updnStarMag).EndInit();
			((ISupportInitialize)updnSolarElong).EndInit();
			((ISupportInitialize)updnDiameter).EndInit();
			((ISupportInitialize)updnMaxDurn).EndInit();
			((ISupportInitialize)updnMagDrop).EndInit();
			((Control)pnlSiteFilter).ResumeLayout(false);
			((Control)pnlSiteFilter).PerformLayout();
			((Control)pnlSiteOptions).ResumeLayout(false);
			((Control)pnlSiteOptions).PerformLayout();
			((ISupportInitialize)updnDistKM).EndInit();
			((ISupportInitialize)updnDistArcSec).EndInit();
			((ISupportInitialize)updnLocalAltitude).EndInit();
			((Control)PanelCancel).ResumeLayout(false);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
