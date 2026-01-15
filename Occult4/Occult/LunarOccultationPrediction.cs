using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using System.Text;
using System.Windows.Forms;
using GifCreator;
using LightCurves;
using Occult.File_Actions;
using Occult.Properties;
using Occult.Star_Catalogues;

namespace Occult
{
	public class LunarOccultationPrediction : Form
	{
		private readonly string AppPath;

		private readonly string StarCat0;

		private readonly string StarCat1;

		private readonly string StarCat2;

		private readonly string StarCat3;

		private readonly string StarCat4;

		private const double Radian = 180.0 / Math.PI;

		private bool FormCreated;

		private bool SingleDayComputation = true;

		private bool DatesSet;

		private string StarCatalogue;

		private static bool CancelFlag;

		private static bool SettingFilter = false;

		public static bool UsingSingleSite = false;

		public static bool CurrentElementsAreValid = false;

		private Sites S;

		private Sites CurrentSite = new Sites();

		private List<Sites> AllSites = new List<Sites>();

		internal ArrayList lstPrediction_Index = new ArrayList();

		private bool OutputIsGraze;

		private bool OutputIsMulti;

		private double JDFirstDate;

		private double EastLimit;

		private double WestLimit;

		private double NorthLimit;

		private double SouthLimit;

		private double MaxAperture;

		public double GlobalMagLimitAdjustment;

		private int StartHour;

		public static readonly int[] DaysInMonth = new int[12]
		{
			31, 28, 31, 30, 31, 30, 31, 31, 30, 31,
			30, 31
		};

		private IContainer components;

		private MenuStrip menuStrip1;

		private Label label28;

		private ComboBox cmbLunarDayEnd;

		private ComboBox cmbLunarMonthEnd;

		private NumericUpDown updnLunarYearEnd;

		private ComboBox cmbSiteFiles;

		private ComboBox cmbSite;

		internal ProgressBar pBar;

		private ProgressBar pBar2;

		private GroupBox grpXZ80;

		private RadioButton optXZ9;

		private RadioButton optXZ6;

		private RadioButton optXZ3;

		private RadioButton optZC;

		private Button cmdComputeGrazes;

		internal ListBox lstPrediction;

		private GroupBox grpObjects;

		private ToolStripMenuItem withPredictionToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private CheckBox chkListGrazes;

		private Label lblCurrentDate;

		private Button cmdWorldMap;

		private ContextMenuStrip contextMenuStrip1;

		private ToolStripMenuItem displayStarDetailsToolStripMenuItem;

		private ToolStripMenuItem moonMapToolStripMenuItem;

		private ToolStripMenuItem worldMapToolStripMenuItem;

		private ToolStripMenuItem computeGrazeToolStripMenuItem;

		private ToolStripMenuItem reListOccultationsToolStripMenuItem;

		private Button cmdMultiLocation;

		private ToolStripMenuItem multiSitePredictionToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private Label LabelRightClick;

		private ToolStripMenuItem grazePathMapToolStripMenuItem;

		private ToolStripMenuItem grazeProfileToolStripMenuItem;

		private ToolStripMenuItem computePredictionsForToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		internal Label labelIntegration;

		private CheckBox chkFilter;

		private ToolStripMenuItem viewInGoogleEarthToolStripMenuItem;

		private ToolStripMenuItem saveGoogleEarthKMLFileToolStripMenuItem;

		private ToolStripMenuItem saveGoogleMapsHTMFileToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator2;

		private ToolStripMenuItem showLunarEphemerisToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator3;

		private Button cmdCancel;

		private ToolStripMenuItem limitingMagnitudeTableToolStripMenuItem;

		private Button cmdHome;

		private Button cmdHomeSet;

		private GroupBox groupBox1;

		private Label label6;

		private Label label5;

		private Label label4;

		private Label label1;

		private Button cmdFullYear;

		private Button cmdFullMonth;

		private Button cmdOneDay;

		private Button button1;

		private Label label2;

		private Label label3;

		private Label label7;

		private Label label8;

		private GroupBox groupBox3;

		private GroupBox groupBox2;

		private GroupBox grpAnyWhere;

		private ToolStripMenuItem saveInOtherMapFormatsToolStripMenuItem;

		private ToolStripMenuItem cmxPrecisionMappingStreetsToolStripMenuItem;

		private ToolStripMenuItem gpxEasyGPSToolStripMenuItem;

		private ToolStripMenuItem mifMSMapPointToolStripMenuItem;

		private ToolStripMenuItem pltOziExplorerToolStripMenuItem;

		private ToolStripMenuItem genforMakingESRIShapefilesToolStripMenuItem;

		private ToolStripMenuItem txtforDelormeStreetAtlasToolStripMenuItem;

		private Label lblRange;

		private GroupBox grpSingleSite;

		private Button cmdUseSingleSite;

		private Button cmdUseSiteFiles;

		private Label label11;

		private Label label12;

		private Label label13;

		private Label label24;

		private Label label25;

		private Label label20;

		private Label label37;

		private Label label14;

		private TextBox txtName;

		private CheckBox chkAbbrev;

		internal CheckBox chkAsteroids;

		private ToolStripMenuItem magLimitAdjustmentToolStripMenuItem;

		private ToolStripMenuItem toolStripMenuItem2;

		private ToolStripMenuItem toolStripMenuItem3;

		private ToolStripMenuItem toolStripMenuItem4;

		private ToolStripMenuItem toolStripMenuItem5;

		private ToolStripMenuItem toolStripMenuItem6;

		private ToolStripMenuItem toolStripMenuItem7;

		private ToolStripMenuItem toolStripMenuItem8;

		private ToolStripMenuItem toolStripMenuItem9;

		private ToolStripMenuItem helpToolStripMenuItem;

		private RadioButton radioButton1;

		private Label label17;

		private RadioButton radioButton4;

		private RadioButton radioButton3;

		private ToolTip toolTip1;

		internal NumericUpDown updnLunarYearStart;

		internal Button cmdCompute;

		internal ComboBox cmbLunarDayStart;

		internal ComboBox cmbLunarMonthStart;

		internal RadioButton optXZ;

		internal CheckBox chkPlanets;

		internal CheckBox chkStars;

		internal TextBox txtLatS;

		internal TextBox txtLatM;

		internal TextBox txtLatD;

		internal TextBox txtLongS;

		internal TextBox txtLongM;

		internal TextBox txtLongD;

		internal TextBox txtAltitude;

		internal NumericUpDown updnAperture;

		internal RadioButton radioButton2;

		private ToolStripMenuItem grazeProfileSetUpAnimatedGIFToolStripMenuItem;

		private ToolStripMenuItem setUpAnimatedGIFToolStripMenuItem;

		private ToolStripMenuItem createAnimatedGIFToolStripMenuItem;

		internal CheckBox chkDoublesOnly;

		private ToolStripMenuItem doubleStarDetailsToolStripMenuItem;

		private ToolStripMenuItem weatherForecastsToolStripMenuItem;

		private ToolStripMenuItem dayWeatherForecastToolStripMenuItem1;

		private ToolStripMenuItem weatherMapsToolStripMenuItem;

		private ToolStripMenuItem setOutputFilterToolStripMenuItem;

		internal CheckBox chkFilterOutput;

		private ToolStripMenuItem copySingleLineeventToolStripMenuItem;

		private ToolStripMenuItem placeEventInRecordingTimerToolStripMenuItem;

		private ToolStripMenuItem showVDubTimerToolStripMenuItem;

		private Panel panel1;

		private ToolStripMenuItem selectVirtualDubRecordingDurationToolStripMenuItem;

		private ToolStripMenuItem sToolStripMenuItem;

		private ToolStripMenuItem secToolStripMenuItem;

		private ToolStripMenuItem secToolStripMenuItem1;

		private ToolStripMenuItem secToolStripMenuItem2;

		private ToolStripMenuItem secToolStripMenuItem3;

		private ToolStripMenuItem showPastLightCurvesToolStripMenuItem;

		private ToolStripMenuItem placeNext10EventsIntoRecordingTimerToolStripMenuItem;

		private ToolStripMenuItem showApparentstarPositioNnotJ2000ToolStripMenuItem;

		private Panel pnlMultiSiteOutput;

		private Label label9;

		private CheckBox chkShort;

		private CheckBox chkLong;

		private Label label10;

		private Button cmdMultiOK;

		private Button cmdFormat;

		public LunarOccultationPrediction()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
			StarCatalogue = (StarCat0 = AppPath + "\\Resource Files\\XZ80.dat");
			StarCat1 = AppPath + "\\Resource Files\\ZC.dat";
			StarCat2 = AppPath + "\\Resource Files\\XZ80Mag4.dat";
			StarCat3 = AppPath + "\\Resource Files\\XZ80Mag7.dat";
			StarCat4 = AppPath + "\\Resource Files\\XZ80Mag9.dat";
			LunarOccultations.InitialiseStarCatIndexArray(StarCatalogue);
			((Control)grpSingleSite).set_Left(((Control)groupBox1).get_Left());
			((Control)grpSingleSite).set_Top(((Control)groupBox1).get_Top());
		}

		private void LunarOccultationPrediction_Load(object sender, EventArgs e)
		{
			//IL_022e: Unknown result type (might be due to invalid IL or missing references)
			//IL_042e: Unknown result type (might be due to invalid IL or missing references)
			//IL_043a: Unknown result type (might be due to invalid IL or missing references)
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			((Control)this).set_Height(Settings.Default.LunarPredictFormHeight);
			((Control)this).set_Width(Settings.Default.LunarPredictFormWidth);
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			showApparentstarPositioNnotJ2000ToolStripMenuItem.set_Checked(Settings.Default.LunarPredict_DisplayApparentPosition);
			FormCreated = false;
			StartHour = Settings.Default.StartHour;
			if (Settings.Default.StartHour == -6)
			{
				radioButton1.set_Checked(true);
			}
			else if (Settings.Default.StartHour == 6)
			{
				radioButton3.set_Checked(true);
			}
			else if (Settings.Default.StartHour == 12)
			{
				radioButton4.set_Checked(true);
			}
			else
			{
				radioButton2.set_Checked(true);
				StartHour = 0;
			}
			DateTime dateTime = DateTime.Now.ToUniversalTime().AddHours(-StartHour);
			DatesSet = false;
			NumericUpDown obj = updnLunarYearStart;
			decimal value;
			updnLunarYearEnd.set_Value(value = dateTime.Year);
			obj.set_Value(value);
			ComboBox obj2 = cmbLunarMonthStart;
			int selectedIndex;
			((ListControl)cmbLunarMonthEnd).set_SelectedIndex(selectedIndex = dateTime.Month - 1);
			((ListControl)obj2).set_SelectedIndex(selectedIndex);
			DatesSet = true;
			ComboBox obj3 = cmbLunarDayStart;
			((ListControl)cmbLunarDayEnd).set_SelectedIndex(selectedIndex = dateTime.Day - 1);
			((ListControl)obj3).set_SelectedIndex(selectedIndex);
			SetTimerOffsetChecks();
			string[] files = Directory.GetFiles(AppPath + "\\Sites", "*.site");
			foreach (string path in files)
			{
				cmbSiteFiles.get_Items().Add((object)Path.GetFileName(path));
			}
			if (cmbSiteFiles.get_Items().get_Count() <= 0)
			{
				MessageBox.Show("No site files available", "No site files", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			((ListControl)cmbSiteFiles).set_SelectedIndex(0);
			string siteFileLunarPredict = Settings.Default.SiteFileLunarPredict;
			for (int i = 0; i < cmbSiteFiles.get_Items().get_Count(); i++)
			{
				if (siteFileLunarPredict == cmbSiteFiles.get_Items().get_Item(i).ToString())
				{
					((ListControl)cmbSiteFiles).set_SelectedIndex(i);
					break;
				}
			}
			CurrentSite = AllSites[((ListControl)cmbSite).get_SelectedIndex()];
			SetStarCats();
			if (!File.Exists(AppPath + "\\Resource Files\\UserMinorPlanetElements_Lunar.csv"))
			{
				List<AsteroidElements> astElements = Elements.MainAsteroids.AstElements;
				if (astElements.Count < 1)
				{
					Elements.MainAsteroids.Fill_AllAsteroids();
				}
				using StreamWriter streamWriter = new StreamWriter(AppPath + "\\Resource Files\\UserMinorPlanetElements_Lunar.csv");
				streamWriter.WriteLine(AsteroidElements.CSVHeader());
				for (int j = 0; j <= 100; j++)
				{
					double a = astElements[j].A;
					if (astElements[j].H0 + 5.0 * Math.Log10(a * (a - 1.0)) <= 9.0)
					{
						streamWriter.WriteLine(Elements.MainAsteroids.AstElements[j].CSVString());
					}
				}
			}
			if (Elements.UserAsteroids_Lunar.AstElements.Count < 1)
			{
				Elements.UserAsteroids_Lunar.Fill_AllAsteroids();
			}
			if (Elements.UserAsteroids_Lunar.AstElements.Count > 0)
			{
				double osculatingJD = Elements.UserAsteroids_Lunar.AstElements[0].OsculatingJD;
				double num = Utilities.JD_from_Date(DateTime.Now.Year, DateTime.Now.Month, DateTime.Now.Day);
				if (Math.Abs(osculatingJD - num) > 1096.0)
				{
					MessageBox.Show("Epoch of asteroid elements is more than 3 years from the present date.\r\nThe elements should be replaced with a more recent orbit\r\n[using   'Edit the User file of minor planets']\r\n\r\n\r\nPredictions of lunar occultations of asteroids will not be made\r\nif the epoch of the elements differ from the prediction date by\r\nmore than 5 years.\r\n\r\n\r\nYou will now be taken to the editor for the User file of elements. Assuming you have recently updated the file of asteroid elements using ASTORB or MPCORB, the simplest option is to delete all asteroids from the list, click the 'Add' button, then save the new list.", "Check for current asteroid elements.", (MessageBoxButtons)0, (MessageBoxIcon)48);
					((Form)new UserAsteroids_Edit(LunarOccultations: true)).ShowDialog();
				}
			}
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem).set_Visible(Settings.Default.GoogleEarthInstalled);
			FormCreated = true;
		}

		private void LunarOccultationPrediction_Resize(object sender, EventArgs e)
		{
			if (!((((Control)this).get_Width() < 550) | (((Control)this).get_Height() < 300)))
			{
				((Control)lstPrediction).set_Width(((Control)this).get_Width() - 26);
				((Control)lstPrediction).set_Height(((Control)this).get_Height() - 186);
			}
		}

		internal void cmdCompute_Click(object sender, EventArgs e)
		{
			if (UsingSingleSite)
			{
				Set_CurrentSite_for_Singles();
			}
			double longitude = CurrentSite.Longitude / (180.0 / Math.PI);
			double pCos = CurrentSite.pCos;
			double pSin = CurrentSite.pSin;
			LunarOccultations.ListGrazesOnly = chkListGrazes.get_Checked();
			LunarOccultations.Prediction.Clear();
			double num = Utilities.JD_from_Date((int)updnLunarYearStart.get_Value(), ((ListControl)cmbLunarMonthStart).get_SelectedIndex() + 1, ((ListControl)cmbLunarDayStart).get_SelectedIndex() + 1);
			double num2 = Utilities.JD_from_Date((int)updnLunarYearEnd.get_Value(), ((ListControl)cmbLunarMonthEnd).get_SelectedIndex() + 1, ((ListControl)cmbLunarDayEnd).get_SelectedIndex() + 1);
			SettingFilter = true;
			chkFilterOutput.set_Checked(false);
			SettingFilter = false;
			if (!CurrentElementsAreValid || num != num2)
			{
				SingleDayComputation = num == num2;
				SetContextMenuItems(ToReturn: false, SingleDayComputation);
				LunarOccultations.InitialiseSearch();
				CancelFlag = false;
				((Control)cmdCancel).set_Visible(true);
				for (double num3 = num; num3 <= num2; num3 += 1.0)
				{
					((Control)lblCurrentDate).set_Text("[" + Utilities.Date_from_JD(num3, 0) + "]");
					Application.DoEvents();
					if (CancelFlag)
					{
						break;
					}
					LunarOccultations.NewDate(num3, StartHour, chkFilter.get_Checked(), NorthLimit, SouthLimit, EastLimit, WestLimit);
					double readMagLimit = LunarOccultations.GetReadMagLimit(MaxAperture, (double)CurrentSite.MagCorrection + GlobalMagLimitAdjustment);
					LunarOccultations.ScanXZFile(StarCatalogue, NorthLimit, SouthLimit, readMagLimit, chkDoublesOnly.get_Checked(), Auto: false);
					pBar2.set_Minimum(0);
					pBar2.set_Maximum(LunarOccultations.Elements.Count);
					((Control)pBar2).set_Visible(true);
					for (int i = 0; i < LunarOccultations.Elements.Count; i++)
					{
						pBar2.set_Value(i);
						LunarOccultations.ComputeEventForALocation(i, longitude, CurrentSite.Latitude, CurrentSite.Altitude, pSin, pCos, CurrentSite.ApertureCM * 10f, (double)CurrentSite.MagCorrection + GlobalMagLimitAdjustment, CurrentSite.GrazeTravelDist, MultiSite: false, 0);
					}
					((Control)pBar2).set_Visible(false);
					if (LunarOccultations.Prediction.Count < 80)
					{
						LunarOccultations.Prediction.Sort();
						Display(chkAbbrev.get_Checked(), ApplyFilter: false);
					}
				}
				LunarOccultations.Prediction.Sort();
				Display(chkAbbrev.get_Checked(), ApplyFilter: false);
				((Control)cmdCancel).set_Visible(false);
				CurrentElementsAreValid = true;
			}
			else
			{
				ReCompute_SingleDay();
			}
		}

		private void ReCompute_SingleDay()
		{
			double longitude = CurrentSite.Longitude / (180.0 / Math.PI);
			double pCos = CurrentSite.pCos;
			double pSin = CurrentSite.pSin;
			SingleDayComputation = true;
			SetContextMenuItems(ToReturn: false, SingleDayComputation);
			pBar2.set_Minimum(0);
			pBar2.set_Maximum(LunarOccultations.Elements.Count);
			((Control)pBar2).set_Visible(true);
			LunarOccultations.Prediction.Clear();
			for (int i = 0; i < LunarOccultations.Elements.Count; i++)
			{
				pBar2.set_Value(i);
				LunarOccultations.ComputeEventForALocation(i, longitude, CurrentSite.Latitude, CurrentSite.Altitude, pSin, pCos, CurrentSite.ApertureCM * 10f, (double)CurrentSite.MagCorrection + GlobalMagLimitAdjustment, CurrentSite.GrazeTravelDist, MultiSite: false, 0);
			}
			((Control)pBar2).set_Visible(false);
			LunarOccultations.Prediction.Sort();
			Display(chkAbbrev.get_Checked(), chkFilterOutput.get_Checked());
		}

		private void Display(bool ShortLine, bool ApplyFilter)
		{
			bool flag = ApplyFilter & Settings.Default.LunarFilter_TimeRange;
			double num = (double)Settings.Default.LunarFilter_Tstart;
			double num2 = (double)Settings.Default.LunarFilter_Tend;
			if (num2 < num)
			{
				num2 += 24.0;
			}
			bool flag2 = ApplyFilter & Settings.Default.LunarFilter_NoDaytime;
			bool flag3 = ApplyFilter & Settings.Default.LunarFilter_NoBrightLimb;
			bool flag4 = ApplyFilter & Settings.Default.LunarFilter_ExcludeUnlessK2;
			bool flag5 = ApplyFilter & Settings.Default.LunarFilter_NoLightCurve;
			bool flag6 = ApplyFilter & Settings.Default.LunarFilter_ApplyAlt;
			int num3 = (int)Settings.Default.LunarFilter_MinimumAltitude;
			bool flag7 = !(ApplyFilter & Settings.Default.LunarFilter_BrightLimbMessage);
			bool flag8 = !(ApplyFilter & Settings.Default.LunarFilter_StarNameMessage);
			bool flag9 = !(ApplyFilter & Settings.Default.LunarFilter_DoubleStarMessage);
			bool flag10 = !(ApplyFilter & Settings.Default.LunarFilter_VariableStarMessage);
			bool flag11 = !(ApplyFilter & Settings.Default.LunarFilter_ExcludeDurations);
			bool flag12 = ApplyFilter & Settings.Default.LunarFilter_ExcludeGrazeMessage;
			lstPrediction.get_Items().Clear();
			lstPrediction_Index.Clear();
			if (LunarOccultations.ListGrazesOnly)
			{
				lstPrediction.get_Items().Add((object)("Grazing Occultations near " + CurrentSite.Name));
			}
			else
			{
				lstPrediction.get_Items().Add((object)("Occultation prediction for " + CurrentSite.Name));
			}
			lstPrediction_Index.Add(-1);
			lstPrediction.get_Items().Add((object)("E. Longitude " + Utilities.DEGtoDMS(CurrentSite.Longitude, 4, 1, MinutesOnly: false) + ",  Latitude " + Utilities.DEGtoDMS(CurrentSite.Latitude, 3, 1, MinutesOnly: false) + ",  Alt." + string.Format("{0,5:F0}m", CurrentSite.Altitude) + ";  Telescope dia" + string.Format("{0,3:F0}cm", CurrentSite.ApertureCM) + ";  dMag" + string.Format("{0,4:F1}", (double)CurrentSite.MagCorrection + GlobalMagLimitAdjustment)));
			lstPrediction_Index.Add(-1);
			StringBuilder stringBuilder = new StringBuilder();
			if (flag || flag2 || flag3 || flag4 || flag6)
			{
				stringBuilder.Append("   Events excluded:");
				if (flag4)
				{
					stringBuilder.Append(" All except K2 stars, and doubles,");
				}
				if (flag)
				{
					stringBuilder.AppendFormat(" Outside {0,1:f1}h to {1,1:f1}h UT,", num, num2);
				}
				if (flag2)
				{
					stringBuilder.Append(" Daytime,");
				}
				if (flag3)
				{
					stringBuilder.Append(" Bright-limb");
				}
				if (flag6)
				{
					stringBuilder.Append(" Alt > " + num3 + "°");
				}
				lstPrediction.get_Items().Add((object)stringBuilder.ToString());
				lstPrediction_Index.Add(-1);
			}
			StringBuilder stringBuilder2 = new StringBuilder();
			if (!flag7 || !flag8 || !flag9 || !flag10 || !flag11)
			{
				stringBuilder2.Append("   No messages for:");
				if (!flag8)
				{
					stringBuilder2.Append(" Name,");
				}
				if (!flag9)
				{
					stringBuilder2.Append(" Doubles & K2 stars,");
				}
				if (!flag10)
				{
					stringBuilder2.Append(" Variables,");
				}
				if (!flag7)
				{
					stringBuilder2.Append(" Limb-nearby,");
				}
				if (!flag11)
				{
					stringBuilder2.Append(" Durations,");
				}
				if (flag12)
				{
					stringBuilder2.Append(" Graze nearby");
				}
				lstPrediction.get_Items().Add((object)stringBuilder2.ToString());
				lstPrediction_Index.Add(-1);
			}
			lstPrediction.get_Items().Add((object)"");
			lstPrediction_Index.Add(-1);
			if (chkListGrazes.get_Checked())
			{
				lstPrediction.get_Items().Add((object)"       day  Time   P   Star  Sp  Mag  Mag    % Elon Sun Moon      Cusp angle   Distance       Azimuth    Path formula");
			}
			else if (ShortLine)
			{
				lstPrediction.get_Items().Add((object)"       day  Time     P   Star  Sp  Mag  Mag    % Elon Sun  Moon   CA   PA  VA  AA");
			}
			else
			{
				string text = "J2000";
				if (Settings.Default.LunarPredict_DisplayApparentPosition)
				{
					text = "Appnt";
				}
				lstPrediction.get_Items().Add((object)("       day  Time     P   Star  Sp  Mag  Mag    % Elon Sun  Moon   CA   PA  VA  AA Libration   A   B   RV    Cct durn  R.A. (" + text + ")  Dec   Mdist   SV"));
			}
			lstPrediction_Index.Add(-1);
			if (chkListGrazes.get_Checked())
			{
				lstPrediction.get_Items().Add((object)" y   m  d  h  m  s      No  D     v    r V  ill     Alt Alt               o          km ");
			}
			else if (ShortLine)
			{
				lstPrediction.get_Items().Add((object)" y   m  d  h  m   s       No  D     v    r V  ill     Alt Alt Az   o    o   o   o");
			}
			else
			{
				lstPrediction.get_Items().Add((object)" y   m  d  h  m   s       No  D     v    r V  ill     Alt Alt Az   o    o   o   o   L    B   m/o m/o  \"/s     o  sec  h  m   s    o  m  s   Mm   m/s");
			}
			lstPrediction_Index.Add(-1);
			int count = LunarOccultations.Prediction.Count;
			if (count < 1)
			{
				return;
			}
			for (int i = 0; i < count; i++)
			{
				if (flag)
				{
					double num4 = LunarOccultations.Prediction[i].EventUT();
					if (num4 < num)
					{
						num4 += 24.0;
					}
					if (num4 > num2)
					{
						continue;
					}
				}
				if ((flag5 && !LunarOccultations.Prediction[i].LightCurveAvailable) || (flag2 && LunarOccultations.Prediction[i].SunAlt > -6.0) || (flag6 && LunarOccultations.Prediction[i].MoonAlt < (double)num3) || (flag3 && LunarOccultations.Prediction[i].CA < 0.0) || (flag12 && LunarOccultations.Prediction[i].GrazeLatitude2 < 100.0))
				{
					continue;
				}
				lstPrediction.get_Items().Add((object)LunarOccultations.Prediction[i].FormatLine(ShortLine));
				lstPrediction_Index.Add(i);
				if (LunarOccultations.Prediction[i].ZCNameExists && flag8)
				{
					lstPrediction.get_Items().Add((object)LunarOccultations.Prediction[i].ZCName);
					lstPrediction_Index.Add(i);
				}
				if (LunarOccultations.Prediction[i].DoubleDetailsExist && flag9)
				{
					lstPrediction.get_Items().Add((object)LunarOccultations.Prediction[i].DoubleDetails);
					lstPrediction_Index.Add(i);
				}
				if (LunarOccultations.Prediction[i].DoubleWantedExists && flag9)
				{
					lstPrediction.get_Items().Add((object)LunarOccultations.Prediction[i].DoubleObservationsWanted);
					lstPrediction_Index.Add(i);
				}
				if (LunarOccultations.Prediction[i].VariableDetailsExist && flag10)
				{
					lstPrediction.get_Items().Add((object)LunarOccultations.Prediction[i].VariableDetails);
					lstPrediction_Index.Add(i);
				}
				if (!chkListGrazes.get_Checked() && ((LunarOccultations.Prediction[i].LimbDistance < 20.0) & (LunarOccultations.Prediction[i].LimbDistance > 0.0)) && flag7)
				{
					StringBuilder stringBuilder3 = new StringBuilder();
					if (LunarOccultations.Prediction[i].Planet < 1)
					{
						stringBuilder3.Append("   Distance of " + LunarOccultations.Prediction[i].StarId.PadRight(7).Substring(0, 7).Trim());
					}
					else
					{
						stringBuilder3.Append("   Distance of " + LunarOccultations.Prediction[i].StarId.Trim());
					}
					stringBuilder3.AppendFormat(" to Terminator = {0,1:F1}", LunarOccultations.Prediction[i].LimbDistance);
					stringBuilder3.AppendFormat("\"; to 3km sunlit peak = {0,1:F1}\"", LunarOccultations.Prediction[i].MountainDistance);
					lstPrediction.get_Items().Add((object)stringBuilder3.ToString());
					lstPrediction_Index.Add(i);
				}
				if (!(LunarOccultations.Prediction[i].Durn > 0.0 && flag11) || !(!LunarOccultations.Prediction[i].EventPhase.ToUpper().Contains("G") & !LunarOccultations.Prediction[i].EventPhase.ToUpper().Contains("M")))
				{
					continue;
				}
				double num5 = 0.0;
				double num6 = 0.0;
				double num7 = 0.0;
				double num8 = 0.0;
				string text2 = LunarOccultations.Prediction[i].StarId.Trim();
				if ((LunarOccultations.Prediction[i].Planet > 0) & (LunarOccultations.Prediction[i].Planet < 5))
				{
					string text3 = string.Format(": diam = {0,3:f1}\"", LunarOccultations.Prediction[i].DiaPlanet) + "; %illum = " + string.Format("{0,1:F1}", LunarOccultations.Prediction[i].IllumPlanet * 100.0) + "%; PA bright limb = " + string.Format("{0,1:F1}", LunarOccultations.Prediction[i].LimbPlanet);
					bool DarkLimbClosest = true;
					bool flag13 = false;
					if (LunarOccultations.Prediction[i].EventPhase.ToUpper().Contains("D"))
					{
						flag13 = true;
					}
					string text4 = "; Cusp";
					if (LunarOccultations.Prediction[i].PhaseAngle < 90.0)
					{
						text4 = "; Terminator";
					}
					double num9 = Utilities.Planet_MaxTerminatorHeightAboveReferenceLine(LunarOccultations.Prediction[i].PhaseAngle, LunarOccultations.Prediction[i].PABrightLimb, LunarOccultations.Prediction[i].PA, out DarkLimbClosest);
					int num10 = 1;
					num10 = (DarkLimbClosest ? (flag13 ? 1 : (-1)) : ((!flag13) ? 1 : (-1)));
					num6 = (double)(-num10) * LunarOccultations.Prediction[i].Durn / 2.0;
					num7 = (0.0 - num9) * num6;
					num8 = (double)num10 * LunarOccultations.Prediction[i].Durn / 2.0;
					if (num10 == 1)
					{
						lstPrediction.get_Items().Add((object)("   " + text2 + " contacts: Bright limb" + OffsetTimes(LunarOccultations.Prediction[i].JD, num6, SingleTimeOnly: true) + text4 + OffsetTimes(LunarOccultations.Prediction[i].JD, num7, SingleTimeOnly: true) + "; Dark limb" + OffsetTimes(LunarOccultations.Prediction[i].JD, num8, SingleTimeOnly: true) + text3));
					}
					else
					{
						lstPrediction.get_Items().Add((object)("   " + text2 + " contacts: Dark limb" + OffsetTimes(LunarOccultations.Prediction[i].JD, num8, SingleTimeOnly: true) + text4 + OffsetTimes(LunarOccultations.Prediction[i].JD, num7, SingleTimeOnly: true) + "; Bright limb" + OffsetTimes(LunarOccultations.Prediction[i].JD, num6, SingleTimeOnly: true) + text3));
					}
					lstPrediction_Index.Add(i);
				}
				else if (LunarOccultations.Prediction[i].Planet == 5)
				{
					if (text2 == "Jupiter")
					{
						Utilities.Planet_MaxLimbHeightAboveReferenceLine(0.935, LunarOccultations.Prediction[i].PAPole_deg, LunarOccultations.Prediction[i].PA);
					}
					num5 = 1.0 * LunarOccultations.Prediction[i].Durn / 2.0;
					lstPrediction.get_Items().Add((object)("   " + text2 + " limb contacts offset by ±" + string.Format("{0,1:F1}", num5) + " secs, at" + OffsetTimes(LunarOccultations.Prediction[i].JD, num5, SingleTimeOnly: false) + "  Both contacts are against the bright limb of " + text2));
					lstPrediction_Index.Add(i);
				}
				else if (LunarOccultations.Prediction[i].Planet == 6)
				{
					double num11 = 1.0;
					if (text2 == "Saturn")
					{
						num5 = 2.265 * Utilities.Planet_MaxLimbHeightAboveReferenceLine(Math.Sin(LunarOccultations.Prediction[i].EarthDec_deg / (180.0 / Math.PI)), LunarOccultations.Prediction[i].PAPole_deg, LunarOccultations.Prediction[i].PA) * LunarOccultations.Prediction[i].Durn / 2.0;
						lstPrediction.get_Items().Add((object)("   Saturn ring contacts offset by ±" + string.Format("{0,1:F1}", num5) + " secs, at" + OffsetTimes(LunarOccultations.Prediction[i].JD, num5, SingleTimeOnly: false)));
						lstPrediction_Index.Add(i);
					}
					if (text2 == "Saturn")
					{
						num11 = Utilities.Planet_MaxLimbHeightAboveReferenceLine(0.903, LunarOccultations.Prediction[i].PAPole_deg, LunarOccultations.Prediction[i].PA);
					}
					num5 = num11 * LunarOccultations.Prediction[i].Durn / 2.0;
					lstPrediction.get_Items().Add((object)("   " + text2 + " limb contacts offset by ±" + string.Format("{0,1:F1}", num5) + " secs, at" + OffsetTimes(LunarOccultations.Prediction[i].JD, num5, SingleTimeOnly: false) + "  Both contacts are against the bright limb of " + text2));
					lstPrediction_Index.Add(i);
				}
				else if (LunarOccultations.Prediction[i].Planet > 6)
				{
					num5 = LunarOccultations.Prediction[i].Durn / 2.0;
					lstPrediction.get_Items().Add((object)("   " + text2 + " limb contacts offset by ±" + string.Format("{0,1:F1}", num5) + " secs, at" + OffsetTimes(LunarOccultations.Prediction[i].JD, num5, SingleTimeOnly: false) + "  Both contacts are against the bright limb of " + text2));
					lstPrediction_Index.Add(i);
				}
			}
			Application.DoEvents();
			OutputIsGraze = false;
			OutputIsMulti = false;
		}

		private string OffsetTimes(double EventJD, double OffsetInSecs, bool SingleTimeOnly)
		{
			string text = "";
			string text2 = "";
			int decimalLength = 1;
			if (Math.Abs(OffsetInSecs) > 5.0)
			{
				decimalLength = 0;
			}
			double num = Math.Floor(EventJD - 0.5) + 0.5;
			double num2 = EventJD - num;
			text = Utilities.DEGtoDMS(num2 * 24.0 - OffsetInSecs / 3600.0, 3, decimalLength, MinutesOnly: false);
			text2 = Utilities.DEGtoDMS(num2 * 24.0 + OffsetInSecs / 3600.0, 3, decimalLength, MinutesOnly: false);
			if (SingleTimeOnly)
			{
				return text2;
			}
			return text + " and" + text2;
		}

		private void cmdComputeGrazes_Click(object sender, EventArgs e)
		{
			ComputeGrazes();
		}

		private void ComputeGrazes()
		{
			if (UsingSingleSite)
			{
				Set_CurrentSite_for_Singles();
			}
			if (!CurrentElementsAreValid)
			{
				double jD = Utilities.JD_from_Date((int)updnLunarYearStart.get_Value(), ((ListControl)cmbLunarMonthStart).get_SelectedIndex() + 1, ((ListControl)cmbLunarDayStart).get_SelectedIndex() + 1);
				lstPrediction.get_Items().Clear();
				LunarOccultations.Prediction.Clear();
				LunarOccultations.InitialiseSearch();
				CancelFlag = false;
				((Control)lblCurrentDate).set_Text("[" + Utilities.Date_from_JD(jD, 0) + "]");
				LunarOccultations.NewDate(jD, StartHour, chkFilter.get_Checked(), NorthLimit, SouthLimit, EastLimit, WestLimit);
				double readMagLimit = LunarOccultations.GetReadMagLimit(MaxAperture, (double)CurrentSite.MagCorrection + GlobalMagLimitAdjustment);
				LunarOccultations.ScanXZFile(StarCatalogue, NorthLimit, SouthLimit, readMagLimit, DoublesOnly: false, Auto: false);
				CurrentElementsAreValid = true;
			}
			SingleDayComputation = true;
			SetContextMenuItems(ToReturn: false, SingleDayComputation);
			LunarOccultations.GrazePrediction(GetStar: true, cmbSiteFiles.get_Items().get_Item(((ListControl)cmbSiteFiles).get_SelectedIndex()).ToString());
			DisplayGrazes();
		}

		private void DisplayGrazes()
		{
			if ((LunarOccultations.GrazeHeader.Count < 1) | (LunarOccultations.GrazePredictionLines.Count < 1))
			{
				lstPrediction.get_Items().Add((object)"No graze");
				return;
			}
			int num = 1;
			lstPrediction.get_Items().Clear();
			lstPrediction_Index.Clear();
			for (int i = 0; i < LunarOccultations.GrazeHeader.Count; i++)
			{
				lstPrediction.get_Items().Add((object)LunarOccultations.GrazeHeader[i]!.ToString());
				lstPrediction_Index.Add(-1);
			}
			if (LunarOccultations.GrazePredictionLines[0].GrazeInnerOuterLimit == -1)
			{
				num = -1;
				lstPrediction.get_Items().Add((object)"");
				lstPrediction_Index.Add(-1);
				lstPrediction.get_Items().Add((object)(" Inner limit - " + LunarOccultations.GrazePredictionLines[0].StarId.Trim() + " fully obscured"));
				lstPrediction_Index.Add(-1);
			}
			for (int i = 0; i < LunarOccultations.GrazePredictionLines.Count; i++)
			{
				if (LunarOccultations.GrazePredictionLines[i].GrazeInnerOuterLimit != num)
				{
					num = 1;
					lstPrediction.get_Items().Add((object)"");
					lstPrediction_Index.Add(-1);
					lstPrediction.get_Items().Add((object)(" Outer limit - " + LunarOccultations.GrazePredictionLines[0].StarId.Trim() + " just touches the moon"));
					lstPrediction_Index.Add(-1);
				}
				lstPrediction.get_Items().Add((object)LunarOccultations.GrazePredictionLines[i].ToString());
				lstPrediction_Index.Add(i);
			}
			for (int i = 0; i < LunarOccultations.GrazeFooter.Count; i++)
			{
				lstPrediction.get_Items().Add((object)LunarOccultations.GrazeFooter[i]!.ToString());
			}
			lstPrediction.get_Items().Add((object)"Results of Observer Scan                                    UT");
			lstPrediction.get_Items().Add((object)"Site                              Long.   Lat.   Dist.    h  m  s");
			for (int i = 0; i < LunarOccultations.GrazeSiteScan.Count; i++)
			{
				lstPrediction.get_Items().Add((object)LunarOccultations.GrazeSiteScan[i]!.ToString());
			}
			lstPrediction.get_Items().Add((object)"");
			lstPrediction_Index.Add(-1);
			OutputIsGraze = true;
			OutputIsMulti = false;
			SetContextMenuItems(ToReturn: true, SingleDayComputation);
		}

		internal void SetStarCats()
		{
			((Control)optXZ).set_Enabled(File.Exists(StarCat0));
			((Control)optZC).set_Enabled(File.Exists(StarCat1));
			((Control)optXZ3).set_Enabled(File.Exists(StarCat2));
			((Control)optXZ6).set_Enabled(File.Exists(StarCat3));
			((Control)optXZ9).set_Enabled(File.Exists(StarCat4));
		}

		private void cmbSiteFiles_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (chkFilter.get_Checked())
			{
				CurrentElementsAreValid = false;
			}
			EastLimit = -180.0;
			WestLimit = 180.0;
			NorthLimit = -90.0;
			SouthLimit = 90.0;
			MaxAperture = 0.0;
			AllSites.Clear();
			StreamReader streamReader = new StreamReader(AppPath + "\\sites\\" + cmbSiteFiles.get_Items().get_Item(((ListControl)cmbSiteFiles).get_SelectedIndex()));
			do
			{
				S = new Sites();
				S.Read_SiteFile(streamReader.ReadLine());
				AllSites.Add(S);
				if (S.Longitude > EastLimit)
				{
					EastLimit = S.Longitude;
				}
				if (S.Longitude < WestLimit)
				{
					WestLimit = S.Longitude;
				}
				if (S.Latitude > NorthLimit)
				{
					NorthLimit = S.Latitude;
				}
				if (S.Latitude < SouthLimit)
				{
					SouthLimit = S.Latitude;
				}
				if ((double)S.ApertureCM > MaxAperture)
				{
					MaxAperture = S.ApertureCM;
				}
			}
			while (!streamReader.EndOfStream);
			streamReader.Close();
			EastLimit += 2.0;
			WestLimit -= 2.0;
			NorthLimit += 2.0;
			SouthLimit -= 2.0;
			((Control)lblRange).set_Text($"{WestLimit:F1} to {EastLimit:F1}, {SouthLimit:F1} to {NorthLimit:F1}");
			Sites.SortField = 0;
			AllSites.Sort();
			cmbSite.get_Items().Clear();
			for (int i = 0; i < AllSites.Count; i++)
			{
				cmbSite.get_Items().Add((object)AllSites[i].Name);
				if (AllSites[i].Name == Settings.Default.ObserverLunarPredict)
				{
					((ListControl)cmbSite).set_SelectedIndex(i);
				}
			}
			if ((((ListControl)cmbSite).get_SelectedIndex() < 0) & (cmbSite.get_Items().get_Count() > 0))
			{
				((ListControl)cmbSite).set_SelectedIndex(0);
			}
			if (FormCreated)
			{
				Settings.Default.SiteFileLunarPredict = cmbSiteFiles.get_Items().get_Item(((ListControl)cmbSiteFiles).get_SelectedIndex()).ToString();
			}
		}

		private void cmbSite_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (FormCreated)
			{
				Settings.Default.ObserverLunarPredict = cmbSite.get_Items().get_Item(((ListControl)cmbSite).get_SelectedIndex()).ToString();
				CurrentSite = AllSites[((ListControl)cmbSite).get_SelectedIndex()];
			}
		}

		private void optXZ_CheckedChanged(object sender, EventArgs e)
		{
			StarCatalogue = StarCat0;
			LunarOccultations.InitialiseStarCatIndexArray(StarCatalogue);
			CurrentElementsAreValid = false;
		}

		private void optZC_CheckedChanged(object sender, EventArgs e)
		{
			StarCatalogue = StarCat1;
			LunarOccultations.InitialiseStarCatIndexArray(StarCatalogue);
			CurrentElementsAreValid = false;
		}

		private void optXZ3_CheckedChanged(object sender, EventArgs e)
		{
			StarCatalogue = StarCat2;
			LunarOccultations.InitialiseStarCatIndexArray(StarCatalogue);
			CurrentElementsAreValid = false;
		}

		private void optXZ6_CheckedChanged(object sender, EventArgs e)
		{
			StarCatalogue = StarCat3;
			LunarOccultations.InitialiseStarCatIndexArray(StarCatalogue);
			CurrentElementsAreValid = false;
		}

		private void optXZ9_CheckedChanged(object sender, EventArgs e)
		{
			StarCatalogue = StarCat4;
			LunarOccultations.InitialiseStarCatIndexArray(StarCatalogue);
			CurrentElementsAreValid = false;
		}

		private void updnLunarYearStart_ValueChanged(object sender, EventArgs e)
		{
			if (((ListControl)cmbLunarMonthStart).get_SelectedIndex() == 1)
			{
				SetStartDays();
			}
			JDFirstDate = SetDefaultEndDate();
			SetSingleDayButtons(singleday: true);
			if (updnLunarYearStart.get_Value() < 1920m)
			{
				chkAsteroids.set_Checked(false);
			}
		}

		private void cmbLunarMonthStart_SelectedIndexChanged(object sender, EventArgs e)
		{
			SetStartDays();
			JDFirstDate = SetDefaultEndDate();
			SetSingleDayButtons(singleday: true);
		}

		private void SetStartDays()
		{
			if (!DatesSet)
			{
				return;
			}
			int num = DaysInMonth[((ListControl)cmbLunarMonthStart).get_SelectedIndex()];
			int selectedIndex = ((ListControl)cmbLunarDayStart).get_SelectedIndex();
			if (((ListControl)cmbLunarMonthStart).get_SelectedIndex() == 1)
			{
				if ((updnLunarYearStart.get_Value() > 1600m) & (updnLunarYearStart.get_Value() % 400m == 0m))
				{
					num = 29;
				}
				else if (updnLunarYearStart.get_Value() % 100m == 0m)
				{
					num = 28;
				}
				else if (updnLunarYearStart.get_Value() % 4m == 0m)
				{
					num = 29;
				}
			}
			cmbLunarDayStart.get_Items().Clear();
			for (int i = 1; i <= num; i++)
			{
				cmbLunarDayStart.get_Items().Add((object)i);
			}
			if (selectedIndex < cmbLunarDayStart.get_Items().get_Count())
			{
				((ListControl)cmbLunarDayStart).set_SelectedIndex(selectedIndex);
			}
			else
			{
				((ListControl)cmbLunarDayStart).set_SelectedIndex(cmbLunarDayStart.get_Items().get_Count() - 1);
			}
		}

		private void cmbLunarDayStart_SelectedIndexChanged(object sender, EventArgs e)
		{
			JDFirstDate = SetDefaultEndDate();
			SetSingleDayButtons(singleday: true);
		}

		private void updnLunarYearEnd_ValueChanged(object sender, EventArgs e)
		{
			if (((ListControl)cmbLunarMonthEnd).get_SelectedIndex() == 1)
			{
				SetEndDays();
			}
			SetSingleDayButtons(JDFirstDate == GetEndDate());
		}

		private void SetEndDays()
		{
			if (!DatesSet)
			{
				return;
			}
			int num = DaysInMonth[((ListControl)cmbLunarMonthEnd).get_SelectedIndex()];
			int selectedIndex = ((ListControl)cmbLunarDayEnd).get_SelectedIndex();
			if (((ListControl)cmbLunarMonthEnd).get_SelectedIndex() == 1)
			{
				if ((updnLunarYearStart.get_Value() > 1600m) & (updnLunarYearEnd.get_Value() % 400m == 0m))
				{
					num = 29;
				}
				else if (updnLunarYearEnd.get_Value() % 100m == 0m)
				{
					num = 28;
				}
				else if (updnLunarYearEnd.get_Value() % 4m == 0m)
				{
					num = 29;
				}
			}
			cmbLunarDayEnd.get_Items().Clear();
			for (int i = 1; i <= num; i++)
			{
				cmbLunarDayEnd.get_Items().Add((object)i);
			}
			if (selectedIndex < cmbLunarDayEnd.get_Items().get_Count())
			{
				((ListControl)cmbLunarDayEnd).set_SelectedIndex(selectedIndex);
			}
			else
			{
				((ListControl)cmbLunarDayEnd).set_SelectedIndex(cmbLunarDayEnd.get_Items().get_Count() - 1);
			}
		}

		private void cmbLunarMonthEnd_SelectedIndexChanged(object sender, EventArgs e)
		{
			SetEndDays();
			SetSingleDayButtons(JDFirstDate == GetEndDate());
		}

		private void cmbLunarDayEnd_SelectedIndexChanged(object sender, EventArgs e)
		{
			SetSingleDayButtons(JDFirstDate == GetEndDate());
		}

		private void cmdFullYear_Click(object sender, EventArgs e)
		{
			((ListControl)cmbLunarMonthStart).set_SelectedIndex(0);
			((ListControl)cmbLunarDayStart).set_SelectedIndex(0);
			updnLunarYearEnd.set_Value(updnLunarYearStart.get_Value());
			((ListControl)cmbLunarMonthEnd).set_SelectedIndex(11);
			((ListControl)cmbLunarDayEnd).set_SelectedIndex(30);
		}

		private void cmdFullMonth_Click(object sender, EventArgs e)
		{
			((ListControl)cmbLunarDayStart).set_SelectedIndex(0);
			updnLunarYearEnd.set_Value(updnLunarYearStart.get_Value());
			((ListControl)cmbLunarMonthEnd).set_SelectedIndex(((ListControl)cmbLunarMonthStart).get_SelectedIndex());
			((ListControl)cmbLunarDayEnd).set_SelectedIndex(cmbLunarDayEnd.get_Items().get_Count() - 1);
		}

		private void cmdOneDay_Click(object sender, EventArgs e)
		{
			updnLunarYearEnd.set_Value(updnLunarYearStart.get_Value());
			((ListControl)cmbLunarMonthEnd).set_SelectedIndex(((ListControl)cmbLunarMonthStart).get_SelectedIndex());
			((ListControl)cmbLunarDayEnd).set_SelectedIndex(((ListControl)cmbLunarDayStart).get_SelectedIndex());
		}

		private void button1_Click(object sender, EventArgs e)
		{
			DateTime now = DateTime.Now;
			updnLunarYearStart.set_Value((decimal)now.ToUniversalTime().Year);
			((ListControl)cmbLunarMonthStart).set_SelectedIndex(now.ToUniversalTime().Month - 1);
			((ListControl)cmbLunarDayStart).set_SelectedIndex(now.ToUniversalTime().Day - 1);
			cmdOneDay_Click(sender, e);
		}

		private double SetDefaultEndDate()
		{
			CurrentElementsAreValid = false;
			int Year = (int)updnLunarYearStart.get_Value();
			int Month = ((ListControl)cmbLunarMonthStart).get_SelectedIndex() + 1;
			double day = (double)((ListControl)cmbLunarDayStart).get_SelectedIndex() + 1.0;
			double num = Utilities.JD_from_Date(Year, Month, day);
			Utilities.Date_from_JD(num, out Year, out Month, out day);
			updnLunarYearEnd.set_Value((decimal)Year);
			((ListControl)cmbLunarMonthEnd).set_SelectedIndex(Month - 1);
			((ListControl)cmbLunarDayEnd).set_SelectedIndex((int)(day - 1.0));
			return num;
		}

		private double GetEndDate()
		{
			int year = (int)updnLunarYearEnd.get_Value();
			int month = ((ListControl)cmbLunarMonthEnd).get_SelectedIndex() + 1;
			double day = (double)((ListControl)cmbLunarDayEnd).get_SelectedIndex() + 1.0;
			return Utilities.JD_from_Date(year, month, day);
		}

		private void SetSingleDayButtons(bool singleday)
		{
			if (singleday)
			{
				((Control)grpAnyWhere).set_Enabled(true);
			}
			else
			{
				((Control)grpAnyWhere).set_Enabled(false);
			}
		}

		internal void SetDatesExternally(double JD)
		{
			Utilities.Date_from_JD(JD, out var Year, out var Month, out var day);
			NumericUpDown obj = updnLunarYearStart;
			decimal value;
			updnLunarYearEnd.set_Value(value = Year);
			obj.set_Value(value);
			ComboBox obj2 = cmbLunarMonthStart;
			int selectedIndex;
			((ListControl)cmbLunarMonthEnd).set_SelectedIndex(selectedIndex = Month - 1);
			((ListControl)obj2).set_SelectedIndex(selectedIndex);
			ComboBox obj3 = cmbLunarDayStart;
			((ListControl)cmbLunarDayEnd).set_SelectedIndex(selectedIndex = (int)day - 1);
			((ListControl)obj3).set_SelectedIndex(selectedIndex);
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents());
		}

		private void copySingleLineeventToolStripMenuItem_Click(object sender, EventArgs e)
		{
			string text = "";
			Clipboard.Clear();
			if ((((ListControl)lstPrediction).get_SelectedIndex() < 0) | OutputIsGraze | OutputIsMulti)
			{
				return;
			}
			int num = (int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()];
			if (num < 0)
			{
				return;
			}
			int num2 = ((ListControl)lstPrediction).get_SelectedIndex() - 6;
			int num3 = ((ListControl)lstPrediction).get_SelectedIndex() + 6;
			if (num2 < 0)
			{
				num2 = 0;
			}
			if (num3 > lstPrediction.get_Items().get_Count())
			{
				num3 = lstPrediction.get_Items().get_Count();
			}
			if (lstPrediction.get_Items().get_Count() < 5)
			{
				return;
			}
			for (int i = 0; i < 5; i++)
			{
				text = text + lstPrediction.get_Items().get_Item(i).ToString() + "\r\n";
			}
			for (int j = num2; j < num3; j++)
			{
				if (num == (int)lstPrediction_Index[j])
				{
					text = text + lstPrediction.get_Items().get_Item(j).ToString() + "\r\n";
				}
			}
			if (text.Length > 0)
			{
				Clipboard.SetText(text);
			}
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintPreviewText(CollectEvents());
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(CollectEvents());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_LunarPredictions = Output.SaveAppendPredictionText(CollectEvents(), cmbSite.get_Items().get_Item(((ListControl)cmbSite).get_SelectedIndex())?.ToString() + " Lunar", Settings.Default.Save_LunarPredictions);
		}

		private string CollectEvents()
		{
			if (lstPrediction.get_Items().get_Count() < 0)
			{
				return "";
			}
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstPrediction.get_Items().get_Count();
			for (int i = 0; i < count; i++)
			{
				stringBuilder.AppendLine(lstPrediction.get_Items().get_Item(i).ToString());
			}
			stringBuilder.AppendLine("");
			return stringBuilder.ToString();
		}

		private void lstPrediction_DoubleClick(object sender, EventArgs e)
		{
			grazeProfileToolStripMenuItem_Click(sender, e);
		}

		private void cmdWorldMap_Click(object sender, EventArgs e)
		{
			if (UsingSingleSite)
			{
				Set_CurrentSite_for_Singles();
			}
			if (!CurrentElementsAreValid)
			{
				double jD = Utilities.JD_from_Date((int)updnLunarYearStart.get_Value(), ((ListControl)cmbLunarMonthStart).get_SelectedIndex() + 1, ((ListControl)cmbLunarDayStart).get_SelectedIndex() + 1);
				lstPrediction.get_Items().Clear();
				LunarOccultations.Prediction.Clear();
				LunarOccultations.InitialiseSearch();
				CancelFlag = false;
				((Control)lblCurrentDate).set_Text("[" + Utilities.Date_from_JD(jD, 0) + "]");
				LunarOccultations.NewDate(jD, StartHour, chkFilter.get_Checked(), NorthLimit, SouthLimit, EastLimit, WestLimit);
				double readMagLimit = LunarOccultations.GetReadMagLimit(MaxAperture, (double)CurrentSite.MagCorrection + GlobalMagLimitAdjustment);
				LunarOccultations.ScanXZFile(StarCatalogue, NorthLimit, SouthLimit, readMagLimit, DoublesOnly: false, Auto: false);
				CurrentElementsAreValid = true;
			}
			SingleDayComputation = true;
			SetContextMenuItems(ToReturn: false, SingleDayComputation);
			LunarOccultations.WorldMapPrediction();
			((Control)cmdCancel).set_Visible(false);
		}

		private void cmdWorldMap_SameDay_Click(object sender, EventArgs e)
		{
			SingleDayComputation = true;
			SetContextMenuItems(ToReturn: false, SingleDayComputation);
			LunarOccultations.WorldMapPrediction();
		}

		private void cmdMultiLocation_Click(object sender, EventArgs e)
		{
			if (!CurrentElementsAreValid)
			{
				double jD = Utilities.JD_from_Date((int)updnLunarYearStart.get_Value(), ((ListControl)cmbLunarMonthStart).get_SelectedIndex() + 1, ((ListControl)cmbLunarDayStart).get_SelectedIndex() + 1);
				lstPrediction.get_Items().Clear();
				LunarOccultations.Prediction.Clear();
				LunarOccultations.InitialiseSearch();
				CancelFlag = false;
				((Control)lblCurrentDate).set_Text("[" + Utilities.Date_from_JD(jD, 0) + "]");
				LunarOccultations.NewDate(jD, StartHour, chkFilter.get_Checked(), NorthLimit, SouthLimit, EastLimit, WestLimit);
				double readMagLimit = LunarOccultations.GetReadMagLimit(MaxAperture, (double)CurrentSite.MagCorrection + GlobalMagLimitAdjustment);
				LunarOccultations.ScanXZFile(StarCatalogue, NorthLimit, SouthLimit, readMagLimit, DoublesOnly: false, Auto: false);
				CurrentElementsAreValid = true;
			}
			SingleDayComputation = true;
			SetContextMenuItems(ToReturn: false, SingleDayComputation);
			LunarOccultations.MultiLocationPredictions(cmbSiteFiles.get_Items().get_Item(((ListControl)cmbSiteFiles).get_SelectedIndex()).ToString(), GetStar: true);
			if (!chkLong.get_Checked() & !chkShort.get_Checked())
			{
				CheckBox obj = chkLong;
				bool @checked;
				chkShort.set_Checked(@checked = true);
				obj.set_Checked(@checked);
			}
			if (DisplayMultiLocationPrediction(chkLong.get_Checked(), chkShort.get_Checked()))
			{
				OutputIsMulti = true;
			}
		}

		private void multiSitePredictionToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (!((((ListControl)lstPrediction).get_SelectedIndex() < 0) | OutputIsGraze | OutputIsMulti) && (int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()] >= 0)
			{
				LunarOccultations.MultiLocationStar = LunarOccultations.Prediction[(int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()]].ElementRecordNumber;
				LunarOccultations.MultiLocationPredictions(cmbSiteFiles.get_Items().get_Item(((ListControl)cmbSiteFiles).get_SelectedIndex()).ToString(), GetStar: false);
				bool @checked;
				if (!chkLong.get_Checked() & !chkShort.get_Checked())
				{
					CheckBox obj = chkLong;
					chkShort.set_Checked(@checked = true);
					obj.set_Checked(@checked);
				}
				DisplayMultiLocationPrediction(chkLong.get_Checked(), chkShort.get_Checked());
				ToolStripMenuItem obj2 = worldMapToolStripMenuItem;
				ToolStripMenuItem obj3 = displayStarDetailsToolStripMenuItem;
				bool flag;
				((ToolStripItem)multiSitePredictionToolStripMenuItem).set_Visible(flag = false);
				((ToolStripItem)obj3).set_Visible(@checked = flag);
				((ToolStripItem)obj2).set_Visible(@checked);
				((ToolStripItem)computeGrazeToolStripMenuItem).set_Visible(false);
				ToolStripMenuItem obj4 = reListOccultationsToolStripMenuItem;
				((ToolStripItem)moonMapToolStripMenuItem).set_Visible(@checked = true);
				((ToolStripItem)obj4).set_Visible(@checked);
				OutputIsMulti = true;
			}
		}

		private bool DisplayMultiLocationPrediction(bool includeLong, bool IncludeShort)
		{
			int multiLocationStar = LunarOccultations.MultiLocationStar;
			if (multiLocationStar < 0)
			{
				return false;
			}
			bool flag = LunarOccultations.LongD.Count > 0;
			bool flag2 = LunarOccultations.LongR.Count > 0;
			if (!flag && !flag2)
			{
				return false;
			}
			lstPrediction.get_Items().Clear();
			lstPrediction.get_Items().Add((object)("Occultation predictions of " + LunarOccultations.Elements[multiLocationStar].StarId + $"   Magnitude {LunarOccultations.Elements[multiLocationStar].Mv:F1}"));
			lstPrediction.get_Items().Add((object)("Date " + LunarOccultations.MultiLocDate));
			lstPrediction.get_Items().Add((object)"");
			lstPrediction.get_Items().Add((object)("Moon: % illumination = " + LunarOccultations.MultiLoc_Illum + ",  Solar elongation = " + LunarOccultations.MultiLoc_Elong));
			if (includeLong)
			{
				if (flag)
				{
					lstPrediction.get_Items().Add((object)"");
					lstPrediction.get_Items().Add((object)"");
					lstPrediction.get_Items().Add((object)"      Disappearance");
					lstPrediction.get_Items().Add((object)"                                       U.T.  Sun Moon     CA  PA  WA   a    b");
					lstPrediction.get_Items().Add((object)"Location                             h  m  s Alt Alt Az    o   o   o  m/o  m/o");
					lstPrediction.get_Items().Add((object)"");
					for (int i = 0; i < LunarOccultations.LongD.Count; i++)
					{
						lstPrediction.get_Items().Add((object)LunarOccultations.LongD[i]!.ToString());
					}
				}
				if (flag2)
				{
					lstPrediction.get_Items().Add((object)"");
					lstPrediction.get_Items().Add((object)"");
					lstPrediction.get_Items().Add((object)"      Reappearance");
					lstPrediction.get_Items().Add((object)"                                       U.T.  Sun Moon     CA  PA  WA   a    b");
					lstPrediction.get_Items().Add((object)"Location                             h  m  s Alt Alt Az    o   o   o  m/o  m/o");
					lstPrediction.get_Items().Add((object)"");
					for (int j = 0; j < LunarOccultations.LongR.Count; j++)
					{
						lstPrediction.get_Items().Add((object)LunarOccultations.LongR[j]!.ToString());
					}
				}
			}
			if (IncludeShort)
			{
				if (flag)
				{
					lstPrediction.get_Items().Add((object)"");
					lstPrediction.get_Items().Add((object)"");
					lstPrediction.get_Items().Add((object)"      Disappearance");
					lstPrediction.get_Items().Add((object)"                                       U.T.  Sun Moon CA  PA ");
					lstPrediction.get_Items().Add((object)"Location                             h  m  s Alt Alt   o   o ");
					lstPrediction.get_Items().Add((object)"");
					for (int k = 0; k < LunarOccultations.ShortD.Count; k++)
					{
						lstPrediction.get_Items().Add((object)LunarOccultations.ShortD[k]!.ToString());
					}
				}
				if (flag2)
				{
					lstPrediction.get_Items().Add((object)"");
					lstPrediction.get_Items().Add((object)"");
					lstPrediction.get_Items().Add((object)"      Reappearance");
					lstPrediction.get_Items().Add((object)"                                       U.T.  Sun Moon CA  PA ");
					lstPrediction.get_Items().Add((object)"Location                             h  m  s Alt Alt   o   o ");
					lstPrediction.get_Items().Add((object)"");
					for (int l = 0; l < LunarOccultations.ShortR.Count; l++)
					{
						lstPrediction.get_Items().Add((object)LunarOccultations.ShortR[l]!.ToString());
					}
				}
			}
			if (flag || flag2)
			{
				lstPrediction.get_Items().Add((object)"");
				lstPrediction.get_Items().Add((object)"");
				lstPrediction.get_Items().Add((object)"");
				lstPrediction.get_Items().Add((object)"      Sites");
				lstPrediction.get_Items().Add((object)"Location                             E. Long    Latitude");
				lstPrediction.get_Items().Add((object)"");
				for (int m = 0; m < LunarOccultations.MultiSites.Count; m++)
				{
					lstPrediction.get_Items().Add((object)LunarOccultations.MultiSites[m]!.ToString());
				}
			}
			SetContextMenuItems(ToReturn: true, SingleDayComputation);
			((ToolStripItem)moonMapToolStripMenuItem).set_Visible(true);
			return true;
		}

		private void lstPrediction_MouseDown(object sender, MouseEventArgs e)
		{
			//IL_0010: Unknown result type (might be due to invalid IL or missing references)
			//IL_001a: Invalid comparison between Unknown and I4
			if (lstPrediction_Index.Count >= 1 && (int)e.get_Button() == 2097152)
			{
				int num = lstPrediction.IndexFromPoint(e.get_X(), e.get_Y());
				if (num >= 0 && num < lstPrediction.get_Items().get_Count())
				{
					((ListControl)lstPrediction).set_SelectedIndex(num);
				}
				((Control)lstPrediction).Refresh();
				if ((!OutputIsGraze & !OutputIsMulti) && !((((ListControl)lstPrediction).get_SelectedIndex() < 0) | OutputIsGraze | OutputIsMulti) && (int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()] >= 0)
				{
					((ToolStrip)contextMenuStrip1).get_Items().get_Item(13).set_Enabled(LunarOccultations.Prediction[(int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()]].DoubleDetailsExist | LunarOccultations.Prediction[(int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()]].VariableDetailsExist);
					((ToolStrip)contextMenuStrip1).get_Items().get_Item(14).set_Enabled(LunarOccultations.Prediction[(int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()]].LightCurveAvailable);
					((ToolStrip)contextMenuStrip1).get_Items().get_Item(22).set_Text("Compute predictions for " + LunarOccultations.Prediction[(int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()]].EventDate());
				}
			}
		}

		private void displayStarDetailsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (!((((ListControl)lstPrediction).get_SelectedIndex() < 0) | OutputIsGraze | OutputIsMulti) && (int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()] >= 0)
			{
				LunarOccultations.StarCatalogueShowForm();
				LunarOccultations.StarCatDetails.updnYear.set_Value((decimal)(2000.0 + (LunarOccultations.Prediction[(int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()]].JD - 2451545.0) / 365.25));
				LunarOccultations.StarCatDetails.ExternalDisplayXZStar(LunarOccultations.Prediction[(int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()]].StarId);
			}
		}

		private void doubleStarDetailsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (!((((ListControl)lstPrediction).get_SelectedIndex() < 0) | OutputIsGraze | OutputIsMulti) && (int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()] >= 0)
			{
				Interferometric_Plus_WDS.Find_WDS_IF_Matches(LunarOccultations.Prediction[(int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()]].RA2000 * (180.0 / Math.PI) / 15.0, LunarOccultations.Prediction[(int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()]].Dec2000 * (180.0 / Math.PI), HighPrecision: false, ShowResults: true, out WDS_Interferometer_Display.VariableID);
			}
		}

		private void moonMapToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayMoonMap();
		}

		private void DisplayMoonMap()
		{
			if (OutputIsGraze)
			{
				return;
			}
			if (OutputIsMulti)
			{
				if (OutputIsMulti & (LunarOccultations.MultiPrediction.Count < 0))
				{
					return;
				}
			}
			else
			{
				if (((ListControl)lstPrediction).get_SelectedIndex() < 0 || (int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()] < 0)
				{
					return;
				}
				LunarOccultations.MoonMapSelectedStar = (int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()];
			}
			MoonMap.MultiSite = OutputIsMulti;
			LunarOccultations.MoonMapShowForm();
		}

		private void worldMapToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (!((((ListControl)lstPrediction).get_SelectedIndex() < 0) | OutputIsGraze | OutputIsMulti) && (int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()] >= 0)
			{
				LunarOccultations.ShowOccultationWorld();
				LunarOccultations.MapSelectedStar = LunarOccultations.Prediction[(int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()]].ElementRecordNumber;
				LunarOccultations.PlotOccultationWorld(BWFlag: false);
			}
		}

		private void viewInGoogleEarthToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
			}
			else if (OutputIsGraze)
			{
				string text = LunarOccultations.EventID(LunarOccultations.GrazeSelectedStar);
				LunarOccultations.CreateGoogleEarthKMZFile_ForGrazes(AppPath + "\\Predictions\\LunarGrazePath.KML", "Graze of " + text, AutoOpenFile: true, View: true, "");
			}
			else if (!((((ListControl)lstPrediction).get_SelectedIndex() < 0) | OutputIsGraze | OutputIsMulti) && (int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()] >= 0)
			{
				LunarOccultations.MapSelectedStar = LunarOccultations.Prediction[(int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()]].ElementRecordNumber;
				string text2 = LunarOccultations.EventID(LunarOccultations.MapSelectedStar);
				LunarOccultations.Compute_StartEnd_at_RiseSet_Curves();
				LunarOccultations.Compute_Limit_Lines();
				LunarOccultations.CreateGoogleEarthKMZFile_ForWorld(text2, "Occultation of " + text2, AutoOpenFile: true, View: true, "");
			}
		}

		private void saveGoogleEarthKMLFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (OutputIsGraze)
			{
				string text = LunarOccultations.EventID(LunarOccultations.GrazeSelectedStar);
				LunarOccultations.CreateGoogleEarthKMZFile_ForGrazes(text, "Graze of " + text, AutoOpenFile: false, View: false, "");
			}
			else if (!((((ListControl)lstPrediction).get_SelectedIndex() < 0) | OutputIsGraze | OutputIsMulti) && (int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()] >= 0)
			{
				LunarOccultations.MapSelectedStar = LunarOccultations.Prediction[(int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()]].ElementRecordNumber;
				string text2 = LunarOccultations.EventID(LunarOccultations.MapSelectedStar);
				LunarOccultations.Compute_StartEnd_at_RiseSet_Curves();
				LunarOccultations.Compute_Limit_Lines();
				LunarOccultations.CreateGoogleEarthKMZFile_ForWorld(text2, "Occultation of " + text2, AutoOpenFile: false, View: false, "");
			}
		}

		private void saveGoogleMapsHTMFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (OutputIsGraze)
			{
				string text = LunarOccultations.EventID(LunarOccultations.GrazeSelectedStar);
				LunarOccultations.CreateGoogleMapHTMFile_ForGrazes(text, "Graze of " + text + " ; Nominal site altitude " + string.Format("{0,1}", LunarOccultations.GrazeNominalAltitude) + "m", AutoOpenFile: false);
			}
			else if (!((((ListControl)lstPrediction).get_SelectedIndex() < 0) | OutputIsGraze | OutputIsMulti) && (int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()] >= 0)
			{
				LunarOccultations.MapSelectedStar = LunarOccultations.Prediction[(int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()]].ElementRecordNumber;
				string text2 = LunarOccultations.EventID(LunarOccultations.MapSelectedStar);
				LunarOccultations.Compute_StartEnd_at_RiseSet_Curves();
				LunarOccultations.Compute_Limit_Lines();
				LunarOccultations.CreateGoogleMapHTMFile_ForWorld(text2, "Occultation of " + text2, AutoOpenFile: false);
			}
		}

		private void cmxPrecisionMappingStreetsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarOccultations.CreateCMXFile_ForGrazes(LunarOccultations.EventID(LunarOccultations.GrazeSelectedStar));
		}

		private void genforMakingESRIShapefilesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarOccultations.CreateGENFile_ForGrazes(LunarOccultations.EventID(LunarOccultations.GrazeSelectedStar));
		}

		private void gpxEasyGPSToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarOccultations.CreateGPXFile_ForGrazes(LunarOccultations.EventID(LunarOccultations.GrazeSelectedStar));
		}

		private void mifMSMapPointToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarOccultations.CreateMIFFile_ForGrazes(LunarOccultations.EventID(LunarOccultations.GrazeSelectedStar));
		}

		private void pltOziExplorerToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarOccultations.CreatePLTFile_ForGrazes(LunarOccultations.EventID(LunarOccultations.GrazeSelectedStar));
		}

		private void txtforDelormeStreetAtlasToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarOccultations.CreateTXT_DeLorme_File_ForGrazes(LunarOccultations.EventID(LunarOccultations.GrazeSelectedStar));
		}

		private void computeGrazeToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (!((((ListControl)lstPrediction).get_SelectedIndex() < 0) | OutputIsGraze | OutputIsMulti) && (int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()] >= 0)
			{
				LunarOccultations.GrazeSelectedStar = LunarOccultations.Prediction[(int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()]].ElementRecordNumber;
				LunarOccultations.GrazeStartLongitude = Math.Floor(CurrentSite.Longitude - 2.5);
				LunarOccultations.GrazeEndLongitude = Math.Floor(CurrentSite.Longitude + 2.5);
				LunarOccultations.GrazeLongitudeStep = 0.25;
				LunarOccultations.GrazeNominalAltitude = CurrentSite.Altitude;
				LunarOccultations.GrazeLimit = 1;
				if ((LunarOccultations.Prediction[(int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()]].AA > 90.0) & (LunarOccultations.Prediction[(int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()]].AA < 270.0))
				{
					LunarOccultations.GrazeLimit = -1;
				}
				LunarOccultations.GrazePrediction(GetStar: false, cmbSiteFiles.get_Items().get_Item(((ListControl)cmbSiteFiles).get_SelectedIndex()).ToString());
				DisplayGrazes();
				ToolStripMenuItem obj = worldMapToolStripMenuItem;
				ToolStripMenuItem obj2 = moonMapToolStripMenuItem;
				ToolStripMenuItem obj3 = displayStarDetailsToolStripMenuItem;
				bool flag;
				((ToolStripItem)multiSitePredictionToolStripMenuItem).set_Visible(flag = false);
				bool flag2;
				((ToolStripItem)obj3).set_Visible(flag2 = flag);
				bool visible;
				((ToolStripItem)obj2).set_Visible(visible = flag2);
				((ToolStripItem)obj).set_Visible(visible);
				((ToolStripItem)computeGrazeToolStripMenuItem).set_Visible(false);
				((ToolStripItem)reListOccultationsToolStripMenuItem).set_Visible(true);
			}
		}

		private void grazeProfileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_00f3: Unknown result type (might be due to invalid IL or missing references)
			if (((((ListControl)lstPrediction).get_SelectedIndex() < 0) | (((ListControl)lstPrediction).get_SelectedIndex() >= lstPrediction_Index.Count) | !OutputIsGraze) || (int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()] < 0)
			{
				return;
			}
			int librationDetails = (int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()];
			if (!OutputIsGraze)
			{
				return;
			}
			int num = -1;
			int num2 = -1;
			for (int i = 0; i < lstPrediction.get_Items().get_Count(); i++)
			{
				if (lstPrediction.get_Items().get_Item(i).ToString()!.Contains("Inner limit"))
				{
					num = i;
				}
				if (lstPrediction.get_Items().get_Item(i).ToString()!.Contains("Outer limit"))
				{
					num2 = i;
				}
				if (num > 0 && num2 > num && ((ListControl)lstPrediction).get_SelectedIndex() >= num2)
				{
					MessageBox.Show("Please select a point along the  'Inner limit'  graze path", "Select Inner limit", (MessageBoxButtons)0, (MessageBoxIcon)48);
					return;
				}
			}
			LunarProfile.SetLibrationDetails(librationDetails);
			LunarProfile.InitialiseProfile();
			LunarProfile.PlotGrazeProfile(HighlightRePlot: false, "", "", "", ReReadHires: true);
		}

		private void reListOccultationsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ReCompute_SingleDay();
			Display(chkAbbrev.get_Checked(), chkFilterOutput.get_Checked());
			SetContextMenuItems(ToReturn: false, SingleDayComputation);
		}

		private void SetContextMenuItems(bool ToReturn, bool SingleDay)
		{
			if (ToReturn)
			{
				((ToolStripItem)worldMapToolStripMenuItem).set_Visible(false);
				((ToolStripItem)viewInGoogleEarthToolStripMenuItem).set_Visible(false);
				((ToolStripItem)saveGoogleEarthKMLFileToolStripMenuItem).set_Visible(false);
				((ToolStripItem)saveGoogleMapsHTMFileToolStripMenuItem).set_Visible(false);
				((ToolStripItem)placeEventInRecordingTimerToolStripMenuItem).set_Visible(false);
				((ToolStripItem)displayStarDetailsToolStripMenuItem).set_Visible(false);
				((ToolStripItem)showPastLightCurvesToolStripMenuItem).set_Visible(false);
				((ToolStripItem)doubleStarDetailsToolStripMenuItem).set_Visible(false);
				((ToolStripItem)multiSitePredictionToolStripMenuItem).set_Visible(false);
				((ToolStripItem)computeGrazeToolStripMenuItem).set_Visible(false);
				((ToolStripItem)reListOccultationsToolStripMenuItem).set_Visible(true);
				((ToolStripItem)computePredictionsForToolStripMenuItem).set_Visible(false);
				if (OutputIsGraze)
				{
					((ToolStripItem)grazePathMapToolStripMenuItem).set_Visible(true);
					((ToolStripItem)grazeProfileToolStripMenuItem).set_Visible(true);
					((ToolStripItem)grazeProfileSetUpAnimatedGIFToolStripMenuItem).set_Visible(true);
					((ToolStripItem)limitingMagnitudeTableToolStripMenuItem).set_Visible(true);
					((ToolStripItem)viewInGoogleEarthToolStripMenuItem).set_Visible(Settings.Default.GoogleEarthInstalled);
					((ToolStripItem)saveGoogleEarthKMLFileToolStripMenuItem).set_Visible(true);
					((ToolStripItem)saveGoogleMapsHTMFileToolStripMenuItem).set_Visible(true);
					((ToolStripItem)saveInOtherMapFormatsToolStripMenuItem).set_Visible(true);
				}
				else
				{
					((ToolStripItem)grazePathMapToolStripMenuItem).set_Visible(false);
					((ToolStripItem)grazeProfileToolStripMenuItem).set_Visible(false);
					((ToolStripItem)grazeProfileSetUpAnimatedGIFToolStripMenuItem).set_Visible(false);
					((ToolStripItem)saveInOtherMapFormatsToolStripMenuItem).set_Visible(false);
					((ToolStripItem)limitingMagnitudeTableToolStripMenuItem).set_Visible(false);
				}
				if (OutputIsMulti)
				{
					((ToolStripItem)moonMapToolStripMenuItem).set_Visible(true);
				}
				else
				{
					((ToolStripItem)moonMapToolStripMenuItem).set_Visible(false);
				}
			}
			else
			{
				((ToolStripItem)worldMapToolStripMenuItem).set_Visible(SingleDay);
				((ToolStripItem)viewInGoogleEarthToolStripMenuItem).set_Visible(SingleDay & Settings.Default.GoogleEarthInstalled);
				((ToolStripItem)saveGoogleEarthKMLFileToolStripMenuItem).set_Visible(SingleDay);
				((ToolStripItem)saveGoogleMapsHTMFileToolStripMenuItem).set_Visible(SingleDay);
				((ToolStripItem)saveInOtherMapFormatsToolStripMenuItem).set_Visible(false);
				((ToolStripItem)moonMapToolStripMenuItem).set_Visible(true);
				((ToolStripItem)placeEventInRecordingTimerToolStripMenuItem).set_Visible(true);
				((ToolStripItem)displayStarDetailsToolStripMenuItem).set_Visible(true);
				((ToolStripItem)doubleStarDetailsToolStripMenuItem).set_Visible(true);
				((ToolStripItem)doubleStarDetailsToolStripMenuItem).set_Visible(true);
				((ToolStripItem)multiSitePredictionToolStripMenuItem).set_Visible(SingleDay);
				((ToolStripItem)computeGrazeToolStripMenuItem).set_Visible(SingleDay);
				((ToolStripItem)reListOccultationsToolStripMenuItem).set_Visible(false);
				((ToolStripItem)grazePathMapToolStripMenuItem).set_Visible(false);
				((ToolStripItem)grazeProfileToolStripMenuItem).set_Visible(false);
				((ToolStripItem)grazeProfileSetUpAnimatedGIFToolStripMenuItem).set_Visible(false);
				((ToolStripItem)limitingMagnitudeTableToolStripMenuItem).set_Visible(false);
				((ToolStripItem)computePredictionsForToolStripMenuItem).set_Visible(!SingleDay);
			}
		}

		private void grazePathMapToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (!((((ListControl)lstPrediction).get_SelectedIndex() < 0) | (((ListControl)lstPrediction).get_SelectedIndex() >= lstPrediction_Index.Count) | OutputIsMulti) && (int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()] >= 0)
			{
				LunarOccultations.GrazeMapShowForm();
			}
		}

		private void limitingMagnitudeTableToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (!((((ListControl)lstPrediction).get_SelectedIndex() < 0) | (((ListControl)lstPrediction).get_SelectedIndex() >= lstPrediction_Index.Count) | !OutputIsGraze) && (int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()] >= 0)
			{
				LunarOccultations.GetMagLimit((int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()]);
			}
		}

		private void computePredictionsForToolStripMenuItem_Click(object sender, EventArgs e)
		{
			double jD = LunarOccultations.Prediction[(int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()]].JD;
			Math.Floor(jD - 0.5);
			Utilities.Date_from_JD(jD, out var Year, out var Month, out var day);
			NumericUpDown obj = updnLunarYearStart;
			decimal value;
			updnLunarYearEnd.set_Value(value = Year);
			obj.set_Value(value);
			ComboBox obj2 = cmbLunarMonthStart;
			int selectedIndex;
			((ListControl)cmbLunarMonthEnd).set_SelectedIndex(selectedIndex = Month - 1);
			((ListControl)obj2).set_SelectedIndex(selectedIndex);
			ComboBox obj3 = cmbLunarDayStart;
			((ListControl)cmbLunarDayEnd).set_SelectedIndex(selectedIndex = (int)(day - 1.0));
			((ListControl)obj3).set_SelectedIndex(selectedIndex);
			cmdCompute_Click(sender, e);
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void showLunarEphemerisToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			MoonSeriesViewer moonSeriesViewer = new MoonSeriesViewer();
			moonSeriesViewer.GetMoonSeries(JDFirstDate);
			((Form)moonSeriesViewer).ShowDialog();
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			CancelFlag = true;
		}

		private void cmdHome_Click(object sender, EventArgs e)
		{
			string homeSiteFileLunarPredict = Settings.Default.HomeSiteFileLunarPredict;
			for (int i = 0; i < cmbSiteFiles.get_Items().get_Count(); i++)
			{
				if (cmbSiteFiles.get_Items().get_Item(i).ToString() == homeSiteFileLunarPredict)
				{
					((ListControl)cmbSiteFiles).set_SelectedIndex(i);
					break;
				}
			}
			homeSiteFileLunarPredict = Settings.Default.HomeSiteLunarPredict;
			for (int j = 0; j < cmbSite.get_Items().get_Count(); j++)
			{
				if (cmbSite.get_Items().get_Item(j).ToString() == homeSiteFileLunarPredict)
				{
					((ListControl)cmbSite).set_SelectedIndex(j);
					break;
				}
			}
		}

		private void cmdHomeSet_Click(object sender, EventArgs e)
		{
			//IL_0035: Unknown result type (might be due to invalid IL or missing references)
			//IL_003b: Invalid comparison between Unknown and I4
			//IL_00bd: Unknown result type (might be due to invalid IL or missing references)
			if ((int)MessageBox.Show("Do you want to store as your Home location:\r\n\r\nSite File :  " + Settings.Default.HomeSiteFileLunarPredict + "\r\n\r\nSite      :  " + Settings.Default.HomeSiteLunarPredict, "Home Site set", (MessageBoxButtons)4, (MessageBoxIcon)64, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				Settings.Default.HomeSiteFileLunarPredict = cmbSiteFiles.get_Items().get_Item(((ListControl)cmbSiteFiles).get_SelectedIndex()).ToString();
				Settings.Default.HomeSiteLunarPredict = cmbSite.get_Items().get_Item(((ListControl)cmbSite).get_SelectedIndex()).ToString();
				MessageBox.Show("The following have been stored as your Home location:\r\n\r\nSite File :  " + Settings.Default.HomeSiteFileLunarPredict + "\r\n\r\nSite      :  " + Settings.Default.HomeSiteLunarPredict, "Home Site set", (MessageBoxButtons)0, (MessageBoxIcon)64);
			}
		}

		private void chkFilter_CheckedChanged(object sender, EventArgs e)
		{
			if (!chkFilter.get_Checked())
			{
				CurrentElementsAreValid = false;
			}
		}

		private void chkStars_CheckedChanged(object sender, EventArgs e)
		{
			CurrentElementsAreValid = false;
		}

		private void chkPlanets_CheckedChanged(object sender, EventArgs e)
		{
			CurrentElementsAreValid = false;
		}

		private void chkAsteroids_CheckedChanged(object sender, EventArgs e)
		{
			CurrentElementsAreValid = false;
		}

		private void cmdUseSingleSite_Click(object sender, EventArgs e)
		{
			ShowSingleSiteBox();
		}

		internal void ShowSingleSiteBox()
		{
			((Control)grpSingleSite).set_Visible(true);
			UsingSingleSite = true;
			((Control)cmdMultiLocation).set_Visible(false);
			CurrentElementsAreValid = false;
			CurrentSite = new Sites();
		}

		private void cmdUseSiteFiles_Click(object sender, EventArgs e)
		{
			((Control)grpSingleSite).set_Visible(false);
			UsingSingleSite = false;
			((Control)cmdMultiLocation).set_Visible(true);
			CurrentElementsAreValid = false;
			CurrentSite = AllSites[((ListControl)cmbSite).get_SelectedIndex()];
		}

		private void Set_CurrentSite_for_Singles()
		{
			CurrentSite.Longitude = GetDMS(((Control)txtLongD).get_Text(), ((Control)txtLongM).get_Text(), ((Control)txtLongS).get_Text());
			if (CurrentSite.Longitude < -180.0)
			{
				CurrentSite.Longitude += 360.0;
			}
			if (CurrentSite.Longitude > 180.0)
			{
				CurrentSite.Longitude += 360.0;
			}
			CurrentSite.Latitude = GetDMS(((Control)txtLatD).get_Text(), ((Control)txtLatM).get_Text(), ((Control)txtLatS).get_Text());
			if (!double.TryParse(((Control)txtAltitude).get_Text(), out var result))
			{
				result = 0.0;
			}
			CurrentSite.Altitude = result;
			CurrentSite.GeoidHeight = Utilities.GeoidHeight(CurrentSite.Longitude, CurrentSite.Latitude);
			CurrentSite.ApertureCM = (float)updnAperture.get_Value();
			CurrentSite.MagCorrection = 0f;
			CurrentSite.GrazeTravelDist = 50.0;
			CurrentSite.TimeZone = 0f;
			CurrentSite.Name = ((Control)txtName).get_Text();
			CurrentSite.ShortName = ((Control)txtName).get_Text();
			CurrentSite.PlotOnMap = 0;
			CurrentSite.PlotInGoogle = 0;
			NorthLimit = CurrentSite.Latitude + 2.0;
			SouthLimit = CurrentSite.Latitude - 2.0;
			EastLimit = CurrentSite.Longitude + 2.0;
			WestLimit = CurrentSite.Longitude - 2.0;
		}

		private double GetDMS(string Deg, string Min, string Sec)
		{
			double num = 0.0;
			if (!double.TryParse(Deg.Replace('-', ' '), out var result))
			{
				result = 0.0;
			}
			if (!double.TryParse(Min, out var result2))
			{
				result2 = 0.0;
			}
			if (!double.TryParse(Sec, out var result3))
			{
				result3 = 0.0;
			}
			num = result + result2 / 60.0 + result3 / 3600.0;
			if (Deg.Contains("-"))
			{
				num = 0.0 - num;
			}
			return num;
		}

		private void chkAbbrev_CheckedChanged(object sender, EventArgs e)
		{
			Display(chkAbbrev.get_Checked(), chkFilterOutput.get_Checked());
		}

		private void toolStripMenuItem2_Click(object sender, EventArgs e)
		{
			SetGlobalMagLimit(-1);
		}

		private void toolStripMenuItem3_Click(object sender, EventArgs e)
		{
			SetGlobalMagLimit(0);
		}

		private void toolStripMenuItem4_Click(object sender, EventArgs e)
		{
			SetGlobalMagLimit(1);
		}

		private void toolStripMenuItem5_Click(object sender, EventArgs e)
		{
			SetGlobalMagLimit(2);
		}

		private void toolStripMenuItem6_Click(object sender, EventArgs e)
		{
			SetGlobalMagLimit(3);
		}

		private void toolStripMenuItem7_Click(object sender, EventArgs e)
		{
			SetGlobalMagLimit(4);
		}

		private void toolStripMenuItem8_Click(object sender, EventArgs e)
		{
			SetGlobalMagLimit(5);
		}

		private void toolStripMenuItem9_Click(object sender, EventArgs e)
		{
			SetGlobalMagLimit(10);
		}

		private void SetGlobalMagLimit(int x)
		{
			GlobalMagLimitAdjustment = x;
			toolStripMenuItem2.set_Checked(x == -1);
			toolStripMenuItem3.set_Checked(x == 0);
			toolStripMenuItem4.set_Checked(x == 1);
			toolStripMenuItem5.set_Checked(x == 2);
			toolStripMenuItem6.set_Checked(x == 3);
			toolStripMenuItem7.set_Checked(x == 4);
			toolStripMenuItem8.set_Checked(x == 5);
			toolStripMenuItem9.set_Checked(x == 10);
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Lunar Occultations - Predictions");
		}

		private void radioButton1_CheckedChanged(object sender, EventArgs e)
		{
			SetStartHour();
		}

		private void radioButton2_CheckedChanged(object sender, EventArgs e)
		{
			SetStartHour();
		}

		private void radioButton3_CheckedChanged(object sender, EventArgs e)
		{
			SetStartHour();
		}

		private void radioButton4_CheckedChanged(object sender, EventArgs e)
		{
			SetStartHour();
		}

		private void SetStartHour()
		{
			if (radioButton1.get_Checked())
			{
				StartHour = -6;
			}
			else if (radioButton3.get_Checked())
			{
				StartHour = 6;
			}
			else if (radioButton4.get_Checked())
			{
				StartHour = 12;
			}
			else
			{
				StartHour = 0;
			}
			Settings.Default.StartHour = StartHour;
		}

		private void setUpAnimatedGIFToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			GIFsettings gIFsettings = new GIFsettings();
			((Form)gIFsettings).ShowDialog();
			((Component)(object)gIFsettings).Dispose();
		}

		private void createAnimatedGIFToolStripMenuItem_Click(object sender, EventArgs e)
		{
			List<string> list = new List<string>();
			int num = 10000;
			string path = Utilities.AppPath + "\\Predictions\\AnimatedGIF";
			if (!Directory.Exists(path))
			{
				Directory.CreateDirectory(path);
			}
			string[] files = Directory.GetFiles(path, "*.gif");
			foreach (string path2 in files)
			{
				try
				{
					File.Delete(path2);
				}
				catch
				{
				}
			}
			for (int j = 0; j < LunarOccultations.GrazePredictionLines.Count; j++)
			{
				LunarProfile.SetLibrationDetails(j);
				LunarProfile.InitialiseProfile();
				LunarProfile.PlotGrazeProfile(HighlightRePlot: false, "", "", "", ReReadHires: true);
				string text = Utilities.AppPath + "\\Predictions\\AnimatedGIF\\" + num + ".gif";
				try
				{
					LunarProfile.GrazeProfile.picProfile.get_Image().Save(text, ImageFormat.Gif);
					list.Add(text);
				}
				catch
				{
				}
				num++;
			}
			global::GifCreator.GifCreator.Create_Animated_Gif(list, "GrazeProfile " + LunarOccultations.GrazePredictionLines[0].StarId + " " + Utilities.Date_from_JD(Math.Floor(LunarOccultations.GrazePredictionLines[0].JD - 0.5) + 0.5, 0) + ".gif");
		}

		private void chkDoublesOnly_CheckedChanged(object sender, EventArgs e)
		{
			CurrentElementsAreValid = false;
		}

		private void dayWeatherForecastToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_0045: Unknown result type (might be due to invalid IL or missing references)
			//IL_004b: Invalid comparison between Unknown and I4
			//IL_0169: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a9: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
			}
			else
			{
				if (OutputIsMulti)
				{
					return;
				}
				if (OutputIsGraze)
				{
					if ((int)MessageBox.Show("Graze weather predictions can take several minutes\r\n\r\nDo you want to continue \r\n\r\n[ If you continue, click Cancel on the weather form to abort ]", "Confirm - get graze weather", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
					{
						return;
					}
					LunarOccultations.ShowGrazeWeather();
					LunarOccultations.GrazeWeather.Cancel = false;
					((ToolStripItem)LunarOccultations.GrazeWeather.cancelToolStripMenuItem).set_Visible(true);
					LunarOccultations.GrazeWeather.lstWeather.get_Items().Clear();
					int num = LunarOccultations.GrazePredictionLines.Count / 10;
					if (num < 1)
					{
						num = 1;
					}
					for (int i = 0; i < LunarOccultations.GrazePredictionLines.Count; i += num)
					{
						Cursor.set_Current(Cursors.get_WaitCursor());
						double grazeLongitude = LunarOccultations.GrazePredictionLines[i].GrazeLongitude;
						double grazeLatitude = LunarOccultations.GrazePredictionLines[i].GrazeLatitude;
						new APanelWeatherInfo(APanelWeatherInfo.Get7TimerWeatherForLocation(grazeLongitude, grazeLatitude));
						Utilities.Date_from_JD(LunarOccultations.GrazePredictionLines[i].JD, out var Year, out var Month, out var day);
						int num2 = (int)Math.Floor(day);
						int num3 = (int)(24.0 * (day - (double)num2));
						DateTime dateTime = new DateTime(Year, Month, num2, num3, 0, 0);
						int num4 = (int)(12.5 * (double)APanelWeatherInfo.GetCloudCoverForTime(dateTime));
						int tcForTime = APanelWeatherInfo.GetTcForTime(dateTime);
						if (num4 < 0)
						{
							if (dateTime < DateTime.Now.ToUniversalTime())
							{
								MessageBox.Show("Cannot display weather for past events", "Timing error", (MessageBoxButtons)0, (MessageBoxIcon)48);
								break;
							}
							if (dateTime > DateTime.Now.ToUniversalTime().AddHours(60.0))
							{
								MessageBox.Show("Cannot display weather more than about 3 days ahead", "Timing error", (MessageBoxButtons)0, (MessageBoxIcon)48);
								break;
							}
						}
						if (i == 0)
						{
							LunarOccultations.GrazeWeather.lstWeather.get_Items().Add((object)("  " + Year + " " + Utilities.ShortMonths[Month] + num2.ToString().PadLeft(2, '0').PadLeft(3) + string.Format(", at{0,3:f0} hrs UT", num3)));
							LunarOccultations.GrazeWeather.lstWeather.get_Items().Add((object)"");
							LunarOccultations.GrazeWeather.lstWeather.get_Items().Add((object)"  Long.    Lat.   cloud    Tc");
						}
						LunarOccultations.GrazeWeather.lstWeather.get_Items().Add((object)(string.Format("{0,8:F2}", grazeLongitude) + string.Format("{0,8:F2}", grazeLatitude) + string.Format("  {0,4:F0}%", num4) + string.Format("  {0,4:F0}", tcForTime)));
						Application.DoEvents();
						if (LunarOccultations.GrazeWeather.Cancel)
						{
							break;
						}
					}
					((ToolStripItem)LunarOccultations.GrazeWeather.cancelToolStripMenuItem).set_Visible(false);
					Cursor.set_Current(Cursors.get_Default());
				}
				else
				{
					APanelWeatherInfo.ShowSiteCloud(CurrentSite.Longitude, CurrentSite.Latitude);
				}
			}
		}

		private void weatherMapsToolStripMenuItem_Click(object sender, EventArgs e)
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

		private void showPastLightCurvesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_002b: Unknown result type (might be due to invalid IL or missing references)
			LightData.ShowLightCurveViewer();
			((Control)LightData.LightCurveView).Focus();
			if (((ListControl)lstPrediction).get_SelectedIndex() < 0)
			{
				MessageBox.Show("Select a prediction line", "Select", (MessageBoxButtons)0, (MessageBoxIcon)16);
			}
			else
			{
				LightData.LightCurveView.ReadXZIndexForDates(LunarOccultations.Prediction[(int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()]].XZnum);
			}
		}

		private void chkFilterOutput_CheckedChanged(object sender, EventArgs e)
		{
			if (!SettingFilter)
			{
				Display(chkAbbrev.get_Checked(), chkFilterOutput.get_Checked());
			}
		}

		private void setOutputFilterToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarOccultations.ShowOutputFilter();
		}

		private void showApparentstarPositioNnotJ2000ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.LunarPredict_DisplayApparentPosition = !Settings.Default.LunarPredict_DisplayApparentPosition;
			showApparentstarPositioNnotJ2000ToolStripMenuItem.set_Checked(Settings.Default.LunarPredict_DisplayApparentPosition);
			ReCompute_SingleDay();
		}

		private void chkLong_Click(object sender, EventArgs e)
		{
			chkLong.set_Checked(!chkLong.get_Checked());
			if (!chkLong.get_Checked())
			{
				chkShort.set_Checked(true);
			}
		}

		private void chkShort_Click(object sender, EventArgs e)
		{
			chkShort.set_Checked(!chkShort.get_Checked());
			if (!chkShort.get_Checked())
			{
				chkLong.set_Checked(true);
			}
		}

		private void cmdMultiOK_Click(object sender, EventArgs e)
		{
			((Control)pnlMultiSiteOutput).set_Visible(false);
		}

		private void cmdFormat_Click(object sender, EventArgs e)
		{
			((Control)pnlMultiSiteOutput).set_Visible(true);
			((Control)pnlMultiSiteOutput).set_Top(35);
			((Control)pnlMultiSiteOutput).set_Left(((Control)groupBox2).get_Left() + 30);
		}

		private void lstPrediction_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (LunarOccultations.MoonMap != null && ((Control)LunarOccultations.MoonMap).get_Created())
			{
				DisplayMoonMap();
			}
		}

		private void LunarOccultationPrediction_FormClosing(object sender, FormClosingEventArgs e)
		{
			Kepler2.XZinK2.Clear();
			Settings.Default.LunarPredictFormHeight = ((Control)this).get_Height();
			Settings.Default.LunarPredictFormWidth = ((Control)this).get_Width();
		}

		private void placeEventInRecordingTimerToolStripMenuItem_Click(object sender, EventArgs e)
		{
			PlaceEventsIntoTimer(Add10: false);
		}

		private void placeNext10EventsIntoRecordingTimerToolStripMenuItem_Click(object sender, EventArgs e)
		{
			PlaceEventsIntoTimer(Add10: true);
		}

		private void PlaceEventsIntoTimer(bool Add10)
		{
			//IL_0245: Unknown result type (might be due to invalid IL or missing references)
			int num = 0;
			int num2 = 1;
			if (Add10)
			{
				num2 = 10;
			}
			if (((ListControl)lstPrediction).get_SelectedIndex() < 0 || (int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()] < 0)
			{
				return;
			}
			int num3 = (int)lstPrediction_Index[((ListControl)lstPrediction).get_SelectedIndex()];
			string text = "";
			double num4 = 1.0;
			for (int i = 0; i < num2; i++)
			{
				num = num3 + i;
				if (num >= LunarOccultations.Prediction.Count)
				{
					break;
				}
				if (!"MG".Contains(LunarOccultations.Prediction[num].EventPhase.ToUpper().Substring(0, 1)))
				{
					LunarOccultations.Prediction[num].EventDate(out var year, out var month, out var day);
					string text2 = LunarOccultations.Prediction[num].EventTime();
					num4 = Math.Sqrt(Math.Abs(0.35 / LunarOccultations.Prediction[num].RV));
					string text3 = LunarOccultations.Prediction[num].EventID();
					int hour = int.Parse(text2.Substring(1, 2));
					int minute = int.Parse(text2.Substring(4, 2));
					double num5 = double.Parse(text2.Substring(7, 4));
					int num6 = (int)num5;
					num5 -= (double)num6;
					DateTime dateTime = new DateTime(year, month, day, hour, minute, num6).ToLocalTime().AddSeconds(num5 - (double)Settings.Default.TimerOffsets_Lunar * num4);
					DateTime dateTime2 = new DateTime(year, month, day, hour, minute, num6).ToLocalTime().AddSeconds(num5 + (double)Settings.Default.TimerOffsets_Lunar * num4);
					DateTime now = DateTime.Now;
					if (dateTime < now)
					{
						text = "The event { " + text3.Trim() + " } has already started";
					}
					if (dateTime2 < now)
					{
						text = "The event { " + text3.Trim() + " } has already finished";
					}
					if (dateTime > now.AddDays(3.0))
					{
						text = "The event { " + text3.Trim() + " } is more than 72 hours in the future";
					}
					if (text.Length > 0)
					{
						MessageBox.Show(text + "\r\nThe event will not be added to Recording Timer", "Event not added", (MessageBoxButtons)0);
						continue;
					}
					LunarOccultations.Show_RecordingTimer();
					LunarOccultations.VDTimer.chkEnableTimer.set_Checked(false);
					LunarOccultations.VDTimer.AddAnEvent_External(dateTime, dateTime2, text3, i);
				}
			}
		}

		private void showRecordingTimerToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarOccultations.Show_RecordingTimer();
		}

		private void sToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetTimerOffsets(3);
		}

		private void secToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetTimerOffsets(4);
		}

		private void secToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			SetTimerOffsets(5);
		}

		private void secToolStripMenuItem2_Click(object sender, EventArgs e)
		{
			SetTimerOffsets(6);
		}

		private void secToolStripMenuItem3_Click(object sender, EventArgs e)
		{
			SetTimerOffsets(7);
		}

		private void SetTimerOffsets(int TimerVal)
		{
			Settings.Default.TimerOffsets_Lunar = TimerVal;
			SetTimerOffsetChecks();
		}

		private void SetTimerOffsetChecks()
		{
			int timerOffsets_Lunar = Settings.Default.TimerOffsets_Lunar;
			sToolStripMenuItem.set_Checked(timerOffsets_Lunar == 3);
			secToolStripMenuItem.set_Checked(timerOffsets_Lunar == 4);
			secToolStripMenuItem1.set_Checked(timerOffsets_Lunar == 5);
			secToolStripMenuItem2.set_Checked(timerOffsets_Lunar == 6);
			secToolStripMenuItem3.set_Checked(timerOffsets_Lunar == 7);
			((ToolStripItem)placeEventInRecordingTimerToolStripMenuItem).set_Text("Place event into Recording Timer  -" + timerOffsets_Lunar + " sec, +" + (timerOffsets_Lunar - 1) + " sec");
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
			//IL_001d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0027: Expected O, but got Unknown
			//IL_0028: Unknown result type (might be due to invalid IL or missing references)
			//IL_0032: Expected O, but got Unknown
			//IL_0033: Unknown result type (might be due to invalid IL or missing references)
			//IL_003d: Expected O, but got Unknown
			//IL_003e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0048: Expected O, but got Unknown
			//IL_0049: Unknown result type (might be due to invalid IL or missing references)
			//IL_0053: Expected O, but got Unknown
			//IL_0054: Unknown result type (might be due to invalid IL or missing references)
			//IL_005e: Expected O, but got Unknown
			//IL_005f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0069: Expected O, but got Unknown
			//IL_006a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0074: Expected O, but got Unknown
			//IL_0075: Unknown result type (might be due to invalid IL or missing references)
			//IL_007f: Expected O, but got Unknown
			//IL_0080: Unknown result type (might be due to invalid IL or missing references)
			//IL_008a: Expected O, but got Unknown
			//IL_008b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0095: Expected O, but got Unknown
			//IL_0096: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a0: Expected O, but got Unknown
			//IL_00a1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ab: Expected O, but got Unknown
			//IL_00ac: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b6: Expected O, but got Unknown
			//IL_00b7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c1: Expected O, but got Unknown
			//IL_00c2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00cc: Expected O, but got Unknown
			//IL_00cd: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d7: Expected O, but got Unknown
			//IL_00d8: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e2: Expected O, but got Unknown
			//IL_00e3: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ed: Expected O, but got Unknown
			//IL_00ee: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f8: Expected O, but got Unknown
			//IL_00f9: Unknown result type (might be due to invalid IL or missing references)
			//IL_0103: Expected O, but got Unknown
			//IL_0104: Unknown result type (might be due to invalid IL or missing references)
			//IL_010e: Expected O, but got Unknown
			//IL_010f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0119: Expected O, but got Unknown
			//IL_011a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0124: Expected O, but got Unknown
			//IL_0125: Unknown result type (might be due to invalid IL or missing references)
			//IL_012f: Expected O, but got Unknown
			//IL_0130: Unknown result type (might be due to invalid IL or missing references)
			//IL_013a: Expected O, but got Unknown
			//IL_013b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0145: Expected O, but got Unknown
			//IL_0146: Unknown result type (might be due to invalid IL or missing references)
			//IL_0150: Expected O, but got Unknown
			//IL_0151: Unknown result type (might be due to invalid IL or missing references)
			//IL_015b: Expected O, but got Unknown
			//IL_015c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0166: Expected O, but got Unknown
			//IL_0167: Unknown result type (might be due to invalid IL or missing references)
			//IL_0171: Expected O, but got Unknown
			//IL_0172: Unknown result type (might be due to invalid IL or missing references)
			//IL_017c: Expected O, but got Unknown
			//IL_017d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0187: Expected O, but got Unknown
			//IL_0188: Unknown result type (might be due to invalid IL or missing references)
			//IL_0192: Expected O, but got Unknown
			//IL_0193: Unknown result type (might be due to invalid IL or missing references)
			//IL_019d: Expected O, but got Unknown
			//IL_019e: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a8: Expected O, but got Unknown
			//IL_01a9: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b3: Expected O, but got Unknown
			//IL_01b4: Unknown result type (might be due to invalid IL or missing references)
			//IL_01be: Expected O, but got Unknown
			//IL_01bf: Unknown result type (might be due to invalid IL or missing references)
			//IL_01c9: Expected O, but got Unknown
			//IL_01ca: Unknown result type (might be due to invalid IL or missing references)
			//IL_01d4: Expected O, but got Unknown
			//IL_01d5: Unknown result type (might be due to invalid IL or missing references)
			//IL_01df: Expected O, but got Unknown
			//IL_01e0: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ea: Expected O, but got Unknown
			//IL_01eb: Unknown result type (might be due to invalid IL or missing references)
			//IL_01f5: Expected O, but got Unknown
			//IL_01f6: Unknown result type (might be due to invalid IL or missing references)
			//IL_0200: Expected O, but got Unknown
			//IL_0201: Unknown result type (might be due to invalid IL or missing references)
			//IL_020b: Expected O, but got Unknown
			//IL_020c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0216: Expected O, but got Unknown
			//IL_0217: Unknown result type (might be due to invalid IL or missing references)
			//IL_0221: Expected O, but got Unknown
			//IL_0222: Unknown result type (might be due to invalid IL or missing references)
			//IL_022c: Expected O, but got Unknown
			//IL_022d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0237: Expected O, but got Unknown
			//IL_0238: Unknown result type (might be due to invalid IL or missing references)
			//IL_0242: Expected O, but got Unknown
			//IL_0243: Unknown result type (might be due to invalid IL or missing references)
			//IL_024d: Expected O, but got Unknown
			//IL_024e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0258: Expected O, but got Unknown
			//IL_0259: Unknown result type (might be due to invalid IL or missing references)
			//IL_0263: Expected O, but got Unknown
			//IL_0264: Unknown result type (might be due to invalid IL or missing references)
			//IL_026e: Expected O, but got Unknown
			//IL_026f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0279: Expected O, but got Unknown
			//IL_027a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0284: Expected O, but got Unknown
			//IL_0285: Unknown result type (might be due to invalid IL or missing references)
			//IL_028f: Expected O, but got Unknown
			//IL_0290: Unknown result type (might be due to invalid IL or missing references)
			//IL_029a: Expected O, but got Unknown
			//IL_029b: Unknown result type (might be due to invalid IL or missing references)
			//IL_02a5: Expected O, but got Unknown
			//IL_02a6: Unknown result type (might be due to invalid IL or missing references)
			//IL_02b0: Expected O, but got Unknown
			//IL_02b1: Unknown result type (might be due to invalid IL or missing references)
			//IL_02bb: Expected O, but got Unknown
			//IL_02bc: Unknown result type (might be due to invalid IL or missing references)
			//IL_02c6: Expected O, but got Unknown
			//IL_02c7: Unknown result type (might be due to invalid IL or missing references)
			//IL_02d1: Expected O, but got Unknown
			//IL_02d2: Unknown result type (might be due to invalid IL or missing references)
			//IL_02dc: Expected O, but got Unknown
			//IL_02dd: Unknown result type (might be due to invalid IL or missing references)
			//IL_02e7: Expected O, but got Unknown
			//IL_02e8: Unknown result type (might be due to invalid IL or missing references)
			//IL_02f2: Expected O, but got Unknown
			//IL_02f3: Unknown result type (might be due to invalid IL or missing references)
			//IL_02fd: Expected O, but got Unknown
			//IL_02fe: Unknown result type (might be due to invalid IL or missing references)
			//IL_0308: Expected O, but got Unknown
			//IL_0309: Unknown result type (might be due to invalid IL or missing references)
			//IL_0313: Expected O, but got Unknown
			//IL_0314: Unknown result type (might be due to invalid IL or missing references)
			//IL_031e: Expected O, but got Unknown
			//IL_031f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0329: Expected O, but got Unknown
			//IL_032a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0334: Expected O, but got Unknown
			//IL_0335: Unknown result type (might be due to invalid IL or missing references)
			//IL_033f: Expected O, but got Unknown
			//IL_0340: Unknown result type (might be due to invalid IL or missing references)
			//IL_034a: Expected O, but got Unknown
			//IL_034b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0355: Expected O, but got Unknown
			//IL_0356: Unknown result type (might be due to invalid IL or missing references)
			//IL_0360: Expected O, but got Unknown
			//IL_0361: Unknown result type (might be due to invalid IL or missing references)
			//IL_036b: Expected O, but got Unknown
			//IL_036c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0376: Expected O, but got Unknown
			//IL_0377: Unknown result type (might be due to invalid IL or missing references)
			//IL_0381: Expected O, but got Unknown
			//IL_0382: Unknown result type (might be due to invalid IL or missing references)
			//IL_038c: Expected O, but got Unknown
			//IL_038d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0397: Expected O, but got Unknown
			//IL_0398: Unknown result type (might be due to invalid IL or missing references)
			//IL_03a2: Expected O, but got Unknown
			//IL_03a3: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ad: Expected O, but got Unknown
			//IL_03ae: Unknown result type (might be due to invalid IL or missing references)
			//IL_03b8: Expected O, but got Unknown
			//IL_03b9: Unknown result type (might be due to invalid IL or missing references)
			//IL_03c3: Expected O, but got Unknown
			//IL_03c4: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ce: Expected O, but got Unknown
			//IL_03cf: Unknown result type (might be due to invalid IL or missing references)
			//IL_03d9: Expected O, but got Unknown
			//IL_03da: Unknown result type (might be due to invalid IL or missing references)
			//IL_03e4: Expected O, but got Unknown
			//IL_03e5: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ef: Expected O, but got Unknown
			//IL_03f0: Unknown result type (might be due to invalid IL or missing references)
			//IL_03fa: Expected O, but got Unknown
			//IL_03fb: Unknown result type (might be due to invalid IL or missing references)
			//IL_0405: Expected O, but got Unknown
			//IL_0406: Unknown result type (might be due to invalid IL or missing references)
			//IL_0410: Expected O, but got Unknown
			//IL_0411: Unknown result type (might be due to invalid IL or missing references)
			//IL_041b: Expected O, but got Unknown
			//IL_041c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0426: Expected O, but got Unknown
			//IL_0427: Unknown result type (might be due to invalid IL or missing references)
			//IL_0431: Expected O, but got Unknown
			//IL_0432: Unknown result type (might be due to invalid IL or missing references)
			//IL_043c: Expected O, but got Unknown
			//IL_043d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0447: Expected O, but got Unknown
			//IL_0448: Unknown result type (might be due to invalid IL or missing references)
			//IL_0452: Expected O, but got Unknown
			//IL_0453: Unknown result type (might be due to invalid IL or missing references)
			//IL_045d: Expected O, but got Unknown
			//IL_045e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0468: Expected O, but got Unknown
			//IL_0469: Unknown result type (might be due to invalid IL or missing references)
			//IL_0473: Expected O, but got Unknown
			//IL_0474: Unknown result type (might be due to invalid IL or missing references)
			//IL_047e: Expected O, but got Unknown
			//IL_047f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0489: Expected O, but got Unknown
			//IL_048a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0494: Expected O, but got Unknown
			//IL_0495: Unknown result type (might be due to invalid IL or missing references)
			//IL_049f: Expected O, but got Unknown
			//IL_04a0: Unknown result type (might be due to invalid IL or missing references)
			//IL_04aa: Expected O, but got Unknown
			//IL_04ab: Unknown result type (might be due to invalid IL or missing references)
			//IL_04b5: Expected O, but got Unknown
			//IL_04b6: Unknown result type (might be due to invalid IL or missing references)
			//IL_04c0: Expected O, but got Unknown
			//IL_04c1: Unknown result type (might be due to invalid IL or missing references)
			//IL_04cb: Expected O, but got Unknown
			//IL_04cc: Unknown result type (might be due to invalid IL or missing references)
			//IL_04d6: Expected O, but got Unknown
			//IL_04d7: Unknown result type (might be due to invalid IL or missing references)
			//IL_04e1: Expected O, but got Unknown
			//IL_04e2: Unknown result type (might be due to invalid IL or missing references)
			//IL_04ec: Expected O, but got Unknown
			//IL_04ed: Unknown result type (might be due to invalid IL or missing references)
			//IL_04f7: Expected O, but got Unknown
			//IL_04f8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0502: Expected O, but got Unknown
			//IL_0503: Unknown result type (might be due to invalid IL or missing references)
			//IL_050d: Expected O, but got Unknown
			//IL_050e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0518: Expected O, but got Unknown
			//IL_0519: Unknown result type (might be due to invalid IL or missing references)
			//IL_0523: Expected O, but got Unknown
			//IL_0524: Unknown result type (might be due to invalid IL or missing references)
			//IL_052e: Expected O, but got Unknown
			//IL_052f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0539: Expected O, but got Unknown
			//IL_053a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0544: Expected O, but got Unknown
			//IL_0545: Unknown result type (might be due to invalid IL or missing references)
			//IL_054f: Expected O, but got Unknown
			//IL_0550: Unknown result type (might be due to invalid IL or missing references)
			//IL_055a: Expected O, but got Unknown
			//IL_055b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0565: Expected O, but got Unknown
			//IL_0566: Unknown result type (might be due to invalid IL or missing references)
			//IL_0570: Expected O, but got Unknown
			//IL_0571: Unknown result type (might be due to invalid IL or missing references)
			//IL_057b: Expected O, but got Unknown
			//IL_057c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0586: Expected O, but got Unknown
			//IL_0587: Unknown result type (might be due to invalid IL or missing references)
			//IL_0591: Expected O, but got Unknown
			//IL_0592: Unknown result type (might be due to invalid IL or missing references)
			//IL_059c: Expected O, but got Unknown
			//IL_059d: Unknown result type (might be due to invalid IL or missing references)
			//IL_05a7: Expected O, but got Unknown
			//IL_05a8: Unknown result type (might be due to invalid IL or missing references)
			//IL_05b2: Expected O, but got Unknown
			//IL_05b3: Unknown result type (might be due to invalid IL or missing references)
			//IL_05bd: Expected O, but got Unknown
			//IL_05be: Unknown result type (might be due to invalid IL or missing references)
			//IL_05c8: Expected O, but got Unknown
			//IL_05c9: Unknown result type (might be due to invalid IL or missing references)
			//IL_05d3: Expected O, but got Unknown
			//IL_05d4: Unknown result type (might be due to invalid IL or missing references)
			//IL_05de: Expected O, but got Unknown
			//IL_05df: Unknown result type (might be due to invalid IL or missing references)
			//IL_05e9: Expected O, but got Unknown
			//IL_05ea: Unknown result type (might be due to invalid IL or missing references)
			//IL_05f4: Expected O, but got Unknown
			//IL_05f5: Unknown result type (might be due to invalid IL or missing references)
			//IL_05ff: Expected O, but got Unknown
			//IL_0600: Unknown result type (might be due to invalid IL or missing references)
			//IL_060a: Expected O, but got Unknown
			//IL_060b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0615: Expected O, but got Unknown
			//IL_0616: Unknown result type (might be due to invalid IL or missing references)
			//IL_0620: Expected O, but got Unknown
			//IL_0621: Unknown result type (might be due to invalid IL or missing references)
			//IL_062b: Expected O, but got Unknown
			//IL_062c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0636: Expected O, but got Unknown
			//IL_063d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0647: Expected O, but got Unknown
			//IL_0648: Unknown result type (might be due to invalid IL or missing references)
			//IL_0652: Expected O, but got Unknown
			//IL_0653: Unknown result type (might be due to invalid IL or missing references)
			//IL_065d: Expected O, but got Unknown
			//IL_065e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0668: Expected O, but got Unknown
			//IL_0669: Unknown result type (might be due to invalid IL or missing references)
			//IL_0673: Expected O, but got Unknown
			//IL_0674: Unknown result type (might be due to invalid IL or missing references)
			//IL_067e: Expected O, but got Unknown
			//IL_067f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0689: Expected O, but got Unknown
			//IL_0814: Unknown result type (might be due to invalid IL or missing references)
			//IL_081e: Expected O, but got Unknown
			//IL_2dd3: Unknown result type (might be due to invalid IL or missing references)
			//IL_2ddd: Expected O, but got Unknown
			//IL_2ec7: Unknown result type (might be due to invalid IL or missing references)
			//IL_2ed1: Expected O, but got Unknown
			//IL_2f90: Unknown result type (might be due to invalid IL or missing references)
			//IL_2f9a: Expected O, but got Unknown
			//IL_304d: Unknown result type (might be due to invalid IL or missing references)
			//IL_3057: Expected O, but got Unknown
			//IL_311e: Unknown result type (might be due to invalid IL or missing references)
			//IL_3128: Expected O, but got Unknown
			//IL_3900: Unknown result type (might be due to invalid IL or missing references)
			//IL_390a: Expected O, but got Unknown
			//IL_4b43: Unknown result type (might be due to invalid IL or missing references)
			//IL_4b4d: Expected O, but got Unknown
			//IL_4c4e: Unknown result type (might be due to invalid IL or missing references)
			//IL_4c58: Expected O, but got Unknown
			//IL_4e4f: Unknown result type (might be due to invalid IL or missing references)
			//IL_4e59: Expected O, but got Unknown
			//IL_4edb: Unknown result type (might be due to invalid IL or missing references)
			//IL_4ee5: Expected O, but got Unknown
			//IL_4f67: Unknown result type (might be due to invalid IL or missing references)
			//IL_4f71: Expected O, but got Unknown
			//IL_4ff3: Unknown result type (might be due to invalid IL or missing references)
			//IL_4ffd: Expected O, but got Unknown
			//IL_507f: Unknown result type (might be due to invalid IL or missing references)
			//IL_5089: Expected O, but got Unknown
			//IL_510b: Unknown result type (might be due to invalid IL or missing references)
			//IL_5115: Expected O, but got Unknown
			//IL_5197: Unknown result type (might be due to invalid IL or missing references)
			//IL_51a1: Expected O, but got Unknown
			//IL_56b5: Unknown result type (might be due to invalid IL or missing references)
			//IL_56bf: Expected O, but got Unknown
			//IL_578a: Unknown result type (might be due to invalid IL or missing references)
			//IL_5794: Expected O, but got Unknown
			//IL_5962: Unknown result type (might be due to invalid IL or missing references)
			//IL_596c: Expected O, but got Unknown
			//IL_59d3: Unknown result type (might be due to invalid IL or missing references)
			//IL_59dd: Expected O, but got Unknown
			components = new Container();
			lstPrediction = new ListBox();
			contextMenuStrip1 = new ContextMenuStrip(components);
			computeGrazeToolStripMenuItem = new ToolStripMenuItem();
			copySingleLineeventToolStripMenuItem = new ToolStripMenuItem();
			grazePathMapToolStripMenuItem = new ToolStripMenuItem();
			grazeProfileToolStripMenuItem = new ToolStripMenuItem();
			grazeProfileSetUpAnimatedGIFToolStripMenuItem = new ToolStripMenuItem();
			setUpAnimatedGIFToolStripMenuItem = new ToolStripMenuItem();
			createAnimatedGIFToolStripMenuItem = new ToolStripMenuItem();
			limitingMagnitudeTableToolStripMenuItem = new ToolStripMenuItem();
			moonMapToolStripMenuItem = new ToolStripMenuItem();
			placeEventInRecordingTimerToolStripMenuItem = new ToolStripMenuItem();
			placeNext10EventsIntoRecordingTimerToolStripMenuItem = new ToolStripMenuItem();
			selectVirtualDubRecordingDurationToolStripMenuItem = new ToolStripMenuItem();
			sToolStripMenuItem = new ToolStripMenuItem();
			secToolStripMenuItem = new ToolStripMenuItem();
			secToolStripMenuItem1 = new ToolStripMenuItem();
			secToolStripMenuItem2 = new ToolStripMenuItem();
			secToolStripMenuItem3 = new ToolStripMenuItem();
			multiSitePredictionToolStripMenuItem = new ToolStripMenuItem();
			reListOccultationsToolStripMenuItem = new ToolStripMenuItem();
			displayStarDetailsToolStripMenuItem = new ToolStripMenuItem();
			doubleStarDetailsToolStripMenuItem = new ToolStripMenuItem();
			showPastLightCurvesToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			worldMapToolStripMenuItem = new ToolStripMenuItem();
			viewInGoogleEarthToolStripMenuItem = new ToolStripMenuItem();
			saveGoogleEarthKMLFileToolStripMenuItem = new ToolStripMenuItem();
			saveGoogleMapsHTMFileToolStripMenuItem = new ToolStripMenuItem();
			saveInOtherMapFormatsToolStripMenuItem = new ToolStripMenuItem();
			cmxPrecisionMappingStreetsToolStripMenuItem = new ToolStripMenuItem();
			genforMakingESRIShapefilesToolStripMenuItem = new ToolStripMenuItem();
			gpxEasyGPSToolStripMenuItem = new ToolStripMenuItem();
			mifMSMapPointToolStripMenuItem = new ToolStripMenuItem();
			pltOziExplorerToolStripMenuItem = new ToolStripMenuItem();
			txtforDelormeStreetAtlasToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			computePredictionsForToolStripMenuItem = new ToolStripMenuItem();
			menuStrip1 = new MenuStrip();
			withPredictionToolStripMenuItem = new ToolStripMenuItem();
			showLunarEphemerisToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator3 = new ToolStripSeparator();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			showApparentstarPositioNnotJ2000ToolStripMenuItem = new ToolStripMenuItem();
			setOutputFilterToolStripMenuItem = new ToolStripMenuItem();
			magLimitAdjustmentToolStripMenuItem = new ToolStripMenuItem();
			toolStripMenuItem2 = new ToolStripMenuItem();
			toolStripMenuItem3 = new ToolStripMenuItem();
			toolStripMenuItem4 = new ToolStripMenuItem();
			toolStripMenuItem5 = new ToolStripMenuItem();
			toolStripMenuItem6 = new ToolStripMenuItem();
			toolStripMenuItem7 = new ToolStripMenuItem();
			toolStripMenuItem8 = new ToolStripMenuItem();
			toolStripMenuItem9 = new ToolStripMenuItem();
			showVDubTimerToolStripMenuItem = new ToolStripMenuItem();
			weatherForecastsToolStripMenuItem = new ToolStripMenuItem();
			dayWeatherForecastToolStripMenuItem1 = new ToolStripMenuItem();
			weatherMapsToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			label28 = new Label();
			cmbLunarDayEnd = new ComboBox();
			cmbLunarDayStart = new ComboBox();
			cmbLunarMonthEnd = new ComboBox();
			cmbLunarMonthStart = new ComboBox();
			updnLunarYearEnd = new NumericUpDown();
			updnLunarYearStart = new NumericUpDown();
			cmdCompute = new Button();
			cmbSiteFiles = new ComboBox();
			cmbSite = new ComboBox();
			pBar = new ProgressBar();
			pBar2 = new ProgressBar();
			grpXZ80 = new GroupBox();
			optXZ9 = new RadioButton();
			optXZ6 = new RadioButton();
			optXZ3 = new RadioButton();
			optZC = new RadioButton();
			optXZ = new RadioButton();
			cmdComputeGrazes = new Button();
			grpObjects = new GroupBox();
			chkDoublesOnly = new CheckBox();
			chkStars = new CheckBox();
			chkPlanets = new CheckBox();
			chkListGrazes = new CheckBox();
			chkAsteroids = new CheckBox();
			lblCurrentDate = new Label();
			cmdWorldMap = new Button();
			cmdMultiLocation = new Button();
			LabelRightClick = new Label();
			labelIntegration = new Label();
			cmdCancel = new Button();
			cmdHome = new Button();
			cmdHomeSet = new Button();
			groupBox1 = new GroupBox();
			panel1 = new Panel();
			cmdUseSingleSite = new Button();
			lblRange = new Label();
			chkFilter = new CheckBox();
			label6 = new Label();
			label5 = new Label();
			label4 = new Label();
			label1 = new Label();
			cmdFullYear = new Button();
			cmdFullMonth = new Button();
			cmdOneDay = new Button();
			button1 = new Button();
			label2 = new Label();
			label3 = new Label();
			label7 = new Label();
			label8 = new Label();
			groupBox3 = new GroupBox();
			label17 = new Label();
			radioButton4 = new RadioButton();
			radioButton3 = new RadioButton();
			radioButton2 = new RadioButton();
			radioButton1 = new RadioButton();
			groupBox2 = new GroupBox();
			chkFilterOutput = new CheckBox();
			chkAbbrev = new CheckBox();
			grpAnyWhere = new GroupBox();
			cmdFormat = new Button();
			grpSingleSite = new GroupBox();
			label14 = new Label();
			txtName = new TextBox();
			label37 = new Label();
			updnAperture = new NumericUpDown();
			label11 = new Label();
			label12 = new Label();
			label13 = new Label();
			txtLatS = new TextBox();
			txtLatM = new TextBox();
			txtLatD = new TextBox();
			txtLongS = new TextBox();
			txtLongM = new TextBox();
			txtLongD = new TextBox();
			txtAltitude = new TextBox();
			label24 = new Label();
			label25 = new Label();
			cmdUseSiteFiles = new Button();
			label20 = new Label();
			toolTip1 = new ToolTip(components);
			pnlMultiSiteOutput = new Panel();
			cmdMultiOK = new Button();
			label10 = new Label();
			label9 = new Label();
			chkShort = new CheckBox();
			chkLong = new CheckBox();
			((Control)contextMenuStrip1).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((ISupportInitialize)updnLunarYearEnd).BeginInit();
			((ISupportInitialize)updnLunarYearStart).BeginInit();
			((Control)grpXZ80).SuspendLayout();
			((Control)grpObjects).SuspendLayout();
			((Control)groupBox1).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((Control)groupBox3).SuspendLayout();
			((Control)groupBox2).SuspendLayout();
			((Control)grpAnyWhere).SuspendLayout();
			((Control)grpSingleSite).SuspendLayout();
			((ISupportInitialize)updnAperture).BeginInit();
			((Control)pnlMultiSiteOutput).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)lstPrediction).set_ContextMenuStrip(contextMenuStrip1);
			((Control)lstPrediction).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstPrediction).set_FormattingEnabled(true);
			lstPrediction.set_HorizontalExtent(1040);
			lstPrediction.set_HorizontalScrollbar(true);
			lstPrediction.set_ItemHeight(14);
			((Control)lstPrediction).set_Location(new Point(12, 150));
			((Control)lstPrediction).set_Name("lstPrediction");
			((Control)lstPrediction).set_Size(new Size(976, 382));
			((Control)lstPrediction).set_TabIndex(0);
			lstPrediction.add_SelectedIndexChanged((EventHandler)lstPrediction_SelectedIndexChanged);
			((Control)lstPrediction).add_DoubleClick((EventHandler)lstPrediction_DoubleClick);
			((Control)lstPrediction).add_MouseDown(new MouseEventHandler(lstPrediction_MouseDown));
			((ToolStrip)contextMenuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[23]
			{
				(ToolStripItem)computeGrazeToolStripMenuItem,
				(ToolStripItem)copySingleLineeventToolStripMenuItem,
				(ToolStripItem)grazePathMapToolStripMenuItem,
				(ToolStripItem)grazeProfileToolStripMenuItem,
				(ToolStripItem)grazeProfileSetUpAnimatedGIFToolStripMenuItem,
				(ToolStripItem)limitingMagnitudeTableToolStripMenuItem,
				(ToolStripItem)moonMapToolStripMenuItem,
				(ToolStripItem)placeEventInRecordingTimerToolStripMenuItem,
				(ToolStripItem)placeNext10EventsIntoRecordingTimerToolStripMenuItem,
				(ToolStripItem)selectVirtualDubRecordingDurationToolStripMenuItem,
				(ToolStripItem)multiSitePredictionToolStripMenuItem,
				(ToolStripItem)reListOccultationsToolStripMenuItem,
				(ToolStripItem)displayStarDetailsToolStripMenuItem,
				(ToolStripItem)doubleStarDetailsToolStripMenuItem,
				(ToolStripItem)showPastLightCurvesToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)worldMapToolStripMenuItem,
				(ToolStripItem)viewInGoogleEarthToolStripMenuItem,
				(ToolStripItem)saveGoogleEarthKMLFileToolStripMenuItem,
				(ToolStripItem)saveGoogleMapsHTMFileToolStripMenuItem,
				(ToolStripItem)saveInOtherMapFormatsToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)computePredictionsForToolStripMenuItem
			});
			((Control)contextMenuStrip1).set_Name("contextMenuStrip1");
			((Control)contextMenuStrip1).set_Size(new Size(304, 478));
			((ToolStripItem)computeGrazeToolStripMenuItem).set_Name("computeGrazeToolStripMenuItem");
			((ToolStripItem)computeGrazeToolStripMenuItem).set_Size(new Size(303, 22));
			((ToolStripItem)computeGrazeToolStripMenuItem).set_Text("Compute graze");
			((ToolStripItem)computeGrazeToolStripMenuItem).add_Click((EventHandler)computeGrazeToolStripMenuItem_Click);
			((ToolStripItem)copySingleLineeventToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copySingleLineeventToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copySingleLineeventToolStripMenuItem).set_Name("copySingleLineeventToolStripMenuItem");
			((ToolStripItem)copySingleLineeventToolStripMenuItem).set_Size(new Size(303, 22));
			((ToolStripItem)copySingleLineeventToolStripMenuItem).set_Text("Copy single line {event}");
			((ToolStripItem)copySingleLineeventToolStripMenuItem).add_Click((EventHandler)copySingleLineeventToolStripMenuItem_Click);
			((ToolStripItem)grazePathMapToolStripMenuItem).set_Name("grazePathMapToolStripMenuItem");
			((ToolStripItem)grazePathMapToolStripMenuItem).set_Size(new Size(303, 22));
			((ToolStripItem)grazePathMapToolStripMenuItem).set_Text("Graze path map");
			((ToolStripItem)grazePathMapToolStripMenuItem).add_Click((EventHandler)grazePathMapToolStripMenuItem_Click);
			((ToolStripItem)grazeProfileToolStripMenuItem).set_Image((Image)Resources.GrazeBW);
			((ToolStripItem)grazeProfileToolStripMenuItem).set_Name("grazeProfileToolStripMenuItem");
			((ToolStripItem)grazeProfileToolStripMenuItem).set_Size(new Size(303, 22));
			((ToolStripItem)grazeProfileToolStripMenuItem).set_Text("Graze Profile                     [at selected line]");
			((ToolStripItem)grazeProfileToolStripMenuItem).add_Click((EventHandler)grazeProfileToolStripMenuItem_Click);
			((ToolStripDropDownItem)grazeProfileSetUpAnimatedGIFToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)setUpAnimatedGIFToolStripMenuItem,
				(ToolStripItem)createAnimatedGIFToolStripMenuItem
			});
			((ToolStripItem)grazeProfileSetUpAnimatedGIFToolStripMenuItem).set_Name("grazeProfileSetUpAnimatedGIFToolStripMenuItem");
			((ToolStripItem)grazeProfileSetUpAnimatedGIFToolStripMenuItem).set_Size(new Size(303, 22));
			((ToolStripItem)grazeProfileSetUpAnimatedGIFToolStripMenuItem).set_Text("Graze profile - animated GIF");
			((ToolStripItem)setUpAnimatedGIFToolStripMenuItem).set_Name("setUpAnimatedGIFToolStripMenuItem");
			((ToolStripItem)setUpAnimatedGIFToolStripMenuItem).set_Size(new Size(181, 22));
			((ToolStripItem)setUpAnimatedGIFToolStripMenuItem).set_Text("Set up animated GIF");
			((ToolStripItem)setUpAnimatedGIFToolStripMenuItem).add_Click((EventHandler)setUpAnimatedGIFToolStripMenuItem_Click);
			((ToolStripItem)createAnimatedGIFToolStripMenuItem).set_Name("createAnimatedGIFToolStripMenuItem");
			((ToolStripItem)createAnimatedGIFToolStripMenuItem).set_Size(new Size(181, 22));
			((ToolStripItem)createAnimatedGIFToolStripMenuItem).set_Text("Create animated GIF");
			((ToolStripItem)createAnimatedGIFToolStripMenuItem).add_Click((EventHandler)createAnimatedGIFToolStripMenuItem_Click);
			((ToolStripItem)limitingMagnitudeTableToolStripMenuItem).set_Name("limitingMagnitudeTableToolStripMenuItem");
			((ToolStripItem)limitingMagnitudeTableToolStripMenuItem).set_Size(new Size(303, 22));
			((ToolStripItem)limitingMagnitudeTableToolStripMenuItem).set_Text("Limiting magnitude table   [at selected line]");
			((ToolStripItem)limitingMagnitudeTableToolStripMenuItem).add_Click((EventHandler)limitingMagnitudeTableToolStripMenuItem_Click);
			((ToolStripItem)moonMapToolStripMenuItem).set_Image((Image)Resources.MOON02);
			((ToolStripItem)moonMapToolStripMenuItem).set_Name("moonMapToolStripMenuItem");
			((ToolStripItem)moonMapToolStripMenuItem).set_Size(new Size(303, 22));
			((ToolStripItem)moonMapToolStripMenuItem).set_Text("Moon map");
			((ToolStripItem)moonMapToolStripMenuItem).add_Click((EventHandler)moonMapToolStripMenuItem_Click);
			((ToolStripItem)placeEventInRecordingTimerToolStripMenuItem).set_Image((Image)Resources.CLOCK02);
			((ToolStripItem)placeEventInRecordingTimerToolStripMenuItem).set_Name("placeEventInRecordingTimerToolStripMenuItem");
			((ToolStripItem)placeEventInRecordingTimerToolStripMenuItem).set_Size(new Size(303, 22));
			((ToolStripItem)placeEventInRecordingTimerToolStripMenuItem).set_Text("Place event into Recording Timer");
			((ToolStripItem)placeEventInRecordingTimerToolStripMenuItem).add_Click((EventHandler)placeEventInRecordingTimerToolStripMenuItem_Click);
			((ToolStripItem)placeNext10EventsIntoRecordingTimerToolStripMenuItem).set_Image((Image)Resources.CLOCK02);
			((ToolStripItem)placeNext10EventsIntoRecordingTimerToolStripMenuItem).set_Name("placeNext10EventsIntoRecordingTimerToolStripMenuItem");
			((ToolStripItem)placeNext10EventsIntoRecordingTimerToolStripMenuItem).set_Size(new Size(303, 22));
			((ToolStripItem)placeNext10EventsIntoRecordingTimerToolStripMenuItem).set_Text("Place next 10 events into Recording Timer");
			((ToolStripItem)placeNext10EventsIntoRecordingTimerToolStripMenuItem).add_Click((EventHandler)placeNext10EventsIntoRecordingTimerToolStripMenuItem_Click);
			((ToolStripDropDownItem)selectVirtualDubRecordingDurationToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)sToolStripMenuItem,
				(ToolStripItem)secToolStripMenuItem,
				(ToolStripItem)secToolStripMenuItem1,
				(ToolStripItem)secToolStripMenuItem2,
				(ToolStripItem)secToolStripMenuItem3
			});
			((ToolStripItem)selectVirtualDubRecordingDurationToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Italic));
			((ToolStripItem)selectVirtualDubRecordingDurationToolStripMenuItem).set_Name("selectVirtualDubRecordingDurationToolStripMenuItem");
			((ToolStripItem)selectVirtualDubRecordingDurationToolStripMenuItem).set_Size(new Size(303, 22));
			((ToolStripItem)selectVirtualDubRecordingDurationToolStripMenuItem).set_Text("Select time offsets for Recording Timer");
			((ToolStripItem)sToolStripMenuItem).set_Name("sToolStripMenuItem");
			((ToolStripItem)sToolStripMenuItem).set_Size(new Size(140, 22));
			((ToolStripItem)sToolStripMenuItem).set_Text("-3 sec,+2 sec");
			((ToolStripItem)sToolStripMenuItem).add_Click((EventHandler)sToolStripMenuItem_Click);
			((ToolStripItem)secToolStripMenuItem).set_Name("secToolStripMenuItem");
			((ToolStripItem)secToolStripMenuItem).set_Size(new Size(140, 22));
			((ToolStripItem)secToolStripMenuItem).set_Text("-4 sec,+3 sec");
			((ToolStripItem)secToolStripMenuItem).add_Click((EventHandler)secToolStripMenuItem_Click);
			((ToolStripItem)secToolStripMenuItem1).set_Name("secToolStripMenuItem1");
			((ToolStripItem)secToolStripMenuItem1).set_Size(new Size(140, 22));
			((ToolStripItem)secToolStripMenuItem1).set_Text("-5 sec,+4 sec");
			((ToolStripItem)secToolStripMenuItem1).add_Click((EventHandler)secToolStripMenuItem1_Click);
			((ToolStripItem)secToolStripMenuItem2).set_Name("secToolStripMenuItem2");
			((ToolStripItem)secToolStripMenuItem2).set_Size(new Size(140, 22));
			((ToolStripItem)secToolStripMenuItem2).set_Text("-6 sec,+5 sec");
			((ToolStripItem)secToolStripMenuItem2).add_Click((EventHandler)secToolStripMenuItem2_Click);
			((ToolStripItem)secToolStripMenuItem3).set_Name("secToolStripMenuItem3");
			((ToolStripItem)secToolStripMenuItem3).set_Size(new Size(140, 22));
			((ToolStripItem)secToolStripMenuItem3).set_Text("-7 sec,+6 sec");
			((ToolStripItem)secToolStripMenuItem3).add_Click((EventHandler)secToolStripMenuItem3_Click);
			((ToolStripItem)multiSitePredictionToolStripMenuItem).set_Image((Image)Resources.MultiLocation);
			((ToolStripItem)multiSitePredictionToolStripMenuItem).set_Name("multiSitePredictionToolStripMenuItem");
			((ToolStripItem)multiSitePredictionToolStripMenuItem).set_Size(new Size(303, 22));
			((ToolStripItem)multiSitePredictionToolStripMenuItem).set_Text("Multi-Site prediction");
			((ToolStripItem)multiSitePredictionToolStripMenuItem).add_Click((EventHandler)multiSitePredictionToolStripMenuItem_Click);
			((ToolStripItem)reListOccultationsToolStripMenuItem).set_Font(new Font("Tahoma", 8.25f, FontStyle.Bold));
			((ToolStripItem)reListOccultationsToolStripMenuItem).set_Name("reListOccultationsToolStripMenuItem");
			((ToolStripItem)reListOccultationsToolStripMenuItem).set_Size(new Size(303, 22));
			((ToolStripItem)reListOccultationsToolStripMenuItem).set_Text("ReList Occultations");
			((ToolStripItem)reListOccultationsToolStripMenuItem).add_Click((EventHandler)reListOccultationsToolStripMenuItem_Click);
			((ToolStripItem)displayStarDetailsToolStripMenuItem).set_Image((Image)Resources.action_create_16xLG);
			((ToolStripItem)displayStarDetailsToolStripMenuItem).set_Name("displayStarDetailsToolStripMenuItem");
			((ToolStripItem)displayStarDetailsToolStripMenuItem).set_Size(new Size(303, 22));
			((ToolStripItem)displayStarDetailsToolStripMenuItem).set_Text("Star details");
			((ToolStripItem)displayStarDetailsToolStripMenuItem).add_Click((EventHandler)displayStarDetailsToolStripMenuItem_Click);
			((ToolStripItem)doubleStarDetailsToolStripMenuItem).set_Name("doubleStarDetailsToolStripMenuItem");
			((ToolStripItem)doubleStarDetailsToolStripMenuItem).set_Size(new Size(303, 22));
			((ToolStripItem)doubleStarDetailsToolStripMenuItem).set_Text("Double and Variable details");
			((ToolStripItem)doubleStarDetailsToolStripMenuItem).add_Click((EventHandler)doubleStarDetailsToolStripMenuItem_Click);
			((ToolStripItem)showPastLightCurvesToolStripMenuItem).set_Name("showPastLightCurvesToolStripMenuItem");
			((ToolStripItem)showPastLightCurvesToolStripMenuItem).set_Size(new Size(303, 22));
			((ToolStripItem)showPastLightCurvesToolStripMenuItem).set_Text("Observed light curves");
			((ToolStripItem)showPastLightCurvesToolStripMenuItem).add_Click((EventHandler)showPastLightCurvesToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(300, 6));
			((ToolStripItem)worldMapToolStripMenuItem).set_Image((Image)Resources.EARTH);
			((ToolStripItem)worldMapToolStripMenuItem).set_Name("worldMapToolStripMenuItem");
			((ToolStripItem)worldMapToolStripMenuItem).set_Size(new Size(303, 22));
			((ToolStripItem)worldMapToolStripMenuItem).set_Text("World map");
			((ToolStripItem)worldMapToolStripMenuItem).add_Click((EventHandler)worldMapToolStripMenuItem_Click);
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem).set_Image((Image)Resources.google_earth);
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem).set_Name("viewInGoogleEarthToolStripMenuItem");
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem).set_Size(new Size(303, 22));
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem).set_Text("View in GoogleEarth");
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem).add_Click((EventHandler)viewInGoogleEarthToolStripMenuItem_Click);
			((ToolStripItem)saveGoogleEarthKMLFileToolStripMenuItem).set_Image((Image)Resources.kml_file);
			((ToolStripItem)saveGoogleEarthKMLFileToolStripMenuItem).set_Name("saveGoogleEarthKMLFileToolStripMenuItem");
			((ToolStripItem)saveGoogleEarthKMLFileToolStripMenuItem).set_Size(new Size(303, 22));
			((ToolStripItem)saveGoogleEarthKMLFileToolStripMenuItem).set_Text("Save GoogleEarth KMZ file");
			((ToolStripItem)saveGoogleEarthKMLFileToolStripMenuItem).add_Click((EventHandler)saveGoogleEarthKMLFileToolStripMenuItem_Click);
			((ToolStripItem)saveGoogleMapsHTMFileToolStripMenuItem).set_Image((Image)Resources.mm_20_red);
			((ToolStripItem)saveGoogleMapsHTMFileToolStripMenuItem).set_Name("saveGoogleMapsHTMFileToolStripMenuItem");
			((ToolStripItem)saveGoogleMapsHTMFileToolStripMenuItem).set_Size(new Size(303, 22));
			((ToolStripItem)saveGoogleMapsHTMFileToolStripMenuItem).set_Text("Save GoogleMaps HTM file");
			((ToolStripItem)saveGoogleMapsHTMFileToolStripMenuItem).add_Click((EventHandler)saveGoogleMapsHTMFileToolStripMenuItem_Click);
			((ToolStripDropDownItem)saveInOtherMapFormatsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)cmxPrecisionMappingStreetsToolStripMenuItem,
				(ToolStripItem)genforMakingESRIShapefilesToolStripMenuItem,
				(ToolStripItem)gpxEasyGPSToolStripMenuItem,
				(ToolStripItem)mifMSMapPointToolStripMenuItem,
				(ToolStripItem)pltOziExplorerToolStripMenuItem,
				(ToolStripItem)txtforDelormeStreetAtlasToolStripMenuItem
			});
			((ToolStripItem)saveInOtherMapFormatsToolStripMenuItem).set_Name("saveInOtherMapFormatsToolStripMenuItem");
			((ToolStripItem)saveInOtherMapFormatsToolStripMenuItem).set_Size(new Size(303, 22));
			((ToolStripItem)saveInOtherMapFormatsToolStripMenuItem).set_Text("Save in other map formats");
			((ToolStripItem)cmxPrecisionMappingStreetsToolStripMenuItem).set_Name("cmxPrecisionMappingStreetsToolStripMenuItem");
			((ToolStripItem)cmxPrecisionMappingStreetsToolStripMenuItem).set_Size(new Size(342, 38));
			((ToolStripItem)cmxPrecisionMappingStreetsToolStripMenuItem).set_Text(".cmx   [Precision Mapping Streets (zip to email)]");
			((ToolStripItem)cmxPrecisionMappingStreetsToolStripMenuItem).add_Click((EventHandler)cmxPrecisionMappingStreetsToolStripMenuItem_Click);
			((ToolStripItem)genforMakingESRIShapefilesToolStripMenuItem).set_Name("genforMakingESRIShapefilesToolStripMenuItem");
			((ToolStripItem)genforMakingESRIShapefilesToolStripMenuItem).set_Size(new Size(342, 38));
			((ToolStripItem)genforMakingESRIShapefilesToolStripMenuItem).set_Text(".gen   [for making ESRI shapefiles]");
			((ToolStripItem)genforMakingESRIShapefilesToolStripMenuItem).add_Click((EventHandler)genforMakingESRIShapefilesToolStripMenuItem_Click);
			((ToolStripItem)gpxEasyGPSToolStripMenuItem).set_Image((Image)Resources.easygps);
			((ToolStripItem)gpxEasyGPSToolStripMenuItem).set_ImageScaling((ToolStripItemImageScaling)0);
			((ToolStripItem)gpxEasyGPSToolStripMenuItem).set_Name("gpxEasyGPSToolStripMenuItem");
			((ToolStripItem)gpxEasyGPSToolStripMenuItem).set_Size(new Size(342, 38));
			((ToolStripItem)gpxEasyGPSToolStripMenuItem).set_Text(".gpx   [Easy GPS]");
			((ToolStripItem)gpxEasyGPSToolStripMenuItem).add_Click((EventHandler)gpxEasyGPSToolStripMenuItem_Click);
			((ToolStripItem)mifMSMapPointToolStripMenuItem).set_Image((Image)Resources.web_article_icons_mp_cd);
			((ToolStripItem)mifMSMapPointToolStripMenuItem).set_Name("mifMSMapPointToolStripMenuItem");
			((ToolStripItem)mifMSMapPointToolStripMenuItem).set_Size(new Size(342, 38));
			((ToolStripItem)mifMSMapPointToolStripMenuItem).set_Text(".mif    [MS MapPoint]");
			((ToolStripItem)mifMSMapPointToolStripMenuItem).add_Click((EventHandler)mifMSMapPointToolStripMenuItem_Click);
			((ToolStripItem)pltOziExplorerToolStripMenuItem).set_Name("pltOziExplorerToolStripMenuItem");
			((ToolStripItem)pltOziExplorerToolStripMenuItem).set_Size(new Size(342, 38));
			((ToolStripItem)pltOziExplorerToolStripMenuItem).set_Text(".plt     [OziExplorer]");
			((ToolStripItem)pltOziExplorerToolStripMenuItem).add_Click((EventHandler)pltOziExplorerToolStripMenuItem_Click);
			((ToolStripItem)txtforDelormeStreetAtlasToolStripMenuItem).set_Name("txtforDelormeStreetAtlasToolStripMenuItem");
			((ToolStripItem)txtforDelormeStreetAtlasToolStripMenuItem).set_Size(new Size(342, 38));
			((ToolStripItem)txtforDelormeStreetAtlasToolStripMenuItem).set_Text(".txt     [for Delorme Street Atlas]");
			((ToolStripItem)txtforDelormeStreetAtlasToolStripMenuItem).add_Click((EventHandler)txtforDelormeStreetAtlasToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(300, 6));
			((ToolStripItem)computePredictionsForToolStripMenuItem).set_Name("computePredictionsForToolStripMenuItem");
			((ToolStripItem)computePredictionsForToolStripMenuItem).set_Size(new Size(303, 22));
			((ToolStripItem)computePredictionsForToolStripMenuItem).set_Text("Compute predictions for");
			((ToolStripItem)computePredictionsForToolStripMenuItem).add_Click((EventHandler)computePredictionsForToolStripMenuItem_Click);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[7]
			{
				(ToolStripItem)withPredictionToolStripMenuItem,
				(ToolStripItem)setOutputFilterToolStripMenuItem,
				(ToolStripItem)magLimitAdjustmentToolStripMenuItem,
				(ToolStripItem)showVDubTimerToolStripMenuItem,
				(ToolStripItem)weatherForecastsToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(986, 24));
			((Control)menuStrip1).set_TabIndex(1);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withPredictionToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[7]
			{
				(ToolStripItem)showLunarEphemerisToolStripMenuItem,
				(ToolStripItem)toolStripSeparator3,
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem,
				(ToolStripItem)showApparentstarPositioNnotJ2000ToolStripMenuItem
			});
			((ToolStripItem)withPredictionToolStripMenuItem).set_Name("withPredictionToolStripMenuItem");
			((ToolStripItem)withPredictionToolStripMenuItem).set_Size(new Size(111, 20));
			((ToolStripItem)withPredictionToolStripMenuItem).set_Text("with Prediction ...");
			((ToolStripItem)showLunarEphemerisToolStripMenuItem).set_Name("showLunarEphemerisToolStripMenuItem");
			showLunarEphemerisToolStripMenuItem.set_ShortcutKeys((Keys)131141);
			((ToolStripItem)showLunarEphemerisToolStripMenuItem).set_Size(new Size(287, 22));
			((ToolStripItem)showLunarEphemerisToolStripMenuItem).set_Text("Show Lunar Ephemeris");
			((ToolStripItem)showLunarEphemerisToolStripMenuItem).add_Click((EventHandler)showLunarEphemerisToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator3).set_Name("toolStripSeparator3");
			((ToolStripItem)toolStripSeparator3).set_Size(new Size(284, 6));
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(287, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy all");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(287, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("&Print preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(287, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(287, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)showApparentstarPositioNnotJ2000ToolStripMenuItem).set_Name("showApparentstarPositioNnotJ2000ToolStripMenuItem");
			((ToolStripItem)showApparentstarPositioNnotJ2000ToolStripMenuItem).set_Size(new Size(287, 22));
			((ToolStripItem)showApparentstarPositioNnotJ2000ToolStripMenuItem).set_Text("Show Apparent star position (not J2000))");
			((ToolStripItem)showApparentstarPositioNnotJ2000ToolStripMenuItem).add_Click((EventHandler)showApparentstarPositioNnotJ2000ToolStripMenuItem_Click);
			((ToolStripItem)setOutputFilterToolStripMenuItem).set_Image((Image)Resources.Filter2HS);
			((ToolStripItem)setOutputFilterToolStripMenuItem).set_Name("setOutputFilterToolStripMenuItem");
			((ToolStripItem)setOutputFilterToolStripMenuItem).set_Size(new Size(134, 20));
			((ToolStripItem)setOutputFilterToolStripMenuItem).set_Text("Set Output filter     ");
			((ToolStripItem)setOutputFilterToolStripMenuItem).add_Click((EventHandler)setOutputFilterToolStripMenuItem_Click);
			((ToolStripDropDownItem)magLimitAdjustmentToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[8]
			{
				(ToolStripItem)toolStripMenuItem2,
				(ToolStripItem)toolStripMenuItem3,
				(ToolStripItem)toolStripMenuItem4,
				(ToolStripItem)toolStripMenuItem5,
				(ToolStripItem)toolStripMenuItem6,
				(ToolStripItem)toolStripMenuItem7,
				(ToolStripItem)toolStripMenuItem8,
				(ToolStripItem)toolStripMenuItem9
			});
			((ToolStripItem)magLimitAdjustmentToolStripMenuItem).set_Image((Image)Resources.Escalator);
			((ToolStripItem)magLimitAdjustmentToolStripMenuItem).set_Name("magLimitAdjustmentToolStripMenuItem");
			((ToolStripItem)magLimitAdjustmentToolStripMenuItem).set_Size(new Size(173, 20));
			((ToolStripItem)magLimitAdjustmentToolStripMenuItem).set_Text("Mag limit adjustment...     ");
			((ToolStripItem)toolStripMenuItem2).set_Name("toolStripMenuItem2");
			((ToolStripItem)toolStripMenuItem2).set_Size(new Size(86, 22));
			((ToolStripItem)toolStripMenuItem2).set_Text("-1");
			((ToolStripItem)toolStripMenuItem2).add_Click((EventHandler)toolStripMenuItem2_Click);
			toolStripMenuItem3.set_Checked(true);
			toolStripMenuItem3.set_CheckState((CheckState)1);
			((ToolStripItem)toolStripMenuItem3).set_Name("toolStripMenuItem3");
			((ToolStripItem)toolStripMenuItem3).set_Size(new Size(86, 22));
			((ToolStripItem)toolStripMenuItem3).set_Text("0");
			((ToolStripItem)toolStripMenuItem3).add_Click((EventHandler)toolStripMenuItem3_Click);
			((ToolStripItem)toolStripMenuItem4).set_Name("toolStripMenuItem4");
			((ToolStripItem)toolStripMenuItem4).set_Size(new Size(86, 22));
			((ToolStripItem)toolStripMenuItem4).set_Text("1");
			((ToolStripItem)toolStripMenuItem4).add_Click((EventHandler)toolStripMenuItem4_Click);
			((ToolStripItem)toolStripMenuItem5).set_Name("toolStripMenuItem5");
			((ToolStripItem)toolStripMenuItem5).set_Size(new Size(86, 22));
			((ToolStripItem)toolStripMenuItem5).set_Text("2");
			((ToolStripItem)toolStripMenuItem5).add_Click((EventHandler)toolStripMenuItem5_Click);
			((ToolStripItem)toolStripMenuItem6).set_Name("toolStripMenuItem6");
			((ToolStripItem)toolStripMenuItem6).set_Size(new Size(86, 22));
			((ToolStripItem)toolStripMenuItem6).set_Text("3");
			((ToolStripItem)toolStripMenuItem6).add_Click((EventHandler)toolStripMenuItem6_Click);
			((ToolStripItem)toolStripMenuItem7).set_Name("toolStripMenuItem7");
			((ToolStripItem)toolStripMenuItem7).set_Size(new Size(86, 22));
			((ToolStripItem)toolStripMenuItem7).set_Text("4");
			((ToolStripItem)toolStripMenuItem7).add_Click((EventHandler)toolStripMenuItem7_Click);
			((ToolStripItem)toolStripMenuItem8).set_Name("toolStripMenuItem8");
			((ToolStripItem)toolStripMenuItem8).set_Size(new Size(86, 22));
			((ToolStripItem)toolStripMenuItem8).set_Text("5");
			((ToolStripItem)toolStripMenuItem8).add_Click((EventHandler)toolStripMenuItem8_Click);
			((ToolStripItem)toolStripMenuItem9).set_Name("toolStripMenuItem9");
			((ToolStripItem)toolStripMenuItem9).set_Size(new Size(86, 22));
			((ToolStripItem)toolStripMenuItem9).set_Text("10");
			((ToolStripItem)toolStripMenuItem9).add_Click((EventHandler)toolStripMenuItem9_Click);
			((ToolStripItem)showVDubTimerToolStripMenuItem).set_Image((Image)Resources.CLOCK02);
			((ToolStripItem)showVDubTimerToolStripMenuItem).set_Name("showVDubTimerToolStripMenuItem");
			((ToolStripItem)showVDubTimerToolStripMenuItem).set_Size(new Size(165, 20));
			((ToolStripItem)showVDubTimerToolStripMenuItem).set_Text("show Recording Timer    ");
			((ToolStripItem)showVDubTimerToolStripMenuItem).add_Click((EventHandler)showRecordingTimerToolStripMenuItem_Click);
			((ToolStripDropDownItem)weatherForecastsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)dayWeatherForecastToolStripMenuItem1,
				(ToolStripItem)weatherMapsToolStripMenuItem
			});
			((ToolStripItem)weatherForecastsToolStripMenuItem).set_Image((Image)Resources.CLOUD);
			((ToolStripItem)weatherForecastsToolStripMenuItem).set_Name("weatherForecastsToolStripMenuItem");
			((ToolStripItem)weatherForecastsToolStripMenuItem).set_Size(new Size(147, 20));
			((ToolStripItem)weatherForecastsToolStripMenuItem).set_Text("Weather forecasts...   ");
			((ToolStripItem)dayWeatherForecastToolStripMenuItem1).set_Name("dayWeatherForecastToolStripMenuItem1");
			((ToolStripItem)dayWeatherForecastToolStripMenuItem1).set_Size(new Size(209, 22));
			((ToolStripItem)dayWeatherForecastToolStripMenuItem1).set_Text("3-day weather forecast");
			((ToolStripItem)dayWeatherForecastToolStripMenuItem1).add_Click((EventHandler)dayWeatherForecastToolStripMenuItem1_Click);
			((ToolStripItem)weatherMapsToolStripMenuItem).set_Name("weatherMapsToolStripMenuItem");
			((ToolStripItem)weatherMapsToolStripMenuItem).set_Size(new Size(209, 22));
			((ToolStripItem)weatherMapsToolStripMenuItem).set_Text("Regional 16-day forecasts");
			((ToolStripItem)weatherMapsToolStripMenuItem).add_Click((EventHandler)weatherMapsToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(69, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help   ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)label28).set_AutoSize(true);
			((Control)label28).set_Location(new Point(7, 59));
			((Control)label28).set_Name("label28");
			((Control)label28).set_Size(new Size(26, 13));
			((Control)label28).set_TabIndex(41);
			((Control)label28).set_Text("End");
			cmbLunarDayEnd.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbLunarDayEnd).set_FormattingEnabled(true);
			cmbLunarDayEnd.get_Items().AddRange(new object[31]
			{
				"1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
				"11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
				"21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
				"31"
			});
			((Control)cmbLunarDayEnd).set_Location(new Point(129, 55));
			cmbLunarDayEnd.set_MaxDropDownItems(31);
			((Control)cmbLunarDayEnd).set_Name("cmbLunarDayEnd");
			((Control)cmbLunarDayEnd).set_Size(new Size(39, 21));
			((Control)cmbLunarDayEnd).set_TabIndex(44);
			cmbLunarDayEnd.add_SelectedIndexChanged((EventHandler)cmbLunarDayEnd_SelectedIndexChanged);
			cmbLunarDayStart.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbLunarDayStart).set_FormattingEnabled(true);
			cmbLunarDayStart.get_Items().AddRange(new object[31]
			{
				"1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
				"11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
				"21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
				"31"
			});
			((Control)cmbLunarDayStart).set_Location(new Point(128, 26));
			cmbLunarDayStart.set_MaxDropDownItems(31);
			((Control)cmbLunarDayStart).set_Name("cmbLunarDayStart");
			((Control)cmbLunarDayStart).set_Size(new Size(40, 21));
			((Control)cmbLunarDayStart).set_TabIndex(40);
			cmbLunarDayStart.add_SelectedIndexChanged((EventHandler)cmbLunarDayStart_SelectedIndexChanged);
			cmbLunarMonthEnd.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbLunarMonthEnd).set_FormattingEnabled(true);
			cmbLunarMonthEnd.get_Items().AddRange(new object[12]
			{
				"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
				"Nov", "Dec"
			});
			((Control)cmbLunarMonthEnd).set_Location(new Point(85, 55));
			cmbLunarMonthEnd.set_MaxDropDownItems(12);
			((Control)cmbLunarMonthEnd).set_Name("cmbLunarMonthEnd");
			((Control)cmbLunarMonthEnd).set_Size(new Size(44, 21));
			((Control)cmbLunarMonthEnd).set_TabIndex(43);
			cmbLunarMonthEnd.add_SelectedIndexChanged((EventHandler)cmbLunarMonthEnd_SelectedIndexChanged);
			cmbLunarMonthStart.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbLunarMonthStart).set_FormattingEnabled(true);
			cmbLunarMonthStart.get_Items().AddRange(new object[12]
			{
				"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
				"Nov", "Dec"
			});
			((Control)cmbLunarMonthStart).set_Location(new Point(85, 26));
			cmbLunarMonthStart.set_MaxDropDownItems(12);
			((Control)cmbLunarMonthStart).set_Name("cmbLunarMonthStart");
			((Control)cmbLunarMonthStart).set_Size(new Size(43, 21));
			((Control)cmbLunarMonthStart).set_TabIndex(39);
			cmbLunarMonthStart.add_SelectedIndexChanged((EventHandler)cmbLunarMonthStart_SelectedIndexChanged);
			((Control)updnLunarYearEnd).set_Location(new Point(34, 56));
			updnLunarYearEnd.set_Maximum(new decimal(new int[4] { 17000, 0, 0, 0 }));
			updnLunarYearEnd.set_Minimum(new decimal(new int[4] { 13000, 0, 0, -2147483648 }));
			((Control)updnLunarYearEnd).set_Name("updnLunarYearEnd");
			((Control)updnLunarYearEnd).set_Size(new Size(51, 20));
			((Control)updnLunarYearEnd).set_TabIndex(42);
			updnLunarYearEnd.set_Value(new decimal(new int[4] { 1800, 0, 0, 0 }));
			updnLunarYearEnd.add_ValueChanged((EventHandler)updnLunarYearEnd_ValueChanged);
			((Control)updnLunarYearStart).set_Location(new Point(34, 27));
			updnLunarYearStart.set_Maximum(new decimal(new int[4] { 17000, 0, 0, 0 }));
			updnLunarYearStart.set_Minimum(new decimal(new int[4] { 13000, 0, 0, -2147483648 }));
			((Control)updnLunarYearStart).set_Name("updnLunarYearStart");
			((Control)updnLunarYearStart).set_Size(new Size(51, 20));
			((Control)updnLunarYearStart).set_TabIndex(38);
			updnLunarYearStart.set_Value(new decimal(new int[4] { 1800, 0, 0, 0 }));
			updnLunarYearStart.add_ValueChanged((EventHandler)updnLunarYearStart_ValueChanged);
			((Control)cmdCompute).set_Location(new Point(9, 16));
			((Control)cmdCompute).set_Name("cmdCompute");
			((Control)cmdCompute).set_Size(new Size(85, 47));
			((Control)cmdCompute).set_TabIndex(45);
			((Control)cmdCompute).set_Text("&Occultations");
			((ButtonBase)cmdCompute).set_UseVisualStyleBackColor(true);
			((Control)cmdCompute).add_Click((EventHandler)cmdCompute_Click);
			cmbSiteFiles.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbSiteFiles).set_FormattingEnabled(true);
			((Control)cmbSiteFiles).set_Location(new Point(76, 17));
			cmbSiteFiles.set_MaxDropDownItems(20);
			((Control)cmbSiteFiles).set_Name("cmbSiteFiles");
			((Control)cmbSiteFiles).set_Size(new Size(132, 21));
			cmbSiteFiles.set_Sorted(true);
			((Control)cmbSiteFiles).set_TabIndex(46);
			cmbSiteFiles.add_SelectedIndexChanged((EventHandler)cmbSiteFiles_SelectedIndexChanged);
			cmbSite.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbSite).set_FormattingEnabled(true);
			((Control)cmbSite).set_Location(new Point(76, 58));
			cmbSite.set_MaxDropDownItems(20);
			((Control)cmbSite).set_Name("cmbSite");
			((Control)cmbSite).set_Size(new Size(132, 21));
			((Control)cmbSite).set_TabIndex(47);
			cmbSite.add_SelectedIndexChanged((EventHandler)cmbSite_SelectedIndexChanged);
			((Control)pBar).set_Anchor((AnchorStyles)1);
			((Control)pBar).set_Location(new Point(845, 110));
			((Control)pBar).set_Name("pBar");
			((Control)pBar).set_Size(new Size(121, 10));
			((Control)pBar).set_TabIndex(48);
			((Control)pBar).set_Visible(false);
			((Control)pBar2).set_Anchor((AnchorStyles)1);
			((Control)pBar2).set_Location(new Point(845, 121));
			((Control)pBar2).set_Name("pBar2");
			((Control)pBar2).set_Size(new Size(122, 10));
			((Control)pBar2).set_TabIndex(49);
			((Control)pBar2).set_Visible(false);
			((Control)grpXZ80).set_Anchor((AnchorStyles)1);
			((Control)grpXZ80).set_BackColor(Color.Azure);
			((Control)grpXZ80).get_Controls().Add((Control)(object)optXZ9);
			((Control)grpXZ80).get_Controls().Add((Control)(object)optXZ6);
			((Control)grpXZ80).get_Controls().Add((Control)(object)optXZ3);
			((Control)grpXZ80).get_Controls().Add((Control)(object)optZC);
			((Control)grpXZ80).get_Controls().Add((Control)(object)optXZ);
			((Control)grpXZ80).set_Location(new Point(247, 27));
			((Control)grpXZ80).set_Name("grpXZ80");
			((Control)grpXZ80).set_Size(new Size(90, 108));
			((Control)grpXZ80).set_TabIndex(50);
			grpXZ80.set_TabStop(false);
			((Control)grpXZ80).set_Text("2. Star cat.");
			((Control)optXZ9).set_AutoSize(true);
			((Control)optXZ9).set_Location(new Point(5, 35));
			((Control)optXZ9).set_Name("optXZ9");
			((Control)optXZ9).set_Size(new Size(83, 17));
			((Control)optXZ9).set_TabIndex(4);
			((Control)optXZ9).set_Text("XZ  < mag 9");
			((ButtonBase)optXZ9).set_UseVisualStyleBackColor(true);
			optXZ9.add_CheckedChanged((EventHandler)optXZ9_CheckedChanged);
			((Control)optXZ6).set_AutoSize(true);
			((Control)optXZ6).set_Location(new Point(5, 52));
			((Control)optXZ6).set_Name("optXZ6");
			((Control)optXZ6).set_Size(new Size(83, 17));
			((Control)optXZ6).set_TabIndex(3);
			((Control)optXZ6).set_Text("XZ  < mag 7");
			((ButtonBase)optXZ6).set_UseVisualStyleBackColor(true);
			optXZ6.add_CheckedChanged((EventHandler)optXZ6_CheckedChanged);
			((Control)optXZ3).set_AutoSize(true);
			((Control)optXZ3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optXZ3).set_Location(new Point(5, 69));
			((Control)optXZ3).set_Name("optXZ3");
			((Control)optXZ3).set_Size(new Size(83, 17));
			((Control)optXZ3).set_TabIndex(2);
			((Control)optXZ3).set_Text("XZ  < mag 4");
			((ButtonBase)optXZ3).set_UseVisualStyleBackColor(true);
			optXZ3.add_CheckedChanged((EventHandler)optXZ3_CheckedChanged);
			((Control)optZC).set_AutoSize(true);
			((Control)optZC).set_Location(new Point(5, 86));
			((Control)optZC).set_Name("optZC");
			((Control)optZC).set_Size(new Size(39, 17));
			((Control)optZC).set_TabIndex(1);
			((Control)optZC).set_Text("ZC");
			((ButtonBase)optZC).set_UseVisualStyleBackColor(true);
			optZC.add_CheckedChanged((EventHandler)optZC_CheckedChanged);
			((Control)optXZ).set_AutoSize(true);
			optXZ.set_Checked(true);
			((Control)optXZ).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optXZ).set_Location(new Point(5, 18));
			((Control)optXZ).set_Name("optXZ");
			((Control)optXZ).set_Size(new Size(41, 17));
			((Control)optXZ).set_TabIndex(0);
			optXZ.set_TabStop(true);
			((Control)optXZ).set_Text("XZ");
			((ButtonBase)optXZ).set_UseVisualStyleBackColor(true);
			optXZ.add_CheckedChanged((EventHandler)optXZ_CheckedChanged);
			((Control)cmdComputeGrazes).set_Location(new Point(4, 14));
			((Control)cmdComputeGrazes).set_Name("cmdComputeGrazes");
			((Control)cmdComputeGrazes).set_Size(new Size(59, 47));
			((Control)cmdComputeGrazes).set_TabIndex(51);
			((Control)cmdComputeGrazes).set_Text("&Grazes");
			((ButtonBase)cmdComputeGrazes).set_UseVisualStyleBackColor(true);
			((Control)cmdComputeGrazes).add_Click((EventHandler)cmdComputeGrazes_Click);
			((Control)grpObjects).set_Anchor((AnchorStyles)1);
			((Control)grpObjects).set_BackColor(Color.LavenderBlush);
			((Control)grpObjects).get_Controls().Add((Control)(object)chkDoublesOnly);
			((Control)grpObjects).get_Controls().Add((Control)(object)chkStars);
			((Control)grpObjects).get_Controls().Add((Control)(object)chkPlanets);
			((Control)grpObjects).get_Controls().Add((Control)(object)chkListGrazes);
			((Control)grpObjects).get_Controls().Add((Control)(object)chkAsteroids);
			((Control)grpObjects).set_Location(new Point(340, 27));
			((Control)grpObjects).set_Name("grpObjects");
			((Control)grpObjects).set_Size(new Size(88, 108));
			((Control)grpObjects).set_TabIndex(54);
			grpObjects.set_TabStop(false);
			((Control)grpObjects).set_Text("3. Objects");
			((Control)chkDoublesOnly).set_AutoSize(true);
			chkDoublesOnly.set_Checked(Settings.Default.DoublesOnly_LunarSearch);
			((Control)chkDoublesOnly).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "DoublesOnly_LunarSearch", true, (DataSourceUpdateMode)1));
			((Control)chkDoublesOnly).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkDoublesOnly).set_Location(new Point(3, 89));
			((Control)chkDoublesOnly).set_Name("chkDoublesOnly");
			((Control)chkDoublesOnly).set_Size(new Size(87, 17));
			((Control)chkDoublesOnly).set_TabIndex(57);
			((Control)chkDoublesOnly).set_Text("Doubles only");
			((ButtonBase)chkDoublesOnly).set_TextAlign(ContentAlignment.MiddleRight);
			((ButtonBase)chkDoublesOnly).set_UseVisualStyleBackColor(true);
			chkDoublesOnly.add_CheckedChanged((EventHandler)chkDoublesOnly_CheckedChanged);
			((Control)chkStars).set_AutoSize(true);
			chkStars.set_Checked(Settings.Default.Lunar_IncludeStars);
			chkStars.set_CheckState((CheckState)1);
			((Control)chkStars).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "Lunar_IncludeStars", true, (DataSourceUpdateMode)1));
			((Control)chkStars).set_Location(new Point(3, 16));
			((Control)chkStars).set_Name("chkStars");
			((Control)chkStars).set_Size(new Size(50, 17));
			((Control)chkStars).set_TabIndex(0);
			((Control)chkStars).set_Text("Stars");
			((ButtonBase)chkStars).set_UseVisualStyleBackColor(true);
			chkStars.add_CheckedChanged((EventHandler)chkStars_CheckedChanged);
			((Control)chkPlanets).set_AutoSize(true);
			chkPlanets.set_Checked(Settings.Default.Lunar_IncludePlanets);
			chkPlanets.set_CheckState((CheckState)1);
			((Control)chkPlanets).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "Lunar_IncludePlanets", true, (DataSourceUpdateMode)1));
			((Control)chkPlanets).set_Location(new Point(3, 33));
			((Control)chkPlanets).set_Name("chkPlanets");
			((Control)chkPlanets).set_Size(new Size(61, 17));
			((Control)chkPlanets).set_TabIndex(1);
			((Control)chkPlanets).set_Text("Planets");
			((ButtonBase)chkPlanets).set_UseVisualStyleBackColor(true);
			chkPlanets.add_CheckedChanged((EventHandler)chkPlanets_CheckedChanged);
			((Control)chkListGrazes).set_AutoSize(true);
			chkListGrazes.set_Checked(Settings.Default.GrazesOnly_LunarSearch);
			((Control)chkListGrazes).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "GrazesOnly_LunarSearch", true, (DataSourceUpdateMode)1));
			((Control)chkListGrazes).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkListGrazes).set_Location(new Point(3, 73));
			((Control)chkListGrazes).set_Name("chkListGrazes");
			((Control)chkListGrazes).set_Size(new Size(81, 17));
			((Control)chkListGrazes).set_TabIndex(55);
			((Control)chkListGrazes).set_Text("Grazes only");
			((ButtonBase)chkListGrazes).set_TextAlign(ContentAlignment.MiddleRight);
			((ButtonBase)chkListGrazes).set_UseVisualStyleBackColor(true);
			((Control)chkAsteroids).set_AutoSize(true);
			chkAsteroids.set_Checked(Settings.Default.Lunar_IncludeAsteroids);
			((Control)chkAsteroids).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "Lunar_IncludeAsteroids", true, (DataSourceUpdateMode)1));
			((Control)chkAsteroids).set_Location(new Point(3, 50));
			((Control)chkAsteroids).set_Name("chkAsteroids");
			((Control)chkAsteroids).set_Size(new Size(69, 17));
			((Control)chkAsteroids).set_TabIndex(2);
			((Control)chkAsteroids).set_Text("Asteroids");
			((ButtonBase)chkAsteroids).set_UseVisualStyleBackColor(true);
			chkAsteroids.add_CheckedChanged((EventHandler)chkAsteroids_CheckedChanged);
			((Control)lblCurrentDate).set_Anchor((AnchorStyles)1);
			((Control)lblCurrentDate).set_AutoSize(true);
			((Control)lblCurrentDate).set_Location(new Point(894, 133));
			((Control)lblCurrentDate).set_Name("lblCurrentDate");
			((Control)lblCurrentDate).set_Size(new Size(13, 13));
			((Control)lblCurrentDate).set_TabIndex(56);
			((Control)lblCurrentDate).set_Text("[]");
			((Control)cmdWorldMap).set_Location(new Point(131, 14));
			((Control)cmdWorldMap).set_Name("cmdWorldMap");
			((Control)cmdWorldMap).set_Size(new Size(59, 47));
			((Control)cmdWorldMap).set_TabIndex(59);
			((Control)cmdWorldMap).set_Text("World\r\nmap");
			((ButtonBase)cmdWorldMap).set_UseVisualStyleBackColor(true);
			((Control)cmdWorldMap).add_Click((EventHandler)cmdWorldMap_Click);
			((Control)cmdMultiLocation).set_Location(new Point(67, 14));
			((Control)cmdMultiLocation).set_Name("cmdMultiLocation");
			((Control)cmdMultiLocation).set_Size(new Size(60, 34));
			((Control)cmdMultiLocation).set_TabIndex(61);
			((Control)cmdMultiLocation).set_Text("Multi-site\r\nfor 1 star");
			((ButtonBase)cmdMultiLocation).set_UseVisualStyleBackColor(true);
			((Control)cmdMultiLocation).add_Click((EventHandler)cmdMultiLocation_Click);
			((Control)LabelRightClick).set_AutoSize(true);
			((Control)LabelRightClick).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)LabelRightClick).set_Location(new Point(6, 134));
			((Control)LabelRightClick).set_Name("LabelRightClick");
			((Control)LabelRightClick).set_Size(new Size(246, 13));
			((Control)LabelRightClick).set_TabIndex(62);
			((Control)LabelRightClick).set_Text("Right-click on prediction for further options");
			((Control)labelIntegration).set_AutoSize(true);
			((Control)labelIntegration).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)labelIntegration).set_Location(new Point(290, 135));
			((Control)labelIntegration).set_Name("labelIntegration");
			((Control)labelIntegration).set_Size(new Size(138, 12));
			((Control)labelIntegration).set_TabIndex(64);
			((Control)labelIntegration).set_Text("Asteroid elements not integrated");
			((Control)labelIntegration).set_Visible(false);
			((Control)cmdCancel).set_Anchor((AnchorStyles)1);
			((Control)cmdCancel).set_Location(new Point(774, 108));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(59, 34));
			((Control)cmdCancel).set_TabIndex(66);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).set_Visible(false);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)cmdHome).set_BackColor(Color.PaleGreen);
			((ButtonBase)cmdHome).get_FlatAppearance().set_BorderSize(2);
			((Control)cmdHome).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdHome).set_Location(new Point(1, 3));
			((Control)cmdHome).set_Name("cmdHome");
			((Control)cmdHome).set_Size(new Size(56, 21));
			((Control)cmdHome).set_TabIndex(67);
			((Control)cmdHome).set_Text("Use home");
			((ButtonBase)cmdHome).set_UseVisualStyleBackColor(false);
			((Control)cmdHome).add_Click((EventHandler)cmdHome_Click);
			((Control)cmdHomeSet).set_BackColor(Color.Khaki);
			((Control)cmdHomeSet).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdHomeSet).set_Location(new Point(1, 28));
			((Control)cmdHomeSet).set_Name("cmdHomeSet");
			((Control)cmdHomeSet).set_Size(new Size(56, 21));
			((Control)cmdHomeSet).set_TabIndex(68);
			((Control)cmdHomeSet).set_Text("Set home");
			((ButtonBase)cmdHomeSet).set_UseVisualStyleBackColor(false);
			((Control)cmdHomeSet).add_Click((EventHandler)cmdHomeSet_Click);
			((Control)groupBox1).set_Anchor((AnchorStyles)1);
			((Control)groupBox1).set_BackColor(Color.PeachPuff);
			((Control)groupBox1).get_Controls().Add((Control)(object)panel1);
			((Control)groupBox1).get_Controls().Add((Control)(object)lblRange);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkFilter);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmbSite);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmbSiteFiles);
			((Control)groupBox1).set_Location(new Point(21, 27));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(223, 108));
			((Control)groupBox1).set_TabIndex(69);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("1.  Select site for predictions");
			((Control)panel1).set_BackColor(Color.Yellow);
			panel1.set_BorderStyle((BorderStyle)1);
			((Control)panel1).get_Controls().Add((Control)(object)cmdHomeSet);
			((Control)panel1).get_Controls().Add((Control)(object)cmdHome);
			((Control)panel1).get_Controls().Add((Control)(object)cmdUseSingleSite);
			((Control)panel1).set_Location(new Point(4, 19));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(60, 79));
			((Control)panel1).set_TabIndex(73);
			((Control)cmdUseSingleSite).set_BackColor(Color.PowderBlue);
			((Control)cmdUseSingleSite).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdUseSingleSite).set_Location(new Point(1, 53));
			((Control)cmdUseSingleSite).set_Name("cmdUseSingleSite");
			((Control)cmdUseSingleSite).set_Size(new Size(56, 21));
			((Control)cmdUseSingleSite).set_TabIndex(68);
			((Control)cmdUseSingleSite).set_Text("Use single");
			((ButtonBase)cmdUseSingleSite).set_UseVisualStyleBackColor(false);
			((Control)cmdUseSingleSite).add_Click((EventHandler)cmdUseSingleSite_Click);
			((Control)lblRange).set_AutoSize(true);
			((Control)lblRange).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblRange).set_Location(new Point(74, 39));
			((Control)lblRange).set_Name("lblRange");
			((Control)lblRange).set_Size(new Size(70, 12));
			((Control)lblRange).set_TabIndex(72);
			((Control)lblRange).set_Text("Long, lat ranges");
			((Control)chkFilter).set_AutoSize(true);
			chkFilter.set_Checked(Settings.Default.LunarElements_FilterBySite);
			chkFilter.set_CheckState((CheckState)1);
			((Control)chkFilter).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "LunarElements_FilterBySite", true, (DataSourceUpdateMode)1));
			((Control)chkFilter).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkFilter).set_Location(new Point(77, 85));
			((Control)chkFilter).set_Name("chkFilter");
			((Control)chkFilter).set_Size(new Size(145, 17));
			((Control)chkFilter).set_TabIndex(65);
			((Control)chkFilter).set_Text("Filter search to sites in file");
			((ButtonBase)chkFilter).set_UseVisualStyleBackColor(true);
			chkFilter.add_CheckedChanged((EventHandler)chkFilter_CheckedChanged);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(126, 13));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(26, 13));
			((Control)label6).set_TabIndex(102);
			((Control)label6).set_Text("Day");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(81, 13));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(37, 13));
			((Control)label5).set_TabIndex(101);
			((Control)label5).set_Text("Month");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(35, 13));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(29, 13));
			((Control)label4).set_TabIndex(100);
			((Control)label4).set_Text("Year");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(4, 30));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(29, 13));
			((Control)label1).set_TabIndex(103);
			((Control)label1).set_Text("Start");
			((Control)cmdFullYear).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdFullYear).set_Location(new Point(18, 95));
			((Control)cmdFullYear).set_Name("cmdFullYear");
			((Control)cmdFullYear).set_Size(new Size(29, 17));
			((Control)cmdFullYear).set_TabIndex(104);
			((ButtonBase)cmdFullYear).set_UseVisualStyleBackColor(true);
			((Control)cmdFullYear).add_Click((EventHandler)cmdFullYear_Click);
			((Control)cmdFullMonth).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdFullMonth).set_Location(new Point(56, 95));
			((Control)cmdFullMonth).set_Name("cmdFullMonth");
			((Control)cmdFullMonth).set_Size(new Size(29, 17));
			((Control)cmdFullMonth).set_TabIndex(105);
			((ButtonBase)cmdFullMonth).set_UseVisualStyleBackColor(true);
			((Control)cmdFullMonth).add_Click((EventHandler)cmdFullMonth_Click);
			((Control)cmdOneDay).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdOneDay).set_Location(new Point(94, 95));
			((Control)cmdOneDay).set_Name("cmdOneDay");
			((Control)cmdOneDay).set_Size(new Size(29, 17));
			((Control)cmdOneDay).set_TabIndex(106);
			((ButtonBase)cmdOneDay).set_UseVisualStyleBackColor(true);
			((Control)cmdOneDay).add_Click((EventHandler)cmdOneDay_Click);
			((Control)button1).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)button1).set_Location(new Point(132, 95));
			((Control)button1).set_Name("button1");
			((Control)button1).set_Size(new Size(29, 17));
			((Control)button1).set_TabIndex(107);
			((ButtonBase)button1).set_UseVisualStyleBackColor(true);
			((Control)button1).add_Click((EventHandler)button1_Click);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(18, 82));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(29, 13));
			((Control)label2).set_TabIndex(108);
			((Control)label2).set_Text("Year");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(129, 82));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(35, 13));
			((Control)label3).set_TabIndex(109);
			((Control)label3).set_Text("Today");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(95, 82));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(26, 13));
			((Control)label7).set_TabIndex(110);
			((Control)label7).set_Text("Day");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(52, 82));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(37, 13));
			((Control)label8).set_TabIndex(111);
			((Control)label8).set_Text("Month");
			((Control)groupBox3).set_Anchor((AnchorStyles)1);
			((Control)groupBox3).set_BackColor(Color.LightGoldenrodYellow);
			((Control)groupBox3).get_Controls().Add((Control)(object)button1);
			((Control)groupBox3).get_Controls().Add((Control)(object)cmdFullMonth);
			((Control)groupBox3).get_Controls().Add((Control)(object)cmdOneDay);
			((Control)groupBox3).get_Controls().Add((Control)(object)cmdFullYear);
			((Control)groupBox3).get_Controls().Add((Control)(object)label17);
			((Control)groupBox3).get_Controls().Add((Control)(object)label1);
			((Control)groupBox3).get_Controls().Add((Control)(object)label6);
			((Control)groupBox3).get_Controls().Add((Control)(object)label5);
			((Control)groupBox3).get_Controls().Add((Control)(object)label4);
			((Control)groupBox3).get_Controls().Add((Control)(object)label28);
			((Control)groupBox3).get_Controls().Add((Control)(object)cmbLunarDayEnd);
			((Control)groupBox3).get_Controls().Add((Control)(object)cmbLunarDayStart);
			((Control)groupBox3).get_Controls().Add((Control)(object)cmbLunarMonthEnd);
			((Control)groupBox3).get_Controls().Add((Control)(object)cmbLunarMonthStart);
			((Control)groupBox3).get_Controls().Add((Control)(object)updnLunarYearEnd);
			((Control)groupBox3).get_Controls().Add((Control)(object)updnLunarYearStart);
			((Control)groupBox3).get_Controls().Add((Control)(object)radioButton4);
			((Control)groupBox3).get_Controls().Add((Control)(object)radioButton3);
			((Control)groupBox3).get_Controls().Add((Control)(object)radioButton2);
			((Control)groupBox3).get_Controls().Add((Control)(object)radioButton1);
			((Control)groupBox3).get_Controls().Add((Control)(object)label8);
			((Control)groupBox3).get_Controls().Add((Control)(object)label7);
			((Control)groupBox3).get_Controls().Add((Control)(object)label3);
			((Control)groupBox3).get_Controls().Add((Control)(object)label2);
			((Control)groupBox3).set_Location(new Point(431, 27));
			((Control)groupBox3).set_Name("groupBox3");
			((Control)groupBox3).set_Size(new Size(230, 117));
			((Control)groupBox3).set_TabIndex(112);
			groupBox3.set_TabStop(false);
			((Control)groupBox3).set_Text("4.  Set UT dates");
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_BackColor(Color.Yellow);
			((Control)label17).set_Location(new Point(172, 12));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(55, 13));
			((Control)label17).set_TabIndex(116);
			((Control)label17).set_Text("Starting at");
			((Control)radioButton4).set_AutoSize(true);
			radioButton4.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)radioButton4).set_Location(new Point(169, 83));
			((Control)radioButton4).set_Name("radioButton4");
			((Control)radioButton4).set_Size(new Size(57, 17));
			((Control)radioButton4).set_TabIndex(115);
			((Control)radioButton4).set_Text("+12hrs");
			toolTip1.SetToolTip((Control)(object)radioButton4, "Europe, Africa, Middle East");
			((ButtonBase)radioButton4).set_UseVisualStyleBackColor(true);
			radioButton4.add_CheckedChanged((EventHandler)radioButton4_CheckedChanged);
			((Control)radioButton3).set_AutoSize(true);
			radioButton3.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)radioButton3).set_Location(new Point(169, 64));
			((Control)radioButton3).set_Name("radioButton3");
			((Control)radioButton3).set_Size(new Size(57, 17));
			((Control)radioButton3).set_TabIndex(114);
			((Control)radioButton3).set_Text("+ 6 hrs");
			toolTip1.SetToolTip((Control)(object)radioButton3, "Japan, western Australia, India, China");
			((ButtonBase)radioButton3).set_UseVisualStyleBackColor(true);
			radioButton3.add_CheckedChanged((EventHandler)radioButton3_CheckedChanged);
			((Control)radioButton2).set_AutoSize(true);
			radioButton2.set_CheckAlign(ContentAlignment.MiddleRight);
			radioButton2.set_Checked(true);
			((Control)radioButton2).set_Location(new Point(172, 45));
			((Control)radioButton2).set_Name("radioButton2");
			((Control)radioButton2).set_Size(new Size(54, 17));
			((Control)radioButton2).set_TabIndex(113);
			radioButton2.set_TabStop(true);
			((Control)radioButton2).set_Text("  0 hrs");
			toolTip1.SetToolTip((Control)(object)radioButton2, "Eastern Australia, New Zealand");
			((ButtonBase)radioButton2).set_UseVisualStyleBackColor(true);
			radioButton2.add_CheckedChanged((EventHandler)radioButton2_CheckedChanged);
			((Control)radioButton1).set_AutoSize(true);
			radioButton1.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)radioButton1).set_Location(new Point(172, 26));
			((Control)radioButton1).set_Name("radioButton1");
			((Control)radioButton1).set_Size(new Size(54, 17));
			((Control)radioButton1).set_TabIndex(112);
			((Control)radioButton1).set_Text("- 6 hrs");
			toolTip1.SetToolTip((Control)(object)radioButton1, "North and South America");
			((ButtonBase)radioButton1).set_UseVisualStyleBackColor(true);
			radioButton1.add_CheckedChanged((EventHandler)radioButton1_CheckedChanged);
			((Control)groupBox2).set_Anchor((AnchorStyles)1);
			((Control)groupBox2).set_BackColor(Color.Honeydew);
			((Control)groupBox2).get_Controls().Add((Control)(object)chkFilterOutput);
			((Control)groupBox2).get_Controls().Add((Control)(object)chkAbbrev);
			((Control)groupBox2).get_Controls().Add((Control)(object)cmdCompute);
			((Control)groupBox2).set_Location(new Point(664, 27));
			((Control)groupBox2).set_Name("groupBox2");
			((Control)groupBox2).set_Size(new Size(102, 117));
			((Control)groupBox2).set_TabIndex(113);
			groupBox2.set_TabStop(false);
			((Control)groupBox2).set_Text("5. Events for Site");
			((Control)chkFilterOutput).set_AutoSize(true);
			((Control)chkFilterOutput).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkFilterOutput).set_Location(new Point(14, 94));
			((Control)chkFilterOutput).set_Name("chkFilterOutput");
			((Control)chkFilterOutput).set_Size(new Size(77, 17));
			((Control)chkFilterOutput).set_TabIndex(57);
			((Control)chkFilterOutput).set_Text("Apply Filter");
			((ButtonBase)chkFilterOutput).set_UseVisualStyleBackColor(true);
			chkFilterOutput.add_CheckedChanged((EventHandler)chkFilterOutput_CheckedChanged);
			((Control)chkAbbrev).set_AutoSize(true);
			((Control)chkAbbrev).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkAbbrev).set_Location(new Point(14, 73));
			((Control)chkAbbrev).set_Name("chkAbbrev");
			((Control)chkAbbrev).set_Size(new Size(86, 17));
			((Control)chkAbbrev).set_TabIndex(56);
			((Control)chkAbbrev).set_Text("Short Output");
			((ButtonBase)chkAbbrev).set_TextAlign(ContentAlignment.MiddleRight);
			((ButtonBase)chkAbbrev).set_UseVisualStyleBackColor(true);
			chkAbbrev.add_CheckedChanged((EventHandler)chkAbbrev_CheckedChanged);
			((Control)grpAnyWhere).set_Anchor((AnchorStyles)1);
			((Control)grpAnyWhere).set_BackColor(Color.PaleTurquoise);
			((Control)grpAnyWhere).get_Controls().Add((Control)(object)cmdFormat);
			((Control)grpAnyWhere).get_Controls().Add((Control)(object)cmdComputeGrazes);
			((Control)grpAnyWhere).get_Controls().Add((Control)(object)cmdWorldMap);
			((Control)grpAnyWhere).get_Controls().Add((Control)(object)cmdMultiLocation);
			((Control)grpAnyWhere).set_Location(new Point(769, 27));
			((Control)grpAnyWhere).set_Name("grpAnyWhere");
			((Control)grpAnyWhere).set_Size(new Size(194, 78));
			((Control)grpAnyWhere).set_TabIndex(114);
			grpAnyWhere.set_TabStop(false);
			((Control)grpAnyWhere).set_Text("6.  Events anywhere");
			((ButtonBase)cmdFormat).set_FlatStyle((FlatStyle)3);
			((Control)cmdFormat).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdFormat).set_Location(new Point(67, 48));
			((Control)cmdFormat).set_Name("cmdFormat");
			((Control)cmdFormat).set_Size(new Size(60, 29));
			((Control)cmdFormat).set_TabIndex(62);
			((Control)cmdFormat).set_Text("Output\r\nFormat");
			((ButtonBase)cmdFormat).set_UseVisualStyleBackColor(true);
			((Control)cmdFormat).add_Click((EventHandler)cmdFormat_Click);
			((Control)grpSingleSite).set_Anchor((AnchorStyles)1);
			((Control)grpSingleSite).set_BackColor(Color.PeachPuff);
			((Control)grpSingleSite).get_Controls().Add((Control)(object)label14);
			((Control)grpSingleSite).get_Controls().Add((Control)(object)txtName);
			((Control)grpSingleSite).get_Controls().Add((Control)(object)label37);
			((Control)grpSingleSite).get_Controls().Add((Control)(object)updnAperture);
			((Control)grpSingleSite).get_Controls().Add((Control)(object)label11);
			((Control)grpSingleSite).get_Controls().Add((Control)(object)label12);
			((Control)grpSingleSite).get_Controls().Add((Control)(object)label13);
			((Control)grpSingleSite).get_Controls().Add((Control)(object)txtLatS);
			((Control)grpSingleSite).get_Controls().Add((Control)(object)txtLatM);
			((Control)grpSingleSite).get_Controls().Add((Control)(object)txtLatD);
			((Control)grpSingleSite).get_Controls().Add((Control)(object)txtLongS);
			((Control)grpSingleSite).get_Controls().Add((Control)(object)txtLongM);
			((Control)grpSingleSite).get_Controls().Add((Control)(object)txtLongD);
			((Control)grpSingleSite).get_Controls().Add((Control)(object)txtAltitude);
			((Control)grpSingleSite).get_Controls().Add((Control)(object)label24);
			((Control)grpSingleSite).get_Controls().Add((Control)(object)label25);
			((Control)grpSingleSite).get_Controls().Add((Control)(object)cmdUseSiteFiles);
			((Control)grpSingleSite).get_Controls().Add((Control)(object)label20);
			((Control)grpSingleSite).set_Location(new Point(27, 168));
			((Control)grpSingleSite).set_Name("grpSingleSite");
			((Control)grpSingleSite).set_Size(new Size(223, 106));
			((Control)grpSingleSite).set_TabIndex(115);
			grpSingleSite.set_TabStop(false);
			((Control)grpSingleSite).set_Text("1.   Specify a single site location");
			((Control)grpSingleSite).set_Visible(false);
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label14).set_Location(new Point(148, 11));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(46, 12));
			((Control)label14).set_TabIndex(85);
			((Control)label14).set_Text("Site name");
			((Control)txtName).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "LunarSingleSiteName", true, (DataSourceUpdateMode)1));
			((Control)txtName).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtName).set_Location(new Point(122, 23));
			((TextBoxBase)txtName).set_MaxLength(32);
			((Control)txtName).set_Name("txtName");
			((Control)txtName).set_Size(new Size(95, 20));
			((Control)txtName).set_TabIndex(84);
			((Control)txtName).set_Text(Settings.Default.LunarSingleSiteName);
			((Control)label37).set_AutoSize(true);
			((Control)label37).set_Location(new Point(114, 47));
			((Control)label37).set_Name("label37");
			((Control)label37).set_Size(new Size(52, 13));
			((Control)label37).set_TabIndex(82);
			((Control)label37).set_Text("Apert(cm)");
			((Control)updnAperture).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "LunarSingleSiteAperture", true, (DataSourceUpdateMode)1));
			updnAperture.set_Increment(new decimal(new int[4] { 5, 0, 0, 0 }));
			((Control)updnAperture).set_Location(new Point(166, 45));
			updnAperture.set_Maximum(new decimal(new int[4] { 500, 0, 0, 0 }));
			updnAperture.set_Minimum(new decimal(new int[4] { 5, 0, 0, 0 }));
			((Control)updnAperture).set_Name("updnAperture");
			((Control)updnAperture).set_Size(new Size(52, 20));
			((Control)updnAperture).set_TabIndex(83);
			((UpDownBase)updnAperture).set_TextAlign((HorizontalAlignment)2);
			updnAperture.set_Value(Settings.Default.LunarSingleSiteAperture);
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Location(new Point(4, 68));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(59, 13));
			((Control)label11).set_TabIndex(80);
			((Control)label11).set_Text("Altitude (m)");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Location(new Point(3, 47));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(22, 13));
			((Control)label12).set_TabIndex(76);
			((Control)label12).set_Text("Lat");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Location(new Point(1, 26));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(25, 13));
			((Control)label13).set_TabIndex(69);
			((Control)label13).set_Text("Lon");
			((Control)txtLatS).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "LunarSingleSiteLatSec", true, (DataSourceUpdateMode)1));
			((Control)txtLatS).set_Location(new Point(77, 44));
			((Control)txtLatS).set_Name("txtLatS");
			((Control)txtLatS).set_Size(new Size(31, 20));
			((Control)txtLatS).set_TabIndex(79);
			((Control)txtLatS).set_Text(Settings.Default.LunarSingleSiteLatSec);
			txtLatS.set_TextAlign((HorizontalAlignment)2);
			((Control)txtLatM).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "LunarSingleSiteLatMin", true, (DataSourceUpdateMode)1));
			((Control)txtLatM).set_Location(new Point(56, 44));
			((Control)txtLatM).set_Name("txtLatM");
			((Control)txtLatM).set_Size(new Size(20, 20));
			((Control)txtLatM).set_TabIndex(78);
			((Control)txtLatM).set_Text(Settings.Default.LunarSingleSiteLatMin);
			txtLatM.set_TextAlign((HorizontalAlignment)1);
			((Control)txtLatD).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "LunarSingleSiteLatDeg", true, (DataSourceUpdateMode)1));
			((Control)txtLatD).set_Location(new Point(27, 44));
			((Control)txtLatD).set_Name("txtLatD");
			((Control)txtLatD).set_Size(new Size(28, 20));
			((Control)txtLatD).set_TabIndex(77);
			((Control)txtLatD).set_Text(Settings.Default.LunarSingleSiteLatDeg);
			txtLatD.set_TextAlign((HorizontalAlignment)1);
			((Control)txtLongS).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "LunarSingleSiteLongSec", true, (DataSourceUpdateMode)1));
			((Control)txtLongS).set_Location(new Point(77, 23));
			((Control)txtLongS).set_Name("txtLongS");
			((Control)txtLongS).set_Size(new Size(31, 20));
			((Control)txtLongS).set_TabIndex(74);
			((Control)txtLongS).set_Text(Settings.Default.LunarSingleSiteLongSec);
			txtLongS.set_TextAlign((HorizontalAlignment)2);
			((Control)txtLongM).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "LunarSingleSiteLongMin", true, (DataSourceUpdateMode)1));
			((Control)txtLongM).set_Location(new Point(56, 23));
			((Control)txtLongM).set_Name("txtLongM");
			((Control)txtLongM).set_Size(new Size(20, 20));
			((Control)txtLongM).set_TabIndex(72);
			((Control)txtLongM).set_Text(Settings.Default.LunarSingleSiteLongMin);
			txtLongM.set_TextAlign((HorizontalAlignment)1);
			((Control)txtLongD).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "LunarSingleSiteLongDeg", true, (DataSourceUpdateMode)1));
			((Control)txtLongD).set_Location(new Point(27, 23));
			((Control)txtLongD).set_Name("txtLongD");
			((Control)txtLongD).set_Size(new Size(28, 20));
			((Control)txtLongD).set_TabIndex(70);
			((Control)txtLongD).set_Text(Settings.Default.LunarSingleSiteLongDeg);
			txtLongD.set_TextAlign((HorizontalAlignment)1);
			((Control)txtAltitude).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "LunarSingleSiteAlt", true, (DataSourceUpdateMode)1));
			((Control)txtAltitude).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAltitude).set_Location(new Point(71, 65));
			((Control)txtAltitude).set_Name("txtAltitude");
			((Control)txtAltitude).set_Size(new Size(38, 20));
			((Control)txtAltitude).set_TabIndex(81);
			((Control)txtAltitude).set_Text(Settings.Default.LunarSingleSiteAlt);
			txtAltitude.set_TextAlign((HorizontalAlignment)2);
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Location(new Point(64, 15));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(9, 13));
			((Control)label24).set_TabIndex(73);
			((Control)label24).set_Text("'");
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Location(new Point(90, 15));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(12, 13));
			((Control)label25).set_TabIndex(75);
			((Control)label25).set_Text("\"");
			((Control)cmdUseSiteFiles).set_BackColor(Color.PowderBlue);
			((ButtonBase)cmdUseSiteFiles).get_FlatAppearance().set_BorderSize(2);
			((Control)cmdUseSiteFiles).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdUseSiteFiles).set_Location(new Point(147, 80));
			((Control)cmdUseSiteFiles).set_Name("cmdUseSiteFiles");
			((Control)cmdUseSiteFiles).set_Size(new Size(69, 21));
			((Control)cmdUseSiteFiles).set_TabIndex(67);
			((Control)cmdUseSiteFiles).set_Text("Use Site files");
			((ButtonBase)cmdUseSiteFiles).set_TextAlign(ContentAlignment.TopCenter);
			((ButtonBase)cmdUseSiteFiles).set_UseVisualStyleBackColor(false);
			((Control)cmdUseSiteFiles).add_Click((EventHandler)cmdUseSiteFiles_Click);
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Location(new Point(43, 11));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(13, 13));
			((Control)label20).set_TabIndex(71);
			((Control)label20).set_Text("o");
			((Control)pnlMultiSiteOutput).set_BackColor(Color.LightCyan);
			pnlMultiSiteOutput.set_BorderStyle((BorderStyle)2);
			((Control)pnlMultiSiteOutput).get_Controls().Add((Control)(object)cmdMultiOK);
			((Control)pnlMultiSiteOutput).get_Controls().Add((Control)(object)label10);
			((Control)pnlMultiSiteOutput).get_Controls().Add((Control)(object)label9);
			((Control)pnlMultiSiteOutput).get_Controls().Add((Control)(object)chkShort);
			((Control)pnlMultiSiteOutput).get_Controls().Add((Control)(object)chkLong);
			((Control)pnlMultiSiteOutput).set_Location(new Point(702, 154));
			((Control)pnlMultiSiteOutput).set_Name("pnlMultiSiteOutput");
			((Control)pnlMultiSiteOutput).set_Size(new Size(127, 115));
			((Control)pnlMultiSiteOutput).set_TabIndex(116);
			((Control)pnlMultiSiteOutput).set_Visible(false);
			((Control)cmdMultiOK).set_BackColor(Color.Honeydew);
			((Control)cmdMultiOK).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdMultiOK).set_Location(new Point(34, 85));
			((Control)cmdMultiOK).set_Name("cmdMultiOK");
			((Control)cmdMultiOK).set_Size(new Size(54, 22));
			((Control)cmdMultiOK).set_TabIndex(6);
			((Control)cmdMultiOK).set_Text("Close");
			((ButtonBase)cmdMultiOK).set_UseVisualStyleBackColor(false);
			((Control)cmdMultiOK).add_Click((EventHandler)cmdMultiOK_Click);
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label10).set_Location(new Point(1, 2));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(120, 13));
			((Control)label10).set_TabIndex(5);
			((Control)label10).set_Text("Multisite predictions");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(17, 20));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(88, 13));
			((Control)label9).set_TabIndex(2);
			((Control)label9).set_Text("Set output format");
			chkShort.set_AutoCheck(false);
			((Control)chkShort).set_AutoSize(true);
			chkShort.set_Checked(Settings.Default.LunarMultiShort);
			((Control)chkShort).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "LunarMultiShort", true, (DataSourceUpdateMode)1));
			((Control)chkShort).set_Location(new Point(7, 62));
			((Control)chkShort).set_Name("chkShort");
			((Control)chkShort).set_Size(new Size(108, 17));
			((Control)chkShort).set_TabIndex(4);
			((Control)chkShort).set_Text("Short output lines");
			((ButtonBase)chkShort).set_UseVisualStyleBackColor(true);
			((Control)chkShort).add_Click((EventHandler)chkShort_Click);
			chkLong.set_AutoCheck(false);
			((Control)chkLong).set_AutoSize(true);
			chkLong.set_Checked(Settings.Default.LunarMultiLong);
			chkLong.set_CheckState((CheckState)1);
			((Control)chkLong).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "LunarMultiLong", true, (DataSourceUpdateMode)1));
			((Control)chkLong).set_Location(new Point(7, 41));
			((Control)chkLong).set_Name("chkLong");
			((Control)chkLong).set_Size(new Size(107, 17));
			((Control)chkLong).set_TabIndex(3);
			((Control)chkLong).set_Text("Long output lines");
			((ButtonBase)chkLong).set_UseVisualStyleBackColor(true);
			((Control)chkLong).add_Click((EventHandler)chkLong_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(986, 540));
			((Control)this).get_Controls().Add((Control)(object)pnlMultiSiteOutput);
			((Control)this).get_Controls().Add((Control)(object)grpObjects);
			((Control)this).get_Controls().Add((Control)(object)grpSingleSite);
			((Control)this).get_Controls().Add((Control)(object)grpAnyWhere);
			((Control)this).get_Controls().Add((Control)(object)groupBox3);
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_Controls().Add((Control)(object)pBar2);
			((Control)this).get_Controls().Add((Control)(object)grpXZ80);
			((Control)this).get_Controls().Add((Control)(object)pBar);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)lstPrediction);
			((Control)this).get_Controls().Add((Control)(object)LabelRightClick);
			((Control)this).get_Controls().Add((Control)(object)lblCurrentDate);
			((Control)this).get_Controls().Add((Control)(object)groupBox2);
			((Control)this).get_Controls().Add((Control)(object)labelIntegration);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationLunarPredictions", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationLunarPredictions);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MinimumSize(new Size(550, 300));
			((Control)this).set_Name("LunarOccultationPrediction");
			((Control)this).set_Text("Lunar occultation predictions");
			((Form)this).add_FormClosing(new FormClosingEventHandler(LunarOccultationPrediction_FormClosing));
			((Form)this).add_Load((EventHandler)LunarOccultationPrediction_Load);
			((Control)this).add_Resize((EventHandler)LunarOccultationPrediction_Resize);
			((Control)contextMenuStrip1).ResumeLayout(false);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((ISupportInitialize)updnLunarYearEnd).EndInit();
			((ISupportInitialize)updnLunarYearStart).EndInit();
			((Control)grpXZ80).ResumeLayout(false);
			((Control)grpXZ80).PerformLayout();
			((Control)grpObjects).ResumeLayout(false);
			((Control)grpObjects).PerformLayout();
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((Control)panel1).ResumeLayout(false);
			((Control)groupBox3).ResumeLayout(false);
			((Control)groupBox3).PerformLayout();
			((Control)groupBox2).ResumeLayout(false);
			((Control)groupBox2).PerformLayout();
			((Control)grpAnyWhere).ResumeLayout(false);
			((Control)grpSingleSite).ResumeLayout(false);
			((Control)grpSingleSite).PerformLayout();
			((ISupportInitialize)updnAperture).EndInit();
			((Control)pnlMultiSiteOutput).ResumeLayout(false);
			((Control)pnlMultiSiteOutput).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
