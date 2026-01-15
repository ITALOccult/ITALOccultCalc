using System;
using System.ComponentModel;
using System.Drawing;
using System.Globalization;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class SatellitePlanetEclipsesTransits : Form
	{
		private readonly string AppPath;

		private const double Radian = 180.0 / Math.PI;

		private static double[] NextDate = new double[10];

		private static double[] EventTimes = new double[24];

		private static int[] EventTypes = new int[24];

		private static double[] BufferTimes = new double[46];

		private static int[] BufferTypes = new int[46];

		private double JDstart;

		private double CurrentDay;

		private static string[] MonthArray;

		private static readonly string[] RingLabel = new string[6] { "", ".C", ".B", ".B", ".A", ".A" };

		private static readonly string[] EventDescriptor = new string[8] { "Oc.D", "Oc.R", "Ec.D", "Ec.R", "Tr.I", "Tr.E", "Sh.I", "Sh.E" };

		private static int Planet = 0;

		private static int EventCount;

		private static int TopOfBuffer = -1;

		private static int CurrentMonth;

		private static int Column1Count;

		private static int Column2Count;

		private static int Column3Count;

		private static bool CancelFlag = false;

		private bool ChangingDate;

		private IContainer components;

		private GroupBox groupBox3;

		private CheckBox chkSiteLimited;

		private Label label3;

		private Label label4;

		private NumericUpDown updnLongitude;

		private NumericUpDown updnLatitude;

		private ProgressBar pbar;

		private Panel panel3;

		private Label label2;

		private Label label1;

		private NumericUpDown updnEndMonth;

		private NumericUpDown updnEndYear;

		private NumericUpDown updnStartMonth;

		private NumericUpDown updnStartYear;

		private RadioButton optSaturn;

		private RadioButton optJupiter;

		private RadioButton optUranus;

		private Button cmdCompute;

		private Button cmdCancel;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withPredictionToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ListBox lstEvents;

		private RadioButton optNeptune;

		private GroupBox groupBox1;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		public SatellitePlanetEclipsesTransits()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
			updnStartYear.set_Value((decimal)DateTime.Now.Year);
			updnStartMonth.set_Value((decimal)DateTime.Now.Month);
			updnEndYear.set_Value((decimal)DateTime.Now.Year);
			updnEndMonth.set_Value((decimal)DateTime.Now.Month);
			if (!decimal.TryParse(Settings.Default.Site_Longitude_dd_d, out var result))
			{
				result = default(decimal);
			}
			updnLongitude.set_Value(result);
			if (!decimal.TryParse(Settings.Default.Site_Latitude_dd_d, out result))
			{
				result = default(decimal);
			}
			updnLatitude.set_Value(result);
		}

		private void SatellitePlanetEclipsesTransits_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : v." + Utilities.OccultVersion_Short);
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
		}

		private void cmdCompute_Click(object sender, EventArgs e)
		{
			Compute();
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			CancelFlag = true;
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void Compute()
		{
			//IL_0085: Unknown result type (might be due to invalid IL or missing references)
			JDstart = Utilities.JD_from_Date((int)updnStartYear.get_Value(), (int)updnStartMonth.get_Value(), 1.0);
			double num = Utilities.JD_from_Date((int)updnEndYear.get_Value(), (int)updnEndMonth.get_Value() + 1, 1.0);
			if (num - JDstart < 0.0)
			{
				MessageBox.Show("The end date is earlier than the start date", "Invalid date range", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return;
			}
			double siteLongitude = (double)updnLongitude.get_Value() / (180.0 / Math.PI);
			double siteLatitude = (double)updnLatitude.get_Value() / (180.0 / Math.PI);
			int ArrayNum = 0;
			((Control)cmdCompute).set_Visible(false);
			((Control)pbar).set_Visible(true);
			pbar.set_Value(0);
			pbar.set_Maximum((int)(num - JDstart + 4.0));
			lstEvents.get_Items().Clear();
			if (optJupiter.get_Checked())
			{
				Planet = 5;
			}
			else if (optSaturn.get_Checked())
			{
				Planet = 6;
			}
			else if (optUranus.get_Checked())
			{
				Planet = 7;
			}
			else if (optNeptune.get_Checked())
			{
				Planet = 8;
			}
			Initialise(JDstart);
			InitialiseMonthArray(JDstart);
			TopOfBuffer = -1;
			double next;
			do
			{
				next = GetNext(out var Moon, out var eclipse, out ArrayNum);
				pbar.set_Value((int)(next - JDstart));
				int num2 = Moon;
				if (Planet == 6)
				{
					num2 = Moon + 2;
					if (num2 == 7)
					{
						num2++;
					}
				}
				Satellites.SatellitePlanetEclipsesTransits(next, Planet, num2, eclipse, chkSiteLimited.get_Checked(), siteLongitude, siteLatitude, ref EventTimes, ref EventTypes, out EventCount);
				for (int i = 0; i < 24; i++)
				{
					if (EventTypes[i] > 0 && ((EventTimes[i] > JDstart) & (EventTimes[i] < num)))
					{
						TopOfBuffer++;
						BufferTimes[TopOfBuffer] = EventTimes[i];
						BufferTypes[TopOfBuffer] = EventTypes[i];
					}
				}
				if (TopOfBuffer > 1 && ((BufferTimes[TopOfBuffer] - BufferTimes[0] > 2.0) | (TopOfBuffer > 20)))
				{
					MakeOutput(EmptyTheBuffer: false);
				}
				Application.DoEvents();
				if (CancelFlag)
				{
					break;
				}
				NextDate[ArrayNum] = (EventTimes[1] + EventTimes[2]) / 2.0 + Math.PI * 2.0 / Satellites.MeanDailyMotion;
			}
			while (next <= num);
			MakeOutput(EmptyTheBuffer: true);
			MonthArrayToOutput();
			CancelFlag = false;
			((Control)pbar).set_Visible(false);
			((Control)cmdCompute).set_Visible(true);
		}

		private void Initialise(double JDinit)
		{
			double Sepn = 0.0;
			double PA = 0.0;
			for (int i = 1; i <= 5; i++)
			{
				NextDate[2 * (i - 1)] = (NextDate[2 * (i - 1) + 1] = 0.0);
			}
			for (int j = 1; j <= 5 && !(Planet == 5 && j == 5); j++)
			{
				int num = j;
				if (Planet == 6)
				{
					num = j + 2;
					if (num == 7)
					{
						num++;
					}
				}
				if (!(Planet == 8 && j == 2))
				{
					Satellites.SatelliteCoordinates(JDinit - 1.0, Planet, num, ViewedFromSun: false, ref Sepn, ref PA);
					double num2 = Satellites.U - Satellites.UPlanet;
					double meanDailyMotion = Satellites.MeanDailyMotion;
					while (num2 > Math.PI)
					{
						num2 -= Math.PI * 2.0;
					}
					for (; num2 < -Math.PI; num2 += Math.PI * 2.0)
					{
					}
					double num3 = (0.0 - num2) / meanDailyMotion;
					if (num3 < 0.0)
					{
						num3 += Math.PI * 2.0 / meanDailyMotion;
					}
					NextDate[2 * (j - 1)] = JDinit - 1.0 + num3;
					for (num2 -= Math.PI; num2 > Math.PI; num2 -= Math.PI * 2.0)
					{
					}
					for (; num2 < -Math.PI; num2 += Math.PI * 2.0)
					{
					}
					num3 = (0.0 - num2) / meanDailyMotion;
					if (num3 < 0.0)
					{
						num3 += Math.PI * 2.0 / meanDailyMotion;
					}
					NextDate[2 * (j - 1) + 1] = JDinit - 1.0 + num3;
					continue;
				}
				break;
			}
		}

		private double GetNext(out int Moon, out bool eclipse, out int ArrayNum)
		{
			double num = 6000000.0;
			eclipse = true;
			ArrayNum = 0;
			for (int i = 0; i <= 9; i++)
			{
				if ((NextDate[i] < num) & (NextDate[i] > 0.0))
				{
					num = NextDate[i];
					ArrayNum = i;
				}
			}
			Moon = ArrayNum / 2 + 1;
			eclipse = ArrayNum % 2 == 1;
			return num;
		}

		private void MakeOutput(bool EmptyTheBuffer)
		{
			int num = 0;
			int Year = 0;
			int Month = 0;
			double day = 0.0;
			if (TopOfBuffer < 0)
			{
				return;
			}
			int topOfBuffer = TopOfBuffer;
			int num2 = topOfBuffer / 2;
			do
			{
				int num3 = topOfBuffer - num2;
				int num4;
				do
				{
					num4 = 0;
					for (int i = 0; i <= num3; i++)
					{
						if (BufferTimes[i] > BufferTimes[i + num2])
						{
							Utilities.Swap(ref BufferTimes, i, num2);
							Utilities.Swap(ref BufferTypes, i, num2);
							num4 = i;
						}
					}
					num3 = num4 - num2;
				}
				while (num4 > 0);
				num2 /= 2;
			}
			while (num2 > 0);
			double num5 = Math.Floor(BufferTimes[TopOfBuffer] - 0.5) - 0.5;
			num = 0;
			for (int j = 0; j <= TopOfBuffer; j++)
			{
				if (BufferTimes[j] < num5 || EmptyTheBuffer)
				{
					double num6 = Math.Floor(BufferTimes[j] - 0.5) + 0.5;
					double degree = 24.0 * (BufferTimes[j] - num6);
					int num7 = BufferTypes[j] / 100;
					int num8 = BufferTypes[j] % 100 / 10;
					int num9 = BufferTypes[j] % 10;
					Utilities.Date_from_JD(num6, out Year, out Month, out day);
					string text = ((day != CurrentDay) ? Convert.ToString(day).PadLeft(3) : "   ");
					CurrentDay = day;
					if (Month != CurrentMonth)
					{
						MonthArrayToOutput();
						InitialiseMonthArray(num6);
					}
					string value = text + " " + Utilities.DEGtoDMS(degree, 2, 1, MinutesOnly: true) + "  " + num8 + "." + EventDescriptor[num9] + RingLabel[num7];
					if (day < 11.0)
					{
						MonthArray[Column1Count] = MonthArray[Column1Count].Remove(0, 19).Insert(0, value);
						Column1Count++;
					}
					else if (day < 21.0)
					{
						MonthArray[Column2Count] = MonthArray[Column2Count].Remove(23, 19).Insert(23, value);
						Column2Count++;
					}
					else
					{
						MonthArray[Column3Count] = MonthArray[Column3Count].Remove(46, 19).Insert(46, value);
						Column3Count++;
					}
					num++;
				}
			}
			for (int k = num; k <= TopOfBuffer; k++)
			{
				BufferTimes[k - num] = BufferTimes[k];
				BufferTypes[k - num] = BufferTypes[k];
			}
			TopOfBuffer -= num;
		}

		private void InitialiseMonthArray(double JD)
		{
			int Year = 0;
			int Month = 0;
			double day = 0.0;
			MonthArray = new string[151];
			for (int i = 0; i < 151; i++)
			{
				MonthArray[i] = "".PadRight(70);
			}
			Utilities.Date_from_JD(JD, out Year, out Month, out day);
			string text = "Satellites of " + Utilities.Planets[Planet] + Year.ToString(" ####");
			int length = text.Length;
			MonthArray[0] = MonthArray[0].Remove(21, length).Insert(21, text);
			text = CultureInfo.CurrentCulture.DateTimeFormat.GetMonthName(Month);
			length = text.Length;
			MonthArray[1] = MonthArray[1].Remove(30, length).Insert(30, text);
			CurrentMonth = Month;
			Column1Count = (Column2Count = (Column3Count = 3));
		}

		private void MonthArrayToOutput()
		{
			int num = Column1Count;
			if (Column2Count > num)
			{
				num = Column2Count;
			}
			if (Column3Count > num)
			{
				num = Column3Count;
			}
			if (num >= 2)
			{
				num += 2;
				for (int i = 0; i <= num; i++)
				{
					lstEvents.get_Items().Add((object)MonthArray[i]);
				}
			}
		}

		private string CollectEvents()
		{
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstEvents.get_Items().get_Count();
			for (int i = 0; i < count; i++)
			{
				stringBuilder.AppendLine(lstEvents.get_Items().get_Item(i).ToString());
			}
			stringBuilder.AppendLine("");
			return stringBuilder.ToString();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents());
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(CollectEvents());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_EphemerisData = Output.SaveAppendPredictionText(CollectEvents(), "Satellite events of " + Utilities.Planets[Planet] + " " + updnStartYear.get_Value(), Settings.Default.Save_EphemerisData);
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintPreviewText(CollectEvents());
		}

		private void SatellitePlanetEclipsesTransits_Resize(object sender, EventArgs e)
		{
			((Control)this).set_Width(634);
			if (((Control)this).get_Height() < 250)
			{
				((Control)this).set_Height(250);
			}
			((Control)lstEvents).set_Height(((Control)this).get_Height() - 170);
		}

		private void updnStartMonth_ValueChanged(object sender, EventArgs e)
		{
			if (ChangingDate)
			{
				return;
			}
			ChangingDate = true;
			if (updnStartMonth.get_Value() == 13m)
			{
				NumericUpDown obj = updnStartYear;
				obj.set_Value(obj.get_Value() + 1m);
				updnStartMonth.set_Value(1m);
				if (updnStartYear.get_Value() > updnEndYear.get_Value())
				{
					updnEndYear.set_Value(updnStartYear.get_Value());
					updnEndMonth.set_Value(updnStartMonth.get_Value());
				}
			}
			else if (updnStartMonth.get_Value() == 0m)
			{
				NumericUpDown obj2 = updnStartYear;
				obj2.set_Value(obj2.get_Value() - 1m);
				updnStartMonth.set_Value(12m);
			}
			if (updnEndMonth.get_Value() < updnStartMonth.get_Value())
			{
				if (updnStartYear.get_Value() > updnEndYear.get_Value())
				{
					updnEndYear.set_Value(updnStartYear.get_Value());
				}
				if (updnEndYear.get_Value() == updnStartYear.get_Value())
				{
					updnEndMonth.set_Value(updnStartMonth.get_Value());
				}
			}
			ChangingDate = false;
		}

		private void updnEndMonth_ValueChanged(object sender, EventArgs e)
		{
			if (ChangingDate)
			{
				return;
			}
			ChangingDate = true;
			if (updnEndMonth.get_Value() == 13m)
			{
				NumericUpDown obj = updnEndYear;
				obj.set_Value(obj.get_Value() + 1m);
				updnEndMonth.set_Value(1m);
			}
			if (updnEndMonth.get_Value() == 0m)
			{
				NumericUpDown obj2 = updnEndYear;
				obj2.set_Value(obj2.get_Value() - 1m);
				updnEndMonth.set_Value(12m);
				if (updnStartYear.get_Value() > updnEndYear.get_Value())
				{
					updnStartYear.set_Value(updnEndYear.get_Value());
					updnStartMonth.set_Value(updnEndMonth.get_Value());
				}
			}
			if (updnEndMonth.get_Value() < updnStartMonth.get_Value())
			{
				if (updnStartYear.get_Value() > updnEndYear.get_Value())
				{
					updnStartYear.set_Value(updnEndYear.get_Value());
				}
				if (updnEndYear.get_Value() == updnStartYear.get_Value())
				{
					updnStartMonth.set_Value(updnEndMonth.get_Value());
				}
			}
			ChangingDate = false;
		}

		private void updnStartYear_ValueChanged(object sender, EventArgs e)
		{
			if (!ChangingDate)
			{
				ChangingDate = true;
				if (updnStartYear.get_Value() > updnEndYear.get_Value())
				{
					updnEndYear.set_Value(updnStartYear.get_Value());
					updnEndMonth.set_Value(updnStartMonth.get_Value());
				}
				ChangingDate = false;
			}
		}

		private void updnEndYear_ValueChanged(object sender, EventArgs e)
		{
			if (!ChangingDate)
			{
				ChangingDate = true;
				if (updnStartYear.get_Value() > updnEndYear.get_Value())
				{
					updnStartYear.set_Value(updnEndYear.get_Value());
					updnStartMonth.set_Value(updnStartMonth.get_Value());
				}
				ChangingDate = false;
			}
		}

		private void updnStartYear_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnStartYear).Select(0, 10);
		}

		private void updnStartMonth_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnStartMonth).Select(0, 10);
		}

		private void updnEndYear_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnEndYear).Select(0, 10);
		}

		private void updnEndMonth_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnEndMonth).Select(0, 10);
		}

		private void updnLongitude_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnLongitude).Select(0, 10);
		}

		private void updnLatitude_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnLatitude).Select(0, 10);
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"transits by satellites");
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
			//IL_02df: Unknown result type (might be due to invalid IL or missing references)
			//IL_02e9: Expected O, but got Unknown
			//IL_132b: Unknown result type (might be due to invalid IL or missing references)
			//IL_1335: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(SatellitePlanetEclipsesTransits));
			groupBox3 = new GroupBox();
			chkSiteLimited = new CheckBox();
			label3 = new Label();
			label4 = new Label();
			updnLongitude = new NumericUpDown();
			updnLatitude = new NumericUpDown();
			pbar = new ProgressBar();
			panel3 = new Panel();
			label2 = new Label();
			label1 = new Label();
			updnEndMonth = new NumericUpDown();
			updnEndYear = new NumericUpDown();
			updnStartMonth = new NumericUpDown();
			updnStartYear = new NumericUpDown();
			optSaturn = new RadioButton();
			optJupiter = new RadioButton();
			optUranus = new RadioButton();
			cmdCompute = new Button();
			cmdCancel = new Button();
			menuStrip1 = new MenuStrip();
			withPredictionToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			lstEvents = new ListBox();
			optNeptune = new RadioButton();
			groupBox1 = new GroupBox();
			((Control)groupBox3).SuspendLayout();
			((ISupportInitialize)updnLongitude).BeginInit();
			((ISupportInitialize)updnLatitude).BeginInit();
			((Control)panel3).SuspendLayout();
			((ISupportInitialize)updnEndMonth).BeginInit();
			((ISupportInitialize)updnEndYear).BeginInit();
			((ISupportInitialize)updnStartMonth).BeginInit();
			((ISupportInitialize)updnStartYear).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((Control)groupBox1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)groupBox3).get_Controls().Add((Control)(object)chkSiteLimited);
			((Control)groupBox3).get_Controls().Add((Control)(object)label3);
			((Control)groupBox3).get_Controls().Add((Control)(object)label4);
			((Control)groupBox3).get_Controls().Add((Control)(object)updnLongitude);
			((Control)groupBox3).get_Controls().Add((Control)(object)updnLatitude);
			((Control)groupBox3).set_Location(new Point(316, 39));
			((Control)groupBox3).set_Name("groupBox3");
			((Control)groupBox3).set_Size(new Size(211, 70));
			((Control)groupBox3).set_TabIndex(2);
			groupBox3.set_TabStop(false);
			((Control)groupBox3).set_Text("Limit to site location");
			((Control)chkSiteLimited).set_AutoSize(true);
			chkSiteLimited.set_Checked(Settings.Default.SatelliteEclipsesTransits_LimitToLocal);
			((Control)chkSiteLimited).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "SatelliteEclipsesTransits_LimitToLocal", true, (DataSourceUpdateMode)1));
			((Control)chkSiteLimited).set_Location(new Point(6, 30));
			((Control)chkSiteLimited).set_Name("chkSiteLimited");
			((Control)chkSiteLimited).set_Size(new Size(94, 30));
			((Control)chkSiteLimited).set_TabIndex(0);
			((Control)chkSiteLimited).set_Text("Limit to events\r\nvisible at:");
			((ButtonBase)chkSiteLimited).set_TextAlign(ContentAlignment.MiddleRight);
			((ButtonBase)chkSiteLimited).set_UseVisualStyleBackColor(true);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(97, 18));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(54, 13));
			((Control)label3).set_TabIndex(1);
			((Control)label3).set_Text("Longitude");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(155, 18));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(45, 13));
			((Control)label4).set_TabIndex(3);
			((Control)label4).set_Text("Latitude");
			((Control)updnLongitude).set_Location(new Point(100, 37));
			updnLongitude.set_Maximum(new decimal(new int[4] { 360, 0, 0, 0 }));
			updnLongitude.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)updnLongitude).set_Name("updnLongitude");
			((Control)updnLongitude).set_Size(new Size(52, 20));
			((Control)updnLongitude).set_TabIndex(2);
			((Control)updnLongitude).add_Enter((EventHandler)updnLongitude_Enter);
			((Control)updnLatitude).set_Location(new Point(159, 37));
			updnLatitude.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			updnLatitude.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)updnLatitude).set_Name("updnLatitude");
			((Control)updnLatitude).set_Size(new Size(43, 20));
			((Control)updnLatitude).set_TabIndex(4);
			((Control)updnLatitude).add_Enter((EventHandler)updnLatitude_Enter);
			((Control)pbar).set_Location(new Point(209, 111));
			((Control)pbar).set_Name("pbar");
			((Control)pbar).set_Size(new Size(200, 10));
			pbar.set_Step(1);
			((Control)pbar).set_TabIndex(7);
			((Control)pbar).set_Visible(false);
			((Control)panel3).get_Controls().Add((Control)(object)label2);
			((Control)panel3).get_Controls().Add((Control)(object)label1);
			((Control)panel3).get_Controls().Add((Control)(object)updnEndMonth);
			((Control)panel3).get_Controls().Add((Control)(object)updnEndYear);
			((Control)panel3).get_Controls().Add((Control)(object)updnStartMonth);
			((Control)panel3).get_Controls().Add((Control)(object)updnStartYear);
			((Control)panel3).set_Location(new Point(106, 46));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(203, 57));
			((Control)panel3).set_TabIndex(1);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(5, 33));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(92, 13));
			((Control)label2).set_TabIndex(3);
			((Control)label2).set_Text("End Year && month");
			label2.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(2, 6));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(95, 13));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text("Start Year && month");
			label1.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)updnEndMonth).set_Location(new Point(158, 31));
			updnEndMonth.set_Maximum(new decimal(new int[4] { 13, 0, 0, 0 }));
			((Control)updnEndMonth).set_Name("updnEndMonth");
			((Control)updnEndMonth).set_Size(new Size(39, 20));
			((Control)updnEndMonth).set_TabIndex(5);
			updnEndMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnEndMonth.add_ValueChanged((EventHandler)updnEndMonth_ValueChanged);
			((Control)updnEndMonth).add_Enter((EventHandler)updnEndMonth_Enter);
			((Control)updnEndYear).set_Location(new Point(99, 31));
			updnEndYear.set_Maximum(new decimal(new int[4] { 3000, 0, 0, 0 }));
			updnEndYear.set_Minimum(new decimal(new int[4] { 1000, 0, 0, 0 }));
			((Control)updnEndYear).set_Name("updnEndYear");
			((Control)updnEndYear).set_Size(new Size(51, 20));
			((Control)updnEndYear).set_TabIndex(4);
			updnEndYear.set_Value(new decimal(new int[4] { 2007, 0, 0, 0 }));
			updnEndYear.add_ValueChanged((EventHandler)updnEndYear_ValueChanged);
			((Control)updnEndYear).add_Enter((EventHandler)updnEndYear_Enter);
			((Control)updnStartMonth).set_Location(new Point(157, 4));
			updnStartMonth.set_Maximum(new decimal(new int[4] { 13, 0, 0, 0 }));
			((Control)updnStartMonth).set_Name("updnStartMonth");
			((Control)updnStartMonth).set_Size(new Size(40, 20));
			((Control)updnStartMonth).set_TabIndex(2);
			updnStartMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnStartMonth.add_ValueChanged((EventHandler)updnStartMonth_ValueChanged);
			((Control)updnStartMonth).add_Enter((EventHandler)updnStartMonth_Enter);
			((Control)updnStartYear).set_Location(new Point(99, 4));
			updnStartYear.set_Maximum(new decimal(new int[4] { 3000, 0, 0, 0 }));
			updnStartYear.set_Minimum(new decimal(new int[4] { 1000, 0, 0, 0 }));
			((Control)updnStartYear).set_Name("updnStartYear");
			((Control)updnStartYear).set_Size(new Size(51, 20));
			((Control)updnStartYear).set_TabIndex(1);
			updnStartYear.set_Value(new decimal(new int[4] { 2007, 0, 0, 0 }));
			updnStartYear.add_ValueChanged((EventHandler)updnStartYear_ValueChanged);
			((Control)updnStartYear).add_Enter((EventHandler)updnStartYear_Enter);
			((Control)optSaturn).set_AutoSize(true);
			((Control)optSaturn).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optSaturn).set_Location(new Point(6, 31));
			((Control)optSaturn).set_Name("optSaturn");
			((Control)optSaturn).set_Size(new Size(62, 17));
			((Control)optSaturn).set_TabIndex(1);
			optSaturn.set_TabStop(true);
			((Control)optSaturn).set_Text("Saturn");
			((ButtonBase)optSaturn).set_UseVisualStyleBackColor(true);
			((Control)optJupiter).set_AutoSize(true);
			optJupiter.set_Checked(true);
			((Control)optJupiter).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optJupiter).set_Location(new Point(6, 14));
			((Control)optJupiter).set_Name("optJupiter");
			((Control)optJupiter).set_Size(new Size(63, 17));
			((Control)optJupiter).set_TabIndex(0);
			optJupiter.set_TabStop(true);
			((Control)optJupiter).set_Text("Jupiter");
			((ButtonBase)optJupiter).set_UseVisualStyleBackColor(true);
			((Control)optUranus).set_AutoSize(true);
			((Control)optUranus).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optUranus).set_Location(new Point(6, 48));
			((Control)optUranus).set_Name("optUranus");
			((Control)optUranus).set_Size(new Size(65, 17));
			((Control)optUranus).set_TabIndex(2);
			optUranus.set_TabStop(true);
			((Control)optUranus).set_Text("Uranus");
			((ButtonBase)optUranus).set_UseVisualStyleBackColor(true);
			((Control)cmdCompute).set_Location(new Point(535, 61));
			((Control)cmdCompute).set_Name("cmdCompute");
			((Control)cmdCompute).set_Size(new Size(63, 27));
			((Control)cmdCompute).set_TabIndex(3);
			((Control)cmdCompute).set_Text("&Compute");
			((ButtonBase)cmdCompute).set_UseVisualStyleBackColor(true);
			((Control)cmdCompute).add_Click((EventHandler)cmdCompute_Click);
			((Control)cmdCancel).set_Location(new Point(535, 61));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(63, 27));
			((Control)cmdCancel).set_TabIndex(4);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withPredictionToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(618, 24));
			((Control)menuStrip1).set_TabIndex(6);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withPredictionToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withPredictionToolStripMenuItem).set_Name("withPredictionToolStripMenuItem");
			((ToolStripItem)withPredictionToolStripMenuItem).set_Size(new Size(108, 20));
			((ToolStripItem)withPredictionToolStripMenuItem).set_Text("with Prediction...");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print Preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)lstEvents).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstEvents).set_FormattingEnabled(true);
			lstEvents.set_ItemHeight(14);
			((Control)lstEvents).set_Location(new Point(46, 124));
			((Control)lstEvents).set_Name("lstEvents");
			((Control)lstEvents).set_Size(new Size(527, 438));
			((Control)lstEvents).set_TabIndex(5);
			((Control)optNeptune).set_AutoSize(true);
			((Control)optNeptune).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optNeptune).set_Location(new Point(6, 65));
			((Control)optNeptune).set_Name("optNeptune");
			((Control)optNeptune).set_Size(new Size(73, 17));
			((Control)optNeptune).set_TabIndex(3);
			optNeptune.set_TabStop(true);
			((Control)optNeptune).set_Text("Neptune");
			((ButtonBase)optNeptune).set_UseVisualStyleBackColor(true);
			((Control)groupBox1).get_Controls().Add((Control)(object)optNeptune);
			((Control)groupBox1).get_Controls().Add((Control)(object)optJupiter);
			((Control)groupBox1).get_Controls().Add((Control)(object)optUranus);
			((Control)groupBox1).get_Controls().Add((Control)(object)optSaturn);
			((Control)groupBox1).set_Location(new Point(12, 31));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(87, 87));
			((Control)groupBox1).set_TabIndex(0);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Planet");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(618, 569));
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Control)this).get_Controls().Add((Control)(object)lstEvents);
			((Control)this).get_Controls().Add((Control)(object)groupBox3);
			((Control)this).get_Controls().Add((Control)(object)pbar);
			((Control)this).get_Controls().Add((Control)(object)panel3);
			((Control)this).get_Controls().Add((Control)(object)cmdCompute);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationSatellitePlanet", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationSatellitePlanet);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Control)this).set_Name("SatellitePlanetEclipsesTransits");
			((Control)this).set_Text("Eclipses and transits of the major moons of Jupiter, Saturn, Uranus and Neptune");
			((Form)this).add_Load((EventHandler)SatellitePlanetEclipsesTransits_Load);
			((Control)this).add_Resize((EventHandler)SatellitePlanetEclipsesTransits_Resize);
			((Control)groupBox3).ResumeLayout(false);
			((Control)groupBox3).PerformLayout();
			((ISupportInitialize)updnLongitude).EndInit();
			((ISupportInitialize)updnLatitude).EndInit();
			((Control)panel3).ResumeLayout(false);
			((Control)panel3).PerformLayout();
			((ISupportInitialize)updnEndMonth).EndInit();
			((ISupportInitialize)updnEndYear).EndInit();
			((ISupportInitialize)updnStartMonth).EndInit();
			((ISupportInitialize)updnStartYear).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
