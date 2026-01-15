using System;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class PlanetRiseSet : Form
	{
		private readonly string AppPath;

		private bool CancelFlag;

		private IContainer components;

		private MenuStrip menuStrip1;

		private Label label2;

		private Label label1;

		private NumericUpDown updnEndMonth;

		private NumericUpDown updnEndYear;

		private NumericUpDown updnStartMonth;

		private NumericUpDown updnStartYear;

		private ToolStripMenuItem withPredictionToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ListBox lstRiseSet;

		private Button cmdCompute;

		private Button cmdCancel;

		private NumericUpDown updnInterval;

		private Panel panel1;

		private GroupBox groupBox1;

		private Label label6;

		private Label label5;

		private Label label4;

		private Label label3;

		private NumericUpDown updnLatitude;

		private NumericUpDown updnLongitude;

		private NumericUpDown updnTimeZone;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		public PlanetRiseSet()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
		}

		private void PlanetRiseSet_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			updnStartYear.set_Value((decimal)DateTime.Now.Year);
			updnStartMonth.set_Value((decimal)DateTime.Now.Month);
			updnEndYear.set_Value((decimal)DateTime.Now.Year);
			updnEndMonth.set_Value(12m);
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
			if (!decimal.TryParse(Settings.Default.Site_TimeZone_Hrs, out result))
			{
				result = default(decimal);
			}
			updnTimeZone.set_Value(result);
		}

		private void PlanetRiseSet_Resize(object sender, EventArgs e)
		{
			if (!((((Control)this).get_Width() < 600) | (((Control)this).get_Height() < 400)))
			{
				((Control)lstRiseSet).set_Width(((Control)this).get_Width() - 29);
				((Control)lstRiseSet).set_Height(((Control)this).get_Height() - 130);
			}
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			CancelFlag = true;
		}

		private void cmdCompute_Click(object sender, EventArgs e)
		{
			((Control)cmdCompute).set_Visible(false);
			RiseSet();
			((Control)cmdCompute).set_Visible(true);
		}

		private void RiseSet()
		{
			double num = Utilities.JD_from_Date((int)updnStartYear.get_Value(), (int)updnStartMonth.get_Value(), 1.0);
			double num2 = Utilities.JD_from_Date((int)updnEndYear.get_Value(), (int)updnEndMonth.get_Value(), 31.0);
			double num3 = (double)updnInterval.get_Value();
			double[] TRiseSet_hrs = new double[18];
			int num4 = 0;
			double longitude_deg = (double)updnLongitude.get_Value();
			double latitude_deg = (double)updnLatitude.get_Value();
			float timeZone_hrs = (float)updnTimeZone.get_Value();
			CancelFlag = false;
			lstRiseSet.get_Items().Clear();
			lstRiseSet.get_Items().Add((object)"Local time of Rise and Set ");
			lstRiseSet.get_Items().Add((object)"");
			lstRiseSet.get_Items().Add((object)"      Date        Sun        Mercury       Venus         Mars       Jupiter       Saturn       Uranus      Neptune");
			lstRiseSet.get_Items().Add((object)"               Rise  Set    Rise  Set    Rise  Set    Rise  Set    Rise  Set    Rise  Set    Rise  Set    Rise  Set");
			lstRiseSet.BeginUpdate();
			for (double num5 = num; num5 <= num2; num5 += num3)
			{
				StringBuilder stringBuilder = new StringBuilder();
				Utilities.PlanetsRiseSetTime(num5, longitude_deg, latitude_deg, timeZone_hrs, ref TRiseSet_hrs);
				stringBuilder.Append(Utilities.Date_from_JD(num5, 0) + " ");
				stringBuilder.AppendFormat("  {0} {1}", Utilities.DEGtoDMS(TRiseSet_hrs[4], 2, 0, MinutesOnly: true), Utilities.DEGtoDMS(TRiseSet_hrs[5], 2, 0, MinutesOnly: true));
				stringBuilder.AppendFormat("  {0} {1}", Utilities.DEGtoDMS(TRiseSet_hrs[0], 2, 0, MinutesOnly: true), Utilities.DEGtoDMS(TRiseSet_hrs[1], 2, 0, MinutesOnly: true));
				stringBuilder.AppendFormat("  {0} {1}", Utilities.DEGtoDMS(TRiseSet_hrs[2], 2, 0, MinutesOnly: true), Utilities.DEGtoDMS(TRiseSet_hrs[3], 2, 0, MinutesOnly: true));
				stringBuilder.AppendFormat("  {0} {1}", Utilities.DEGtoDMS(TRiseSet_hrs[6], 2, 0, MinutesOnly: true), Utilities.DEGtoDMS(TRiseSet_hrs[7], 2, 0, MinutesOnly: true));
				stringBuilder.AppendFormat("  {0} {1}", Utilities.DEGtoDMS(TRiseSet_hrs[8], 2, 0, MinutesOnly: true), Utilities.DEGtoDMS(TRiseSet_hrs[9], 2, 0, MinutesOnly: true));
				stringBuilder.AppendFormat("  {0} {1}", Utilities.DEGtoDMS(TRiseSet_hrs[10], 2, 0, MinutesOnly: true), Utilities.DEGtoDMS(TRiseSet_hrs[11], 2, 0, MinutesOnly: true));
				stringBuilder.AppendFormat("  {0} {1}", Utilities.DEGtoDMS(TRiseSet_hrs[12], 2, 0, MinutesOnly: true), Utilities.DEGtoDMS(TRiseSet_hrs[13], 2, 0, MinutesOnly: true));
				stringBuilder.AppendFormat("  {0} {1}", Utilities.DEGtoDMS(TRiseSet_hrs[14], 2, 0, MinutesOnly: true), Utilities.DEGtoDMS(TRiseSet_hrs[15], 2, 0, MinutesOnly: true));
				lstRiseSet.get_Items().Add((object)stringBuilder.ToString());
				num4++;
				if (num4 % 5 == 0)
				{
					lstRiseSet.get_Items().Add((object)"");
				}
				Application.DoEvents();
				if (CancelFlag)
				{
					break;
				}
			}
			CancelFlag = false;
			lstRiseSet.EndUpdate();
		}

		private string CollectEvents()
		{
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstRiseSet.get_Items().get_Count();
			for (int i = 0; i < count; i++)
			{
				stringBuilder.AppendLine(lstRiseSet.get_Items().get_Item(i).ToString());
			}
			stringBuilder.AppendLine("");
			return stringBuilder.ToString();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents());
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
			Settings.Default.Save_EphemerisData = Output.SaveAppendPredictionText(CollectEvents(), "Planet Rise-Set Times", Settings.Default.Save_EphemerisData);
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void updnStartYear_ValueChanged(object sender, EventArgs e)
		{
			if (updnStartYear.get_Value() > updnEndYear.get_Value())
			{
				updnEndYear.set_Value(updnStartYear.get_Value());
			}
		}

		private void updnEndYear_ValueChanged(object sender, EventArgs e)
		{
			if (updnStartYear.get_Value() > updnEndYear.get_Value())
			{
				updnStartYear.set_Value(updnEndYear.get_Value());
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

		private void updnInterval_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnInterval).Select(0, 10);
		}

		private void updnLongitude_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnLongitude).Select(0, 10);
		}

		private void updnLatitude_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnLatitude).Select(0, 10);
		}

		private void updnTimeZone_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnTimeZone).Select(0, 10);
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Planet Rise Set times");
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
			//IL_0abc: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ac6: Expected O, but got Unknown
			//IL_11a2: Unknown result type (might be due to invalid IL or missing references)
			//IL_11ac: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(PlanetRiseSet));
			menuStrip1 = new MenuStrip();
			withPredictionToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			label2 = new Label();
			label1 = new Label();
			updnEndMonth = new NumericUpDown();
			updnEndYear = new NumericUpDown();
			updnStartMonth = new NumericUpDown();
			updnStartYear = new NumericUpDown();
			lstRiseSet = new ListBox();
			cmdCompute = new Button();
			cmdCancel = new Button();
			updnInterval = new NumericUpDown();
			panel1 = new Panel();
			groupBox1 = new GroupBox();
			updnTimeZone = new NumericUpDown();
			updnLatitude = new NumericUpDown();
			updnLongitude = new NumericUpDown();
			label5 = new Label();
			label4 = new Label();
			label6 = new Label();
			label3 = new Label();
			((Control)menuStrip1).SuspendLayout();
			((ISupportInitialize)updnEndMonth).BeginInit();
			((ISupportInitialize)updnEndYear).BeginInit();
			((ISupportInitialize)updnStartMonth).BeginInit();
			((ISupportInitialize)updnStartYear).BeginInit();
			((ISupportInitialize)updnInterval).BeginInit();
			((Control)panel1).SuspendLayout();
			((Control)groupBox1).SuspendLayout();
			((ISupportInitialize)updnTimeZone).BeginInit();
			((ISupportInitialize)updnLatitude).BeginInit();
			((ISupportInitialize)updnLongitude).BeginInit();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withPredictionToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(859, 24));
			((Control)menuStrip1).set_TabIndex(7);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withPredictionToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withPredictionToolStripMenuItem).set_Name("withPredictionToolStripMenuItem");
			((ToolStripItem)withPredictionToolStripMenuItem).set_Size(new Size(117, 20));
			((ToolStripItem)withPredictionToolStripMenuItem).set_Text("with Prediction...   ");
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
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print preview");
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
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(75, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help     ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(53, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(4, 32));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(92, 13));
			((Control)label2).set_TabIndex(3);
			((Control)label2).set_Text("End Year && month");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(1, 10));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(95, 13));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text("Start Year && month");
			((Control)updnEndMonth).set_Location(new Point(161, 28));
			updnEndMonth.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnEndMonth.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnEndMonth).set_Name("updnEndMonth");
			((Control)updnEndMonth).set_Size(new Size(39, 20));
			((Control)updnEndMonth).set_TabIndex(5);
			updnEndMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnEndMonth).add_Enter((EventHandler)updnEndMonth_Enter);
			((Control)updnEndYear).set_Location(new Point(98, 28));
			updnEndYear.set_Maximum(new decimal(new int[4] { 19999, 0, 0, 0 }));
			updnEndYear.set_Minimum(new decimal(new int[4] { 19999, 0, 0, -2147483648 }));
			((Control)updnEndYear).set_Name("updnEndYear");
			((Control)updnEndYear).set_Size(new Size(55, 20));
			((Control)updnEndYear).set_TabIndex(4);
			updnEndYear.set_Value(new decimal(new int[4] { 1000, 0, 0, 0 }));
			updnEndYear.add_ValueChanged((EventHandler)updnEndYear_ValueChanged);
			((Control)updnEndYear).add_Enter((EventHandler)updnEndYear_Enter);
			((Control)updnStartMonth).set_Location(new Point(160, 6));
			updnStartMonth.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnStartMonth.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnStartMonth).set_Name("updnStartMonth");
			((Control)updnStartMonth).set_Size(new Size(40, 20));
			((Control)updnStartMonth).set_TabIndex(2);
			updnStartMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnStartMonth).add_Enter((EventHandler)updnStartMonth_Enter);
			((Control)updnStartYear).set_Location(new Point(98, 6));
			updnStartYear.set_Maximum(new decimal(new int[4] { 17000, 0, 0, 0 }));
			updnStartYear.set_Minimum(new decimal(new int[4] { 13000, 0, 0, -2147483648 }));
			((Control)updnStartYear).set_Name("updnStartYear");
			((Control)updnStartYear).set_Size(new Size(55, 20));
			((Control)updnStartYear).set_TabIndex(1);
			updnStartYear.set_Value(new decimal(new int[4] { 1000, 0, 0, 0 }));
			updnStartYear.add_ValueChanged((EventHandler)updnStartYear_ValueChanged);
			((Control)updnStartYear).add_Enter((EventHandler)updnStartYear_Enter);
			((Control)lstRiseSet).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstRiseSet).set_FormattingEnabled(true);
			lstRiseSet.set_ItemHeight(14);
			((Control)lstRiseSet).set_Location(new Point(11, 90));
			((Control)lstRiseSet).set_Name("lstRiseSet");
			((Control)lstRiseSet).set_Size(new Size(836, 354));
			((Control)lstRiseSet).set_TabIndex(6);
			((Control)cmdCompute).set_Anchor((AnchorStyles)1);
			((Control)cmdCompute).set_Location(new Point(697, 46));
			((Control)cmdCompute).set_Name("cmdCompute");
			((Control)cmdCompute).set_Size(new Size(75, 27));
			((Control)cmdCompute).set_TabIndex(4);
			((Control)cmdCompute).set_Text("Compute");
			((ButtonBase)cmdCompute).set_UseVisualStyleBackColor(true);
			((Control)cmdCompute).add_Click((EventHandler)cmdCompute_Click);
			((Control)cmdCancel).set_Anchor((AnchorStyles)1);
			((Control)cmdCancel).set_Location(new Point(697, 46));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(75, 27));
			((Control)cmdCancel).set_TabIndex(5);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)updnInterval).set_Anchor((AnchorStyles)1);
			((Control)updnInterval).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "EphemerisInterval", true, (DataSourceUpdateMode)1));
			((Control)updnInterval).set_Location(new Point(318, 60));
			updnInterval.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnInterval).set_Name("updnInterval");
			((Control)updnInterval).set_Size(new Size(47, 20));
			((Control)updnInterval).set_TabIndex(2);
			updnInterval.set_Value(Settings.Default.EphemerisInterval);
			((Control)updnInterval).add_Enter((EventHandler)updnInterval_Enter);
			((Control)panel1).set_Anchor((AnchorStyles)1);
			((Control)panel1).get_Controls().Add((Control)(object)label1);
			((Control)panel1).get_Controls().Add((Control)(object)updnStartYear);
			((Control)panel1).get_Controls().Add((Control)(object)updnStartMonth);
			((Control)panel1).get_Controls().Add((Control)(object)label2);
			((Control)panel1).get_Controls().Add((Control)(object)updnEndYear);
			((Control)panel1).get_Controls().Add((Control)(object)updnEndMonth);
			((Control)panel1).set_Location(new Point(86, 32));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(210, 48));
			((Control)panel1).set_TabIndex(0);
			((Control)groupBox1).set_Anchor((AnchorStyles)1);
			((Control)groupBox1).get_Controls().Add((Control)(object)updnTimeZone);
			((Control)groupBox1).get_Controls().Add((Control)(object)updnLatitude);
			((Control)groupBox1).get_Controls().Add((Control)(object)updnLongitude);
			((Control)groupBox1).get_Controls().Add((Control)(object)label5);
			((Control)groupBox1).get_Controls().Add((Control)(object)label4);
			((Control)groupBox1).get_Controls().Add((Control)(object)label6);
			((Control)groupBox1).set_Location(new Point(422, 28));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(194, 60));
			((Control)groupBox1).set_TabIndex(3);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Location");
			updnTimeZone.set_DecimalPlaces(1);
			updnTimeZone.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnTimeZone).set_Location(new Point(125, 38));
			updnTimeZone.set_Maximum(new decimal(new int[4] { 14, 0, 0, 0 }));
			updnTimeZone.set_Minimum(new decimal(new int[4] { 12, 0, 0, -2147483648 }));
			((Control)updnTimeZone).set_Name("updnTimeZone");
			((Control)updnTimeZone).set_Size(new Size(46, 20));
			((Control)updnTimeZone).set_TabIndex(5);
			((Control)updnTimeZone).add_Enter((EventHandler)updnTimeZone_Enter);
			updnLatitude.set_DecimalPlaces(1);
			((Control)updnLatitude).set_Location(new Point(59, 38));
			updnLatitude.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			updnLatitude.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)updnLatitude).set_Name("updnLatitude");
			((Control)updnLatitude).set_Size(new Size(48, 20));
			((Control)updnLatitude).set_TabIndex(3);
			((Control)updnLatitude).add_Enter((EventHandler)updnLatitude_Enter);
			updnLongitude.set_DecimalPlaces(1);
			((Control)updnLongitude).set_Location(new Point(59, 13));
			updnLongitude.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			updnLongitude.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)updnLongitude).set_Name("updnLongitude");
			((Control)updnLongitude).set_Size(new Size(54, 20));
			((Control)updnLongitude).set_TabIndex(1);
			((Control)updnLongitude).add_Enter((EventHandler)updnLongitude_Enter);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(15, 40));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(41, 13));
			((Control)label5).set_TabIndex(2);
			((Control)label5).set_Text("latitude");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(6, 16));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(50, 13));
			((Control)label4).set_TabIndex(0);
			((Control)label4).set_Text("longitude");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(119, 9));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(58, 26));
			((Control)label6).set_TabIndex(4);
			((Control)label6).set_Text("Time Zone\r\n[hrs]");
			label6.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label3).set_Anchor((AnchorStyles)1);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(315, 43));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(42, 13));
			((Control)label3).set_TabIndex(1);
			((Control)label3).set_Text("Interval");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(859, 460));
			((Control)this).get_Controls().Add((Control)(object)cmdCompute);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)updnInterval);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_Controls().Add((Control)(object)lstRiseSet);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationEphemPlanetRiseSet", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationEphemPlanetRiseSet);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MinimumSize(new Size(500, 400));
			((Control)this).set_Name("PlanetRiseSet");
			((Control)this).set_Text("Rise and set times of the planets");
			((Form)this).add_Load((EventHandler)PlanetRiseSet_Load);
			((Control)this).add_Resize((EventHandler)PlanetRiseSet_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((ISupportInitialize)updnEndMonth).EndInit();
			((ISupportInitialize)updnEndYear).EndInit();
			((ISupportInitialize)updnStartMonth).EndInit();
			((ISupportInitialize)updnStartYear).EndInit();
			((ISupportInitialize)updnInterval).EndInit();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((ISupportInitialize)updnTimeZone).EndInit();
			((ISupportInitialize)updnLatitude).EndInit();
			((ISupportInitialize)updnLongitude).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
