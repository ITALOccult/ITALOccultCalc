using System;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class SiderealTime_etc : Form
	{
		private readonly string AppPath;

		private const double Radian = 180.0 / Math.PI;

		private IContainer components;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withPredictionToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ListBox lstSidereal;

		private Panel panel3;

		private Label label2;

		private Label label3;

		private NumericUpDown updnEndMonth;

		private NumericUpDown updnEndYear;

		private NumericUpDown updnStartMonth;

		private NumericUpDown updnStartYear;

		private Button cmdCompute;

		private NumericUpDown updnInterval;

		private Label label1;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private CheckBox chkHighPrecision;

		public SiderealTime_etc()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
		}

		private void SiderealTime_etc_Load(object sender, EventArgs e)
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
		}

		private void SiderealTime_etc_Resize(object sender, EventArgs e)
		{
			if (!((((Control)this).get_Width() < 400) | (((Control)this).get_Height() < 400)))
			{
				((Control)lstSidereal).set_Width(((Control)this).get_Width() - 23);
				((Control)lstSidereal).set_Height(((Control)this).get_Height() - 135);
			}
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
			Settings.Default.Save_EphemerisData = Output.SaveAppendPredictionText(CollectEvents(), "Sidereal time Etc for " + updnStartYear.get_Value(), Settings.Default.Save_EphemerisData);
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private string CollectEvents()
		{
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstSidereal.get_Items().get_Count();
			for (int i = 0; i < count; i++)
			{
				stringBuilder.AppendLine(lstSidereal.get_Items().get_Item(i).ToString());
			}
			stringBuilder.AppendLine("");
			return stringBuilder.ToString();
		}

		private void cmdCompute_Click(object sender, EventArgs e)
		{
			int num = 0;
			double num2 = Utilities.JD_from_Date((int)updnStartYear.get_Value(), (int)updnStartMonth.get_Value(), 1.0);
			double num3 = Utilities.JD_from_Date((int)updnEndYear.get_Value(), (int)updnEndMonth.get_Value() + 1, 1.0) - num2;
			int num4 = (int)updnInterval.get_Value();
			string text = "";
			int num5 = 0;
			if (chkHighPrecision.get_Checked())
			{
				text = "  ";
				num5 = 1;
			}
			lstSidereal.get_Items().Clear();
			lstSidereal.get_Items().Add((object)"Julian Day Number, Apparent Sidereal Time, Obliquity of the Ecliptic,");
			lstSidereal.get_Items().Add((object)"and UT of Solar transit on the Greenwich meridian");
			lstSidereal.get_Items().Add((object)"");
			lstSidereal.get_Items().Add((object)("                Julian      Sidereal" + text + "      Solar"));
			lstSidereal.get_Items().Add((object)("   Date           day         Time" + text + "       Transit" + text + "     Ecliptic"));
			if (chkHighPrecision.get_Checked())
			{
				lstSidereal.get_Items().Add((object)"                             h  m   s      h  m   s      o  '   \"");
			}
			else
			{
				lstSidereal.get_Items().Add((object)"                             h  m  s     h  m  s     o  '   \"");
			}
			for (int i = 0; (double)i <= num3; i += num4)
			{
				double num6 = num2 + (double)i;
				StringBuilder stringBuilder = new StringBuilder();
				stringBuilder.Append(Utilities.Date_from_JD(num6, 0));
				stringBuilder.AppendFormat("{0,13:F1}", num6);
				double num7 = Utilities.SiderealTime_deg(num6, Apparent: true);
				if (num7 < 0.0)
				{
					num7 += 360.0;
				}
				stringBuilder.Append("    " + Utilities.DEGtoDMS(num7 / 15.0, 2, num5, MinutesOnly: false));
				double degree = Utilities.PlanetTransitTime_HrsDT(num6, 3, 0.0, 0.0);
				stringBuilder.Append("    " + Utilities.DEGtoDMS(degree, 2, num5, MinutesOnly: false));
				Utilities.Nutation(num6, out var _, out var _, out var TrueEcliptic);
				stringBuilder.Append("    " + Utilities.DEGtoDMS(TrueEcliptic * (180.0 / Math.PI), 2, num5 + 1, MinutesOnly: false));
				lstSidereal.get_Items().Add((object)stringBuilder.ToString());
				num++;
				if (num % 5 == 0)
				{
					lstSidereal.get_Items().Add((object)"");
				}
			}
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

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Solar transit");
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
			//IL_0aac: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ab6: Expected O, but got Unknown
			//IL_0c09: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c13: Expected O, but got Unknown
			//IL_0d36: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d40: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(SiderealTime_etc));
			menuStrip1 = new MenuStrip();
			withPredictionToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			lstSidereal = new ListBox();
			panel3 = new Panel();
			label2 = new Label();
			label3 = new Label();
			updnEndMonth = new NumericUpDown();
			updnEndYear = new NumericUpDown();
			updnStartMonth = new NumericUpDown();
			updnStartYear = new NumericUpDown();
			cmdCompute = new Button();
			updnInterval = new NumericUpDown();
			label1 = new Label();
			chkHighPrecision = new CheckBox();
			((Control)menuStrip1).SuspendLayout();
			((Control)panel3).SuspendLayout();
			((ISupportInitialize)updnEndMonth).BeginInit();
			((ISupportInitialize)updnEndYear).BeginInit();
			((ISupportInitialize)updnStartMonth).BeginInit();
			((ISupportInitialize)updnStartYear).BeginInit();
			((ISupportInitialize)updnInterval).BeginInit();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withPredictionToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(521, 24));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withPredictionToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withPredictionToolStripMenuItem).set_Name("withPredictionToolStripMenuItem");
			((ToolStripItem)withPredictionToolStripMenuItem).set_Size(new Size(114, 20));
			((ToolStripItem)withPredictionToolStripMenuItem).set_Text("with Prediction...  ");
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
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(53, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)lstSidereal).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstSidereal).set_FormattingEnabled(true);
			lstSidereal.set_ItemHeight(14);
			((Control)lstSidereal).set_Location(new Point(7, 88));
			((Control)lstSidereal).set_Name("lstSidereal");
			((Control)lstSidereal).set_Size(new Size(506, 424));
			((Control)lstSidereal).set_TabIndex(5);
			((Control)panel3).set_Anchor((AnchorStyles)1);
			((Control)panel3).get_Controls().Add((Control)(object)label2);
			((Control)panel3).get_Controls().Add((Control)(object)label3);
			((Control)panel3).get_Controls().Add((Control)(object)updnEndMonth);
			((Control)panel3).get_Controls().Add((Control)(object)updnEndYear);
			((Control)panel3).get_Controls().Add((Control)(object)updnStartMonth);
			((Control)panel3).get_Controls().Add((Control)(object)updnStartYear);
			((Control)panel3).set_Location(new Point(22, 27));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(207, 57));
			((Control)panel3).set_TabIndex(6);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(5, 33));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(92, 13));
			((Control)label2).set_TabIndex(3);
			((Control)label2).set_Text("End Year && month");
			label2.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(2, 6));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(95, 13));
			((Control)label3).set_TabIndex(0);
			((Control)label3).set_Text("Start Year && month");
			label3.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)updnEndMonth).set_Location(new Point(162, 31));
			updnEndMonth.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnEndMonth.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnEndMonth).set_Name("updnEndMonth");
			((Control)updnEndMonth).set_Size(new Size(39, 20));
			((Control)updnEndMonth).set_TabIndex(5);
			updnEndMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnEndMonth).add_Enter((EventHandler)updnEndMonth_Enter);
			((Control)updnEndYear).set_Location(new Point(99, 31));
			updnEndYear.set_Maximum(new decimal(new int[4] { 19999, 0, 0, 0 }));
			updnEndYear.set_Minimum(new decimal(new int[4] { 19999, 0, 0, -2147483648 }));
			((Control)updnEndYear).set_Name("updnEndYear");
			((Control)updnEndYear).set_Size(new Size(55, 20));
			((Control)updnEndYear).set_TabIndex(4);
			updnEndYear.set_Value(new decimal(new int[4] { 2007, 0, 0, 0 }));
			updnEndYear.add_ValueChanged((EventHandler)updnEndYear_ValueChanged);
			((Control)updnEndYear).add_Enter((EventHandler)updnEndYear_Enter);
			((Control)updnStartMonth).set_Location(new Point(161, 4));
			updnStartMonth.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnStartMonth.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnStartMonth).set_Name("updnStartMonth");
			((Control)updnStartMonth).set_Size(new Size(40, 20));
			((Control)updnStartMonth).set_TabIndex(2);
			updnStartMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnStartMonth).add_Enter((EventHandler)updnStartMonth_Enter);
			((Control)updnStartYear).set_Location(new Point(99, 4));
			updnStartYear.set_Maximum(new decimal(new int[4] { 17000, 0, 0, 0 }));
			updnStartYear.set_Minimum(new decimal(new int[4] { 13000, 0, 0, -2147483648 }));
			((Control)updnStartYear).set_Name("updnStartYear");
			((Control)updnStartYear).set_Size(new Size(55, 20));
			((Control)updnStartYear).set_TabIndex(1);
			updnStartYear.set_Value(new decimal(new int[4] { 2007, 0, 0, 0 }));
			updnStartYear.add_ValueChanged((EventHandler)updnStartYear_ValueChanged);
			((Control)updnStartYear).add_Enter((EventHandler)updnStartYear_Enter);
			((Control)cmdCompute).set_Anchor((AnchorStyles)1);
			((Control)cmdCompute).set_Location(new Point(411, 41));
			((Control)cmdCompute).set_Name("cmdCompute");
			((Control)cmdCompute).set_Size(new Size(77, 28));
			((Control)cmdCompute).set_TabIndex(7);
			((Control)cmdCompute).set_Text("Compute");
			((ButtonBase)cmdCompute).set_UseVisualStyleBackColor(true);
			((Control)cmdCompute).add_Click((EventHandler)cmdCompute_Click);
			((Control)updnInterval).set_Anchor((AnchorStyles)1);
			((Control)updnInterval).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "EphemerisInterval", true, (DataSourceUpdateMode)1));
			((Control)updnInterval).set_Location(new Point(320, 33));
			updnInterval.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnInterval).set_Name("updnInterval");
			((Control)updnInterval).set_Size(new Size(49, 20));
			((Control)updnInterval).set_TabIndex(8);
			updnInterval.set_Value(Settings.Default.EphemerisInterval);
			((Control)updnInterval).add_Enter((EventHandler)updnInterval_Enter);
			((Control)label1).set_Anchor((AnchorStyles)1);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(241, 35));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(73, 13));
			((Control)label1).set_TabIndex(9);
			((Control)label1).set_Text("Interval (days)");
			((Control)chkHighPrecision).set_Anchor((AnchorStyles)1);
			((Control)chkHighPrecision).set_AutoSize(true);
			chkHighPrecision.set_CheckAlign(ContentAlignment.MiddleRight);
			chkHighPrecision.set_Checked(Settings.Default.EphemJD_Sidereal_HighPrecision);
			((Control)chkHighPrecision).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "EphemJD_Sidereal_HighPrecision", true, (DataSourceUpdateMode)1));
			((Control)chkHighPrecision).set_Location(new Point(240, 64));
			((Control)chkHighPrecision).set_Name("chkHighPrecision");
			((Control)chkHighPrecision).set_Size(new Size(94, 17));
			((Control)chkHighPrecision).set_TabIndex(10);
			((Control)chkHighPrecision).set_Text("High Precision");
			((ButtonBase)chkHighPrecision).set_UseVisualStyleBackColor(true);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(521, 525));
			((Control)this).get_Controls().Add((Control)(object)chkHighPrecision);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)updnInterval);
			((Control)this).get_Controls().Add((Control)(object)cmdCompute);
			((Control)this).get_Controls().Add((Control)(object)panel3);
			((Control)this).get_Controls().Add((Control)(object)lstSidereal);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationEphemSiderial", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationEphemSiderial);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("SiderealTime_etc");
			((Control)this).set_Text("Julian Date, Sidereal time, Solar Transit, and Ecliptic");
			((Form)this).add_Load((EventHandler)SiderealTime_etc_Load);
			((Control)this).add_Resize((EventHandler)SiderealTime_etc_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)panel3).ResumeLayout(false);
			((Control)panel3).PerformLayout();
			((ISupportInitialize)updnEndMonth).EndInit();
			((ISupportInitialize)updnEndYear).EndInit();
			((ISupportInitialize)updnStartMonth).EndInit();
			((ISupportInitialize)updnStartYear).EndInit();
			((ISupportInitialize)updnInterval).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
