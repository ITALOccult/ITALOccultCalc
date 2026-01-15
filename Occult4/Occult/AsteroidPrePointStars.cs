using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class AsteroidPrePointStars : Form
	{
		private readonly string AppPath;

		private const double Radian = 180.0 / Math.PI;

		private const double TwoPi = Math.PI * 2.0;

		private IContainer components;

		private ListBox lstStars;

		private MenuStrip menuStrip1;

		private NumericUpDown updnFaintMagLimit;

		private NumericUpDown updnFaintWidth;

		private NumericUpDown updnBrightWidth;

		private NumericUpDown updnPrePointBrightStar;

		private Button cmdCreateList;

		private GroupBox groupBox1;

		private Label label3;

		private Label label1;

		private GroupBox groupBox2;

		private Label label4;

		private Label label2;

		private ToolStripMenuItem withPredictionToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private GroupBox groupBox3;

		private Label label5;

		private NumericUpDown numericUpDown1;

		private Label label6;

		private TextBox txtSec;

		private TextBox txtMin;

		private TextBox txtHr;

		private CheckBox chkEventTime;

		private ToolStripMenuItem helpToolStripMenuItem;

		private CheckBox chkGetSAO;

		public AsteroidPrePointStars()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
			((Control)this).set_Height((int)(0.95 * (double)Screen.GetWorkingArea((Control)(object)this).Height));
			((Control)this).set_Top((int)(0.02 * (double)((Control)this).get_Height()));
		}

		private void AsteroidPrePointStars_Load(object sender, EventArgs e)
		{
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			numericUpDown1.set_Value(Settings.Default.PrePointLeadTime);
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			if (Utilities.InternetIsAvailable() && !File.Exists(Utilities.AppPath + "\\Resource files\\SAO1950.bin"))
			{
				http.GetSAOcatalogue();
			}
		}

		private void AsteroidPrePointStars_Resize(object sender, EventArgs e)
		{
			if (!((((Control)this).get_Width() < 100) | (((Control)this).get_Height() < 200)))
			{
				((Control)lstStars).set_Width(((Control)this).get_Width() - 25);
				((Control)lstStars).set_Height(((Control)this).get_Height() - 133);
			}
		}

		public void ComputeList(bool AddSAO)
		{
			Cursor.set_Current(Cursors.get_WaitCursor());
			decimal value = updnFaintWidth.get_Value();
			if (updnBrightWidth.get_Value() > value)
			{
				value = updnBrightWidth.get_Value();
			}
			DisplayMPOccultations.ComputePrePointStars((double)value, AddSAO);
			Display(chkEventTime.get_Checked());
			Cursor.set_Current(Cursors.get_Default());
			((Control)this).Focus();
		}

		private void cmdCreateList_Click(object sender, EventArgs e)
		{
			ComputeList(chkGetSAO.get_Checked());
		}

		private string CollectEvents()
		{
			if (lstStars.get_Items().get_Count() < 0)
			{
				return "";
			}
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstStars.get_Items().get_Count();
			for (int i = 0; i < count; i++)
			{
				stringBuilder.AppendLine(lstStars.get_Items().get_Item(i).ToString());
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
			Settings.Default.Save_AsteroidPredictions = Output.SaveAppendPredictionText(CollectEvents(), lstStars.get_Items().get_Item(0).ToString() + "Pre-point stars", Settings.Default.Save_AsteroidPredictions);
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void Display(bool FromBaseTime)
		{
			int num = DisplayMPOccultations.PrePointList.Count - 1;
			if (!double.TryParse(((Control)txtHr).get_Text(), out var result))
			{
				result = 0.0;
			}
			if (!double.TryParse(((Control)txtMin).get_Text(), out var result2))
			{
				result2 = 0.0;
			}
			if (!double.TryParse(((Control)txtSec).get_Text(), out var result3))
			{
				result3 = 0.0;
			}
			double num2 = result + result2 / 60.0 + result3 / 3600.0;
			if (num < 0)
			{
				return;
			}
			lstStars.get_Items().Clear();
			string text = "Occultation of " + DisplayMPOccultations.StarNo.Trim() + " by ";
			int totalWidth = 23 + text.Length / 2;
			lstStars.get_Items().Add((object)text.PadLeft(totalWidth));
			text = DisplayMPOccultations.AsteroidName.Trim() + " on " + DisplayMPOccultations.UTDate;
			totalWidth = 23 + text.Length / 2;
			lstStars.get_Items().Add((object)text.PadLeft(totalWidth));
			lstStars.get_Items().Add((object)"              Pre-point stars");
			lstStars.get_Items().Add((object)"");
			lstStars.get_Items().Add((object)("         " + DisplayMPOccultations.PredictionDate));
			lstStars.get_Items().Add((object)"");
			if (FromBaseTime)
			{
				lstStars.get_Items().Add((object)"  Point               J2000         Dec");
				lstStars.get_Items().Add((object)"  Time     Star    RA       Dec   Offset   SAO");
			}
			else
			{
				lstStars.get_Items().Add((object)"  Time                J2000         Dec");
				lstStars.get_Items().Add((object)" Offset    Star    RA       Dec   Offset   SAO");
			}
			lstStars.get_Items().Add((object)" h  m  s    mag   h   m     o  '  ArcMin");
			lstStars.get_Items().Add((object)"");
			if (FromBaseTime)
			{
				for (int i = 0; i <= num; i++)
				{
					lstStars.get_Items().Add((object)DisplayMPOccultations.PrePointList[i].ToString(num2));
					if (i < num && Math.Floor(num2 - DisplayMPOccultations.PrePointList[i].TOffset) != Math.Floor(num2 - DisplayMPOccultations.PrePointList[i + 1].TOffset))
					{
						lstStars.get_Items().Add((object)"");
					}
				}
				return;
			}
			for (int j = 0; j <= num; j++)
			{
				lstStars.get_Items().Add((object)DisplayMPOccultations.PrePointList[j].ToString());
				if (j < num && Math.Floor(DisplayMPOccultations.PrePointList[j].TOffset) != Math.Floor(DisplayMPOccultations.PrePointList[j + 1].TOffset))
				{
					lstStars.get_Items().Add((object)"");
				}
			}
		}

		private void chkEventTime_CheckedChanged(object sender, EventArgs e)
		{
			Display(chkEventTime.get_Checked());
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Asteroid - prepoint stars");
		}

		private void chkGetSAO_CheckedChanged(object sender, EventArgs e)
		{
			ComputeList(chkGetSAO.get_Checked());
		}

		private void txtHr_TextChanged(object sender, EventArgs e)
		{
			Display(chkEventTime.get_Checked());
		}

		private void txtMin_TextChanged(object sender, EventArgs e)
		{
			Display(chkEventTime.get_Checked());
		}

		private void txtSec_TextChanged(object sender, EventArgs e)
		{
			Display(chkEventTime.get_Checked());
		}

		private void numericUpDown1_ValueChanged(object sender, EventArgs e)
		{
			Settings.Default.PrePointLeadTime = numericUpDown1.get_Value();
			chkGetSAO.set_Checked(false);
			ComputeList(chkGetSAO.get_Checked());
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
			//IL_080d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0817: Expected O, but got Unknown
			//IL_08cd: Unknown result type (might be due to invalid IL or missing references)
			//IL_08d7: Expected O, but got Unknown
			//IL_0b44: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b4e: Expected O, but got Unknown
			//IL_0c07: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c11: Expected O, but got Unknown
			//IL_1274: Unknown result type (might be due to invalid IL or missing references)
			//IL_127e: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(AsteroidPrePointStars));
			lstStars = new ListBox();
			menuStrip1 = new MenuStrip();
			withPredictionToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			cmdCreateList = new Button();
			groupBox1 = new GroupBox();
			label3 = new Label();
			label1 = new Label();
			updnFaintWidth = new NumericUpDown();
			updnFaintMagLimit = new NumericUpDown();
			groupBox2 = new GroupBox();
			label4 = new Label();
			label2 = new Label();
			updnBrightWidth = new NumericUpDown();
			updnPrePointBrightStar = new NumericUpDown();
			groupBox3 = new GroupBox();
			txtSec = new TextBox();
			txtMin = new TextBox();
			txtHr = new TextBox();
			chkEventTime = new CheckBox();
			label6 = new Label();
			numericUpDown1 = new NumericUpDown();
			label5 = new Label();
			chkGetSAO = new CheckBox();
			((Control)menuStrip1).SuspendLayout();
			((Control)groupBox1).SuspendLayout();
			((ISupportInitialize)updnFaintWidth).BeginInit();
			((ISupportInitialize)updnFaintMagLimit).BeginInit();
			((Control)groupBox2).SuspendLayout();
			((ISupportInitialize)updnBrightWidth).BeginInit();
			((ISupportInitialize)updnPrePointBrightStar).BeginInit();
			((Control)groupBox3).SuspendLayout();
			((ISupportInitialize)numericUpDown1).BeginInit();
			((Control)this).SuspendLayout();
			lstStars.set_ColumnWidth(360);
			((Control)lstStars).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstStars).set_FormattingEnabled(true);
			lstStars.set_ItemHeight(14);
			((Control)lstStars).set_Location(new Point(8, 101));
			lstStars.set_MultiColumn(true);
			((Control)lstStars).set_Name("lstStars");
			((Control)lstStars).set_Size(new Size(637, 424));
			((Control)lstStars).set_TabIndex(0);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withPredictionToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(656, 24));
			((Control)menuStrip1).set_TabIndex(1);
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
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(69, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help   ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(53, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)cmdCreateList).set_Anchor((AnchorStyles)1);
			((Control)cmdCreateList).set_Location(new Point(571, 61));
			((Control)cmdCreateList).set_Name("cmdCreateList");
			((Control)cmdCreateList).set_Size(new Size(64, 30));
			((Control)cmdCreateList).set_TabIndex(6);
			((Control)cmdCreateList).set_Text("&List stars");
			((ButtonBase)cmdCreateList).set_UseVisualStyleBackColor(true);
			((Control)cmdCreateList).add_Click((EventHandler)cmdCreateList_Click);
			((Control)groupBox1).set_Anchor((AnchorStyles)1);
			((Control)groupBox1).get_Controls().Add((Control)(object)label3);
			((Control)groupBox1).get_Controls().Add((Control)(object)label1);
			((Control)groupBox1).get_Controls().Add((Control)(object)updnFaintWidth);
			((Control)groupBox1).get_Controls().Add((Control)(object)updnFaintMagLimit);
			((Control)groupBox1).set_Location(new Point(18, 27));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(185, 68));
			((Control)groupBox1).set_TabIndex(7);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Faint star limits");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(79, 23));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(101, 13));
			((Control)label3).set_TabIndex(5);
			((Control)label3).set_Text("Separation [arc-min]");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(6, 23));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(57, 13));
			((Control)label1).set_TabIndex(4);
			((Control)label1).set_Text("Magnitude");
			((Control)updnFaintWidth).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "PrePointFaintWidth", true, (DataSourceUpdateMode)1));
			((Control)updnFaintWidth).set_Location(new Point(108, 39));
			updnFaintWidth.set_Maximum(new decimal(new int[4] { 60, 0, 0, 0 }));
			updnFaintWidth.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnFaintWidth).set_Name("updnFaintWidth");
			((Control)updnFaintWidth).set_Size(new Size(42, 20));
			((Control)updnFaintWidth).set_TabIndex(3);
			((UpDownBase)updnFaintWidth).set_TextAlign((HorizontalAlignment)2);
			updnFaintWidth.set_Value(Settings.Default.PrePointFaintWidth);
			((Control)updnFaintMagLimit).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "PrePointFaintStar", true, (DataSourceUpdateMode)1));
			updnFaintMagLimit.set_DecimalPlaces(1);
			updnFaintMagLimit.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnFaintMagLimit).set_Location(new Point(10, 39));
			updnFaintMagLimit.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnFaintMagLimit.set_Minimum(new decimal(new int[4] { 4, 0, 0, 0 }));
			((Control)updnFaintMagLimit).set_Name("updnFaintMagLimit");
			((Control)updnFaintMagLimit).set_Size(new Size(52, 20));
			((Control)updnFaintMagLimit).set_TabIndex(2);
			((UpDownBase)updnFaintMagLimit).set_TextAlign((HorizontalAlignment)2);
			updnFaintMagLimit.set_Value(Settings.Default.PrePointFaintStar);
			((Control)groupBox2).set_Anchor((AnchorStyles)1);
			((Control)groupBox2).get_Controls().Add((Control)(object)label4);
			((Control)groupBox2).get_Controls().Add((Control)(object)label2);
			((Control)groupBox2).get_Controls().Add((Control)(object)updnBrightWidth);
			((Control)groupBox2).get_Controls().Add((Control)(object)updnPrePointBrightStar);
			((Control)groupBox2).set_Location(new Point(210, 27));
			((Control)groupBox2).set_Name("groupBox2");
			((Control)groupBox2).set_Size(new Size(185, 68));
			((Control)groupBox2).set_TabIndex(8);
			groupBox2.set_TabStop(false);
			((Control)groupBox2).set_Text("Bright star limits");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(77, 23));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(101, 13));
			((Control)label4).set_TabIndex(7);
			((Control)label4).set_Text("Separation [arc-min]");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(6, 23));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(57, 13));
			((Control)label2).set_TabIndex(6);
			((Control)label2).set_Text("Magnitude");
			((Control)updnBrightWidth).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "PrePointBrightWidth", true, (DataSourceUpdateMode)1));
			((Control)updnBrightWidth).set_Location(new Point(106, 39));
			updnBrightWidth.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			updnBrightWidth.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnBrightWidth).set_Name("updnBrightWidth");
			((Control)updnBrightWidth).set_Size(new Size(42, 20));
			((Control)updnBrightWidth).set_TabIndex(5);
			((UpDownBase)updnBrightWidth).set_TextAlign((HorizontalAlignment)2);
			updnBrightWidth.set_Value(Settings.Default.PrePointBrightWidth);
			((Control)updnPrePointBrightStar).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "PrePointBrightStar", true, (DataSourceUpdateMode)1));
			updnPrePointBrightStar.set_DecimalPlaces(1);
			updnPrePointBrightStar.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnPrePointBrightStar).set_Location(new Point(10, 39));
			updnPrePointBrightStar.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnPrePointBrightStar.set_Minimum(new decimal(new int[4] { 4, 0, 0, 0 }));
			((Control)updnPrePointBrightStar).set_Name("updnPrePointBrightStar");
			((Control)updnPrePointBrightStar).set_Size(new Size(52, 20));
			((Control)updnPrePointBrightStar).set_TabIndex(4);
			((UpDownBase)updnPrePointBrightStar).set_TextAlign((HorizontalAlignment)2);
			updnPrePointBrightStar.set_Value(Settings.Default.PrePointBrightStar);
			((Control)groupBox3).set_Anchor((AnchorStyles)1);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtSec);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtMin);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtHr);
			((Control)groupBox3).get_Controls().Add((Control)(object)chkEventTime);
			((Control)groupBox3).get_Controls().Add((Control)(object)label6);
			((Control)groupBox3).get_Controls().Add((Control)(object)numericUpDown1);
			((Control)groupBox3).get_Controls().Add((Control)(object)label5);
			((Control)groupBox3).set_Location(new Point(401, 27));
			((Control)groupBox3).set_Name("groupBox3");
			((Control)groupBox3).set_Size(new Size(152, 68));
			((Control)groupBox3).set_TabIndex(9);
			groupBox3.set_TabStop(false);
			((Control)groupBox3).set_Text("Lead-time limit");
			((Control)txtSec).set_Location(new Point(126, 44));
			((Control)txtSec).set_Name("txtSec");
			((Control)txtSec).set_Size(new Size(21, 20));
			((Control)txtSec).set_TabIndex(12);
			((Control)txtSec).set_Text("0");
			((Control)txtSec).add_TextChanged((EventHandler)txtSec_TextChanged);
			((Control)txtMin).set_Location(new Point(102, 44));
			((Control)txtMin).set_Name("txtMin");
			((Control)txtMin).set_Size(new Size(21, 20));
			((Control)txtMin).set_TabIndex(11);
			((Control)txtMin).set_Text("0");
			((Control)txtMin).add_TextChanged((EventHandler)txtMin_TextChanged);
			((Control)txtHr).set_Location(new Point(78, 44));
			((Control)txtHr).set_Name("txtHr");
			((Control)txtHr).set_Size(new Size(21, 20));
			((Control)txtHr).set_TabIndex(10);
			((Control)txtHr).set_Text("0");
			((Control)txtHr).add_TextChanged((EventHandler)txtHr_TextChanged);
			((Control)chkEventTime).set_AutoSize(true);
			((Control)chkEventTime).set_Location(new Point(4, 46));
			((Control)chkEventTime).set_Name("chkEventTime");
			((Control)chkEventTime).set_Size(new Size(76, 17));
			((Control)chkEventTime).set_TabIndex(9);
			((Control)chkEventTime).set_Text("Event time");
			((ButtonBase)chkEventTime).set_UseVisualStyleBackColor(true);
			chkEventTime.add_CheckedChanged((EventHandler)chkEventTime_CheckedChanged);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(6, 16));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(53, 13));
			((Control)label6).set_TabIndex(8);
			((Control)label6).set_Text("Lead-time");
			((Control)numericUpDown1).set_Location(new Point(71, 14));
			numericUpDown1.set_Maximum(new decimal(new int[4] { 24, 0, 0, 0 }));
			numericUpDown1.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)numericUpDown1).set_Name("numericUpDown1");
			((Control)numericUpDown1).set_Size(new Size(39, 20));
			((Control)numericUpDown1).set_TabIndex(0);
			((UpDownBase)numericUpDown1).set_TextAlign((HorizontalAlignment)2);
			numericUpDown1.set_Value(new decimal(new int[4] { 6, 0, 0, 0 }));
			numericUpDown1.add_ValueChanged((EventHandler)numericUpDown1_ValueChanged);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(109, 16));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(23, 13));
			((Control)label5).set_TabIndex(7);
			((Control)label5).set_Text("Hrs");
			((Control)chkGetSAO).set_Anchor((AnchorStyles)1);
			((Control)chkGetSAO).set_AutoSize(true);
			((Control)chkGetSAO).set_Location(new Point(563, 30));
			((Control)chkGetSAO).set_Name("chkGetSAO");
			((Control)chkGetSAO).set_Size(new Size(87, 30));
			((Control)chkGetSAO).set_TabIndex(10);
			((Control)chkGetSAO).set_Text("Add SAO #'s\r\nusing VizieR");
			((ButtonBase)chkGetSAO).set_UseVisualStyleBackColor(true);
			chkGetSAO.add_CheckedChanged((EventHandler)chkGetSAO_CheckedChanged);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(656, 537));
			((Control)this).get_Controls().Add((Control)(object)chkGetSAO);
			((Control)this).get_Controls().Add((Control)(object)groupBox3);
			((Control)this).get_Controls().Add((Control)(object)groupBox2);
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Control)this).get_Controls().Add((Control)(object)cmdCreateList);
			((Control)this).get_Controls().Add((Control)(object)lstStars);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationAsterPrepoint", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationAsterPrepoint);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("AsteroidPrePointStars");
			((Control)this).set_Text("Asteroid - Pre-point stars");
			((Form)this).add_Load((EventHandler)AsteroidPrePointStars_Load);
			((Control)this).add_Resize((EventHandler)AsteroidPrePointStars_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((ISupportInitialize)updnFaintWidth).EndInit();
			((ISupportInitialize)updnFaintMagLimit).EndInit();
			((Control)groupBox2).ResumeLayout(false);
			((Control)groupBox2).PerformLayout();
			((ISupportInitialize)updnBrightWidth).EndInit();
			((ISupportInitialize)updnPrePointBrightStar).EndInit();
			((Control)groupBox3).ResumeLayout(false);
			((Control)groupBox3).PerformLayout();
			((ISupportInitialize)numericUpDown1).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
