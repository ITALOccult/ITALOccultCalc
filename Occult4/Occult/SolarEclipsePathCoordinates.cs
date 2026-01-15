using System;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class SolarEclipsePathCoordinates : Form
	{
		private static bool Add5th = true;

		private IContainer components;

		private MenuStrip menuStrip1;

		private ListBox lstCoordinates;

		private NumericUpDown updnWest;

		private NumericUpDown updnEast;

		private Button cmdCompute;

		private Label label1;

		private Label label2;

		private Label label3;

		private Label label4;

		private NumericUpDown updnInterval;

		private ToolStripMenuItem withPredictionsToolStripMenuItem;

		private ToolStripMenuItem include5thlineBreakToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem multisitePredictionToolStripMenuItem;

		private ToolStripMenuItem detailedMapToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator3;

		private ToolStripMenuItem localPredictionsToolStripMenuItem;

		private ToolStripMenuItem multiLocationPredictionsToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		public SolarEclipsePathCoordinates()
		{
			InitializeComponent();
		}

		private void SolarEclipsePathCoordinates_Load(object sender, EventArgs e)
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
		}

		private void SolarEclipsePathCoordinates_Resize(object sender, EventArgs e)
		{
			if (!((((Control)this).get_Height() < 150) | (((Control)this).get_Width() < 100)))
			{
				((Control)lstCoordinates).set_Width(((Control)this).get_Width() - 25);
				((Control)lstCoordinates).set_Height(((Control)this).get_Height() - 108);
			}
		}

		private void cmdCompute_Click(object sender, EventArgs e)
		{
			ComputeCoordinates();
		}

		public void ComputeCoordinates()
		{
			SolarEclipses.ComputePathCoords((double)updnWest.get_Value(), (double)updnEast.get_Value(), (double)updnInterval.get_Value());
			Display();
		}

		private void Display()
		{
			lstCoordinates.BeginUpdate();
			lstCoordinates.get_Items().Clear();
			lstCoordinates.get_Items().Add((object)(SolarEclipses.SolarEclipseLabel + " - Path coordinates"));
			lstCoordinates.get_Items().Add((object)"");
			lstCoordinates.get_Items().Add((object)"               Central Eclipse Details             Central Path Limits       Penumbral Limits ");
			lstCoordinates.get_Items().Add((object)"Longitude       U.T.    Durn.   Mag  Alt       Centre     North     South     North    South  ");
			lstCoordinates.get_Items().Add((object)"   o   '      h  m  s     s            o       o   '     o   '     o   '       o  '     o  '  ");
			int count = SolarEclipses.Coordinates.Count;
			for (int i = 0; i < count; i++)
			{
				lstCoordinates.get_Items().Add((object)SolarEclipses.Coordinates[i]!.ToString());
				if (((i + 1) % 5 == 0) & Add5th)
				{
					lstCoordinates.get_Items().Add((object)"");
				}
			}
			lstCoordinates.EndUpdate();
			((Control)this).Focus();
		}

		private void updnWest_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnWest).Select(0, 10);
		}

		private void updnEast_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnEast).Select(0, 10);
		}

		private void updnInterval_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnInterval).Select(0, 10);
		}

		private void detailedMapToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.ShowLocalMap(UseCursorPosition: false);
		}

		private void localPredictionsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.ShowSingleLocation();
		}

		private void multiLocationPredictionsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SolarEclipses.Show_Multilocations();
		}

		private void include5thlineBreakToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Add5th = !Add5th;
			include5thlineBreakToolStripMenuItem.set_Checked(Add5th);
			Display();
		}

		private string CollectEvents()
		{
			if (lstCoordinates.get_Items().get_Count() < 0)
			{
				return "";
			}
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstCoordinates.get_Items().get_Count();
			for (int i = 0; i < count; i++)
			{
				stringBuilder.AppendLine(lstCoordinates.get_Items().get_Item(i).ToString());
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
			Settings.Default.Save_SolarEclipses = Output.SaveAppendPredictionText(CollectEvents(), SolarEclipses.SolarEclipseLabel + " - Path", Settings.Default.Save_SolarEclipses);
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Solar eclipse - path coordinates");
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
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Expected O, but got Unknown
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
			//IL_0b85: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b8f: Expected O, but got Unknown
			//IL_0d5f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d69: Expected O, but got Unknown
			menuStrip1 = new MenuStrip();
			withPredictionsToolStripMenuItem = new ToolStripMenuItem();
			include5thlineBreakToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			multisitePredictionToolStripMenuItem = new ToolStripMenuItem();
			detailedMapToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator3 = new ToolStripSeparator();
			localPredictionsToolStripMenuItem = new ToolStripMenuItem();
			multiLocationPredictionsToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			lstCoordinates = new ListBox();
			updnWest = new NumericUpDown();
			updnEast = new NumericUpDown();
			cmdCompute = new Button();
			label1 = new Label();
			label2 = new Label();
			label3 = new Label();
			label4 = new Label();
			updnInterval = new NumericUpDown();
			((Control)menuStrip1).SuspendLayout();
			((ISupportInitialize)updnWest).BeginInit();
			((ISupportInitialize)updnEast).BeginInit();
			((ISupportInitialize)updnInterval).BeginInit();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)withPredictionsToolStripMenuItem,
				(ToolStripItem)multisitePredictionToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(703, 24));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withPredictionsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)include5thlineBreakToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withPredictionsToolStripMenuItem).set_Name("withPredictionsToolStripMenuItem");
			((ToolStripItem)withPredictionsToolStripMenuItem).set_Size(new Size(113, 20));
			((ToolStripItem)withPredictionsToolStripMenuItem).set_Text("with Predictions...");
			include5thlineBreakToolStripMenuItem.set_Checked(true);
			include5thlineBreakToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)include5thlineBreakToolStripMenuItem).set_Name("include5thlineBreakToolStripMenuItem");
			include5thlineBreakToolStripMenuItem.set_ShortcutKeys((Keys)131125);
			((ToolStripItem)include5thlineBreakToolStripMenuItem).set_Size(new Size(229, 22));
			((ToolStripItem)include5thlineBreakToolStripMenuItem).set_Text("Include 5th-line break");
			((ToolStripItem)include5thlineBreakToolStripMenuItem).add_Click((EventHandler)include5thlineBreakToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(226, 6));
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(229, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(229, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(229, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(229, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripDropDownItem)multisitePredictionToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)detailedMapToolStripMenuItem,
				(ToolStripItem)toolStripSeparator3,
				(ToolStripItem)localPredictionsToolStripMenuItem,
				(ToolStripItem)multiLocationPredictionsToolStripMenuItem
			});
			((ToolStripItem)multisitePredictionToolStripMenuItem).set_Name("multisitePredictionToolStripMenuItem");
			((ToolStripItem)multisitePredictionToolStripMenuItem).set_Size(new Size(133, 20));
			((ToolStripItem)multisitePredictionToolStripMenuItem).set_Text("Detailed predictions...");
			((ToolStripItem)detailedMapToolStripMenuItem).set_Name("detailedMapToolStripMenuItem");
			((ToolStripItem)detailedMapToolStripMenuItem).set_Size(new Size(210, 22));
			((ToolStripItem)detailedMapToolStripMenuItem).set_Text("Detailed map");
			((ToolStripItem)detailedMapToolStripMenuItem).add_Click((EventHandler)detailedMapToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator3).set_Name("toolStripSeparator3");
			((ToolStripItem)toolStripSeparator3).set_Size(new Size(207, 6));
			((ToolStripItem)localPredictionsToolStripMenuItem).set_Name("localPredictionsToolStripMenuItem");
			((ToolStripItem)localPredictionsToolStripMenuItem).set_Size(new Size(210, 22));
			((ToolStripItem)localPredictionsToolStripMenuItem).set_Text("Local predictions");
			((ToolStripItem)localPredictionsToolStripMenuItem).add_Click((EventHandler)localPredictionsToolStripMenuItem_Click);
			((ToolStripItem)multiLocationPredictionsToolStripMenuItem).set_Name("multiLocationPredictionsToolStripMenuItem");
			((ToolStripItem)multiLocationPredictionsToolStripMenuItem).set_Size(new Size(210, 22));
			((ToolStripItem)multiLocationPredictionsToolStripMenuItem).set_Text("MultiLocation predictions");
			((ToolStripItem)multiLocationPredictionsToolStripMenuItem).add_Click((EventHandler)multiLocationPredictionsToolStripMenuItem_Click);
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
			((Control)lstCoordinates).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstCoordinates).set_FormattingEnabled(true);
			lstCoordinates.set_ItemHeight(14);
			((Control)lstCoordinates).set_Location(new Point(9, 63));
			((Control)lstCoordinates).set_Name("lstCoordinates");
			((Control)lstCoordinates).set_Size(new Size(686, 592));
			((Control)lstCoordinates).set_TabIndex(1);
			((Control)updnWest).set_Anchor((AnchorStyles)1);
			((Control)updnWest).set_Location(new Point(212, 33));
			updnWest.set_Maximum(new decimal(new int[4] { 359, 0, 0, 0 }));
			updnWest.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)updnWest).set_Name("updnWest");
			((Control)updnWest).set_Size(new Size(56, 20));
			((Control)updnWest).set_TabIndex(2);
			updnWest.set_Value(new decimal(new int[4] { 40, 0, 0, -2147483648 }));
			((Control)updnWest).add_Enter((EventHandler)updnWest_Enter);
			((Control)updnEast).set_Anchor((AnchorStyles)1);
			((Control)updnEast).set_Location(new Point(323, 33));
			updnEast.set_Maximum(new decimal(new int[4] { 359, 0, 0, 0 }));
			updnEast.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)updnEast).set_Name("updnEast");
			((Control)updnEast).set_Size(new Size(60, 20));
			((Control)updnEast).set_TabIndex(3);
			updnEast.set_Value(new decimal(new int[4] { 40, 0, 0, 0 }));
			((Control)updnEast).add_Enter((EventHandler)updnEast_Enter);
			((Control)cmdCompute).set_Anchor((AnchorStyles)1);
			((Control)cmdCompute).set_Location(new Point(556, 32));
			((Control)cmdCompute).set_Name("cmdCompute");
			((Control)cmdCompute).set_Size(new Size(69, 26));
			((Control)cmdCompute).set_TabIndex(4);
			((Control)cmdCompute).set_Text("Compute");
			((ButtonBase)cmdCompute).set_UseVisualStyleBackColor(true);
			((Control)cmdCompute).add_Click((EventHandler)cmdCompute_Click);
			((Control)label1).set_Anchor((AnchorStyles)1);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(77, 37));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(84, 13));
			((Control)label1).set_TabIndex(5);
			((Control)label1).set_Text("Longitude range");
			((Control)label2).set_Anchor((AnchorStyles)1);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(180, 37));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(32, 13));
			((Control)label2).set_TabIndex(6);
			((Control)label2).set_Text("West");
			((Control)label3).set_Anchor((AnchorStyles)1);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(295, 37));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(28, 13));
			((Control)label3).set_TabIndex(7);
			((Control)label3).set_Text("East");
			((Control)label4).set_Anchor((AnchorStyles)1);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(409, 37));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(42, 13));
			((Control)label4).set_TabIndex(8);
			((Control)label4).set_Text("Interval");
			((Control)updnInterval).set_Anchor((AnchorStyles)1);
			((Control)updnInterval).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "SolarEclipsePathStepSize", true, (DataSourceUpdateMode)1));
			updnInterval.set_DecimalPlaces(1);
			updnInterval.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnInterval).set_Location(new Point(451, 33));
			updnInterval.set_Maximum(new decimal(new int[4] { 5, 0, 0, 0 }));
			updnInterval.set_Minimum(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnInterval).set_Name("updnInterval");
			((Control)updnInterval).set_Size(new Size(48, 20));
			((Control)updnInterval).set_TabIndex(9);
			updnInterval.set_Value(Settings.Default.SolarEclipsePathStepSize);
			((Control)updnInterval).add_Enter((EventHandler)updnInterval_Enter);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(703, 666));
			((Control)this).get_Controls().Add((Control)(object)updnInterval);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)cmdCompute);
			((Control)this).get_Controls().Add((Control)(object)updnEast);
			((Control)this).get_Controls().Add((Control)(object)updnWest);
			((Control)this).get_Controls().Add((Control)(object)lstCoordinates);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationEclipseSolarPath", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationEclipseSolarPath);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("SolarEclipsePathCoordinates");
			((Control)this).set_Text("Solar Eclipse     Path coordinates");
			((Form)this).add_Load((EventHandler)SolarEclipsePathCoordinates_Load);
			((Control)this).add_Resize((EventHandler)SolarEclipsePathCoordinates_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((ISupportInitialize)updnWest).EndInit();
			((ISupportInitialize)updnEast).EndInit();
			((ISupportInitialize)updnInterval).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
