using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Eclipses
{
	public class ClosePlanetaryConjunctions : Form
	{
		internal List<double> EclipseJD_TT_Planets = new List<double>();

		private bool IsLunar;

		private IContainer components;

		private Label lblHeader;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withListToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private NumericUpDown updnStartYear;

		private NumericUpDown updnEndYear;

		private Label label2;

		private Label label3;

		private Button cmdFind;

		internal ListBox lstEclipses;

		internal ProgressBar pBar;

		private Button cmdCancel;

		private Label label4;

		private Label lblCode;

		public ClosePlanetaryConjunctions(bool LunarEclipses)
		{
			InitializeComponent();
			IsLunar = LunarEclipses;
			((Control)cmdCancel).set_Location(((Control)cmdFind).get_Location());
		}

		private void cmdFind_Click(object sender, EventArgs e)
		{
			((Control)cmdCancel).set_Visible(true);
			Application.DoEvents();
			if (IsLunar)
			{
				SolarEclipses.LunarEclipse.FindPlanetConjunctions((int)updnStartYear.get_Value(), (int)updnEndYear.get_Value());
			}
			else
			{
				SolarEclipses.WorldMap.FindPlanetConjunctions((int)updnStartYear.get_Value(), (int)updnEndYear.get_Value());
			}
			((Control)cmdCancel).set_Visible(false);
		}

		private string CollectEvents()
		{
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstEclipses.get_Items().get_Count();
			for (int i = 0; i < count; i++)
			{
				stringBuilder.AppendLine(lstEclipses.get_Items().get_Item(i).ToString());
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
			if (IsLunar)
			{
				Output.SaveAppendPredictionText(CollectEvents(), "Planet conjunctions at Lunar eclipses " + updnStartYear.get_Value() + " to " + updnEndYear.get_Value(), Utilities.AppPath + "\\Predictions\\");
			}
			else
			{
				Output.SaveAppendPredictionText(CollectEvents(), "Planet conjunctions at Solar eclipses " + updnStartYear.get_Value() + " to " + updnEndYear.get_Value(), Utilities.AppPath + "\\Predictions\\");
			}
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			if (IsLunar)
			{
				SolarEclipses.LunarEclipse.CancelPlanetSearch = true;
			}
			else
			{
				SolarEclipseWorldView.CancelPlanetSearch = true;
			}
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Solar Eclipse conjunctions");
		}

		private void lstEclipses_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (((ListControl)lstEclipses).get_SelectedIndex() >= 0)
			{
				if (IsLunar)
				{
					SolarEclipses.LunarEclipse.GeneratePrediction(EclipseJD_TT_Planets[((ListControl)lstEclipses).get_SelectedIndex()]);
				}
				else
				{
					SolarEclipses.SolarEclipseBessellianElements(EclipseJD_TT_Planets[((ListControl)lstEclipses).get_SelectedIndex()]);
				}
			}
		}

		private void ClosePlanetaryConjunctions_Resize(object sender, EventArgs e)
		{
			((Control)lstEclipses).set_Width(((Control)this).get_Width() - 30);
			((Control)lstEclipses).set_Height(((Control)this).get_Height() - 156);
		}

		private void ClosePlanetaryConjunctions_Load(object sender, EventArgs e)
		{
			if (IsLunar)
			{
				((Control)lblCode).set_Text("☺ = umbral occultn possible   ☼ = in umbra");
				((Control)this).set_Text("Close planetary conjunctions at Lunar eclipses");
				((Control)lblHeader).set_Text("This form is used to identify eclipses where a planet is close to, or occulted\r\nby, the Moon during a lunar eclipse.   Year range: -13000 to +17000");
			}
			else
			{
				((Control)this).set_Text("Close planetary conjunctions at Solar eclipses");
				((Control)lblCode).set_Text("☼ = occulted by Sun      ☺ = in transit");
				((Control)lblHeader).set_Text("This form is used to identify eclipses where a planet is close to, or occulted\r\nby, the Sun during a solar eclipse.   Year range: -13000 to +17000");
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
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(ClosePlanetaryConjunctions));
			lblHeader = new Label();
			menuStrip1 = new MenuStrip();
			withListToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			lstEclipses = new ListBox();
			updnStartYear = new NumericUpDown();
			updnEndYear = new NumericUpDown();
			label2 = new Label();
			label3 = new Label();
			cmdFind = new Button();
			pBar = new ProgressBar();
			cmdCancel = new Button();
			label4 = new Label();
			lblCode = new Label();
			((Control)menuStrip1).SuspendLayout();
			((ISupportInitialize)updnStartYear).BeginInit();
			((ISupportInitialize)updnEndYear).BeginInit();
			((Control)this).SuspendLayout();
			((Control)lblHeader).set_AutoSize(true);
			((Control)lblHeader).set_Location(new Point(10, 30));
			((Control)lblHeader).set_Name("lblHeader");
			((Control)lblHeader).set_Size(new Size(357, 26));
			((Control)lblHeader).set_TabIndex(0);
			((Control)lblHeader).set_Text("This form is used to identify eclipses where a planet is close to, or\r\nocculted by, the Sun at the time of the eclipse.   Range: -13000 to +17000\r\n");
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withListToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(384, 24));
			((Control)menuStrip1).set_TabIndex(1);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withListToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withListToolStripMenuItem).set_Name("withListToolStripMenuItem");
			((ToolStripItem)withListToolStripMenuItem).set_Size(new Size(84, 20));
			((ToolStripItem)withListToolStripMenuItem).set_Text("with List...    ");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(102, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(102, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(102, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(69, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help   ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(62, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit   ");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)lstEclipses).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstEclipses).set_FormattingEnabled(true);
			lstEclipses.set_ItemHeight(14);
			((Control)lstEclipses).set_Location(new Point(7, 114));
			((Control)lstEclipses).set_Name("lstEclipses");
			((Control)lstEclipses).set_Size(new Size(370, 144));
			((Control)lstEclipses).set_TabIndex(2);
			lstEclipses.add_SelectedIndexChanged((EventHandler)lstEclipses_SelectedIndexChanged);
			updnStartYear.set_Increment(new decimal(new int[4] { 100, 0, 0, 0 }));
			((Control)updnStartYear).set_Location(new Point(62, 64));
			updnStartYear.set_Maximum(new decimal(new int[4] { 17000, 0, 0, 0 }));
			updnStartYear.set_Minimum(new decimal(new int[4] { 13000, 0, 0, -2147483648 }));
			((Control)updnStartYear).set_Name("updnStartYear");
			((Control)updnStartYear).set_Size(new Size(55, 20));
			((Control)updnStartYear).set_TabIndex(3);
			updnStartYear.set_Value(new decimal(new int[4] { 1000, 0, 0, 0 }));
			updnEndYear.set_Increment(new decimal(new int[4] { 100, 0, 0, 0 }));
			((Control)updnEndYear).set_Location(new Point(188, 64));
			updnEndYear.set_Maximum(new decimal(new int[4] { 17000, 0, 0, 0 }));
			updnEndYear.set_Minimum(new decimal(new int[4] { 13000, 0, 0, -2147483648 }));
			((Control)updnEndYear).set_Name("updnEndYear");
			((Control)updnEndYear).set_Size(new Size(55, 20));
			((Control)updnEndYear).set_TabIndex(4);
			updnEndYear.set_Value(new decimal(new int[4] { 3000, 0, 0, 0 }));
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(13, 68));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(49, 13));
			((Control)label2).set_TabIndex(5);
			((Control)label2).set_Text("First year");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(138, 68));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(50, 13));
			((Control)label3).set_TabIndex(6);
			((Control)label3).set_Text("Last year");
			((Control)cmdFind).set_Location(new Point(284, 64));
			((Control)cmdFind).set_Name("cmdFind");
			((Control)cmdFind).set_Size(new Size(83, 21));
			((Control)cmdFind).set_TabIndex(7);
			((Control)cmdFind).set_Text("Find eclipses");
			((ButtonBase)cmdFind).set_UseVisualStyleBackColor(true);
			((Control)cmdFind).add_Click((EventHandler)cmdFind_Click);
			((Control)pBar).set_Location(new Point(7, 88));
			((Control)pBar).set_Name("pBar");
			((Control)pBar).set_Size(new Size(370, 7));
			((Control)pBar).set_TabIndex(8);
			((Control)pBar).set_Visible(false);
			((Control)cmdCancel).set_Location(new Point(284, 93));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(83, 21));
			((Control)cmdCancel).set_TabIndex(9);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).set_Visible(false);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(5, 100));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(146, 13));
			((Control)label4).set_TabIndex(10);
			((Control)label4).set_Text("Select event to plot prediction");
			((Control)lblCode).set_AutoSize(true);
			((Control)lblCode).set_Font(new Font("Arial", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblCode).set_Location(new Point(163, 100));
			((Control)lblCode).set_Name("lblCode");
			((Control)lblCode).set_Size(new Size(140, 14));
			((Control)lblCode).set_TabIndex(11);
			((Control)lblCode).set_Text("☼ = occulted, ☺ = in transit");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(384, 264));
			((Control)this).get_Controls().Add((Control)(object)lblCode);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_Controls().Add((Control)(object)pBar);
			((Control)this).get_Controls().Add((Control)(object)cmdFind);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)updnEndYear);
			((Control)this).get_Controls().Add((Control)(object)updnStartYear);
			((Control)this).get_Controls().Add((Control)(object)lstEclipses);
			((Control)this).get_Controls().Add((Control)(object)lblHeader);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MinimumSize(new Size(400, 300));
			((Control)this).set_Name("ClosePlanetaryConjunctions");
			((Control)this).set_Text("Close planetary conjunctions at eclipses");
			((Form)this).add_Load((EventHandler)ClosePlanetaryConjunctions_Load);
			((Control)this).add_Resize((EventHandler)ClosePlanetaryConjunctions_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((ISupportInitialize)updnStartYear).EndInit();
			((ISupportInitialize)updnEndYear).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
