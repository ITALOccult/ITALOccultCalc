using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Asteroid_Observations
{
	public class StarDiameterPlotAll : Form
	{
		internal string SaveFileLabel = "";

		private IContainer components;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withDiameterPlotsToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		internal Panel panelAll;

		internal PictureBox picPlotAll;

		private ToolStripMenuItem copyDiameterSolutionsToolStripMenuItem;

		public StarDiameterPlotAll()
		{
			InitializeComponent();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picPlotAll.get_Image());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_StarDiameters = Output.SaveGraphic(picPlotAll.get_Image(), SaveFileLabel, Settings.Default.Save_StarDiameters);
		}

		private void StarDiameterPlotAll_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			((Control)this).set_Height((int)(0.95 * (double)Screen.GetWorkingArea((Control)(object)this).Height));
			((Control)this).set_Width((int)(0.95 * (double)Screen.GetWorkingArea((Control)(object)this).Width));
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			((Control)panelAll).set_Width(((Control)this).get_Width() - 25);
			((Control)picPlotAll).set_Width(((Control)panelAll).get_Width() - 25);
		}

		private void StarDiameterPlotAll_Resize(object sender, EventArgs e)
		{
			((Control)panelAll).set_Width(((Control)this).get_Width() - 25);
			((Control)picPlotAll).set_Width(((Control)panelAll).get_Width() - 25);
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Star diameter Analyser");
		}

		private void copyDiameterSolutionsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(StarDiameterAnalysis.DiameterSolutions);
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
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(StarDiameterPlotAll));
			menuStrip1 = new MenuStrip();
			withDiameterPlotsToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			panelAll = new Panel();
			picPlotAll = new PictureBox();
			copyDiameterSolutionsToolStripMenuItem = new ToolStripMenuItem();
			((Control)menuStrip1).SuspendLayout();
			((Control)panelAll).SuspendLayout();
			((ISupportInitialize)picPlotAll).BeginInit();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withDiameterPlotsToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(834, 24));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withDiameterPlotsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem,
				(ToolStripItem)copyDiameterSolutionsToolStripMenuItem
			});
			((ToolStripItem)withDiameterPlotsToolStripMenuItem).set_Name("withDiameterPlotsToolStripMenuItem");
			((ToolStripItem)withDiameterPlotsToolStripMenuItem).set_Size(new Size(143, 20));
			((ToolStripItem)withDiameterPlotsToolStripMenuItem).set_Text("with Diameter plots...    ");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(203, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(203, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(53, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((ScrollableControl)panelAll).set_AutoScroll(true);
			((ScrollableControl)panelAll).set_AutoScrollMargin(new Size(0, 10));
			((ScrollableControl)panelAll).set_AutoScrollMinSize(new Size(500, 500));
			((Control)panelAll).get_Controls().Add((Control)(object)picPlotAll);
			((Control)panelAll).set_Location(new Point(2, 26));
			((Control)panelAll).set_Name("panelAll");
			((Control)panelAll).set_Size(new Size(837, 523));
			((Control)panelAll).set_TabIndex(2);
			((Control)picPlotAll).set_BackColor(Color.White);
			picPlotAll.set_BorderStyle((BorderStyle)2);
			((Control)picPlotAll).set_Location(new Point(3, 3));
			((Control)picPlotAll).set_Name("picPlotAll");
			((Control)picPlotAll).set_Size(new Size(792, 499));
			picPlotAll.set_TabIndex(0);
			picPlotAll.set_TabStop(false);
			((ToolStripItem)copyDiameterSolutionsToolStripMenuItem).set_Name("copyDiameterSolutionsToolStripMenuItem");
			((ToolStripItem)copyDiameterSolutionsToolStripMenuItem).set_Size(new Size(203, 22));
			((ToolStripItem)copyDiameterSolutionsToolStripMenuItem).set_Text("Copy diameter solutions");
			((ToolStripItem)copyDiameterSolutionsToolStripMenuItem).add_Click((EventHandler)copyDiameterSolutionsToolStripMenuItem_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(834, 541));
			((Control)this).get_Controls().Add((Control)(object)panelAll);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("StarDiameterPlotAll");
			((Control)this).set_Text("Star diameter - plot all");
			((Form)this).add_Load((EventHandler)StarDiameterPlotAll_Load);
			((Control)this).add_Resize((EventHandler)StarDiameterPlotAll_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)panelAll).ResumeLayout(false);
			((ISupportInitialize)picPlotAll).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
