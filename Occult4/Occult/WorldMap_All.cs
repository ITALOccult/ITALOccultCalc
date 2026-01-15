using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class WorldMap_All : Form
	{
		internal string SaveFileLabel;

		private IContainer components;

		internal PictureBox picPlotAll;

		internal Panel panelAll;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withChartsToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		public WorldMap_All()
		{
			InitializeComponent();
		}

		private void WorldMap_All_Load(object sender, EventArgs e)
		{
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
			((Control)panelAll).set_Width(((Control)this).get_Width() - 10);
			((Control)picPlotAll).set_Width(((Control)panelAll).get_Width() - 25);
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
		}

		private void WorldMap_All_FormClosed(object sender, FormClosedEventArgs e)
		{
			((Component)this).Dispose();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picPlotAll.get_Image());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_LunarPredictions = Output.SaveGraphic(picPlotAll.get_Image(), SaveFileLabel, Settings.Default.Save_LunarPredictions);
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Lunar Occultations - World map All");
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
			//IL_0465: Unknown result type (might be due to invalid IL or missing references)
			//IL_046f: Expected O, but got Unknown
			//IL_04dd: Unknown result type (might be due to invalid IL or missing references)
			//IL_04e7: Expected O, but got Unknown
			picPlotAll = new PictureBox();
			panelAll = new Panel();
			menuStrip1 = new MenuStrip();
			withChartsToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			((ISupportInitialize)picPlotAll).BeginInit();
			((Control)panelAll).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)picPlotAll).set_BackColor(Color.White);
			picPlotAll.set_BorderStyle((BorderStyle)2);
			((Control)picPlotAll).set_Location(new Point(3, 3));
			((Control)picPlotAll).set_Name("picPlotAll");
			((Control)picPlotAll).set_Size(new Size(792, 499));
			picPlotAll.set_TabIndex(0);
			picPlotAll.set_TabStop(false);
			((ScrollableControl)panelAll).set_AutoScroll(true);
			((ScrollableControl)panelAll).set_AutoScrollMargin(new Size(0, 10));
			((ScrollableControl)panelAll).set_AutoScrollMinSize(new Size(500, 500));
			((Control)panelAll).get_Controls().Add((Control)(object)picPlotAll);
			((Control)panelAll).set_Location(new Point(2, 26));
			((Control)panelAll).set_Name("panelAll");
			((Control)panelAll).set_Size(new Size(837, 523));
			((Control)panelAll).set_TabIndex(1);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withChartsToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(840, 24));
			((Control)menuStrip1).set_TabIndex(2);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withChartsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withChartsToolStripMenuItem).set_Name("withChartsToolStripMenuItem");
			((ToolStripItem)withChartsToolStripMenuItem).set_Size(new Size(106, 20));
			((ToolStripItem)withChartsToolStripMenuItem).set_Text("with Charts...      ");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
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
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(840, 547));
			((Control)this).get_Controls().Add((Control)(object)panelAll);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationLunarWorldMapAll", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Location(Settings.Default.LocationLunarWorldMapAll);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("WorldMap_All");
			((Form)this).set_StartPosition((FormStartPosition)1);
			((Control)this).set_Text("World Map -All");
			((Form)this).add_FormClosed(new FormClosedEventHandler(WorldMap_All_FormClosed));
			((Form)this).add_Load((EventHandler)WorldMap_All_Load);
			((ISupportInitialize)picPlotAll).EndInit();
			((Control)panelAll).ResumeLayout(false);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
