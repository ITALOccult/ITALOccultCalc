using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class LunarOccultationWorldView : Form
	{
		private bool FormCreated;

		private bool WorldBWFlag;

		private IContainer components;

		private MenuStrip menuStrip1;

		internal PictureBox picWorld;

		private ToolStripMenuItem withChartToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem bWToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripMenuItem helpToolStripMenuItem;

		public LunarOccultationWorldView()
		{
			InitializeComponent();
			bWToolStripMenuItem.set_Checked(WorldBWFlag);
		}

		private void LunarOccultationWorldView_Resize(object sender, EventArgs e)
		{
			if (!((((Control)this).get_Width() < 200) | (((Control)this).get_Height() < 200)))
			{
				((Control)picWorld).set_Width(((Control)this).get_Width() - 21);
				((Control)picWorld).set_Height(((Control)this).get_Height() - 68);
				if (FormCreated)
				{
					LunarOccultations.PlotOccultationWorld(WorldBWFlag);
				}
			}
		}

		internal void DrawMap()
		{
			if (FormCreated)
			{
				LunarOccultations.PlotOccultationWorld(BWFlag: false);
			}
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picWorld.get_Image());
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarOccultations.PrintPreviewOccultationWorldGraphic();
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarOccultations.PrintWorldGraphic();
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_LunarPredictions = Output.SaveGraphic(picWorld.get_Image(), "World map", Settings.Default.Save_LunarPredictions);
		}

		private void LunarOccultationWorldView_Load(object sender, EventArgs e)
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
			FormCreated = true;
		}

		private void bWToolStripMenuItem_Click(object sender, EventArgs e)
		{
			WorldBWFlag = !WorldBWFlag;
			bWToolStripMenuItem.set_Checked(WorldBWFlag);
			if (FormCreated)
			{
				LunarOccultations.PlotOccultationWorld(WorldBWFlag);
			}
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Lunar Occultations - World map");
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
			//IL_051d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0527: Expected O, but got Unknown
			menuStrip1 = new MenuStrip();
			withChartToolStripMenuItem = new ToolStripMenuItem();
			bWToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			picWorld = new PictureBox();
			((Control)menuStrip1).SuspendLayout();
			((ISupportInitialize)picWorld).BeginInit();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)withChartToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(821, 24));
			((Control)menuStrip1).set_TabIndex(1);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withChartToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)bWToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withChartToolStripMenuItem).set_Name("withChartToolStripMenuItem");
			((ToolStripItem)withChartToolStripMenuItem).set_Size(new Size(86, 20));
			((ToolStripItem)withChartToolStripMenuItem).set_Text("with Chart....");
			((ToolStripItem)bWToolStripMenuItem).set_Image((Image)Resources.ColorHS);
			((ToolStripItem)bWToolStripMenuItem).set_Name("bWToolStripMenuItem");
			((ToolStripItem)bWToolStripMenuItem).set_Size(new Size(152, 22));
			((ToolStripItem)bWToolStripMenuItem).set_Text("B&&W");
			((ToolStripItem)bWToolStripMenuItem).add_Click((EventHandler)bWToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(149, 6));
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(152, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(152, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("&Print preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(152, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(152, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(69, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help   ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((Control)picWorld).set_BackColor(Color.Black);
			((Control)picWorld).set_Location(new Point(6, 27));
			((Control)picWorld).set_Name("picWorld");
			((Control)picWorld).set_Size(new Size(808, 484));
			picWorld.set_TabIndex(0);
			picWorld.set_TabStop(false);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(821, 518));
			((Control)this).get_Controls().Add((Control)(object)picWorld);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationLunarWorldMap", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationLunarWorldMap);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("LunarOccultationWorldView");
			((Control)this).set_Text("Lunar occultation  -  world view");
			((Form)this).add_Load((EventHandler)LunarOccultationWorldView_Load);
			((Control)this).add_Resize((EventHandler)LunarOccultationWorldView_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((ISupportInitialize)picWorld).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
