using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Mapping
{
	public class LunarLimbMap : Form
	{
		private IContainer components;

		private MenuStrip menuStrip1;

		internal PictureBox picMoonMap;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripMenuItem withMapToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem resToolStripMenuItem;

		private ToolStripMenuItem toolStripMenuItem2;

		private ToolStripMenuItem toolStripMenuItem3;

		private ToolStripMenuItem toolStripMenuItem4;

		private ToolStripMenuItem htToolStripMenuItem;

		private ToolStripMenuItem toolStripMenuItem5;

		private ToolStripMenuItem toolStripMenuItem6;

		private ToolStripMenuItem toolStripMenuItem7;

		private TrackBar tpScale;

		internal Panel pnlMap;

		private ToolTip toolTip;

		private ToolStripMenuItem showLimbPointsToolStripMenuItem;

		private ToolStripMenuItem showHeightLinesForLimbPointsToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private Label label1;

		public LunarLimbMap()
		{
			InitializeComponent();
		}

		private void LunarLimbMap_Load(object sender, EventArgs e)
		{
			SetFormSize();
		}

		private void LunarLimbMap_Resize(object sender, EventArgs e)
		{
			SetFormSize();
		}

		internal void SetFormSize()
		{
			SetMapSizeScale();
		}

		private void SetMapSizeScale()
		{
			if (((Control)this).get_Width() > 100)
			{
				((Control)pnlMap).set_Width(((Control)this).get_Width() - 21);
			}
			if (((Control)this).get_Height() > 120)
			{
				((Control)pnlMap).set_Height(((Control)this).get_Height() - 90);
			}
			((Control)picMoonMap).set_Width((int)((float)((Control)pnlMap).get_Width() * LOLAHiRes.MapScale - 29f));
			((Control)picMoonMap).set_Height(2 * ((Control)picMoonMap).get_Width());
			LOLAHiRes.DrawLimbMap(LOLAHiRes.LastShowLimbPoint, showLimbPointsToolStripMenuItem.get_Checked(), showHeightLinesForLimbPointsToolStripMenuItem.get_Checked());
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void toolStripMenuItem2_Click(object sender, EventArgs e)
		{
			SetRes(1);
		}

		private void toolStripMenuItem3_Click(object sender, EventArgs e)
		{
			SetRes(2);
		}

		private void toolStripMenuItem4_Click(object sender, EventArgs e)
		{
			SetRes(3);
		}

		private void SetRes(int item)
		{
			toolStripMenuItem2.set_Checked(item == 1);
			toolStripMenuItem3.set_Checked(item == 2);
			toolStripMenuItem4.set_Checked(item == 3);
			LOLAHiRes.Dresolution = item;
			SetMapSizeScale();
		}

		private void toolStripMenuItem5_Click(object sender, EventArgs e)
		{
			SetHeight(1);
		}

		private void toolStripMenuItem6_Click(object sender, EventArgs e)
		{
			SetHeight(2);
		}

		private void toolStripMenuItem7_Click(object sender, EventArgs e)
		{
			SetHeight(3);
		}

		private void SetHeight(int item)
		{
			toolStripMenuItem5.set_Checked(item == 1);
			toolStripMenuItem6.set_Checked(item == 2);
			toolStripMenuItem7.set_Checked(item == 3);
			LOLAHiRes.HeightResolution = (float)item * 0.005f;
			SetMapSizeScale();
		}

		private void tpScale_Scroll(object sender, EventArgs e)
		{
			LOLAHiRes.MapScale = 1f + (float)(tpScale.get_Value() - 40) / 40f;
			SetMapSizeScale();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picMoonMap.get_Image());
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintPicBoxImage(picMoonMap.get_Image());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.SaveGraphic(picMoonMap.get_Image(), "Lunar Limb", Utilities.AppPath + "/observations");
		}

		private void picMoonMap_MouseMove(object sender, MouseEventArgs e)
		{
			float x = e.get_X();
			float y = e.get_Y();
			toolTip.SetToolTip((Control)(object)picMoonMap, LOLAHiRes.ToolTipText(x, y));
		}

		private void showLimbPointsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			showLimbPointsToolStripMenuItem.set_Checked(!showLimbPointsToolStripMenuItem.get_Checked());
			LOLAHiRes.DrawLimbMap(LOLAHiRes.LastShowLimbPoint, showLimbPointsToolStripMenuItem.get_Checked(), showHeightLinesForLimbPointsToolStripMenuItem.get_Checked());
		}

		private void showHeightLinesForLimbPointsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			showHeightLinesForLimbPointsToolStripMenuItem.set_Checked(!showHeightLinesForLimbPointsToolStripMenuItem.get_Checked());
			LOLAHiRes.DrawLimbMap(LOLAHiRes.LastShowLimbPoint, showLimbPointsToolStripMenuItem.get_Checked(), showHeightLinesForLimbPointsToolStripMenuItem.get_Checked());
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Lunar limb map");
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
			//IL_00e3: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ed: Expected O, but got Unknown
			//IL_00ee: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f8: Expected O, but got Unknown
			//IL_00f9: Unknown result type (might be due to invalid IL or missing references)
			//IL_0103: Expected O, but got Unknown
			//IL_01a6: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b0: Expected O, but got Unknown
			components = new Container();
			picMoonMap = new PictureBox();
			menuStrip1 = new MenuStrip();
			withMapToolStripMenuItem = new ToolStripMenuItem();
			showLimbPointsToolStripMenuItem = new ToolStripMenuItem();
			showHeightLinesForLimbPointsToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			resToolStripMenuItem = new ToolStripMenuItem();
			toolStripMenuItem2 = new ToolStripMenuItem();
			toolStripMenuItem3 = new ToolStripMenuItem();
			toolStripMenuItem4 = new ToolStripMenuItem();
			htToolStripMenuItem = new ToolStripMenuItem();
			toolStripMenuItem5 = new ToolStripMenuItem();
			toolStripMenuItem6 = new ToolStripMenuItem();
			toolStripMenuItem7 = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			pnlMap = new Panel();
			tpScale = new TrackBar();
			toolTip = new ToolTip(components);
			helpToolStripMenuItem = new ToolStripMenuItem();
			label1 = new Label();
			((ISupportInitialize)picMoonMap).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((Control)pnlMap).SuspendLayout();
			((ISupportInitialize)tpScale).BeginInit();
			((Control)this).SuspendLayout();
			((Control)picMoonMap).set_BackColor(Color.White);
			((Control)picMoonMap).set_Location(new Point(3, 3));
			((Control)picMoonMap).set_Name("picMoonMap");
			((Control)picMoonMap).set_Size(new Size(340, 575));
			picMoonMap.set_TabIndex(0);
			picMoonMap.set_TabStop(false);
			((Control)picMoonMap).add_MouseMove(new MouseEventHandler(picMoonMap_MouseMove));
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)withMapToolStripMenuItem,
				(ToolStripItem)resToolStripMenuItem,
				(ToolStripItem)htToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(384, 24));
			((Control)menuStrip1).set_TabIndex(1);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withMapToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)showLimbPointsToolStripMenuItem,
				(ToolStripItem)showHeightLinesForLimbPointsToolStripMenuItem,
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withMapToolStripMenuItem).set_Name("withMapToolStripMenuItem");
			((ToolStripItem)withMapToolStripMenuItem).set_Size(new Size(78, 20));
			((ToolStripItem)withMapToolStripMenuItem).set_Text("with Map...");
			showLimbPointsToolStripMenuItem.set_Checked(true);
			showLimbPointsToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)showLimbPointsToolStripMenuItem).set_Name("showLimbPointsToolStripMenuItem");
			((ToolStripItem)showLimbPointsToolStripMenuItem).set_Size(new Size(248, 22));
			((ToolStripItem)showLimbPointsToolStripMenuItem).set_Text("Show limb points");
			((ToolStripItem)showLimbPointsToolStripMenuItem).add_Click((EventHandler)showLimbPointsToolStripMenuItem_Click);
			((ToolStripItem)showHeightLinesForLimbPointsToolStripMenuItem).set_Name("showHeightLinesForLimbPointsToolStripMenuItem");
			((ToolStripItem)showHeightLinesForLimbPointsToolStripMenuItem).set_Size(new Size(248, 22));
			((ToolStripItem)showHeightLinesForLimbPointsToolStripMenuItem).set_Text("Show height lines for limb points");
			((ToolStripItem)showHeightLinesForLimbPointsToolStripMenuItem).add_Click((EventHandler)showHeightLinesForLimbPointsToolStripMenuItem_Click);
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(248, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(248, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(248, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripDropDownItem)resToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)toolStripMenuItem2,
				(ToolStripItem)toolStripMenuItem3,
				(ToolStripItem)toolStripMenuItem4
			});
			((ToolStripItem)resToolStripMenuItem).set_Name("resToolStripMenuItem");
			((ToolStripItem)resToolStripMenuItem).set_Size(new Size(78, 20));
			((ToolStripItem)resToolStripMenuItem).set_Text("Resolution ");
			((ToolStripItem)toolStripMenuItem2).set_Name("toolStripMenuItem2");
			((ToolStripItem)toolStripMenuItem2).set_Size(new Size(152, 22));
			((ToolStripItem)toolStripMenuItem2).set_Text("1");
			((ToolStripItem)toolStripMenuItem2).add_Click((EventHandler)toolStripMenuItem2_Click);
			toolStripMenuItem3.set_Checked(true);
			toolStripMenuItem3.set_CheckState((CheckState)1);
			((ToolStripItem)toolStripMenuItem3).set_Name("toolStripMenuItem3");
			((ToolStripItem)toolStripMenuItem3).set_Size(new Size(152, 22));
			((ToolStripItem)toolStripMenuItem3).set_Text("2");
			((ToolStripItem)toolStripMenuItem3).add_Click((EventHandler)toolStripMenuItem3_Click);
			((ToolStripItem)toolStripMenuItem4).set_Name("toolStripMenuItem4");
			((ToolStripItem)toolStripMenuItem4).set_Size(new Size(152, 22));
			((ToolStripItem)toolStripMenuItem4).set_Text("3");
			((ToolStripItem)toolStripMenuItem4).add_Click((EventHandler)toolStripMenuItem4_Click);
			((ToolStripDropDownItem)htToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)toolStripMenuItem5,
				(ToolStripItem)toolStripMenuItem6,
				(ToolStripItem)toolStripMenuItem7
			});
			((ToolStripItem)htToolStripMenuItem).set_Name("htToolStripMenuItem");
			((ToolStripItem)htToolStripMenuItem).set_Size(new Size(58, 20));
			((ToolStripItem)htToolStripMenuItem).set_Text("Height ");
			((ToolStripItem)toolStripMenuItem5).set_Name("toolStripMenuItem5");
			((ToolStripItem)toolStripMenuItem5).set_Size(new Size(152, 22));
			((ToolStripItem)toolStripMenuItem5).set_Text("1");
			((ToolStripItem)toolStripMenuItem5).add_Click((EventHandler)toolStripMenuItem5_Click);
			toolStripMenuItem6.set_Checked(true);
			toolStripMenuItem6.set_CheckState((CheckState)1);
			((ToolStripItem)toolStripMenuItem6).set_Name("toolStripMenuItem6");
			((ToolStripItem)toolStripMenuItem6).set_Size(new Size(152, 22));
			((ToolStripItem)toolStripMenuItem6).set_Text("2");
			((ToolStripItem)toolStripMenuItem6).add_Click((EventHandler)toolStripMenuItem6_Click);
			((ToolStripItem)toolStripMenuItem7).set_Name("toolStripMenuItem7");
			((ToolStripItem)toolStripMenuItem7).set_Size(new Size(152, 22));
			((ToolStripItem)toolStripMenuItem7).set_Text("3");
			((ToolStripItem)toolStripMenuItem7).add_Click((EventHandler)toolStripMenuItem7_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(53, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((ScrollableControl)pnlMap).set_AutoScroll(true);
			((ScrollableControl)pnlMap).set_AutoScrollMargin(new Size(5, 5));
			pnlMap.set_BorderStyle((BorderStyle)2);
			((Control)pnlMap).get_Controls().Add((Control)(object)picMoonMap);
			((Control)pnlMap).set_Location(new Point(3, 49));
			((Control)pnlMap).set_Name("pnlMap");
			((Control)pnlMap).set_Size(new Size(369, 600));
			((Control)pnlMap).set_TabIndex(0);
			((Control)tpScale).set_AutoSize(false);
			tpScale.set_LargeChange(1);
			((Control)tpScale).set_Location(new Point(43, 21));
			tpScale.set_Maximum(60);
			tpScale.set_Minimum(40);
			((Control)tpScale).set_Name("tpScale");
			((Control)tpScale).set_Size(new Size(215, 26));
			((Control)tpScale).set_TabIndex(2);
			tpScale.set_TickStyle((TickStyle)1);
			tpScale.set_Value(40);
			tpScale.add_Scroll((EventHandler)tpScale_Scroll);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(63, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(9, 28));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(34, 13));
			((Control)label1).set_TabIndex(3);
			((Control)label1).set_Text("Scale");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(384, 661));
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)pnlMap);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)tpScale);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("LunarLimbMap");
			((Control)this).set_Text("Lunar limb map");
			((Form)this).add_Load((EventHandler)LunarLimbMap_Load);
			((Control)this).add_Resize((EventHandler)LunarLimbMap_Resize);
			((ISupportInitialize)picMoonMap).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)pnlMap).ResumeLayout(false);
			((ISupportInitialize)tpScale).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
