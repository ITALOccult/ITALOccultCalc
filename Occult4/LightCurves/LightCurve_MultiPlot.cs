using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace LightCurves
{
	public class LightCurve_MultiPlot : Form
	{
		private IContainer components;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withChartsToolStripMenuItem;

		internal PictureBox picPlotAll;

		internal Panel panelAll;

		private ToolStripMenuItem viewNext600ToolStripMenuItem;

		private ToolStripMenuItem openLightCurveViewerToolStripMenuItem;

		public LightCurve_MultiPlot()
		{
			InitializeComponent();
		}

		private void LightCurve_MultiPlot_Resize(object sender, EventArgs e)
		{
			((Control)panelAll).set_Height(((Control)this).get_Height() - 75);
		}

		private void viewNext600ToolStripMenuItem_Click(object sender, EventArgs e)
		{
		}

		private void openLightCurveViewerToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LightData.ShowLightCurveViewer();
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
			//IL_0344: Unknown result type (might be due to invalid IL or missing references)
			//IL_034e: Expected O, but got Unknown
			menuStrip1 = new MenuStrip();
			withChartsToolStripMenuItem = new ToolStripMenuItem();
			viewNext600ToolStripMenuItem = new ToolStripMenuItem();
			panelAll = new Panel();
			picPlotAll = new PictureBox();
			openLightCurveViewerToolStripMenuItem = new ToolStripMenuItem();
			((Control)menuStrip1).SuspendLayout();
			((Control)panelAll).SuspendLayout();
			((ISupportInitialize)picPlotAll).BeginInit();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)withChartsToolStripMenuItem,
				(ToolStripItem)openLightCurveViewerToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(809, 24));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withChartsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[1] { (ToolStripItem)viewNext600ToolStripMenuItem });
			((ToolStripItem)withChartsToolStripMenuItem).set_Name("withChartsToolStripMenuItem");
			((ToolStripItem)withChartsToolStripMenuItem).set_Size(new Size(118, 20));
			((ToolStripItem)withChartsToolStripMenuItem).set_Text("with Charts...          ");
			((ToolStripItem)viewNext600ToolStripMenuItem).set_Name("viewNext600ToolStripMenuItem");
			((ToolStripItem)viewNext600ToolStripMenuItem).set_Size(new Size(145, 22));
			((ToolStripItem)viewNext600ToolStripMenuItem).set_Text("view next 600");
			((ToolStripItem)viewNext600ToolStripMenuItem).add_Click((EventHandler)viewNext600ToolStripMenuItem_Click);
			((ScrollableControl)panelAll).set_AutoScroll(true);
			((ScrollableControl)panelAll).set_AutoScrollMinSize(new Size(250, 220));
			((Control)panelAll).get_Controls().Add((Control)(object)picPlotAll);
			((Control)panelAll).set_Location(new Point(5, 27));
			((Control)panelAll).set_Name("panelAll");
			((Control)panelAll).set_Size(new Size(794, 220));
			((Control)panelAll).set_TabIndex(2);
			((Control)picPlotAll).set_BackColor(Color.White);
			picPlotAll.set_BorderStyle((BorderStyle)2);
			((Control)picPlotAll).set_Location(new Point(3, 3));
			((Control)picPlotAll).set_Name("picPlotAll");
			((Control)picPlotAll).set_Size(new Size(773, 214));
			picPlotAll.set_TabIndex(0);
			picPlotAll.set_TabStop(false);
			((ToolStripItem)openLightCurveViewerToolStripMenuItem).set_Image((Image)Resources.View_Portrait_16x);
			((ToolStripItem)openLightCurveViewerToolStripMenuItem).set_Name("openLightCurveViewerToolStripMenuItem");
			((ToolStripItem)openLightCurveViewerToolStripMenuItem).set_Size(new Size(163, 20));
			((ToolStripItem)openLightCurveViewerToolStripMenuItem).set_Text("Open Light curve viewer");
			((ToolStripItem)openLightCurveViewerToolStripMenuItem).add_Click((EventHandler)openLightCurveViewerToolStripMenuItem_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(809, 256));
			((Control)this).get_Controls().Add((Control)(object)panelAll);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationMultiLightCurves", true, (DataSourceUpdateMode)1));
			((Form)this).set_Location(Settings.Default.LocationMultiLightCurves);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MaximumSize(new Size(825, 2000));
			((Control)this).set_MinimumSize(new Size(825, 295));
			((Control)this).set_Name("LightCurve_MultiPlot");
			((Control)this).set_Text("Light curves from Occult light curves file");
			((Control)this).add_Resize((EventHandler)LightCurve_MultiPlot_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)panelAll).ResumeLayout(false);
			((ISupportInitialize)picPlotAll).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
