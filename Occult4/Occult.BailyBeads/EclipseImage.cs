using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.BailyBeads
{
	public class EclipseImage : Form
	{
		private IContainer components;

		private MenuStrip menuStrip1;

		internal PictureBox picEclipse;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem stayOnTopToolStripMenuItem;

		public EclipseImage()
		{
			InitializeComponent();
		}

		private void EclipseImage_Resize(object sender, EventArgs e)
		{
			((Control)picEclipse).set_Width(((Control)this).get_Width() - 18);
			((Control)picEclipse).set_Height(((Control)this).get_Height() - 64);
		}

		private void EclipseImage_ResizeEnd(object sender, EventArgs e)
		{
			BailyBeads.DrawEclipseImage();
		}

		private void picEclipse_MouseMove(object sender, MouseEventArgs e)
		{
			BailyBeads.CursorLocationOnEclipseImage(e.get_X(), e.get_Y());
		}

		private void picEclipse_MouseDown(object sender, MouseEventArgs e)
		{
			BailyBeads.PlotLimbAtCursorLocation(e.get_X(), e.get_Y());
		}

		private void EclipseImage_Load(object sender, EventArgs e)
		{
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			((Control)this).set_Top(325);
			((Control)this).set_Left(500);
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
		}

		private void stayOnTopToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).set_TopMost(!((Form)this).get_TopMost());
			if (((Form)this).get_TopMost())
			{
				((ToolStripItem)stayOnTopToolStripMenuItem).set_Text("return to &Normal");
			}
			else
			{
				((ToolStripItem)stayOnTopToolStripMenuItem).set_Text("stay on &Top");
			}
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_SolarEclipses = Output.SaveGraphic(picEclipse.get_Image(), "Eclipse image", Settings.Default.Save_SolarEclipses);
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
			//IL_0127: Unknown result type (might be due to invalid IL or missing references)
			//IL_0131: Expected O, but got Unknown
			//IL_013e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0148: Expected O, but got Unknown
			//IL_0278: Unknown result type (might be due to invalid IL or missing references)
			//IL_0282: Expected O, but got Unknown
			menuStrip1 = new MenuStrip();
			picEclipse = new PictureBox();
			saveToolStripMenuItem = new ToolStripMenuItem();
			stayOnTopToolStripMenuItem = new ToolStripMenuItem();
			((Control)menuStrip1).SuspendLayout();
			((ISupportInitialize)picEclipse).BeginInit();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)saveToolStripMenuItem,
				(ToolStripItem)stayOnTopToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(379, 24));
			((Control)menuStrip1).set_TabIndex(1);
			((Control)menuStrip1).set_Text("menuStrip1");
			((Control)picEclipse).set_Location(new Point(1, 27));
			((Control)picEclipse).set_Name("picEclipse");
			((Control)picEclipse).set_Size(new Size(377, 291));
			picEclipse.set_TabIndex(0);
			picEclipse.set_TabStop(false);
			((Control)picEclipse).add_MouseMove(new MouseEventHandler(picEclipse_MouseMove));
			((Control)picEclipse).add_MouseDown(new MouseEventHandler(picEclipse_MouseDown));
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(113, 20));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save image      ");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)stayOnTopToolStripMenuItem).set_Image((Image)Resources.FillUpHS);
			((ToolStripItem)stayOnTopToolStripMenuItem).set_Name("stayOnTopToolStripMenuItem");
			((ToolStripItem)stayOnTopToolStripMenuItem).set_Size(new Size(97, 20));
			((ToolStripItem)stayOnTopToolStripMenuItem).set_Text("stay on &Top");
			((ToolStripItem)stayOnTopToolStripMenuItem).add_Click((EventHandler)stayOnTopToolStripMenuItem_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(379, 319));
			((Control)this).get_Controls().Add((Control)(object)picEclipse);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationBailyEclipseImage", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationBailyEclipseImage);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MinimumSize(new Size(300, 300));
			((Control)this).set_Name("EclipseImage");
			((Control)this).set_Text("Eclipse Image");
			((Form)this).add_Load((EventHandler)EclipseImage_Load);
			((Control)this).add_Resize((EventHandler)EclipseImage_Resize);
			((Form)this).add_ResizeEnd((EventHandler)EclipseImage_ResizeEnd);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((ISupportInitialize)picEclipse).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
