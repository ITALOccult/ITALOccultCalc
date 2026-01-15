using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class MutualLightCurve : Form
	{
		private IContainer components;

		internal PictureBox picMutual;

		private TrackBar trackBar1;

		private Label label1;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withPlotToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripMenuItem displayInNTSCFramesToolStripMenuItem;

		private ToolStripMenuItem displayInPALFramesToolStripMenuItem;

		private ToolStripMenuItem displayInMinutesToolStripMenuItem;

		private Panel panel1;

		internal TrackBar trackBarScale;

		private Panel panel2;

		private Label label2;

		internal Label lblHeight;

		private Panel panel3;

		public MutualLightCurve()
		{
			InitializeComponent();
		}

		private void MutualLightCurve_Resize(object sender, EventArgs e)
		{
			if (((Control)this).get_Height() > 160)
			{
				((Control)picMutual).set_Height(((Control)this).get_Height() - 75);
			}
			if (((Control)this).get_Width() > 40)
			{
				((Control)picMutual).set_Width(((Control)this).get_Width() - 21);
			}
			((Control)panel3).set_Width(((Control)picMutual).get_Right() - ((Control)panel3).get_Left());
			Satellites.DrawLightCurve(Satellites.MutualLightCurve.picMutual, ShowLabels: true, (double)trackBarScale.get_Value() / 100.0);
		}

		private void trackBar1_Scroll(object sender, EventArgs e)
		{
			((Form)this).set_Opacity((double)trackBar1.get_Value() / 100.0);
			((Form)this).set_TopMost(((Form)this).get_Opacity() < 0.95);
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (picMutual.get_Image() != null)
			{
				Clipboard.SetImage(picMutual.get_Image());
			}
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (picMutual.get_Image() != null)
			{
				Output.SaveGraphic(picMutual.get_Image(), Satellites.EventName.Replace(":", "-"), Settings.Default.Save_EphemerisData);
			}
		}

		private void displayInMinutesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (!displayInMinutesToolStripMenuItem.get_Checked())
			{
				Satellites.DisplayInMinutes = true;
				Satellites.DisplayInNTSC = false;
				Satellites.DisplayInPAL = false;
				SetChecks();
			}
		}

		private void displayInNTSCFramesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (!displayInNTSCFramesToolStripMenuItem.get_Checked())
			{
				Satellites.DisplayInMinutes = false;
				Satellites.DisplayInNTSC = true;
				Satellites.DisplayInPAL = false;
				SetChecks();
			}
		}

		private void displayInPALFramesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (!displayInPALFramesToolStripMenuItem.get_Checked())
			{
				Satellites.DisplayInMinutes = false;
				Satellites.DisplayInNTSC = false;
				Satellites.DisplayInPAL = true;
				SetChecks();
			}
		}

		private void SetChecks()
		{
			displayInMinutesToolStripMenuItem.set_Checked(Satellites.DisplayInMinutes);
			displayInNTSCFramesToolStripMenuItem.set_Checked(Satellites.DisplayInNTSC);
			displayInPALFramesToolStripMenuItem.set_Checked(Satellites.DisplayInPAL);
			Satellites.DrawLightCurve(picMutual, ShowLabels: true, 1.0);
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Mutual event light curve");
		}

		private void trackBarScale_Scroll(object sender, EventArgs e)
		{
			((Control)lblHeight).set_Text($"= {1.0 - (double)trackBarScale.get_Value() / 100.0:+0.00;-0.00}");
			Satellites.DrawLightCurve(picMutual, ShowLabels: true, (double)trackBarScale.get_Value() / 100.0);
		}

		private void MutualLightCurve_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
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
			//IL_01be: Unknown result type (might be due to invalid IL or missing references)
			//IL_078f: Unknown result type (might be due to invalid IL or missing references)
			picMutual = new PictureBox();
			trackBar1 = new TrackBar();
			label1 = new Label();
			menuStrip1 = new MenuStrip();
			withPlotToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			displayInMinutesToolStripMenuItem = new ToolStripMenuItem();
			displayInNTSCFramesToolStripMenuItem = new ToolStripMenuItem();
			displayInPALFramesToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			panel1 = new Panel();
			trackBarScale = new TrackBar();
			panel2 = new Panel();
			label2 = new Label();
			lblHeight = new Label();
			panel3 = new Panel();
			((ISupportInitialize)picMutual).BeginInit();
			((ISupportInitialize)trackBar1).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((ISupportInitialize)trackBarScale).BeginInit();
			((Control)panel2).SuspendLayout();
			((Control)panel3).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)picMutual).set_Location(new Point(3, 32));
			((Control)picMutual).set_Name("picMutual");
			((Control)picMutual).set_Size(new Size(714, 475));
			picMutual.set_TabIndex(0);
			picMutual.set_TabStop(false);
			((Control)trackBar1).set_AutoSize(false);
			((Control)trackBar1).set_BackColor(Color.SaddleBrown);
			trackBar1.set_LargeChange(10);
			((Control)trackBar1).set_Location(new Point(0, -1));
			((Control)trackBar1).set_Margin(new Padding(3, 3, 3, 6));
			trackBar1.set_Maximum(100);
			trackBar1.set_Minimum(30);
			((Control)trackBar1).set_Name("trackBar1");
			((Control)trackBar1).set_Size(new Size(136, 25));
			trackBar1.set_SmallChange(5);
			((Control)trackBar1).set_TabIndex(1);
			trackBar1.set_TickFrequency(10);
			trackBar1.set_TickStyle((TickStyle)1);
			trackBar1.set_Value(100);
			trackBar1.add_Scroll((EventHandler)trackBar1_Scroll);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_BackColor(SystemColors.Control);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(13, 9));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(84, 13));
			((Control)label1).set_TabIndex(2);
			((Control)label1).set_Text("Transparency");
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withPlotToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(721, 24));
			((Control)menuStrip1).set_TabIndex(3);
			((Control)menuStrip1).set_Text("menuStrip1");
			withPlotToolStripMenuItem.set_Checked(true);
			withPlotToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripDropDownItem)withPlotToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)displayInMinutesToolStripMenuItem,
				(ToolStripItem)displayInNTSCFramesToolStripMenuItem,
				(ToolStripItem)displayInPALFramesToolStripMenuItem
			});
			((ToolStripItem)withPlotToolStripMenuItem).set_Name("withPlotToolStripMenuItem");
			((ToolStripItem)withPlotToolStripMenuItem).set_Size(new Size(78, 20));
			((ToolStripItem)withPlotToolStripMenuItem).set_Text("with Plot....");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(197, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(197, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(194, 6));
			displayInMinutesToolStripMenuItem.set_Checked(true);
			displayInMinutesToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)displayInMinutesToolStripMenuItem).set_Name("displayInMinutesToolStripMenuItem");
			((ToolStripItem)displayInMinutesToolStripMenuItem).set_Size(new Size(197, 22));
			((ToolStripItem)displayInMinutesToolStripMenuItem).set_Text("Display in Minutes");
			((ToolStripItem)displayInMinutesToolStripMenuItem).add_Click((EventHandler)displayInMinutesToolStripMenuItem_Click);
			((ToolStripItem)displayInNTSCFramesToolStripMenuItem).set_Name("displayInNTSCFramesToolStripMenuItem");
			((ToolStripItem)displayInNTSCFramesToolStripMenuItem).set_Size(new Size(197, 22));
			((ToolStripItem)displayInNTSCFramesToolStripMenuItem).set_Text("Display in NTSC frames");
			((ToolStripItem)displayInNTSCFramesToolStripMenuItem).add_Click((EventHandler)displayInNTSCFramesToolStripMenuItem_Click);
			((ToolStripItem)displayInPALFramesToolStripMenuItem).set_Name("displayInPALFramesToolStripMenuItem");
			((ToolStripItem)displayInPALFramesToolStripMenuItem).set_Size(new Size(197, 22));
			((ToolStripItem)displayInPALFramesToolStripMenuItem).set_Text("Display in PAL frames");
			((ToolStripItem)displayInPALFramesToolStripMenuItem).add_Click((EventHandler)displayInPALFramesToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(60, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(53, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)panel1).set_BackColor(SystemColors.Control);
			panel1.set_BorderStyle((BorderStyle)2);
			((Control)panel1).get_Controls().Add((Control)(object)trackBar1);
			((Control)panel1).set_Location(new Point(96, 3));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(136, 26));
			((Control)panel1).set_TabIndex(4);
			((Control)trackBarScale).set_AutoSize(false);
			((Control)trackBarScale).set_BackColor(Color.SaddleBrown);
			trackBarScale.set_LargeChange(2);
			((Control)trackBarScale).set_Location(new Point(0, 0));
			((Control)trackBarScale).set_Margin(new Padding(3, 3, 3, 6));
			trackBarScale.set_Maximum(130);
			trackBarScale.set_Minimum(70);
			((Control)trackBarScale).set_Name("trackBarScale");
			((Control)trackBarScale).set_Size(new Size(119, 25));
			((Control)trackBarScale).set_TabIndex(5);
			trackBarScale.set_TickFrequency(10);
			trackBarScale.set_TickStyle((TickStyle)1);
			trackBarScale.set_Value(100);
			trackBarScale.add_Scroll((EventHandler)trackBarScale_Scroll);
			((Control)panel2).set_BackColor(SystemColors.Control);
			panel2.set_BorderStyle((BorderStyle)2);
			((Control)panel2).get_Controls().Add((Control)(object)trackBarScale);
			((Control)panel2).set_Location(new Point(326, 3));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(119, 26));
			((Control)panel2).set_TabIndex(6);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_BackColor(SystemColors.Control);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(262, 3));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(64, 26));
			((Control)label2).set_TabIndex(7);
			((Control)label2).set_Text("Zero-level\r\nheight");
			((Control)lblHeight).set_AutoSize(true);
			((Control)lblHeight).set_BackColor(SystemColors.Control);
			lblHeight.set_BorderStyle((BorderStyle)1);
			((Control)lblHeight).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblHeight).set_ForeColor(SystemColors.ControlText);
			((Control)lblHeight).set_Location(new Point(445, 6));
			((Control)lblHeight).set_Name("lblHeight");
			((Control)lblHeight).set_Size(new Size(33, 19));
			((Control)lblHeight).set_TabIndex(8);
			((Control)lblHeight).set_Text("= 0");
			((Control)panel3).set_BackColor(Color.DarkSlateGray);
			((Control)panel3).get_Controls().Add((Control)(object)lblHeight);
			((Control)panel3).get_Controls().Add((Control)(object)label2);
			((Control)panel3).get_Controls().Add((Control)(object)panel2);
			((Control)panel3).get_Controls().Add((Control)(object)panel1);
			((Control)panel3).get_Controls().Add((Control)(object)label1);
			((Control)panel3).set_Location(new Point(196, 0));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(521, 32));
			((Control)panel3).set_TabIndex(9);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(721, 510));
			((Control)this).get_Controls().Add((Control)(object)panel3);
			((Control)this).get_Controls().Add((Control)(object)picMutual);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("MutualLightCurve");
			((Control)this).set_Text("Mutual event - predicted light curve");
			((Form)this).add_Load((EventHandler)MutualLightCurve_Load);
			((Control)this).add_Resize((EventHandler)MutualLightCurve_Resize);
			((ISupportInitialize)picMutual).EndInit();
			((ISupportInitialize)trackBar1).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)panel1).ResumeLayout(false);
			((ISupportInitialize)trackBarScale).EndInit();
			((Control)panel2).ResumeLayout(false);
			((Control)panel3).ResumeLayout(false);
			((Control)panel3).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
