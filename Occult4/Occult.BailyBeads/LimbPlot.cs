using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.BailyBeads
{
	public class LimbPlot : Form
	{
		private const double Radian = 180.0 / Math.PI;

		private bool FormLoaded;

		internal bool Updating;

		private bool GetToolTip = true;

		internal int ChartWidthDeg;

		internal int ChartHeightSec;

		private IContainer components;

		internal PictureBox picLimb;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem cartAxisAngleRangeToolStripMenuItem;

		private ToolStripMenuItem degToolStripMenuItem;

		private ToolStripMenuItem degToolStripMenuItem1;

		private ToolStripMenuItem degToolStripMenuItem2;

		private ToolStripMenuItem degToolStripMenuItem3;

		private ToolStripMenuItem degToolStripMenuItem4;

		private ToolStripMenuItem degToolStripMenuItem5;

		private ToolStripMenuItem degToolStripMenuItem6;

		private ToolStripMenuItem degToolStripMenuItem7;

		private ToolStripMenuItem degToolStripMenuItem8;

		private ToolStripMenuItem degToolStripMenuItem9;

		private ToolStripMenuItem degToolStripMenuItem10;

		private ToolStripMenuItem degToolStripMenuItem11;

		private ToolStripMenuItem degToolStripMenuItem12;

		private ToolStripMenuItem degToolStripMenuItem13;

		private ToolStripMenuItem degToolStripMenuItem14;

		private ToolStripMenuItem degToolStripMenuItem15;

		private ToolStripMenuItem degToolStripMenuItem16;

		private ToolStripMenuItem degToolStripMenuItem17;

		private ToolStripMenuItem heightToolStripMenuItem;

		private ToolStripMenuItem toolStripMenuItem2;

		private ToolStripMenuItem toolStripMenuItem3;

		private ToolStripMenuItem toolStripMenuItem4;

		private ToolStripMenuItem toolStripMenuItem5;

		private ToolStripMenuItem toolStripMenuItem6;

		private ToolStripMenuItem toolStripMenuItem7;

		private ToolStripMenuItem observedGrazeoccultationDataToolStripMenuItem;

		private ToolStripMenuItem includeOrdinaryOccultationsToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator3;

		private ToolStripMenuItem T0_ToolStripMenuItem;

		private ToolStripMenuItem T1_ToolStripMenuItem;

		private ToolStripMenuItem T2_ToolStripMenuItem;

		private ToolStripMenuItem T3_ToolStripMenuItem;

		private ToolStripMenuItem T4_ToolStripMenuItem;

		private ToolStripMenuItem T5_ToolStripMenuItem;

		private ToolStripMenuItem T6_ToolStripMenuItem;

		private ToolStripMenuItem T7_ToolStripMenuItem;

		private ToolStripMenuItem T8_ToolStripMenuItem;

		private ToolStripMenuItem T9_ToolStripMenuItem;

		private ToolTip toolTip1;

		private Label label1;

		private ToolStripMenuItem stayOnTopToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		internal CheckBox chkLOLApoints;

		private ToolStripMenuItem mnuSavePlot;

		public LimbPlot()
		{
			InitializeComponent();
		}

		private void LimbPlot_Load(object sender, EventArgs e)
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
			FormLoaded = false;
			SetChartWidth(16);
			SetChartHeight(3);
			includeOrdinaryOccultationsToolStripMenuItem.set_Checked(BailyBeads.LimitToGrazes);
			SetLibrationTags(0);
			((Control)chkLOLApoints).set_Enabled(Utilities.LOLAFileExists);
			FormLoaded = true;
		}

		private void LimbPlot_Resize(object sender, EventArgs e)
		{
			((Control)picLimb).set_Width(((Control)this).get_Width() - 20);
			((Control)picLimb).set_Height(((Control)this).get_Height() - 81);
		}

		private void LimbPlot_ResizeEnd(object sender, EventArgs e)
		{
			if (FormLoaded)
			{
				BailyBeads.DrawLunarLimb();
			}
		}

		private void degToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetChartWidth(4);
		}

		private void degToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			SetChartWidth(6);
		}

		private void degToolStripMenuItem2_Click(object sender, EventArgs e)
		{
			SetChartWidth(8);
		}

		private void degToolStripMenuItem3_Click(object sender, EventArgs e)
		{
			SetChartWidth(10);
		}

		private void degToolStripMenuItem4_Click(object sender, EventArgs e)
		{
			SetChartWidth(12);
		}

		private void degToolStripMenuItem5_Click(object sender, EventArgs e)
		{
			SetChartWidth(14);
		}

		private void degToolStripMenuItem6_Click(object sender, EventArgs e)
		{
			SetChartWidth(16);
		}

		private void degToolStripMenuItem7_Click(object sender, EventArgs e)
		{
			SetChartWidth(18);
		}

		private void degToolStripMenuItem8_Click(object sender, EventArgs e)
		{
			SetChartWidth(20);
		}

		private void degToolStripMenuItem9_Click(object sender, EventArgs e)
		{
			SetChartWidth(30);
		}

		private void degToolStripMenuItem10_Click(object sender, EventArgs e)
		{
			SetChartWidth(40);
		}

		private void degToolStripMenuItem11_Click(object sender, EventArgs e)
		{
			SetChartWidth(50);
		}

		private void degToolStripMenuItem12_Click(object sender, EventArgs e)
		{
			SetChartWidth(60);
		}

		private void degToolStripMenuItem13_Click(object sender, EventArgs e)
		{
			SetChartWidth(80);
		}

		private void degToolStripMenuItem14_Click(object sender, EventArgs e)
		{
			SetChartWidth(100);
		}

		private void degToolStripMenuItem15_Click(object sender, EventArgs e)
		{
			SetChartWidth(120);
		}

		private void degToolStripMenuItem16_Click(object sender, EventArgs e)
		{
			SetChartWidth(150);
		}

		private void degToolStripMenuItem17_Click(object sender, EventArgs e)
		{
			SetChartWidth(180);
		}

		private void SetChartWidth(int Width)
		{
			ChartWidthDeg = Width;
			degToolStripMenuItem.set_Checked(Width == 4);
			degToolStripMenuItem1.set_Checked(Width == 6);
			degToolStripMenuItem2.set_Checked(Width == 8);
			degToolStripMenuItem3.set_Checked(Width == 10);
			degToolStripMenuItem4.set_Checked(Width == 12);
			degToolStripMenuItem5.set_Checked(Width == 14);
			degToolStripMenuItem6.set_Checked(Width == 16);
			degToolStripMenuItem7.set_Checked(Width == 18);
			degToolStripMenuItem8.set_Checked(Width == 20);
			degToolStripMenuItem9.set_Checked(Width == 30);
			degToolStripMenuItem10.set_Checked(Width == 40);
			degToolStripMenuItem11.set_Checked(Width == 50);
			degToolStripMenuItem12.set_Checked(Width == 60);
			degToolStripMenuItem13.set_Checked(Width == 80);
			degToolStripMenuItem14.set_Checked(Width == 100);
			degToolStripMenuItem15.set_Checked(Width == 120);
			degToolStripMenuItem16.set_Checked(Width == 150);
			degToolStripMenuItem17.set_Checked(Width == 180);
			if (FormLoaded)
			{
				BailyBeads.DrawLunarLimb();
			}
		}

		private void toolStripMenuItem2_Click(object sender, EventArgs e)
		{
			SetChartHeight(2);
		}

		private void toolStripMenuItem3_Click(object sender, EventArgs e)
		{
			SetChartHeight(3);
		}

		private void toolStripMenuItem4_Click(object sender, EventArgs e)
		{
			SetChartHeight(4);
		}

		private void toolStripMenuItem5_Click(object sender, EventArgs e)
		{
			SetChartHeight(5);
		}

		private void toolStripMenuItem6_Click(object sender, EventArgs e)
		{
			SetChartHeight(10);
		}

		private void toolStripMenuItem7_Click(object sender, EventArgs e)
		{
			SetChartHeight(20);
		}

		private void SetChartHeight(int Height)
		{
			ChartHeightSec = Height;
			toolStripMenuItem2.set_Checked(Height == 2);
			toolStripMenuItem3.set_Checked(Height == 3);
			toolStripMenuItem4.set_Checked(Height == 4);
			toolStripMenuItem5.set_Checked(Height == 5);
			toolStripMenuItem6.set_Checked(Height == 10);
			toolStripMenuItem7.set_Checked(Height == 20);
			if (FormLoaded)
			{
				BailyBeads.DrawLunarLimb();
			}
		}

		private void T0_ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetLibrationTags(0);
		}

		private void T1_ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetLibrationTags(1);
		}

		private void T2_ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetLibrationTags(2);
		}

		private void T3_ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetLibrationTags(3);
		}

		private void T4_ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetLibrationTags(4);
		}

		private void T5_ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetLibrationTags(5);
		}

		private void T6_ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetLibrationTags(6);
		}

		private void T7_ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetLibrationTags(7);
		}

		private void T8_ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetLibrationTags(8);
		}

		private void T9_ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SetLibrationTags(9);
		}

		private void SetLibrationTags(int f)
		{
			T0_ToolStripMenuItem.set_Checked(f == 0);
			T1_ToolStripMenuItem.set_Checked(f == 1);
			T2_ToolStripMenuItem.set_Checked(f == 2);
			T3_ToolStripMenuItem.set_Checked(f == 3);
			T4_ToolStripMenuItem.set_Checked(f == 4);
			T5_ToolStripMenuItem.set_Checked(f == 5);
			T6_ToolStripMenuItem.set_Checked(f == 6);
			T7_ToolStripMenuItem.set_Checked(f == 7);
			T8_ToolStripMenuItem.set_Checked(f == 8);
			T9_ToolStripMenuItem.set_Checked(f == 9);
			BailyBeads.GrazeDataType = f;
			if (FormLoaded)
			{
				BailyBeads.DrawLunarLimb();
			}
		}

		private void includeOrdinaryOccultationsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			BailyBeads.LimitToGrazes = !BailyBeads.LimitToGrazes;
			includeOrdinaryOccultationsToolStripMenuItem.set_Checked(BailyBeads.LimitToGrazes);
			if (FormLoaded)
			{
				BailyBeads.DrawLunarLimb();
			}
		}

		private void picLimb_MouseMove(object sender, MouseEventArgs e)
		{
			if (!Updating)
			{
				double num = ((!BailyBeads.Mirror) ? ((double)((float)BailyBeads.AA_Min + (BailyBeads.MapRight - (float)e.get_X()) / BailyBeads.HorizontalScale_deg)) : ((double)((float)BailyBeads.AA_Min + ((float)e.get_X() - BailyBeads.MapLeft) / BailyBeads.HorizontalScale_deg)));
				if (num < 0.0)
				{
					num += 360.0;
				}
				if (num >= 360.0)
				{
					num -= 360.0;
				}
				double num2 = (BailyBeads.MapCenterY - (float)e.get_Y()) / BailyBeads.Map_VerticalScale_sec;
				GetToolTip = !GetToolTip;
				if (GetToolTip)
				{
					toolTip1.SetToolTip((Control)(object)picLimb, "AA = " + string.Format("{0,3:F2}", num) + ", H = " + string.Format("{0,3:F2}", num2));
				}
			}
		}

		private void picLimb_MouseDown(object sender, MouseEventArgs e)
		{
			//IL_0090: Unknown result type (might be due to invalid IL or missing references)
			//IL_009a: Invalid comparison between Unknown and I4
			//IL_026b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0275: Invalid comparison between Unknown and I4
			if (Updating)
			{
				return;
			}
			double num = ((!BailyBeads.Mirror) ? ((double)((float)BailyBeads.AA_Min + (BailyBeads.MapRight - (float)e.get_X()) / BailyBeads.HorizontalScale_deg)) : ((double)((float)BailyBeads.AA_Min + ((float)e.get_X() - BailyBeads.MapLeft) / BailyBeads.HorizontalScale_deg)));
			if (num < 0.0)
			{
				num += 360.0;
			}
			if (num >= 360.0)
			{
				num -= 360.0;
			}
			double height = (BailyBeads.MapCenterY - (float)e.get_Y()) / BailyBeads.Map_VerticalScale_sec;
			if ((int)e.get_Button() == 1048576)
			{
				BailyBeads.Show_Heights();
				BailyBeads.Limb_Heights.lstHeights.get_Items().Clear();
				BailyBeads.Limb_Heights.lstHeights.get_Items().Add((object)" Axis    Limb    Sun   Diff");
				num = Math.Floor(10.0 * num) / 10.0;
				for (double num2 = num - 1.1; num2 <= num + 1.1; num2 += 0.1)
				{
					double num3 = num2;
					if (num3 < 0.0)
					{
						num3 += 360.0;
					}
					if (num3 >= 360.0)
					{
						num3 -= 360.0;
					}
					height = ((!chkLOLApoints.get_Checked()) ? 0.0 : Utilities.LOLA_LimbHeight_ActualDistance(num3, BailyBeads.L, BailyBeads.B, BailyBeads.MoonScale));
					double num4 = Math.Atan2(BailyBeads.SunMoonX, BailyBeads.SunMoonY);
					double num5 = Math.Sqrt(BailyBeads.SunMoonX * BailyBeads.SunMoonX + BailyBeads.SunMoonY * BailyBeads.SunMoonY);
					double num6 = (num3 + BailyBeads.C) / (180.0 / Math.PI);
					double num7 = num5 * Math.Cos(num6 - num4) + Math.Sqrt(BailyBeads.SunR * BailyBeads.SunR - num5 * num5 * Math.Sin(num6 - num4) * Math.Sin(num6 - num4)) - BailyBeads.MoonR;
					BailyBeads.Limb_Heights.lstHeights.get_Items().Add((object)(string.Format("{0,6:F2}", num3) + string.Format("{0,7:F2}", height) + string.Format("{0,7:F2}", num7) + string.Format("{0,7:F2}", height - num7)));
				}
				((Control)BailyBeads.Limb_Heights).Focus();
			}
			else if ((int)e.get_Button() == 2097152)
			{
				BailyBeads.Show_Results();
				BailyBeads.ResultLine = new BailyResults();
				BailyBeads.ResultLine.Hr = BailyBeads.BailyBeads_Main.Hr;
				BailyBeads.ResultLine.Min = BailyBeads.BailyBeads_Main.Min;
				BailyBeads.ResultLine.Sec = BailyBeads.BailyBeads_Main.Sec;
				BailyBeads.ResultLine.Height = height;
				BailyBeads.ResultLine.Check = true;
				BailyBeads.ResultLine.AA = num;
				BailyBeads.Observations.Add(BailyBeads.ResultLine);
				BailyBeads.Results.UpdateBox();
				((ListControl)BailyBeads.Results.boxEvents).set_SelectedIndex(((ObjectCollection)BailyBeads.Results.boxEvents.get_Items()).get_Count() - 1);
			}
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

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Baily beads limb plot");
		}

		private void chkLOLApoints_CheckedChanged(object sender, EventArgs e)
		{
			if (FormLoaded)
			{
				BailyBeads.DrawLunarLimb();
			}
		}

		private void mnuSavePlot_Click(object sender, EventArgs e)
		{
			//IL_000d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0013: Invalid comparison between Unknown and I4
			string input = "Lunar limb plot -";
			if ((int)Utilities.InputDialog("File name for save", ref input) == 1)
			{
				Output.SaveGraphic(picLimb.get_Image(), input, Utilities.AppPath + "\\Predictions\\");
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
			//IL_0109: Unknown result type (might be due to invalid IL or missing references)
			//IL_0113: Expected O, but got Unknown
			//IL_0114: Unknown result type (might be due to invalid IL or missing references)
			//IL_011e: Expected O, but got Unknown
			//IL_011f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0129: Expected O, but got Unknown
			//IL_012a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0134: Expected O, but got Unknown
			//IL_0135: Unknown result type (might be due to invalid IL or missing references)
			//IL_013f: Expected O, but got Unknown
			//IL_0140: Unknown result type (might be due to invalid IL or missing references)
			//IL_014a: Expected O, but got Unknown
			//IL_014b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0155: Expected O, but got Unknown
			//IL_0156: Unknown result type (might be due to invalid IL or missing references)
			//IL_0160: Expected O, but got Unknown
			//IL_0161: Unknown result type (might be due to invalid IL or missing references)
			//IL_016b: Expected O, but got Unknown
			//IL_016c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0176: Expected O, but got Unknown
			//IL_0177: Unknown result type (might be due to invalid IL or missing references)
			//IL_0181: Expected O, but got Unknown
			//IL_0182: Unknown result type (might be due to invalid IL or missing references)
			//IL_018c: Expected O, but got Unknown
			//IL_018d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0197: Expected O, but got Unknown
			//IL_0198: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a2: Expected O, but got Unknown
			//IL_01a3: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ad: Expected O, but got Unknown
			//IL_01ae: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b8: Expected O, but got Unknown
			//IL_01b9: Unknown result type (might be due to invalid IL or missing references)
			//IL_01c3: Expected O, but got Unknown
			//IL_01c4: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ce: Expected O, but got Unknown
			//IL_01cf: Unknown result type (might be due to invalid IL or missing references)
			//IL_01d9: Expected O, but got Unknown
			//IL_01da: Unknown result type (might be due to invalid IL or missing references)
			//IL_01e4: Expected O, but got Unknown
			//IL_01e5: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ef: Expected O, but got Unknown
			//IL_01f6: Unknown result type (might be due to invalid IL or missing references)
			//IL_0200: Expected O, but got Unknown
			//IL_0201: Unknown result type (might be due to invalid IL or missing references)
			//IL_020b: Expected O, but got Unknown
			//IL_020c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0216: Expected O, but got Unknown
			//IL_12d1: Unknown result type (might be due to invalid IL or missing references)
			//IL_12db: Expected O, but got Unknown
			//IL_12e8: Unknown result type (might be due to invalid IL or missing references)
			//IL_12f2: Expected O, but got Unknown
			//IL_14c5: Unknown result type (might be due to invalid IL or missing references)
			//IL_14cf: Expected O, but got Unknown
			components = new Container();
			menuStrip1 = new MenuStrip();
			cartAxisAngleRangeToolStripMenuItem = new ToolStripMenuItem();
			degToolStripMenuItem = new ToolStripMenuItem();
			degToolStripMenuItem1 = new ToolStripMenuItem();
			degToolStripMenuItem2 = new ToolStripMenuItem();
			degToolStripMenuItem3 = new ToolStripMenuItem();
			degToolStripMenuItem4 = new ToolStripMenuItem();
			degToolStripMenuItem5 = new ToolStripMenuItem();
			degToolStripMenuItem6 = new ToolStripMenuItem();
			degToolStripMenuItem7 = new ToolStripMenuItem();
			degToolStripMenuItem8 = new ToolStripMenuItem();
			degToolStripMenuItem9 = new ToolStripMenuItem();
			degToolStripMenuItem10 = new ToolStripMenuItem();
			degToolStripMenuItem11 = new ToolStripMenuItem();
			degToolStripMenuItem12 = new ToolStripMenuItem();
			degToolStripMenuItem13 = new ToolStripMenuItem();
			degToolStripMenuItem14 = new ToolStripMenuItem();
			degToolStripMenuItem15 = new ToolStripMenuItem();
			degToolStripMenuItem16 = new ToolStripMenuItem();
			degToolStripMenuItem17 = new ToolStripMenuItem();
			heightToolStripMenuItem = new ToolStripMenuItem();
			toolStripMenuItem2 = new ToolStripMenuItem();
			toolStripMenuItem3 = new ToolStripMenuItem();
			toolStripMenuItem4 = new ToolStripMenuItem();
			toolStripMenuItem5 = new ToolStripMenuItem();
			toolStripMenuItem6 = new ToolStripMenuItem();
			toolStripMenuItem7 = new ToolStripMenuItem();
			observedGrazeoccultationDataToolStripMenuItem = new ToolStripMenuItem();
			includeOrdinaryOccultationsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator3 = new ToolStripSeparator();
			T0_ToolStripMenuItem = new ToolStripMenuItem();
			T1_ToolStripMenuItem = new ToolStripMenuItem();
			T2_ToolStripMenuItem = new ToolStripMenuItem();
			T3_ToolStripMenuItem = new ToolStripMenuItem();
			T4_ToolStripMenuItem = new ToolStripMenuItem();
			T5_ToolStripMenuItem = new ToolStripMenuItem();
			T6_ToolStripMenuItem = new ToolStripMenuItem();
			T7_ToolStripMenuItem = new ToolStripMenuItem();
			T8_ToolStripMenuItem = new ToolStripMenuItem();
			T9_ToolStripMenuItem = new ToolStripMenuItem();
			mnuSavePlot = new ToolStripMenuItem();
			stayOnTopToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			picLimb = new PictureBox();
			toolTip1 = new ToolTip(components);
			label1 = new Label();
			chkLOLApoints = new CheckBox();
			((Control)menuStrip1).SuspendLayout();
			((ISupportInitialize)picLimb).BeginInit();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)cartAxisAngleRangeToolStripMenuItem,
				(ToolStripItem)heightToolStripMenuItem,
				(ToolStripItem)observedGrazeoccultationDataToolStripMenuItem,
				(ToolStripItem)mnuSavePlot,
				(ToolStripItem)stayOnTopToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(684, 24));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)cartAxisAngleRangeToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[18]
			{
				(ToolStripItem)degToolStripMenuItem,
				(ToolStripItem)degToolStripMenuItem1,
				(ToolStripItem)degToolStripMenuItem2,
				(ToolStripItem)degToolStripMenuItem3,
				(ToolStripItem)degToolStripMenuItem4,
				(ToolStripItem)degToolStripMenuItem5,
				(ToolStripItem)degToolStripMenuItem6,
				(ToolStripItem)degToolStripMenuItem7,
				(ToolStripItem)degToolStripMenuItem8,
				(ToolStripItem)degToolStripMenuItem9,
				(ToolStripItem)degToolStripMenuItem10,
				(ToolStripItem)degToolStripMenuItem11,
				(ToolStripItem)degToolStripMenuItem12,
				(ToolStripItem)degToolStripMenuItem13,
				(ToolStripItem)degToolStripMenuItem14,
				(ToolStripItem)degToolStripMenuItem15,
				(ToolStripItem)degToolStripMenuItem16,
				(ToolStripItem)degToolStripMenuItem17
			});
			((ToolStripItem)cartAxisAngleRangeToolStripMenuItem).set_Name("cartAxisAngleRangeToolStripMenuItem");
			((ToolStripItem)cartAxisAngleRangeToolStripMenuItem).set_Size(new Size(120, 20));
			((ToolStripItem)cartAxisAngleRangeToolStripMenuItem).set_Text("AxisAngle Range... ");
			((ToolStripItem)degToolStripMenuItem).set_Name("degToolStripMenuItem");
			((ToolStripItem)degToolStripMenuItem).set_Size(new Size(115, 22));
			((ToolStripItem)degToolStripMenuItem).set_Text("4 deg");
			((ToolStripItem)degToolStripMenuItem).add_Click((EventHandler)degToolStripMenuItem_Click);
			((ToolStripItem)degToolStripMenuItem1).set_Name("degToolStripMenuItem1");
			((ToolStripItem)degToolStripMenuItem1).set_Size(new Size(115, 22));
			((ToolStripItem)degToolStripMenuItem1).set_Text("6 deg");
			((ToolStripItem)degToolStripMenuItem1).add_Click((EventHandler)degToolStripMenuItem1_Click);
			((ToolStripItem)degToolStripMenuItem2).set_Name("degToolStripMenuItem2");
			((ToolStripItem)degToolStripMenuItem2).set_Size(new Size(115, 22));
			((ToolStripItem)degToolStripMenuItem2).set_Text("8 deg");
			((ToolStripItem)degToolStripMenuItem2).add_Click((EventHandler)degToolStripMenuItem2_Click);
			((ToolStripItem)degToolStripMenuItem3).set_Name("degToolStripMenuItem3");
			((ToolStripItem)degToolStripMenuItem3).set_Size(new Size(115, 22));
			((ToolStripItem)degToolStripMenuItem3).set_Text("10 deg");
			((ToolStripItem)degToolStripMenuItem3).add_Click((EventHandler)degToolStripMenuItem3_Click);
			((ToolStripItem)degToolStripMenuItem4).set_Name("degToolStripMenuItem4");
			((ToolStripItem)degToolStripMenuItem4).set_Size(new Size(115, 22));
			((ToolStripItem)degToolStripMenuItem4).set_Text("12 deg");
			((ToolStripItem)degToolStripMenuItem4).add_Click((EventHandler)degToolStripMenuItem4_Click);
			((ToolStripItem)degToolStripMenuItem5).set_Name("degToolStripMenuItem5");
			((ToolStripItem)degToolStripMenuItem5).set_Size(new Size(115, 22));
			((ToolStripItem)degToolStripMenuItem5).set_Text("14 deg");
			((ToolStripItem)degToolStripMenuItem5).add_Click((EventHandler)degToolStripMenuItem5_Click);
			degToolStripMenuItem6.set_Checked(true);
			degToolStripMenuItem6.set_CheckState((CheckState)1);
			((ToolStripItem)degToolStripMenuItem6).set_Name("degToolStripMenuItem6");
			((ToolStripItem)degToolStripMenuItem6).set_Size(new Size(115, 22));
			((ToolStripItem)degToolStripMenuItem6).set_Text("16 deg");
			((ToolStripItem)degToolStripMenuItem6).add_Click((EventHandler)degToolStripMenuItem6_Click);
			((ToolStripItem)degToolStripMenuItem7).set_Name("degToolStripMenuItem7");
			((ToolStripItem)degToolStripMenuItem7).set_Size(new Size(115, 22));
			((ToolStripItem)degToolStripMenuItem7).set_Text("18 deg");
			((ToolStripItem)degToolStripMenuItem7).add_Click((EventHandler)degToolStripMenuItem7_Click);
			((ToolStripItem)degToolStripMenuItem8).set_Name("degToolStripMenuItem8");
			((ToolStripItem)degToolStripMenuItem8).set_Size(new Size(115, 22));
			((ToolStripItem)degToolStripMenuItem8).set_Text("20 deg");
			((ToolStripItem)degToolStripMenuItem8).add_Click((EventHandler)degToolStripMenuItem8_Click);
			((ToolStripItem)degToolStripMenuItem9).set_Name("degToolStripMenuItem9");
			((ToolStripItem)degToolStripMenuItem9).set_Size(new Size(115, 22));
			((ToolStripItem)degToolStripMenuItem9).set_Text("30 deg");
			((ToolStripItem)degToolStripMenuItem9).add_Click((EventHandler)degToolStripMenuItem9_Click);
			((ToolStripItem)degToolStripMenuItem10).set_Name("degToolStripMenuItem10");
			((ToolStripItem)degToolStripMenuItem10).set_Size(new Size(115, 22));
			((ToolStripItem)degToolStripMenuItem10).set_Text("40 deg");
			((ToolStripItem)degToolStripMenuItem10).add_Click((EventHandler)degToolStripMenuItem10_Click);
			((ToolStripItem)degToolStripMenuItem11).set_Name("degToolStripMenuItem11");
			((ToolStripItem)degToolStripMenuItem11).set_Size(new Size(115, 22));
			((ToolStripItem)degToolStripMenuItem11).set_Text("50 deg");
			((ToolStripItem)degToolStripMenuItem11).add_Click((EventHandler)degToolStripMenuItem11_Click);
			((ToolStripItem)degToolStripMenuItem12).set_Name("degToolStripMenuItem12");
			((ToolStripItem)degToolStripMenuItem12).set_Size(new Size(115, 22));
			((ToolStripItem)degToolStripMenuItem12).set_Text("60 deg");
			((ToolStripItem)degToolStripMenuItem12).add_Click((EventHandler)degToolStripMenuItem12_Click);
			((ToolStripItem)degToolStripMenuItem13).set_Name("degToolStripMenuItem13");
			((ToolStripItem)degToolStripMenuItem13).set_Size(new Size(115, 22));
			((ToolStripItem)degToolStripMenuItem13).set_Text("80 deg");
			((ToolStripItem)degToolStripMenuItem13).add_Click((EventHandler)degToolStripMenuItem13_Click);
			((ToolStripItem)degToolStripMenuItem14).set_Name("degToolStripMenuItem14");
			((ToolStripItem)degToolStripMenuItem14).set_Size(new Size(115, 22));
			((ToolStripItem)degToolStripMenuItem14).set_Text("100 deg");
			((ToolStripItem)degToolStripMenuItem14).add_Click((EventHandler)degToolStripMenuItem14_Click);
			((ToolStripItem)degToolStripMenuItem15).set_Name("degToolStripMenuItem15");
			((ToolStripItem)degToolStripMenuItem15).set_Size(new Size(115, 22));
			((ToolStripItem)degToolStripMenuItem15).set_Text("120 deg");
			((ToolStripItem)degToolStripMenuItem15).add_Click((EventHandler)degToolStripMenuItem15_Click);
			((ToolStripItem)degToolStripMenuItem16).set_Name("degToolStripMenuItem16");
			((ToolStripItem)degToolStripMenuItem16).set_Size(new Size(115, 22));
			((ToolStripItem)degToolStripMenuItem16).set_Text("150 deg");
			((ToolStripItem)degToolStripMenuItem16).add_Click((EventHandler)degToolStripMenuItem16_Click);
			((ToolStripItem)degToolStripMenuItem17).set_Name("degToolStripMenuItem17");
			((ToolStripItem)degToolStripMenuItem17).set_Size(new Size(115, 22));
			((ToolStripItem)degToolStripMenuItem17).set_Text("180deg");
			((ToolStripItem)degToolStripMenuItem17).add_Click((EventHandler)degToolStripMenuItem17_Click);
			((ToolStripDropDownItem)heightToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)toolStripMenuItem2,
				(ToolStripItem)toolStripMenuItem3,
				(ToolStripItem)toolStripMenuItem4,
				(ToolStripItem)toolStripMenuItem5,
				(ToolStripItem)toolStripMenuItem6,
				(ToolStripItem)toolStripMenuItem7
			});
			((ToolStripItem)heightToolStripMenuItem).set_Name("heightToolStripMenuItem");
			((ToolStripItem)heightToolStripMenuItem).set_Size(new Size(67, 20));
			((ToolStripItem)heightToolStripMenuItem).set_Text("Height... ");
			((ToolStripItem)toolStripMenuItem2).set_Name("toolStripMenuItem2");
			((ToolStripItem)toolStripMenuItem2).set_Size(new Size(91, 22));
			((ToolStripItem)toolStripMenuItem2).set_Text("2\"");
			((ToolStripItem)toolStripMenuItem2).add_Click((EventHandler)toolStripMenuItem2_Click);
			toolStripMenuItem3.set_Checked(true);
			toolStripMenuItem3.set_CheckState((CheckState)1);
			((ToolStripItem)toolStripMenuItem3).set_Name("toolStripMenuItem3");
			((ToolStripItem)toolStripMenuItem3).set_Size(new Size(91, 22));
			((ToolStripItem)toolStripMenuItem3).set_Text("3\" ");
			((ToolStripItem)toolStripMenuItem3).add_Click((EventHandler)toolStripMenuItem3_Click);
			((ToolStripItem)toolStripMenuItem4).set_Name("toolStripMenuItem4");
			((ToolStripItem)toolStripMenuItem4).set_Size(new Size(91, 22));
			((ToolStripItem)toolStripMenuItem4).set_Text("4\"");
			((ToolStripItem)toolStripMenuItem4).add_Click((EventHandler)toolStripMenuItem4_Click);
			((ToolStripItem)toolStripMenuItem5).set_Name("toolStripMenuItem5");
			((ToolStripItem)toolStripMenuItem5).set_Size(new Size(91, 22));
			((ToolStripItem)toolStripMenuItem5).set_Text("5\"");
			((ToolStripItem)toolStripMenuItem5).add_Click((EventHandler)toolStripMenuItem5_Click);
			((ToolStripItem)toolStripMenuItem6).set_Name("toolStripMenuItem6");
			((ToolStripItem)toolStripMenuItem6).set_Size(new Size(91, 22));
			((ToolStripItem)toolStripMenuItem6).set_Text("10\"");
			((ToolStripItem)toolStripMenuItem6).add_Click((EventHandler)toolStripMenuItem6_Click);
			((ToolStripItem)toolStripMenuItem7).set_Name("toolStripMenuItem7");
			((ToolStripItem)toolStripMenuItem7).set_Size(new Size(91, 22));
			((ToolStripItem)toolStripMenuItem7).set_Text("20\"");
			((ToolStripItem)toolStripMenuItem7).add_Click((EventHandler)toolStripMenuItem7_Click);
			((ToolStripDropDownItem)observedGrazeoccultationDataToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[12]
			{
				(ToolStripItem)includeOrdinaryOccultationsToolStripMenuItem,
				(ToolStripItem)toolStripSeparator3,
				(ToolStripItem)T0_ToolStripMenuItem,
				(ToolStripItem)T1_ToolStripMenuItem,
				(ToolStripItem)T2_ToolStripMenuItem,
				(ToolStripItem)T3_ToolStripMenuItem,
				(ToolStripItem)T4_ToolStripMenuItem,
				(ToolStripItem)T5_ToolStripMenuItem,
				(ToolStripItem)T6_ToolStripMenuItem,
				(ToolStripItem)T7_ToolStripMenuItem,
				(ToolStripItem)T8_ToolStripMenuItem,
				(ToolStripItem)T9_ToolStripMenuItem
			});
			((ToolStripItem)observedGrazeoccultationDataToolStripMenuItem).set_Image((Image)Resources.DataSet_TableView);
			((ToolStripItem)observedGrazeoccultationDataToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)observedGrazeoccultationDataToolStripMenuItem).set_Name("observedGrazeoccultationDataToolStripMenuItem");
			((ToolStripItem)observedGrazeoccultationDataToolStripMenuItem).set_Size(new Size(230, 20));
			((ToolStripItem)observedGrazeoccultationDataToolStripMenuItem).set_Text("Observed graze && occultation data    ");
			((ToolStripItem)includeOrdinaryOccultationsToolStripMenuItem).set_Name("includeOrdinaryOccultationsToolStripMenuItem");
			includeOrdinaryOccultationsToolStripMenuItem.set_ShortcutKeys((Keys)131145);
			((ToolStripItem)includeOrdinaryOccultationsToolStripMenuItem).set_Size(new Size(301, 22));
			((ToolStripItem)includeOrdinaryOccultationsToolStripMenuItem).set_Text("Exclude ordinary Visual occultations");
			((ToolStripItem)includeOrdinaryOccultationsToolStripMenuItem).set_Visible(false);
			((ToolStripItem)includeOrdinaryOccultationsToolStripMenuItem).add_Click((EventHandler)includeOrdinaryOccultationsToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator3).set_Name("toolStripSeparator3");
			((ToolStripItem)toolStripSeparator3).set_Size(new Size(298, 6));
			((ToolStripItem)toolStripSeparator3).set_Visible(false);
			T0_ToolStripMenuItem.set_Checked(true);
			T0_ToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)T0_ToolStripMenuItem).set_Name("T0_ToolStripMenuItem");
			T0_ToolStripMenuItem.set_ShortcutKeys((Keys)131120);
			((ToolStripItem)T0_ToolStripMenuItem).set_Size(new Size(301, 22));
			((ToolStripItem)T0_ToolStripMenuItem).set_Text("0  None");
			((ToolStripItem)T0_ToolStripMenuItem).add_Click((EventHandler)T0_ToolStripMenuItem_Click);
			((ToolStripItem)T1_ToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)T1_ToolStripMenuItem).set_Name("T1_ToolStripMenuItem");
			T1_ToolStripMenuItem.set_ShortcutKeys((Keys)131121);
			((ToolStripItem)T1_ToolStripMenuItem).set_Size(new Size(301, 22));
			((ToolStripItem)T1_ToolStripMenuItem).set_Text("1  P,  D[+/-0.3 deg]");
			((ToolStripItem)T1_ToolStripMenuItem).set_Visible(false);
			((ToolStripItem)T1_ToolStripMenuItem).add_Click((EventHandler)T1_ToolStripMenuItem_Click);
			((ToolStripItem)T2_ToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)T2_ToolStripMenuItem).set_Name("T2_ToolStripMenuItem");
			T2_ToolStripMenuItem.set_ShortcutKeys((Keys)131122);
			((ToolStripItem)T2_ToolStripMenuItem).set_Size(new Size(301, 22));
			((ToolStripItem)T2_ToolStripMenuItem).set_Text("2  P,  D[+/-0.5 deg]");
			((ToolStripItem)T2_ToolStripMenuItem).set_Visible(false);
			((ToolStripItem)T2_ToolStripMenuItem).add_Click((EventHandler)T2_ToolStripMenuItem_Click);
			((ToolStripItem)T3_ToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)T3_ToolStripMenuItem).set_Name("T3_ToolStripMenuItem");
			T3_ToolStripMenuItem.set_ShortcutKeys((Keys)131123);
			((ToolStripItem)T3_ToolStripMenuItem).set_Size(new Size(301, 22));
			((ToolStripItem)T3_ToolStripMenuItem).set_Text("3  P,  D[+/- 1.0 deg]");
			((ToolStripItem)T3_ToolStripMenuItem).set_Visible(false);
			((ToolStripItem)T3_ToolStripMenuItem).add_Click((EventHandler)T3_ToolStripMenuItem_Click);
			((ToolStripItem)T4_ToolStripMenuItem).set_Name("T4_ToolStripMenuItem");
			T4_ToolStripMenuItem.set_ShortcutKeys((Keys)131124);
			((ToolStripItem)T4_ToolStripMenuItem).set_Size(new Size(301, 22));
			((ToolStripItem)T4_ToolStripMenuItem).set_Text("4  AA,  L[+/- 1 deg],  B[+/- 0.5 deg]");
			((ToolStripItem)T4_ToolStripMenuItem).set_Visible(false);
			((ToolStripItem)T4_ToolStripMenuItem).add_Click((EventHandler)T4_ToolStripMenuItem_Click);
			((ToolStripItem)T5_ToolStripMenuItem).set_Name("T5_ToolStripMenuItem");
			T5_ToolStripMenuItem.set_ShortcutKeys((Keys)131125);
			((ToolStripItem)T5_ToolStripMenuItem).set_Size(new Size(301, 22));
			((ToolStripItem)T5_ToolStripMenuItem).set_Text("5  AA,  L[+/- 2 deg],  B[+/- 0.5 deg]");
			((ToolStripItem)T5_ToolStripMenuItem).set_Visible(false);
			((ToolStripItem)T5_ToolStripMenuItem).add_Click((EventHandler)T5_ToolStripMenuItem_Click);
			((ToolStripItem)T6_ToolStripMenuItem).set_Name("T6_ToolStripMenuItem");
			T6_ToolStripMenuItem.set_ShortcutKeys((Keys)131126);
			((ToolStripItem)T6_ToolStripMenuItem).set_Size(new Size(301, 22));
			((ToolStripItem)T6_ToolStripMenuItem).set_Text("6  AA,  L[+/- 1 deg],  B[+/- 1 deg]");
			((ToolStripItem)T6_ToolStripMenuItem).set_Visible(false);
			((ToolStripItem)T6_ToolStripMenuItem).add_Click((EventHandler)T6_ToolStripMenuItem_Click);
			((ToolStripItem)T7_ToolStripMenuItem).set_Name("T7_ToolStripMenuItem");
			T7_ToolStripMenuItem.set_ShortcutKeys((Keys)131127);
			((ToolStripItem)T7_ToolStripMenuItem).set_Size(new Size(301, 22));
			((ToolStripItem)T7_ToolStripMenuItem).set_Text("7  AA,  L[+/- 2 deg],  B[+/- 1 deg]");
			((ToolStripItem)T7_ToolStripMenuItem).set_Visible(false);
			((ToolStripItem)T7_ToolStripMenuItem).add_Click((EventHandler)T7_ToolStripMenuItem_Click);
			((ToolStripItem)T8_ToolStripMenuItem).set_Name("T8_ToolStripMenuItem");
			T8_ToolStripMenuItem.set_ShortcutKeys((Keys)131128);
			((ToolStripItem)T8_ToolStripMenuItem).set_Size(new Size(301, 22));
			((ToolStripItem)T8_ToolStripMenuItem).set_Text("8  AA,  L[+/- 4 deg],  B[+/- 1 deg]");
			((ToolStripItem)T8_ToolStripMenuItem).set_Visible(false);
			((ToolStripItem)T8_ToolStripMenuItem).add_Click((EventHandler)T8_ToolStripMenuItem_Click);
			((ToolStripItem)T9_ToolStripMenuItem).set_Name("T9_ToolStripMenuItem");
			T9_ToolStripMenuItem.set_ShortcutKeys((Keys)131129);
			((ToolStripItem)T9_ToolStripMenuItem).set_Size(new Size(301, 22));
			((ToolStripItem)T9_ToolStripMenuItem).set_Text("9  AA,  L[+/- 4 deg],  B[+/- 2 deg]");
			((ToolStripItem)T9_ToolStripMenuItem).set_Visible(false);
			((ToolStripItem)T9_ToolStripMenuItem).add_Click((EventHandler)T9_ToolStripMenuItem_Click);
			((ToolStripItem)mnuSavePlot).set_Image((Image)Resources.Save);
			((ToolStripItem)mnuSavePlot).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)mnuSavePlot).set_Name("mnuSavePlot");
			((ToolStripItem)mnuSavePlot).set_Size(new Size(113, 20));
			((ToolStripItem)mnuSavePlot).set_Text("Save Limb Plot");
			((ToolStripItem)mnuSavePlot).add_Click((EventHandler)mnuSavePlot_Click);
			((ToolStripItem)stayOnTopToolStripMenuItem).set_Image((Image)Resources.FillDownHS);
			((ToolStripItem)stayOnTopToolStripMenuItem).set_Name("stayOnTopToolStripMenuItem");
			((ToolStripItem)stayOnTopToolStripMenuItem).set_Size(new Size(95, 20));
			((ToolStripItem)stayOnTopToolStripMenuItem).set_Text("stay on &Top");
			((ToolStripItem)stayOnTopToolStripMenuItem).add_Click((EventHandler)stayOnTopToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(60, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((Control)picLimb).set_Location(new Point(2, 43));
			((Control)picLimb).set_Name("picLimb");
			((Control)picLimb).set_Size(new Size(677, 256));
			picLimb.set_TabIndex(0);
			picLimb.set_TabStop(false);
			((Control)picLimb).add_MouseDown(new MouseEventHandler(picLimb_MouseDown));
			((Control)picLimb).add_MouseMove(new MouseEventHandler(picLimb_MouseMove));
			toolTip1.set_AutoPopDelay(2000);
			toolTip1.set_InitialDelay(100);
			toolTip1.set_ReshowDelay(100);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(336, 27));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(283, 13));
			((Control)label1).set_TabIndex(5);
			((Control)label1).set_Text("left-click - list heights;    right-click - add point as observation");
			((Control)chkLOLApoints).set_AutoSize(true);
			chkLOLApoints.set_Checked(true);
			chkLOLApoints.set_CheckState((CheckState)1);
			((Control)chkLOLApoints).set_Location(new Point(5, 25));
			((Control)chkLOLApoints).set_Name("chkLOLApoints");
			((Control)chkLOLApoints).set_Size(new Size(84, 17));
			((Control)chkLOLApoints).set_TabIndex(1);
			((Control)chkLOLApoints).set_Text("LOLA points");
			((ButtonBase)chkLOLApoints).set_UseVisualStyleBackColor(true);
			chkLOLApoints.add_CheckedChanged((EventHandler)chkLOLApoints_CheckedChanged);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(684, 301));
			((Control)this).get_Controls().Add((Control)(object)chkLOLApoints);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)picLimb);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationBailyLimbPlot", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationBailyLimbPlot);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MinimumSize(new Size(500, 200));
			((Control)this).set_Name("LimbPlot");
			((Control)this).set_Text("Limb Plot");
			((Form)this).add_Load((EventHandler)LimbPlot_Load);
			((Form)this).add_ResizeEnd((EventHandler)LimbPlot_ResizeEnd);
			((Control)this).add_Resize((EventHandler)LimbPlot_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((ISupportInitialize)picLimb).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
