using System;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Ephemerides
{
	public class Magnitudes : Form
	{
		private bool MagChanging;

		private string Combined = "Not set";

		private string MagIntensity = "Not set";

		private string MagsFromLightCurve = "Not set";

		private string MagsFromLightLevels = "Not set";

		private IContainer components;

		private GroupBox groupBox1;

		private Label lblCombined;

		private Label label3;

		private Label label2;

		private Label label1;

		private NumericUpDown updnM2;

		private NumericUpDown updnM1;

		private NumericUpDown updnStarMag;

		private Label label4;

		private Label label5;

		private Label label6;

		private Label label7;

		private Label label8;

		private Label label9;

		private Label label10;

		private Label label11;

		private Label label12;

		private NumericUpDown updnMaxD;

		private NumericUpDown updnMinD;

		private NumericUpDown updnInterD;

		private NumericUpDown updnMinR;

		private NumericUpDown updnInterR;

		private NumericUpDown updnMaxR;

		private Label lblDiff1;

		private GroupBox groupBox2;

		private Label lblDiff4;

		private Label lblDiff3;

		private GroupBox groupBox3;

		private Label label16;

		private Label label17;

		private NumericUpDown updndIllum;

		private NumericUpDown updndMag;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private Button cmdCopyCombined;

		private Button cmdCopyMags;

		private Button cmdCopyMagInten;

		private GroupBox groupBox4;

		private GroupBox groupBox6;

		private NumericUpDown updnMagFaint;

		private NumericUpDown updnMagBright;

		private Label label26;

		private Label label25;

		private Label label24;

		private Label label29;

		private NumericUpDown updnFaint;

		private NumericUpDown updnTarget;

		private NumericUpDown updnBright;

		private Label label28;

		private Label label27;

		private TextBox txtTargetMag;

		private Label label30;

		private Label label31;

		private Label label32;

		private Label label34;

		private NumericUpDown updnAsteroidMag;

		private Label label33;

		private Label lblBase;

		private Label lblBaseD;

		private Label lblBaseR;

		private Label lblBaseRef;

		private Button cmdCopyFromLevels;

		private Label label15;

		private GroupBox groupBox5;

		private TextBox txtDia;

		private Label label14;

		private Label label13;

		private NumericUpDown updnH0;

		private NumericUpDown updnAlbedo;

		private Label label18;

		private GroupBox groupBox7;

		private Label label20;

		private Label label19;

		private NumericUpDown updnLimitingMag;

		private NumericUpDown updnTargetMag;

		private TextBox txt0;

		private TextBox txt10;

		private TextBox txt20;

		private TextBox txt30;

		private TextBox txt40;

		private TextBox txt50;

		private TextBox txt60;

		private TextBox txt70;

		private TextBox txt80;

		private TextBox txt90;

		private TextBox txt100;

		private Label label42;

		private Label label41;

		private Label label40;

		private Label label39;

		private Label label38;

		private Label label37;

		private Label label36;

		private Label label35;

		private Label label23;

		private Label label22;

		private Label label21;

		private Label label43;

		private Panel panel1;

		private NumericUpDown updnPredMagDrop;

		private Label label47;

		private Label label46;

		private Label label45;

		private TextBox txtMagDropLessThan;

		private TextBox txtMagDropGreaterThan;

		private CheckBox chkSatelliteLevels;

		private Label label44;

		private Panel panel2;

		private Panel panel3;

		private Label label48;

		private Button cmdCopyIntermediateMagnitudes;

		private Button cmdCopyMagDia;

		private ToolTip toolTip1;

		private TextBox txtInverseBright;

		private Label label49;

		private Panel panel4;

		private TextBox txtAxisRatio;

		public Magnitudes()
		{
			InitializeComponent();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Magnitude Calculator");
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void Magnitudes_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			ComputeCombined();
			ComputeIntermediate();
			SetErrorColors();
			ComputeFromLightCurve();
			DiaFromMag();
			CalculateMagValues();
		}

		private void updnM1_ValueChanged(object sender, EventArgs e)
		{
			ComputeCombined();
		}

		private void updnM2_ValueChanged(object sender, EventArgs e)
		{
			ComputeCombined();
		}

		private void ComputeCombined()
		{
			double num = (double)updnM1.get_Value();
			double num2 = (double)updnM2.get_Value();
			double num3 = -2.5 * Math.Log10(Math.Pow(10.0, num / -2.5) + Math.Pow(10.0, num2 / -2.5));
			((Control)lblCombined).set_Text(string.Format("{0,2:F2}", num3));
			Combined = string.Format("Mag {0,2:F2} + Mag {1,2:F2} = Mag {2,2:F2}", num, num2, num3);
		}

		private void cmdCopyCombined_Click(object sender, EventArgs e)
		{
			try
			{
				Clipboard.SetText(Combined);
			}
			catch
			{
			}
		}

		private void updndMag_ValueChanged(object sender, EventArgs e)
		{
			IlluminationFromMag();
		}

		private void updndIllum_ValueChanged(object sender, EventArgs e)
		{
			MagFromIllumination();
		}

		private void IlluminationFromMag()
		{
			if (!MagChanging)
			{
				MagChanging = true;
				double num = Math.Abs((double)updndMag.get_Value());
				double num2 = Math.Pow(10.0, num / 2.5);
				updndIllum.set_Value((decimal)num2);
				string text = ((int)Math.Log10(num2) + 3).ToString();
				((Control)txtInverseBright).set_Text(string.Format("{0,1:F" + text + "}", 1.0 / num2));
				if (num2 >= 1.0)
				{
					((Control)txtAxisRatio).set_Text(string.Format("{0,1:F2}", num2));
				}
				else
				{
					((Control)txtAxisRatio).set_Text(string.Format("{0,1:F2}", 1.0 / num2));
				}
				MagChanging = false;
				MagIntensity = string.Format("Mag change of {0,2:F2} = Intensity change by a factor of {1,2:F2} (or {2,2:F" + text + "})", num, num2, 1.0 / num2);
			}
		}

		private void MagFromIllumination()
		{
			if (!MagChanging)
			{
				MagChanging = true;
				double num = (double)updndIllum.get_Value();
				double num2 = 2.5 * Math.Log10(num);
				updndMag.set_Value((decimal)num2);
				string text = ((int)Math.Log10(num) + 3).ToString();
				((Control)txtInverseBright).set_Text(string.Format("{0,1:F" + text + "}", 1.0 / num));
				if (num >= 1.0)
				{
					((Control)txtAxisRatio).set_Text(string.Format("{0,1:F2}", num));
				}
				else
				{
					((Control)txtAxisRatio).set_Text(string.Format("{0,1:F2}", 1.0 / num));
				}
				MagChanging = false;
				MagIntensity = string.Format("Intensity change by a factor of {1,2:F2} (or {2,3:F" + text + "}) = Mag change of {0,2:F2}", num2, num, 1.0 / num);
			}
		}

		private void cmdCopyMagInten_Click(object sender, EventArgs e)
		{
			try
			{
				Clipboard.SetText(MagIntensity);
			}
			catch
			{
			}
		}

		private void ComputeFromLightCurve()
		{
			double num = (double)updnStarMag.get_Value();
			double num2 = (double)updnAsteroidMag.get_Value();
			double magBright = -2.5 * Math.Log10(Math.Pow(10.0, num / -2.5) + Math.Pow(10.0, num2 / -2.5));
			double num3 = (double)updnMaxD.get_Value();
			double num4 = (double)updnInterD.get_Value();
			double num5 = (double)updnMinD.get_Value();
			double num6 = (double)updnMinR.get_Value();
			double num7 = (double)updnInterR.get_Value();
			double num8 = (double)updnMaxR.get_Value();
			double num9 = 0.0;
			double num10 = 0.0;
			double num11 = 0.0;
			double num12 = 0.0;
			double num13 = 0.0;
			double num14 = 0.0;
			double num15 = 0.0;
			double num16 = 0.0;
			double num17 = 0.0;
			double num18 = 0.0;
			double num19 = 0.0;
			GetIntermediateMag_and_Floor(magBright, num2, num3, num5, num4, out var ZeroLightLevel, out var MagTarget);
			((Control)lblBaseD).set_Text(string.Format("{0,1:f0}", ZeroLightLevel));
			GetIntermediateMag_and_Floor(magBright, num2, num8, num6, num7, out var ZeroLightLevel2, out var MagTarget2);
			((Control)lblBaseR).set_Text(string.Format("{0,1:f0}", ZeroLightLevel2));
			if (double.IsNaN(MagTarget2))
			{
				num12 = (num14 = (num16 = -2.5 * Math.Log10(Math.Pow(10.0, num / -2.5) - Math.Pow(10.0, MagTarget / -2.5))));
				num13 = (num15 = (num18 = -2.5 * Math.Log10(Math.Pow(10.0, MagTarget / -2.5) - Math.Pow(10.0, num2 / -2.5))));
				num19 = (num17 = 50.0);
			}
			else if (double.IsNaN(MagTarget))
			{
				num12 = (num14 = (num16 = -2.5 * Math.Log10(Math.Pow(10.0, num / -2.5) - Math.Pow(10.0, MagTarget2 / -2.5))));
				num13 = (num15 = (num18 = -2.5 * Math.Log10(Math.Pow(10.0, MagTarget2 / -2.5) - Math.Pow(10.0, num2 / -2.5))));
				num16 = (num18 = 50.0);
			}
			else
			{
				num9 = -2.5 * Math.Log10(Math.Pow(10.0, MagTarget / -2.5) - Math.Pow(10.0, num2 / -2.5));
				num10 = -2.5 * Math.Log10(Math.Pow(10.0, MagTarget2 / -2.5) - Math.Pow(10.0, num2 / -2.5));
				num11 = -2.5 * Math.Log10((Math.Pow(10.0, MagTarget / -2.5) + Math.Pow(10.0, MagTarget2 / -2.5)) / 2.0 - Math.Pow(10.0, num2 / -2.5));
				num12 = -2.5 * Math.Log10(Math.Pow(10.0, num / -2.5) - Math.Pow(10.0, num11 / -2.5));
				num13 = -2.5 * Math.Log10(Math.Pow(10.0, num11 / -2.5) - Math.Pow(10.0, num2 / -2.5));
				num14 = -2.5 * Math.Log10((Math.Pow(10.0, num / -2.5) - Math.Pow(10.0, num9 / -2.5) + Math.Pow(10.0, num10 / -2.5) - Math.Pow(10.0, num2 / -2.5)) / 2.0);
				num15 = -2.5 * Math.Log10((Math.Pow(10.0, num / -2.5) - Math.Pow(10.0, num10 / -2.5) + Math.Pow(10.0, num9 / -2.5) - Math.Pow(10.0, num2 / -2.5)) / 2.0);
				num16 = -2.5 * Math.Log10(Math.Pow(10.0, num / -2.5) - Math.Pow(10.0, num9 / -2.5));
				num17 = -2.5 * Math.Log10(Math.Pow(10.0, num / -2.5) - Math.Pow(10.0, num10 / -2.5));
				num18 = -2.5 * Math.Log10(Math.Pow(10.0, num9 / -2.5) - Math.Pow(10.0, num2 / -2.5));
				num19 = -2.5 * Math.Log10(Math.Pow(10.0, num10 / -2.5) - Math.Pow(10.0, num2 / -2.5));
			}
			if (double.IsNaN(num12) | double.IsNaN(num13))
			{
				((Control)lblDiff3).set_Text("Sequence A-B-B-A:  No valid solution");
			}
			else
			{
				((Control)lblDiff3).set_Text(string.Format("Sequence A-B-B-A:  A = {0,2:F2},  B = {1,2:F2}", num12, num13));
			}
			if (double.IsNaN(num14) | double.IsNaN(num15))
			{
				((Control)lblDiff4).set_Text("Sequence A-B-A-B:  No valid solution");
			}
			else
			{
				((Control)lblDiff4).set_Text(string.Format("Sequence A-B-A-B:  A = {0,2:F2},  B = {1,2:F2}", num14, num15));
			}
			if (((((((double.IsNaN(num16) || num16 == 50.0) | double.IsNaN(num18)) || num18 == 50.0) | double.IsNaN(num19)) || num19 == 50.0) | double.IsNaN(num17)) || num17 == 50.0)
			{
				((Control)lblDiff1).set_Text("Sequence A-B-C-D:  No valid solution");
			}
			else
			{
				((Control)lblDiff1).set_Text(string.Format("Sequence A-B-C-D:  A = {0,2:F2},  B = {1,2:F2},  C = {2,2:F2},  D = {3,2:F2}", num16, num18, num19, num17));
			}
			MagsFromLightCurve = "Magnitudes from stepped light curve values\r\nAssuming:\r\n* Star magnitude of " + string.Format("{0,1:f2}\r\n* Asteroid magnitude of {1,1:f2}", num, num2);
			MagsFromLightCurve = MagsFromLightCurve + "\r\n* Light levels at D of " + string.Format("{0,2:F0} => {1,2:F0} => {2,2:F0}", num3, num4, num5);
			MagsFromLightCurve = MagsFromLightCurve + "\r\n* Light levels at R of " + string.Format("{0,2:F0} => {1,2:F0} => {2,2:F0}", num6, num7, num8);
			MagsFromLightCurve += string.Format("\r\n\r\nThe base light levels are  {0,1:f0} for D,  {1,1:f0} for R", ZeroLightLevel, ZeroLightLevel2);
			MagsFromLightCurve = MagsFromLightCurve + "\r\nand the derived magnitude possibilities are:\r\n" + ((Control)lblDiff3).get_Text() + "\r\n" + ((Control)lblDiff4).get_Text() + "\r\n" + ((Control)lblDiff1).get_Text();
		}

		private void cmdCopyMags_Click(object sender, EventArgs e)
		{
			try
			{
				Clipboard.SetText(MagsFromLightCurve);
			}
			catch
			{
			}
		}

		private void updnStarMag_ValueChanged(object sender, EventArgs e)
		{
			SetErrorColors();
			ComputeFromLightCurve();
		}

		private void updnAsteroidMag_ValueChanged(object sender, EventArgs e)
		{
			SetErrorColors();
			ComputeFromLightCurve();
		}

		private void updnStarMag_Leave(object sender, EventArgs e)
		{
			SetErrorColors();
			ComputeFromLightCurve();
		}

		private void updnAsteroidMag_Leave(object sender, EventArgs e)
		{
			SetErrorColors();
			ComputeFromLightCurve();
		}

		private void updnMaxD_ValueChanged(object sender, EventArgs e)
		{
			SetErrorColors();
			ComputeFromLightCurve();
		}

		private void updnInterD_ValueChanged(object sender, EventArgs e)
		{
			SetErrorColors();
			ComputeFromLightCurve();
		}

		private void updnMinD_ValueChanged(object sender, EventArgs e)
		{
			SetErrorColors();
			ComputeFromLightCurve();
		}

		private void updnMaxR_ValueChanged(object sender, EventArgs e)
		{
			SetErrorColors();
			ComputeFromLightCurve();
		}

		private void updnInterR_ValueChanged(object sender, EventArgs e)
		{
			SetErrorColors();
			ComputeFromLightCurve();
		}

		private void updnMinR_ValueChanged(object sender, EventArgs e)
		{
			SetErrorColors();
			ComputeFromLightCurve();
		}

		private void updnMaxD_Leave(object sender, EventArgs e)
		{
			SetErrorColors();
			ComputeFromLightCurve();
		}

		private void updnInterD_Leave(object sender, EventArgs e)
		{
			SetErrorColors();
			ComputeFromLightCurve();
		}

		private void updnMinD_Leave(object sender, EventArgs e)
		{
			SetErrorColors();
			ComputeFromLightCurve();
		}

		private void updnMaxR_Leave(object sender, EventArgs e)
		{
			SetErrorColors();
			ComputeFromLightCurve();
		}

		private void updnInterR_Leave(object sender, EventArgs e)
		{
			SetErrorColors();
			ComputeFromLightCurve();
		}

		private void updnMinR_Leave(object sender, EventArgs e)
		{
			SetErrorColors();
			ComputeFromLightCurve();
		}

		private void SetErrorColors()
		{
			NumericUpDown obj = updnMaxD;
			NumericUpDown obj2 = updnInterD;
			NumericUpDown obj3 = updnMinD;
			NumericUpDown obj4 = updnMinR;
			NumericUpDown obj5 = updnInterR;
			Color control;
			((Control)updnMaxR).set_BackColor(control = SystemColors.Control);
			Color color;
			((Control)obj5).set_BackColor(color = control);
			Color color2;
			((Control)obj4).set_BackColor(color2 = color);
			Color color3;
			((Control)obj3).set_BackColor(color3 = color2);
			Color backColor;
			((Control)obj2).set_BackColor(backColor = color3);
			((Control)obj).set_BackColor(backColor);
			if (updnMaxD.get_Value() - updnInterD.get_Value() + updnMaxR.get_Value() - updnInterR.get_Value() == 0m)
			{
				NumericUpDown obj6 = updnMaxD;
				NumericUpDown obj7 = updnInterD;
				NumericUpDown obj8 = updnInterR;
				((Control)updnMaxR).set_BackColor(color2 = Color.Coral);
				((Control)obj8).set_BackColor(color3 = color2);
				((Control)obj7).set_BackColor(backColor = color3);
				((Control)obj6).set_BackColor(backColor);
			}
			if (updnInterD.get_Value() - updnMinD.get_Value() + updnInterR.get_Value() - updnMinR.get_Value() == 0m)
			{
				NumericUpDown obj9 = updnInterD;
				NumericUpDown obj10 = updnMinD;
				NumericUpDown obj11 = updnMinR;
				((Control)updnInterR).set_BackColor(color2 = Color.Coral);
				((Control)obj11).set_BackColor(color3 = color2);
				((Control)obj10).set_BackColor(backColor = color3);
				((Control)obj9).set_BackColor(backColor);
			}
			if (updnMaxD.get_Value() - updnInterD.get_Value() + updnInterR.get_Value() - updnMinR.get_Value() == 0m)
			{
				NumericUpDown obj12 = updnMaxD;
				NumericUpDown obj13 = updnInterD;
				NumericUpDown obj14 = updnMinR;
				((Control)updnInterR).set_BackColor(color2 = Color.Coral);
				((Control)obj14).set_BackColor(color3 = color2);
				((Control)obj13).set_BackColor(backColor = color3);
				((Control)obj12).set_BackColor(backColor);
			}
			if (updnInterD.get_Value() - updnMinD.get_Value() + updnMaxR.get_Value() - updnInterR.get_Value() == 0m)
			{
				NumericUpDown obj15 = updnInterD;
				NumericUpDown obj16 = updnMinD;
				NumericUpDown obj17 = updnInterR;
				((Control)updnMaxR).set_BackColor(color2 = Color.Coral);
				((Control)obj17).set_BackColor(color3 = color2);
				((Control)obj16).set_BackColor(backColor = color3);
				((Control)obj15).set_BackColor(backColor);
			}
			if (updnMaxD.get_Value() <= updnInterD.get_Value())
			{
				NumericUpDown obj18 = updnMaxD;
				((Control)updnInterD).set_BackColor(backColor = Color.Pink);
				((Control)obj18).set_BackColor(backColor);
			}
			if (updnInterD.get_Value() <= updnMinD.get_Value())
			{
				NumericUpDown obj19 = updnInterD;
				((Control)updnMinD).set_BackColor(backColor = Color.Pink);
				((Control)obj19).set_BackColor(backColor);
			}
			if (updnMaxR.get_Value() <= updnInterR.get_Value())
			{
				NumericUpDown obj20 = updnMaxR;
				((Control)updnInterR).set_BackColor(backColor = Color.Pink);
				((Control)obj20).set_BackColor(backColor);
			}
			if (updnInterR.get_Value() <= updnMinR.get_Value())
			{
				NumericUpDown obj21 = updnInterR;
				((Control)updnMinR).set_BackColor(backColor = Color.Pink);
				((Control)obj21).set_BackColor(backColor);
			}
		}

		internal void ComputeIntermediate()
		{
			double num = (double)updnMagBright.get_Value();
			double num2 = (double)updnMagFaint.get_Value();
			double num3 = (double)updnBright.get_Value();
			double num4 = (double)updnFaint.get_Value();
			double num5 = (double)updnTarget.get_Value();
			GetIntermediateMag_and_Floor(num, num2, num3, num4, num5, out var ZeroLightLevel, out var MagTarget);
			if (MagTarget > -5.0 && MagTarget < 20.0)
			{
				((Control)txtTargetMag).set_Text(string.Format("{0,1:f2}", MagTarget));
				((Control)lblBaseRef).set_Text(string.Format("{0,1:f0}", ZeroLightLevel));
				MagsFromLightLevels = string.Format("Magnitudes using two reference magnitudes\r\nAssuming:\r\n* Bright reference star of magnitude {0,1:f2} at Level {1,1:f0}\r\n* Faint reference star of magnitude {2,1:f2} at Level {3,1:f0}", num, num3, num2, num4);
				MagsFromLightLevels += string.Format("\r\nthe base light level is at {0,1:f0}", ZeroLightLevel);
				MagsFromLightLevels += string.Format("\r\nand the target at level {0,1:f0} is at magnitude {1,1:f2}", num5, MagTarget);
			}
			else
			{
				TextBox obj = txtTargetMag;
				string text;
				((Control)lblBaseRef).set_Text(text = "-----");
				((Control)obj).set_Text(text);
				MagsFromLightLevels = "Not set";
			}
		}

		private static void GetIntermediateMag_and_Floor(double MagBright, double MagFaint, double BrightLevel, double FaintLevel, double TargetLevel, out double ZeroLightLevel, out double MagTarget)
		{
			double num = Math.Pow(10.0, (0.0 - (MagBright - 25.0)) / 2.5);
			double num2 = Math.Pow(10.0, (0.0 - (MagFaint - 25.0)) / 2.5);
			double num3 = (num - num2) / (BrightLevel - FaintLevel);
			double num4 = num - num3 * BrightLevel;
			ZeroLightLevel = (0.0 - num4) / num3;
			double d = num4 + num3 * TargetLevel;
			MagTarget = -2.5 * Math.Log10(d) + 25.0;
		}

		private void updnMagBright_ValueChanged(object sender, EventArgs e)
		{
			ComputeIntermediate();
		}

		private void updnMagFaint_ValueChanged(object sender, EventArgs e)
		{
			ComputeIntermediate();
		}

		private void updnBright_ValueChanged(object sender, EventArgs e)
		{
			ComputeIntermediate();
		}

		private void updnTarget_ValueChanged(object sender, EventArgs e)
		{
			ComputeIntermediate();
		}

		private void updnFaint_ValueChanged(object sender, EventArgs e)
		{
			ComputeIntermediate();
		}

		private void updnMagBright_Leave(object sender, EventArgs e)
		{
			ComputeIntermediate();
		}

		private void updnMagFaint_Leave(object sender, EventArgs e)
		{
			ComputeIntermediate();
		}

		private void updnBright_Leave(object sender, EventArgs e)
		{
			ComputeIntermediate();
		}

		private void updnTarget_Leave(object sender, EventArgs e)
		{
			ComputeIntermediate();
		}

		private void updnFaint_Leave(object sender, EventArgs e)
		{
			ComputeIntermediate();
		}

		private void cmdCopyFromLevels_Click(object sender, EventArgs e)
		{
			try
			{
				Clipboard.SetText(MagsFromLightLevels);
			}
			catch
			{
			}
		}

		private void updnH0_ValueChanged(object sender, EventArgs e)
		{
			DiaFromMag();
		}

		private void updnAlbedo_ValueChanged(object sender, EventArgs e)
		{
			DiaFromMag();
		}

		private void DiaFromMag()
		{
			Utilities.GetAsteroidDiameter(0, (double)updnH0.get_Value(), (double)updnAlbedo.get_Value(), out var MeanDiameter, out var _, out var _, out var _);
			if (MeanDiameter > 10.0)
			{
				((Control)txtDia).set_Text(string.Format("{0,1:f0}", MeanDiameter));
			}
			else if (MeanDiameter > 2.0)
			{
				((Control)txtDia).set_Text(string.Format("{0,1:f1}", MeanDiameter));
			}
			else
			{
				((Control)txtDia).set_Text(string.Format("{0,1:f2}", MeanDiameter));
			}
		}

		private void updnTargetMag_ValueChanged(object sender, EventArgs e)
		{
			if (updnTargetMag.get_Value() > updnLimitingMag.get_Value())
			{
				updnTargetMag.set_Value(updnLimitingMag.get_Value());
			}
			CalculateMagValues();
		}

		private void updnLimitingMag_ValueChanged(object sender, EventArgs e)
		{
			if (updnTargetMag.get_Value() > updnLimitingMag.get_Value())
			{
				updnLimitingMag.set_Value(updnTargetMag.get_Value());
			}
			CalculateMagValues();
		}

		private void CalculateMagValues()
		{
			double num = 1000.0;
			double num2 = 1000.0 / (Math.Pow(10.0, (double)updnPredMagDrop.get_Value() / 2.5) - 1.0);
			double num3 = 400.0 + num2;
			double num4 = -2.5 * Math.Log10(num3 / (num + num2));
			((Control)txtMagDropGreaterThan).set_Text(string.Format("{0,1:f2}", num4));
			((Control)txtMagDropLessThan).set_Text(string.Format("{0,1:f2}", updnPredMagDrop.get_Value()));
			double num5 = (double)updnTargetMag.get_Value();
			double num6 = (double)updnLimitingMag.get_Value();
			double num7 = 1000.0;
			double num8 = num7 * Math.Pow(10.0, (num5 - num6) / 2.5);
			double num9 = (num7 - num8) / 10.0;
			string text = "";
			Color white = Color.White;
			for (int i = 0; i <= 10; i++)
			{
				double num10 = 1000.0 - (double)i * num9;
				double num11 = num5 - 2.5 * Math.Log10(num10 / num7);
				text = string.Format("{0,1:f2}", num11);
				white = Color.White;
				if (chkSatelliteLevels.get_Checked())
				{
					white = ((num11 - num5 < num4) ? Color.Red : ((!(num11 - num5 > (double)updnPredMagDrop.get_Value())) ? Color.LightGreen : Color.Red));
				}
				switch (i)
				{
				case 0:
					((Control)txt100).set_Text(text);
					((Control)txt100).set_BackColor(white);
					break;
				case 1:
					((Control)txt90).set_Text(text);
					((Control)txt90).set_BackColor(white);
					break;
				case 2:
					((Control)txt80).set_Text(text);
					((Control)txt80).set_BackColor(white);
					break;
				case 3:
					((Control)txt70).set_Text(text);
					((Control)txt70).set_BackColor(white);
					break;
				case 4:
					((Control)txt60).set_Text(text);
					((Control)txt60).set_BackColor(white);
					break;
				case 5:
					((Control)txt50).set_Text(text);
					((Control)txt50).set_BackColor(white);
					break;
				case 6:
					((Control)txt40).set_Text(text);
					((Control)txt40).set_BackColor(white);
					break;
				case 7:
					((Control)txt30).set_Text(text);
					((Control)txt30).set_BackColor(white);
					break;
				case 8:
					((Control)txt20).set_Text(text);
					((Control)txt20).set_BackColor(white);
					break;
				case 9:
					((Control)txt10).set_Text(text);
					((Control)txt10).set_BackColor(white);
					break;
				case 10:
					((Control)txt0).set_Text(text);
					((Control)txt0).set_BackColor(white);
					break;
				}
			}
		}

		private void updnPredMagDrop_ValueChanged(object sender, EventArgs e)
		{
			CalculateMagValues();
		}

		private void chkSatelliteLevels_CheckedChanged(object sender, EventArgs e)
		{
			CalculateMagValues();
		}

		private void cmdCopyIntermediateMagnitudes_Click(object sender, EventArgs e)
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendFormat("Target Magnitude (100% level) = {0,1:f2}\r\n", updnTargetMag.get_Value());
			stringBuilder.AppendFormat("Limiting Magnitude (faintest stars consistently detectable) = {0,1:f2}", updnLimitingMag.get_Value());
			stringBuilder.Append("\r\n\r\nLight\r\nlevel    Mag");
			stringBuilder.Append("\r\n100%  = " + ((Control)txt100).get_Text());
			stringBuilder.Append("\r\n 90%  = " + ((Control)txt90).get_Text());
			stringBuilder.Append("\r\n 80%  = " + ((Control)txt80).get_Text());
			stringBuilder.Append("\r\n 70%  = " + ((Control)txt70).get_Text());
			stringBuilder.Append("\r\n 60%  = " + ((Control)txt60).get_Text());
			stringBuilder.Append("\r\n 50%  = " + ((Control)txt50).get_Text());
			stringBuilder.Append("\r\n 40%  = " + ((Control)txt40).get_Text());
			stringBuilder.Append("\r\n 30%  = " + ((Control)txt30).get_Text());
			stringBuilder.Append("\r\n 20%  = " + ((Control)txt20).get_Text());
			stringBuilder.Append("\r\n 10%  = " + ((Control)txt10).get_Text());
			stringBuilder.Append("\r\n  0%  = " + ((Control)txt0).get_Text());
			stringBuilder.Append("\r\n\r\n\r\nMinimum and Maximum magnitude drops for a satellite detection");
			stringBuilder.AppendFormat("\r\n Predicted magnitude drop for the event: {0,1:f2}", updnPredMagDrop.get_Value());
			stringBuilder.AppendFormat("\r\n =============================");
			stringBuilder.Append("\r\n Magnitude drop of both drops must be equal and");
			stringBuilder.Append("\r\n greater than " + ((Control)txtMagDropGreaterThan).get_Text() + ", AND less than  " + ((Control)txtMagDropLessThan).get_Text());
			stringBuilder.Append("\r\n\r\n    NOTE 1: An equal magnitude double star will have a light drop of 50% (0.75 mag)");
			stringBuilder.Append("\r\n    To clearly distinguish from a double star, a light drop of 60% (1.00 mag) is used");
			stringBuilder.Append("\r\n    NOTE 2: If the satellite is small (<2km), Fresnel diffraction may affect the light drop.");
			Clipboard.SetText(stringBuilder.ToString());
		}

		private void cmdCopyMagDia_Click(object sender, EventArgs e)
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendFormat("Absolute Magnitude (H0) = {0,1:f2}\r\n", updnH0.get_Value());
			stringBuilder.AppendFormat("Albedo = {0,1:f2}\r\n====================\r\n", updnAlbedo.get_Value());
			stringBuilder.Append("Diameter = " + ((Control)txtDia).get_Text() + " km");
			Clipboard.SetText(stringBuilder.ToString());
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
			//IL_00d7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e1: Expected O, but got Unknown
			//IL_00e2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ec: Expected O, but got Unknown
			//IL_00ed: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f7: Expected O, but got Unknown
			//IL_00f8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0102: Expected O, but got Unknown
			//IL_0103: Unknown result type (might be due to invalid IL or missing references)
			//IL_010d: Expected O, but got Unknown
			//IL_010e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0118: Expected O, but got Unknown
			//IL_0119: Unknown result type (might be due to invalid IL or missing references)
			//IL_0123: Expected O, but got Unknown
			//IL_0124: Unknown result type (might be due to invalid IL or missing references)
			//IL_012e: Expected O, but got Unknown
			//IL_012f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0139: Expected O, but got Unknown
			//IL_013a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0144: Expected O, but got Unknown
			//IL_0145: Unknown result type (might be due to invalid IL or missing references)
			//IL_014f: Expected O, but got Unknown
			//IL_0150: Unknown result type (might be due to invalid IL or missing references)
			//IL_015a: Expected O, but got Unknown
			//IL_015b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0165: Expected O, but got Unknown
			//IL_0166: Unknown result type (might be due to invalid IL or missing references)
			//IL_0170: Expected O, but got Unknown
			//IL_0171: Unknown result type (might be due to invalid IL or missing references)
			//IL_017b: Expected O, but got Unknown
			//IL_017c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0186: Expected O, but got Unknown
			//IL_0187: Unknown result type (might be due to invalid IL or missing references)
			//IL_0191: Expected O, but got Unknown
			//IL_0192: Unknown result type (might be due to invalid IL or missing references)
			//IL_019c: Expected O, but got Unknown
			//IL_019d: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a7: Expected O, but got Unknown
			//IL_01a8: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b2: Expected O, but got Unknown
			//IL_01b3: Unknown result type (might be due to invalid IL or missing references)
			//IL_01bd: Expected O, but got Unknown
			//IL_01be: Unknown result type (might be due to invalid IL or missing references)
			//IL_01c8: Expected O, but got Unknown
			//IL_01c9: Unknown result type (might be due to invalid IL or missing references)
			//IL_01d3: Expected O, but got Unknown
			//IL_01d4: Unknown result type (might be due to invalid IL or missing references)
			//IL_01de: Expected O, but got Unknown
			//IL_01df: Unknown result type (might be due to invalid IL or missing references)
			//IL_01e9: Expected O, but got Unknown
			//IL_01ea: Unknown result type (might be due to invalid IL or missing references)
			//IL_01f4: Expected O, but got Unknown
			//IL_01f5: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ff: Expected O, but got Unknown
			//IL_0200: Unknown result type (might be due to invalid IL or missing references)
			//IL_020a: Expected O, but got Unknown
			//IL_020b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0215: Expected O, but got Unknown
			//IL_0216: Unknown result type (might be due to invalid IL or missing references)
			//IL_0220: Expected O, but got Unknown
			//IL_0221: Unknown result type (might be due to invalid IL or missing references)
			//IL_022b: Expected O, but got Unknown
			//IL_022c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0236: Expected O, but got Unknown
			//IL_0237: Unknown result type (might be due to invalid IL or missing references)
			//IL_0241: Expected O, but got Unknown
			//IL_0242: Unknown result type (might be due to invalid IL or missing references)
			//IL_024c: Expected O, but got Unknown
			//IL_024d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0257: Expected O, but got Unknown
			//IL_0258: Unknown result type (might be due to invalid IL or missing references)
			//IL_0262: Expected O, but got Unknown
			//IL_0263: Unknown result type (might be due to invalid IL or missing references)
			//IL_026d: Expected O, but got Unknown
			//IL_026e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0278: Expected O, but got Unknown
			//IL_0279: Unknown result type (might be due to invalid IL or missing references)
			//IL_0283: Expected O, but got Unknown
			//IL_0284: Unknown result type (might be due to invalid IL or missing references)
			//IL_028e: Expected O, but got Unknown
			//IL_028f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0299: Expected O, but got Unknown
			//IL_029a: Unknown result type (might be due to invalid IL or missing references)
			//IL_02a4: Expected O, but got Unknown
			//IL_02a5: Unknown result type (might be due to invalid IL or missing references)
			//IL_02af: Expected O, but got Unknown
			//IL_02b0: Unknown result type (might be due to invalid IL or missing references)
			//IL_02ba: Expected O, but got Unknown
			//IL_02bb: Unknown result type (might be due to invalid IL or missing references)
			//IL_02c5: Expected O, but got Unknown
			//IL_02c6: Unknown result type (might be due to invalid IL or missing references)
			//IL_02d0: Expected O, but got Unknown
			//IL_02d1: Unknown result type (might be due to invalid IL or missing references)
			//IL_02db: Expected O, but got Unknown
			//IL_02dc: Unknown result type (might be due to invalid IL or missing references)
			//IL_02e6: Expected O, but got Unknown
			//IL_02e7: Unknown result type (might be due to invalid IL or missing references)
			//IL_02f1: Expected O, but got Unknown
			//IL_02f2: Unknown result type (might be due to invalid IL or missing references)
			//IL_02fc: Expected O, but got Unknown
			//IL_02fd: Unknown result type (might be due to invalid IL or missing references)
			//IL_0307: Expected O, but got Unknown
			//IL_0308: Unknown result type (might be due to invalid IL or missing references)
			//IL_0312: Expected O, but got Unknown
			//IL_0313: Unknown result type (might be due to invalid IL or missing references)
			//IL_031d: Expected O, but got Unknown
			//IL_031e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0328: Expected O, but got Unknown
			//IL_0329: Unknown result type (might be due to invalid IL or missing references)
			//IL_0333: Expected O, but got Unknown
			//IL_0334: Unknown result type (might be due to invalid IL or missing references)
			//IL_033e: Expected O, but got Unknown
			//IL_033f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0349: Expected O, but got Unknown
			//IL_034a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0354: Expected O, but got Unknown
			//IL_0355: Unknown result type (might be due to invalid IL or missing references)
			//IL_035f: Expected O, but got Unknown
			//IL_0360: Unknown result type (might be due to invalid IL or missing references)
			//IL_036a: Expected O, but got Unknown
			//IL_036b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0375: Expected O, but got Unknown
			//IL_0376: Unknown result type (might be due to invalid IL or missing references)
			//IL_0380: Expected O, but got Unknown
			//IL_0381: Unknown result type (might be due to invalid IL or missing references)
			//IL_038b: Expected O, but got Unknown
			//IL_038c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0396: Expected O, but got Unknown
			//IL_0397: Unknown result type (might be due to invalid IL or missing references)
			//IL_03a1: Expected O, but got Unknown
			//IL_03a2: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ac: Expected O, but got Unknown
			//IL_03ad: Unknown result type (might be due to invalid IL or missing references)
			//IL_03b7: Expected O, but got Unknown
			//IL_03b8: Unknown result type (might be due to invalid IL or missing references)
			//IL_03c2: Expected O, but got Unknown
			//IL_03c3: Unknown result type (might be due to invalid IL or missing references)
			//IL_03cd: Expected O, but got Unknown
			//IL_03ce: Unknown result type (might be due to invalid IL or missing references)
			//IL_03d8: Expected O, but got Unknown
			//IL_03d9: Unknown result type (might be due to invalid IL or missing references)
			//IL_03e3: Expected O, but got Unknown
			//IL_03e4: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ee: Expected O, but got Unknown
			//IL_03ef: Unknown result type (might be due to invalid IL or missing references)
			//IL_03f9: Expected O, but got Unknown
			//IL_03fa: Unknown result type (might be due to invalid IL or missing references)
			//IL_0404: Expected O, but got Unknown
			//IL_0405: Unknown result type (might be due to invalid IL or missing references)
			//IL_040f: Expected O, but got Unknown
			//IL_0410: Unknown result type (might be due to invalid IL or missing references)
			//IL_041a: Expected O, but got Unknown
			//IL_041b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0425: Expected O, but got Unknown
			//IL_0426: Unknown result type (might be due to invalid IL or missing references)
			//IL_0430: Expected O, but got Unknown
			//IL_0431: Unknown result type (might be due to invalid IL or missing references)
			//IL_043b: Expected O, but got Unknown
			//IL_043c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0446: Expected O, but got Unknown
			//IL_0447: Unknown result type (might be due to invalid IL or missing references)
			//IL_0451: Expected O, but got Unknown
			//IL_0452: Unknown result type (might be due to invalid IL or missing references)
			//IL_045c: Expected O, but got Unknown
			//IL_045d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0467: Expected O, but got Unknown
			//IL_0468: Unknown result type (might be due to invalid IL or missing references)
			//IL_0472: Expected O, but got Unknown
			//IL_0473: Unknown result type (might be due to invalid IL or missing references)
			//IL_047d: Expected O, but got Unknown
			//IL_047e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0488: Expected O, but got Unknown
			//IL_0489: Unknown result type (might be due to invalid IL or missing references)
			//IL_0493: Expected O, but got Unknown
			//IL_0494: Unknown result type (might be due to invalid IL or missing references)
			//IL_049e: Expected O, but got Unknown
			//IL_049f: Unknown result type (might be due to invalid IL or missing references)
			//IL_04a9: Expected O, but got Unknown
			//IL_04aa: Unknown result type (might be due to invalid IL or missing references)
			//IL_04b4: Expected O, but got Unknown
			//IL_04b5: Unknown result type (might be due to invalid IL or missing references)
			//IL_04bf: Expected O, but got Unknown
			//IL_04c0: Unknown result type (might be due to invalid IL or missing references)
			//IL_04ca: Expected O, but got Unknown
			//IL_04cb: Unknown result type (might be due to invalid IL or missing references)
			//IL_04d5: Expected O, but got Unknown
			//IL_04d6: Unknown result type (might be due to invalid IL or missing references)
			//IL_04e0: Expected O, but got Unknown
			//IL_04e1: Unknown result type (might be due to invalid IL or missing references)
			//IL_04eb: Expected O, but got Unknown
			//IL_04ec: Unknown result type (might be due to invalid IL or missing references)
			//IL_04f6: Expected O, but got Unknown
			//IL_04fd: Unknown result type (might be due to invalid IL or missing references)
			//IL_0507: Expected O, but got Unknown
			//IL_0508: Unknown result type (might be due to invalid IL or missing references)
			//IL_0512: Expected O, but got Unknown
			//IL_0513: Unknown result type (might be due to invalid IL or missing references)
			//IL_051d: Expected O, but got Unknown
			//IL_051e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0528: Expected O, but got Unknown
			//IL_0529: Unknown result type (might be due to invalid IL or missing references)
			//IL_0533: Expected O, but got Unknown
			//IL_5af7: Unknown result type (might be due to invalid IL or missing references)
			//IL_5b01: Expected O, but got Unknown
			components = new Container();
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(Magnitudes));
			groupBox1 = new GroupBox();
			cmdCopyCombined = new Button();
			lblCombined = new Label();
			label3 = new Label();
			label2 = new Label();
			label1 = new Label();
			updnM2 = new NumericUpDown();
			updnM1 = new NumericUpDown();
			updnStarMag = new NumericUpDown();
			label4 = new Label();
			label5 = new Label();
			label6 = new Label();
			label7 = new Label();
			label8 = new Label();
			label9 = new Label();
			label10 = new Label();
			label11 = new Label();
			label12 = new Label();
			updnMaxD = new NumericUpDown();
			updnMinD = new NumericUpDown();
			updnInterD = new NumericUpDown();
			updnMinR = new NumericUpDown();
			updnInterR = new NumericUpDown();
			updnMaxR = new NumericUpDown();
			lblDiff1 = new Label();
			groupBox2 = new GroupBox();
			label34 = new Label();
			updnAsteroidMag = new NumericUpDown();
			label31 = new Label();
			groupBox4 = new GroupBox();
			lblBaseR = new Label();
			lblBaseD = new Label();
			label33 = new Label();
			lblBase = new Label();
			cmdCopyMags = new Button();
			lblDiff4 = new Label();
			lblDiff3 = new Label();
			groupBox3 = new GroupBox();
			cmdCopyMagInten = new Button();
			label16 = new Label();
			label17 = new Label();
			updndIllum = new NumericUpDown();
			updndMag = new NumericUpDown();
			menuStrip1 = new MenuStrip();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			groupBox6 = new GroupBox();
			cmdCopyFromLevels = new Button();
			lblBaseRef = new Label();
			label32 = new Label();
			label30 = new Label();
			txtTargetMag = new TextBox();
			label29 = new Label();
			updnFaint = new NumericUpDown();
			updnTarget = new NumericUpDown();
			updnBright = new NumericUpDown();
			label28 = new Label();
			label27 = new Label();
			updnMagFaint = new NumericUpDown();
			updnMagBright = new NumericUpDown();
			label26 = new Label();
			label25 = new Label();
			label24 = new Label();
			label15 = new Label();
			groupBox5 = new GroupBox();
			cmdCopyMagDia = new Button();
			updnAlbedo = new NumericUpDown();
			label18 = new Label();
			txtDia = new TextBox();
			label14 = new Label();
			label13 = new Label();
			updnH0 = new NumericUpDown();
			groupBox7 = new GroupBox();
			label48 = new Label();
			panel3 = new Panel();
			label19 = new Label();
			updnLimitingMag = new NumericUpDown();
			updnTargetMag = new NumericUpDown();
			label20 = new Label();
			panel2 = new Panel();
			cmdCopyIntermediateMagnitudes = new Button();
			label42 = new Label();
			label41 = new Label();
			label40 = new Label();
			label39 = new Label();
			label38 = new Label();
			label37 = new Label();
			label36 = new Label();
			label35 = new Label();
			label23 = new Label();
			label22 = new Label();
			label21 = new Label();
			txt0 = new TextBox();
			txt10 = new TextBox();
			txt20 = new TextBox();
			txt30 = new TextBox();
			txt40 = new TextBox();
			txt50 = new TextBox();
			txt60 = new TextBox();
			txt70 = new TextBox();
			txt80 = new TextBox();
			txt90 = new TextBox();
			txt100 = new TextBox();
			panel1 = new Panel();
			chkSatelliteLevels = new CheckBox();
			label44 = new Label();
			txtMagDropLessThan = new TextBox();
			txtMagDropGreaterThan = new TextBox();
			label47 = new Label();
			label46 = new Label();
			label45 = new Label();
			updnPredMagDrop = new NumericUpDown();
			label43 = new Label();
			toolTip1 = new ToolTip(components);
			txtInverseBright = new TextBox();
			txtAxisRatio = new TextBox();
			panel4 = new Panel();
			label49 = new Label();
			((Control)groupBox1).SuspendLayout();
			((ISupportInitialize)updnM2).BeginInit();
			((ISupportInitialize)updnM1).BeginInit();
			((ISupportInitialize)updnStarMag).BeginInit();
			((ISupportInitialize)updnMaxD).BeginInit();
			((ISupportInitialize)updnMinD).BeginInit();
			((ISupportInitialize)updnInterD).BeginInit();
			((ISupportInitialize)updnMinR).BeginInit();
			((ISupportInitialize)updnInterR).BeginInit();
			((ISupportInitialize)updnMaxR).BeginInit();
			((Control)groupBox2).SuspendLayout();
			((ISupportInitialize)updnAsteroidMag).BeginInit();
			((Control)groupBox4).SuspendLayout();
			((Control)groupBox3).SuspendLayout();
			((ISupportInitialize)updndIllum).BeginInit();
			((ISupportInitialize)updndMag).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((Control)groupBox6).SuspendLayout();
			((ISupportInitialize)updnFaint).BeginInit();
			((ISupportInitialize)updnTarget).BeginInit();
			((ISupportInitialize)updnBright).BeginInit();
			((ISupportInitialize)updnMagFaint).BeginInit();
			((ISupportInitialize)updnMagBright).BeginInit();
			((Control)groupBox5).SuspendLayout();
			((ISupportInitialize)updnAlbedo).BeginInit();
			((ISupportInitialize)updnH0).BeginInit();
			((Control)groupBox7).SuspendLayout();
			((Control)panel3).SuspendLayout();
			((ISupportInitialize)updnLimitingMag).BeginInit();
			((ISupportInitialize)updnTargetMag).BeginInit();
			((Control)panel2).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((ISupportInitialize)updnPredMagDrop).BeginInit();
			((Control)panel4).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)groupBox1).set_BackColor(Color.AntiqueWhite);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdCopyCombined);
			((Control)groupBox1).get_Controls().Add((Control)(object)lblCombined);
			((Control)groupBox1).get_Controls().Add((Control)(object)label3);
			((Control)groupBox1).get_Controls().Add((Control)(object)label2);
			((Control)groupBox1).get_Controls().Add((Control)(object)label1);
			((Control)groupBox1).get_Controls().Add((Control)(object)updnM2);
			((Control)groupBox1).get_Controls().Add((Control)(object)updnM1);
			((Control)groupBox1).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox1).set_Location(new Point(7, 27));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(491, 59));
			((Control)groupBox1).set_TabIndex(0);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("1.  Combined magnitude of two known stars");
			((Control)cmdCopyCombined).set_BackColor(Color.DeepSkyBlue);
			((Control)cmdCopyCombined).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdCopyCombined).set_Location(new Point(434, 25));
			((Control)cmdCopyCombined).set_Name("cmdCopyCombined");
			((Control)cmdCopyCombined).set_Size(new Size(46, 24));
			((Control)cmdCopyCombined).set_TabIndex(2);
			((Control)cmdCopyCombined).set_Text("Copy");
			((ButtonBase)cmdCopyCombined).set_UseVisualStyleBackColor(false);
			((Control)cmdCopyCombined).add_Click((EventHandler)cmdCopyCombined_Click);
			((Control)lblCombined).set_AutoSize(true);
			((Control)lblCombined).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblCombined).set_Location(new Point(373, 31));
			((Control)lblCombined).set_Name("lblCombined");
			((Control)lblCombined).set_Size(new Size(16, 13));
			((Control)lblCombined).set_TabIndex(6);
			((Control)lblCombined).set_Text("...");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(248, 31));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(119, 13));
			((Control)label3).set_TabIndex(5);
			((Control)label3).set_Text("Combined Magnitude = ");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(129, 31));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(44, 13));
			((Control)label2).set_TabIndex(4);
			((Control)label2).set_Text("Mag #2");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(12, 31));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(44, 13));
			((Control)label1).set_TabIndex(3);
			((Control)label1).set_Text("Mag #1");
			updnM2.set_DecimalPlaces(2);
			((Control)updnM2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnM2.set_Increment(new decimal(new int[4] { 2, 0, 0, 65536 }));
			((Control)updnM2).set_Location(new Point(175, 27));
			updnM2.set_Maximum(new decimal(new int[4] { 20, 0, 0, 0 }));
			updnM2.set_Minimum(new decimal(new int[4] { 2, 0, 0, -2147483648 }));
			((Control)updnM2).set_Name("updnM2");
			((Control)updnM2).set_Size(new Size(55, 20));
			((Control)updnM2).set_TabIndex(1);
			updnM2.set_Value(new decimal(new int[4] { 8, 0, 0, 0 }));
			updnM2.add_ValueChanged((EventHandler)updnM2_ValueChanged);
			updnM1.set_DecimalPlaces(2);
			((Control)updnM1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnM1.set_Increment(new decimal(new int[4] { 2, 0, 0, 65536 }));
			((Control)updnM1).set_Location(new Point(58, 27));
			updnM1.set_Maximum(new decimal(new int[4] { 20, 0, 0, 0 }));
			updnM1.set_Minimum(new decimal(new int[4] { 2, 0, 0, -2147483648 }));
			((Control)updnM1).set_Name("updnM1");
			((Control)updnM1).set_Size(new Size(55, 20));
			((Control)updnM1).set_TabIndex(0);
			updnM1.set_Value(new decimal(new int[4] { 8, 0, 0, 0 }));
			updnM1.add_ValueChanged((EventHandler)updnM1_ValueChanged);
			updnStarMag.set_DecimalPlaces(2);
			((Control)updnStarMag).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnStarMag.set_Increment(new decimal(new int[4] { 2, 0, 0, 65536 }));
			((Control)updnStarMag).set_Location(new Point(15, 183));
			updnStarMag.set_Maximum(new decimal(new int[4] { 20, 0, 0, 0 }));
			updnStarMag.set_Minimum(new decimal(new int[4] { 2, 0, 0, -2147483648 }));
			((Control)updnStarMag).set_Name("updnStarMag");
			((Control)updnStarMag).set_Size(new Size(55, 20));
			((Control)updnStarMag).set_TabIndex(2);
			updnStarMag.set_Value(new decimal(new int[4] { 8, 0, 0, 0 }));
			updnStarMag.add_ValueChanged((EventHandler)updnStarMag_ValueChanged);
			((Control)updnStarMag).add_Leave((EventHandler)updnStarMag_Leave);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(10, 152));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(56, 26));
			((Control)label4).set_TabIndex(1);
			((Control)label4).set_Text("Star's\nmagnitude");
			label4.set_TextAlign(ContentAlignment.TopCenter);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label5).set_ForeColor(SystemColors.ControlText);
			((Control)label5).set_Location(new Point(29, 84));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(99, 13));
			((Control)label5).set_TabIndex(7);
			((Control)label5).set_Text("Minimum light value");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label6).set_ForeColor(SystemColors.ControlText);
			((Control)label6).set_Location(new Point(12, 62));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(116, 13));
			((Control)label6).set_TabIndex(5);
			((Control)label6).set_Text("Intermediate light value");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label7).set_ForeColor(SystemColors.ControlText);
			((Control)label7).set_Location(new Point(54, 40));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(74, 13));
			((Control)label7).set_TabIndex(3);
			((Control)label7).set_Text("Full light value");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label8).set_ForeColor(Color.Indigo);
			((Control)label8).set_Location(new Point(110, 15));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(103, 17));
			((Control)label8).set_TabIndex(1);
			((Control)label8).set_Text("D-part of curve");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label9).set_ForeColor(Color.Indigo);
			((Control)label9).set_Location(new Point(343, 15));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(103, 17));
			((Control)label9).set_TabIndex(2);
			((Control)label9).set_Text("R-part of curve");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label10).set_ForeColor(SystemColors.ControlText);
			((Control)label10).set_Location(new Point(287, 40));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(74, 13));
			((Control)label10).set_TabIndex(9);
			((Control)label10).set_Text("Full light value");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label11).set_ForeColor(SystemColors.ControlText);
			((Control)label11).set_Location(new Point(245, 62));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(116, 13));
			((Control)label11).set_TabIndex(11);
			((Control)label11).set_Text("Intermediate light value");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label12).set_ForeColor(SystemColors.ControlText);
			((Control)label12).set_Location(new Point(262, 84));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(99, 13));
			((Control)label12).set_TabIndex(13);
			((Control)label12).set_Text("Minimum light value");
			((Control)updnMaxD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnMaxD.set_Increment(new decimal(new int[4] { 10, 0, 0, 0 }));
			((Control)updnMaxD).set_Location(new Point(137, 36));
			updnMaxD.set_Maximum(new decimal(new int[4] { 100000, 0, 0, 0 }));
			updnMaxD.set_Minimum(new decimal(new int[4] { 1000, 0, 0, -2147483648 }));
			((Control)updnMaxD).set_Name("updnMaxD");
			((Control)updnMaxD).set_Size(new Size(66, 20));
			((Control)updnMaxD).set_TabIndex(4);
			updnMaxD.set_Value(new decimal(new int[4] { 10000, 0, 0, 0 }));
			updnMaxD.add_ValueChanged((EventHandler)updnMaxD_ValueChanged);
			((Control)updnMaxD).add_Leave((EventHandler)updnMaxD_Leave);
			((Control)updnMinD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnMinD.set_Increment(new decimal(new int[4] { 10, 0, 0, 0 }));
			((Control)updnMinD).set_Location(new Point(137, 80));
			updnMinD.set_Maximum(new decimal(new int[4] { 100000, 0, 0, 0 }));
			updnMinD.set_Minimum(new decimal(new int[4] { 1000, 0, 0, -2147483648 }));
			((Control)updnMinD).set_Name("updnMinD");
			((Control)updnMinD).set_Size(new Size(66, 20));
			((Control)updnMinD).set_TabIndex(8);
			updnMinD.set_Value(new decimal(new int[4] { 4000, 0, 0, 0 }));
			updnMinD.add_ValueChanged((EventHandler)updnMinD_ValueChanged);
			((Control)updnMinD).add_Leave((EventHandler)updnMinD_Leave);
			((Control)updnInterD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnInterD.set_Increment(new decimal(new int[4] { 10, 0, 0, 0 }));
			((Control)updnInterD).set_Location(new Point(137, 58));
			updnInterD.set_Maximum(new decimal(new int[4] { 100000, 0, 0, 0 }));
			updnInterD.set_Minimum(new decimal(new int[4] { 1000, 0, 0, -2147483648 }));
			((Control)updnInterD).set_Name("updnInterD");
			((Control)updnInterD).set_Size(new Size(66, 20));
			((Control)updnInterD).set_TabIndex(6);
			updnInterD.set_Value(new decimal(new int[4] { 8000, 0, 0, 0 }));
			updnInterD.add_ValueChanged((EventHandler)updnInterD_ValueChanged);
			((Control)updnInterD).add_Leave((EventHandler)updnInterD_Leave);
			((Control)updnMinR).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnMinR.set_Increment(new decimal(new int[4] { 10, 0, 0, 0 }));
			((Control)updnMinR).set_Location(new Point(370, 80));
			updnMinR.set_Maximum(new decimal(new int[4] { 100000, 0, 0, 0 }));
			updnMinR.set_Minimum(new decimal(new int[4] { 1000, 0, 0, -2147483648 }));
			((Control)updnMinR).set_Name("updnMinR");
			((Control)updnMinR).set_Size(new Size(66, 20));
			((Control)updnMinR).set_TabIndex(14);
			updnMinR.set_Value(new decimal(new int[4] { 4000, 0, 0, 0 }));
			updnMinR.add_ValueChanged((EventHandler)updnMinR_ValueChanged);
			((Control)updnMinR).add_Leave((EventHandler)updnMinR_Leave);
			((Control)updnInterR).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnInterR.set_Increment(new decimal(new int[4] { 10, 0, 0, 0 }));
			((Control)updnInterR).set_Location(new Point(370, 58));
			updnInterR.set_Maximum(new decimal(new int[4] { 100000, 0, 0, 0 }));
			updnInterR.set_Minimum(new decimal(new int[4] { 1000, 0, 0, -2147483648 }));
			((Control)updnInterR).set_Name("updnInterR");
			((Control)updnInterR).set_Size(new Size(66, 20));
			((Control)updnInterR).set_TabIndex(12);
			updnInterR.set_Value(new decimal(new int[4] { 7800, 0, 0, 0 }));
			updnInterR.add_ValueChanged((EventHandler)updnInterR_ValueChanged);
			((Control)updnInterR).add_Leave((EventHandler)updnInterR_Leave);
			((Control)updnMaxR).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnMaxR.set_Increment(new decimal(new int[4] { 10, 0, 0, 0 }));
			((Control)updnMaxR).set_Location(new Point(370, 36));
			updnMaxR.set_Maximum(new decimal(new int[4] { 100000, 0, 0, 0 }));
			updnMaxR.set_Minimum(new decimal(new int[4] { 1000, 0, 0, -2147483648 }));
			((Control)updnMaxR).set_Name("updnMaxR");
			((Control)updnMaxR).set_Size(new Size(66, 20));
			((Control)updnMaxR).set_TabIndex(10);
			updnMaxR.set_Value(new decimal(new int[4] { 10000, 0, 0, 0 }));
			updnMaxR.add_ValueChanged((EventHandler)updnMaxR_ValueChanged);
			((Control)updnMaxR).add_Leave((EventHandler)updnMaxR_Leave);
			((Control)lblDiff1).set_AutoSize(true);
			((Control)lblDiff1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblDiff1).set_Location(new Point(181, 189));
			((Control)lblDiff1).set_Name("lblDiff1");
			((Control)lblDiff1).set_Size(new Size(103, 13));
			((Control)lblDiff1).set_TabIndex(3);
			((Control)lblDiff1).set_Text("Sequence  A-B-C-D:");
			((Control)groupBox2).set_BackColor(Color.AliceBlue);
			((Control)groupBox2).get_Controls().Add((Control)(object)label34);
			((Control)groupBox2).get_Controls().Add((Control)(object)updnAsteroidMag);
			((Control)groupBox2).get_Controls().Add((Control)(object)label31);
			((Control)groupBox2).get_Controls().Add((Control)(object)groupBox4);
			((Control)groupBox2).get_Controls().Add((Control)(object)cmdCopyMags);
			((Control)groupBox2).get_Controls().Add((Control)(object)lblDiff4);
			((Control)groupBox2).get_Controls().Add((Control)(object)lblDiff3);
			((Control)groupBox2).get_Controls().Add((Control)(object)lblDiff1);
			((Control)groupBox2).get_Controls().Add((Control)(object)label4);
			((Control)groupBox2).get_Controls().Add((Control)(object)updnStarMag);
			((Control)groupBox2).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox2).set_Location(new Point(7, 275));
			((Control)groupBox2).set_Name("groupBox2");
			((Control)groupBox2).set_Size(new Size(491, 209));
			((Control)groupBox2).set_TabIndex(2);
			groupBox2.set_TabStop(false);
			((Control)groupBox2).set_Text("4.  Magnitudes from stepped light curve values ");
			((Control)label34).set_AutoSize(true);
			((Control)label34).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label34).set_Location(new Point(78, 151));
			((Control)label34).set_Name("label34");
			((Control)label34).set_Size(new Size(56, 26));
			((Control)label34).set_TabIndex(8);
			((Control)label34).set_Text("Asteroid's\nmagnitude");
			label34.set_TextAlign(ContentAlignment.TopCenter);
			updnAsteroidMag.set_DecimalPlaces(2);
			((Control)updnAsteroidMag).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnAsteroidMag.set_Increment(new decimal(new int[4] { 2, 0, 0, 65536 }));
			((Control)updnAsteroidMag).set_Location(new Point(83, 182));
			updnAsteroidMag.set_Maximum(new decimal(new int[4] { 20, 0, 0, 0 }));
			updnAsteroidMag.set_Minimum(new decimal(new int[4] { 2, 0, 0, -2147483648 }));
			((Control)updnAsteroidMag).set_Name("updnAsteroidMag");
			((Control)updnAsteroidMag).set_Size(new Size(55, 20));
			((Control)updnAsteroidMag).set_TabIndex(9);
			updnAsteroidMag.set_Value(new decimal(new int[4] { 10, 0, 0, 0 }));
			updnAsteroidMag.add_ValueChanged((EventHandler)updnAsteroidMag_ValueChanged);
			((Control)updnAsteroidMag).add_Leave((EventHandler)updnAsteroidMag_Leave);
			((Control)label31).set_AutoSize(true);
			((Control)label31).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label31).set_ForeColor(Color.Red);
			((Control)label31).set_Location(new Point(315, 14));
			((Control)label31).set_Name("label31");
			((Control)label31).set_Size(new Size(161, 13));
			((Control)label31).set_TabIndex(7);
			((Control)label31).set_Text("Assumes linear camera response");
			((Control)groupBox4).get_Controls().Add((Control)(object)lblBaseR);
			((Control)groupBox4).get_Controls().Add((Control)(object)lblBaseD);
			((Control)groupBox4).get_Controls().Add((Control)(object)label33);
			((Control)groupBox4).get_Controls().Add((Control)(object)lblBase);
			((Control)groupBox4).get_Controls().Add((Control)(object)updnMaxR);
			((Control)groupBox4).get_Controls().Add((Control)(object)updnInterR);
			((Control)groupBox4).get_Controls().Add((Control)(object)updnMinR);
			((Control)groupBox4).get_Controls().Add((Control)(object)updnInterD);
			((Control)groupBox4).get_Controls().Add((Control)(object)updnMinD);
			((Control)groupBox4).get_Controls().Add((Control)(object)updnMaxD);
			((Control)groupBox4).get_Controls().Add((Control)(object)label9);
			((Control)groupBox4).get_Controls().Add((Control)(object)label10);
			((Control)groupBox4).get_Controls().Add((Control)(object)label11);
			((Control)groupBox4).get_Controls().Add((Control)(object)label12);
			((Control)groupBox4).get_Controls().Add((Control)(object)label8);
			((Control)groupBox4).get_Controls().Add((Control)(object)label7);
			((Control)groupBox4).get_Controls().Add((Control)(object)label6);
			((Control)groupBox4).get_Controls().Add((Control)(object)label5);
			((Control)groupBox4).set_ForeColor(Color.DarkRed);
			((Control)groupBox4).set_Location(new Point(8, 22));
			((Control)groupBox4).set_Name("groupBox4");
			((Control)groupBox4).set_Size(new Size(471, 126));
			((Control)groupBox4).set_TabIndex(0);
			groupBox4.set_TabStop(false);
			((Control)groupBox4).set_Text("Light curve values");
			((Control)lblBaseR).set_BackColor(Color.LightSalmon);
			lblBaseR.set_BorderStyle((BorderStyle)2);
			((Control)lblBaseR).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblBaseR).set_Location(new Point(370, 103));
			((Control)lblBaseR).set_Name("lblBaseR");
			((Control)lblBaseR).set_Size(new Size(48, 20));
			((Control)lblBaseR).set_TabIndex(20);
			((Control)lblBaseR).set_Text("100");
			lblBaseR.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)lblBaseD).set_BackColor(Color.LightSalmon);
			lblBaseD.set_BorderStyle((BorderStyle)2);
			((Control)lblBaseD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblBaseD).set_Location(new Point(137, 103));
			((Control)lblBaseD).set_Name("lblBaseD");
			((Control)lblBaseD).set_Size(new Size(48, 20));
			((Control)lblBaseD).set_TabIndex(19);
			((Control)lblBaseD).set_Text("100");
			lblBaseD.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label33).set_AutoSize(true);
			((Control)label33).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label33).set_ForeColor(SystemColors.ControlText);
			((Control)label33).set_Location(new Point(305, 106));
			((Control)label33).set_Name("label33");
			((Control)label33).set_Size(new Size(56, 13));
			((Control)label33).set_TabIndex(16);
			((Control)label33).set_Text("Base level");
			((Control)lblBase).set_AutoSize(true);
			((Control)lblBase).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblBase).set_ForeColor(SystemColors.ControlText);
			((Control)lblBase).set_Location(new Point(72, 106));
			((Control)lblBase).set_Name("lblBase");
			((Control)lblBase).set_Size(new Size(56, 13));
			((Control)lblBase).set_TabIndex(15);
			((Control)lblBase).set_Text("Base level");
			((Control)cmdCopyMags).set_BackColor(Color.DeepSkyBlue);
			((Control)cmdCopyMags).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdCopyMags).set_Location(new Point(434, 157));
			((Control)cmdCopyMags).set_Name("cmdCopyMags");
			((Control)cmdCopyMags).set_Size(new Size(46, 24));
			((Control)cmdCopyMags).set_TabIndex(6);
			((Control)cmdCopyMags).set_Text("Copy");
			((ButtonBase)cmdCopyMags).set_UseVisualStyleBackColor(false);
			((Control)cmdCopyMags).add_Click((EventHandler)cmdCopyMags_Click);
			((Control)lblDiff4).set_AutoSize(true);
			((Control)lblDiff4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblDiff4).set_Location(new Point(181, 170));
			((Control)lblDiff4).set_Name("lblDiff4");
			((Control)lblDiff4).set_Size(new Size(102, 13));
			((Control)lblDiff4).set_TabIndex(5);
			((Control)lblDiff4).set_Text("Sequence  A-B-A-B:");
			((Control)lblDiff3).set_AutoSize(true);
			((Control)lblDiff3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblDiff3).set_Location(new Point(181, 151));
			((Control)lblDiff3).set_Name("lblDiff3");
			((Control)lblDiff3).set_Size(new Size(102, 13));
			((Control)lblDiff3).set_TabIndex(4);
			((Control)lblDiff3).set_Text("Sequence  A-B-B-A:");
			((Control)groupBox3).set_BackColor(Color.Honeydew);
			((Control)groupBox3).get_Controls().Add((Control)(object)label49);
			((Control)groupBox3).get_Controls().Add((Control)(object)panel4);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtAxisRatio);
			((Control)groupBox3).get_Controls().Add((Control)(object)cmdCopyMagInten);
			((Control)groupBox3).get_Controls().Add((Control)(object)label16);
			((Control)groupBox3).get_Controls().Add((Control)(object)label17);
			((Control)groupBox3).get_Controls().Add((Control)(object)updndMag);
			((Control)groupBox3).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox3).set_Location(new Point(7, 93));
			((Control)groupBox3).set_Name("groupBox3");
			((Control)groupBox3).set_Size(new Size(491, 59));
			((Control)groupBox3).set_TabIndex(1);
			groupBox3.set_TabStop(false);
			((Control)groupBox3).set_Text("2.  Magnitude change to intensity change");
			((Control)cmdCopyMagInten).set_BackColor(Color.DeepSkyBlue);
			((Control)cmdCopyMagInten).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdCopyMagInten).set_Location(new Point(434, 24));
			((Control)cmdCopyMagInten).set_Name("cmdCopyMagInten");
			((Control)cmdCopyMagInten).set_Size(new Size(46, 24));
			((Control)cmdCopyMagInten).set_TabIndex(2);
			((Control)cmdCopyMagInten).set_Text("Copy");
			((ButtonBase)cmdCopyMagInten).set_UseVisualStyleBackColor(false);
			((Control)cmdCopyMagInten).add_Click((EventHandler)cmdCopyMagInten_Click);
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label16).set_Location(new Point(135, 23));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(56, 26));
			((Control)label16).set_TabIndex(4);
			((Control)label16).set_Text("Brightness\r\nratio");
			label16.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label17).set_Location(new Point(11, 23));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(57, 26));
			((Control)label17).set_TabIndex(3);
			((Control)label17).set_Text("Magnitude\r\nchange");
			label17.set_TextAlign(ContentAlignment.MiddleCenter);
			updndIllum.set_DecimalPlaces(2);
			((Control)updndIllum).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updndIllum.set_Increment(new decimal(new int[4] { 2, 0, 0, 131072 }));
			((Control)updndIllum).set_Location(new Point(5, 3));
			updndIllum.set_Maximum(new decimal(new int[4] { 1000000, 0, 0, 0 }));
			updndIllum.set_Minimum(new decimal(new int[4] { 5, 0, 0, 131072 }));
			((Control)updndIllum).set_Name("updndIllum");
			((Control)updndIllum).set_Size(new Size(58, 20));
			((Control)updndIllum).set_TabIndex(1);
			updndIllum.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updndIllum.add_ValueChanged((EventHandler)updndIllum_ValueChanged);
			updndMag.set_DecimalPlaces(2);
			((Control)updndMag).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updndMag.set_Increment(new decimal(new int[4] { 2, 0, 0, 131072 }));
			((Control)updndMag).set_Location(new Point(70, 26));
			updndMag.set_Maximum(new decimal(new int[4] { 15, 0, 0, 0 }));
			updndMag.set_Minimum(new decimal(new int[4] { 15, 0, 0, -2147483648 }));
			((Control)updndMag).set_Name("updndMag");
			((Control)updndMag).set_Size(new Size(51, 20));
			((Control)updndMag).set_TabIndex(0);
			updndMag.add_ValueChanged((EventHandler)updndMag_ValueChanged);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(505, 24));
			((Control)menuStrip1).set_TabIndex(3);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)groupBox6).set_BackColor(Color.Lavender);
			((Control)groupBox6).get_Controls().Add((Control)(object)cmdCopyFromLevels);
			((Control)groupBox6).get_Controls().Add((Control)(object)lblBaseRef);
			((Control)groupBox6).get_Controls().Add((Control)(object)label32);
			((Control)groupBox6).get_Controls().Add((Control)(object)label30);
			((Control)groupBox6).get_Controls().Add((Control)(object)txtTargetMag);
			((Control)groupBox6).get_Controls().Add((Control)(object)label29);
			((Control)groupBox6).get_Controls().Add((Control)(object)updnFaint);
			((Control)groupBox6).get_Controls().Add((Control)(object)updnTarget);
			((Control)groupBox6).get_Controls().Add((Control)(object)updnBright);
			((Control)groupBox6).get_Controls().Add((Control)(object)label28);
			((Control)groupBox6).get_Controls().Add((Control)(object)label27);
			((Control)groupBox6).get_Controls().Add((Control)(object)updnMagFaint);
			((Control)groupBox6).get_Controls().Add((Control)(object)updnMagBright);
			((Control)groupBox6).get_Controls().Add((Control)(object)label26);
			((Control)groupBox6).get_Controls().Add((Control)(object)label25);
			((Control)groupBox6).get_Controls().Add((Control)(object)label24);
			((Control)groupBox6).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox6).set_Location(new Point(7, 159));
			((Control)groupBox6).set_Name("groupBox6");
			((Control)groupBox6).set_Size(new Size(491, 109));
			((Control)groupBox6).set_TabIndex(5);
			groupBox6.set_TabStop(false);
			((Control)groupBox6).set_Text("3. Magnitude from light levels, using two reference magnitudes");
			((Control)cmdCopyFromLevels).set_BackColor(Color.DeepSkyBlue);
			((Control)cmdCopyFromLevels).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdCopyFromLevels).set_Location(new Point(439, 58));
			((Control)cmdCopyFromLevels).set_Name("cmdCopyFromLevels");
			((Control)cmdCopyFromLevels).set_Size(new Size(46, 24));
			((Control)cmdCopyFromLevels).set_TabIndex(22);
			((Control)cmdCopyFromLevels).set_Text("Copy");
			((ButtonBase)cmdCopyFromLevels).set_UseVisualStyleBackColor(false);
			((Control)cmdCopyFromLevels).add_Click((EventHandler)cmdCopyFromLevels_Click);
			((Control)lblBaseRef).set_BackColor(Color.LightSalmon);
			lblBaseRef.set_BorderStyle((BorderStyle)2);
			((Control)lblBaseRef).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblBaseRef).set_Location(new Point(379, 82));
			((Control)lblBaseRef).set_Name("lblBaseRef");
			((Control)lblBaseRef).set_Size(new Size(43, 20));
			((Control)lblBaseRef).set_TabIndex(21);
			((Control)lblBaseRef).set_Text("100");
			lblBaseRef.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label32).set_AutoSize(true);
			((Control)label32).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label32).set_ForeColor(Color.Red);
			((Control)label32).set_Location(new Point(294, 18));
			((Control)label32).set_Name("label32");
			((Control)label32).set_Size(new Size(161, 13));
			((Control)label32).set_TabIndex(15);
			((Control)label32).set_Text("Assumes linear camera response");
			((Control)label30).set_AutoSize(true);
			((Control)label30).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label30).set_Location(new Point(321, 86));
			((Control)label30).set_Name("label30");
			((Control)label30).set_Size(new Size(56, 13));
			((Control)label30).set_TabIndex(13);
			((Control)label30).set_Text("Base level");
			((Control)txtTargetMag).set_BackColor(Color.LightGreen);
			((Control)txtTargetMag).set_Location(new Point(379, 37));
			((Control)txtTargetMag).set_Name("txtTargetMag");
			((Control)txtTargetMag).set_Size(new Size(43, 23));
			((Control)txtTargetMag).set_TabIndex(12);
			((Control)label29).set_AutoSize(true);
			((Control)label29).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label29).set_Location(new Point(286, 41));
			((Control)label29).set_Name("label29");
			((Control)label29).set_Size(new Size(91, 13));
			((Control)label29).set_TabIndex(10);
			((Control)label29).set_Text("Target Magnitude");
			((Control)updnFaint).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnFaint.set_Increment(new decimal(new int[4] { 10, 0, 0, 0 }));
			((Control)updnFaint).set_Location(new Point(209, 82));
			updnFaint.set_Maximum(new decimal(new int[4] { 100000, 0, 0, 0 }));
			updnFaint.set_Minimum(new decimal(new int[4] { 1000, 0, 0, -2147483648 }));
			((Control)updnFaint).set_Name("updnFaint");
			((Control)updnFaint).set_Size(new Size(66, 20));
			((Control)updnFaint).set_TabIndex(9);
			updnFaint.set_Value(new decimal(new int[4] { 1584, 0, 0, 0 }));
			updnFaint.add_ValueChanged((EventHandler)updnFaint_ValueChanged);
			((Control)updnFaint).add_Leave((EventHandler)updnFaint_Leave);
			((Control)updnTarget).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnTarget.set_Increment(new decimal(new int[4] { 10, 0, 0, 0 }));
			((Control)updnTarget).set_Location(new Point(210, 59));
			updnTarget.set_Maximum(new decimal(new int[4] { 100000, 0, 0, 0 }));
			updnTarget.set_Minimum(new decimal(new int[4] { 1000, 0, 0, -2147483648 }));
			((Control)updnTarget).set_Name("updnTarget");
			((Control)updnTarget).set_Size(new Size(66, 20));
			((Control)updnTarget).set_TabIndex(8);
			updnTarget.set_Value(new decimal(new int[4] { 3980, 0, 0, 0 }));
			updnTarget.add_ValueChanged((EventHandler)updnTarget_ValueChanged);
			((Control)updnTarget).add_Leave((EventHandler)updnTarget_Leave);
			((Control)updnBright).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnBright.set_Increment(new decimal(new int[4] { 10, 0, 0, 0 }));
			((Control)updnBright).set_Location(new Point(210, 36));
			updnBright.set_Maximum(new decimal(new int[4] { 100000, 0, 0, 0 }));
			updnBright.set_Minimum(new decimal(new int[4] { 1000, 0, 0, -2147483648 }));
			((Control)updnBright).set_Name("updnBright");
			((Control)updnBright).set_Size(new Size(66, 20));
			((Control)updnBright).set_TabIndex(7);
			updnBright.set_Value(new decimal(new int[4] { 10000, 0, 0, 0 }));
			updnBright.add_ValueChanged((EventHandler)updnBright_ValueChanged);
			((Control)updnBright).add_Leave((EventHandler)updnBright_Leave);
			((Control)label28).set_AutoSize(true);
			((Control)label28).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label28).set_Location(new Point(206, 22));
			((Control)label28).set_Name("label28");
			((Control)label28).set_Size(new Size(55, 13));
			((Control)label28).set_TabIndex(6);
			((Control)label28).set_Text("Light level");
			((Control)label27).set_AutoSize(true);
			((Control)label27).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label27).set_Location(new Point(130, 22));
			((Control)label27).set_Name("label27");
			((Control)label27).set_Size(new Size(57, 13));
			((Control)label27).set_TabIndex(5);
			((Control)label27).set_Text("Magnitude");
			updnMagFaint.set_DecimalPlaces(2);
			((Control)updnMagFaint).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnMagFaint.set_Increment(new decimal(new int[4] { 2, 0, 0, 65536 }));
			((Control)updnMagFaint).set_Location(new Point(130, 82));
			updnMagFaint.set_Maximum(new decimal(new int[4] { 20, 0, 0, 0 }));
			updnMagFaint.set_Minimum(new decimal(new int[4] { 2, 0, 0, -2147483648 }));
			((Control)updnMagFaint).set_Name("updnMagFaint");
			((Control)updnMagFaint).set_Size(new Size(55, 20));
			((Control)updnMagFaint).set_TabIndex(4);
			updnMagFaint.set_Value(new decimal(new int[4] { 10, 0, 0, 0 }));
			updnMagFaint.add_ValueChanged((EventHandler)updnMagFaint_ValueChanged);
			((Control)updnMagFaint).add_Leave((EventHandler)updnMagFaint_Leave);
			updnMagBright.set_DecimalPlaces(2);
			((Control)updnMagBright).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnMagBright.set_Increment(new decimal(new int[4] { 2, 0, 0, 65536 }));
			((Control)updnMagBright).set_Location(new Point(130, 36));
			updnMagBright.set_Maximum(new decimal(new int[4] { 20, 0, 0, 0 }));
			updnMagBright.set_Minimum(new decimal(new int[4] { 2, 0, 0, -2147483648 }));
			((Control)updnMagBright).set_Name("updnMagBright");
			((Control)updnMagBright).set_Size(new Size(55, 20));
			((Control)updnMagBright).set_TabIndex(3);
			updnMagBright.set_Value(new decimal(new int[4] { 8, 0, 0, 0 }));
			updnMagBright.add_ValueChanged((EventHandler)updnMagBright_ValueChanged);
			((Control)updnMagBright).add_Leave((EventHandler)updnMagBright_Leave);
			((Control)label26).set_AutoSize(true);
			((Control)label26).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label26).set_Location(new Point(11, 63));
			((Control)label26).set_Name("label26");
			((Control)label26).set_Size(new Size(95, 13));
			((Control)label26).set_TabIndex(2);
			((Control)label26).set_Text("Intermediate target");
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label25).set_Location(new Point(11, 86));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(98, 13));
			((Control)label25).set_TabIndex(1);
			((Control)label25).set_Text("Faint reference star");
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label24).set_Location(new Point(10, 40));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(102, 13));
			((Control)label24).set_TabIndex(0);
			((Control)label24).set_Text("Bright reference star");
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_BackColor(Color.PaleTurquoise);
			((Control)label15).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label15).set_ForeColor(Color.DarkRed);
			((Control)label15).set_Location(new Point(228, 7));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(270, 13));
			((Control)label15).set_TabIndex(8);
			((Control)label15).set_Text("If values are changed by keying, press Enter to compute");
			((Control)groupBox5).set_BackColor(Color.MistyRose);
			((Control)groupBox5).get_Controls().Add((Control)(object)cmdCopyMagDia);
			((Control)groupBox5).get_Controls().Add((Control)(object)updnAlbedo);
			((Control)groupBox5).get_Controls().Add((Control)(object)label18);
			((Control)groupBox5).get_Controls().Add((Control)(object)txtDia);
			((Control)groupBox5).get_Controls().Add((Control)(object)label14);
			((Control)groupBox5).get_Controls().Add((Control)(object)label13);
			((Control)groupBox5).get_Controls().Add((Control)(object)updnH0);
			((Control)groupBox5).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox5).set_Location(new Point(7, 650));
			((Control)groupBox5).set_Name("groupBox5");
			((Control)groupBox5).set_Size(new Size(491, 51));
			((Control)groupBox5).set_TabIndex(9);
			groupBox5.set_TabStop(false);
			((Control)groupBox5).set_Text("6. Asteroid diameter from Absolute magnitude (H0)");
			((Control)cmdCopyMagDia).set_BackColor(Color.DeepSkyBlue);
			((Control)cmdCopyMagDia).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdCopyMagDia).set_Location(new Point(434, 21));
			((Control)cmdCopyMagDia).set_Name("cmdCopyMagDia");
			((Control)cmdCopyMagDia).set_Size(new Size(46, 24));
			((Control)cmdCopyMagDia).set_TabIndex(28);
			((Control)cmdCopyMagDia).set_Text("Copy");
			((ButtonBase)cmdCopyMagDia).set_UseVisualStyleBackColor(false);
			((Control)cmdCopyMagDia).add_Click((EventHandler)cmdCopyMagDia_Click);
			updnAlbedo.set_DecimalPlaces(2);
			((Control)updnAlbedo).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnAlbedo.set_Increment(new decimal(new int[4] { 5, 0, 0, 131072 }));
			((Control)updnAlbedo).set_Location(new Point(235, 24));
			updnAlbedo.set_Maximum(new decimal(new int[4] { 30, 0, 0, 131072 }));
			updnAlbedo.set_Minimum(new decimal(new int[4] { 5, 0, 0, 131072 }));
			((Control)updnAlbedo).set_Name("updnAlbedo");
			((Control)updnAlbedo).set_Size(new Size(45, 20));
			((Control)updnAlbedo).set_TabIndex(9);
			updnAlbedo.set_Value(new decimal(new int[4] { 15, 0, 0, 131072 }));
			updnAlbedo.add_ValueChanged((EventHandler)updnAlbedo_ValueChanged);
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label18).set_Location(new Point(195, 28));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(40, 13));
			((Control)label18).set_TabIndex(8);
			((Control)label18).set_Text("Albedo");
			((Control)txtDia).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtDia).set_Location(new Point(367, 24));
			((Control)txtDia).set_Name("txtDia");
			((Control)txtDia).set_Size(new Size(47, 20));
			((Control)txtDia).set_TabIndex(7);
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label14).set_Location(new Point(295, 28));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(72, 13));
			((Control)label14).set_TabIndex(6);
			((Control)label14).set_Text("Diameter (km)");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label13).set_Location(new Point(10, 28));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(120, 13));
			((Control)label13).set_TabIndex(5);
			((Control)label13).set_Text("Absolute magnitude  H0");
			updnH0.set_DecimalPlaces(2);
			((Control)updnH0).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnH0.set_Increment(new decimal(new int[4] { 5, 0, 0, 131072 }));
			((Control)updnH0).set_Location(new Point(132, 24));
			updnH0.set_Maximum(new decimal(new int[4] { 25, 0, 0, 0 }));
			updnH0.set_Minimum(new decimal(new int[4] { 5, 0, 0, 0 }));
			((Control)updnH0).set_Name("updnH0");
			((Control)updnH0).set_Size(new Size(51, 20));
			((Control)updnH0).set_TabIndex(4);
			updnH0.set_Value(new decimal(new int[4] { 10, 0, 0, 0 }));
			updnH0.add_ValueChanged((EventHandler)updnH0_ValueChanged);
			((Control)groupBox7).set_BackColor(Color.PaleTurquoise);
			((Control)groupBox7).get_Controls().Add((Control)(object)label48);
			((Control)groupBox7).get_Controls().Add((Control)(object)panel3);
			((Control)groupBox7).get_Controls().Add((Control)(object)panel2);
			((Control)groupBox7).get_Controls().Add((Control)(object)panel1);
			((Control)groupBox7).get_Controls().Add((Control)(object)label43);
			((Control)groupBox7).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox7).set_Location(new Point(8, 491));
			((Control)groupBox7).set_Name("groupBox7");
			((Control)groupBox7).set_Size(new Size(489, 152));
			((Control)groupBox7).set_TabIndex(10);
			groupBox7.set_TabStop(false);
			((Control)groupBox7).set_Text("5. Intermediate magnitudes from a light curve plot");
			((Control)label48).set_AutoSize(true);
			((Control)label48).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label48).set_ForeColor(Color.DarkBlue);
			((Control)label48).set_Location(new Point(0, 19));
			((Control)label48).set_Name("label48");
			((Control)label48).set_Size(new Size(165, 13));
			((Control)label48).set_TabIndex(32);
			((Control)label48).set_Text("Set target and limiting magnitudes");
			((Control)panel3).set_BackColor(Color.Moccasin);
			panel3.set_BorderStyle((BorderStyle)2);
			((Control)panel3).get_Controls().Add((Control)(object)label19);
			((Control)panel3).get_Controls().Add((Control)(object)updnLimitingMag);
			((Control)panel3).get_Controls().Add((Control)(object)updnTargetMag);
			((Control)panel3).get_Controls().Add((Control)(object)label20);
			((Control)panel3).set_Location(new Point(4, 35));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(158, 76));
			((Control)panel3).set_TabIndex(31);
			toolTip1.SetToolTip((Control)(object)panel3, componentResourceManager.GetString("panel3.ToolTip"));
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label19).set_ForeColor(Color.DarkRed);
			((Control)label19).set_Location(new Point(9, 5));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(90, 26));
			((Control)label19).set_TabIndex(2);
			((Control)label19).set_Text("Target magnitude\r\n( full light )");
			label19.set_TextAlign(ContentAlignment.MiddleCenter);
			updnLimitingMag.set_DecimalPlaces(2);
			((Control)updnLimitingMag).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnLimitingMag.set_Increment(new decimal(new int[4] { 5, 0, 0, 131072 }));
			((Control)updnLimitingMag).set_Location(new Point(101, 46));
			updnLimitingMag.set_Maximum(new decimal(new int[4] { 20, 0, 0, 0 }));
			((Control)updnLimitingMag).set_Name("updnLimitingMag");
			((Control)updnLimitingMag).set_Size(new Size(49, 20));
			((Control)updnLimitingMag).set_TabIndex(1);
			toolTip1.SetToolTip((Control)(object)updnLimitingMag, componentResourceManager.GetString("updnLimitingMag.ToolTip"));
			updnLimitingMag.set_Value(new decimal(new int[4] { 14, 0, 0, 0 }));
			updnLimitingMag.add_ValueChanged((EventHandler)updnLimitingMag_ValueChanged);
			updnTargetMag.set_DecimalPlaces(2);
			((Control)updnTargetMag).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnTargetMag.set_Increment(new decimal(new int[4] { 5, 0, 0, 131072 }));
			((Control)updnTargetMag).set_Location(new Point(101, 9));
			updnTargetMag.set_Maximum(new decimal(new int[4] { 20, 0, 0, 0 }));
			((Control)updnTargetMag).set_Name("updnTargetMag");
			((Control)updnTargetMag).set_Size(new Size(49, 20));
			((Control)updnTargetMag).set_TabIndex(0);
			updnTargetMag.set_Value(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnTargetMag.add_ValueChanged((EventHandler)updnTargetMag_ValueChanged);
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label20).set_ForeColor(Color.DarkRed);
			((Control)label20).set_Location(new Point(0, 41));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(102, 26));
			((Control)label20).set_TabIndex(3);
			((Control)label20).set_Text("Mag of faintest stars\r\n(Limiting magnitude)");
			label20.set_TextAlign(ContentAlignment.MiddleCenter);
			toolTip1.SetToolTip((Control)(object)label20, componentResourceManager.GetString("label20.ToolTip"));
			((Control)panel2).set_BackColor(Color.PeachPuff);
			panel2.set_BorderStyle((BorderStyle)2);
			((Control)panel2).get_Controls().Add((Control)(object)cmdCopyIntermediateMagnitudes);
			((Control)panel2).get_Controls().Add((Control)(object)label42);
			((Control)panel2).get_Controls().Add((Control)(object)label41);
			((Control)panel2).get_Controls().Add((Control)(object)label40);
			((Control)panel2).get_Controls().Add((Control)(object)label39);
			((Control)panel2).get_Controls().Add((Control)(object)label38);
			((Control)panel2).get_Controls().Add((Control)(object)label37);
			((Control)panel2).get_Controls().Add((Control)(object)label36);
			((Control)panel2).get_Controls().Add((Control)(object)label35);
			((Control)panel2).get_Controls().Add((Control)(object)label23);
			((Control)panel2).get_Controls().Add((Control)(object)label22);
			((Control)panel2).get_Controls().Add((Control)(object)label21);
			((Control)panel2).get_Controls().Add((Control)(object)txt0);
			((Control)panel2).get_Controls().Add((Control)(object)txt10);
			((Control)panel2).get_Controls().Add((Control)(object)txt20);
			((Control)panel2).get_Controls().Add((Control)(object)txt30);
			((Control)panel2).get_Controls().Add((Control)(object)txt40);
			((Control)panel2).get_Controls().Add((Control)(object)txt50);
			((Control)panel2).get_Controls().Add((Control)(object)txt60);
			((Control)panel2).get_Controls().Add((Control)(object)txt70);
			((Control)panel2).get_Controls().Add((Control)(object)txt80);
			((Control)panel2).get_Controls().Add((Control)(object)txt90);
			((Control)panel2).get_Controls().Add((Control)(object)txt100);
			((Control)panel2).set_Location(new Point(167, 35));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(317, 76));
			((Control)panel2).set_TabIndex(30);
			((Control)cmdCopyIntermediateMagnitudes).set_BackColor(Color.DeepSkyBlue);
			((Control)cmdCopyIntermediateMagnitudes).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdCopyIntermediateMagnitudes).set_Location(new Point(264, 46));
			((Control)cmdCopyIntermediateMagnitudes).set_Name("cmdCopyIntermediateMagnitudes");
			((Control)cmdCopyIntermediateMagnitudes).set_Size(new Size(46, 24));
			((Control)cmdCopyIntermediateMagnitudes).set_TabIndex(27);
			((Control)cmdCopyIntermediateMagnitudes).set_Text("Copy");
			((ButtonBase)cmdCopyIntermediateMagnitudes).set_UseVisualStyleBackColor(false);
			((Control)cmdCopyIntermediateMagnitudes).add_Click((EventHandler)cmdCopyIntermediateMagnitudes_Click);
			((Control)label42).set_AutoSize(true);
			((Control)label42).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label42).set_Location(new Point(223, 37));
			((Control)label42).set_Name("label42");
			((Control)label42).set_Size(new Size(27, 13));
			((Control)label42).set_TabIndex(26);
			((Control)label42).set_Text("00%");
			((Control)label41).set_AutoSize(true);
			((Control)label41).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label41).set_Location(new Point(8, 1));
			((Control)label41).set_Name("label41");
			((Control)label41).set_Size(new Size(33, 13));
			((Control)label41).set_TabIndex(25);
			((Control)label41).set_Text("100%");
			((Control)label40).set_AutoSize(true);
			((Control)label40).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label40).set_Location(new Point(170, 37));
			((Control)label40).set_Name("label40");
			((Control)label40).set_Size(new Size(27, 13));
			((Control)label40).set_TabIndex(24);
			((Control)label40).set_Text("10%");
			((Control)label39).set_AutoSize(true);
			((Control)label39).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label39).set_Location(new Point(276, 0));
			((Control)label39).set_Name("label39");
			((Control)label39).set_Size(new Size(27, 13));
			((Control)label39).set_TabIndex(23);
			((Control)label39).set_Text("50%");
			((Control)label38).set_AutoSize(true);
			((Control)label38).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label38).set_Location(new Point(223, 0));
			((Control)label38).set_Name("label38");
			((Control)label38).set_Size(new Size(27, 13));
			((Control)label38).set_TabIndex(22);
			((Control)label38).set_Text("60%");
			((Control)label37).set_AutoSize(true);
			((Control)label37).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label37).set_Location(new Point(170, 0));
			((Control)label37).set_Name("label37");
			((Control)label37).set_Size(new Size(27, 13));
			((Control)label37).set_TabIndex(21);
			((Control)label37).set_Text("70%");
			((Control)label36).set_AutoSize(true);
			((Control)label36).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label36).set_Location(new Point(117, 0));
			((Control)label36).set_Name("label36");
			((Control)label36).set_Size(new Size(27, 13));
			((Control)label36).set_TabIndex(20);
			((Control)label36).set_Text("80%");
			((Control)label35).set_AutoSize(true);
			((Control)label35).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label35).set_Location(new Point(64, 37));
			((Control)label35).set_Name("label35");
			((Control)label35).set_Size(new Size(27, 13));
			((Control)label35).set_TabIndex(19);
			((Control)label35).set_Text("30%");
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label23).set_Location(new Point(117, 37));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(27, 13));
			((Control)label23).set_TabIndex(18);
			((Control)label23).set_Text("20%");
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label22).set_Location(new Point(11, 37));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(27, 13));
			((Control)label22).set_TabIndex(17);
			((Control)label22).set_Text("40%");
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label21).set_Location(new Point(64, 0));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(27, 13));
			((Control)label21).set_TabIndex(16);
			((Control)label21).set_Text("90%");
			((Control)txt0).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txt0).set_Location(new Point(218, 51));
			((Control)txt0).set_Name("txt0");
			((TextBoxBase)txt0).set_ReadOnly(true);
			((Control)txt0).set_Size(new Size(37, 20));
			((Control)txt0).set_TabIndex(14);
			((Control)txt0).set_Text("---");
			txt0.set_TextAlign((HorizontalAlignment)2);
			((Control)txt10).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txt10).set_Location(new Point(165, 51));
			((Control)txt10).set_Name("txt10");
			((TextBoxBase)txt10).set_ReadOnly(true);
			((Control)txt10).set_Size(new Size(37, 20));
			((Control)txt10).set_TabIndex(13);
			((Control)txt10).set_Text("---");
			txt10.set_TextAlign((HorizontalAlignment)2);
			((Control)txt20).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txt20).set_Location(new Point(112, 51));
			((Control)txt20).set_Name("txt20");
			((TextBoxBase)txt20).set_ReadOnly(true);
			((Control)txt20).set_Size(new Size(37, 20));
			((Control)txt20).set_TabIndex(12);
			((Control)txt20).set_Text("---");
			txt20.set_TextAlign((HorizontalAlignment)2);
			((Control)txt30).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txt30).set_Location(new Point(59, 51));
			((Control)txt30).set_Name("txt30");
			((TextBoxBase)txt30).set_ReadOnly(true);
			((Control)txt30).set_Size(new Size(37, 20));
			((Control)txt30).set_TabIndex(11);
			((Control)txt30).set_Text("---");
			txt30.set_TextAlign((HorizontalAlignment)2);
			((Control)txt40).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txt40).set_Location(new Point(6, 51));
			((Control)txt40).set_Name("txt40");
			((TextBoxBase)txt40).set_ReadOnly(true);
			((Control)txt40).set_Size(new Size(37, 20));
			((Control)txt40).set_TabIndex(10);
			((Control)txt40).set_Text("---");
			txt40.set_TextAlign((HorizontalAlignment)2);
			((Control)txt50).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txt50).set_Location(new Point(271, 14));
			((Control)txt50).set_Name("txt50");
			((TextBoxBase)txt50).set_ReadOnly(true);
			((Control)txt50).set_Size(new Size(37, 20));
			((Control)txt50).set_TabIndex(9);
			((Control)txt50).set_Text("---");
			txt50.set_TextAlign((HorizontalAlignment)2);
			((Control)txt60).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txt60).set_Location(new Point(218, 14));
			((Control)txt60).set_Name("txt60");
			((TextBoxBase)txt60).set_ReadOnly(true);
			((Control)txt60).set_Size(new Size(37, 20));
			((Control)txt60).set_TabIndex(8);
			((Control)txt60).set_Text("---");
			txt60.set_TextAlign((HorizontalAlignment)2);
			((Control)txt70).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txt70).set_Location(new Point(165, 14));
			((Control)txt70).set_Name("txt70");
			((TextBoxBase)txt70).set_ReadOnly(true);
			((Control)txt70).set_Size(new Size(37, 20));
			((Control)txt70).set_TabIndex(7);
			((Control)txt70).set_Text("---");
			txt70.set_TextAlign((HorizontalAlignment)2);
			((Control)txt80).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txt80).set_Location(new Point(112, 14));
			((Control)txt80).set_Name("txt80");
			((TextBoxBase)txt80).set_ReadOnly(true);
			((Control)txt80).set_Size(new Size(37, 20));
			((Control)txt80).set_TabIndex(6);
			((Control)txt80).set_Text("---");
			txt80.set_TextAlign((HorizontalAlignment)2);
			((Control)txt90).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txt90).set_Location(new Point(59, 14));
			((Control)txt90).set_Name("txt90");
			((TextBoxBase)txt90).set_ReadOnly(true);
			((Control)txt90).set_Size(new Size(37, 20));
			((Control)txt90).set_TabIndex(5);
			((Control)txt90).set_Text("---");
			txt90.set_TextAlign((HorizontalAlignment)2);
			((Control)txt100).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txt100).set_Location(new Point(6, 14));
			((Control)txt100).set_Name("txt100");
			((TextBoxBase)txt100).set_ReadOnly(true);
			((Control)txt100).set_Size(new Size(37, 20));
			((Control)txt100).set_TabIndex(4);
			((Control)txt100).set_Text("---");
			txt100.set_TextAlign((HorizontalAlignment)2);
			((Control)panel1).set_BackColor(Color.PaleGreen);
			panel1.set_BorderStyle((BorderStyle)2);
			((Control)panel1).get_Controls().Add((Control)(object)chkSatelliteLevels);
			((Control)panel1).get_Controls().Add((Control)(object)label44);
			((Control)panel1).get_Controls().Add((Control)(object)txtMagDropLessThan);
			((Control)panel1).get_Controls().Add((Control)(object)txtMagDropGreaterThan);
			((Control)panel1).get_Controls().Add((Control)(object)label47);
			((Control)panel1).get_Controls().Add((Control)(object)label46);
			((Control)panel1).get_Controls().Add((Control)(object)label45);
			((Control)panel1).get_Controls().Add((Control)(object)updnPredMagDrop);
			((Control)panel1).set_Location(new Point(4, 114));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(480, 34));
			((Control)panel1).set_TabIndex(29);
			toolTip1.SetToolTip((Control)(object)panel1, componentResourceManager.GetString("panel1.ToolTip"));
			((Control)chkSatelliteLevels).set_AutoSize(true);
			((Control)chkSatelliteLevels).set_BackColor(Color.BlanchedAlmond);
			((Control)chkSatelliteLevels).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkSatelliteLevels).set_ForeColor(Color.DarkRed);
			((Control)chkSatelliteLevels).set_Location(new Point(69, 0));
			((Control)chkSatelliteLevels).set_Name("chkSatelliteLevels");
			((Control)chkSatelliteLevels).set_Size(new Size(59, 30));
			((Control)chkSatelliteLevels).set_TabIndex(35);
			((Control)chkSatelliteLevels).set_Text("Show\r\nlevels");
			toolTip1.SetToolTip((Control)(object)chkSatelliteLevels, componentResourceManager.GetString("chkSatelliteLevels.ToolTip"));
			((ButtonBase)chkSatelliteLevels).set_UseVisualStyleBackColor(false);
			chkSatelliteLevels.add_CheckedChanged((EventHandler)chkSatelliteLevels_CheckedChanged);
			((Control)label44).set_AutoSize(true);
			((Control)label44).set_BackColor(Color.BlanchedAlmond);
			((Control)label44).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label44).set_ForeColor(Color.DarkGreen);
			((Control)label44).set_Location(new Point(1, -11));
			((Control)label44).set_Name("label44");
			((Control)label44).set_Size(new Size(76, 52));
			((Control)label44).set_TabIndex(35);
			((Control)label44).set_Text(" \r\nSatellite\r\ndetection    \r\n ");
			label44.set_TextAlign(ContentAlignment.BottomLeft);
			toolTip1.SetToolTip((Control)(object)label44, componentResourceManager.GetString("label44.ToolTip"));
			((Control)txtMagDropLessThan).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMagDropLessThan).set_Location(new Point(427, 5));
			((Control)txtMagDropLessThan).set_Name("txtMagDropLessThan");
			((TextBoxBase)txtMagDropLessThan).set_ReadOnly(true);
			((Control)txtMagDropLessThan).set_Size(new Size(37, 20));
			((Control)txtMagDropLessThan).set_TabIndex(34);
			((Control)txtMagDropLessThan).set_Text("---");
			txtMagDropLessThan.set_TextAlign((HorizontalAlignment)2);
			((Control)txtMagDropGreaterThan).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMagDropGreaterThan).set_Location(new Point(328, 5));
			((Control)txtMagDropGreaterThan).set_Name("txtMagDropGreaterThan");
			((TextBoxBase)txtMagDropGreaterThan).set_ReadOnly(true);
			((Control)txtMagDropGreaterThan).set_Size(new Size(37, 20));
			((Control)txtMagDropGreaterThan).set_TabIndex(33);
			((Control)txtMagDropGreaterThan).set_Text("---");
			txtMagDropGreaterThan.set_TextAlign((HorizontalAlignment)2);
			toolTip1.SetToolTip((Control)(object)txtMagDropGreaterThan, componentResourceManager.GetString("txtMagDropGreaterThan.ToolTip"));
			((Control)label47).set_AutoSize(true);
			((Control)label47).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label47).set_Location(new Point(381, 2));
			((Control)label47).set_Name("label47");
			((Control)label47).set_Size(new Size(46, 26));
			((Control)label47).set_TabIndex(32);
			((Control)label47).set_Text("and less\r\nthan");
			((Control)label46).set_AutoSize(true);
			((Control)label46).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label46).set_Location(new Point(249, 2));
			((Control)label46).set_Name("label46");
			((Control)label46).set_Size(new Size(79, 26));
			((Control)label46).set_TabIndex(31);
			((Control)label46).set_Text("Mag drop must\r\nbe greater than");
			toolTip1.SetToolTip((Control)(object)label46, componentResourceManager.GetString("label46.ToolTip"));
			((Control)label45).set_AutoSize(true);
			((Control)label45).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label45).set_Location(new Point(136, 2));
			((Control)label45).set_Name("label45");
			((Control)label45).set_Size(new Size(52, 26));
			((Control)label45).set_TabIndex(30);
			((Control)label45).set_Text("Predicted\r\nMag drop");
			updnPredMagDrop.set_DecimalPlaces(2);
			((Control)updnPredMagDrop).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnPredMagDrop.set_Increment(new decimal(new int[4] { 5, 0, 0, 131072 }));
			((Control)updnPredMagDrop).set_Location(new Point(188, 5));
			updnPredMagDrop.set_Maximum(new decimal(new int[4] { 9, 0, 0, 0 }));
			updnPredMagDrop.set_Minimum(new decimal(new int[4] { 5, 0, 0, 131072 }));
			((Control)updnPredMagDrop).set_Name("updnPredMagDrop");
			((Control)updnPredMagDrop).set_Size(new Size(45, 20));
			((Control)updnPredMagDrop).set_TabIndex(28);
			updnPredMagDrop.set_Value(new decimal(new int[4] { 4, 0, 0, 0 }));
			updnPredMagDrop.add_ValueChanged((EventHandler)updnPredMagDrop_ValueChanged);
			((Control)label43).set_AutoSize(true);
			((Control)label43).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label43).set_ForeColor(Color.DarkBlue);
			((Control)label43).set_Location(new Point(229, 19));
			((Control)label43).set_Name("label43");
			((Control)label43).set_Size(new Size(192, 13));
			((Control)label43).set_TabIndex(27);
			((Control)label43).set_Text("Magnitude at different light curve levels");
			toolTip1.set_AutoPopDelay(15000);
			toolTip1.set_BackColor(Color.NavajoWhite);
			toolTip1.set_InitialDelay(50);
			toolTip1.set_IsBalloon(true);
			toolTip1.set_OwnerDraw(true);
			toolTip1.set_ReshowDelay(50);
			toolTip1.set_ShowAlways(true);
			toolTip1.set_ToolTipIcon((ToolTipIcon)1);
			((Control)txtInverseBright).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtInverseBright).set_Location(new Point(74, 2));
			((Control)txtInverseBright).set_Name("txtInverseBright");
			((TextBoxBase)txtInverseBright).set_ReadOnly(true);
			((Control)txtInverseBright).set_Size(new Size(40, 20));
			((Control)txtInverseBright).set_TabIndex(6);
			((Control)txtInverseBright).set_Text("1.00");
			((Control)txtAxisRatio).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAxisRatio).set_Location(new Point(375, 26));
			((Control)txtAxisRatio).set_Name("txtAxisRatio");
			((TextBoxBase)txtAxisRatio).set_ReadOnly(true);
			((Control)txtAxisRatio).set_Size(new Size(40, 20));
			((Control)txtAxisRatio).set_TabIndex(7);
			((Control)txtAxisRatio).set_Text("1.00");
			panel4.set_BorderStyle((BorderStyle)2);
			((Control)panel4).get_Controls().Add((Control)(object)txtInverseBright);
			((Control)panel4).get_Controls().Add((Control)(object)updndIllum);
			((Control)panel4).set_Location(new Point(192, 22));
			((Control)panel4).set_Name("panel4");
			((Control)panel4).set_Size(new Size(123, 28));
			((Control)panel4).set_TabIndex(8);
			((Control)label49).set_AutoSize(true);
			((Control)label49).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label49).set_Location(new Point(324, 23));
			((Control)label49).set_Name("label49");
			((Control)label49).set_Size(new Size(49, 26));
			((Control)label49).set_TabIndex(9);
			((Control)label49).set_Text("Asteroid\r\nAxis ratio");
			label49.set_TextAlign(ContentAlignment.MiddleCenter);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(505, 705));
			((Control)this).get_Controls().Add((Control)(object)groupBox7);
			((Control)this).get_Controls().Add((Control)(object)groupBox5);
			((Control)this).get_Controls().Add((Control)(object)label15);
			((Control)this).get_Controls().Add((Control)(object)groupBox6);
			((Control)this).get_Controls().Add((Control)(object)groupBox3);
			((Control)this).get_Controls().Add((Control)(object)groupBox2);
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationEphemMag", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Location(Settings.Default.LocationEphemMag);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Control)this).set_Name("Magnitudes");
			((Control)this).set_Text("Magnitude Calculator");
			((Form)this).add_Load((EventHandler)Magnitudes_Load);
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((ISupportInitialize)updnM2).EndInit();
			((ISupportInitialize)updnM1).EndInit();
			((ISupportInitialize)updnStarMag).EndInit();
			((ISupportInitialize)updnMaxD).EndInit();
			((ISupportInitialize)updnMinD).EndInit();
			((ISupportInitialize)updnInterD).EndInit();
			((ISupportInitialize)updnMinR).EndInit();
			((ISupportInitialize)updnInterR).EndInit();
			((ISupportInitialize)updnMaxR).EndInit();
			((Control)groupBox2).ResumeLayout(false);
			((Control)groupBox2).PerformLayout();
			((ISupportInitialize)updnAsteroidMag).EndInit();
			((Control)groupBox4).ResumeLayout(false);
			((Control)groupBox4).PerformLayout();
			((Control)groupBox3).ResumeLayout(false);
			((Control)groupBox3).PerformLayout();
			((ISupportInitialize)updndIllum).EndInit();
			((ISupportInitialize)updndMag).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)groupBox6).ResumeLayout(false);
			((Control)groupBox6).PerformLayout();
			((ISupportInitialize)updnFaint).EndInit();
			((ISupportInitialize)updnTarget).EndInit();
			((ISupportInitialize)updnBright).EndInit();
			((ISupportInitialize)updnMagFaint).EndInit();
			((ISupportInitialize)updnMagBright).EndInit();
			((Control)groupBox5).ResumeLayout(false);
			((Control)groupBox5).PerformLayout();
			((ISupportInitialize)updnAlbedo).EndInit();
			((ISupportInitialize)updnH0).EndInit();
			((Control)groupBox7).ResumeLayout(false);
			((Control)groupBox7).PerformLayout();
			((Control)panel3).ResumeLayout(false);
			((Control)panel3).PerformLayout();
			((ISupportInitialize)updnLimitingMag).EndInit();
			((ISupportInitialize)updnTargetMag).EndInit();
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((ISupportInitialize)updnPredMagDrop).EndInit();
			((Control)panel4).ResumeLayout(false);
			((Control)panel4).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
