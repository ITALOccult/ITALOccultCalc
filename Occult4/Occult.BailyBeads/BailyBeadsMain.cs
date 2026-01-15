using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.BailyBeads
{
	public class BailyBeadsMain : Form
	{
		private bool UpdatingData;

		private bool AnimatedGIF;

		private const double Radian = 180.0 / Math.PI;

		internal int Hr;

		internal int Min;

		internal double Sec;

		private IContainer components;

		private MenuStrip menuStrip1;

		private GroupBox grpObserver;

		private Panel panelDDD;

		private Panel panelDMM;

		private TextBox txtLongDmm;

		private TextBox txtLatMM;

		private TextBox txtLatDmm;

		private TextBox txtLongMM;

		private TextBox txtAlt_ft;

		private Panel panelDMS;

		private TextBox txtLongDeg;

		private TextBox txtLatSec;

		private TextBox txtLatMin;

		private TextBox txtLatDeg;

		private TextBox txtLongSec;

		private TextBox txtLongMin;

		private Label label21;

		private Label label19;

		private Label label18;

		private Label label17;

		private Label label16;

		private Label label15;

		private Panel panel2;

		private RadioButton optFeet;

		private RadioButton optMeters;

		private Panel panel1;

		private RadioButton optDMM;

		private RadioButton optDDD;

		private RadioButton optDMS;

		private ComboBox cmbDatum;

		private Label label2;

		private Label label1;

		private GroupBox groupBox1;

		private Label label9;

		private Label label8;

		private Label label7;

		private Label label6;

		private Label label4;

		private Label label3;

		private NumericUpDown updnDecOffset;

		private NumericUpDown updnSolarRadius;

		private NumericUpDown updnRAOffset;

		private Label label5;

		private GroupBox groupBox2;

		private Label label11;

		private Label label10;

		private GroupBox groupBox3;

		private Label label13;

		private Label label12;

		private Button cmdDecrement;

		private Button cmdIncrement;

		private GroupBox groupBox4;

		private Label label20;

		private Label label14;

		private Button cmdResume10Back;

		private Button cmdPause;

		private Label label22;

		private GroupBox groupBox5;

		private CheckBox chkAutoSave;

		private Label label24;

		private Label label23;

		private GroupBox groupBox6;

		private Label label25;

		private TextBox txtID;

		private ToolStripMenuItem fileToolStripMenuItem;

		private ToolStripMenuItem openToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem showToolStripMenuItem;

		internal TextBox txtL;

		internal TextBox txtB;

		internal TextBox txt_y;

		internal TextBox txt_x;

		internal TextBox txtC;

		internal NumericUpDown updnAxisAngle;

		internal TextBox txtHr;

		internal TextBox txtMin;

		internal NumericUpDown updnSec;

		internal Button cmdPlot;

		internal ToolStripMenuItem LunarLimbToolStripMenuItem;

		internal ToolStripMenuItem eclipseImageToolStripMenuItem;

		internal ToolStripMenuItem observationsToolStripMenuItem;

		internal NumericUpDown updnMagnification;

		internal CheckBox chkMirror;

		internal Button cmdCancel;

		internal TextBox txtCursorHeight;

		internal TextBox txtCursorAA;

		private ToolStripMenuItem stayOnTopToolStripMenuItem;

		internal TextBox txtLongDDD;

		internal TextBox txtLatDDD;

		internal TextBox txtAlt_m;

		internal TextBox txtYear;

		internal TextBox txtMonth;

		internal TextBox txtDay;

		private Label label30;

		private Label label29;

		private Label label28;

		private Label label27;

		private Label label26;

		private NumericUpDown updnStepSize;

		private ToolStripMenuItem helpToolStripMenuItem;

		internal NumericUpDown updnAnimateDuration;

		internal CheckBox chkAnimation;

		internal NumericUpDown updnAnimateStep;

		private ToolStripMenuItem animatedGIFToolStripMenuItem;

		private ToolStripMenuItem animatedGIFSettingsToolStripMenuItem1;

		private ToolStripMenuItem createAnimatedGIToolStripMenuItem;

		private Label label31;

		internal RadioButton optLOLA;

		public BailyBeadsMain()
		{
			InitializeComponent();
			((Control)txtAlt_ft).set_Top(((Control)txtAlt_m).get_Top());
			((Control)panelDDD).set_Top(((Control)panelDMS).get_Top());
			((Control)panelDMM).set_Top(((Control)panelDMS).get_Top());
			((Control)panelDDD).set_Left(((Control)panelDMS).get_Left());
			((Control)panelDMM).set_Left(((Control)panelDMS).get_Left());
		}

		private void BailyBeadsMain_Load(object sender, EventArgs e)
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
			AltitudeVisibility();
			DMSformatVisibility();
			((ListControl)cmbDatum).set_SelectedIndex(0);
			RadioButton obj = optLOLA;
			bool lOLAFileExists;
			((Control)optLOLA).set_Enabled(lOLAFileExists = Utilities.LOLAFileExists);
			obj.set_Checked(lOLAFileExists);
			BailyBeads.GetDataFiles();
		}

		private void optMeters_CheckedChanged(object sender, EventArgs e)
		{
			AltitudeVisibility();
		}

		private void optFeet_CheckedChanged(object sender, EventArgs e)
		{
			AltitudeVisibility();
		}

		private void AltitudeVisibility()
		{
			((Control)txtAlt_m).set_Visible(optMeters.get_Checked());
			((Control)txtAlt_ft).set_Visible(!((Control)txtAlt_m).get_Visible());
		}

		private void txtAlt_m_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				if (!double.TryParse(((Control)txtAlt_m).get_Text(), out var result))
				{
					result = 0.0;
				}
				((Control)txtAlt_m).set_Text(string.Format("{0,1:F0}", result));
				((Control)txtAlt_ft).set_Text(string.Format("{0,1:F0}", result / 0.3048));
				UpdatingData = false;
			}
		}

		private void txtAlt_ft_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				if (!double.TryParse(((Control)txtAlt_ft).get_Text(), out var result))
				{
					result = 0.0;
				}
				((Control)txtAlt_ft).set_Text(string.Format("{0,1:F0}", result));
				((Control)txtAlt_m).set_Text(string.Format("{0,1:F0}", result * 0.3048));
				UpdatingData = false;
			}
		}

		private void optDMS_CheckedChanged(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				DMSformatVisibility();
				UpdatingData = false;
			}
		}

		private void optDMM_CheckedChanged(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				DMSformatVisibility();
				UpdatingData = false;
			}
		}

		private void optDDD_CheckedChanged(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				DMSformatVisibility();
				UpdatingData = false;
			}
		}

		private void DMSformatVisibility()
		{
			((Control)panelDMS).set_Visible(optDMS.get_Checked());
			((Control)panelDMM).set_Visible(optDMM.get_Checked());
			((Control)panelDDD).set_Visible(optDDD.get_Checked());
		}

		private void txtLongDeg_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLongitudes(0);
				UpdatingData = false;
			}
		}

		private void txtLongMin_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLongitudes(0);
				UpdatingData = false;
			}
		}

		private void txtLongSec_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLongitudes(0);
				UpdatingData = false;
			}
		}

		private void txtLatDeg_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLatitudes(0);
				UpdatingData = false;
			}
		}

		private void txtLatMin_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLatitudes(0);
				UpdatingData = false;
			}
		}

		private void txtLatSec_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLatitudes(0);
				UpdatingData = false;
			}
		}

		private void txtLongDDD_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLongitudes(2);
				UpdatingData = false;
			}
		}

		private void txtLatDDD_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLatitudes(2);
				UpdatingData = false;
			}
		}

		private void txtLongDmm_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLongitudes(1);
				UpdatingData = false;
			}
		}

		private void txtLongMM_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLongitudes(1);
				UpdatingData = false;
			}
		}

		private void txtLatDmm_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLatitudes(1);
				UpdatingData = false;
			}
		}

		private void txtLatMM_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLatitudes(1);
				UpdatingData = false;
			}
		}

		internal void UpdateLongitudes(int style)
		{
			double result = 0.0;
			double result2 = 0.0;
			double result3 = 0.0;
			bool flag = false;
			switch (style)
			{
			case 0:
				flag = ((Control)txtLongDeg).get_Text().Contains("-");
				if (!double.TryParse(((Control)txtLongDeg).get_Text().Replace('-', ' '), out result))
				{
					result = 0.0;
				}
				if (!double.TryParse(((Control)txtLongMin).get_Text().Replace('-', ' '), out result2))
				{
					result2 = 0.0;
				}
				if (!double.TryParse(((Control)txtLongSec).get_Text().Replace('-', ' '), out result3))
				{
					result3 = 0.0;
				}
				break;
			case 1:
				flag = ((Control)txtLongDmm).get_Text().Contains("-");
				if (!double.TryParse(((Control)txtLongDmm).get_Text().Replace('-', ' '), out result))
				{
					result = 0.0;
				}
				if (!double.TryParse(((Control)txtLongMM).get_Text().Replace('-', ' '), out result2))
				{
					result2 = 0.0;
				}
				break;
			default:
				flag = ((Control)txtLongDDD).get_Text().Contains("-");
				if (!double.TryParse(((Control)txtLongDDD).get_Text().Replace('-', ' '), out result))
				{
					result = 0.0;
				}
				break;
			}
			double num = result + result2 / 60.0 + result3 / 3600.0;
			if (flag)
			{
				num = 0.0 - num;
			}
			string text = Utilities.DEGtoDMS(num, 4, 1, MinutesOnly: false);
			((Control)txtLongDeg).set_Text(text.Substring(0, 4).Replace(" ", ""));
			((Control)txtLongMin).set_Text(text.Substring(5, 2).Replace(" ", ""));
			((Control)txtLongSec).set_Text(text.Substring(8).Replace(" ", ""));
			text = Utilities.DEGtoDMS(num, 4, 3, MinutesOnly: true);
			((Control)txtLongDmm).set_Text(text.Substring(0, 4).Replace(" ", ""));
			((Control)txtLongMM).set_Text(text.Substring(5).Replace(" ", ""));
			((Control)txtLongDDD).set_Text(string.Format("{0,2:F6}", num));
		}

		internal void UpdateLatitudes(int style)
		{
			double result = 0.0;
			double result2 = 0.0;
			double result3 = 0.0;
			bool flag = false;
			switch (style)
			{
			case 0:
				flag = ((Control)txtLatDeg).get_Text().Contains("-");
				if (!double.TryParse(((Control)txtLatDeg).get_Text().Replace('-', ' '), out result))
				{
					result = 0.0;
				}
				if (!double.TryParse(((Control)txtLatMin).get_Text().Replace('-', ' '), out result2))
				{
					result2 = 0.0;
				}
				if (!double.TryParse(((Control)txtLatSec).get_Text().Replace('-', ' '), out result3))
				{
					result3 = 0.0;
				}
				break;
			case 1:
				flag = ((Control)txtLatDmm).get_Text().Contains("-");
				if (!double.TryParse(((Control)txtLatDmm).get_Text().Replace('-', ' '), out result))
				{
					result = 0.0;
				}
				if (!double.TryParse(((Control)txtLatMM).get_Text().Replace('-', ' '), out result2))
				{
					result2 = 0.0;
				}
				break;
			default:
				flag = ((Control)txtLatDDD).get_Text().Contains("-");
				if (!double.TryParse(((Control)txtLatDDD).get_Text().Replace('-', ' '), out result))
				{
					result = 0.0;
				}
				break;
			}
			double num = result + result2 / 60.0 + result3 / 3600.0;
			if (flag)
			{
				num = 0.0 - num;
			}
			string text = Utilities.DEGtoDMS(num, 3, 1, MinutesOnly: false);
			((Control)txtLatDeg).set_Text(text.Substring(0, 3).Replace(" ", ""));
			((Control)txtLatMin).set_Text(text.Substring(4, 2).Replace(" ", ""));
			((Control)txtLatSec).set_Text(text.Substring(7).Replace(" ", ""));
			text = Utilities.DEGtoDMS(num, 3, 3, MinutesOnly: true);
			((Control)txtLatDmm).set_Text(text.Substring(0, 3).Replace(" ", ""));
			((Control)txtLatMM).set_Text(text.Substring(4).Replace(" ", ""));
			((Control)txtLatDDD).set_Text(string.Format("{0,2:F6}", num));
		}

		private void openToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_0027: Unknown result type (might be due to invalid IL or missing references)
			//IL_002d: Invalid comparison between Unknown and I4
			OpenFileDialog val = new OpenFileDialog();
			((FileDialog)val).set_Title("Specify file of Baily Bead observations");
			((FileDialog)val).set_InitialDirectory(Utilities.AppPath + "\\Observations");
			if ((int)((CommonDialog)val).ShowDialog() != 1)
			{
				return;
			}
			using StreamReader streamReader = new StreamReader(((FileDialog)val).get_FileName());
			((Control)txtID).set_Text(Path.GetFileNameWithoutExtension(((FileDialog)val).get_FileName()));
			string text = streamReader.ReadLine()!.PadRight(12);
			((Control)txtYear).set_Text(text.Substring(0, 4).Trim());
			((Control)txtMonth).set_Text(text.Substring(5, 2).Trim());
			((Control)txtDay).set_Text(text.Substring(8, 2).Trim());
			text = streamReader.ReadLine()!.PadRight(12);
			((Control)txtHr).set_Text(text.Substring(0, 2).Trim());
			((Control)txtMin).set_Text(text.Substring(3, 2).Trim());
			if (!double.TryParse(text.Substring(6), out var result))
			{
				result = 0.0;
			}
			updnSec.set_Value((decimal)result);
			UpdatingData = true;
			text = streamReader.ReadLine()!.PadRight(15);
			((Control)txtLongDeg).set_Text(text.Substring(0, 4).Trim());
			((Control)txtLongMin).set_Text(text.Substring(5, 2).Trim());
			((Control)txtLongSec).set_Text(text.Substring(8).Trim());
			text = streamReader.ReadLine()!.PadRight(15);
			((Control)txtLatDeg).set_Text(text.Substring(0, 3).Trim());
			((Control)txtLatMin).set_Text(text.Substring(4, 2).Trim());
			((Control)txtLatSec).set_Text(text.Substring(7).Trim());
			UpdatingData = false;
			UpdateLongitudes(0);
			UpdateLatitudes(0);
			text = streamReader.ReadLine();
			((Control)txtAlt_m).set_Text(text.Trim());
			if (!double.TryParse(((Control)txtAlt_m).get_Text(), out result))
			{
				result = 0.0;
			}
			((Control)txtAlt_m).set_Text(string.Format("{0,1:F0}", result));
			((Control)txtAlt_ft).set_Text(string.Format("{0,1:F0}", result / 0.3048));
			((Control)cmbDatum).set_Text(streamReader.ReadLine()!.Substring(6));
			text = streamReader.ReadLine();
			if (!double.TryParse(text.Substring(5), out result))
			{
				result = 0.0;
			}
			updnAxisAngle.set_Value((decimal)result);
			text = streamReader.ReadLine();
			if (!double.TryParse(text.Substring(8), out result))
			{
				result = 1.0;
			}
			updnMagnification.set_Value((decimal)result);
			text = streamReader.ReadLine();
			chkMirror.set_Checked(text.Substring(14, 1) == "1");
			text = streamReader.ReadLine()!.Substring(11);
			if (!double.TryParse(text, out result))
			{
				result = 0.0;
			}
			updnRAOffset.set_Value((decimal)result);
			text = streamReader.ReadLine()!.Substring(12);
			if (!double.TryParse(text, out result))
			{
				result = 0.0;
			}
			updnDecOffset.set_Value((decimal)result);
			text = streamReader.ReadLine()!.Substring(20);
			if (!double.TryParse(text, out result))
			{
				result = 0.0;
			}
			updnSolarRadius.set_Value((decimal)result);
			text = streamReader.ReadLine();
			text = streamReader.ReadLine();
			BailyBeads.ReadObservations(streamReader);
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0035: Unknown result type (might be due to invalid IL or missing references)
			//IL_0143: Unknown result type (might be due to invalid IL or missing references)
			//IL_0149: Invalid comparison between Unknown and I4
			int result = 0;
			int result2 = 0;
			int result3 = 0;
			int result4 = 0;
			int result5 = 0;
			string text = ((Control)txtID).get_Text().Trim();
			if (text.Length < 1)
			{
				MessageBox.Show("To save data, a Site Name must be specified", "No site name", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return;
			}
			if (!int.TryParse(((Control)txtYear).get_Text(), out result))
			{
				result = 0;
			}
			if (!int.TryParse(((Control)txtMonth).get_Text(), out result2))
			{
				result2 = 1;
			}
			if (!int.TryParse(((Control)txtDay).get_Text(), out result3))
			{
				result3 = 1;
			}
			if (!int.TryParse(((Control)txtHr).get_Text(), out result4))
			{
				result4 = 0;
			}
			if (!int.TryParse(((Control)txtMin).get_Text(), out result5))
			{
				result5 = 0;
			}
			string text2 = result + " " + Utilities.ShortMonths[result2] + " " + result3;
			string text3 = Utilities.AppPath + "\\Observations\\Eclipse " + text2;
			if (!Directory.Exists(text3))
			{
				Directory.CreateDirectory(text3);
			}
			if (File.Exists(text3 + "\\" + text + ".dat") && (int)MessageBox.Show("Do you want to overwrite the file\r\n\r\n" + text + ".dat", "Confirm Overwite", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			using StreamWriter streamWriter = new StreamWriter(text3 + "\\" + text + ".dat", append: false);
			streamWriter.WriteLine(result.ToString().PadLeft(4) + result2.ToString().PadLeft(3) + result3.ToString().PadLeft(3));
			streamWriter.WriteLine(result4.ToString().PadLeft(2) + result5.ToString().PadLeft(3) + string.Format("{0,6:F2}", (double)updnSec.get_Value()));
			streamWriter.WriteLine(((Control)txtLongDeg).get_Text().PadLeft(4) + ((Control)txtLongMin).get_Text().PadLeft(3) + ((Control)txtLongSec).get_Text().PadLeft(5));
			streamWriter.WriteLine(((Control)txtLatDeg).get_Text().PadLeft(3) + ((Control)txtLatMin).get_Text().PadLeft(3) + ((Control)txtLatSec).get_Text().PadLeft(5));
			streamWriter.WriteLine(((Control)txtAlt_m).get_Text());
			streamWriter.WriteLine("Datum " + cmbDatum.get_Items().get_Item(((ListControl)cmbDatum).get_SelectedIndex()));
			streamWriter.WriteLine("AA = " + updnAxisAngle.get_Value());
			streamWriter.WriteLine("Scale = " + updnMagnification.get_Value());
			if (chkMirror.get_Checked())
			{
				streamWriter.WriteLine("MirrorImage = 1");
			}
			else
			{
				streamWriter.WriteLine("MirrorImage = 0");
			}
			streamWriter.WriteLine("RAOffset = " + updnRAOffset.get_Value());
			streamWriter.WriteLine("DecOffset = " + updnDecOffset.get_Value());
			streamWriter.WriteLine("Radius Correction = " + updnSolarRadius.get_Value());
			streamWriter.WriteLine("Profile Correction = 0");
			streamWriter.WriteLine("Bead identifications");
			BailyBeads.WriteObservations(streamWriter);
		}

		private void BailyBeadsMain_FormClosing(object sender, FormClosingEventArgs e)
		{
			BailyBeads.Close_Eclipse_Image();
			BailyBeads.Close_Limb_Plot();
			BailyBeads.CloseResults();
			BailyBeads.Close_Heights();
			((Component)this).Dispose();
		}

		private void LunarLimbToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarLimbToolStripMenuItem.set_Checked(!LunarLimbToolStripMenuItem.get_Checked());
			if (LunarLimbToolStripMenuItem.get_Checked())
			{
				BailyBeads.Show_Limb_Plot();
			}
			else
			{
				BailyBeads.Close_Limb_Plot();
			}
		}

		private void eclipseImageToolStripMenuItem_Click(object sender, EventArgs e)
		{
			eclipseImageToolStripMenuItem.set_Checked(!eclipseImageToolStripMenuItem.get_Checked());
			if (eclipseImageToolStripMenuItem.get_Checked())
			{
				BailyBeads.Show_Eclipse_Image();
			}
			else
			{
				BailyBeads.Close_Eclipse_Image();
			}
		}

		private void observationsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			observationsToolStripMenuItem.set_Checked(!observationsToolStripMenuItem.get_Checked());
			if (observationsToolStripMenuItem.get_Checked())
			{
				BailyBeads.Show_Results();
			}
			else
			{
				BailyBeads.CloseResults();
			}
		}

		internal void cmdPlot_Click(object sender, EventArgs e)
		{
			BailyBeads.ResumeLess10 = false;
			BailyBeads.ResumeFlag = false;
			BailyBeads.MoviePause = false;
			((Control)cmdPause).set_Enabled(chkAnimation.get_Checked());
			Plot();
		}

		internal void Plot()
		{
			BailyBeads.CancelMovie = false;
			if (!int.TryParse(((Control)txtYear).get_Text(), out var result))
			{
				result = 1900;
			}
			if (!int.TryParse(((Control)txtMonth).get_Text(), out var result2))
			{
				result2 = 1;
			}
			if (!double.TryParse(((Control)txtDay).get_Text(), out var result3))
			{
				result3 = 1.0;
			}
			result3 = Math.Floor(result3);
			if (!int.TryParse(((Control)txtHr).get_Text(), out Hr))
			{
				Hr = 0;
			}
			if (!int.TryParse(((Control)txtMin).get_Text(), out Min))
			{
				Min = 0;
			}
			Sec = (double)updnSec.get_Value();
			double num = Utilities.JD_from_Date(result, result2, result3) + ((double)Hr + (double)Min / 60.0 + Sec / 3600.0) / 24.0;
			double jDatEnd = num + (double)updnAnimateDuration.get_Value() / 3600.0 / 24.0;
			double timeStep = (double)updnAnimateStep.get_Value() / 3600.0 / 24.0;
			if (!double.TryParse(((Control)txtLongDDD).get_Text(), out var result4))
			{
				result4 = 0.0;
			}
			if (!double.TryParse(((Control)txtLatDDD).get_Text(), out var result5))
			{
				result5 = 0.0;
			}
			if (!double.TryParse(((Control)txtAlt_m).get_Text(), out var result6))
			{
				result6 = 0.0;
			}
			BailyBeads.PlotImages(CentralAA: (double)updnAxisAngle.get_Value(), PlotScale: (double)updnMagnification.get_Value(), RAOffset: (double)updnRAOffset.get_Value(), DecOffset: (double)updnDecOffset.get_Value(), SunRadiusCorrn: (double)updnSolarRadius.get_Value(), Datum: cmbDatum.get_Items().get_Item(((ListControl)cmbDatum).get_SelectedIndex()).ToString(), PlotMirror: chkMirror.get_Checked(), JDatStart: num, JDatEnd: jDatEnd, TimeStep: timeStep, Longitude: result4 / (180.0 / Math.PI), Latitude: result5 / (180.0 / Math.PI), Altitude: result6, Movie: chkAnimation.get_Checked(), AnimatedGIF: AnimatedGIF);
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			BailyBeads.CancelMovie = true;
			((Control)cmdPause).set_Text("Pause");
			((Control)cmdResume10Back).set_Enabled(false);
			BailyBeads.ResumeLess10 = false;
			BailyBeads.ResumeFlag = false;
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

		private void cmdIncrement_Click(object sender, EventArgs e)
		{
			AdjustTime((double)updnStepSize.get_Value());
		}

		private void cmdDecrement_Click(object sender, EventArgs e)
		{
			AdjustTime(0.0 - (double)updnStepSize.get_Value());
		}

		private void AdjustTime(double T)
		{
			if (!int.TryParse(((Control)txtHr).get_Text(), out var result))
			{
				result = 0;
			}
			if (!int.TryParse(((Control)txtMin).get_Text(), out var result2))
			{
				result2 = 0;
			}
			double num = (double)updnSec.get_Value() + T;
			if (num >= 60.0)
			{
				num -= 60.0;
				result2++;
				if (result2 >= 60)
				{
					result2 -= 60;
					result++;
				}
			}
			else if (num < 0.0)
			{
				num += 60.0;
				result2--;
				if (result2 < 0)
				{
					result2 += 60;
					result--;
				}
			}
			((Control)txtHr).set_Text(result.ToString());
			((Control)txtMin).set_Text(result2.ToString());
			updnSec.set_Value((decimal)num);
		}

		private void updnStepSize_ValueChanged(object sender, EventArgs e)
		{
			if (updnStepSize.get_Value() >= 2m)
			{
				updnStepSize.set_Increment(1m);
			}
			else
			{
				updnStepSize.set_Increment(0.1m);
			}
		}

		private void cmdPause_Click(object sender, EventArgs e)
		{
			BailyBeads.MoviePause = !BailyBeads.MoviePause;
			if (!BailyBeads.MoviePause)
			{
				((Control)cmdPause).set_Text("Pause");
				((Control)cmdResume10Back).set_Enabled(false);
				if (!BailyBeads.ResumeLess10)
				{
					BailyBeads.ResumeFlag = true;
				}
				Plot();
			}
			else
			{
				((Control)cmdPause).set_Text("Resume");
				((Control)cmdResume10Back).set_Enabled(true);
			}
		}

		private void cmdResume10Back_Click(object sender, EventArgs e)
		{
			BailyBeads.ResumeLess10 = true;
			((Control)cmdResume10Back).set_Enabled(false);
			cmdPause_Click(sender, e);
		}

		private void chkAnimation_CheckedChanged(object sender, EventArgs e)
		{
			if (chkAnimation.get_Checked() & (updnAnimateDuration.get_Value() > 0m))
			{
				((Control)cmdPlot).set_Text("Start movie");
				((Control)cmdPause).set_Enabled(true);
			}
			else
			{
				((Control)cmdPlot).set_Text("Plot");
				((Control)cmdPause).set_Enabled(false);
			}
		}

		private void updnAnimateDuration_ValueChanged(object sender, EventArgs e)
		{
			if (updnAnimateDuration.get_Value() > 0m)
			{
				((Control)cmdPlot).set_Text("Start movie");
				((Control)cmdPause).set_Enabled(true);
			}
			else
			{
				((Control)cmdPlot).set_Text("Plot");
				((Control)cmdPause).set_Enabled(false);
			}
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Baily Beads - main");
		}

		private void animatedGIFSettingsToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			GIFsettings gIFsettings = new GIFsettings();
			((Form)gIFsettings).ShowDialog();
			((Component)(object)gIFsettings).Dispose();
		}

		private void createAnimatedGIToolStripMenuItem_Click(object sender, EventArgs e)
		{
			BailyBeads.ResumeLess10 = false;
			BailyBeads.ResumeFlag = false;
			BailyBeads.MoviePause = false;
			((Control)cmdPause).set_Enabled(chkAnimation.get_Checked());
			AnimatedGIF = true;
			Plot();
			AnimatedGIF = false;
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
			//IL_01f0: Unknown result type (might be due to invalid IL or missing references)
			//IL_01fa: Expected O, but got Unknown
			//IL_01fb: Unknown result type (might be due to invalid IL or missing references)
			//IL_0205: Expected O, but got Unknown
			//IL_0206: Unknown result type (might be due to invalid IL or missing references)
			//IL_0210: Expected O, but got Unknown
			//IL_0211: Unknown result type (might be due to invalid IL or missing references)
			//IL_021b: Expected O, but got Unknown
			//IL_021c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0226: Expected O, but got Unknown
			//IL_0227: Unknown result type (might be due to invalid IL or missing references)
			//IL_0231: Expected O, but got Unknown
			//IL_0232: Unknown result type (might be due to invalid IL or missing references)
			//IL_023c: Expected O, but got Unknown
			//IL_023d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0247: Expected O, but got Unknown
			//IL_0248: Unknown result type (might be due to invalid IL or missing references)
			//IL_0252: Expected O, but got Unknown
			//IL_0253: Unknown result type (might be due to invalid IL or missing references)
			//IL_025d: Expected O, but got Unknown
			//IL_025e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0268: Expected O, but got Unknown
			//IL_0269: Unknown result type (might be due to invalid IL or missing references)
			//IL_0273: Expected O, but got Unknown
			//IL_0274: Unknown result type (might be due to invalid IL or missing references)
			//IL_027e: Expected O, but got Unknown
			//IL_027f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0289: Expected O, but got Unknown
			//IL_028a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0294: Expected O, but got Unknown
			//IL_0295: Unknown result type (might be due to invalid IL or missing references)
			//IL_029f: Expected O, but got Unknown
			//IL_02a0: Unknown result type (might be due to invalid IL or missing references)
			//IL_02aa: Expected O, but got Unknown
			//IL_02ab: Unknown result type (might be due to invalid IL or missing references)
			//IL_02b5: Expected O, but got Unknown
			//IL_02b6: Unknown result type (might be due to invalid IL or missing references)
			//IL_02c0: Expected O, but got Unknown
			//IL_02c1: Unknown result type (might be due to invalid IL or missing references)
			//IL_02cb: Expected O, but got Unknown
			//IL_02cc: Unknown result type (might be due to invalid IL or missing references)
			//IL_02d6: Expected O, but got Unknown
			//IL_02d7: Unknown result type (might be due to invalid IL or missing references)
			//IL_02e1: Expected O, but got Unknown
			//IL_02e2: Unknown result type (might be due to invalid IL or missing references)
			//IL_02ec: Expected O, but got Unknown
			//IL_02ed: Unknown result type (might be due to invalid IL or missing references)
			//IL_02f7: Expected O, but got Unknown
			//IL_02f8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0302: Expected O, but got Unknown
			//IL_0303: Unknown result type (might be due to invalid IL or missing references)
			//IL_030d: Expected O, but got Unknown
			//IL_030e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0318: Expected O, but got Unknown
			//IL_0319: Unknown result type (might be due to invalid IL or missing references)
			//IL_0323: Expected O, but got Unknown
			//IL_0324: Unknown result type (might be due to invalid IL or missing references)
			//IL_032e: Expected O, but got Unknown
			//IL_032f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0339: Expected O, but got Unknown
			//IL_033a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0344: Expected O, but got Unknown
			//IL_0345: Unknown result type (might be due to invalid IL or missing references)
			//IL_034f: Expected O, but got Unknown
			//IL_0350: Unknown result type (might be due to invalid IL or missing references)
			//IL_035a: Expected O, but got Unknown
			//IL_035b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0365: Expected O, but got Unknown
			//IL_0366: Unknown result type (might be due to invalid IL or missing references)
			//IL_0370: Expected O, but got Unknown
			//IL_0371: Unknown result type (might be due to invalid IL or missing references)
			//IL_037b: Expected O, but got Unknown
			//IL_037c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0386: Expected O, but got Unknown
			//IL_0387: Unknown result type (might be due to invalid IL or missing references)
			//IL_0391: Expected O, but got Unknown
			//IL_0392: Unknown result type (might be due to invalid IL or missing references)
			//IL_039c: Expected O, but got Unknown
			//IL_039d: Unknown result type (might be due to invalid IL or missing references)
			//IL_03a7: Expected O, but got Unknown
			//IL_03a8: Unknown result type (might be due to invalid IL or missing references)
			//IL_03b2: Expected O, but got Unknown
			//IL_03b3: Unknown result type (might be due to invalid IL or missing references)
			//IL_03bd: Expected O, but got Unknown
			//IL_03be: Unknown result type (might be due to invalid IL or missing references)
			//IL_03c8: Expected O, but got Unknown
			//IL_03c9: Unknown result type (might be due to invalid IL or missing references)
			//IL_03d3: Expected O, but got Unknown
			//IL_03d4: Unknown result type (might be due to invalid IL or missing references)
			//IL_03de: Expected O, but got Unknown
			//IL_03df: Unknown result type (might be due to invalid IL or missing references)
			//IL_03e9: Expected O, but got Unknown
			//IL_03ea: Unknown result type (might be due to invalid IL or missing references)
			//IL_03f4: Expected O, but got Unknown
			//IL_03f5: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ff: Expected O, but got Unknown
			//IL_0400: Unknown result type (might be due to invalid IL or missing references)
			//IL_040a: Expected O, but got Unknown
			//IL_040b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0415: Expected O, but got Unknown
			//IL_0416: Unknown result type (might be due to invalid IL or missing references)
			//IL_0420: Expected O, but got Unknown
			//IL_0421: Unknown result type (might be due to invalid IL or missing references)
			//IL_042b: Expected O, but got Unknown
			//IL_042c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0436: Expected O, but got Unknown
			//IL_0437: Unknown result type (might be due to invalid IL or missing references)
			//IL_0441: Expected O, but got Unknown
			//IL_0442: Unknown result type (might be due to invalid IL or missing references)
			//IL_044c: Expected O, but got Unknown
			//IL_044d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0457: Expected O, but got Unknown
			//IL_0458: Unknown result type (might be due to invalid IL or missing references)
			//IL_0462: Expected O, but got Unknown
			//IL_0463: Unknown result type (might be due to invalid IL or missing references)
			//IL_046d: Expected O, but got Unknown
			//IL_046e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0478: Expected O, but got Unknown
			//IL_0479: Unknown result type (might be due to invalid IL or missing references)
			//IL_0483: Expected O, but got Unknown
			//IL_0484: Unknown result type (might be due to invalid IL or missing references)
			//IL_048e: Expected O, but got Unknown
			//IL_048f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0499: Expected O, but got Unknown
			//IL_049a: Unknown result type (might be due to invalid IL or missing references)
			//IL_04a4: Expected O, but got Unknown
			//IL_42fc: Unknown result type (might be due to invalid IL or missing references)
			//IL_4306: Expected O, but got Unknown
			//IL_435f: Unknown result type (might be due to invalid IL or missing references)
			//IL_4369: Expected O, but got Unknown
			menuStrip1 = new MenuStrip();
			fileToolStripMenuItem = new ToolStripMenuItem();
			openToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			animatedGIFToolStripMenuItem = new ToolStripMenuItem();
			animatedGIFSettingsToolStripMenuItem1 = new ToolStripMenuItem();
			createAnimatedGIToolStripMenuItem = new ToolStripMenuItem();
			showToolStripMenuItem = new ToolStripMenuItem();
			LunarLimbToolStripMenuItem = new ToolStripMenuItem();
			eclipseImageToolStripMenuItem = new ToolStripMenuItem();
			observationsToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			stayOnTopToolStripMenuItem = new ToolStripMenuItem();
			grpObserver = new GroupBox();
			label25 = new Label();
			txtID = new TextBox();
			chkMirror = new CheckBox();
			label2 = new Label();
			label1 = new Label();
			txtDay = new TextBox();
			panelDDD = new Panel();
			txtLongDDD = new TextBox();
			txtLatDDD = new TextBox();
			panelDMM = new Panel();
			txtLongDmm = new TextBox();
			txtLatMM = new TextBox();
			txtLatDmm = new TextBox();
			txtLongMM = new TextBox();
			txtAlt_ft = new TextBox();
			panelDMS = new Panel();
			txtLongDeg = new TextBox();
			txtLatSec = new TextBox();
			txtLatMin = new TextBox();
			txtLatDeg = new TextBox();
			txtLongSec = new TextBox();
			txtLongMin = new TextBox();
			label21 = new Label();
			label19 = new Label();
			label18 = new Label();
			label17 = new Label();
			label16 = new Label();
			label15 = new Label();
			panel2 = new Panel();
			optFeet = new RadioButton();
			optMeters = new RadioButton();
			panel1 = new Panel();
			optDMM = new RadioButton();
			optDDD = new RadioButton();
			optDMS = new RadioButton();
			cmbDatum = new ComboBox();
			txtYear = new TextBox();
			txtAlt_m = new TextBox();
			txtMonth = new TextBox();
			groupBox1 = new GroupBox();
			optLOLA = new RadioButton();
			label31 = new Label();
			label9 = new Label();
			label8 = new Label();
			label7 = new Label();
			label6 = new Label();
			label3 = new Label();
			updnMagnification = new NumericUpDown();
			updnDecOffset = new NumericUpDown();
			updnSolarRadius = new NumericUpDown();
			updnRAOffset = new NumericUpDown();
			updnAxisAngle = new NumericUpDown();
			label4 = new Label();
			label5 = new Label();
			groupBox2 = new GroupBox();
			label13 = new Label();
			updnSec = new NumericUpDown();
			label11 = new Label();
			label10 = new Label();
			txtHr = new TextBox();
			txtMin = new TextBox();
			groupBox3 = new GroupBox();
			updnStepSize = new NumericUpDown();
			label12 = new Label();
			cmdDecrement = new Button();
			cmdIncrement = new Button();
			cmdPlot = new Button();
			groupBox4 = new GroupBox();
			chkAutoSave = new CheckBox();
			chkAnimation = new CheckBox();
			label22 = new Label();
			cmdResume10Back = new Button();
			cmdPause = new Button();
			cmdCancel = new Button();
			label20 = new Label();
			label14 = new Label();
			updnAnimateStep = new NumericUpDown();
			updnAnimateDuration = new NumericUpDown();
			groupBox5 = new GroupBox();
			label24 = new Label();
			label23 = new Label();
			txtCursorHeight = new TextBox();
			txtCursorAA = new TextBox();
			groupBox6 = new GroupBox();
			label30 = new Label();
			label29 = new Label();
			label28 = new Label();
			label27 = new Label();
			label26 = new Label();
			txtL = new TextBox();
			txtB = new TextBox();
			txt_y = new TextBox();
			txt_x = new TextBox();
			txtC = new TextBox();
			((Control)menuStrip1).SuspendLayout();
			((Control)grpObserver).SuspendLayout();
			((Control)panelDDD).SuspendLayout();
			((Control)panelDMM).SuspendLayout();
			((Control)panelDMS).SuspendLayout();
			((Control)panel2).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((Control)groupBox1).SuspendLayout();
			((ISupportInitialize)updnMagnification).BeginInit();
			((ISupportInitialize)updnDecOffset).BeginInit();
			((ISupportInitialize)updnSolarRadius).BeginInit();
			((ISupportInitialize)updnRAOffset).BeginInit();
			((ISupportInitialize)updnAxisAngle).BeginInit();
			((Control)groupBox2).SuspendLayout();
			((ISupportInitialize)updnSec).BeginInit();
			((Control)groupBox3).SuspendLayout();
			((ISupportInitialize)updnStepSize).BeginInit();
			((Control)groupBox4).SuspendLayout();
			((ISupportInitialize)updnAnimateStep).BeginInit();
			((ISupportInitialize)updnAnimateDuration).BeginInit();
			((Control)groupBox5).SuspendLayout();
			((Control)groupBox6).SuspendLayout();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)fileToolStripMenuItem,
				(ToolStripItem)animatedGIFToolStripMenuItem,
				(ToolStripItem)showToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)stayOnTopToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(789, 24));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)fileToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)openToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)fileToolStripMenuItem).set_Name("fileToolStripMenuItem");
			fileToolStripMenuItem.set_ShowShortcutKeys(false);
			((ToolStripItem)fileToolStripMenuItem).set_Size(new Size(49, 20));
			((ToolStripItem)fileToolStripMenuItem).set_Text("File....");
			((ToolStripItem)openToolStripMenuItem).set_Image((Image)Resources.openfolderHS);
			((ToolStripItem)openToolStripMenuItem).set_Name("openToolStripMenuItem");
			openToolStripMenuItem.set_ShortcutKeys((Keys)131151);
			((ToolStripItem)openToolStripMenuItem).set_Size(new Size(146, 22));
			((ToolStripItem)openToolStripMenuItem).set_Text("&Open");
			((ToolStripItem)openToolStripMenuItem).add_Click((EventHandler)openToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(146, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripDropDownItem)animatedGIFToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)animatedGIFSettingsToolStripMenuItem1,
				(ToolStripItem)createAnimatedGIToolStripMenuItem
			});
			((ToolStripItem)animatedGIFToolStripMenuItem).set_Name("animatedGIFToolStripMenuItem");
			((ToolStripItem)animatedGIFToolStripMenuItem).set_Size(new Size(112, 20));
			((ToolStripItem)animatedGIFToolStripMenuItem).set_Text("Animated GIF...    ");
			((ToolStripItem)animatedGIFSettingsToolStripMenuItem1).set_Name("animatedGIFSettingsToolStripMenuItem1");
			animatedGIFSettingsToolStripMenuItem1.set_ShortcutKeys((Keys)131137);
			((ToolStripItem)animatedGIFSettingsToolStripMenuItem1).set_Size(new Size(232, 22));
			((ToolStripItem)animatedGIFSettingsToolStripMenuItem1).set_Text("Animated GIF settings");
			((ToolStripItem)animatedGIFSettingsToolStripMenuItem1).add_Click((EventHandler)animatedGIFSettingsToolStripMenuItem1_Click);
			((ToolStripItem)createAnimatedGIToolStripMenuItem).set_Name("createAnimatedGIToolStripMenuItem");
			createAnimatedGIToolStripMenuItem.set_ShortcutKeys((Keys)131143);
			((ToolStripItem)createAnimatedGIToolStripMenuItem).set_Size(new Size(232, 22));
			((ToolStripItem)createAnimatedGIToolStripMenuItem).set_Text("Create Animated GIF");
			((ToolStripItem)createAnimatedGIToolStripMenuItem).add_Click((EventHandler)createAnimatedGIToolStripMenuItem_Click);
			((ToolStripDropDownItem)showToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)LunarLimbToolStripMenuItem,
				(ToolStripItem)eclipseImageToolStripMenuItem,
				(ToolStripItem)observationsToolStripMenuItem
			});
			((ToolStripItem)showToolStripMenuItem).set_Name("showToolStripMenuItem");
			((ToolStripItem)showToolStripMenuItem).set_Size(new Size(60, 20));
			((ToolStripItem)showToolStripMenuItem).set_Text("Show....");
			LunarLimbToolStripMenuItem.set_Checked(true);
			LunarLimbToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)LunarLimbToolStripMenuItem).set_Name("LunarLimbToolStripMenuItem");
			LunarLimbToolStripMenuItem.set_ShortcutKeys((Keys)131148);
			((ToolStripItem)LunarLimbToolStripMenuItem).set_Size(new Size(225, 22));
			((ToolStripItem)LunarLimbToolStripMenuItem).set_Text("Lunar limb");
			((ToolStripItem)LunarLimbToolStripMenuItem).add_Click((EventHandler)LunarLimbToolStripMenuItem_Click);
			eclipseImageToolStripMenuItem.set_Checked(true);
			eclipseImageToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)eclipseImageToolStripMenuItem).set_Name("eclipseImageToolStripMenuItem");
			eclipseImageToolStripMenuItem.set_ShortcutKeys((Keys)131141);
			((ToolStripItem)eclipseImageToolStripMenuItem).set_Size(new Size(225, 22));
			((ToolStripItem)eclipseImageToolStripMenuItem).set_Text("Eclipse Image");
			((ToolStripItem)eclipseImageToolStripMenuItem).add_Click((EventHandler)eclipseImageToolStripMenuItem_Click);
			observationsToolStripMenuItem.set_Checked(true);
			observationsToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)observationsToolStripMenuItem).set_Name("observationsToolStripMenuItem");
			observationsToolStripMenuItem.set_ShortcutKeys((Keys)131156);
			((ToolStripItem)observationsToolStripMenuItem).set_Size(new Size(225, 22));
			((ToolStripItem)observationsToolStripMenuItem).set_Text("Table of observations");
			((ToolStripItem)observationsToolStripMenuItem).add_Click((EventHandler)observationsToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(69, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help   ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)stayOnTopToolStripMenuItem).set_Image((Image)Resources.FillUpHS);
			((ToolStripItem)stayOnTopToolStripMenuItem).set_Name("stayOnTopToolStripMenuItem");
			((ToolStripItem)stayOnTopToolStripMenuItem).set_Size(new Size(95, 20));
			((ToolStripItem)stayOnTopToolStripMenuItem).set_Text("stay on &Top");
			((ToolStripItem)stayOnTopToolStripMenuItem).add_Click((EventHandler)stayOnTopToolStripMenuItem_Click);
			((Control)grpObserver).get_Controls().Add((Control)(object)label25);
			((Control)grpObserver).get_Controls().Add((Control)(object)txtID);
			((Control)grpObserver).get_Controls().Add((Control)(object)chkMirror);
			((Control)grpObserver).get_Controls().Add((Control)(object)label2);
			((Control)grpObserver).get_Controls().Add((Control)(object)label1);
			((Control)grpObserver).get_Controls().Add((Control)(object)txtDay);
			((Control)grpObserver).get_Controls().Add((Control)(object)panelDDD);
			((Control)grpObserver).get_Controls().Add((Control)(object)panelDMM);
			((Control)grpObserver).get_Controls().Add((Control)(object)txtAlt_ft);
			((Control)grpObserver).get_Controls().Add((Control)(object)panelDMS);
			((Control)grpObserver).get_Controls().Add((Control)(object)label21);
			((Control)grpObserver).get_Controls().Add((Control)(object)label19);
			((Control)grpObserver).get_Controls().Add((Control)(object)label18);
			((Control)grpObserver).get_Controls().Add((Control)(object)label17);
			((Control)grpObserver).get_Controls().Add((Control)(object)label16);
			((Control)grpObserver).get_Controls().Add((Control)(object)label15);
			((Control)grpObserver).get_Controls().Add((Control)(object)panel2);
			((Control)grpObserver).get_Controls().Add((Control)(object)panel1);
			((Control)grpObserver).get_Controls().Add((Control)(object)cmbDatum);
			((Control)grpObserver).get_Controls().Add((Control)(object)txtYear);
			((Control)grpObserver).get_Controls().Add((Control)(object)txtAlt_m);
			((Control)grpObserver).get_Controls().Add((Control)(object)txtMonth);
			((Control)grpObserver).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpObserver).set_Location(new Point(11, 27));
			((Control)grpObserver).set_Name("grpObserver");
			((Control)grpObserver).set_Size(new Size(269, 208));
			((Control)grpObserver).set_TabIndex(0);
			grpObserver.set_TabStop(false);
			((Control)grpObserver).set_Text("Date && Site");
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label25).set_Location(new Point(27, 58));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(54, 13));
			((Control)label25).set_TabIndex(7);
			((Control)label25).set_Text("Site name");
			((Control)txtID).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtID).set_Location(new Point(89, 54));
			((Control)txtID).set_Name("txtID");
			((Control)txtID).set_Size(new Size(110, 20));
			((Control)txtID).set_TabIndex(8);
			((Control)chkMirror).set_AutoSize(true);
			chkMirror.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)chkMirror).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkMirror).set_Location(new Point(34, 187));
			((Control)chkMirror).set_Name("chkMirror");
			((Control)chkMirror).set_Size(new Size(124, 17));
			((Control)chkMirror).set_TabIndex(20);
			((Control)chkMirror).set_Text("Draw as mirror image");
			((ButtonBase)chkMirror).set_UseVisualStyleBackColor(true);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(142, 13));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(24, 13));
			((Control)label2).set_TabIndex(1);
			((Control)label2).set_Text("mth");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(169, 13));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(24, 13));
			((Control)label1).set_TabIndex(2);
			((Control)label1).set_Text("day");
			((Control)txtDay).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtDay).set_Location(new Point(172, 30));
			((TextBoxBase)txtDay).set_MaxLength(37);
			((Control)txtDay).set_Name("txtDay");
			((Control)txtDay).set_Size(new Size(25, 20));
			((Control)txtDay).set_TabIndex(6);
			((Control)panelDDD).get_Controls().Add((Control)(object)txtLongDDD);
			((Control)panelDDD).get_Controls().Add((Control)(object)txtLatDDD);
			((Control)panelDDD).set_Location(new Point(220, 154));
			((Control)panelDDD).set_Name("panelDDD");
			((Control)panelDDD).set_Size(new Size(113, 52));
			((Control)panelDDD).set_TabIndex(13);
			((Control)txtLongDDD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongDDD).set_Location(new Point(4, 2));
			((Control)txtLongDDD).set_Name("txtLongDDD");
			((Control)txtLongDDD).set_Size(new Size(82, 20));
			((Control)txtLongDDD).set_TabIndex(0);
			((Control)txtLongDDD).add_Leave((EventHandler)txtLongDDD_Leave);
			((Control)txtLatDDD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatDDD).set_Location(new Point(4, 28));
			((Control)txtLatDDD).set_Name("txtLatDDD");
			((Control)txtLatDDD).set_Size(new Size(82, 20));
			((Control)txtLatDDD).set_TabIndex(1);
			((Control)txtLatDDD).add_Leave((EventHandler)txtLatDDD_Leave);
			((Control)panelDMM).get_Controls().Add((Control)(object)txtLongDmm);
			((Control)panelDMM).get_Controls().Add((Control)(object)txtLatMM);
			((Control)panelDMM).get_Controls().Add((Control)(object)txtLatDmm);
			((Control)panelDMM).get_Controls().Add((Control)(object)txtLongMM);
			((Control)panelDMM).set_Location(new Point(209, 131));
			((Control)panelDMM).set_Name("panelDMM");
			((Control)panelDMM).set_Size(new Size(113, 52));
			((Control)panelDMM).set_TabIndex(12);
			((Control)txtLongDmm).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongDmm).set_Location(new Point(4, 2));
			((Control)txtLongDmm).set_Name("txtLongDmm");
			((Control)txtLongDmm).set_Size(new Size(34, 20));
			((Control)txtLongDmm).set_TabIndex(0);
			((Control)txtLongDmm).add_Leave((EventHandler)txtLongDmm_Leave);
			((Control)txtLatMM).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatMM).set_Location(new Point(44, 28));
			((Control)txtLatMM).set_Name("txtLatMM");
			((Control)txtLatMM).set_Size(new Size(44, 20));
			((Control)txtLatMM).set_TabIndex(3);
			((Control)txtLatMM).add_Leave((EventHandler)txtLatMM_Leave);
			((Control)txtLatDmm).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatDmm).set_Location(new Point(4, 28));
			((Control)txtLatDmm).set_Name("txtLatDmm");
			((Control)txtLatDmm).set_Size(new Size(34, 20));
			((Control)txtLatDmm).set_TabIndex(2);
			((Control)txtLatDmm).add_Leave((EventHandler)txtLatDmm_Leave);
			((Control)txtLongMM).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongMM).set_Location(new Point(44, 2));
			((Control)txtLongMM).set_Name("txtLongMM");
			((Control)txtLongMM).set_Size(new Size(44, 20));
			((Control)txtLongMM).set_TabIndex(1);
			((Control)txtLongMM).add_Leave((EventHandler)txtLongMM_Leave);
			((Control)txtAlt_ft).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAlt_ft).set_Location(new Point(89, 163));
			((Control)txtAlt_ft).set_Name("txtAlt_ft");
			((Control)txtAlt_ft).set_Size(new Size(53, 20));
			((Control)txtAlt_ft).set_TabIndex(19);
			((Control)txtAlt_ft).add_Leave((EventHandler)txtAlt_ft_Leave);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLongDeg);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLatSec);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLatMin);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLatDeg);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLongSec);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLongMin);
			((Control)panelDMS).set_Location(new Point(85, 75));
			((Control)panelDMS).set_Name("panelDMS");
			((Control)panelDMS).set_Size(new Size(113, 52));
			((Control)panelDMS).set_TabIndex(10);
			((Control)txtLongDeg).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongDeg).set_Location(new Point(4, 2));
			((Control)txtLongDeg).set_Name("txtLongDeg");
			((Control)txtLongDeg).set_Size(new Size(34, 20));
			((Control)txtLongDeg).set_TabIndex(0);
			((Control)txtLongDeg).add_Leave((EventHandler)txtLongDeg_Leave);
			((Control)txtLatSec).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatSec).set_Location(new Point(71, 28));
			((Control)txtLatSec).set_Name("txtLatSec");
			((Control)txtLatSec).set_Size(new Size(39, 20));
			((Control)txtLatSec).set_TabIndex(5);
			((Control)txtLatSec).add_Leave((EventHandler)txtLatSec_Leave);
			((Control)txtLatMin).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatMin).set_Location(new Point(44, 28));
			((Control)txtLatMin).set_Name("txtLatMin");
			((Control)txtLatMin).set_Size(new Size(21, 20));
			((Control)txtLatMin).set_TabIndex(4);
			((Control)txtLatMin).add_Leave((EventHandler)txtLatMin_Leave);
			((Control)txtLatDeg).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatDeg).set_Location(new Point(4, 28));
			((Control)txtLatDeg).set_Name("txtLatDeg");
			((Control)txtLatDeg).set_Size(new Size(34, 20));
			((Control)txtLatDeg).set_TabIndex(3);
			((Control)txtLatDeg).add_Leave((EventHandler)txtLatDeg_Leave);
			((Control)txtLongSec).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongSec).set_Location(new Point(71, 2));
			((Control)txtLongSec).set_Name("txtLongSec");
			((Control)txtLongSec).set_Size(new Size(40, 20));
			((Control)txtLongSec).set_TabIndex(2);
			((Control)txtLongSec).add_Leave((EventHandler)txtLongSec_Leave);
			((Control)txtLongMin).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongMin).set_Location(new Point(44, 2));
			((Control)txtLongMin).set_Name("txtLongMin");
			((Control)txtLongMin).set_Size(new Size(21, 20));
			((Control)txtLongMin).set_TabIndex(1);
			((Control)txtLongMin).add_Leave((EventHandler)txtLongMin_Leave);
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label21).set_Location(new Point(109, 13));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(27, 13));
			((Control)label21).set_TabIndex(0);
			((Control)label21).set_Text("year");
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label19).set_Location(new Point(43, 132));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(38, 13));
			((Control)label19).set_TabIndex(15);
			((Control)label19).set_Text("Datum");
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label18).set_Location(new Point(36, 106));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(45, 13));
			((Control)label18).set_TabIndex(14);
			((Control)label18).set_Text("Latitude");
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label17).set_Location(new Point(14, 80));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(67, 13));
			((Control)label17).set_TabIndex(9);
			((Control)label17).set_Text("E. Longitude");
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label16).set_Location(new Point(51, 34));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(30, 13));
			((Control)label16).set_TabIndex(3);
			((Control)label16).set_Text("Date");
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label15).set_Location(new Point(39, 159));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(42, 13));
			((Control)label15).set_TabIndex(17);
			((Control)label15).set_Text("Altitude");
			((Control)panel2).get_Controls().Add((Control)(object)optFeet);
			((Control)panel2).get_Controls().Add((Control)(object)optMeters);
			((Control)panel2).set_Location(new Point(146, 152));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(53, 34));
			((Control)panel2).set_TabIndex(18);
			((Control)optFeet).set_AutoSize(true);
			((Control)optFeet).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optFeet).set_Location(new Point(2, 16));
			((Control)optFeet).set_Name("optFeet");
			((Control)optFeet).set_Size(new Size(33, 17));
			((Control)optFeet).set_TabIndex(1);
			optFeet.set_TabStop(true);
			((Control)optFeet).set_Text("ft");
			((ButtonBase)optFeet).set_UseVisualStyleBackColor(true);
			optFeet.add_CheckedChanged((EventHandler)optFeet_CheckedChanged);
			((Control)optMeters).set_AutoSize(true);
			optMeters.set_Checked(true);
			((Control)optMeters).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optMeters).set_Location(new Point(2, 1));
			((Control)optMeters).set_Name("optMeters");
			((Control)optMeters).set_Size(new Size(34, 17));
			((Control)optMeters).set_TabIndex(0);
			optMeters.set_TabStop(true);
			((Control)optMeters).set_Text("m");
			((ButtonBase)optMeters).set_UseVisualStyleBackColor(true);
			optMeters.add_CheckedChanged((EventHandler)optMeters_CheckedChanged);
			((Control)panel1).get_Controls().Add((Control)(object)optDMM);
			((Control)panel1).get_Controls().Add((Control)(object)optDDD);
			((Control)panel1).get_Controls().Add((Control)(object)optDMS);
			((Control)panel1).set_Location(new Point(206, 74));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(61, 56));
			((Control)panel1).set_TabIndex(11);
			((Control)optDMM).set_AutoSize(true);
			((Control)optDMM).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optDMM).set_Location(new Point(3, 20));
			((Control)optDMM).set_Name("optDMM");
			((Control)optDMM).set_Size(new Size(50, 17));
			((Control)optDMM).set_TabIndex(1);
			optDMM.set_TabStop(true);
			((Control)optDMM).set_Text("dm.m");
			((ButtonBase)optDMM).set_UseVisualStyleBackColor(true);
			optDMM.add_CheckedChanged((EventHandler)optDMM_CheckedChanged);
			((Control)optDDD).set_AutoSize(true);
			((Control)optDDD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optDDD).set_Location(new Point(3, 35));
			((Control)optDDD).set_Name("optDDD");
			((Control)optDDD).set_Size(new Size(46, 17));
			((Control)optDDD).set_TabIndex(2);
			optDDD.set_TabStop(true);
			((Control)optDDD).set_Text("d.dd");
			((ButtonBase)optDDD).set_UseVisualStyleBackColor(true);
			optDDD.add_CheckedChanged((EventHandler)optDDD_CheckedChanged);
			((Control)optDMS).set_AutoSize(true);
			optDMS.set_Checked(true);
			((Control)optDMS).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optDMS).set_Location(new Point(3, 5));
			((Control)optDMS).set_Name("optDMS");
			((Control)optDMS).set_Size(new Size(44, 17));
			((Control)optDMS).set_TabIndex(0);
			optDMS.set_TabStop(true);
			((Control)optDMS).set_Text("dms");
			((ButtonBase)optDMS).set_UseVisualStyleBackColor(true);
			optDMS.add_CheckedChanged((EventHandler)optDMS_CheckedChanged);
			((Control)cmbDatum).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbDatum).set_FormattingEnabled(true);
			cmbDatum.get_Items().AddRange(new object[7] { "", "WGS84", "NAD1927", "EP1950", "Tokyo", "GBSN80", "?" });
			((Control)cmbDatum).set_Location(new Point(89, 129));
			((Control)cmbDatum).set_Name("cmbDatum");
			((Control)cmbDatum).set_Size(new Size(91, 21));
			((Control)cmbDatum).set_TabIndex(16);
			((Control)txtYear).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtYear).set_Location(new Point(97, 30));
			((Control)txtYear).set_Name("txtYear");
			((Control)txtYear).set_Size(new Size(38, 20));
			((Control)txtYear).set_TabIndex(4);
			((Control)txtAlt_m).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAlt_m).set_Location(new Point(89, 156));
			((Control)txtAlt_m).set_Name("txtAlt_m");
			((Control)txtAlt_m).set_Size(new Size(53, 20));
			((Control)txtAlt_m).set_TabIndex(18);
			((Control)txtAlt_m).add_Leave((EventHandler)txtAlt_m_Leave);
			((Control)txtMonth).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMonth).set_Location(new Point(141, 30));
			((TextBoxBase)txtMonth).set_MaxLength(37);
			((Control)txtMonth).set_Name("txtMonth");
			((Control)txtMonth).set_Size(new Size(25, 20));
			((Control)txtMonth).set_TabIndex(5);
			((Control)groupBox1).get_Controls().Add((Control)(object)optLOLA);
			((Control)groupBox1).get_Controls().Add((Control)(object)label31);
			((Control)groupBox1).get_Controls().Add((Control)(object)label9);
			((Control)groupBox1).get_Controls().Add((Control)(object)label8);
			((Control)groupBox1).get_Controls().Add((Control)(object)label7);
			((Control)groupBox1).get_Controls().Add((Control)(object)label6);
			((Control)groupBox1).get_Controls().Add((Control)(object)label3);
			((Control)groupBox1).get_Controls().Add((Control)(object)updnMagnification);
			((Control)groupBox1).get_Controls().Add((Control)(object)updnDecOffset);
			((Control)groupBox1).get_Controls().Add((Control)(object)updnSolarRadius);
			((Control)groupBox1).get_Controls().Add((Control)(object)updnRAOffset);
			((Control)groupBox1).get_Controls().Add((Control)(object)updnAxisAngle);
			((Control)groupBox1).get_Controls().Add((Control)(object)label4);
			((Control)groupBox1).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)groupBox1).set_Location(new Point(286, 28));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(198, 207));
			((Control)groupBox1).set_TabIndex(1);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Plot Parameters");
			((Control)optLOLA).set_AutoSize(true);
			optLOLA.set_CheckAlign(ContentAlignment.MiddleRight);
			optLOLA.set_Checked(true);
			((Control)optLOLA).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optLOLA).set_Location(new Point(9, 160));
			((Control)optLOLA).set_Name("optLOLA");
			((Control)optLOLA).set_Size(new Size(52, 17));
			((Control)optLOLA).set_TabIndex(15);
			optLOLA.set_TabStop(true);
			((Control)optLOLA).set_Text("LOLA");
			((ButtonBase)optLOLA).set_UseVisualStyleBackColor(true);
			((Control)label31).set_AutoSize(true);
			((Control)label31).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label31).set_Location(new Point(2, 52));
			((Control)label31).set_Name("label31");
			((Control)label31).set_Size(new Size(81, 24));
			((Control)label31).set_TabIndex(14);
			((Control)label31).set_Text("For full disk, set\r\nscale to 0.4 or less");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(33, 30));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(90, 13));
			((Control)label9).set_TabIndex(0);
			((Control)label9).set_Text("Central axis angle");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(89, 61));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(34, 13));
			((Control)label8).set_TabIndex(2);
			((Control)label8).set_Text("Scale");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(12, 81));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(40, 13));
			((Control)label7).set_TabIndex(4);
			((Control)label7).set_Text("Offsets");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(12, 99));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(22, 13));
			((Control)label6).set_TabIndex(5);
			((Control)label6).set_Text("RA");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(98, 99));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(27, 13));
			((Control)label3).set_TabIndex(7);
			((Control)label3).set_Text("Dec");
			updnMagnification.set_DecimalPlaces(1);
			((Control)updnMagnification).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnMagnification.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnMagnification).set_Location(new Point(127, 57));
			updnMagnification.set_Maximum(new decimal(new int[4] { 10, 0, 0, 0 }));
			updnMagnification.set_Minimum(new decimal(new int[4] { 2, 0, 0, 65536 }));
			((Control)updnMagnification).set_Name("updnMagnification");
			((Control)updnMagnification).set_Size(new Size(52, 20));
			((Control)updnMagnification).set_TabIndex(3);
			updnMagnification.set_Value(new decimal(new int[4] { 4, 0, 0, 65536 }));
			updnDecOffset.set_DecimalPlaces(2);
			((Control)updnDecOffset).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnDecOffset.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnDecOffset).set_Location(new Point(127, 96));
			updnDecOffset.set_Minimum(new decimal(new int[4] { 100, 0, 0, -2147483648 }));
			((Control)updnDecOffset).set_Name("updnDecOffset");
			((Control)updnDecOffset).set_Size(new Size(52, 20));
			((Control)updnDecOffset).set_TabIndex(8);
			updnSolarRadius.set_DecimalPlaces(2);
			((Control)updnSolarRadius).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnSolarRadius.set_Increment(new decimal(new int[4] { 1, 0, 0, 131072 }));
			((Control)updnSolarRadius).set_Location(new Point(127, 132));
			updnSolarRadius.set_Maximum(new decimal(new int[4] { 10, 0, 0, 0 }));
			updnSolarRadius.set_Minimum(new decimal(new int[4] { 10, 0, 0, -2147483648 }));
			((Control)updnSolarRadius).set_Name("updnSolarRadius");
			((Control)updnSolarRadius).set_Size(new Size(52, 20));
			((Control)updnSolarRadius).set_TabIndex(10);
			updnRAOffset.set_DecimalPlaces(2);
			((Control)updnRAOffset).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnRAOffset.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnRAOffset).set_Location(new Point(42, 96));
			updnRAOffset.set_Minimum(new decimal(new int[4] { 100, 0, 0, -2147483648 }));
			((Control)updnRAOffset).set_Name("updnRAOffset");
			((Control)updnRAOffset).set_Size(new Size(52, 20));
			((Control)updnRAOffset).set_TabIndex(6);
			updnAxisAngle.set_DecimalPlaces(2);
			((Control)updnAxisAngle).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnAxisAngle).set_Location(new Point(127, 27));
			updnAxisAngle.set_Maximum(new decimal(new int[4] { 359, 0, 0, 0 }));
			((Control)updnAxisAngle).set_Name("updnAxisAngle");
			((Control)updnAxisAngle).set_Size(new Size(63, 20));
			((Control)updnAxisAngle).set_TabIndex(1);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(8, 126));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(113, 26));
			((Control)label4).set_TabIndex(9);
			((Control)label4).set_Text("Correction to solar\r\n radius at unit distance");
			label4.set_TextAlign(ContentAlignment.TopRight);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(62, 12));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(13, 13));
			((Control)label5).set_TabIndex(1);
			((Control)label5).set_Text("h");
			((Control)groupBox2).get_Controls().Add((Control)(object)label13);
			((Control)groupBox2).get_Controls().Add((Control)(object)updnSec);
			((Control)groupBox2).get_Controls().Add((Control)(object)label11);
			((Control)groupBox2).get_Controls().Add((Control)(object)label10);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtHr);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtMin);
			((Control)groupBox2).get_Controls().Add((Control)(object)groupBox3);
			((Control)groupBox2).get_Controls().Add((Control)(object)label5);
			((Control)groupBox2).set_Location(new Point(490, 30));
			((Control)groupBox2).set_Name("groupBox2");
			((Control)groupBox2).set_Size(new Size(258, 90));
			((Control)groupBox2).set_TabIndex(3);
			groupBox2.set_TabStop(false);
			((Control)groupBox2).set_Text("Time for the plot");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label13).set_Location(new Point(20, 28));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(29, 13));
			((Control)label13).set_TabIndex(0);
			((Control)label13).set_Text("UTC");
			updnSec.set_DecimalPlaces(1);
			((Control)updnSec).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnSec.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnSec).set_Location(new Point(118, 25));
			updnSec.set_Maximum(new decimal(new int[4] { 599, 0, 0, 65536 }));
			((Control)updnSec).set_Name("updnSec");
			((Control)updnSec).set_Size(new Size(46, 20));
			((Control)updnSec).set_TabIndex(6);
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label11).set_Location(new Point(124, 11));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(12, 13));
			((Control)label11).set_TabIndex(5);
			((Control)label11).set_Text("s");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label10).set_Location(new Point(93, 11));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(15, 13));
			((Control)label10).set_TabIndex(3);
			((Control)label10).set_Text("m");
			((Control)txtHr).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtHr).set_Location(new Point(56, 25));
			((TextBoxBase)txtHr).set_MaxLength(37);
			((Control)txtHr).set_Name("txtHr");
			((Control)txtHr).set_Size(new Size(25, 20));
			((Control)txtHr).set_TabIndex(2);
			((Control)txtMin).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMin).set_Location(new Point(87, 25));
			((TextBoxBase)txtMin).set_MaxLength(37);
			((Control)txtMin).set_Name("txtMin");
			((Control)txtMin).set_Size(new Size(25, 20));
			((Control)txtMin).set_TabIndex(4);
			((Control)groupBox3).get_Controls().Add((Control)(object)updnStepSize);
			((Control)groupBox3).get_Controls().Add((Control)(object)label12);
			((Control)groupBox3).get_Controls().Add((Control)(object)cmdDecrement);
			((Control)groupBox3).get_Controls().Add((Control)(object)cmdIncrement);
			((Control)groupBox3).set_Location(new Point(10, 47));
			((Control)groupBox3).set_Name("groupBox3");
			((Control)groupBox3).set_Size(new Size(207, 38));
			((Control)groupBox3).set_TabIndex(7);
			groupBox3.set_TabStop(false);
			((Control)groupBox3).set_Text("Change time");
			updnStepSize.set_DecimalPlaces(1);
			((Control)updnStepSize).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnStepSize).set_Location(new Point(94, 12));
			updnStepSize.set_Maximum(new decimal(new int[4] { 60, 0, 0, 0 }));
			((Control)updnStepSize).set_Name("updnStepSize");
			((Control)updnStepSize).set_Size(new Size(46, 20));
			((Control)updnStepSize).set_TabIndex(4);
			updnStepSize.set_Value(new decimal(new int[4] { 1, 0, 0, 65536 }));
			updnStepSize.add_ValueChanged((EventHandler)updnStepSize_ValueChanged);
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(6, 15));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(81, 13));
			((Control)label12).set_TabIndex(0);
			((Control)label12).set_Text("Step size (secs)");
			((Control)cmdDecrement).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdDecrement).set_Location(new Point(176, 10));
			((Control)cmdDecrement).set_Name("cmdDecrement");
			((Control)cmdDecrement).set_Size(new Size(21, 22));
			((Control)cmdDecrement).set_TabIndex(3);
			((Control)cmdDecrement).set_Text("-");
			((ButtonBase)cmdDecrement).set_UseVisualStyleBackColor(true);
			((Control)cmdDecrement).add_Click((EventHandler)cmdDecrement_Click);
			((Control)cmdIncrement).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdIncrement).set_Location(new Point(149, 10));
			((Control)cmdIncrement).set_Name("cmdIncrement");
			((Control)cmdIncrement).set_Size(new Size(21, 22));
			((Control)cmdIncrement).set_TabIndex(2);
			((Control)cmdIncrement).set_Text("+");
			((ButtonBase)cmdIncrement).set_UseVisualStyleBackColor(true);
			((Control)cmdIncrement).add_Click((EventHandler)cmdIncrement_Click);
			((Control)cmdPlot).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdPlot).set_Location(new Point(577, 233));
			((Control)cmdPlot).set_Name("cmdPlot");
			((Control)cmdPlot).set_Size(new Size(104, 51));
			((Control)cmdPlot).set_TabIndex(2);
			((Control)cmdPlot).set_Text("&Plot");
			((ButtonBase)cmdPlot).set_UseVisualStyleBackColor(true);
			((Control)cmdPlot).add_Click((EventHandler)cmdPlot_Click);
			((Control)groupBox4).get_Controls().Add((Control)(object)chkAutoSave);
			((Control)groupBox4).get_Controls().Add((Control)(object)chkAnimation);
			((Control)groupBox4).get_Controls().Add((Control)(object)label22);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmdResume10Back);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmdPause);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmdCancel);
			((Control)groupBox4).get_Controls().Add((Control)(object)label20);
			((Control)groupBox4).get_Controls().Add((Control)(object)label14);
			((Control)groupBox4).get_Controls().Add((Control)(object)updnAnimateStep);
			((Control)groupBox4).get_Controls().Add((Control)(object)updnAnimateDuration);
			((Control)groupBox4).set_Location(new Point(490, 126));
			((Control)groupBox4).set_Name("groupBox4");
			((Control)groupBox4).set_Size(new Size(295, 96));
			((Control)groupBox4).set_TabIndex(4);
			groupBox4.set_TabStop(false);
			((Control)groupBox4).set_Text("Animation");
			((Control)chkAutoSave).set_AutoSize(true);
			((Control)chkAutoSave).set_Enabled(false);
			((Control)chkAutoSave).set_Location(new Point(111, 74));
			((Control)chkAutoSave).set_Name("chkAutoSave");
			((Control)chkAutoSave).set_Size(new Size(74, 17));
			((Control)chkAutoSave).set_TabIndex(9);
			((Control)chkAutoSave).set_Text("Auto-save");
			((ButtonBase)chkAutoSave).set_UseVisualStyleBackColor(true);
			((Control)chkAnimation).set_AutoSize(true);
			((Control)chkAnimation).set_Location(new Point(14, 73));
			((Control)chkAnimation).set_Name("chkAnimation");
			((Control)chkAnimation).set_Size(new Size(89, 17));
			((Control)chkAnimation).set_TabIndex(8);
			((Control)chkAnimation).set_Text("Animation On");
			((ButtonBase)chkAnimation).set_UseVisualStyleBackColor(true);
			chkAnimation.add_CheckedChanged((EventHandler)chkAnimation_CheckedChanged);
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label22).set_Location(new Point(142, 14));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(33, 13));
			((Control)label22).set_TabIndex(4);
			((Control)label22).set_Text("Stop");
			((Control)cmdResume10Back).set_Enabled(false);
			((Control)cmdResume10Back).set_Location(new Point(206, 49));
			((Control)cmdResume10Back).set_Name("cmdResume10Back");
			((Control)cmdResume10Back).set_Size(new Size(80, 40));
			((Control)cmdResume10Back).set_TabIndex(7);
			((Control)cmdResume10Back).set_Text("Resume - 10\r\nsteps back");
			((ButtonBase)cmdResume10Back).set_UseVisualStyleBackColor(true);
			((Control)cmdResume10Back).add_Click((EventHandler)cmdResume10Back_Click);
			((Control)cmdPause).set_Enabled(false);
			((Control)cmdPause).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdPause).set_Location(new Point(204, 17));
			((Control)cmdPause).set_Name("cmdPause");
			((Control)cmdPause).set_Size(new Size(70, 20));
			((Control)cmdPause).set_TabIndex(6);
			((Control)cmdPause).set_Text("Pause");
			((ButtonBase)cmdPause).set_UseVisualStyleBackColor(true);
			((Control)cmdPause).add_Click((EventHandler)cmdPause_Click);
			((Control)cmdCancel).set_Location(new Point(137, 30));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(55, 35));
			((Control)cmdCancel).set_TabIndex(5);
			((Control)cmdCancel).set_Text("...");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label20).set_Location(new Point(8, 23));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(50, 13));
			((Control)label20).set_TabIndex(0);
			((Control)label20).set_Text("Step size");
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label14).set_Location(new Point(9, 48));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(47, 13));
			((Control)label14).set_TabIndex(2);
			((Control)label14).set_Text("Duration");
			updnAnimateStep.set_DecimalPlaces(1);
			((Control)updnAnimateStep).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnAnimateStep).set_Location(new Point(64, 20));
			updnAnimateStep.set_Maximum(new decimal(new int[4] { 60, 0, 0, 0 }));
			((Control)updnAnimateStep).set_Name("updnAnimateStep");
			((Control)updnAnimateStep).set_Size(new Size(46, 20));
			((Control)updnAnimateStep).set_TabIndex(1);
			updnAnimateStep.set_Value(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnAnimateDuration).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnAnimateDuration).set_Location(new Point(62, 46));
			updnAnimateDuration.set_Maximum(new decimal(new int[4] { 5000, 0, 0, 0 }));
			((Control)updnAnimateDuration).set_Name("updnAnimateDuration");
			((Control)updnAnimateDuration).set_Size(new Size(61, 20));
			((Control)updnAnimateDuration).set_TabIndex(3);
			updnAnimateDuration.add_ValueChanged((EventHandler)updnAnimateDuration_ValueChanged);
			((Control)groupBox5).get_Controls().Add((Control)(object)label24);
			((Control)groupBox5).get_Controls().Add((Control)(object)label23);
			((Control)groupBox5).get_Controls().Add((Control)(object)txtCursorHeight);
			((Control)groupBox5).get_Controls().Add((Control)(object)txtCursorAA);
			((Control)groupBox5).set_Location(new Point(289, 238));
			((Control)groupBox5).set_Name("groupBox5");
			((Control)groupBox5).set_Size(new Size(191, 52));
			((Control)groupBox5).set_TabIndex(5);
			groupBox5.set_TabStop(false);
			((Control)groupBox5).set_Text("Cursor location on Eclipse image");
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label24).set_Location(new Point(6, 29));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(21, 13));
			((Control)label24).set_TabIndex(0);
			((Control)label24).set_Text("AA");
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label23).set_Location(new Point(91, 29));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(38, 13));
			((Control)label23).set_TabIndex(2);
			((Control)label23).set_Text("Height");
			((Control)txtCursorHeight).set_Location(new Point(135, 24));
			((Control)txtCursorHeight).set_Name("txtCursorHeight");
			((Control)txtCursorHeight).set_Size(new Size(50, 20));
			((Control)txtCursorHeight).set_TabIndex(3);
			((Control)txtCursorAA).set_Location(new Point(36, 26));
			((Control)txtCursorAA).set_Name("txtCursorAA");
			((Control)txtCursorAA).set_Size(new Size(50, 20));
			((Control)txtCursorAA).set_TabIndex(1);
			((Control)groupBox6).get_Controls().Add((Control)(object)label30);
			((Control)groupBox6).get_Controls().Add((Control)(object)label29);
			((Control)groupBox6).get_Controls().Add((Control)(object)label28);
			((Control)groupBox6).get_Controls().Add((Control)(object)label27);
			((Control)groupBox6).get_Controls().Add((Control)(object)label26);
			((Control)groupBox6).get_Controls().Add((Control)(object)txtL);
			((Control)groupBox6).get_Controls().Add((Control)(object)txtB);
			((Control)groupBox6).get_Controls().Add((Control)(object)txt_y);
			((Control)groupBox6).get_Controls().Add((Control)(object)txt_x);
			((Control)groupBox6).get_Controls().Add((Control)(object)txtC);
			((Control)groupBox6).set_Location(new Point(12, 238));
			((Control)groupBox6).set_Name("groupBox6");
			((Control)groupBox6).set_Size(new Size(267, 52));
			((Control)groupBox6).set_TabIndex(6);
			groupBox6.set_TabStop(false);
			((Control)groupBox6).set_Text("Librations and Moon-Sun offset");
			((Control)label30).set_AutoSize(true);
			((Control)label30).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label30).set_Location(new Point(227, 16));
			((Control)label30).set_Name("label30");
			((Control)label30).set_Size(new Size(17, 13));
			((Control)label30).set_TabIndex(9);
			((Control)label30).set_Text("y\"");
			((Control)label29).set_AutoSize(true);
			((Control)label29).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label29).set_Location(new Point(183, 16));
			((Control)label29).set_Name("label29");
			((Control)label29).set_Size(new Size(17, 13));
			((Control)label29).set_TabIndex(8);
			((Control)label29).set_Text("x\"");
			((Control)label28).set_AutoSize(true);
			((Control)label28).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label28).set_Location(new Point(110, 16));
			((Control)label28).set_Name("label28");
			((Control)label28).set_Size(new Size(14, 13));
			((Control)label28).set_TabIndex(7);
			((Control)label28).set_Text("C");
			((Control)label27).set_AutoSize(true);
			((Control)label27).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label27).set_Location(new Point(62, 16));
			((Control)label27).set_Name("label27");
			((Control)label27).set_Size(new Size(14, 13));
			((Control)label27).set_TabIndex(6);
			((Control)label27).set_Text("B");
			((Control)label26).set_AutoSize(true);
			((Control)label26).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label26).set_Location(new Point(19, 16));
			((Control)label26).set_Name("label26");
			((Control)label26).set_Size(new Size(13, 13));
			((Control)label26).set_TabIndex(5);
			((Control)label26).set_Text("L");
			((Control)txtL).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtL).set_Location(new Point(6, 29));
			((Control)txtL).set_Name("txtL");
			((Control)txtL).set_Size(new Size(38, 20));
			((Control)txtL).set_TabIndex(0);
			((Control)txtB).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtB).set_Location(new Point(50, 29));
			((Control)txtB).set_Name("txtB");
			((Control)txtB).set_Size(new Size(38, 20));
			((Control)txtB).set_TabIndex(1);
			((Control)txt_y).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txt_y).set_Location(new Point(216, 29));
			((Control)txt_y).set_Name("txt_y");
			((Control)txt_y).set_Size(new Size(38, 20));
			((Control)txt_y).set_TabIndex(4);
			((Control)txt_x).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txt_x).set_Location(new Point(172, 29));
			((Control)txt_x).set_Name("txt_x");
			((Control)txt_x).set_Size(new Size(38, 20));
			((Control)txt_x).set_TabIndex(3);
			((Control)txtC).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtC).set_Location(new Point(94, 29));
			((Control)txtC).set_Name("txtC");
			((Control)txtC).set_Size(new Size(47, 20));
			((Control)txtC).set_TabIndex(2);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(789, 294));
			((Control)this).get_Controls().Add((Control)(object)groupBox6);
			((Control)this).get_Controls().Add((Control)(object)groupBox5);
			((Control)this).get_Controls().Add((Control)(object)groupBox4);
			((Control)this).get_Controls().Add((Control)(object)cmdPlot);
			((Control)this).get_Controls().Add((Control)(object)groupBox2);
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Control)this).get_Controls().Add((Control)(object)grpObserver);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationBailyMain", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Location(Settings.Default.LocationBailyMain);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("BailyBeadsMain");
			((Control)this).set_Text("Baily Beads    Main form");
			((Form)this).add_FormClosing(new FormClosingEventHandler(BailyBeadsMain_FormClosing));
			((Form)this).add_Load((EventHandler)BailyBeadsMain_Load);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)grpObserver).ResumeLayout(false);
			((Control)grpObserver).PerformLayout();
			((Control)panelDDD).ResumeLayout(false);
			((Control)panelDDD).PerformLayout();
			((Control)panelDMM).ResumeLayout(false);
			((Control)panelDMM).PerformLayout();
			((Control)panelDMS).ResumeLayout(false);
			((Control)panelDMS).PerformLayout();
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((ISupportInitialize)updnMagnification).EndInit();
			((ISupportInitialize)updnDecOffset).EndInit();
			((ISupportInitialize)updnSolarRadius).EndInit();
			((ISupportInitialize)updnRAOffset).EndInit();
			((ISupportInitialize)updnAxisAngle).EndInit();
			((Control)groupBox2).ResumeLayout(false);
			((Control)groupBox2).PerformLayout();
			((ISupportInitialize)updnSec).EndInit();
			((Control)groupBox3).ResumeLayout(false);
			((Control)groupBox3).PerformLayout();
			((ISupportInitialize)updnStepSize).EndInit();
			((Control)groupBox4).ResumeLayout(false);
			((Control)groupBox4).PerformLayout();
			((ISupportInitialize)updnAnimateStep).EndInit();
			((ISupportInitialize)updnAnimateDuration).EndInit();
			((Control)groupBox5).ResumeLayout(false);
			((Control)groupBox5).PerformLayout();
			((Control)groupBox6).ResumeLayout(false);
			((Control)groupBox6).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
