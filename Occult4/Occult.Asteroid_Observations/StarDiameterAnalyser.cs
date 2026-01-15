using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Asteroid_Observations
{
	public class StarDiameterAnalyser : Form
	{
		internal static double[] Gauss = new double[81]
		{
			0.01, 0.02, 0.03, 0.04, 0.06, 0.09, 0.12, 0.17, 0.24, 0.33,
			0.44, 0.6, 0.79, 1.04, 1.36, 1.75, 2.24, 2.83, 3.55, 4.4,
			5.4, 6.56, 7.9, 9.4, 11.09, 12.95, 14.97, 17.14, 19.42, 21.79,
			24.2, 26.61, 28.97, 31.23, 33.32, 35.21, 36.83, 38.14, 39.1, 39.7,
			39.89, 39.7, 39.1, 38.14, 36.83, 35.21, 33.32, 31.23, 28.97, 26.61,
			24.2, 21.79, 19.42, 17.14, 14.97, 12.95, 11.09, 9.4, 7.9, 6.56,
			5.4, 4.4, 3.55, 2.83, 2.24, 1.75, 1.36, 1.04, 0.79, 0.6,
			0.44, 0.33, 0.24, 0.17, 0.12, 0.09, 0.06, 0.04, 0.03, 0.02,
			0.01
		};

		internal static float[] ObservedD = new float[600];

		internal static float[] ObservedR = new float[600];

		internal static double[] Residuals = new double[600];

		internal static int[] HistogramData = new int[21];

		internal static int[] HistogramDataNormal = new int[21];

		internal static double AverageResidual = 0.0;

		internal static double SDeviation = 0.1;

		internal static string[] ObservedDTime = new string[600];

		internal static string[] ObservedRTime = new string[600];

		internal static int Dcount = 0;

		internal static int Rcount = 0;

		private static bool IsPlotting = false;

		internal static List<StarDiameterData> DiameterData = new List<StarDiameterData>();

		private string Current_StarDia_File = "";

		private static bool LoadingEvent = false;

		private static int Xrev0 = -1;

		private static int Yrev0;

		private static int Xrev1;

		private static int Yrev1;

		private IContainer components;

		internal GroupBox grpD;

		internal GroupBox grpR;

		internal TextBox txtMotionD;

		internal TextBox txtMotionR;

		private Label label6;

		private Label label3;

		private Label label5;

		private Label label4;

		private GroupBox grpStar;

		private Label label7;

		private GroupBox groupBox1;

		internal PictureBox picD;

		internal PictureBox picR;

		private Button cmdPlot;

		private Label label1;

		private Button cmdPasteD;

		private Button cmdPasteR;

		private Label label2;

		private Label label9;

		private Label label10;

		private MenuStrip menuStrip1;

		private Button cmdAddObserver;

		private Button cmdUpdate;

		private ToolStripMenuItem fileToolStripMenuItem;

		private ToolStripMenuItem openToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem saveAsToolStripMenuItem;

		private ToolStripMenuItem newToolStripMenuItem;

		private Button cmdDelete;

		private GroupBox groupBox2;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripMenuItem copyFormToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private Button cmdPlotAll;

		internal ListBox lstObservers;

		internal NumericUpDown updnSecsToDisplay;

		internal NumericUpDown updnDAdjust;

		internal NumericUpDown updnRAdjust;

		internal NumericUpDown updnPlotScale;

		internal Label lblMidD;

		internal Label lblMidR;

		internal RadioButton optSmall;

		internal RadioButton optLarge;

		private GroupBox groupBox3;

		private GroupBox grpCalibrateD;

		private RadioButton optSetBottomD;

		private RadioButton optSetTopD;

		private GroupBox grpCalibrateR;

		private RadioButton optSetBottomR;

		private RadioButton optSetTopR;

		private TextBox txtTopDSD;

		private TextBox txtBottomDSD;

		private Button cmdCalibrateD;

		private Button cmdCalibrateR;

		private TextBox txtTopRSD;

		private TextBox txtBottomRSD;

		internal TextBox txtTop2D;

		internal TextBox txtBottom2D;

		internal TextBox txtBottom1D;

		internal TextBox txtTop1D;

		internal TextBox txtTop2R;

		internal TextBox txtBottom2R;

		internal TextBox txtBottom1R;

		internal TextBox txtTop1R;

		internal CheckBox chkShowRegions;

		internal TextBox txtMeas2D;

		internal TextBox txtMeas1D;

		private RadioButton optSetMeasureD;

		internal TextBox txtMeas2R;

		internal TextBox txtMeas1R;

		private RadioButton optSetMeasureR;

		private GroupBox groupBox4;

		private ComboBox cmbSaturation;

		private PictureBox picStar;

		private Label label8;

		private Label label11;

		private Label label13;

		private Label label15;

		private GroupBox grpSolveD;

		private GroupBox grpSolveR;

		internal NumericUpDown updnDiaD;

		private Label label16;

		internal NumericUpDown updnDiaR;

		private Label label17;

		private Label label18;

		private Label label19;

		private Label lblChi2Ddia2;

		private Label lblChi2Ddia1;

		private Label lblChi2Ddia0;

		private Label lblChi2Dframe2;

		private Label lblChi2Dframe1;

		private Label lblChi2Dframe0;

		private Label lblChi2Rdia2;

		private Label lblChi2Rdia1;

		private Label lblChi2Rdia0;

		private Label lblChi2Rframe2;

		private Label lblChi2Rframe1;

		private Label lblChi2Rframe0;

		private Label label30;

		private Label label20;

		private Label label23;

		private Label label21;

		private TextBox txtCommentD;

		private Label label22;

		private TextBox txtCommentR;

		private Label label28;

		private Label label25;

		private Label label27;

		private Label label26;

		private Label label24;

		private Label label29;

		private Label label31;

		internal TextBox txtCurveNos;

		internal TextBox txtStarUncertainty;

		internal TextBox txtStarSolution;

		private Label label12;

		private Label lblDiffD;

		private Label lblDiffR;

		internal NumericUpDown updnWeightD;

		internal NumericUpDown updnWeightR;

		private Label label14;

		private Label label32;

		private Label label35;

		private Label label34;

		private Label label33;

		private Label label36;

		private Label label37;

		private Label label38;

		private Panel panel2;

		private Panel panel3;

		private Label txtSDD;

		private Label txtSDR;

		private CheckBox chkx10D;

		private CheckBox chkx10R;

		internal PictureBox picResD;

		internal PictureBox picResR;

		private Panel panel4;

		internal PictureBox picNormD;

		internal PictureBox picNormR;

		private ComboBox cmbDRateAccuracy;

		private ComboBox cmbRRateAccuracy;

		internal CheckBox chkLightCurve;

		private Label label39;

		private Label label40;

		internal CheckBox chkPartial;

		private Label label41;

		internal Label lblD;

		internal TextBox txtObserver;

		private Label label42;

		internal TextBox txtFrameRate;

		internal CheckBox chkLimbDarkening;

		public StarDiameterAnalyser()
		{
			InitializeComponent();
		}

		private void StarDiameterAnalyser_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			((ListControl)cmbSaturation).set_SelectedIndex(0);
			ComboBox obj = cmbDRateAccuracy;
			int selectedIndex;
			((ListControl)cmbRRateAccuracy).set_SelectedIndex(selectedIndex = 0);
			((ListControl)obj).set_SelectedIndex(selectedIndex);
		}

		private void cmdPlot_Click(object sender, EventArgs e)
		{
			Plot();
			GetDiameterSolution();
		}

		internal void Plot()
		{
			if (!LoadingEvent && !IsPlotting)
			{
				IsPlotting = true;
				StarDiameterAnalysis.PlotEventOnScreen(Devent: true);
				StarDiameterAnalysis.PlotEventOnScreen(Devent: false);
				IsPlotting = false;
				GetDiameterSolution();
			}
		}

		private void cmdPasteD_Click(object sender, EventArgs e)
		{
			//IL_000b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0011: Invalid comparison between Unknown and I4
			//IL_00b0: Unknown result type (might be due to invalid IL or missing references)
			//IL_00fc: Unknown result type (might be due to invalid IL or missing references)
			int num = 10;
			bool flag = false;
			if ((int)((Form)StarDiameterAnalysis.StarDiameter_Paste).ShowDialog() == 2)
			{
				return;
			}
			flag = Settings.Default.CSVPasteLiMovie;
			num = (flag ? (Settings.Default.CSVPaste3 ? 12 : ((!Settings.Default.CSVPaste2) ? 10 : 11)) : (Settings.Default.CSVPaste4 ? 8 : (Settings.Default.CSVPaste3 ? 6 : ((!Settings.Default.CSVPaste2) ? 2 : 4))));
			string[] array = Clipboard.GetText((TextDataFormat)0).Split(new char[1] { '\n' });
			Dcount = array.GetUpperBound(0);
			if (Dcount < 20)
			{
				MessageBox.Show("At least 20 lines from the csv file must be pasted");
				return;
			}
			for (int i = 0; i < Dcount && i < ObservedD.GetUpperBound(0); i++)
			{
				string[] array2 = array[i].Split(new char[1] { ',' });
				if (num >= array2.GetUpperBound(0))
				{
					if (!flag)
					{
						MessageBox.Show("Object is not in Tangra CSV file");
					}
					return;
				}
				if (!float.TryParse(array2[num], out var result))
				{
					result = 0f;
				}
				if (!flag)
				{
					if (!float.TryParse(array2[num + 1], out var result2))
					{
						result2 = 0f;
					}
					result -= result2;
				}
				ObservedD[i] = result;
				ObservedDTime[i] = array2[0];
				if (ObservedDTime[i].Substring(0, 1) == "\n")
				{
					ObservedDTime[i] = ObservedDTime[i].Substring(1);
				}
			}
			float num2 = 0f;
			for (int j = 0; j < 10; j++)
			{
				num2 += ObservedD[j];
			}
			num2 /= 10f;
			for (int k = 0; k < Dcount; k++)
			{
				ObservedD[k] /= num2;
			}
			updnDAdjust.set_Value(0m);
			Plot();
		}

		internal void fromAOTA()
		{
			//IL_00d1: Unknown result type (might be due to invalid IL or missing references)
			float num = 0f;
			for (int i = 0; i < 10; i++)
			{
				num += ObservedD[i];
			}
			num /= 10f;
			for (int j = 0; j < Dcount; j++)
			{
				ObservedD[j] /= num;
			}
			updnDAdjust.set_Value(0m);
			num = 0f;
			for (int k = Rcount - 10; k < Rcount; k++)
			{
				num += ObservedR[k];
			}
			num /= 10f;
			for (int l = 0; l < Rcount; l++)
			{
				ObservedR[l] /= num;
			}
			updnRAdjust.set_Value(0m);
			((Control)this).Focus();
			Plot();
			MessageBox.Show("Data has been transferred from AOTA. To complete the record you must:\r\n\r\n1. Calibrate the two light curves using the boxes below each plot\r\n2. Add observer details and normal motion, from the plot of the asteroid profile\r\n3. Confirm the PAL/NTSC setting, and the Frames/Fields setting\r\n4. Add the details by clicking 'Add as new record', or by Updating the record\r\n5. + don't forget to Save the Star Diameter data", "Data added froam AOTA", (MessageBoxButtons)0);
		}

		private void cmdPasteR_Click(object sender, EventArgs e)
		{
			//IL_000b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0011: Invalid comparison between Unknown and I4
			//IL_00b0: Unknown result type (might be due to invalid IL or missing references)
			//IL_0126: Unknown result type (might be due to invalid IL or missing references)
			int num = 10;
			bool flag = false;
			if ((int)((Form)StarDiameterAnalysis.StarDiameter_Paste).ShowDialog() == 2)
			{
				return;
			}
			flag = Settings.Default.CSVPasteLiMovie;
			num = (flag ? (Settings.Default.CSVPaste3 ? 12 : ((!Settings.Default.CSVPaste2) ? 10 : 11)) : (Settings.Default.CSVPaste4 ? 8 : (Settings.Default.CSVPaste3 ? 6 : ((!Settings.Default.CSVPaste2) ? 2 : 4))));
			string[] array = Clipboard.GetText((TextDataFormat)0).Split(new char[1] { '\r' });
			Rcount = array.GetUpperBound(0);
			if (Rcount < 20)
			{
				MessageBox.Show("At least 20 lines from the csv file must be pasted");
				return;
			}
			for (int i = 0; i < Rcount && i < ObservedR.GetUpperBound(0); i++)
			{
				string[] array2 = array[i].Split(new char[1] { ',' });
				double result = 0.0;
				if (!double.TryParse(array2[0].Replace("\n", ""), out result))
				{
					continue;
				}
				if (num >= array2.GetUpperBound(0))
				{
					if (!flag)
					{
						MessageBox.Show("Object is not in Tangra CSV file");
					}
					return;
				}
				if (!float.TryParse(array2[num], out var result2))
				{
					result2 = 0f;
				}
				if (!flag)
				{
					if (!float.TryParse(array2[num + 1], out var result3))
					{
						result3 = 0f;
					}
					result2 -= result3;
				}
				ObservedR[i] = result2;
				ObservedRTime[i] = array2[0];
				if (ObservedRTime[i].Substring(0, 1) == "\n")
				{
					ObservedRTime[i] = ObservedRTime[i].Substring(1);
				}
			}
			float num2 = 0f;
			for (int j = Rcount - 10; j < Rcount; j++)
			{
				num2 += ObservedR[j];
			}
			num2 /= 10f;
			for (int k = 0; k < Rcount; k++)
			{
				ObservedR[k] /= num2;
			}
			updnDAdjust.set_Value(0m);
			Plot();
		}

		private void updnDAdjust_ValueChanged(object sender, EventArgs e)
		{
			if (!LoadingEvent)
			{
				GetChi2FrameD();
				DrawHistogram(Dcurve: true, GetHistogramData: false);
			}
		}

		private void GetChi2FrameD()
		{
			((Control)lblMidD).set_Text(ObservedDTime[Dcount / 2 + (int)updnDAdjust.get_Value()]);
			Plot();
			double num = Chi2D(-1, (double)updnDiaD.get_Value(), GetHistogramData: false);
			double num2 = Chi2D(0, (double)updnDiaD.get_Value(), GetHistogramData: true);
			double num3 = Chi2D(1, (double)updnDiaD.get_Value(), GetHistogramData: false);
			if (Math.Abs(num) > 99.0)
			{
				num = 99.0;
			}
			if (Math.Abs(num2) > 99.0)
			{
				num = 99.0;
			}
			if (Math.Abs(num3) > 99.0)
			{
				num = 99.0;
			}
			((Control)lblChi2Dframe0).set_Text(string.Format("{0,1:f4}", num));
			((Control)lblChi2Dframe1).set_Text(string.Format("{0,1:f4}", num2));
			((Control)lblChi2Dframe2).set_Text(string.Format("{0,1:f4}", num3));
		}

		private void updnRAdjust_ValueChanged(object sender, EventArgs e)
		{
			if (!LoadingEvent)
			{
				GetChi2FrameR();
				DrawHistogram(Dcurve: false, GetHistogramData: false);
			}
		}

		private void GetChi2FrameR()
		{
			if (Rcount >= 1)
			{
				((Control)lblMidR).set_Text(ObservedRTime[Rcount / 2 + (int)updnRAdjust.get_Value()]);
				Plot();
				double num = Chi2R(-1, (double)updnDiaR.get_Value(), GetHistogramData: false);
				double num2 = Chi2R(0, (double)updnDiaR.get_Value(), GetHistogramData: true);
				double num3 = Chi2R(1, (double)updnDiaR.get_Value(), GetHistogramData: false);
				if (Math.Abs(num) > 99.0)
				{
					num = 99.0;
				}
				if (Math.Abs(num2) > 99.0)
				{
					num = 99.0;
				}
				if (Math.Abs(num3) > 99.0)
				{
					num = 99.0;
				}
				((Control)lblChi2Rframe0).set_Text(string.Format("{0,1:f4}", num));
				((Control)lblChi2Rframe1).set_Text(string.Format("{0,1:f4}", num2));
				((Control)lblChi2Rframe2).set_Text(string.Format("{0,1:f4}", num3));
			}
		}

		private void updnDiaD_ValueChanged(object sender, EventArgs e)
		{
			if (!LoadingEvent)
			{
				GetChi2DiaD();
				DrawHistogram(Dcurve: true, GetHistogramData: false);
			}
		}

		private void GetChi2DiaD()
		{
			Plot();
			double num = Chi2D(0, (double)updnDiaD.get_Value() - 0.1, GetHistogramData: false);
			double num2 = Chi2D(0, (double)updnDiaD.get_Value(), GetHistogramData: true);
			double num3 = Chi2D(0, (double)updnDiaD.get_Value() + 0.1, GetHistogramData: false);
			if (Math.Abs(num) > 99.0)
			{
				num = 99.0;
			}
			if (Math.Abs(num2) > 99.0)
			{
				num = 99.0;
			}
			if (Math.Abs(num3) > 99.0)
			{
				num = 99.0;
			}
			((Control)lblChi2Ddia0).set_Text(string.Format("{0,1:f4}", num));
			((Control)lblChi2Ddia1).set_Text(string.Format("{0,1:f4}", num2));
			((Control)lblChi2Ddia2).set_Text(string.Format("{0,1:f4}", num3));
			((Control)lblDiffD).set_Text(string.Format("{0,1:f4}", (num + num3) / 2.0 - num2));
		}

		private void updnDiaR_ValueChanged(object sender, EventArgs e)
		{
			if (!LoadingEvent)
			{
				GetChi2DiaR();
				DrawHistogram(Dcurve: false, GetHistogramData: false);
			}
		}

		private void GetChi2DiaR()
		{
			Plot();
			double num = Chi2R(0, (double)updnDiaR.get_Value() - 0.1, GetHistogramData: false);
			double num2 = Chi2R(0, (double)updnDiaR.get_Value(), GetHistogramData: true);
			double num3 = Chi2R(0, (double)updnDiaR.get_Value() + 0.1, GetHistogramData: false);
			if (Math.Abs(num) > 99.0)
			{
				num = 99.0;
			}
			if (Math.Abs(num2) > 99.0)
			{
				num = 99.0;
			}
			if (Math.Abs(num3) > 99.0)
			{
				num = 99.0;
			}
			((Control)lblChi2Rdia0).set_Text(string.Format("{0,1:f4}", num));
			((Control)lblChi2Rdia1).set_Text(string.Format("{0,1:f4}", num2));
			((Control)lblChi2Rdia2).set_Text(string.Format("{0,1:f4}", num3));
			((Control)lblDiffR).set_Text(string.Format("{0,1:f4}", (num + num3) / 2.0 - num2));
		}

		private double Chi2D(int FrameOffset, double StarDia, bool GetHistogramData)
		{
			double num = 0.0;
			double result = 0.5;
			double result2 = 0.1;
			double num2 = 1.0;
			double result3 = 1.0;
			double num3 = 0.1;
			double num4 = 1.0;
			int num5 = 0;
			int num6 = 10;
			int num7 = 10;
			int num8 = 0;
			AverageResidual = 0.0;
			double num9 = 0.0;
			if (GetHistogramData)
			{
				IntialiseHistogramData();
			}
			if (!double.TryParse(((Control)txtMotionD).get_Text(), out result3))
			{
				result3 = 0.1;
			}
			int num10 = int.Parse(((Control)txtMeas1D).get_Text());
			int num11 = int.Parse(((Control)txtMeas2D).get_Text());
			if (!double.TryParse(((Control)txtBottomDSD).get_Text(), out result2))
			{
				result2 = 0.1;
			}
			if (result2 < 0.01)
			{
				result2 = 0.01;
			}
			if (!double.TryParse(((Control)txtTopDSD).get_Text(), out result))
			{
				result = 0.1;
			}
			num3 = result2 * result2;
			num4 = result * result - num3;
			int num12 = (int)((decimal)(3000 - Dcount * 5) - 10m * updnDAdjust.get_Value() + (decimal)(FrameOffset * 10));
			StarDiameterAnalysis.GenerateTheoreticalLightCurve(StarDia, result3, Devent: true);
			for (int i = num10; i <= num11; i++)
			{
				if (!((10 * i + num12 >= 0) & (10 * i + num12 <= StarDiameterAnalysis.Theoretical.GetUpperBound(0))))
				{
					continue;
				}
				num2 = Math.Sqrt(num3 + (double)StarDiameterAnalysis.Theoretical[10 * i + num12] * num4);
				double num13 = ObservedD[i] - StarDiameterAnalysis.Theoretical[10 * i + num12];
				num += num13 * num13 / num2 / num2;
				if (GetHistogramData)
				{
					num6 = Convert.ToInt16(10.0 + 10.0 * num13);
					if ((num6 >= 0) & (num6 <= HistogramData.GetUpperBound(0)))
					{
						HistogramData[num6]++;
						Residuals[num8] = num13;
						num8++;
					}
					if (num2 > 0.0)
					{
						num7 = Convert.ToInt16(10.0 + num13 / num2 / 0.4);
						if ((num7 >= 0) & (num7 <= HistogramDataNormal.GetUpperBound(0)))
						{
							HistogramDataNormal[num7]++;
						}
					}
				}
				num5++;
			}
			if (GetHistogramData)
			{
				for (int j = 0; j < num8; j++)
				{
					AverageResidual += Residuals[j];
				}
				AverageResidual /= num8;
				for (int k = 0; k < num8; k++)
				{
					num9 += (Residuals[k] - AverageResidual) * (Residuals[k] - AverageResidual);
				}
				SDeviation = Math.Sqrt(num9 / (double)num8);
				((Control)txtSDD).set_Text(string.Format("SDev={0,1:f3}", SDeviation));
			}
			return num / (double)(num5 - 2);
		}

		private double Chi2R(int FrameOffset, double StarDia, bool GetHistogramData)
		{
			double num = 0.0;
			double result = 0.5;
			double result2 = 0.1;
			double num2 = 1.0;
			double result3 = 1.0;
			double num3 = 0.1;
			double num4 = 1.0;
			int num5 = 0;
			int num6 = 10;
			int num7 = 10;
			int num8 = 0;
			AverageResidual = 0.0;
			double num9 = 0.0;
			if (GetHistogramData)
			{
				IntialiseHistogramData();
			}
			if (!double.TryParse(((Control)txtMotionR).get_Text(), out result3))
			{
				result3 = 0.1;
			}
			int num10 = int.Parse(((Control)txtMeas1R).get_Text());
			int num11 = int.Parse(((Control)txtMeas2R).get_Text());
			if (num10 < 0 || num11 < 0)
			{
				return 0.0;
			}
			if (!double.TryParse(((Control)txtBottomRSD).get_Text(), out result2))
			{
				result2 = 0.1;
			}
			if (result2 < 0.01)
			{
				result2 = 0.01;
			}
			if (!double.TryParse(((Control)txtTopRSD).get_Text(), out result))
			{
				result = 0.1;
			}
			num3 = result2 * result2;
			num4 = result * result - num3;
			int num12 = (int)((decimal)(3000 - Rcount * 5) - 10m * updnRAdjust.get_Value() + (decimal)(FrameOffset * 10));
			StarDiameterAnalysis.GenerateTheoreticalLightCurve(StarDia, result3, Devent: false);
			for (int i = num10; i <= num11; i++)
			{
				if (!((10 * i + num12 >= 0) & (10 * i + num12 <= StarDiameterAnalysis.Theoretical.GetUpperBound(0))))
				{
					continue;
				}
				num2 = Math.Sqrt(num3 + (double)StarDiameterAnalysis.Theoretical[10 * i + num12] * num4);
				double num13 = ObservedR[i] - StarDiameterAnalysis.Theoretical[10 * i + num12];
				num += num13 * num13 / num2 / num2;
				if (GetHistogramData)
				{
					num6 = Convert.ToInt16(10.0 + 10.0 * num13);
					if ((num6 >= 0) & (num6 <= HistogramData.GetUpperBound(0)))
					{
						HistogramData[num6]++;
						Residuals[num8] = num13;
						num8++;
					}
					if (num2 > 0.0)
					{
						num7 = Convert.ToInt16(10.0 + num13 / num2 / 0.4);
						if ((num7 >= 0) & (num7 <= HistogramDataNormal.GetUpperBound(0)))
						{
							HistogramDataNormal[num7]++;
						}
					}
				}
				num5++;
			}
			if (GetHistogramData)
			{
				AverageResidual = 0.0;
				for (int j = 0; j < num8; j++)
				{
					AverageResidual += Residuals[j];
				}
				AverageResidual /= num8;
				num9 = 0.0;
				for (int k = 0; k < num8; k++)
				{
					num9 += (Residuals[k] - AverageResidual) * (Residuals[k] - AverageResidual);
				}
				SDeviation = Math.Sqrt(num9 / (double)num8);
				((Control)txtSDR).set_Text(string.Format("SDev={0,1:f3}", SDeviation));
			}
			return num / (double)(num5 - 2);
		}

		private void IntialiseHistogramData()
		{
			for (int i = 0; i <= HistogramData.GetUpperBound(0); i++)
			{
				HistogramData[i] = 0;
			}
			for (int j = 0; j <= HistogramDataNormal.GetUpperBound(0); j++)
			{
				HistogramDataNormal[j] = 0;
			}
		}

		internal void DrawHistogram(bool Dcurve, bool GetHistogramData)
		{
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 1.0;
			double num4 = 1.0;
			double num5 = 1.0;
			for (int i = 0; i <= 1; i++)
			{
				Bitmap bitmap = new Bitmap(((Control)picResD).get_Width(), ((Control)picResD).get_Height());
				Graphics graphics = Graphics.FromImage(bitmap);
				double num6 = (double)(bitmap.Width - 2) / (double)(HistogramData.GetUpperBound(0) + 1);
				int num7 = 10;
				if (GetHistogramData)
				{
					if (Dcurve)
					{
						Chi2D(0, (double)updnDiaD.get_Value(), GetHistogramData: true);
					}
					else
					{
						Chi2R(0, (double)updnDiaR.get_Value(), GetHistogramData: true);
					}
				}
				if (i == 1)
				{
					graphics.Clear(Color.LightCyan);
					for (int j = 0; j <= HistogramDataNormal.GetUpperBound(0); j++)
					{
						if (HistogramDataNormal[j] > num7)
						{
							num7 = HistogramDataNormal[j];
						}
					}
					num5 = (double)(bitmap.Height - 2) / (1.1 * (double)num7);
					for (int k = 0; k <= HistogramDataNormal.GetUpperBound(0); k++)
					{
						double num8 = ((double)k - 0.25) * num6 + 1.0;
						double num9 = (double)(bitmap.Height - 1) - (double)HistogramDataNormal[k] * num5;
						graphics.FillRectangle(Brushes.Blue, (float)num8, (float)num9, (float)num6, bitmap.Height - 1);
					}
					num3 = (double)(bitmap.Width - 2) / 8.0;
					if (SDeviation > 0.0)
					{
						num4 = (double)(bitmap.Height - 2) / 1.1 / Utilities.NormalDistributionValue(0.0, 0.0, 1.0);
						for (double num10 = -4.0; num10 <= 4.0; num10 += 0.2)
						{
							num = 0.9 * num4 * Utilities.NormalDistributionValue(num10, 0.0, 1.0);
							if (num10 > -4.0)
							{
								graphics.DrawLine(Pens.Red, (float)((num10 + 3.8) * num3), (float)((double)(bitmap.Height - 1) - num2), (float)((num10 + 4.0) * num3), (float)((double)(bitmap.Height - 1) - num));
							}
							num2 = num;
						}
					}
				}
				else
				{
					graphics.Clear(Color.LemonChiffon);
					for (int l = 0; l <= HistogramData.GetUpperBound(0); l++)
					{
						if (HistogramData[l] > num7)
						{
							num7 = HistogramData[l];
						}
					}
					num5 = (double)(bitmap.Height - 2) / (1.1 * (double)num7);
					for (int m = 0; m <= HistogramData.GetUpperBound(0); m++)
					{
						double num8 = ((double)m - 0.25) * num6 + 1.0;
						double num9 = (double)(bitmap.Height - 1) - (double)HistogramData[m] * num5;
						graphics.FillRectangle(Brushes.Blue, (float)num8, (float)num9, (float)num6, bitmap.Height - 1);
					}
					num3 = (double)(bitmap.Width - 2) / 2.0;
					if (SDeviation > 0.0)
					{
						num4 = (double)(bitmap.Height - 2) / 1.1 / Utilities.NormalDistributionValue(0.0, AverageResidual, SDeviation);
						if (num4 > 100.0)
						{
							num4 = 100.0;
						}
						for (double num11 = -1.0; num11 <= 1.0; num11 += 0.05)
						{
							num = num4 * Utilities.NormalDistributionValue(num11, AverageResidual, SDeviation);
							if (num11 > -1.0)
							{
								graphics.DrawLine(Pens.Red, (float)((num11 + 0.95) * num3), (float)((double)(bitmap.Height - 1) - num2), (float)((num11 + 1.0) * num3), (float)((double)(bitmap.Height - 1) - num));
							}
							num2 = num;
						}
					}
				}
				graphics.DrawRectangle(Pens.Black, 0, 0, bitmap.Width - 1, bitmap.Height - 1);
				graphics.DrawLine(Pens.Black, 5, 5, 10, 5);
				graphics.DrawLine(Pens.Black, bitmap.Width - 6, 5, bitmap.Width - 12, 5);
				graphics.DrawLine(Pens.Black, bitmap.Width - 9, 3, bitmap.Width - 9, 8);
				Font font = new Font("Times New Roman", 8f, FontStyle.Regular);
				new Font("Times New Roman", 6f, FontStyle.Regular);
				if (Dcurve)
				{
					graphics.DrawString("D", font, Brushes.Black, 2f, 15f);
				}
				else
				{
					graphics.DrawString("R", font, Brushes.Black, 2f, 15f);
				}
				if (i == 0)
				{
					if (Dcurve)
					{
						picResD.set_Image((Image)bitmap);
					}
					else
					{
						picResR.set_Image((Image)bitmap);
					}
				}
				else if (Dcurve)
				{
					picNormD.set_Image((Image)bitmap);
				}
				else
				{
					picNormR.set_Image((Image)bitmap);
				}
			}
		}

		private void updnDia_ValueChanged(object sender, EventArgs e)
		{
			SetFadeText();
			Plot();
		}

		private void updnSecsToDisplay_ValueChanged(object sender, EventArgs e)
		{
			Plot();
		}

		private void optNTSC_CheckedChanged(object sender, EventArgs e)
		{
			Plot();
		}

		private void optFrame_CheckedChanged(object sender, EventArgs e)
		{
			Plot();
		}

		private void updnPlotScale_ValueChanged(object sender, EventArgs e)
		{
			Plot();
		}

		private void SetFadeText()
		{
			if (!double.TryParse(((Control)txtMotionD).get_Text(), out var result))
			{
				result = 0.01;
			}
			if (!double.TryParse(((Control)txtMotionR).get_Text(), out result))
			{
				result = 0.01;
			}
		}

		private void cmdAddObserver_Click(object sender, EventArgs e)
		{
			StarDiameterData item = new StarDiameterData();
			DiameterData.Add(item);
			int record = lstObservers.get_Items().Add((object)((Control)txtObserver).get_Text());
			UpdateObserverRecord(record);
		}

		private void UpdateObserverRecord(int Record)
		{
			DiameterData[Record].Observer = ((Control)txtObserver).get_Text();
			DiameterData[Record].DCount = Dcount;
			for (int i = 0; i < Dcount; i++)
			{
				DiameterData[Record].ObservedD[i] = ObservedD[i];
				DiameterData[Record].ObservedDTime[i] = ObservedDTime[i];
			}
			DiameterData[Record].RCount = Rcount;
			for (int j = 0; j < Rcount; j++)
			{
				DiameterData[Record].ObservedR[j] = ObservedR[j];
				DiameterData[Record].ObservedRTime[j] = ObservedRTime[j];
			}
			DiameterData[Record].DAdjust = (float)updnDAdjust.get_Value();
			DiameterData[Record].RAdjust = (float)updnRAdjust.get_Value();
			float.TryParse(((Control)txtFrameRate).get_Text(), out var result);
			if (result == 0f)
			{
				result = 25f;
			}
			DiameterData[Record].FrameRate = result;
			DiameterData[Record].SecondsToDisplay = (float)updnSecsToDisplay.get_Value();
			DiameterData[Record].LimbDarken = 0;
			if (chkLimbDarkening.get_Checked())
			{
				DiameterData[Record].LimbDarken = 1;
			}
			DiameterData[Record].IsPartial = 0;
			if (chkPartial.get_Checked())
			{
				DiameterData[Record].IsPartial = 1;
			}
			DiameterData[Record].SaturationLevelIndex = ((ListControl)cmbSaturation).get_SelectedIndex();
			DiameterData[Record].DMotion = ((Control)txtMotionD).get_Text().Trim();
			DiameterData[Record].RMotion = ((Control)txtMotionR).get_Text().Trim();
			DiameterData[Record].DRateUncertainty = ((ListControl)cmbDRateAccuracy).get_SelectedIndex();
			DiameterData[Record].RRateUncertainty = ((ListControl)cmbRRateAccuracy).get_SelectedIndex();
			DiameterData[Record].Top1D = int.Parse(((Control)txtTop1D).get_Text());
			DiameterData[Record].Top2D = int.Parse(((Control)txtTop2D).get_Text());
			DiameterData[Record].Top1R = int.Parse(((Control)txtTop1R).get_Text());
			DiameterData[Record].Top2R = int.Parse(((Control)txtTop2R).get_Text());
			DiameterData[Record].TopDSD = double.Parse(((Control)txtTopDSD).get_Text());
			DiameterData[Record].TopRSD = double.Parse(((Control)txtTopRSD).get_Text());
			DiameterData[Record].Bottom1D = int.Parse(((Control)txtBottom1D).get_Text());
			DiameterData[Record].Bottom1R = int.Parse(((Control)txtBottom1R).get_Text());
			DiameterData[Record].Bottom2D = int.Parse(((Control)txtBottom2D).get_Text());
			DiameterData[Record].Bottom2R = int.Parse(((Control)txtBottom2R).get_Text());
			DiameterData[Record].BottomDSD = double.Parse(((Control)txtBottomDSD).get_Text());
			DiameterData[Record].BottomRSD = double.Parse(((Control)txtBottomRSD).get_Text());
			DiameterData[Record].Meas1D = int.Parse(((Control)txtMeas1D).get_Text());
			DiameterData[Record].Meas1R = int.Parse(((Control)txtMeas1R).get_Text());
			DiameterData[Record].Meas2D = int.Parse(((Control)txtMeas2D).get_Text());
			DiameterData[Record].Meas2R = int.Parse(((Control)txtMeas2R).get_Text());
			DiameterData[Record].DiaD = (double)updnDiaD.get_Value();
			DiameterData[Record].DiaR = (double)updnDiaR.get_Value();
			DiameterData[Record].WeightD = (int)updnWeightD.get_Value();
			DiameterData[Record].WeightR = (int)updnWeightR.get_Value();
			DiameterData[Record].CommentD = ((Control)txtCommentD).get_Text().Trim();
			DiameterData[Record].CommentR = ((Control)txtCommentR).get_Text().Trim();
			GetDiameterSolution();
		}

		private void lstObservers_SelectedIndexChanged(object sender, EventArgs e)
		{
			int selectedIndex = ((ListControl)lstObservers).get_SelectedIndex();
			if (selectedIndex >= 0)
			{
				LoadingEvent = true;
				SetFieldsforObserverRecord(selectedIndex);
				LoadingEvent = false;
				GetChi2DiaD();
				GetChi2FrameD();
				DrawHistogram(Dcurve: true, GetHistogramData: false);
				GetChi2DiaR();
				GetChi2FrameR();
				DrawHistogram(Dcurve: false, GetHistogramData: false);
			}
		}

		internal void SetFieldsforObserverRecord(int Record)
		{
			((Control)txtObserver).set_Text(DiameterData[Record].Observer);
			Dcount = DiameterData[Record].DCount;
			for (int i = 0; i < Dcount; i++)
			{
				ObservedD[i] = DiameterData[Record].ObservedD[i];
				ObservedDTime[i] = DiameterData[Record].ObservedDTime[i];
			}
			Rcount = DiameterData[Record].RCount;
			for (int j = 0; j < Rcount; j++)
			{
				ObservedR[j] = DiameterData[Record].ObservedR[j];
				ObservedRTime[j] = DiameterData[Record].ObservedRTime[j];
			}
			updnDAdjust.set_Value((decimal)DiameterData[Record].DAdjust);
			updnRAdjust.set_Value((decimal)DiameterData[Record].RAdjust);
			((Control)txtFrameRate).set_Text(string.Format("{0,1:f3}", DiameterData[Record].FrameRate));
			chkLimbDarkening.set_Checked(DiameterData[Record].LimbDarken == 1);
			chkPartial.set_Checked(DiameterData[Record].IsPartial == 1);
			((ListControl)cmbSaturation).set_SelectedIndex(DiameterData[Record].SaturationLevelIndex);
			updnSecsToDisplay.set_Value((decimal)DiameterData[Record].SecondsToDisplay);
			((Control)txtMotionD).set_Text(DiameterData[Record].DMotion);
			((Control)txtMotionR).set_Text(DiameterData[Record].RMotion);
			((ListControl)cmbDRateAccuracy).set_SelectedIndex(DiameterData[Record].DRateUncertainty);
			((ListControl)cmbRRateAccuracy).set_SelectedIndex(DiameterData[Record].RRateUncertainty);
			((Control)txtTop1D).set_Text(DiameterData[Record].Top1D.ToString());
			((Control)txtTop2D).set_Text(DiameterData[Record].Top2D.ToString());
			((Control)txtTop1R).set_Text(DiameterData[Record].Top1R.ToString());
			((Control)txtTop2R).set_Text(DiameterData[Record].Top2R.ToString());
			((Control)txtTopDSD).set_Text(DiameterData[Record].TopDSD.ToString());
			((Control)txtTopRSD).set_Text(DiameterData[Record].TopRSD.ToString());
			((Control)txtBottom1D).set_Text(DiameterData[Record].Bottom1D.ToString());
			((Control)txtBottom1R).set_Text(DiameterData[Record].Bottom1R.ToString());
			((Control)txtBottom2D).set_Text(DiameterData[Record].Bottom2D.ToString());
			((Control)txtBottom2R).set_Text(DiameterData[Record].Bottom2R.ToString());
			((Control)txtBottomDSD).set_Text(DiameterData[Record].BottomDSD.ToString());
			((Control)txtBottomRSD).set_Text(DiameterData[Record].BottomRSD.ToString());
			((Control)txtMeas1D).set_Text(DiameterData[Record].Meas1D.ToString());
			((Control)txtMeas1R).set_Text(DiameterData[Record].Meas1R.ToString());
			((Control)txtMeas2D).set_Text(DiameterData[Record].Meas2D.ToString());
			((Control)txtMeas2R).set_Text(DiameterData[Record].Meas2R.ToString());
			RadioButton obj = optSetMeasureD;
			bool @checked;
			optSetMeasureR.set_Checked(@checked = true);
			obj.set_Checked(@checked);
			updnDiaD.set_Value((decimal)DiameterData[Record].DiaD);
			updnDiaR.set_Value((decimal)DiameterData[Record].DiaR);
			updnWeightD.set_Value((decimal)DiameterData[Record].WeightD);
			updnWeightR.set_Value((decimal)DiameterData[Record].WeightR);
			((Control)txtCommentD).set_Text(DiameterData[Record].CommentD.Trim());
			((Control)txtCommentR).set_Text(DiameterData[Record].CommentR.Trim());
		}

		private void cmdUpdate_Click(object sender, EventArgs e)
		{
			//IL_0043: Unknown result type (might be due to invalid IL or missing references)
			//IL_0049: Invalid comparison between Unknown and I4
			int selectedIndex = ((ListControl)lstObservers).get_SelectedIndex();
			if (selectedIndex >= 0 && (int)MessageBox.Show("Do you want to update the record for \r\n\r\n" + lstObservers.get_Items().get_Item(selectedIndex).ToString(), "Confirm update", (MessageBoxButtons)4, (MessageBoxIcon)64, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				UpdateObserverRecord(selectedIndex);
			}
		}

		private void openToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_004e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0054: Invalid comparison between Unknown and I4
			OpenFileDialog val = new OpenFileDialog();
			((FileDialog)val).set_Title("Specify file to READ star diameter observations from.");
			((FileDialog)val).set_Filter("Text files (*.txt)|*.txt|All files (*.*)|*.*");
			((FileDialog)val).set_FilterIndex(0);
			((FileDialog)val).set_FileName(Settings.Default.StarDia_File_LastName);
			try
			{
				((FileDialog)val).set_InitialDirectory(Path.GetDirectoryName(Settings.Default.StarDia_File_LastName));
			}
			catch
			{
			}
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				Current_StarDia_File = ((FileDialog)val).get_FileName();
				Settings.Default.StarDia_File_LastName = Current_StarDia_File;
				((Control)this).set_Text("Star diameter observations : " + Path.GetFileName(Current_StarDia_File));
				Read_StarDia_file();
			}
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (Current_StarDia_File == null)
			{
				saveAsToolStripMenuItem_Click(sender, e);
			}
			else if (Current_StarDia_File.Length < 1)
			{
				saveAsToolStripMenuItem_Click(sender, e);
			}
			else
			{
				Save_StarDia_file();
			}
		}

		private void saveAsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_0040: Unknown result type (might be due to invalid IL or missing references)
			//IL_0046: Invalid comparison between Unknown and I4
			SaveFileDialog val = new SaveFileDialog();
			((FileDialog)val).set_Title("Specify file to Write observations to.");
			((FileDialog)val).set_Filter("Text files (*.txt)|*.txt|All files (*.*)|*.*");
			((FileDialog)val).set_FilterIndex(0);
			val.set_OverwritePrompt(false);
			((FileDialog)val).set_InitialDirectory(Utilities.AppPath + "\\Asteroid\\Observations");
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				Current_StarDia_File = ((FileDialog)val).get_FileName();
				Settings.Default.StarDia_File_LastName = Current_StarDia_File;
				((Control)this).set_Text("Star diameter observations : " + Path.GetFileName(Current_StarDia_File));
				Save_StarDia_file();
			}
		}

		private void Save_StarDia_file()
		{
			using StreamWriter streamWriter = new StreamWriter(Current_StarDia_File);
			streamWriter.WriteLine("<Star Dia>" + ((Control)txtStarSolution).get_Text().PadRight(1) + "," + ((Control)txtStarUncertainty).get_Text().PadRight(1) + "," + ((Control)txtCurveNos).get_Text().PadRight(1));
			streamWriter.WriteLine("<Num Observers>" + DiameterData.Count);
			for (int i = 0; i < DiameterData.Count; i++)
			{
				streamWriter.WriteLine("<Record>" + i);
				streamWriter.WriteLine("<Observer>" + DiameterData[i].Observer);
				streamWriter.WriteLine("<FrameRate>" + string.Format("{0,1:f3}", DiameterData[i].FrameRate));
				streamWriter.WriteLine("<Display Secs>" + DiameterData[i].SecondsToDisplay);
				streamWriter.WriteLine("<LimbDarken>" + DiameterData[i].LimbDarken);
				streamWriter.WriteLine("<Is Partial>" + DiameterData[i].IsPartial);
				streamWriter.WriteLine("<Saturation>" + DiameterData[i].SaturationLevelIndex);
				streamWriter.WriteLine("<Motion D>" + DiameterData[i].DMotion);
				streamWriter.WriteLine("<Motion R>" + DiameterData[i].RMotion);
				streamWriter.WriteLine("<Uncert D>" + DiameterData[i].DRateUncertainty);
				streamWriter.WriteLine("<Uncert R>" + DiameterData[i].RRateUncertainty);
				streamWriter.WriteLine("<Location D>" + DiameterData[i].DAdjust);
				streamWriter.WriteLine("<Location R>" + DiameterData[i].RAdjust);
				streamWriter.WriteLine("<Count D>" + DiameterData[i].DCount);
				streamWriter.WriteLine("<Count R>" + DiameterData[i].RCount);
				streamWriter.WriteLine("<Dia D>" + DiameterData[i].DiaD);
				streamWriter.WriteLine("<Dia R>" + DiameterData[i].DiaR);
				streamWriter.WriteLine("<Weight D>" + DiameterData[i].WeightD);
				streamWriter.WriteLine("<Weight R>" + DiameterData[i].WeightR);
				streamWriter.WriteLine("<Cal Top1D>" + DiameterData[i].Top1D);
				streamWriter.WriteLine("<Cal Top2D>" + DiameterData[i].Top2D);
				streamWriter.WriteLine("<Cal TopD SD>" + string.Format("{0,1:f4}", DiameterData[i].TopDSD.ToString()));
				streamWriter.WriteLine("<Cal Bottom1D>" + DiameterData[i].Bottom1D);
				streamWriter.WriteLine("<Cal Bottom2D>" + DiameterData[i].Bottom2D);
				streamWriter.WriteLine("<Cal BottomD SD>" + string.Format("{0,1:f4}", DiameterData[i].BottomDSD.ToString()));
				streamWriter.WriteLine("<Cal Meas1D>" + DiameterData[i].Meas1D);
				streamWriter.WriteLine("<Cal Meas2D>" + DiameterData[i].Meas2D);
				streamWriter.WriteLine("<Cal Top1R>" + DiameterData[i].Top1R);
				streamWriter.WriteLine("<Cal Top2R>" + DiameterData[i].Top2R);
				streamWriter.WriteLine("<Cal TopR SD>" + string.Format("{0,1:f4}", DiameterData[i].TopRSD.ToString()));
				streamWriter.WriteLine("<Cal Bottom1R>" + DiameterData[i].Bottom1R);
				streamWriter.WriteLine("<Cal Bottom2R>" + DiameterData[i].Bottom2R);
				streamWriter.WriteLine("<Cal BottomR SD>" + string.Format("{0,1:f4}", DiameterData[i].BottomRSD.ToString()));
				streamWriter.WriteLine("<Cal Meas1R>" + DiameterData[i].Meas1R);
				streamWriter.WriteLine("<Cal Meas2R>" + DiameterData[i].Meas2R);
				streamWriter.WriteLine("<Comment D>" + DiameterData[i].CommentD);
				streamWriter.WriteLine("<Comment R>" + DiameterData[i].CommentR);
				streamWriter.WriteLine("<Data D>");
				for (int j = 0; j < DiameterData[i].DCount; j++)
				{
					streamWriter.WriteLine(DiameterData[i].ObservedDTime[j] + string.Format(", {0,1:f4}", DiameterData[i].ObservedD[j]));
				}
				streamWriter.WriteLine("<Data R>");
				for (int k = 0; k < DiameterData[i].RCount; k++)
				{
					streamWriter.WriteLine(DiameterData[i].ObservedRTime[k] + string.Format(", {0,1:f4}", DiameterData[i].ObservedR[k]));
				}
			}
		}

		private void Read_StarDia_file()
		{
			StreamReader streamReader = new StreamReader(Current_StarDia_File);
			int num = -1;
			if (streamReader.BaseStream.Length > 0)
			{
				string text = streamReader.ReadLine();
				if (text.Contains("<"))
				{
					string[] array = text.Split(new char[1] { ',' });
					((Control)txtStarSolution).set_Text(array[0].Substring(10));
					if (array.GetUpperBound(0) > 0)
					{
						((Control)txtStarUncertainty).set_Text(array[1]);
						((Control)txtCurveNos).set_Text(array[2]);
					}
					if (!int.TryParse(streamReader.ReadLine()!.Substring(15), out var result))
					{
						result = 0;
					}
					DiameterData.Clear();
					lstObservers.get_Items().Clear();
					text = streamReader.ReadLine();
					do
					{
						if (!(text.Substring(0, 8) == "<Record>"))
						{
							continue;
						}
						num++;
						StarDiameterData starDiameterData = new StarDiameterData();
						do
						{
							text = streamReader.ReadLine()!.PadRight(17);
							if (text.Substring(0, 8) == "<Record>")
							{
								break;
							}
							float result2;
							int result3;
							double result4;
							if (text.Contains("<Observer>"))
							{
								starDiameterData.Observer = text.Substring(10);
							}
							else if (text.Contains("<FrameRate>"))
							{
								float.TryParse(text.Substring(11), out result2);
								starDiameterData.FrameRate = result2;
							}
							else if (text.Substring(0, 14) == "<Display Secs>")
							{
								if (!float.TryParse(text.Substring(14), out result2))
								{
									result2 = 1f;
								}
								starDiameterData.SecondsToDisplay = result2;
							}
							else if (text.Substring(0, 12) == "<LimbDarken>")
							{
								if (!int.TryParse(text.Substring(12), out result3))
								{
									result3 = 0;
								}
								starDiameterData.LimbDarken = result3;
							}
							else if (text.Substring(0, 12) == "<Is Partial>")
							{
								if (!int.TryParse(text.Substring(12), out result3))
								{
									result3 = 0;
								}
								starDiameterData.IsPartial = result3;
							}
							else if (text.Substring(0, 12) == "<Saturation>")
							{
								if (!int.TryParse(text.Substring(12), out result3))
								{
									result3 = 0;
								}
								starDiameterData.SaturationLevelIndex = result3;
							}
							else if (text.Substring(0, 7) == "<Dia D>")
							{
								if (!double.TryParse(text.Substring(7), out result4))
								{
									result4 = 0.0;
								}
								starDiameterData.DiaD = result4;
							}
							else if (text.Substring(0, 7) == "<Dia R>")
							{
								if (!double.TryParse(text.Substring(7), out result4))
								{
									result4 = 0.0;
								}
								starDiameterData.DiaR = result4;
							}
							else if (text.Substring(0, 10) == "<Motion D>")
							{
								starDiameterData.DMotion = text.Substring(10).Trim();
							}
							else if (text.Substring(0, 10) == "<Motion R>")
							{
								starDiameterData.RMotion = text.Substring(10).Trim();
							}
							else if (text.Substring(0, 10) == "<Uncert D>")
							{
								if (!int.TryParse(text.Substring(10), out result3))
								{
									result3 = 0;
								}
								starDiameterData.DRateUncertainty = result3;
							}
							else if (text.Substring(0, 10) == "<Uncert R>")
							{
								if (!int.TryParse(text.Substring(10), out result3))
								{
									result3 = 0;
								}
								starDiameterData.RRateUncertainty = result3;
							}
							else if (text.Substring(0, 12) == "<Location D>")
							{
								if (!float.TryParse(text.Substring(12), out result2))
								{
									result2 = 0f;
								}
								starDiameterData.DAdjust = result2;
							}
							else if (text.Substring(0, 12) == "<Location R>")
							{
								if (!float.TryParse(text.Substring(12), out result2))
								{
									result2 = 0f;
								}
								starDiameterData.RAdjust = result2;
							}
							else if (text.Substring(0, 9) == "<Count D>")
							{
								if (!int.TryParse(text.Substring(9), out result3))
								{
									result3 = 0;
								}
								starDiameterData.DCount = result3;
							}
							else if (text.Substring(0, 9) == "<Count R>")
							{
								if (!int.TryParse(text.Substring(9), out result3))
								{
									result3 = 0;
								}
								starDiameterData.RCount = result3;
							}
							else if (text.Substring(0, 10) == "<Weight D>")
							{
								if (!int.TryParse(text.Substring(10), out result3))
								{
									result3 = 0;
								}
								starDiameterData.WeightD = result3;
							}
							else if (text.Substring(0, 10) == "<Weight R>")
							{
								if (!int.TryParse(text.Substring(10), out result3))
								{
									result3 = 0;
								}
								starDiameterData.WeightR = result3;
							}
							else if (text.Substring(0, 11) == "<Cal Top1D>")
							{
								if (!int.TryParse(text.Substring(11), out result3))
								{
									result3 = 0;
								}
								starDiameterData.Top1D = result3;
							}
							else if (text.Substring(0, 11) == "<Cal Top2D>")
							{
								if (!int.TryParse(text.Substring(11), out result3))
								{
									result3 = 0;
								}
								starDiameterData.Top2D = result3;
							}
							else if (text.Substring(0, 13) == "<Cal TopD SD>")
							{
								if (!double.TryParse(text.Substring(13), out result4))
								{
									result4 = 0.0;
								}
								starDiameterData.TopDSD = result4;
							}
							else if (text.Substring(0, 14) == "<Cal Bottom1D>")
							{
								if (!int.TryParse(text.Substring(14), out result3))
								{
									result3 = 0;
								}
								starDiameterData.Bottom1D = result3;
							}
							else if (text.Substring(0, 14) == "<Cal Bottom2D>")
							{
								if (!int.TryParse(text.Substring(14), out result3))
								{
									result3 = 0;
								}
								starDiameterData.Bottom2D = result3;
							}
							else if (text.Substring(0, 16) == "<Cal BottomD SD>")
							{
								if (!double.TryParse(text.Substring(16), out result4))
								{
									result4 = 0.0;
								}
								starDiameterData.BottomDSD = result4;
							}
							else if (text.Substring(0, 12) == "<Cal Meas1D>")
							{
								if (!int.TryParse(text.Substring(12), out result3))
								{
									result3 = 0;
								}
								starDiameterData.Meas1D = result3;
							}
							else if (text.Substring(0, 12) == "<Cal Meas2D>")
							{
								if (!int.TryParse(text.Substring(12), out result3))
								{
									result3 = 0;
								}
								starDiameterData.Meas2D = result3;
							}
							else if (text.Substring(0, 11) == "<Cal Top1R>")
							{
								if (!int.TryParse(text.Substring(11), out result3))
								{
									result3 = 0;
								}
								starDiameterData.Top1R = result3;
							}
							else if (text.Substring(0, 11) == "<Cal Top2R>")
							{
								if (!int.TryParse(text.Substring(11), out result3))
								{
									result3 = 0;
								}
								starDiameterData.Top2R = result3;
							}
							else if (text.Substring(0, 13) == "<Cal TopR SD>")
							{
								if (!double.TryParse(text.Substring(13), out result4))
								{
									result4 = 0.0;
								}
								starDiameterData.TopRSD = result4;
							}
							else if (text.Substring(0, 14) == "<Cal Bottom1R>")
							{
								if (!int.TryParse(text.Substring(14), out result3))
								{
									result3 = 0;
								}
								starDiameterData.Bottom1R = result3;
							}
							else if (text.Substring(0, 14) == "<Cal Bottom2R>")
							{
								if (!int.TryParse(text.Substring(14), out result3))
								{
									result3 = 0;
								}
								starDiameterData.Bottom2R = result3;
							}
							else if (text.Substring(0, 16) == "<Cal BottomR SD>")
							{
								if (!double.TryParse(text.Substring(16), out result4))
								{
									result4 = 0.0;
								}
								starDiameterData.BottomRSD = result4;
							}
							else if (text.Substring(0, 12) == "<Cal Meas1R>")
							{
								if (!int.TryParse(text.Substring(12), out result3))
								{
									result3 = 0;
								}
								starDiameterData.Meas1R = result3;
							}
							else if (text.Substring(0, 12) == "<Cal Meas2R>")
							{
								if (!int.TryParse(text.Substring(12), out result3))
								{
									result3 = 0;
								}
								starDiameterData.Meas2R = result3;
							}
							else if (text.Substring(0, 11) == "<Comment D>")
							{
								starDiameterData.CommentD = text.Substring(11).Trim();
							}
							else if (text.Substring(0, 11) == "<Comment R>")
							{
								starDiameterData.CommentR = text.Substring(11).Trim();
							}
							else if (text.Substring(0, 8) == "<Data D>")
							{
								for (int i = 0; i < starDiameterData.DCount; i++)
								{
									array = streamReader.ReadLine()!.Split(new char[1] { ',' });
									starDiameterData.ObservedDTime[i] = array[0];
									if (!float.TryParse(array[1], out result2))
									{
										result2 = 0f;
									}
									starDiameterData.ObservedD[i] = result2;
								}
							}
							else
							{
								if (!(text.Substring(0, 8) == "<Data R>"))
								{
									continue;
								}
								for (int j = 0; j < starDiameterData.RCount; j++)
								{
									array = streamReader.ReadLine()!.Split(new char[1] { ',' });
									starDiameterData.ObservedRTime[j] = array[0];
									if (!float.TryParse(array[1], out result2))
									{
										result2 = 0f;
									}
									starDiameterData.ObservedR[j] = result2;
								}
							}
						}
						while (!streamReader.EndOfStream);
						lstObservers.get_Items().Add((object)starDiameterData.Observer);
						DiameterData.Add(starDiameterData);
					}
					while (!streamReader.EndOfStream);
				}
			}
			streamReader.Close();
			((ListControl)lstObservers).set_SelectedIndex(0);
		}

		private void newToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DiameterData.Clear();
			lstObservers.get_Items().Clear();
			TextBox obj = txtTop1D;
			TextBox obj2 = txtTop1R;
			TextBox obj3 = txtTop2D;
			TextBox obj4 = txtTop2R;
			TextBox obj5 = txtTopDSD;
			string text;
			((Control)txtTopRSD).set_Text(text = "0");
			string text2;
			((Control)obj5).set_Text(text2 = text);
			string text3;
			((Control)obj4).set_Text(text3 = text2);
			string text4;
			((Control)obj3).set_Text(text4 = text3);
			string text5;
			((Control)obj2).set_Text(text5 = text4);
			((Control)obj).set_Text(text5);
			TextBox obj6 = txtBottom1D;
			TextBox obj7 = txtBottom1R;
			TextBox obj8 = txtBottom2D;
			TextBox obj9 = txtBottom2R;
			TextBox obj10 = txtBottomDSD;
			((Control)txtBottomRSD).set_Text(text = "0");
			((Control)obj10).set_Text(text2 = text);
			((Control)obj9).set_Text(text3 = text2);
			((Control)obj8).set_Text(text4 = text3);
			((Control)obj7).set_Text(text5 = text4);
			((Control)obj6).set_Text(text5);
			TextBox obj11 = txtMeas1D;
			TextBox obj12 = txtMeas1R;
			TextBox obj13 = txtMeas2D;
			((Control)txtMeas2R).set_Text(text3 = "0");
			((Control)obj13).set_Text(text4 = text3);
			((Control)obj12).set_Text(text5 = text4);
			((Control)obj11).set_Text(text5);
		}

		private void cmdDelete_Click(object sender, EventArgs e)
		{
			//IL_0023: Unknown result type (might be due to invalid IL or missing references)
			//IL_0029: Invalid comparison between Unknown and I4
			int selectedIndex = ((ListControl)lstObservers).get_SelectedIndex();
			if ((int)MessageBox.Show("Do you want to remove all data for the selected observer?", "Confirm delete", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				DiameterData.RemoveAt(selectedIndex);
				lstObservers.get_Items().RemoveAt(selectedIndex);
				if (lstObservers.get_Items().get_Count() > 0)
				{
					((ListControl)lstObservers).set_SelectedIndex(0);
				}
			}
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

		private void txtMotionD_TextChanged(object sender, EventArgs e)
		{
			SetFadeText();
		}

		private void txtMotionR_TextChanged(object sender, EventArgs e)
		{
			SetFadeText();
		}

		private void copyFormToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Application.DoEvents();
			Bitmap bitmap = new Bitmap(((Control)this).get_Bounds().Width, ((Control)this).get_Bounds().Height);
			Graphics.FromImage(bitmap).CopyFromScreen(((Form)this).get_Location().X, ((Form)this).get_Location().Y, 0, 0, bitmap.Size);
			Clipboard.SetImage((Image)bitmap);
		}

		private void cmdPlotAll_Click(object sender, EventArgs e)
		{
			int selectedIndex = ((ListControl)lstObservers).get_SelectedIndex();
			StarDiameterAnalysis.ShowStarDiameterPlotAll();
			StarDiameterAnalysis.PlotAll_Plot();
			((ListControl)lstObservers).set_SelectedIndex(selectedIndex);
			SetFieldsforObserverRecord(selectedIndex);
			((Control)StarDiameterAnalysis.StarDiameter_PlotAll).Focus();
			Plot();
		}

		private void picD_MouseMove(object sender, MouseEventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_0021: Invalid comparison between Unknown and I4
			float num = e.get_X();
			float num2 = e.get_Y();
			Color black = Color.Black;
			if ((int)e.get_Button() == 1048576)
			{
				if (Xrev0 < 0)
				{
					Xrev0 = (int)num;
					Yrev0 = (int)num2;
					Xrev1 = (int)num;
					Yrev1 = (int)num2;
					ControlPaint.DrawReversibleFrame(new Rectangle(((Control)picD).PointToScreen(new Point(Xrev0, 2)), new Size(Xrev1 - Xrev0, ((Control)picD).get_Height() - 4)), black, (FrameStyle)0);
				}
				else
				{
					ControlPaint.DrawReversibleFrame(new Rectangle(((Control)picD).PointToScreen(new Point(Xrev0, 2)), new Size(Xrev1 - Xrev0, ((Control)picD).get_Height() - 4)), Color.White, (FrameStyle)0);
					Xrev1 = (int)num;
					Yrev1 = (int)num2;
					ControlPaint.DrawReversibleFrame(new Rectangle(((Control)picD).PointToScreen(new Point(Xrev0, 2)), new Size(Xrev1 - Xrev0, ((Control)picD).get_Height() - 4)), black, (FrameStyle)0);
				}
			}
			else if (Xrev0 >= 0)
			{
				ControlPaint.DrawReversibleFrame(new Rectangle(((Control)picD).PointToScreen(new Point(Xrev0, 2)), new Size(Xrev1 - Xrev0, ((Control)picD).get_Height() - 4)), Color.White, (FrameStyle)0);
				Xrev0 = -1;
			}
		}

		private void picD_MouseUp(object sender, MouseEventArgs e)
		{
			int num;
			int num2;
			if (Xrev0 < Xrev1)
			{
				num = StarDiameterAnalysis.LocalPlotPointFromMap_X(Xrev0) + (int)updnDAdjust.get_Value() + Dcount / 2;
				num2 = StarDiameterAnalysis.LocalPlotPointFromMap_X(Xrev1) + (int)updnDAdjust.get_Value() + Dcount / 2;
			}
			else
			{
				num2 = StarDiameterAnalysis.LocalPlotPointFromMap_X(Xrev0) + (int)updnDAdjust.get_Value() + Dcount / 2;
				num = StarDiameterAnalysis.LocalPlotPointFromMap_X(Xrev1) + (int)updnDAdjust.get_Value() + Dcount / 2;
			}
			if (num < 0)
			{
				num = 0;
			}
			if (num2 < 0)
			{
				num = 0;
			}
			if (num2 > Dcount)
			{
				num2 = Dcount - 1;
			}
			if (num > Dcount)
			{
				num = Dcount - 1;
			}
			string text = string.Format("{0,1:f0}", num);
			string text2 = string.Format("{0,1:f0}", num2);
			if (optSetTopD.get_Checked())
			{
				((Control)txtTop1D).set_Text(text);
				((Control)txtTop2D).set_Text(text2);
			}
			else if (optSetBottomD.get_Checked())
			{
				((Control)txtBottom1D).set_Text(text);
				((Control)txtBottom2D).set_Text(text2);
			}
			else if (optSetMeasureD.get_Checked())
			{
				((Control)txtMeas1D).set_Text(text);
				((Control)txtMeas2D).set_Text(text2);
			}
			Plot();
		}

		private void picR_MouseMove(object sender, MouseEventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_0021: Invalid comparison between Unknown and I4
			float num = e.get_X();
			float num2 = e.get_Y();
			Color black = Color.Black;
			if ((int)e.get_Button() == 1048576)
			{
				if (Xrev0 < 0)
				{
					Xrev0 = (int)num;
					Yrev0 = (int)num2;
					Xrev1 = (int)num;
					Yrev1 = (int)num2;
					ControlPaint.DrawReversibleFrame(new Rectangle(((Control)picR).PointToScreen(new Point(Xrev0, 2)), new Size(Xrev1 - Xrev0, ((Control)picD).get_Height() - 4)), black, (FrameStyle)0);
				}
				else
				{
					ControlPaint.DrawReversibleFrame(new Rectangle(((Control)picR).PointToScreen(new Point(Xrev0, 2)), new Size(Xrev1 - Xrev0, ((Control)picD).get_Height() - 4)), Color.White, (FrameStyle)0);
					Xrev1 = (int)num;
					Yrev1 = (int)num2;
					ControlPaint.DrawReversibleFrame(new Rectangle(((Control)picR).PointToScreen(new Point(Xrev0, 2)), new Size(Xrev1 - Xrev0, ((Control)picD).get_Height() - 4)), black, (FrameStyle)0);
				}
			}
			else if (Xrev0 >= 0)
			{
				ControlPaint.DrawReversibleFrame(new Rectangle(((Control)picR).PointToScreen(new Point(Xrev0, 2)), new Size(Xrev1 - Xrev0, ((Control)picD).get_Height() - 4)), Color.White, (FrameStyle)0);
				Xrev0 = -1;
			}
		}

		private void picR_MouseUp(object sender, MouseEventArgs e)
		{
			int num;
			int num2;
			if (Xrev0 < Xrev1)
			{
				num = (int)((float)(StarDiameterAnalysis.LocalPlotPointFromMap_X(Xrev0) + (int)updnRAdjust.get_Value()) + (float)Rcount / 2f);
				num2 = (int)((float)(StarDiameterAnalysis.LocalPlotPointFromMap_X(Xrev1) + (int)updnRAdjust.get_Value()) + (float)Rcount / 2f);
			}
			else
			{
				num2 = (int)((float)(StarDiameterAnalysis.LocalPlotPointFromMap_X(Xrev0) + (int)updnRAdjust.get_Value()) + (float)Rcount / 2f);
				num = (int)((float)(StarDiameterAnalysis.LocalPlotPointFromMap_X(Xrev1) + (int)updnRAdjust.get_Value()) + (float)Rcount / 2f);
			}
			if (num < 0)
			{
				num = 0;
			}
			if (num2 < 0)
			{
				num2 = 0;
			}
			if (num2 > Rcount)
			{
				num2 = Rcount - 1;
			}
			if (num > Rcount)
			{
				num = Rcount - 1;
			}
			string text = string.Format("{0,1:f0}", num);
			string text2 = string.Format("{0,1:f0}", num2);
			if (optSetTopR.get_Checked())
			{
				((Control)txtTop1R).set_Text(text);
				((Control)txtTop2R).set_Text(text2);
			}
			else if (optSetBottomR.get_Checked())
			{
				((Control)txtBottom1R).set_Text(text);
				((Control)txtBottom2R).set_Text(text2);
			}
			else if (optSetMeasureR.get_Checked())
			{
				((Control)txtMeas1R).set_Text(text);
				((Control)txtMeas2R).set_Text(text2);
			}
			Plot();
		}

		private void chkShowRegions_CheckedChanged(object sender, EventArgs e)
		{
			Plot();
		}

		private void cmdCalibrateD_Click(object sender, EventArgs e)
		{
			int num = int.Parse(((Control)txtBottom1D).get_Text());
			int num2 = int.Parse(((Control)txtBottom2D).get_Text());
			int num3 = int.Parse(((Control)txtTop1D).get_Text());
			int num4 = int.Parse(((Control)txtTop2D).get_Text());
			for (int i = 0; i < 2; i++)
			{
				if (num2 - num > 2)
				{
					double num5 = 0.0;
					for (int j = num; j <= num2; j++)
					{
						num5 += (double)ObservedD[j];
					}
					float num6 = (float)(num5 / (double)(num2 - num + 1));
					if (!chkPartial.get_Checked())
					{
						for (int k = 0; k < Dcount; k++)
						{
							ObservedD[k] -= num6;
						}
						num5 = 0.0;
						for (int l = num; l <= num2; l++)
						{
							num5 += (double)(ObservedD[l] * ObservedD[l]);
						}
						((Control)txtBottomDSD).set_Text(string.Format("{0,1:f4}", Math.Sqrt(num5 / (double)(num2 - num + 1))));
					}
				}
				else
				{
					((Control)txtBottomDSD).set_Text("0");
				}
				if (num4 - num3 > 2)
				{
					double num5 = 0.0;
					for (int m = num3; m <= num4; m++)
					{
						num5 += (double)ObservedD[m];
					}
					float num7 = (float)(num5 / (double)(num4 - num3 + 1));
					for (int n = 0; n < Dcount; n++)
					{
						ObservedD[n] /= num7;
					}
					num5 = 0.0;
					for (int num8 = num3; num8 <= num4; num8++)
					{
						num5 += (double)((ObservedD[num8] - 1f) * (ObservedD[num8] - 1f));
					}
					((Control)txtTopDSD).set_Text(string.Format("{0,1:f4}", Math.Sqrt(num5 / (double)(num2 - num + 1))));
					if (chkPartial.get_Checked())
					{
						((Control)txtBottomDSD).set_Text(((Control)txtTopDSD).get_Text());
					}
				}
				else
				{
					((Control)txtTopDSD).set_Text("0");
				}
			}
			Plot();
		}

		private void cmdCalibrateR_Click(object sender, EventArgs e)
		{
			int num = int.Parse(((Control)txtBottom1R).get_Text());
			int num2 = int.Parse(((Control)txtBottom2R).get_Text());
			int num3 = int.Parse(((Control)txtTop1R).get_Text());
			int num4 = int.Parse(((Control)txtTop2R).get_Text());
			for (int i = 0; i < 2; i++)
			{
				if (num2 - num > 2)
				{
					double num5 = 0.0;
					for (int j = num; j <= num2; j++)
					{
						num5 += (double)ObservedR[j];
					}
					float num6 = (float)(num5 / (double)(num2 - num + 1));
					if (!chkPartial.get_Checked())
					{
						for (int k = 0; k < Rcount; k++)
						{
							ObservedR[k] -= num6;
						}
						num5 = 0.0;
						for (int l = num; l <= num2; l++)
						{
							num5 += (double)(ObservedR[l] * ObservedR[l]);
						}
						((Control)txtBottomRSD).set_Text(string.Format("{0,1:f4}", Math.Sqrt(num5 / (double)(num2 - num + 1))));
					}
				}
				else
				{
					((Control)txtBottomRSD).set_Text("0");
				}
				if (num4 - num3 > 2)
				{
					double num5 = 0.0;
					for (int m = num3; m <= num4; m++)
					{
						num5 += (double)ObservedR[m];
					}
					float num7 = (float)(num5 / (double)(num4 - num3 + 1));
					for (int n = 0; n < Rcount; n++)
					{
						ObservedR[n] /= num7;
					}
					num5 = 0.0;
					for (int num8 = num3; num8 <= num4; num8++)
					{
						num5 += (double)((ObservedR[num8] - 1f) * (ObservedR[num8] - 1f));
					}
					((Control)txtTopRSD).set_Text(string.Format("{0,1:f4}", Math.Sqrt(num5 / (double)(num2 - num + 1))));
					if (chkPartial.get_Checked())
					{
						((Control)txtBottomRSD).set_Text(((Control)txtTopRSD).get_Text());
					}
				}
				else
				{
					((Control)txtTopRSD).set_Text("0");
				}
			}
			Plot();
		}

		private void cmbSaturation_SelectedIndexChanged(object sender, EventArgs e)
		{
			StarDiameterAnalysis.SaturationLevel = 1.0 - (double)((ListControl)cmbSaturation).get_SelectedIndex() / 20.0;
			DrawGaussCurve();
			Plot();
		}

		private void DrawGaussCurve()
		{
			Pen pen = new Pen(Color.Red, 1f);
			pen.DashPattern = new float[2] { 2f, 4f };
			Pen pen2 = new Pen(Color.Blue, 2f);
			double num = Gauss.GetUpperBound(0);
			double num2 = (double)((Control)picStar).get_Width() / num;
			double num3 = (double)((Control)picStar).get_Height() / 50.0;
			double num4 = 40.0 * (1.0 - (double)((ListControl)cmbSaturation).get_SelectedIndex() * 0.05);
			Bitmap image = new Bitmap(((Control)picStar).get_Width(), ((Control)picStar).get_Height());
			Graphics graphics = Graphics.FromImage(image);
			graphics.Clear(Color.White);
			double num5 = 0.0;
			double num6 = ((Control)picStar).get_Height() - 5;
			for (int i = 0; (double)i < num; i++)
			{
				double num7 = (double)i * num2;
				double num8 = (double)(((Control)picStar).get_Height() - 5) - Gauss[i] * num3;
				if (i > 0)
				{
					graphics.DrawLine(pen, (float)num5, (float)num6, (float)num7, (float)num8);
				}
				num5 = num7;
				num6 = num8;
			}
			num5 = (num6 = 0.0);
			for (int j = 0; (double)j < num; j++)
			{
				double num7 = (double)j * num2;
				double num9 = Gauss[j];
				if (num9 > num4)
				{
					num9 = num4;
				}
				double num8 = (double)(((Control)picStar).get_Height() - 5) - num9 * num3;
				if (j > 0)
				{
					graphics.DrawLine(pen2, (float)num5, (float)num6, (float)num7, (float)num8);
				}
				num5 = num7;
				num6 = num8;
			}
			graphics.DrawRectangle(Pens.Black, 0, 0, ((Control)picStar).get_Width() - 1, ((Control)picStar).get_Height() - 1);
			picStar.set_Image((Image)image);
		}

		private void chkLimbDarkening_CheckedChanged(object sender, EventArgs e)
		{
			Plot();
			GetDiameterSolution();
		}

		private void GetDiameterSolution()
		{
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			double num4 = 0.0;
			int num5 = 0;
			int num6 = 0;
			int num7 = 0;
			int num8 = 0;
			for (int i = 0; i < DiameterData.Count; i++)
			{
				num7 = DiameterData[i].WeightD * DiameterData[i].DRateUncertainty;
				if (num7 > 0)
				{
					num += (double)num7 * DiameterData[i].DiaD;
					num5 += num7;
					num6++;
				}
				num8 = DiameterData[i].WeightR * DiameterData[i].RRateUncertainty;
				if (num8 > 0)
				{
					num += (double)num8 * DiameterData[i].DiaR;
					num5 += num8;
					num6++;
				}
			}
			num2 = num / (double)num5;
			((Control)txtStarSolution).set_Text(string.Format("{0,1:f2}", num2));
			((Control)txtCurveNos).set_Text(num6.ToString());
			num = 0.0;
			for (int j = 0; j < DiameterData.Count; j++)
			{
				num7 = DiameterData[j].WeightD * DiameterData[j].DRateUncertainty;
				if (num7 > 0)
				{
					double num9 = DiameterData[j].DiaD - num2;
					num += (double)DiameterData[j].WeightD * num9 * num9;
					num3 += (double)num7;
					num4 += (double)(num7 * num7);
				}
				num8 = DiameterData[j].WeightR * DiameterData[j].RRateUncertainty;
				if (num8 > 0)
				{
					double num9 = DiameterData[j].DiaR - num2;
					num += (double)DiameterData[j].WeightR * num9 * num9;
					num3 += (double)num8;
					num4 += (double)(num8 * num8);
				}
			}
			if (num5 > 1)
			{
				((Control)txtStarUncertainty).set_Text(string.Format("{0,1:f2}", Math.Sqrt(num3 / (num3 * num3 - num4) * num) / Math.Sqrt(num6)));
			}
			else
			{
				((Control)txtStarUncertainty).set_Text("...");
			}
		}

		private void chkx10D_CheckedChanged(object sender, EventArgs e)
		{
			if (chkx10D.get_Checked())
			{
				updnDiaD.set_Increment(0.1m);
			}
			else
			{
				updnDiaD.set_Increment(0.01m);
			}
		}

		private void chkx10R_CheckedChanged(object sender, EventArgs e)
		{
			if (chkx10R.get_Checked())
			{
				updnDiaR.set_Increment(0.1m);
			}
			else
			{
				updnDiaR.set_Increment(0.01m);
			}
		}

		private void chkLightCurve_CheckedChanged(object sender, EventArgs e)
		{
			Plot();
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
			//IL_04f7: Unknown result type (might be due to invalid IL or missing references)
			//IL_0501: Expected O, but got Unknown
			//IL_0502: Unknown result type (might be due to invalid IL or missing references)
			//IL_050c: Expected O, but got Unknown
			//IL_050d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0517: Expected O, but got Unknown
			//IL_0518: Unknown result type (might be due to invalid IL or missing references)
			//IL_0522: Expected O, but got Unknown
			//IL_0523: Unknown result type (might be due to invalid IL or missing references)
			//IL_052d: Expected O, but got Unknown
			//IL_052e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0538: Expected O, but got Unknown
			//IL_0539: Unknown result type (might be due to invalid IL or missing references)
			//IL_0543: Expected O, but got Unknown
			//IL_0544: Unknown result type (might be due to invalid IL or missing references)
			//IL_054e: Expected O, but got Unknown
			//IL_054f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0559: Expected O, but got Unknown
			//IL_055a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0564: Expected O, but got Unknown
			//IL_0565: Unknown result type (might be due to invalid IL or missing references)
			//IL_056f: Expected O, but got Unknown
			//IL_0570: Unknown result type (might be due to invalid IL or missing references)
			//IL_057a: Expected O, but got Unknown
			//IL_057b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0585: Expected O, but got Unknown
			//IL_0586: Unknown result type (might be due to invalid IL or missing references)
			//IL_0590: Expected O, but got Unknown
			//IL_0591: Unknown result type (might be due to invalid IL or missing references)
			//IL_059b: Expected O, but got Unknown
			//IL_059c: Unknown result type (might be due to invalid IL or missing references)
			//IL_05a6: Expected O, but got Unknown
			//IL_05a7: Unknown result type (might be due to invalid IL or missing references)
			//IL_05b1: Expected O, but got Unknown
			//IL_05b2: Unknown result type (might be due to invalid IL or missing references)
			//IL_05bc: Expected O, but got Unknown
			//IL_05bd: Unknown result type (might be due to invalid IL or missing references)
			//IL_05c7: Expected O, but got Unknown
			//IL_05c8: Unknown result type (might be due to invalid IL or missing references)
			//IL_05d2: Expected O, but got Unknown
			//IL_05d3: Unknown result type (might be due to invalid IL or missing references)
			//IL_05dd: Expected O, but got Unknown
			//IL_05de: Unknown result type (might be due to invalid IL or missing references)
			//IL_05e8: Expected O, but got Unknown
			//IL_05e9: Unknown result type (might be due to invalid IL or missing references)
			//IL_05f3: Expected O, but got Unknown
			//IL_05f4: Unknown result type (might be due to invalid IL or missing references)
			//IL_05fe: Expected O, but got Unknown
			//IL_05ff: Unknown result type (might be due to invalid IL or missing references)
			//IL_0609: Expected O, but got Unknown
			//IL_060a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0614: Expected O, but got Unknown
			//IL_0615: Unknown result type (might be due to invalid IL or missing references)
			//IL_061f: Expected O, but got Unknown
			//IL_0620: Unknown result type (might be due to invalid IL or missing references)
			//IL_062a: Expected O, but got Unknown
			//IL_062b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0635: Expected O, but got Unknown
			//IL_0636: Unknown result type (might be due to invalid IL or missing references)
			//IL_0640: Expected O, but got Unknown
			//IL_0641: Unknown result type (might be due to invalid IL or missing references)
			//IL_064b: Expected O, but got Unknown
			//IL_064c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0656: Expected O, but got Unknown
			//IL_0657: Unknown result type (might be due to invalid IL or missing references)
			//IL_0661: Expected O, but got Unknown
			//IL_0662: Unknown result type (might be due to invalid IL or missing references)
			//IL_066c: Expected O, but got Unknown
			//IL_066d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0677: Expected O, but got Unknown
			//IL_0678: Unknown result type (might be due to invalid IL or missing references)
			//IL_0682: Expected O, but got Unknown
			//IL_0683: Unknown result type (might be due to invalid IL or missing references)
			//IL_068d: Expected O, but got Unknown
			//IL_068e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0698: Expected O, but got Unknown
			//IL_2840: Unknown result type (might be due to invalid IL or missing references)
			//IL_284a: Expected O, but got Unknown
			//IL_2857: Unknown result type (might be due to invalid IL or missing references)
			//IL_2861: Expected O, but got Unknown
			//IL_28d4: Unknown result type (might be due to invalid IL or missing references)
			//IL_28de: Expected O, but got Unknown
			//IL_28eb: Unknown result type (might be due to invalid IL or missing references)
			//IL_28f5: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(StarDiameterAnalyser));
			grpD = new GroupBox();
			cmbDRateAccuracy = new ComboBox();
			label6 = new Label();
			label3 = new Label();
			txtMotionD = new TextBox();
			cmdPasteD = new Button();
			grpR = new GroupBox();
			cmbRRateAccuracy = new ComboBox();
			label5 = new Label();
			label4 = new Label();
			txtMotionR = new TextBox();
			cmdPasteR = new Button();
			grpStar = new GroupBox();
			label31 = new Label();
			txtCurveNos = new TextBox();
			label29 = new Label();
			txtStarUncertainty = new TextBox();
			txtStarSolution = new TextBox();
			label7 = new Label();
			groupBox1 = new GroupBox();
			txtFrameRate = new TextBox();
			label42 = new Label();
			cmdPlot = new Button();
			updnSecsToDisplay = new NumericUpDown();
			label1 = new Label();
			updnDAdjust = new NumericUpDown();
			updnRAdjust = new NumericUpDown();
			lblMidD = new Label();
			lblMidR = new Label();
			updnPlotScale = new NumericUpDown();
			label2 = new Label();
			label9 = new Label();
			label10 = new Label();
			menuStrip1 = new MenuStrip();
			fileToolStripMenuItem = new ToolStripMenuItem();
			newToolStripMenuItem = new ToolStripMenuItem();
			openToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			saveAsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			copyFormToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			cmdAddObserver = new Button();
			lstObservers = new ListBox();
			cmdUpdate = new Button();
			cmdDelete = new Button();
			groupBox2 = new GroupBox();
			chkLightCurve = new CheckBox();
			label39 = new Label();
			groupBox3 = new GroupBox();
			optSmall = new RadioButton();
			optLarge = new RadioButton();
			cmdPlotAll = new Button();
			chkShowRegions = new CheckBox();
			picR = new PictureBox();
			picD = new PictureBox();
			grpCalibrateD = new GroupBox();
			label13 = new Label();
			label8 = new Label();
			txtMeas2D = new TextBox();
			txtMeas1D = new TextBox();
			optSetMeasureD = new RadioButton();
			txtTopDSD = new TextBox();
			txtBottomDSD = new TextBox();
			txtTop2D = new TextBox();
			txtBottom2D = new TextBox();
			txtBottom1D = new TextBox();
			txtTop1D = new TextBox();
			cmdCalibrateD = new Button();
			optSetBottomD = new RadioButton();
			optSetTopD = new RadioButton();
			grpCalibrateR = new GroupBox();
			label15 = new Label();
			label11 = new Label();
			txtMeas2R = new TextBox();
			txtMeas1R = new TextBox();
			optSetMeasureR = new RadioButton();
			txtTopRSD = new TextBox();
			txtBottomRSD = new TextBox();
			txtTop2R = new TextBox();
			txtBottom2R = new TextBox();
			txtBottom1R = new TextBox();
			txtTop1R = new TextBox();
			cmdCalibrateR = new Button();
			optSetBottomR = new RadioButton();
			optSetTopR = new RadioButton();
			groupBox4 = new GroupBox();
			chkPartial = new CheckBox();
			label12 = new Label();
			cmbSaturation = new ComboBox();
			picStar = new PictureBox();
			label40 = new Label();
			grpSolveD = new GroupBox();
			picNormD = new PictureBox();
			txtSDD = new Label();
			panel2 = new Panel();
			label35 = new Label();
			label34 = new Label();
			label33 = new Label();
			lblDiffD = new Label();
			label28 = new Label();
			label25 = new Label();
			label23 = new Label();
			lblChi2Ddia2 = new Label();
			lblChi2Ddia1 = new Label();
			lblChi2Ddia0 = new Label();
			lblChi2Dframe2 = new Label();
			lblChi2Dframe1 = new Label();
			lblChi2Dframe0 = new Label();
			label14 = new Label();
			picResD = new PictureBox();
			label18 = new Label();
			updnDiaD = new NumericUpDown();
			label16 = new Label();
			label21 = new Label();
			txtCommentD = new TextBox();
			label30 = new Label();
			updnWeightD = new NumericUpDown();
			chkx10D = new CheckBox();
			grpSolveR = new GroupBox();
			picNormR = new PictureBox();
			txtSDR = new Label();
			panel3 = new Panel();
			label36 = new Label();
			label37 = new Label();
			label38 = new Label();
			lblDiffR = new Label();
			label27 = new Label();
			label26 = new Label();
			label24 = new Label();
			lblChi2Rdia2 = new Label();
			lblChi2Rdia1 = new Label();
			lblChi2Rdia0 = new Label();
			lblChi2Rframe2 = new Label();
			lblChi2Rframe1 = new Label();
			lblChi2Rframe0 = new Label();
			label32 = new Label();
			picResR = new PictureBox();
			label19 = new Label();
			updnDiaR = new NumericUpDown();
			label17 = new Label();
			label22 = new Label();
			txtCommentR = new TextBox();
			label20 = new Label();
			updnWeightR = new NumericUpDown();
			chkx10R = new CheckBox();
			panel4 = new Panel();
			label41 = new Label();
			lblD = new Label();
			txtObserver = new TextBox();
			chkLimbDarkening = new CheckBox();
			((Control)grpD).SuspendLayout();
			((Control)grpR).SuspendLayout();
			((Control)grpStar).SuspendLayout();
			((Control)groupBox1).SuspendLayout();
			((ISupportInitialize)updnSecsToDisplay).BeginInit();
			((ISupportInitialize)updnDAdjust).BeginInit();
			((ISupportInitialize)updnRAdjust).BeginInit();
			((ISupportInitialize)updnPlotScale).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((Control)groupBox2).SuspendLayout();
			((Control)groupBox3).SuspendLayout();
			((ISupportInitialize)picR).BeginInit();
			((ISupportInitialize)picD).BeginInit();
			((Control)grpCalibrateD).SuspendLayout();
			((Control)grpCalibrateR).SuspendLayout();
			((Control)groupBox4).SuspendLayout();
			((ISupportInitialize)picStar).BeginInit();
			((Control)grpSolveD).SuspendLayout();
			((ISupportInitialize)picNormD).BeginInit();
			((Control)panel2).SuspendLayout();
			((ISupportInitialize)picResD).BeginInit();
			((ISupportInitialize)updnDiaD).BeginInit();
			((ISupportInitialize)updnWeightD).BeginInit();
			((Control)grpSolveR).SuspendLayout();
			((ISupportInitialize)picNormR).BeginInit();
			((Control)panel3).SuspendLayout();
			((ISupportInitialize)picResR).BeginInit();
			((ISupportInitialize)updnDiaR).BeginInit();
			((ISupportInitialize)updnWeightR).BeginInit();
			((Control)panel4).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)grpD).get_Controls().Add((Control)(object)cmbDRateAccuracy);
			((Control)grpD).get_Controls().Add((Control)(object)label6);
			((Control)grpD).get_Controls().Add((Control)(object)label3);
			((Control)grpD).get_Controls().Add((Control)(object)txtMotionD);
			((Control)grpD).get_Controls().Add((Control)(object)cmdPasteD);
			((Control)grpD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpD).set_Location(new Point(12, 59));
			((Control)grpD).set_Name("grpD");
			((Control)grpD).set_Size(new Size(321, 56));
			((Control)grpD).set_TabIndex(1);
			grpD.set_TabStop(false);
			((Control)grpD).set_Text("Details for D event");
			((Control)cmbDRateAccuracy).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbDRateAccuracy).set_FormattingEnabled(true);
			cmbDRateAccuracy.get_Items().AddRange(new object[5] { "unusable", ">30%", "20-30%", "10-20%", "<10%" });
			((Control)cmbDRateAccuracy).set_Location(new Point(243, 31));
			((Control)cmbDRateAccuracy).set_Name("cmbDRateAccuracy");
			((Control)cmbDRateAccuracy).set_Size(new Size(67, 21));
			((Control)cmbDRateAccuracy).set_TabIndex(5);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(131, 34));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(109, 13));
			((Control)label6).set_TabIndex(3);
			((Control)label6).set_Text("mas/sec,  uncertainty");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(7, 34));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(74, 13));
			((Control)label3).set_TabIndex(1);
			((Control)label3).set_Text("Normal motion");
			((Control)txtMotionD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMotionD).set_Location(new Point(82, 31));
			((Control)txtMotionD).set_Name("txtMotionD");
			((Control)txtMotionD).set_Size(new Size(49, 20));
			((Control)txtMotionD).set_TabIndex(2);
			((Control)txtMotionD).set_Text("1");
			((Control)txtMotionD).add_TextChanged((EventHandler)txtMotionD_TextChanged);
			((Control)cmdPasteD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdPasteD).set_Location(new Point(137, 11));
			((Control)cmdPasteD).set_Name("cmdPasteD");
			((Control)cmdPasteD).set_Size(new Size(98, 21));
			((Control)cmdPasteD).set_TabIndex(4);
			((Control)cmdPasteD).set_Text("Paste D csv data");
			((ButtonBase)cmdPasteD).set_UseVisualStyleBackColor(true);
			((Control)cmdPasteD).add_Click((EventHandler)cmdPasteD_Click);
			((Control)grpR).get_Controls().Add((Control)(object)cmbRRateAccuracy);
			((Control)grpR).get_Controls().Add((Control)(object)label5);
			((Control)grpR).get_Controls().Add((Control)(object)label4);
			((Control)grpR).get_Controls().Add((Control)(object)txtMotionR);
			((Control)grpR).get_Controls().Add((Control)(object)cmdPasteR);
			((Control)grpR).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpR).set_Location(new Point(364, 59));
			((Control)grpR).set_Name("grpR");
			((Control)grpR).set_Size(new Size(321, 56));
			((Control)grpR).set_TabIndex(2);
			grpR.set_TabStop(false);
			((Control)grpR).set_Text("Details for R event");
			((Control)cmbRRateAccuracy).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbRRateAccuracy).set_FormattingEnabled(true);
			cmbRRateAccuracy.get_Items().AddRange(new object[5] { "unusable", ">30%", "20-30%", "10-20%", "<10%" });
			((Control)cmbRRateAccuracy).set_Location(new Point(240, 30));
			((Control)cmbRRateAccuracy).set_Name("cmbRRateAccuracy");
			((Control)cmbRRateAccuracy).set_Size(new Size(67, 21));
			((Control)cmbRRateAccuracy).set_TabIndex(6);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(129, 34));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(109, 13));
			((Control)label5).set_TabIndex(3);
			((Control)label5).set_Text("mas/sec,  uncertainty");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(6, 34));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(74, 13));
			((Control)label4).set_TabIndex(1);
			((Control)label4).set_Text("Normal motion");
			((Control)txtMotionR).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMotionR).set_Location(new Point(80, 31));
			((Control)txtMotionR).set_Name("txtMotionR");
			((Control)txtMotionR).set_Size(new Size(49, 20));
			((Control)txtMotionR).set_TabIndex(2);
			((Control)txtMotionR).set_Text("1");
			((Control)txtMotionR).add_TextChanged((EventHandler)txtMotionR_TextChanged);
			((Control)cmdPasteR).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdPasteR).set_Location(new Point(130, 11));
			((Control)cmdPasteR).set_Name("cmdPasteR");
			((Control)cmdPasteR).set_Size(new Size(99, 21));
			((Control)cmdPasteR).set_TabIndex(4);
			((Control)cmdPasteR).set_Text("Paste R csv data");
			((ButtonBase)cmdPasteR).set_UseVisualStyleBackColor(true);
			((Control)cmdPasteR).add_Click((EventHandler)cmdPasteR_Click);
			((Control)grpStar).get_Controls().Add((Control)(object)label31);
			((Control)grpStar).get_Controls().Add((Control)(object)txtCurveNos);
			((Control)grpStar).get_Controls().Add((Control)(object)label29);
			((Control)grpStar).get_Controls().Add((Control)(object)txtStarUncertainty);
			((Control)grpStar).get_Controls().Add((Control)(object)txtStarSolution);
			((Control)grpStar).get_Controls().Add((Control)(object)label7);
			((Control)grpStar).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpStar).set_Location(new Point(698, 36));
			((Control)grpStar).set_Name("grpStar");
			((Control)grpStar).set_Size(new Size(261, 46));
			((Control)grpStar).set_TabIndex(3);
			grpStar.set_TabStop(false);
			((Control)grpStar).set_Text("Derived star diameter");
			((Control)label31).set_AutoSize(true);
			((Control)label31).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label31).set_Location(new Point(193, 21));
			((Control)label31).set_Name("label31");
			((Control)label31).set_Size(new Size(61, 13));
			((Control)label31).set_TabIndex(6);
			((Control)label31).set_Text("light curves");
			((TextBoxBase)txtCurveNos).set_BorderStyle((BorderStyle)1);
			((Control)txtCurveNos).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtCurveNos).set_Location(new Point(171, 17));
			((Control)txtCurveNos).set_Name("txtCurveNos");
			((TextBoxBase)txtCurveNos).set_ReadOnly(true);
			((Control)txtCurveNos).set_Size(new Size(22, 20));
			((Control)txtCurveNos).set_TabIndex(5);
			((Control)label29).set_AutoSize(true);
			((Control)label29).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label29).set_Location(new Point(54, 21));
			((Control)label29).set_Name("label29");
			((Control)label29).set_Size(new Size(21, 13));
			((Control)label29).set_TabIndex(4);
			((Control)label29).set_Text("+/-");
			((TextBoxBase)txtStarUncertainty).set_BorderStyle((BorderStyle)1);
			((Control)txtStarUncertainty).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtStarUncertainty).set_Location(new Point(75, 17));
			((Control)txtStarUncertainty).set_Name("txtStarUncertainty");
			((TextBoxBase)txtStarUncertainty).set_ReadOnly(true);
			((Control)txtStarUncertainty).set_Size(new Size(39, 20));
			((Control)txtStarUncertainty).set_TabIndex(3);
			((TextBoxBase)txtStarSolution).set_BorderStyle((BorderStyle)1);
			((Control)txtStarSolution).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtStarSolution).set_Location(new Point(15, 17));
			((Control)txtStarSolution).set_Name("txtStarSolution");
			((TextBoxBase)txtStarSolution).set_ReadOnly(true);
			((Control)txtStarSolution).set_Size(new Size(39, 20));
			((Control)txtStarSolution).set_TabIndex(2);
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(114, 21));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(57, 13));
			((Control)label7).set_TabIndex(1);
			((Control)label7).set_Text("mas, using");
			((Control)groupBox1).get_Controls().Add((Control)(object)txtFrameRate);
			((Control)groupBox1).get_Controls().Add((Control)(object)label42);
			((Control)groupBox1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox1).set_Location(new Point(698, 90));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(261, 44));
			((Control)groupBox1).set_TabIndex(4);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Video parameters");
			((Control)txtFrameRate).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtFrameRate).set_Location(new Point(133, 18));
			((Control)txtFrameRate).set_Name("txtFrameRate");
			((Control)txtFrameRate).set_Size(new Size(58, 20));
			((Control)txtFrameRate).set_TabIndex(1);
			((Control)txtFrameRate).set_Text("25");
			((Control)label42).set_AutoSize(true);
			((Control)label42).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label42).set_Location(new Point(51, 21));
			((Control)label42).set_Name("label42");
			((Control)label42).set_Size(new Size(79, 13));
			((Control)label42).set_TabIndex(0);
			((Control)label42).set_Text("Frames per sec");
			((Control)cmdPlot).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdPlot).set_Location(new Point(177, 21));
			((Control)cmdPlot).set_Name("cmdPlot");
			((Control)cmdPlot).set_Size(new Size(70, 31));
			((Control)cmdPlot).set_TabIndex(8);
			((Control)cmdPlot).set_Text("Plot");
			((ButtonBase)cmdPlot).set_UseVisualStyleBackColor(true);
			((Control)cmdPlot).add_Click((EventHandler)cmdPlot_Click);
			updnSecsToDisplay.set_DecimalPlaces(1);
			((Control)updnSecsToDisplay).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnSecsToDisplay.set_Increment(new decimal(new int[4] { 2, 0, 0, 65536 }));
			((Control)updnSecsToDisplay).set_Location(new Point(94, 32));
			updnSecsToDisplay.set_Minimum(new decimal(new int[4] { 4, 0, 0, 65536 }));
			((Control)updnSecsToDisplay).set_Name("updnSecsToDisplay");
			((Control)updnSecsToDisplay).set_Size(new Size(50, 20));
			((Control)updnSecsToDisplay).set_TabIndex(7);
			updnSecsToDisplay.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnSecsToDisplay.add_ValueChanged((EventHandler)updnSecsToDisplay_ValueChanged);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(71, 17));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(96, 13));
			((Control)label1).set_TabIndex(6);
			((Control)label1).set_Text("Seconds to display");
			updnDAdjust.set_DecimalPlaces(1);
			((Control)updnDAdjust).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnDAdjust.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnDAdjust).set_Location(new Point(54, 12));
			updnDAdjust.set_Maximum(new decimal(new int[4] { 300, 0, 0, 0 }));
			updnDAdjust.set_Minimum(new decimal(new int[4] { 300, 0, 0, -2147483648 }));
			((Control)updnDAdjust).set_Name("updnDAdjust");
			((Control)updnDAdjust).set_Size(new Size(47, 20));
			((Control)updnDAdjust).set_TabIndex(7);
			updnDAdjust.add_ValueChanged((EventHandler)updnDAdjust_ValueChanged);
			updnRAdjust.set_DecimalPlaces(1);
			((Control)updnRAdjust).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnRAdjust.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnRAdjust).set_Location(new Point(57, 13));
			updnRAdjust.set_Maximum(new decimal(new int[4] { 300, 0, 0, 0 }));
			updnRAdjust.set_Minimum(new decimal(new int[4] { 300, 0, 0, -2147483648 }));
			((Control)updnRAdjust).set_Name("updnRAdjust");
			((Control)updnRAdjust).set_Size(new Size(50, 20));
			((Control)updnRAdjust).set_TabIndex(10);
			updnRAdjust.add_ValueChanged((EventHandler)updnRAdjust_ValueChanged);
			((Control)lblMidD).set_AutoSize(true);
			((Control)lblMidD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblMidD).set_Location(new Point(103, 16));
			((Control)lblMidD).set_Name("lblMidD");
			((Control)lblMidD).set_Size(new Size(61, 13));
			((Control)lblMidD).set_TabIndex(8);
			((Control)lblMidD).set_Text("MidDFrame");
			((Control)lblMidR).set_AutoSize(true);
			((Control)lblMidR).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblMidR).set_Location(new Point(110, 17));
			((Control)lblMidR).set_Name("lblMidR");
			((Control)lblMidR).set_Size(new Size(32, 13));
			((Control)lblMidR).set_TabIndex(11);
			((Control)lblMidR).set_Text("MidR");
			updnPlotScale.set_DecimalPlaces(2);
			((Control)updnPlotScale).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnPlotScale.set_Increment(new decimal(new int[4] { 1, 0, 0, 131072 }));
			((Control)updnPlotScale).set_Location(new Point(12, 32));
			updnPlotScale.set_Maximum(new decimal(new int[4] { 12, 0, 0, 65536 }));
			updnPlotScale.set_Minimum(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnPlotScale).set_Name("updnPlotScale");
			((Control)updnPlotScale).set_Size(new Size(51, 20));
			((Control)updnPlotScale).set_TabIndex(1);
			updnPlotScale.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnPlotScale.add_ValueChanged((EventHandler)updnPlotScale_ValueChanged);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(11, 17));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(53, 13));
			((Control)label2).set_TabIndex(0);
			((Control)label2).set_Text("Plot scale");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(2, 16));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(52, 13));
			((Control)label9).set_TabIndex(6);
			((Control)label9).set_Text("mid-frame");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label10).set_Location(new Point(5, 17));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(52, 13));
			((Control)label10).set_TabIndex(9);
			((Control)label10).set_Text("mid-frame");
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)fileToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(968, 24));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)fileToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)newToolStripMenuItem,
				(ToolStripItem)openToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem,
				(ToolStripItem)saveAsToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)copyFormToolStripMenuItem
			});
			((ToolStripItem)fileToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)fileToolStripMenuItem).set_Name("fileToolStripMenuItem");
			((ToolStripItem)fileToolStripMenuItem).set_Size(new Size(61, 20));
			((ToolStripItem)fileToolStripMenuItem).set_Text("File...     ");
			((ToolStripItem)newToolStripMenuItem).set_Image((Image)Resources.NewDocumentHS);
			((ToolStripItem)newToolStripMenuItem).set_Name("newToolStripMenuItem");
			((ToolStripItem)newToolStripMenuItem).set_Size(new Size(236, 22));
			((ToolStripItem)newToolStripMenuItem).set_Text("New");
			((ToolStripItem)newToolStripMenuItem).add_Click((EventHandler)newToolStripMenuItem_Click);
			((ToolStripItem)openToolStripMenuItem).set_Image((Image)Resources.openfolderHS);
			((ToolStripItem)openToolStripMenuItem).set_Name("openToolStripMenuItem");
			((ToolStripItem)openToolStripMenuItem).set_Size(new Size(236, 22));
			((ToolStripItem)openToolStripMenuItem).set_Text("Open");
			((ToolStripItem)openToolStripMenuItem).add_Click((EventHandler)openToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(236, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("Save data");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)saveAsToolStripMenuItem).set_Image((Image)Resources.SaveAsWebPageHS);
			((ToolStripItem)saveAsToolStripMenuItem).set_Name("saveAsToolStripMenuItem");
			((ToolStripItem)saveAsToolStripMenuItem).set_Size(new Size(236, 22));
			((ToolStripItem)saveAsToolStripMenuItem).set_Text("Save data as");
			((ToolStripItem)saveAsToolStripMenuItem).add_Click((EventHandler)saveAsToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(233, 6));
			((ToolStripItem)copyFormToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)copyFormToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyFormToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyFormToolStripMenuItem).set_Name("copyFormToolStripMenuItem");
			((ToolStripItem)copyFormToolStripMenuItem).set_Size(new Size(236, 22));
			((ToolStripItem)copyFormToolStripMenuItem).set_Text("Copy entire form as an image");
			((ToolStripItem)copyFormToolStripMenuItem).add_Click((EventHandler)copyFormToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(75, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help     ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(66, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit    ");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)cmdAddObserver).set_Location(new Point(3, 8));
			((Control)cmdAddObserver).set_Name("cmdAddObserver");
			((Control)cmdAddObserver).set_Size(new Size(70, 34));
			((Control)cmdAddObserver).set_TabIndex(12);
			((Control)cmdAddObserver).set_Text("Add as new record");
			((ButtonBase)cmdAddObserver).set_UseVisualStyleBackColor(true);
			((Control)cmdAddObserver).add_Click((EventHandler)cmdAddObserver_Click);
			((ListControl)lstObservers).set_FormattingEnabled(true);
			((Control)lstObservers).set_Location(new Point(3, 48));
			((Control)lstObservers).set_Name("lstObservers");
			lstObservers.set_ScrollAlwaysVisible(true);
			((Control)lstObservers).set_Size(new Size(261, 303));
			((Control)lstObservers).set_TabIndex(15);
			lstObservers.add_SelectedIndexChanged((EventHandler)lstObservers_SelectedIndexChanged);
			((Control)cmdUpdate).set_Location(new Point(98, 8));
			((Control)cmdUpdate).set_Name("cmdUpdate");
			((Control)cmdUpdate).set_Size(new Size(70, 34));
			((Control)cmdUpdate).set_TabIndex(13);
			((Control)cmdUpdate).set_Text("Update this record");
			((ButtonBase)cmdUpdate).set_UseVisualStyleBackColor(true);
			((Control)cmdUpdate).add_Click((EventHandler)cmdUpdate_Click);
			((Control)cmdDelete).set_Location(new Point(193, 8));
			((Control)cmdDelete).set_Name("cmdDelete");
			((Control)cmdDelete).set_Size(new Size(70, 34));
			((Control)cmdDelete).set_TabIndex(14);
			((Control)cmdDelete).set_Text("Delete this record");
			((ButtonBase)cmdDelete).set_UseVisualStyleBackColor(true);
			((Control)cmdDelete).add_Click((EventHandler)cmdDelete_Click);
			((Control)groupBox2).get_Controls().Add((Control)(object)chkLightCurve);
			((Control)groupBox2).get_Controls().Add((Control)(object)label39);
			((Control)groupBox2).get_Controls().Add((Control)(object)groupBox3);
			((Control)groupBox2).get_Controls().Add((Control)(object)chkShowRegions);
			((Control)groupBox2).get_Controls().Add((Control)(object)label2);
			((Control)groupBox2).get_Controls().Add((Control)(object)updnPlotScale);
			((Control)groupBox2).get_Controls().Add((Control)(object)label1);
			((Control)groupBox2).get_Controls().Add((Control)(object)updnSecsToDisplay);
			((Control)groupBox2).get_Controls().Add((Control)(object)cmdPlot);
			((Control)groupBox2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox2).set_Location(new Point(698, 234));
			((Control)groupBox2).set_Name("groupBox2");
			((Control)groupBox2).set_Size(new Size(261, 116));
			((Control)groupBox2).set_TabIndex(5);
			groupBox2.set_TabStop(false);
			((Control)groupBox2).set_Text("Plot controls");
			((Control)chkLightCurve).set_AutoSize(true);
			chkLightCurve.set_Checked(true);
			chkLightCurve.set_CheckState((CheckState)1);
			((Control)chkLightCurve).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkLightCurve).set_Location(new Point(10, 89));
			((Control)chkLightCurve).set_Name("chkLightCurve");
			((Control)chkLightCurve).set_Size(new Size(79, 17));
			((Control)chkLightCurve).set_TabIndex(22);
			((Control)chkLightCurve).set_Text("Light curve");
			((ButtonBase)chkLightCurve).set_UseVisualStyleBackColor(true);
			chkLightCurve.add_CheckedChanged((EventHandler)chkLightCurve_CheckedChanged);
			((Control)label39).set_AutoSize(true);
			((Control)label39).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label39).set_Location(new Point(26, 58));
			((Control)label39).set_Name("label39");
			((Control)label39).set_Size(new Size(38, 13));
			((Control)label39).set_TabIndex(21);
			((Control)label39).set_Text("Show");
			((Control)groupBox3).get_Controls().Add((Control)(object)optSmall);
			((Control)groupBox3).get_Controls().Add((Control)(object)optLarge);
			((Control)groupBox3).get_Controls().Add((Control)(object)cmdPlotAll);
			((Control)groupBox3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox3).set_Location(new Point(116, 58));
			((Control)groupBox3).set_Name("groupBox3");
			((Control)groupBox3).set_Size(new Size(138, 53));
			((Control)groupBox3).set_TabIndex(19);
			groupBox3.set_TabStop(false);
			((Control)groupBox3).set_Text("Plot ALL observns");
			((Control)optSmall).set_AutoSize(true);
			optSmall.set_Checked(true);
			((Control)optSmall).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optSmall).set_Location(new Point(8, 13));
			((Control)optSmall).set_Name("optSmall");
			((Control)optSmall).set_Size(new Size(50, 17));
			((Control)optSmall).set_TabIndex(18);
			optSmall.set_TabStop(true);
			((Control)optSmall).set_Text("Small");
			((ButtonBase)optSmall).set_UseVisualStyleBackColor(true);
			((Control)optLarge).set_AutoSize(true);
			((Control)optLarge).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optLarge).set_Location(new Point(8, 30));
			((Control)optLarge).set_Name("optLarge");
			((Control)optLarge).set_Size(new Size(52, 17));
			((Control)optLarge).set_TabIndex(17);
			((Control)optLarge).set_Text("Large");
			((ButtonBase)optLarge).set_UseVisualStyleBackColor(true);
			((Control)cmdPlotAll).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdPlotAll).set_Location(new Point(62, 16));
			((Control)cmdPlotAll).set_Name("cmdPlotAll");
			((Control)cmdPlotAll).set_Size(new Size(70, 31));
			((Control)cmdPlotAll).set_TabIndex(16);
			((Control)cmdPlotAll).set_Text("Plot ALL");
			((ButtonBase)cmdPlotAll).set_UseVisualStyleBackColor(true);
			((Control)cmdPlotAll).add_Click((EventHandler)cmdPlotAll_Click);
			((Control)chkShowRegions).set_AutoSize(true);
			chkShowRegions.set_Checked(true);
			chkShowRegions.set_CheckState((CheckState)1);
			((Control)chkShowRegions).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkShowRegions).set_Location(new Point(10, 71));
			((Control)chkShowRegions).set_Name("chkShowRegions");
			((Control)chkShowRegions).set_Size(new Size(88, 17));
			((Control)chkShowRegions).set_TabIndex(20);
			((Control)chkShowRegions).set_Text("Region Lines");
			((ButtonBase)chkShowRegions).set_UseVisualStyleBackColor(true);
			chkShowRegions.add_CheckedChanged((EventHandler)chkShowRegions_CheckedChanged);
			((Control)picR).set_BackColor(Color.White);
			((Control)picR).set_Location(new Point(364, 119));
			((Control)picR).set_Name("picR");
			((Control)picR).set_Size(new Size(321, 321));
			picR.set_TabIndex(1);
			picR.set_TabStop(false);
			((Control)picR).add_MouseMove(new MouseEventHandler(picR_MouseMove));
			((Control)picR).add_MouseUp(new MouseEventHandler(picR_MouseUp));
			((Control)picD).set_BackColor(Color.White);
			((Control)picD).set_Location(new Point(12, 119));
			((Control)picD).set_Name("picD");
			((Control)picD).set_Size(new Size(321, 321));
			picD.set_TabIndex(0);
			picD.set_TabStop(false);
			((Control)picD).add_MouseMove(new MouseEventHandler(picD_MouseMove));
			((Control)picD).add_MouseUp(new MouseEventHandler(picD_MouseUp));
			((Control)grpCalibrateD).get_Controls().Add((Control)(object)label13);
			((Control)grpCalibrateD).get_Controls().Add((Control)(object)label8);
			((Control)grpCalibrateD).get_Controls().Add((Control)(object)txtMeas2D);
			((Control)grpCalibrateD).get_Controls().Add((Control)(object)txtMeas1D);
			((Control)grpCalibrateD).get_Controls().Add((Control)(object)optSetMeasureD);
			((Control)grpCalibrateD).get_Controls().Add((Control)(object)txtTopDSD);
			((Control)grpCalibrateD).get_Controls().Add((Control)(object)txtBottomDSD);
			((Control)grpCalibrateD).get_Controls().Add((Control)(object)txtTop2D);
			((Control)grpCalibrateD).get_Controls().Add((Control)(object)txtBottom2D);
			((Control)grpCalibrateD).get_Controls().Add((Control)(object)txtBottom1D);
			((Control)grpCalibrateD).get_Controls().Add((Control)(object)txtTop1D);
			((Control)grpCalibrateD).get_Controls().Add((Control)(object)cmdCalibrateD);
			((Control)grpCalibrateD).get_Controls().Add((Control)(object)optSetBottomD);
			((Control)grpCalibrateD).get_Controls().Add((Control)(object)optSetTopD);
			((Control)grpCalibrateD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpCalibrateD).set_Location(new Point(12, 605));
			((Control)grpCalibrateD).set_Name("grpCalibrateD");
			((Control)grpCalibrateD).set_Size(new Size(321, 116));
			((Control)grpCalibrateD).set_TabIndex(16);
			grpCalibrateD.set_TabStop(false);
			((Control)grpCalibrateD).set_Text("Calibrate D curve");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label13).set_Location(new Point(155, 8));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(76, 13));
			((Control)label13).set_TabIndex(16);
			((Control)label13).set_Text("Frame location");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(257, 27));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(46, 13));
			((Control)label8).set_TabIndex(15);
			((Control)label8).set_Text("Std Dev");
			((TextBoxBase)txtMeas2D).set_BorderStyle((BorderStyle)1);
			((Control)txtMeas2D).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMeas2D).set_Location(new Point(195, 23));
			((Control)txtMeas2D).set_Name("txtMeas2D");
			((TextBoxBase)txtMeas2D).set_ReadOnly(true);
			((Control)txtMeas2D).set_Size(new Size(39, 20));
			((Control)txtMeas2D).set_TabIndex(14);
			((Control)txtMeas2D).set_Text("0");
			((TextBoxBase)txtMeas1D).set_BorderStyle((BorderStyle)1);
			((Control)txtMeas1D).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMeas1D).set_Location(new Point(151, 23));
			((Control)txtMeas1D).set_Name("txtMeas1D");
			((TextBoxBase)txtMeas1D).set_ReadOnly(true);
			((Control)txtMeas1D).set_Size(new Size(39, 20));
			((Control)txtMeas1D).set_TabIndex(13);
			((Control)txtMeas1D).set_Text("0");
			((Control)optSetMeasureD).set_AutoSize(true);
			optSetMeasureD.set_Checked(true);
			((Control)optSetMeasureD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optSetMeasureD).set_Location(new Point(6, 24));
			((Control)optSetMeasureD).set_Name("optSetMeasureD");
			((Control)optSetMeasureD).set_Size(new Size(139, 17));
			((Control)optSetMeasureD).set_TabIndex(12);
			optSetMeasureD.set_TabStop(true);
			((Control)optSetMeasureD).set_Text("Set measurement region");
			((ButtonBase)optSetMeasureD).set_UseVisualStyleBackColor(true);
			((TextBoxBase)txtTopDSD).set_BorderStyle((BorderStyle)1);
			((Control)txtTopDSD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtTopDSD).set_Location(new Point(257, 48));
			((Control)txtTopDSD).set_Name("txtTopDSD");
			((TextBoxBase)txtTopDSD).set_ReadOnly(true);
			((Control)txtTopDSD).set_Size(new Size(44, 20));
			((Control)txtTopDSD).set_TabIndex(9);
			((Control)txtTopDSD).set_Text("0");
			((TextBoxBase)txtBottomDSD).set_BorderStyle((BorderStyle)1);
			((Control)txtBottomDSD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtBottomDSD).set_Location(new Point(257, 72));
			((Control)txtBottomDSD).set_Name("txtBottomDSD");
			((TextBoxBase)txtBottomDSD).set_ReadOnly(true);
			((Control)txtBottomDSD).set_Size(new Size(44, 20));
			((Control)txtBottomDSD).set_TabIndex(8);
			((Control)txtBottomDSD).set_Text("0");
			((TextBoxBase)txtTop2D).set_BorderStyle((BorderStyle)1);
			((Control)txtTop2D).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtTop2D).set_Location(new Point(195, 47));
			((Control)txtTop2D).set_Name("txtTop2D");
			((TextBoxBase)txtTop2D).set_ReadOnly(true);
			((Control)txtTop2D).set_Size(new Size(39, 20));
			((Control)txtTop2D).set_TabIndex(7);
			((Control)txtTop2D).set_Text("0");
			((TextBoxBase)txtBottom2D).set_BorderStyle((BorderStyle)1);
			((Control)txtBottom2D).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtBottom2D).set_Location(new Point(195, 71));
			((Control)txtBottom2D).set_Name("txtBottom2D");
			((TextBoxBase)txtBottom2D).set_ReadOnly(true);
			((Control)txtBottom2D).set_Size(new Size(39, 20));
			((Control)txtBottom2D).set_TabIndex(6);
			((Control)txtBottom2D).set_Text("0");
			((TextBoxBase)txtBottom1D).set_BorderStyle((BorderStyle)1);
			((Control)txtBottom1D).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtBottom1D).set_Location(new Point(151, 71));
			((Control)txtBottom1D).set_Name("txtBottom1D");
			((TextBoxBase)txtBottom1D).set_ReadOnly(true);
			((Control)txtBottom1D).set_Size(new Size(39, 20));
			((Control)txtBottom1D).set_TabIndex(5);
			((Control)txtBottom1D).set_Text("0");
			((TextBoxBase)txtTop1D).set_BorderStyle((BorderStyle)1);
			((Control)txtTop1D).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtTop1D).set_Location(new Point(151, 47));
			((Control)txtTop1D).set_Name("txtTop1D");
			((TextBoxBase)txtTop1D).set_ReadOnly(true);
			((Control)txtTop1D).set_Size(new Size(39, 20));
			((Control)txtTop1D).set_TabIndex(4);
			((Control)txtTop1D).set_Text("0");
			((Control)cmdCalibrateD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdCalibrateD).set_Location(new Point(45, 90));
			((Control)cmdCalibrateD).set_Name("cmdCalibrateD");
			((Control)cmdCalibrateD).set_Size(new Size(114, 20));
			((Control)cmdCalibrateD).set_TabIndex(3);
			((Control)cmdCalibrateD).set_Text("Calibrate levels");
			((ButtonBase)cmdCalibrateD).set_UseVisualStyleBackColor(true);
			((Control)cmdCalibrateD).add_Click((EventHandler)cmdCalibrateD_Click);
			((Control)optSetBottomD).set_AutoSize(true);
			((Control)optSetBottomD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optSetBottomD).set_Location(new Point(6, 73));
			((Control)optSetBottomD).set_Name("optSetBottomD");
			((Control)optSetBottomD).set_Size(new Size(101, 17));
			((Control)optSetBottomD).set_TabIndex(1);
			((Control)optSetBottomD).set_Text("Set bottom level");
			((ButtonBase)optSetBottomD).set_UseVisualStyleBackColor(true);
			((Control)optSetTopD).set_AutoSize(true);
			((Control)optSetTopD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optSetTopD).set_Location(new Point(6, 49));
			((Control)optSetTopD).set_Name("optSetTopD");
			((Control)optSetTopD).set_Size(new Size(84, 17));
			((Control)optSetTopD).set_TabIndex(0);
			((Control)optSetTopD).set_Text("Set top level");
			((ButtonBase)optSetTopD).set_UseVisualStyleBackColor(true);
			((Control)grpCalibrateR).get_Controls().Add((Control)(object)label15);
			((Control)grpCalibrateR).get_Controls().Add((Control)(object)label11);
			((Control)grpCalibrateR).get_Controls().Add((Control)(object)txtMeas2R);
			((Control)grpCalibrateR).get_Controls().Add((Control)(object)txtMeas1R);
			((Control)grpCalibrateR).get_Controls().Add((Control)(object)optSetMeasureR);
			((Control)grpCalibrateR).get_Controls().Add((Control)(object)txtTopRSD);
			((Control)grpCalibrateR).get_Controls().Add((Control)(object)txtBottomRSD);
			((Control)grpCalibrateR).get_Controls().Add((Control)(object)txtTop2R);
			((Control)grpCalibrateR).get_Controls().Add((Control)(object)txtBottom2R);
			((Control)grpCalibrateR).get_Controls().Add((Control)(object)txtBottom1R);
			((Control)grpCalibrateR).get_Controls().Add((Control)(object)txtTop1R);
			((Control)grpCalibrateR).get_Controls().Add((Control)(object)cmdCalibrateR);
			((Control)grpCalibrateR).get_Controls().Add((Control)(object)optSetBottomR);
			((Control)grpCalibrateR).get_Controls().Add((Control)(object)optSetTopR);
			((Control)grpCalibrateR).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpCalibrateR).set_Location(new Point(364, 605));
			((Control)grpCalibrateR).set_Name("grpCalibrateR");
			((Control)grpCalibrateR).set_Size(new Size(321, 116));
			((Control)grpCalibrateR).set_TabIndex(17);
			grpCalibrateR.set_TabStop(false);
			((Control)grpCalibrateR).set_Text("Calibrate R curve");
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label15).set_Location(new Point(154, 8));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(75, 13));
			((Control)label15).set_TabIndex(22);
			((Control)label15).set_Text("Frame location");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label11).set_Location(new Point(257, 32));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(46, 13));
			((Control)label11).set_TabIndex(21);
			((Control)label11).set_Text("Std Dev");
			((TextBoxBase)txtMeas2R).set_BorderStyle((BorderStyle)1);
			((Control)txtMeas2R).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMeas2R).set_Location(new Point(195, 24));
			((Control)txtMeas2R).set_Name("txtMeas2R");
			((TextBoxBase)txtMeas2R).set_ReadOnly(true);
			((Control)txtMeas2R).set_Size(new Size(39, 20));
			((Control)txtMeas2R).set_TabIndex(20);
			((Control)txtMeas2R).set_Text("0");
			((TextBoxBase)txtMeas1R).set_BorderStyle((BorderStyle)1);
			((Control)txtMeas1R).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMeas1R).set_Location(new Point(151, 24));
			((Control)txtMeas1R).set_Name("txtMeas1R");
			((TextBoxBase)txtMeas1R).set_ReadOnly(true);
			((Control)txtMeas1R).set_Size(new Size(39, 20));
			((Control)txtMeas1R).set_TabIndex(19);
			((Control)txtMeas1R).set_Text("0");
			((Control)optSetMeasureR).set_AutoSize(true);
			optSetMeasureR.set_Checked(true);
			((Control)optSetMeasureR).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optSetMeasureR).set_Location(new Point(6, 24));
			((Control)optSetMeasureR).set_Name("optSetMeasureR");
			((Control)optSetMeasureR).set_Size(new Size(139, 17));
			((Control)optSetMeasureR).set_TabIndex(18);
			optSetMeasureR.set_TabStop(true);
			((Control)optSetMeasureR).set_Text("Set measurement region");
			((ButtonBase)optSetMeasureR).set_UseVisualStyleBackColor(true);
			((TextBoxBase)txtTopRSD).set_BorderStyle((BorderStyle)1);
			((Control)txtTopRSD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtTopRSD).set_Location(new Point(257, 70));
			((Control)txtTopRSD).set_Name("txtTopRSD");
			((TextBoxBase)txtTopRSD).set_ReadOnly(true);
			((Control)txtTopRSD).set_Size(new Size(44, 20));
			((Control)txtTopRSD).set_TabIndex(15);
			((Control)txtTopRSD).set_Text("0");
			((TextBoxBase)txtBottomRSD).set_BorderStyle((BorderStyle)1);
			((Control)txtBottomRSD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtBottomRSD).set_Location(new Point(257, 48));
			((Control)txtBottomRSD).set_Name("txtBottomRSD");
			((TextBoxBase)txtBottomRSD).set_ReadOnly(true);
			((Control)txtBottomRSD).set_Size(new Size(44, 20));
			((Control)txtBottomRSD).set_TabIndex(14);
			((Control)txtBottomRSD).set_Text("0");
			((TextBoxBase)txtTop2R).set_BorderStyle((BorderStyle)1);
			((Control)txtTop2R).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtTop2R).set_Location(new Point(195, 70));
			((Control)txtTop2R).set_Name("txtTop2R");
			((TextBoxBase)txtTop2R).set_ReadOnly(true);
			((Control)txtTop2R).set_Size(new Size(39, 20));
			((Control)txtTop2R).set_TabIndex(13);
			((Control)txtTop2R).set_Text("0");
			((TextBoxBase)txtBottom2R).set_BorderStyle((BorderStyle)1);
			((Control)txtBottom2R).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtBottom2R).set_Location(new Point(195, 48));
			((Control)txtBottom2R).set_Name("txtBottom2R");
			((TextBoxBase)txtBottom2R).set_ReadOnly(true);
			((Control)txtBottom2R).set_Size(new Size(39, 20));
			((Control)txtBottom2R).set_TabIndex(12);
			((Control)txtBottom2R).set_Text("0");
			((TextBoxBase)txtBottom1R).set_BorderStyle((BorderStyle)1);
			((Control)txtBottom1R).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtBottom1R).set_Location(new Point(151, 48));
			((Control)txtBottom1R).set_Name("txtBottom1R");
			((TextBoxBase)txtBottom1R).set_ReadOnly(true);
			((Control)txtBottom1R).set_Size(new Size(39, 20));
			((Control)txtBottom1R).set_TabIndex(11);
			((Control)txtBottom1R).set_Text("0");
			((TextBoxBase)txtTop1R).set_BorderStyle((BorderStyle)1);
			((Control)txtTop1R).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtTop1R).set_Location(new Point(151, 70));
			((Control)txtTop1R).set_Name("txtTop1R");
			((TextBoxBase)txtTop1R).set_ReadOnly(true);
			((Control)txtTop1R).set_Size(new Size(39, 20));
			((Control)txtTop1R).set_TabIndex(10);
			((Control)txtTop1R).set_Text("0");
			((Control)cmdCalibrateR).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdCalibrateR).set_Location(new Point(43, 91));
			((Control)cmdCalibrateR).set_Name("cmdCalibrateR");
			((Control)cmdCalibrateR).set_Size(new Size(114, 19));
			((Control)cmdCalibrateR).set_TabIndex(6);
			((Control)cmdCalibrateR).set_Text("Calibrate levels");
			((ButtonBase)cmdCalibrateR).set_UseVisualStyleBackColor(true);
			((Control)cmdCalibrateR).add_Click((EventHandler)cmdCalibrateR_Click);
			((Control)optSetBottomR).set_AutoSize(true);
			((Control)optSetBottomR).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optSetBottomR).set_Location(new Point(6, 49));
			((Control)optSetBottomR).set_Name("optSetBottomR");
			((Control)optSetBottomR).set_Size(new Size(101, 17));
			((Control)optSetBottomR).set_TabIndex(4);
			((Control)optSetBottomR).set_Text("Set bottom level");
			((ButtonBase)optSetBottomR).set_UseVisualStyleBackColor(true);
			((Control)optSetTopR).set_AutoSize(true);
			((Control)optSetTopR).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optSetTopR).set_Location(new Point(6, 73));
			((Control)optSetTopR).set_Name("optSetTopR");
			((Control)optSetTopR).set_Size(new Size(84, 17));
			((Control)optSetTopR).set_TabIndex(3);
			((Control)optSetTopR).set_Text("Set top level");
			((ButtonBase)optSetTopR).set_UseVisualStyleBackColor(true);
			((Control)groupBox4).get_Controls().Add((Control)(object)chkLimbDarkening);
			((Control)groupBox4).get_Controls().Add((Control)(object)chkPartial);
			((Control)groupBox4).get_Controls().Add((Control)(object)label12);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmbSaturation);
			((Control)groupBox4).get_Controls().Add((Control)(object)picStar);
			((Control)groupBox4).get_Controls().Add((Control)(object)label40);
			((Control)groupBox4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox4).set_Location(new Point(698, 142));
			((Control)groupBox4).set_Name("groupBox4");
			((Control)groupBox4).set_Size(new Size(256, 84));
			((Control)groupBox4).set_TabIndex(20);
			groupBox4.set_TabStop(false);
			((Control)groupBox4).set_Text("Partial / Limb Darkening /  Saturation");
			((Control)chkPartial).set_AutoSize(true);
			((Control)chkPartial).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkPartial).set_Location(new Point(10, 18));
			((Control)chkPartial).set_Name("chkPartial");
			((Control)chkPartial).set_Size(new Size(85, 17));
			((Control)chkPartial).set_TabIndex(19);
			((Control)chkPartial).set_Text("Partial event");
			((ButtonBase)chkPartial).set_UseVisualStyleBackColor(true);
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(117, 17));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(55, 13));
			((Control)label12).set_TabIndex(18);
			((Control)label12).set_Text("Saturation");
			((Control)cmbSaturation).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbSaturation).set_FormattingEnabled(true);
			cmbSaturation.get_Items().AddRange(new object[20]
			{
				"100%+", "95%", "90%", "85%", "80%", "75%", "70%", "65%", "60%", "55%",
				"50%", "45%", "40%", "35%", "30%", "25%", "20%", "15%", "10%", "5%"
			});
			((Control)cmbSaturation).set_Location(new Point(117, 30));
			((Control)cmbSaturation).set_Name("cmbSaturation");
			((Control)cmbSaturation).set_Size(new Size(55, 21));
			((Control)cmbSaturation).set_TabIndex(16);
			cmbSaturation.add_SelectedIndexChanged((EventHandler)cmbSaturation_SelectedIndexChanged);
			((Control)picStar).set_BackColor(Color.White);
			picStar.set_BorderStyle((BorderStyle)1);
			((Control)picStar).set_Location(new Point(176, 14));
			((Control)picStar).set_Name("picStar");
			((Control)picStar).set_Size(new Size(71, 64));
			picStar.set_TabIndex(17);
			picStar.set_TabStop(false);
			((Control)label40).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label40).set_Location(new Point(-3, 50));
			((Control)label40).set_Name("label40");
			((Control)label40).set_Size(new Size(178, 30));
			((Control)label40).set_TabIndex(20);
			((Control)label40).set_Text("NOTE: Saturation is relevant to Aperture Photometry. For PSF, set level at 100%.");
			label40.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)grpSolveD).get_Controls().Add((Control)(object)picNormD);
			((Control)grpSolveD).get_Controls().Add((Control)(object)txtSDD);
			((Control)grpSolveD).get_Controls().Add((Control)(object)panel2);
			((Control)grpSolveD).get_Controls().Add((Control)(object)label14);
			((Control)grpSolveD).get_Controls().Add((Control)(object)picResD);
			((Control)grpSolveD).get_Controls().Add((Control)(object)label18);
			((Control)grpSolveD).get_Controls().Add((Control)(object)updnDiaD);
			((Control)grpSolveD).get_Controls().Add((Control)(object)label16);
			((Control)grpSolveD).get_Controls().Add((Control)(object)label21);
			((Control)grpSolveD).get_Controls().Add((Control)(object)txtCommentD);
			((Control)grpSolveD).get_Controls().Add((Control)(object)label30);
			((Control)grpSolveD).get_Controls().Add((Control)(object)updnWeightD);
			((Control)grpSolveD).get_Controls().Add((Control)(object)label9);
			((Control)grpSolveD).get_Controls().Add((Control)(object)updnDAdjust);
			((Control)grpSolveD).get_Controls().Add((Control)(object)lblMidD);
			((Control)grpSolveD).get_Controls().Add((Control)(object)chkx10D);
			((Control)grpSolveD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpSolveD).set_Location(new Point(13, 448));
			((Control)grpSolveD).set_Name("grpSolveD");
			((Control)grpSolveD).set_Size(new Size(320, 152));
			((Control)grpSolveD).set_TabIndex(21);
			grpSolveD.set_TabStop(false);
			((Control)grpSolveD).set_Text("Solve D");
			((Control)picNormD).set_BackColor(Color.White);
			picNormD.set_BorderStyle((BorderStyle)1);
			((Control)picNormD).set_Location(new Point(234, 52));
			((Control)picNormD).set_Name("picNormD");
			((Control)picNormD).set_Size(new Size(81, 61));
			picNormD.set_TabIndex(39);
			picNormD.set_TabStop(false);
			((Control)txtSDD).set_AutoSize(true);
			((Control)txtSDD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtSDD).set_Location(new Point(177, 38));
			((Control)txtSDD).set_Name("txtSDD");
			((Control)txtSDD).set_Size(new Size(45, 13));
			((Control)txtSDD).set_TabIndex(35);
			((Control)txtSDD).set_Text("SDev D");
			panel2.set_BorderStyle((BorderStyle)1);
			((Control)panel2).get_Controls().Add((Control)(object)label35);
			((Control)panel2).get_Controls().Add((Control)(object)label34);
			((Control)panel2).get_Controls().Add((Control)(object)label33);
			((Control)panel2).get_Controls().Add((Control)(object)lblDiffD);
			((Control)panel2).get_Controls().Add((Control)(object)label28);
			((Control)panel2).get_Controls().Add((Control)(object)label25);
			((Control)panel2).get_Controls().Add((Control)(object)label23);
			((Control)panel2).get_Controls().Add((Control)(object)lblChi2Ddia2);
			((Control)panel2).get_Controls().Add((Control)(object)lblChi2Ddia1);
			((Control)panel2).get_Controls().Add((Control)(object)lblChi2Ddia0);
			((Control)panel2).get_Controls().Add((Control)(object)lblChi2Dframe2);
			((Control)panel2).get_Controls().Add((Control)(object)lblChi2Dframe1);
			((Control)panel2).get_Controls().Add((Control)(object)lblChi2Dframe0);
			((Control)panel2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)panel2).set_Location(new Point(9, 40));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(135, 73));
			((Control)panel2).set_TabIndex(29);
			((Control)label35).set_AutoSize(true);
			((Control)label35).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label35).set_Location(new Point(21, 27));
			((Control)label35).set_Name("label35");
			((Control)label35).set_Size(new Size(13, 13));
			((Control)label35).set_TabIndex(28);
			((Control)label35).set_Text("0");
			((Control)label34).set_AutoSize(true);
			((Control)label34).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label34).set_Location(new Point(15, 40));
			((Control)label34).set_Name("label34");
			((Control)label34).set_Size(new Size(19, 13));
			((Control)label34).set_TabIndex(27);
			((Control)label34).set_Text("+1");
			((Control)label33).set_AutoSize(true);
			((Control)label33).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label33).set_Location(new Point(18, 13));
			((Control)label33).set_Name("label33");
			((Control)label33).set_Size(new Size(16, 13));
			((Control)label33).set_TabIndex(26);
			((Control)label33).set_Text("-1");
			((Control)lblDiffD).set_AutoSize(true);
			((Control)lblDiffD).set_Location(new Point(86, 54));
			((Control)lblDiffD).set_Name("lblDiffD");
			((Control)lblDiffD).set_Size(new Size(26, 13));
			((Control)lblDiffD).set_TabIndex(20);
			((Control)lblDiffD).set_Text("Difd");
			((Control)label28).set_AutoSize(true);
			((Control)label28).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label28).set_Location(new Point(86, 1));
			((Control)label28).set_Name("label28");
			((Control)label28).set_Size(new Size(45, 13));
			((Control)label28).set_TabIndex(19);
			((Control)label28).set_Text("by dia.");
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label25).set_Location(new Point(27, 1));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(55, 13));
			((Control)label25).set_TabIndex(11);
			((Control)label25).set_Text("by frame");
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label23).set_Location(new Point(0, 0));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(23, 20));
			((Control)label23).set_TabIndex(10);
			((Control)label23).set_Text("χ²");
			((Control)lblChi2Ddia2).set_AutoSize(true);
			((Control)lblChi2Ddia2).set_Location(new Point(86, 40));
			((Control)lblChi2Ddia2).set_Name("lblChi2Ddia2");
			((Control)lblChi2Ddia2).set_Size(new Size(47, 13));
			((Control)lblChi2Ddia2).set_TabIndex(5);
			((Control)lblChi2Ddia2).set_Text("Dia +.10");
			((Control)lblChi2Ddia1).set_AutoSize(true);
			((Control)lblChi2Ddia1).set_Location(new Point(86, 27));
			((Control)lblChi2Ddia1).set_Name("lblChi2Ddia1");
			((Control)lblChi2Ddia1).set_Size(new Size(23, 13));
			((Control)lblChi2Ddia1).set_TabIndex(4);
			((Control)lblChi2Ddia1).set_Text("Dia");
			((Control)lblChi2Ddia0).set_AutoSize(true);
			((Control)lblChi2Ddia0).set_Location(new Point(86, 14));
			((Control)lblChi2Ddia0).set_Name("lblChi2Ddia0");
			((Control)lblChi2Ddia0).set_Size(new Size(44, 13));
			((Control)lblChi2Ddia0).set_TabIndex(3);
			((Control)lblChi2Ddia0).set_Text("Dia -.10");
			((Control)lblChi2Dframe2).set_AutoSize(true);
			((Control)lblChi2Dframe2).set_Location(new Point(34, 40));
			((Control)lblChi2Dframe2).set_Name("lblChi2Dframe2");
			((Control)lblChi2Dframe2).set_Size(new Size(51, 13));
			((Control)lblChi2Dframe2).set_TabIndex(2);
			((Control)lblChi2Dframe2).set_Text("Frame +1");
			((Control)lblChi2Dframe1).set_AutoSize(true);
			((Control)lblChi2Dframe1).set_Location(new Point(34, 27));
			((Control)lblChi2Dframe1).set_Name("lblChi2Dframe1");
			((Control)lblChi2Dframe1).set_Size(new Size(45, 13));
			((Control)lblChi2Dframe1).set_TabIndex(1);
			((Control)lblChi2Dframe1).set_Text("Frame 0");
			((Control)lblChi2Dframe0).set_AutoSize(true);
			((Control)lblChi2Dframe0).set_Location(new Point(34, 14));
			((Control)lblChi2Dframe0).set_Name("lblChi2Dframe0");
			((Control)lblChi2Dframe0).set_Size(new Size(48, 13));
			((Control)lblChi2Dframe0).set_TabIndex(0);
			((Control)lblChi2Dframe0).set_Text("Frame -1");
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label14).set_Location(new Point(150, 38));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(25, 13));
			((Control)label14).set_TabIndex(25);
			((Control)label14).set_Text("O-C");
			((Control)picResD).set_BackColor(Color.White);
			picResD.set_BorderStyle((BorderStyle)1);
			((Control)picResD).set_Location(new Point(147, 52));
			((Control)picResD).set_Name("picResD");
			((Control)picResD).set_Size(new Size(81, 61));
			picResD.set_TabIndex(24);
			picResD.set_TabStop(false);
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label18).set_Location(new Point(149, 15));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(23, 13));
			((Control)label18).set_TabIndex(23);
			((Control)label18).set_Text("Dia");
			updnDiaD.set_DecimalPlaces(2);
			((Control)updnDiaD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnDiaD.set_Increment(new decimal(new int[4] { 1, 0, 0, 131072 }));
			((Control)updnDiaD).set_Location(new Point(172, 12));
			((Control)updnDiaD).set_Name("updnDiaD");
			((Control)updnDiaD).set_Size(new Size(50, 20));
			((Control)updnDiaD).set_TabIndex(2);
			updnDiaD.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnDiaD.add_ValueChanged((EventHandler)updnDiaD_ValueChanged);
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label16).set_Location(new Point(222, 15));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(26, 13));
			((Control)label16).set_TabIndex(3);
			((Control)label16).set_Text("mas");
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label21).set_Location(new Point(6, 124));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(51, 13));
			((Control)label21).set_TabIndex(9);
			((Control)label21).set_Text("Comment");
			((Control)txtCommentD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtCommentD).set_Location(new Point(57, 121));
			((Control)txtCommentD).set_Name("txtCommentD");
			((Control)txtCommentD).set_Size(new Size(258, 20));
			((Control)txtCommentD).set_TabIndex(8);
			((Control)label30).set_AutoSize(true);
			((Control)label30).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label30).set_Location(new Point(265, 17));
			((Control)label30).set_Name("label30");
			((Control)label30).set_Size(new Size(21, 13));
			((Control)label30).set_TabIndex(7);
			((Control)label30).set_Text("Wt");
			((Control)updnWeightD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnWeightD).set_Location(new Point(286, 13));
			updnWeightD.set_Maximum(new decimal(new int[4] { 5, 0, 0, 0 }));
			((Control)updnWeightD).set_Name("updnWeightD");
			((Control)updnWeightD).set_Size(new Size(29, 20));
			((Control)updnWeightD).set_TabIndex(6);
			((Control)chkx10D).set_AutoSize(true);
			chkx10D.set_CheckAlign(ContentAlignment.BottomCenter);
			((Control)chkx10D).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkx10D).set_Location(new Point(244, 7));
			((Control)chkx10D).set_Name("chkx10D");
			((Control)chkx10D).set_Size(new Size(28, 31));
			((Control)chkx10D).set_TabIndex(37);
			((Control)chkx10D).set_Text("x10");
			((ButtonBase)chkx10D).set_UseVisualStyleBackColor(true);
			chkx10D.add_CheckedChanged((EventHandler)chkx10D_CheckedChanged);
			((Control)grpSolveR).get_Controls().Add((Control)(object)picNormR);
			((Control)grpSolveR).get_Controls().Add((Control)(object)txtSDR);
			((Control)grpSolveR).get_Controls().Add((Control)(object)panel3);
			((Control)grpSolveR).get_Controls().Add((Control)(object)label32);
			((Control)grpSolveR).get_Controls().Add((Control)(object)picResR);
			((Control)grpSolveR).get_Controls().Add((Control)(object)label19);
			((Control)grpSolveR).get_Controls().Add((Control)(object)updnDiaR);
			((Control)grpSolveR).get_Controls().Add((Control)(object)label17);
			((Control)grpSolveR).get_Controls().Add((Control)(object)label22);
			((Control)grpSolveR).get_Controls().Add((Control)(object)txtCommentR);
			((Control)grpSolveR).get_Controls().Add((Control)(object)label20);
			((Control)grpSolveR).get_Controls().Add((Control)(object)updnWeightR);
			((Control)grpSolveR).get_Controls().Add((Control)(object)label10);
			((Control)grpSolveR).get_Controls().Add((Control)(object)lblMidR);
			((Control)grpSolveR).get_Controls().Add((Control)(object)updnRAdjust);
			((Control)grpSolveR).get_Controls().Add((Control)(object)chkx10R);
			((Control)grpSolveR).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpSolveR).set_Location(new Point(365, 448));
			((Control)grpSolveR).set_Name("grpSolveR");
			((Control)grpSolveR).set_Size(new Size(320, 152));
			((Control)grpSolveR).set_TabIndex(22);
			grpSolveR.set_TabStop(false);
			((Control)grpSolveR).set_Text("Solve R");
			((Control)picNormR).set_BackColor(Color.White);
			picNormR.set_BorderStyle((BorderStyle)1);
			((Control)picNormR).set_Location(new Point(233, 52));
			((Control)picNormR).set_Name("picNormR");
			((Control)picNormR).set_Size(new Size(81, 61));
			picNormR.set_TabIndex(40);
			picNormR.set_TabStop(false);
			((Control)txtSDR).set_AutoSize(true);
			((Control)txtSDR).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtSDR).set_Location(new Point(182, 38));
			((Control)txtSDR).set_Name("txtSDR");
			((Control)txtSDR).set_Size(new Size(45, 13));
			((Control)txtSDR).set_TabIndex(36);
			((Control)txtSDR).set_Text("SDev R");
			panel3.set_BorderStyle((BorderStyle)1);
			((Control)panel3).get_Controls().Add((Control)(object)label36);
			((Control)panel3).get_Controls().Add((Control)(object)label37);
			((Control)panel3).get_Controls().Add((Control)(object)label38);
			((Control)panel3).get_Controls().Add((Control)(object)lblDiffR);
			((Control)panel3).get_Controls().Add((Control)(object)label27);
			((Control)panel3).get_Controls().Add((Control)(object)label26);
			((Control)panel3).get_Controls().Add((Control)(object)label24);
			((Control)panel3).get_Controls().Add((Control)(object)lblChi2Rdia2);
			((Control)panel3).get_Controls().Add((Control)(object)lblChi2Rdia1);
			((Control)panel3).get_Controls().Add((Control)(object)lblChi2Rdia0);
			((Control)panel3).get_Controls().Add((Control)(object)lblChi2Rframe2);
			((Control)panel3).get_Controls().Add((Control)(object)lblChi2Rframe1);
			((Control)panel3).get_Controls().Add((Control)(object)lblChi2Rframe0);
			((Control)panel3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)panel3).set_Location(new Point(9, 40));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(135, 73));
			((Control)panel3).set_TabIndex(32);
			((Control)label36).set_AutoSize(true);
			((Control)label36).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label36).set_Location(new Point(21, 27));
			((Control)label36).set_Name("label36");
			((Control)label36).set_Size(new Size(13, 13));
			((Control)label36).set_TabIndex(31);
			((Control)label36).set_Text("0");
			((Control)label37).set_AutoSize(true);
			((Control)label37).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label37).set_Location(new Point(15, 40));
			((Control)label37).set_Name("label37");
			((Control)label37).set_Size(new Size(19, 13));
			((Control)label37).set_TabIndex(30);
			((Control)label37).set_Text("+1");
			((Control)label38).set_AutoSize(true);
			((Control)label38).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label38).set_Location(new Point(18, 13));
			((Control)label38).set_Name("label38");
			((Control)label38).set_Size(new Size(16, 13));
			((Control)label38).set_TabIndex(29);
			((Control)label38).set_Text("-1");
			((Control)lblDiffR).set_AutoSize(true);
			((Control)lblDiffR).set_Location(new Point(86, 53));
			((Control)lblDiffR).set_Name("lblDiffR");
			((Control)lblDiffR).set_Size(new Size(23, 13));
			((Control)lblDiffR).set_TabIndex(21);
			((Control)lblDiffR).set_Text("Difr");
			((Control)label27).set_AutoSize(true);
			((Control)label27).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label27).set_Location(new Point(86, 1));
			((Control)label27).set_Name("label27");
			((Control)label27).set_Size(new Size(45, 13));
			((Control)label27).set_TabIndex(18);
			((Control)label27).set_Text("by dia.");
			((Control)label26).set_AutoSize(true);
			((Control)label26).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label26).set_Location(new Point(27, 1));
			((Control)label26).set_Name("label26");
			((Control)label26).set_Size(new Size(55, 13));
			((Control)label26).set_TabIndex(17);
			((Control)label26).set_Text("by frame");
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label24).set_Location(new Point(0, 0));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(23, 20));
			((Control)label24).set_TabIndex(16);
			((Control)label24).set_Text("χ²");
			((Control)lblChi2Rdia2).set_AutoSize(true);
			((Control)lblChi2Rdia2).set_Location(new Point(86, 40));
			((Control)lblChi2Rdia2).set_Name("lblChi2Rdia2");
			((Control)lblChi2Rdia2).set_Size(new Size(47, 13));
			((Control)lblChi2Rdia2).set_TabIndex(11);
			((Control)lblChi2Rdia2).set_Text("Dia +.10");
			((Control)lblChi2Rdia1).set_AutoSize(true);
			((Control)lblChi2Rdia1).set_Location(new Point(86, 27));
			((Control)lblChi2Rdia1).set_Name("lblChi2Rdia1");
			((Control)lblChi2Rdia1).set_Size(new Size(23, 13));
			((Control)lblChi2Rdia1).set_TabIndex(10);
			((Control)lblChi2Rdia1).set_Text("Dia");
			((Control)lblChi2Rdia0).set_AutoSize(true);
			((Control)lblChi2Rdia0).set_Location(new Point(86, 14));
			((Control)lblChi2Rdia0).set_Name("lblChi2Rdia0");
			((Control)lblChi2Rdia0).set_Size(new Size(44, 13));
			((Control)lblChi2Rdia0).set_TabIndex(9);
			((Control)lblChi2Rdia0).set_Text("Dia -.10");
			((Control)lblChi2Rframe2).set_AutoSize(true);
			((Control)lblChi2Rframe2).set_Location(new Point(34, 40));
			((Control)lblChi2Rframe2).set_Name("lblChi2Rframe2");
			((Control)lblChi2Rframe2).set_Size(new Size(45, 13));
			((Control)lblChi2Rframe2).set_TabIndex(8);
			((Control)lblChi2Rframe2).set_Text("Frame 1");
			((Control)lblChi2Rframe1).set_AutoSize(true);
			((Control)lblChi2Rframe1).set_Location(new Point(34, 27));
			((Control)lblChi2Rframe1).set_Name("lblChi2Rframe1");
			((Control)lblChi2Rframe1).set_Size(new Size(45, 13));
			((Control)lblChi2Rframe1).set_TabIndex(7);
			((Control)lblChi2Rframe1).set_Text("Frame 0");
			((Control)lblChi2Rframe0).set_AutoSize(true);
			((Control)lblChi2Rframe0).set_Location(new Point(34, 14));
			((Control)lblChi2Rframe0).set_Name("lblChi2Rframe0");
			((Control)lblChi2Rframe0).set_Size(new Size(48, 13));
			((Control)lblChi2Rframe0).set_TabIndex(6);
			((Control)lblChi2Rframe0).set_Text("Frame -1");
			((Control)label32).set_AutoSize(true);
			((Control)label32).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label32).set_Location(new Point(150, 38));
			((Control)label32).set_Name("label32");
			((Control)label32).set_Size(new Size(25, 13));
			((Control)label32).set_TabIndex(26);
			((Control)label32).set_Text("O-C");
			((Control)picResR).set_BackColor(Color.White);
			picResR.set_BorderStyle((BorderStyle)1);
			((Control)picResR).set_Location(new Point(149, 52));
			((Control)picResR).set_Name("picResR");
			((Control)picResR).set_Size(new Size(81, 61));
			picResR.set_TabIndex(25);
			picResR.set_TabStop(false);
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label19).set_Location(new Point(149, 17));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(23, 13));
			((Control)label19).set_TabIndex(24);
			((Control)label19).set_Text("Dia");
			updnDiaR.set_DecimalPlaces(2);
			((Control)updnDiaR).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnDiaR.set_Increment(new decimal(new int[4] { 1, 0, 0, 131072 }));
			((Control)updnDiaR).set_Location(new Point(172, 13));
			((Control)updnDiaR).set_Name("updnDiaR");
			((Control)updnDiaR).set_Size(new Size(50, 20));
			((Control)updnDiaR).set_TabIndex(2);
			updnDiaR.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnDiaR.add_ValueChanged((EventHandler)updnDiaR_ValueChanged);
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label17).set_Location(new Point(222, 17));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(26, 13));
			((Control)label17).set_TabIndex(3);
			((Control)label17).set_Text("mas");
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label22).set_Location(new Point(6, 124));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(51, 13));
			((Control)label22).set_TabIndex(15);
			((Control)label22).set_Text("Comment");
			((Control)txtCommentR).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtCommentR).set_Location(new Point(57, 121));
			((Control)txtCommentR).set_Name("txtCommentR");
			((Control)txtCommentR).set_Size(new Size(258, 20));
			((Control)txtCommentR).set_TabIndex(14);
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label20).set_Location(new Point(265, 17));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(21, 13));
			((Control)label20).set_TabIndex(13);
			((Control)label20).set_Text("Wt");
			((Control)updnWeightR).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnWeightR).set_Location(new Point(286, 13));
			updnWeightR.set_Maximum(new decimal(new int[4] { 5, 0, 0, 0 }));
			((Control)updnWeightR).set_Name("updnWeightR");
			((Control)updnWeightR).set_Size(new Size(29, 20));
			((Control)updnWeightR).set_TabIndex(12);
			((Control)chkx10R).set_AutoSize(true);
			chkx10R.set_CheckAlign(ContentAlignment.BottomCenter);
			((Control)chkx10R).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkx10R).set_Location(new Point(244, 8));
			((Control)chkx10R).set_Name("chkx10R");
			((Control)chkx10R).set_Size(new Size(28, 31));
			((Control)chkx10R).set_TabIndex(38);
			((Control)chkx10R).set_Text("x10");
			((ButtonBase)chkx10R).set_UseVisualStyleBackColor(true);
			chkx10R.add_CheckedChanged((EventHandler)chkx10R_CheckedChanged);
			panel4.set_BorderStyle((BorderStyle)1);
			((Control)panel4).get_Controls().Add((Control)(object)cmdDelete);
			((Control)panel4).get_Controls().Add((Control)(object)cmdUpdate);
			((Control)panel4).get_Controls().Add((Control)(object)lstObservers);
			((Control)panel4).get_Controls().Add((Control)(object)cmdAddObserver);
			((Control)panel4).set_Location(new Point(695, 358));
			((Control)panel4).set_Name("panel4");
			((Control)panel4).set_Size(new Size(268, 361));
			((Control)panel4).set_TabIndex(23);
			((Control)label41).set_AutoSize(true);
			((Control)label41).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label41).set_ForeColor(Color.DarkBlue);
			((Control)label41).set_Location(new Point(5, 22));
			((Control)label41).set_Name("label41");
			((Control)label41).set_Size(new Size(707, 13));
			((Control)label41).set_TabIndex(24);
			((Control)label41).set_Text("For lunar occultations of planets, divide the Normal motion (RV in the predictions) by 100 (for example), and multiply the resulting diameter by that figure");
			((Control)lblD).set_AutoSize(true);
			((Control)lblD).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblD).set_Location(new Point(178, 42));
			((Control)lblD).set_Name("lblD");
			((Control)lblD).set_Size(new Size(56, 15));
			((Control)lblD).set_TabIndex(25);
			((Control)lblD).set_Text("Observer");
			((Control)txtObserver).set_Location(new Point(238, 40));
			((Control)txtObserver).set_Name("txtObserver");
			((Control)txtObserver).set_Size(new Size(116, 20));
			((Control)txtObserver).set_TabIndex(26);
			((Control)chkLimbDarkening).set_AutoSize(true);
			chkLimbDarkening.set_Checked(true);
			chkLimbDarkening.set_CheckState((CheckState)1);
			((Control)chkLimbDarkening).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkLimbDarkening).set_Location(new Point(10, 35));
			((Control)chkLimbDarkening).set_Name("chkLimbDarkening");
			((Control)chkLimbDarkening).set_Size(new Size(100, 17));
			((Control)chkLimbDarkening).set_TabIndex(21);
			((Control)chkLimbDarkening).set_Text("Limb Darkening");
			((ButtonBase)chkLimbDarkening).set_UseVisualStyleBackColor(true);
			chkLimbDarkening.add_CheckedChanged((EventHandler)chkLimbDarkening_CheckedChanged);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(968, 728));
			((Control)this).get_Controls().Add((Control)(object)txtObserver);
			((Control)this).get_Controls().Add((Control)(object)lblD);
			((Control)this).get_Controls().Add((Control)(object)label41);
			((Control)this).get_Controls().Add((Control)(object)panel4);
			((Control)this).get_Controls().Add((Control)(object)grpSolveR);
			((Control)this).get_Controls().Add((Control)(object)grpSolveD);
			((Control)this).get_Controls().Add((Control)(object)groupBox4);
			((Control)this).get_Controls().Add((Control)(object)grpCalibrateR);
			((Control)this).get_Controls().Add((Control)(object)grpCalibrateD);
			((Control)this).get_Controls().Add((Control)(object)groupBox2);
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Control)this).get_Controls().Add((Control)(object)grpStar);
			((Control)this).get_Controls().Add((Control)(object)grpR);
			((Control)this).get_Controls().Add((Control)(object)grpD);
			((Control)this).get_Controls().Add((Control)(object)picR);
			((Control)this).get_Controls().Add((Control)(object)picD);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("StarDiameterAnalyser");
			((Control)this).set_Text("Star diameter analyser");
			((Form)this).add_Load((EventHandler)StarDiameterAnalyser_Load);
			((Control)grpD).ResumeLayout(false);
			((Control)grpD).PerformLayout();
			((Control)grpR).ResumeLayout(false);
			((Control)grpR).PerformLayout();
			((Control)grpStar).ResumeLayout(false);
			((Control)grpStar).PerformLayout();
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((ISupportInitialize)updnSecsToDisplay).EndInit();
			((ISupportInitialize)updnDAdjust).EndInit();
			((ISupportInitialize)updnRAdjust).EndInit();
			((ISupportInitialize)updnPlotScale).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)groupBox2).ResumeLayout(false);
			((Control)groupBox2).PerformLayout();
			((Control)groupBox3).ResumeLayout(false);
			((Control)groupBox3).PerformLayout();
			((ISupportInitialize)picR).EndInit();
			((ISupportInitialize)picD).EndInit();
			((Control)grpCalibrateD).ResumeLayout(false);
			((Control)grpCalibrateD).PerformLayout();
			((Control)grpCalibrateR).ResumeLayout(false);
			((Control)grpCalibrateR).PerformLayout();
			((Control)groupBox4).ResumeLayout(false);
			((Control)groupBox4).PerformLayout();
			((ISupportInitialize)picStar).EndInit();
			((Control)grpSolveD).ResumeLayout(false);
			((Control)grpSolveD).PerformLayout();
			((ISupportInitialize)picNormD).EndInit();
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((ISupportInitialize)picResD).EndInit();
			((ISupportInitialize)updnDiaD).EndInit();
			((ISupportInitialize)updnWeightD).EndInit();
			((Control)grpSolveR).ResumeLayout(false);
			((Control)grpSolveR).PerformLayout();
			((ISupportInitialize)picNormR).EndInit();
			((Control)panel3).ResumeLayout(false);
			((Control)panel3).PerformLayout();
			((ISupportInitialize)picResR).EndInit();
			((ISupportInitialize)updnDiaR).EndInit();
			((ISupportInitialize)updnWeightR).EndInit();
			((Control)panel4).ResumeLayout(false);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
