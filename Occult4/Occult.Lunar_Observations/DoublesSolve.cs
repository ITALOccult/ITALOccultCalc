using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Printing;
using System.IO;
using System.Reflection;
using System.Text;
using System.Windows.Forms;
using Occult.Mapping;
using Occult.Properties;
using Occult.Star_Catalogues;

namespace Occult.Lunar_Observations
{
	public class DoublesSolve : Form
	{
		private static List<DSolve> DEvents = new List<DSolve>();

		private DSolve D_Solve;

		private const double Radian = 180.0 / Math.PI;

		private const string Header = " # Year  M  D  Observer          T1      T2     T2-T1   P.A.    RV       CCT    slope  *    l      b      AA     ht  Scale °/asec Primary_AA/ht Second_AA/ht   CA Ill Alt  O-C";

		private double L;

		private double B;

		private double AA;

		private bool IsPlotting;

		private bool Updating;

		private static Pen[] PenChords = new Pen[30];

		private static SolidBrush[] BrushChords = new SolidBrush[30];

		private static float LineThickness = 1f;

		private double PASoln;

		private double SepSoln;

		private string MeanDate;

		private IContainer components;

		private TextBox txtT1;

		private TextBox txtRV;

		private TextBox txtCCT;

		private TextBox txtPA;

		private TextBox txtT2;

		private Label label2;

		private Label label3;

		private Label label4;

		private Label label5;

		private Label label6;

		private Button cmdAdd;

		private Button cmdReplace;

		private Label lblRMS;

		private Label lblPA;

		private Label lblSep;

		private Button cmdDelete;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripMenuItem withObservationsToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private Label label1;

		private TextBox txtDay;

		private Label label7;

		private TextBox txtMonth;

		private Label label8;

		private TextBox txtYear;

		private Label label9;

		private TextBox txtStarID;

		private ToolStripMenuItem copyToolStripMenuItem;

		private Label label10;

		private Label label11;

		private ToolStripMenuItem magnitudeCalculatorToolStripMenuItem;

		private Label lblBestFit;

		private ToolStripMenuItem toolStripMenuItem1;

		private ToolStripMenuItem fileToolStripMenuItem;

		private ToolStripMenuItem openToolStripMenuItem1;

		private ToolStripMenuItem saveToolStripMenuItem1;

		private ToolStripMenuItem newToolStripMenuItem1;

		private TextBox txtSlope;

		private Label label12;

		private Button cmdPaste;

		private CheckedListBox chkEvents;

		private Button cmdSwapT;

		private GroupBox groupBox1;

		private GroupBox groupBox2;

		private Label label13;

		private Button cmdShowWDS_IF;

		private PictureBox picLimbs;

		private Button cmdPlot;

		private TrackBar trackBar1;

		private Label lblScale;

		private PictureBox picLegend;

		private Label label14;

		private Label label15;

		private Label label16;

		private Label label17;

		private Label label18;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripMenuItem copyLimbPlotToolStripMenuItem;

		private ToolStripMenuItem printLimbPlotToolStripMenuItem;

		private ToolStripMenuItem saveLimbPlotToolStripMenuItem;

		private ToolStripMenuItem plotLimbInBWToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator2;

		private Label label22;

		private TextBox txtObserver;

		private Label label19;

		private TextBox txtY1;

		private Label label20;

		private TextBox txtM1;

		private Label label21;

		private TextBox txtD1;

		private Label label23;

		internal Panel PanelLegend;

		private Button cmdListEquivalents;

		private ToolStripSeparator toolStripSeparator3;

		private ToolStripMenuItem saveEntireFormAsAnImageToolStripMenuItem;

		private Label label27;

		private Label label24;

		private Label label25;

		private Label label26;

		private TextBox txtCA;

		private TextBox txtAlt;

		private TextBox txtIllum;

		private CheckBox chkMagnify;

		public VScrollBar Vbar;

		private HScrollBar Hbar;

		private ToolStripMenuItem readObservationFilesToolStripMenuItem;

		private Button cmdReadMultipleFiles;

		private Button cmdGetLimbSlope;

		private Label label28;

		private Label label29;

		private Label label30;

		private Label lbl31;

		private TextBox txtL;

		private TextBox txtAA;

		private TextBox txtB;

		private Label label31;

		private TextBox txtScale;

		private Button cmdReadSolutionFile;

		private CheckBox chkShowUncertainties;

		private CheckBox chkVisual;

		private ToolStripMenuItem copySolutionToolStripMenuItem;

		private Label lblMeanDate;

		private Panel panel1;

		private Label lblUncertainty;

		public DoublesSolve()
		{
			InitializeComponent();
			InitialiseData();
		}

		private void DoublesSolve_Load(object sender, EventArgs e)
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
			plotLimbInBWToolStripMenuItem.set_Checked(Settings.Default.DoubleSolvePlotBW);
			if (plotLimbInBWToolStripMenuItem.get_Checked())
			{
				((Control)picLimbs).set_BackColor(Color.White);
			}
		}

		private void cmdAdd_Click(object sender, EventArgs e)
		{
			EncodeLine(((ObjectCollection)chkEvents.get_Items()).get_Count(), Edit: false, Checked: true);
			if (DEvents.Count > 1)
			{
				DrawLimbPlot();
			}
		}

		private void cmdReplace_Click(object sender, EventArgs e)
		{
			if (((ListControl)chkEvents).get_SelectedIndex() >= 1)
			{
				EncodeLine(((ListControl)chkEvents).get_SelectedIndex(), Edit: true, DEvents[((ListControl)chkEvents).get_SelectedIndex() - 1].IsChecked);
				if (DEvents.Count > 1)
				{
					DrawLimbPlot();
				}
			}
		}

		private void cmdDelete_Click(object sender, EventArgs e)
		{
			//IL_0043: Unknown result type (might be due to invalid IL or missing references)
			//IL_0049: Invalid comparison between Unknown and I4
			int selectedIndex = ((ListControl)chkEvents).get_SelectedIndex();
			if (selectedIndex >= 1 && (int)MessageBox.Show("Do you want to delete\r\n\r\n" + ((ObjectCollection)chkEvents.get_Items()).get_Item(selectedIndex).ToString(), "Delete an event", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				DEvents.RemoveAt(selectedIndex - 1);
				RePopulate_chkEvents();
				if (((ObjectCollection)chkEvents.get_Items()).get_Count() > 0)
				{
					((ListControl)chkEvents).set_SelectedIndex(((ObjectCollection)chkEvents.get_Items()).get_Count() - 1);
				}
				if (DEvents.Count > 1)
				{
					DrawLimbPlot();
				}
			}
		}

		private void EncodeLine(int Line, bool Edit, bool Checked)
		{
			if (Line >= 1)
			{
				D_Solve = new DSolve();
				if (!double.TryParse(((Control)txtT1).get_Text(), out var result))
				{
					result = 0.0;
				}
				D_Solve.T1 = result;
				if (!double.TryParse(((Control)txtT2).get_Text(), out result))
				{
					result = 0.0;
				}
				D_Solve.T2 = result;
				if (!double.TryParse(((Control)txtPA).get_Text(), out result))
				{
					result = 0.0;
				}
				D_Solve.PA = result;
				if (!double.TryParse(((Control)txtCCT).get_Text(), out result))
				{
					result = 0.0;
				}
				D_Solve.CCT = result;
				if (!double.TryParse(((Control)txtRV).get_Text(), out result))
				{
					result = 0.0;
				}
				D_Solve.RV = result;
				if (!double.TryParse(((Control)txtSlope).get_Text(), out result))
				{
					result = 0.0;
				}
				D_Solve.LimbSlope = result;
				if (!int.TryParse(((Control)txtY1).get_Text(), out var result2))
				{
					result2 = 1600;
				}
				D_Solve.Year = result2;
				if (!int.TryParse(((Control)txtM1).get_Text(), out result2))
				{
					result2 = 1;
				}
				D_Solve.Month = result2;
				if (!int.TryParse(((Control)txtD1).get_Text(), out result2))
				{
					result2 = 1;
				}
				D_Solve.Day = result2;
				D_Solve.Observer = ((Control)txtObserver).get_Text().Trim();
				D_Solve.CA = ((Control)txtCA).get_Text();
				D_Solve.Illum = ((Control)txtIllum).get_Text();
				D_Solve.Alt = ((Control)txtAlt).get_Text();
				if (!double.TryParse(((Control)txtL).get_Text(), out result))
				{
					result = 0.0;
				}
				double num2 = (L = (D_Solve.l = result));
				if (!double.TryParse(((Control)txtB).get_Text(), out result))
				{
					result = 0.0;
				}
				num2 = (B = (D_Solve.b = result));
				if (!double.TryParse(((Control)txtAA).get_Text(), out result))
				{
					result = 0.0;
				}
				num2 = (AA = (D_Solve.AA = result));
				if (!double.TryParse(((Control)txtScale).get_Text(), out result))
				{
					result = 0.0;
				}
				D_Solve.MoonScale = result;
				D_Solve.IsChecked = Checked;
				if (Edit)
				{
					DEvents[Line - 1] = D_Solve;
				}
				else
				{
					DEvents.Add(D_Solve);
				}
				RePopulate_chkEvents();
				if (Edit)
				{
					((ListControl)chkEvents).set_SelectedIndex(Line);
				}
				else
				{
					((ListControl)chkEvents).set_SelectedIndex(((ObjectCollection)chkEvents.get_Items()).get_Count() - 1);
				}
			}
		}

		private void RePopulate_chkEvents()
		{
			((ObjectCollection)chkEvents.get_Items()).Clear();
			((ObjectCollection)chkEvents.get_Items()).Add((object)" # Year  M  D  Observer          T1      T2     T2-T1   P.A.    RV       CCT    slope  *    l      b      AA     ht  Scale °/asec Primary_AA/ht Second_AA/ht   CA Ill Alt  O-C");
			for (int i = 0; i < DEvents.Count; i++)
			{
				int num = ((ObjectCollection)chkEvents.get_Items()).Add((object)(string.Format("{0,2:f0} ", i) + DEvents[i].ToString()));
				chkEvents.SetItemChecked(num, DEvents[i].IsChecked);
			}
		}

		private void chkEvents_SelectedIndexChanged(object sender, EventArgs e)
		{
			DecodeLine(((ListControl)chkEvents).get_SelectedIndex());
		}

		private void DecodeLine(int Line)
		{
			if (Line >= 1)
			{
				((Control)txtT1).set_Text(DEvents[Line - 1].T1.ToString());
				((Control)txtT2).set_Text(DEvents[Line - 1].T2.ToString());
				((Control)txtPA).set_Text(DEvents[Line - 1].PA.ToString());
				((Control)txtCCT).set_Text(DEvents[Line - 1].CCT.ToString());
				((Control)txtRV).set_Text(DEvents[Line - 1].RV.ToString());
				((Control)txtSlope).set_Text(DEvents[Line - 1].LimbSlope.ToString());
				((Control)txtY1).set_Text(DEvents[Line - 1].Year.ToString());
				((Control)txtM1).set_Text(DEvents[Line - 1].Month.ToString());
				((Control)txtD1).set_Text(DEvents[Line - 1].Day.ToString());
				((Control)txtObserver).set_Text(DEvents[Line - 1].Observer.ToString());
				((Control)txtCA).set_Text(DEvents[Line - 1].CA.Trim());
				((Control)txtIllum).set_Text(DEvents[Line - 1].Illum.Trim());
				((Control)txtAlt).set_Text(DEvents[Line - 1].Alt.Trim());
				((Control)txtL).set_Text(DEvents[Line - 1].l.ToString());
				((Control)txtB).set_Text(DEvents[Line - 1].b.ToString());
				((Control)txtAA).set_Text(DEvents[Line - 1].AA.ToString());
				((Control)txtScale).set_Text(string.Format("{0,5:f3}", DEvents[Line - 1].MoonScale));
				L = DEvents[Line - 1].l;
				B = DEvents[Line - 1].b;
				AA = DEvents[Line - 1].AA;
			}
		}

		private static double DoubleResidual(double Rstar, double PAstar, double PAevent, double PAmoonmotion, double RVxDT, double CCT, double Slope)
		{
			double num = RVxDT;
			if (Math.Abs(CCT) > 90.0)
			{
				num = 0.0 - num;
			}
			double num2 = Rstar * Math.Cos((PAstar - PAevent - Slope) / (180.0 / Math.PI));
			return num - num2;
		}

		private void cmdSolve_Click(object sender, EventArgs e)
		{
			SolvePairing();
		}

		private void SolvePairing()
		{
			//IL_00d5: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ee: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f4: Invalid comparison between Unknown and I4
			//IL_0307: Unknown result type (might be due to invalid IL or missing references)
			//IL_030d: Invalid comparison between Unknown and I4
			//IL_06a8: Unknown result type (might be due to invalid IL or missing references)
			//IL_06ae: Invalid comparison between Unknown and I4
			//IL_118f: Unknown result type (might be due to invalid IL or missing references)
			//IL_1195: Invalid comparison between Unknown and I4
			double[,] array = new double[3, 3];
			double[,] array2 = new double[3, 3];
			double[,] array3 = new double[50, 3];
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			double num4 = 0.0;
			double num5 = 0.0;
			double num6 = 0.0;
			double num7 = 0.0;
			double num8 = 0.0;
			int num9 = 0;
			double[] array4 = new double[2];
			double[] array5 = new double[2];
			_ = new double[2];
			double[] array6 = new double[2];
			double[] array7 = new double[2];
			if (chkEvents.GetItemChecked(0))
			{
				chkEvents.SetItemChecked(0, false);
			}
			if (chkEvents.get_CheckedItems().get_Count() < 2)
			{
				MessageBox.Show("Cannot solve. At least 2 observations are required.", "Cannot Solve", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return;
			}
			double SlopeBefore_Deg;
			double SlopeAfter_Deg;
			double P_Limb_deg;
			double D_Limb_deg;
			for (int i = 0; i < DEvents.Count; i++)
			{
				if ((int)chkEvents.GetItemCheckState(i + 1) == 1)
				{
					DEvents[i].Limb_AtAA = LOLAHiRes.LimbHeight_Slope(DEvents[i].AA, DEvents[i].l, DEvents[i].b, DEvents[i].MoonScale, IncludeSlope: false, WideSlope: false, out SlopeBefore_Deg, out SlopeAfter_Deg, out P_Limb_deg, out D_Limb_deg);
					DEvents[i].AA_DegreesPerArcsec = 0.061437924374404686 / DEvents[i].MoonScale;
					DEvents[i].AA_AtEvent = DEvents[i].AA - DEvents[i].RV * (DEvents[i].T2 - DEvents[i].T1) * DEvents[i].AA_DegreesPerArcsec;
					DEvents[i].Limb_AtEvent = LOLAHiRes.LimbHeight_Slope(DEvents[i].AA_AtEvent, DEvents[i].l, DEvents[i].b, DEvents[i].MoonScale, IncludeSlope: false, WideSlope: false, out SlopeBefore_Deg, out SlopeAfter_Deg, out P_Limb_deg, out D_Limb_deg);
					DEvents[i].LimbSlope = 0.0;
					num8 += Utilities.JD_from_Date(DEvents[i].Year, DEvents[i].Month, DEvents[i].Day);
					num9++;
				}
			}
			if (num9 > 0)
			{
				num8 /= (double)num9;
				MeanDate = string.Format("{0,7:f2}", Utilities.BesselianYear(num8));
			}
			for (int i = 0; i < DEvents.Count; i++)
			{
				if ((int)chkEvents.GetItemCheckState(i + 1) == 1)
				{
					num = DEvents[i].RV * (DEvents[i].T2 - DEvents[i].T1) / Math.Cos(DEvents[i].CCT / (180.0 / Math.PI)) + (DEvents[i].Limb_AtAA - DEvents[i].Limb_AtEvent);
					if (num < 0.0)
					{
						num = 0.0 - num;
					}
					break;
				}
			}
			double num10 = 180.0;
			double num11 = 0.1;
			double num12 = 1.0;
			num2 = -0.1;
			num3 = 0.0;
			for (int j = 0; j < 30; j++)
			{
				if (j > 0)
				{
					for (int i = 0; i < DEvents.Count; i++)
					{
						double num13 = num * Math.Sin((num10 + DEvents[i].AA - DEvents[i].PA) / (180.0 / Math.PI));
						DEvents[i].AA_At2nd = DEvents[i].AA + DEvents[i].AA_DegreesPerArcsec * num13;
						DEvents[i].Limb_At2nd = LOLAHiRes.LimbHeight_Slope(DEvents[i].AA_At2nd, DEvents[i].l, DEvents[i].b, DEvents[i].MoonScale, IncludeSlope: true, WideSlope: false, out SlopeBefore_Deg, out SlopeAfter_Deg, out P_Limb_deg, out D_Limb_deg);
						if (Math.Abs(num13) < 0.5)
						{
							if (Math.Abs(DEvents[i].AA_At2nd - DEvents[i].AA_AtEvent) < 0.04)
							{
								DEvents[i].LimbSlope = (SlopeBefore_Deg + SlopeAfter_Deg) / 2.0;
							}
							else if (DEvents[i].AA_At2nd > DEvents[i].AA)
							{
								DEvents[i].LimbSlope = SlopeAfter_Deg;
							}
							else
							{
								DEvents[i].LimbSlope = SlopeBefore_Deg;
							}
						}
						else
						{
							double num14 = (1.0 - Math.Cos(DEvents[i].AA_DegreesPerArcsec * num13 / (180.0 / Math.PI))) * 932.58 * DEvents[i].MoonScale;
							DEvents[i].LimbSlope = 180.0 / Math.PI * Math.Atan((DEvents[i].Limb_At2nd - num14 - DEvents[i].Limb_AtEvent) / num13);
							if (DEvents[i].AA_At2nd < DEvents[i].AA)
							{
								DEvents[i].LimbSlope = 0.0 - DEvents[i].LimbSlope;
							}
						}
					}
				}
				for (int k = 0; k < 100; k++)
				{
					int num15 = 0;
					for (int i = 0; i < DEvents.Count; i++)
					{
						if ((int)chkEvents.GetItemCheckState(i + 1) == 1)
						{
							double num16 = num;
							double num17 = num10;
							double pA = DEvents[i].PA;
							double num18 = DEvents[i].PA + DEvents[i].CCT;
							double limbSlope = DEvents[i].LimbSlope;
							double num19 = DEvents[i].RV * (DEvents[i].T2 - DEvents[i].T1) / Math.Abs(Math.Cos((pA - num18) / (180.0 / Math.PI)) / Math.Cos((pA + limbSlope - num18) / (180.0 / Math.PI)));
							array3[num15, 2] = DoubleResidual(num16, num17, pA, num18, num19, DEvents[i].CCT, limbSlope);
							array3[num15, 0] = DoubleResidual(num16 + num11, num17, pA, num18, num19, DEvents[i].CCT, limbSlope) - array3[num15, 2];
							array3[num15, 1] = DoubleResidual(num16, num17 + num12, pA, num18, num19, DEvents[i].CCT, limbSlope) - array3[num15, 2];
							if (num15 < 2)
							{
								array6[num15] = 1.0 / Math.Tan((pA + limbSlope + 90.0) / (180.0 / Math.PI));
								array4[num15] = num19 * Math.Sin(pA / (180.0 / Math.PI)) * (double)Math.Sign(Math.Cos(DEvents[i].CCT / (180.0 / Math.PI)));
								array5[num15] = num19 * Math.Cos(pA / (180.0 / Math.PI)) * (double)Math.Sign(Math.Cos(DEvents[i].CCT / (180.0 / Math.PI)));
								array7[num15] = array5[num15] - array4[num15] * array6[num15];
							}
							num15++;
						}
					}
					if (num15 < 2)
					{
						return;
					}
					if (num15 == 2)
					{
						double num20 = (array7[1] - array7[0]) / (array6[0] - array6[1]);
						double num21 = array6[0] * num20 + array7[0];
						num = (SepSoln = Math.Sqrt(num20 * num20 + num21 * num21));
						num10 = Math.Atan2(num20, num21) * (180.0 / Math.PI);
						if (num10 < 0.0)
						{
							num10 += 360.0;
						}
						PASoln = num10;
						num2 = SepSoln;
						num3 = PASoln;
						((Control)lblPA).set_Text($"PA = {num10:F2}°");
						((Control)lblSep).set_Text($"Sep = {num:F3}\"");
						break;
					}
					num6 = 0.0;
					for (int i = 0; i < num15; i++)
					{
						num6 += array3[i, 2];
					}
					num6 /= (double)num15;
					double num22 = 0.0;
					for (int i = 0; i < num15; i++)
					{
						num22 += Math.Pow(array3[i, 2] - num6, 2.0);
					}
					num7 = Math.Sqrt(num22 / (double)num15);
					for (int l = 0; l < 2; l++)
					{
						for (int m = l; m <= 2; m++)
						{
							array[l, m] = 0.0;
						}
					}
					for (int i = 0; i < num15; i++)
					{
						for (int l = 0; l < 2; l++)
						{
							for (int m = l; m <= 2; m++)
							{
								array[l, m] += array3[i, l] * array3[i, m];
							}
						}
					}
					for (int l = 1; l < 2; l++)
					{
						for (int m = 0; m < l; m++)
						{
							array[l, m] = array[m, l];
						}
					}
					for (int l = 0; l <= 1; l++)
					{
						for (int m = 0; m <= 1; m++)
						{
							if (l == m)
							{
								array2[l, m] = 1.0;
							}
							else
							{
								array2[l, m] = 0.0;
							}
						}
					}
					for (int n = 0; n <= 1; n++)
					{
						if (array[n, n] == 0.0)
						{
							continue;
						}
						double num23 = array[n, n];
						for (int m = 0; m <= 1; m++)
						{
							array[n, m] /= num23;
							array2[n, m] /= num23;
						}
						for (int l = 0; l <= 1; l++)
						{
							num23 = array[l, n];
							if (l != n)
							{
								for (int m = 0; m <= 1; m++)
								{
									array[l, m] -= array[n, m] * num23;
									array2[l, m] -= array2[n, m] * num23;
								}
							}
						}
					}
					num4 = array2[0, 0] * array[0, 2] + array2[0, 1] * array[1, 2];
					num5 = array2[1, 0] * array[0, 2] + array2[1, 1] * array[1, 2];
					num4 *= num11;
					num5 *= num12;
					if (num5 < -180.0)
					{
						num5 += 360.0;
					}
					if (num5 > 360.0)
					{
						num5 -= 360.0;
					}
					if (j > 0)
					{
						if (num5 > 30.0)
						{
							num5 = 30.0;
						}
						if (num5 < -30.0)
						{
							num5 = -30.0;
						}
						if (Math.Abs(num5 * SepSoln) > 0.1)
						{
							num5 = 0.1 / SepSoln * (double)Math.Sign(num5);
						}
					}
					num10 -= num5;
					if (num10 < 0.0)
					{
						num10 += 360.0;
					}
					if (num10 > 360.0)
					{
						num10 -= 360.0;
					}
					if (chkEvents.get_CheckedItems().get_Count() == 2)
					{
						((Control)lblPA).set_Text($"PA = {num10:F2}°");
					}
					else
					{
						((Control)lblPA).set_Text($"PA = {num10:F2}° ±{num7 * Math.Sqrt(Math.Abs(array2[1, 1])) * num12:F2}°");
					}
					PASoln = num10;
					double num24 = 1.0;
					if (j > 0)
					{
						if (num < 1.0)
						{
							num24 = 0.4;
						}
						if (num < 0.5)
						{
							num24 = 0.2;
						}
						if (num < 0.3)
						{
							num24 = 0.1;
						}
					}
					if (num4 > num24)
					{
						num4 = num24;
					}
					if (num4 < 0.0 - num24)
					{
						num4 = 0.0 - num24;
					}
					num -= num4;
					if (num < 0.0)
					{
						num = 0.1;
						num10 += 180.0;
						if (num10 > 360.0)
						{
							num10 -= 360.0;
						}
					}
					if (chkEvents.get_CheckedItems().get_Count() == 2)
					{
						((Control)lblSep).set_Text($"Sep = {num:F3}\"");
					}
					else
					{
						((Control)lblSep).set_Text($"Sep = {num:F3}\" ±{num7 * Math.Sqrt(Math.Abs(array2[0, 0])) * num11:F3}\"");
					}
					SepSoln = num;
					if (((Math.Abs(num4) < 0.001) & (Math.Abs(num5) < 0.01)) || (j > 0 && ((Math.Abs(SepSoln - num2) < 0.001) & (Math.Abs(PASoln - num3) < 0.01))))
					{
						break;
					}
					num2 = SepSoln;
					num3 = PASoln;
				}
			}
			if ((Math.Abs(num4) < 0.002) & (Math.Abs(num5) < 0.02))
			{
				((Control)lblRMS).set_Text($"Residuals: RMS = ±{num7:F3}\"");
			}
			else
			{
				((Control)lblRMS).set_Text("Residuals: Invalid solution");
			}
			((Control)lblMeanDate).set_Text("Mean Date: " + MeanDate);
			Label obj = lblBestFit;
			bool visible;
			((Control)lblRMS).set_Visible(visible = chkEvents.get_CheckedItems().get_Count() == 2);
			((Control)obj).set_Visible(visible);
			((Control)lblRMS).set_Visible(!((Control)lblBestFit).get_Visible());
			for (int i = 0; i < DEvents.Count; i++)
			{
				double num16 = num;
				double num17 = num10;
				double pA = DEvents[i].PA;
				double num18 = DEvents[i].PA + DEvents[i].CCT;
				double limbSlope = DEvents[i].LimbSlope;
				double num19 = DEvents[i].RV * (DEvents[i].T2 - DEvents[i].T1) / Math.Abs(Math.Cos((pA - num18) / (180.0 / Math.PI)) / Math.Cos((pA + limbSlope - num18) / (180.0 / Math.PI)));
				DEvents[i].O_C = DoubleResidual(num16, num17, pA, num18, num19, DEvents[i].CCT, limbSlope);
			}
			double num25;
			double num26 = (num25 = -100.0);
			double num29;
			double num28;
			double num27;
			double num30 = (num29 = (num28 = (num27 = 0.0)));
			double num31 = 0.0;
			for (int i = 0; i < DEvents.Count; i++)
			{
				if ((int)chkEvents.GetItemCheckState(i + 1) != 1)
				{
					continue;
				}
				if (num26 == -100.0)
				{
					num26 = DEvents[i].PA;
					num25 = DEvents[i].PA + DEvents[i].LimbSlope;
					continue;
				}
				for (num31 = DEvents[i].PA - num26; num31 > 180.0; num31 -= 180.0)
				{
				}
				for (; num31 < -180.0; num31 += 180.0)
				{
				}
				if (num31 > 90.0)
				{
					num31 = 180.0 - num31;
				}
				if (num31 < -90.0)
				{
					num31 = -180.0 + num31;
				}
				if (num31 > num30)
				{
					num30 = num31;
				}
				if (num31 < num28)
				{
					num28 = num31;
				}
				for (num31 = DEvents[i].PA + DEvents[i].LimbSlope - num25; num31 > 180.0; num31 -= 180.0)
				{
				}
				for (; num31 < -180.0; num31 += 180.0)
				{
				}
				if (num31 > 90.0)
				{
					num31 = 180.0 - num31;
				}
				if (num31 < -90.0)
				{
					num31 = -180.0 + num31;
				}
				if (num31 > num29)
				{
					num29 = num31;
				}
				if (num31 < num27)
				{
					num27 = num31;
				}
			}
			double num32 = ((num30 != num28) ? Math.Abs(num30 - num28) : Math.Abs(num30));
			double num33 = ((num29 != num27) ? Math.Abs(num29 - num27) : Math.Abs(num29));
			((Control)lblUncertainty).set_Visible(num32 < 5.0 || num33 < 5.0);
			if (num33 < 5.0)
			{
				((Control)lblUncertainty).set_Text(" Unreliable solution; the difference in limb slopes is too small. ");
			}
			else if (num32 < 5.0)
			{
				((Control)lblUncertainty).set_Text(" Doubtful solution; the difference in event PA's is too small. ");
			}
			RePopulate_chkEvents();
		}

		private void txtT1_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtT1).SelectAll();
		}

		private void txtT2_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtT2).SelectAll();
		}

		private void txtPA_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtPA).SelectAll();
		}

		private void txtCCT_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtCCT).SelectAll();
		}

		private void txtRV_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtRV).SelectAll();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void helpToolStripMenuItem_Click_1(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Solve doubles");
		}

		private void newToolStripMenuItem1_Click_1(object sender, EventArgs e)
		{
			InitialiseData();
		}

		private void InitialiseData()
		{
			DEvents.Clear();
			((ObjectCollection)chkEvents.get_Items()).Clear();
			((ObjectCollection)chkEvents.get_Items()).Add((object)" # Year  M  D  Observer          T1      T2     T2-T1   P.A.    RV       CCT    slope  *    l      b      AA     ht  Scale °/asec Primary_AA/ht Second_AA/ht   CA Ill Alt  O-C");
			((Control)lblBestFit).set_Visible(false);
			((Control)lblPA).set_Text("PA =");
			((Control)lblSep).set_Text("Sep =");
			((Control)lblRMS).set_Text("RMS fit:");
			TextBox obj = txtStarID;
			TextBox obj2 = txtYear;
			TextBox obj3 = txtMonth;
			TextBox obj4 = txtDay;
			TextBox obj5 = txtT1;
			TextBox obj6 = txtT2;
			TextBox obj7 = txtPA;
			TextBox obj8 = txtCCT;
			string text;
			((Control)txtRV).set_Text(text = "");
			string text2;
			((Control)obj8).set_Text(text2 = text);
			string text3;
			((Control)obj7).set_Text(text3 = text2);
			string text4;
			((Control)obj6).set_Text(text4 = text3);
			string text5;
			((Control)obj5).set_Text(text5 = text4);
			string text6;
			((Control)obj4).set_Text(text6 = text5);
			string text7;
			((Control)obj3).set_Text(text7 = text6);
			string text8;
			((Control)obj2).set_Text(text8 = text7);
			((Control)obj).set_Text(text8);
		}

		private void saveToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			//IL_0026: Unknown result type (might be due to invalid IL or missing references)
			//IL_002c: Expected O, but got Unknown
			//IL_0070: Unknown result type (might be due to invalid IL or missing references)
			//IL_0076: Invalid comparison between Unknown and I4
			string text = CollectEvents();
			string text2 = text[..text.IndexOf("\r")] + "_sln.txt";
			SaveFileDialog val = new SaveFileDialog();
			((FileDialog)val).set_CheckPathExists(true);
			((FileDialog)val).set_FileName(text2);
			((FileDialog)val).set_Title(text2);
			val.set_OverwritePrompt(true);
			((FileDialog)val).set_InitialDirectory(Utilities.AppPath + "\\Observations\\Doubles");
			((FileDialog)val).set_Filter("Solution files (*_sln.txt )|*_sln.txt|Text files (*.txt )|*.txt|All files (*.*)|*.*");
			((FileDialog)val).set_FilterIndex(0);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				StreamWriter streamWriter = new StreamWriter(((FileDialog)val).get_FileName(), append: false, Encoding.Default);
				streamWriter.WriteLine(text);
				streamWriter.Close();
			}
		}

		private string CollectEvents()
		{
			string text = ((Control)txtStarID).get_Text().Replace(" ", "") + " " + ((Control)txtYear).get_Text().Trim().PadLeft(4)
				.Substring(0, 4) + ((Control)txtMonth).get_Text().Trim().PadLeft(3)
				.Substring(0, 3) + ((Control)txtDay).get_Text().Trim().PadLeft(3)
				.Substring(0, 3);
			for (int i = 0; i < ((ObjectCollection)chkEvents.get_Items()).get_Count(); i++)
			{
				text = ((i != 0) ? (text + "\r\n" + DEvents[i - 1].ToString()) : (text + "\r\n" + ((ObjectCollection)chkEvents.get_Items()).get_Item(i).ToString()!.Substring(3)));
			}
			return text + "\r\n\r\n" + ((Control)lblSep).get_Text() + "  " + ((Control)lblPA).get_Text() + "  " + ((Control)lblRMS).get_Text();
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(CollectEvents());
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents());
		}

		private void openToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			ReadSolutionFile();
		}

		private void ReadSolutionFile()
		{
			//IL_0002: Unknown result type (might be due to invalid IL or missing references)
			//IL_0008: Expected O, but got Unknown
			//IL_003b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0041: Invalid comparison between Unknown and I4
			bool flag = true;
			OpenFileDialog val = new OpenFileDialog();
			((FileDialog)val).set_Title("Select Double star observation file");
			((FileDialog)val).set_Filter("Solution files (*_sln.txt)|*_sln.txt|Text files (*.txt )|*.txt|All files (*.*)|*.*");
			((FileDialog)val).set_FilterIndex(0);
			((FileDialog)val).set_InitialDirectory(Utilities.AppPath + "\\Observations\\Doubles");
			if ((int)((CommonDialog)val).ShowDialog() != 1)
			{
				return;
			}
			using (StreamReader streamReader = new StreamReader(((FileDialog)val).get_FileName()))
			{
				InitialiseData();
				string text = streamReader.ReadLine();
				int num = text.IndexOf(" ");
				((Control)txtStarID).set_Text(text.Substring(0, num));
				((Control)txtYear).set_Text(text.Substring(num + 1, 4));
				((Control)txtMonth).set_Text(text.Substring(num + 6, 2));
				((Control)txtDay).set_Text(text.Substring(num + 9, 2));
				text = streamReader.ReadLine();
				if (text.Contains("AA/ht"))
				{
					do
					{
						text = streamReader.ReadLine()!.PadRight(122);
						if (text.Length == 160)
						{
							text = text.Insert(107, "      ");
						}
						if (text.Trim().Length < 30)
						{
							break;
						}
						text = text.PadRight(165);
						((Control)txtY1).set_Text(text.Substring(0, 4));
						((Control)txtM1).set_Text(text.Substring(5, 2));
						((Control)txtD1).set_Text(text.Substring(8, 2));
						((Control)txtObserver).set_Text(text.Substring(12, 16));
						((Control)txtT1).set_Text(text.Substring(28, 8));
						((Control)txtT2).set_Text(text.Substring(36, 8));
						((Control)txtPA).set_Text(text.Substring(52, 8));
						((Control)txtRV).set_Text(text.Substring(60, 6));
						((Control)txtCCT).set_Text(text.Substring(66, 9));
						((Control)txtSlope).set_Text(text.Substring(75, 8));
						((Control)txtL).set_Text(text.Substring(85, 7));
						((Control)txtB).set_Text(text.Substring(92, 7));
						((Control)txtAA).set_Text(text.Substring(99, 8));
						((Control)txtScale).set_Text(text.Substring(113, 6));
						((Control)txtCA).set_Text(text.Substring(153, 5));
						((Control)txtIllum).set_Text(text.Substring(158, 4));
						((Control)txtAlt).set_Text(text.Substring(162, 4));
						flag = Verify_L_B_AA_Scale(text.Substring(0, 10));
						EncodeLine(((ObjectCollection)chkEvents.get_Items()).get_Count(), Edit: false, text.Substring(84, 1) == "1" && flag);
					}
					while (!streamReader.EndOfStream);
				}
				else
				{
					do
					{
						text = streamReader.ReadLine()!.PadRight(128);
						if (text.Trim().Length < 30)
						{
							break;
						}
						text = text.PadRight(86);
						((Control)txtT1).set_Text(text.Substring(0, 8));
						((Control)txtT2).set_Text(text.Substring(8, 8));
						((Control)txtPA).set_Text(text.Substring(24, 8));
						((Control)txtRV).set_Text(text.Substring(32, 6));
						((Control)txtCCT).set_Text(text.Substring(38, 9));
						((Control)txtSlope).set_Text(text.Substring(47, 8));
						((Control)txtY1).set_Text(text.Substring(59, 4));
						((Control)txtM1).set_Text(text.Substring(64, 2));
						((Control)txtD1).set_Text(text.Substring(67, 2));
						((Control)txtObserver).set_Text(text.Substring(70, 16));
						((Control)txtCA).set_Text(text.Substring(87, 5));
						((Control)txtIllum).set_Text(text.Substring(92, 4));
						((Control)txtAlt).set_Text(text.Substring(96, 4));
						((Control)txtL).set_Text(text.Substring(100, 7));
						((Control)txtB).set_Text(text.Substring(107, 7));
						((Control)txtAA).set_Text(text.Substring(114, 8));
						((Control)txtScale).set_Text(text.Substring(122, 6));
						flag = Verify_L_B_AA_Scale(text.Substring(59, 10));
						EncodeLine(((ObjectCollection)chkEvents.get_Items()).get_Count(), Edit: false, text.Substring(56, 1) == "1" && flag);
					}
					while (!streamReader.EndOfStream);
				}
				if (!streamReader.EndOfStream)
				{
					text = streamReader.ReadLine()!.PadRight(128);
					if (!double.TryParse(text.Substring(5, 5), out var result))
					{
						result = 0.0;
					}
					chkMagnify.set_Checked(result > 1.0);
				}
			}
			if (DEvents.Count > 1)
			{
				((ScrollBar)Hbar).set_Value(0);
				((ScrollBar)Vbar).set_Value(0);
				DrawLimbPlot();
			}
		}

		private bool Verify_L_B_AA_Scale(string DateString)
		{
			bool result = true;
			string Scale = "";
			string L = "";
			string B = "";
			string AA = "";
			bool num = (((Control)txtL).get_Text().Trim() == "") | (((Control)txtL).get_Text().Trim() == "0") | (((Control)txtL).get_Text().Trim() == "0.00") | (((Control)txtB).get_Text().Trim() == "") | (((Control)txtB).get_Text().Trim() == "0") | (((Control)txtB).get_Text().Trim() == "0.00") | (((Control)txtAA).get_Text().Trim() == "") | (((Control)txtAA).get_Text().Trim() == "0") | (((Control)txtAA).get_Text().Trim() == "0.00") | (((Control)txtScale).get_Text().Trim() == "") | (((Control)txtScale).get_Text().Trim() == "1") | (((Control)txtScale).get_Text().Trim() == "1.000");
			if (((Control)txtL).get_Text().Trim() == "")
			{
				((Control)txtL).set_Text("0");
			}
			if (((Control)txtB).get_Text().Trim() == "")
			{
				((Control)txtB).set_Text("0");
			}
			if (((Control)txtAA).get_Text().Trim() == "")
			{
				((Control)txtAA).set_Text("0");
			}
			if ((((Control)txtScale).get_Text().Trim() == "") | (((Control)txtScale).get_Text().Trim() == "1.000"))
			{
				((Control)txtScale).set_Text("1");
			}
			if (num)
			{
				if (FindEventInArchive(DateString, ((Control)txtObserver).get_Text().Trim(), ((Control)txtStarID).get_Text(), out Scale, out L, out B, out AA))
				{
					if ((((Control)txtL).get_Text().Trim() == "") | (((Control)txtL).get_Text().Trim() == "0") | (((Control)txtL).get_Text().Trim() == "0.00"))
					{
						((Control)txtL).set_Text(L);
					}
					if ((((Control)txtB).get_Text().Trim() == "") | (((Control)txtB).get_Text().Trim() == "0") | (((Control)txtB).get_Text().Trim() == "0.00"))
					{
						((Control)txtB).set_Text(B);
					}
					if ((((Control)txtAA).get_Text().Trim() == "") | (((Control)txtAA).get_Text().Trim() == "0") | (((Control)txtAA).get_Text().Trim() == "0.00"))
					{
						((Control)txtAA).set_Text(AA);
					}
					if ((((Control)txtScale).get_Text().Trim() == "") | (((Control)txtScale).get_Text().Trim() == "1") | (((Control)txtScale).get_Text().Trim() == "1.000"))
					{
						((Control)txtScale).set_Text(Scale);
					}
				}
				else
				{
					result = false;
				}
			}
			return result;
		}

		private bool FindEventInArchive(string date, string Obs, string CatNum, out string Scale, out string L, out string B, out string AA)
		{
			int[] array = new int[11]
			{
				1600, 1950, 1970, 1976, 1981, 1986, 1990, 1995, 2000, 2006,
				2016
			};
			if (!int.TryParse(date.Substring(0, 4), out var result))
			{
				result = 0;
			}
			string[] array2 = new string[2];
			bool flag = false;
			Scale = "1.000";
			L = " 0.00";
			B = " 0.00";
			AA = "  0.00";
			array2[0] = "";
			for (int i = 0; i < 10; i++)
			{
				if ((result >= array[i]) & (result < array[i + 1]))
				{
					array2[0] = "RArchive Observations " + array[i] + "_" + (array[i + 1] - 1) + ".dat";
					break;
				}
			}
			array2[1] = "RArchive Observations recent.dat";
			CatNum = CatNum.Substring(0, 1) + CatNum.Substring(1).Trim().PadLeft(6);
			Obs = Obs.Trim().ToLower();
			int num = Obs.LastIndexOf(" ");
			if (num > 0)
			{
				Obs = Obs.Substring(num);
			}
			num = Obs.LastIndexOf(".");
			if ((num > 0) & (num < Obs.Length - 1))
			{
				Obs = Obs.Substring(num + 1);
			}
			for (int j = 0; j < 2; j++)
			{
				if (array2[0].Length < 10)
				{
					continue;
				}
				using StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Resource Files\\" + array2[j]);
				for (int k = 0; k < 8; k++)
				{
					string text = streamReader.ReadLine();
				}
				do
				{
					string text = streamReader.ReadLine();
					if (text.Substring(36, 10) == date && text.Substring(27, 7) == CatNum && text.Substring(6, 20).ToLower().Contains(Obs))
					{
						Scale = text.Substring(111, 5);
						L = text.Substring(86, 5);
						B = text.Substring(92, 5);
						AA = text.Substring(98, 6);
						flag = true;
						break;
					}
				}
				while (!streamReader.EndOfStream);
				if (flag)
				{
					return true;
				}
			}
			return false;
		}

		private void magnitudeCalculatorToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarObservations.Show_MagnitudeCalculator();
		}

		private void cmdPaste_Click(object sender, EventArgs e)
		{
			string text = Clipboard.GetText();
			PlaceTextInReport(text, MultipleLines: false);
		}

		private void PlaceTextInReport(string InLine, bool MultipleLines)
		{
			//IL_007c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0161: Unknown result type (might be due to invalid IL or missing references)
			//IL_0211: Unknown result type (might be due to invalid IL or missing references)
			//IL_0217: Invalid comparison between Unknown and I4
			string[] array = InLine.Split('\n', '\r');
			string text = "";
			string text2 = "";
			double result = 0.0;
			double result2 = 0.0;
			int upperBound = array.GetUpperBound(0);
			bool flag = true;
			bool flag2 = false;
			if (((ObjectCollection)chkEvents.get_Items()).get_Count() > 1)
			{
				flag = false;
			}
			if ((upperBound < 6) | (InLine.Length < 100))
			{
				MessageBox.Show("Pasted data appears to be invalid. Please Copy and Paste again", "Invalid paste", (MessageBoxButtons)0, (MessageBoxIcon)48);
			}
			else if (array[0].Contains(" at ") & (array[0].Length > 12))
			{
				((Control)txtY1).set_Text(array[0].Substring(0, 4));
				string text3 = array[0].Substring(5, 3);
				for (int i = 1; i < 13; i++)
				{
					if (text3 == Utilities.ShortMonths[i])
					{
						((Control)txtM1).set_Text(i.ToString());
						break;
					}
				}
				((Control)txtD1).set_Text(array[0].Substring(9, 2));
				if (flag)
				{
					((Control)txtYear).set_Text(((Control)txtY1).get_Text());
					((Control)txtMonth).set_Text(((Control)txtM1).get_Text());
					((Control)txtDay).set_Text(((Control)txtD1).get_Text());
				}
				flag2 = false;
				for (int j = 0; j <= upperBound; j++)
				{
					if (array[j].Contains("Star ="))
					{
						text = GetValue(array[j]);
						if (flag)
						{
							((Control)txtStarID).set_Text(text);
						}
						else if (((Control)txtStarID).get_Text().Trim() != text.Trim() && (int)MessageBox.Show("Star numbers (" + ((Control)txtStarID).get_Text().Trim() + ", " + text.Trim() + ") do not match. Do you want to continue?", "Different star number", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
						{
							return;
						}
					}
					if (array[j].Contains(" PA ="))
					{
						((Control)txtPA).set_Text(GetValue(array[j]));
					}
					else if (array[j].Contains(" RV ="))
					{
						((Control)txtRV).set_Text(GetValue(array[j]));
					}
					else if (array[j].Contains(" CCT ="))
					{
						((Control)txtCCT).set_Text(GetValue(array[j]));
					}
					else if (array[j].Contains(" T1 ="))
					{
						((Control)txtT1).set_Text(GetValue(array[j]));
					}
					else if (array[j].Contains(" T2 ="))
					{
						((Control)txtT2).set_Text(GetValue(array[j]));
					}
					else if (array[j].Contains("T2-T1 ="))
					{
						text2 = GetValue(array[j]);
					}
					else if (array[j].Contains("*** No step "))
					{
						flag2 = true;
					}
					else if (array[j].Contains(" l ="))
					{
						((Control)txtL).set_Text(GetValue(array[j]).Trim());
					}
					else if (array[j].Contains(" b ="))
					{
						((Control)txtB).set_Text(GetValue(array[j]).Trim());
					}
					else if (array[j].Contains(" AA ="))
					{
						((Control)txtAA).set_Text(GetValue(array[j]).Trim());
					}
					else if (array[j].Contains(" Scale ="))
					{
						((Control)txtScale).set_Text(GetValue(array[j]).Trim());
					}
					else if (array[j].Contains("Observer ="))
					{
						((Control)txtObserver).set_Text(GetValue(array[j]).Trim());
					}
					else if (array[j].Contains("CA ="))
					{
						((Control)txtCA).set_Text(GetValue(array[j]).Trim());
					}
					else if (array[j].Contains("%Illum ="))
					{
						((Control)txtIllum).set_Text(GetValue(array[j]).Trim());
					}
					else if (array[j].Contains("Alt ="))
					{
						((Control)txtAlt).set_Text(GetValue(array[j]).Trim());
					}
				}
				if (text2.Length > 0)
				{
					if (!double.TryParse(((Control)txtT1).get_Text(), out result))
					{
						result = 0.0;
					}
					if (!double.TryParse(text2, out result2))
					{
						result2 = 0.0;
					}
					((Control)txtT2).set_Text(string.Format("{0,1:f3}", result + result2));
				}
				if (flag2)
				{
					((Control)txtT2).set_Text(((Control)txtT1).get_Text());
				}
				((Control)txtSlope).set_Text("");
				if (!MultipleLines & !Utilities.LOLAFileExists)
				{
					GetLimbSlope();
				}
			}
			else
			{
				MessageBox.Show("Text to be pasted must start with the line that contains the date of \r\nthe observation. This is usually the first line.", "Invalid data");
			}
		}

		private string GetValue(string S)
		{
			int num = S.IndexOf("=") + 1;
			if (S.Length > num)
			{
				return S.Substring(num);
			}
			return "";
		}

		private void cmdSwapT_Click(object sender, EventArgs e)
		{
			string text = ((Control)txtT1).get_Text();
			((Control)txtT1).set_Text(((Control)txtT2).get_Text());
			((Control)txtT2).set_Text(text);
		}

		private void cmdShowWDS_IF_Click(object sender, EventArgs e)
		{
			//IL_0024: Unknown result type (might be due to invalid IL or missing references)
			//IL_004b: Unknown result type (might be due to invalid IL or missing references)
			//IL_006c: Unknown result type (might be due to invalid IL or missing references)
			string text = ((Control)txtStarID).get_Text().Trim();
			if (text.Length < 2)
			{
				MessageBox.Show("No valid star number has been specified", "No Star number");
				return;
			}
			string text2 = text.Substring(0, 1);
			if (!"RSX".Contains(text2))
			{
				MessageBox.Show("The star number must start with either R, S or X", "Invalid star number");
				return;
			}
			if (!int.TryParse(text.Substring(1), out var result))
			{
				MessageBox.Show("No valid number has been specified", "Invalid star number");
				return;
			}
			switch (text2)
			{
			case "R":
				XZ80Q.Get_ZC_Star(result);
				break;
			case "S":
				XZ80Q.Get_SAO_Star(result);
				break;
			case "X":
				XZ80Q.Get_XZ_Star(result);
				break;
			}
			Interferometric_Plus_WDS.Find_WDS_IF_Matches(XZ80Q.RA_rad * (180.0 / Math.PI) / 15.0, XZ80Q.Dec_rad * (180.0 / Math.PI), HighPrecision: false, ShowResults: true, out WDS_Interferometer_Display.VariableID);
		}

		private void cmdPlot_Click(object sender, EventArgs e)
		{
			DrawLimbPlot();
		}

		private void DrawLimbPlot()
		{
			if (!IsPlotting)
			{
				IsPlotting = true;
				int width = ((Control)picLimbs).get_Width();
				int height = ((Control)picLimbs).get_Height();
				if (!(width < 100 || height < 50))
				{
					Bitmap image = new Bitmap(width, height);
					Graphics graphics = Graphics.FromImage(image);
					PlotLimbs(graphics, width, height, Printer: false);
					picLimbs.set_Image((Image)image);
					graphics.Dispose();
				}
			}
		}

		private void printLimbPlotToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0016: Unknown result type (might be due to invalid IL or missing references)
			//IL_001b: Unknown result type (might be due to invalid IL or missing references)
			//IL_002e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0035: Unknown result type (might be due to invalid IL or missing references)
			//IL_003b: Invalid comparison between Unknown and I4
			if (!IsPlotting)
			{
				IsPlotting = true;
				PrintDocument printDocument = new PrintDocument();
				PrintDialog val = new PrintDialog();
				val.set_UseEXDialog(true);
				printDocument.DefaultPageSettings.Landscape = false;
				val.set_Document(printDocument);
				if ((int)((CommonDialog)val).ShowDialog() == 1)
				{
					printDocument.PrintPage += PrintLimbs;
					printDocument.Print();
				}
			}
		}

		private void PrintLimbs(object sender, PrintPageEventArgs e)
		{
			Graphics graphics = e.Graphics;
			int num = (int)(0.92 * (double)e.PageBounds.Width);
			int chartHeight = num;
			PlotLimbs(graphics, num, chartHeight, Printer: true);
		}

		private void PlotLimbs(Graphics formGraphics, int ChartWidth, int ChartHeight, bool Printer)
		{
			//IL_01d0: Unknown result type (might be due to invalid IL or missing references)
			//IL_01d6: Invalid comparison between Unknown and I4
			float num = 0.025f;
			SolvePairing();
			bool flag = plotLimbInBWToolStripMenuItem.get_Checked() || Printer;
			float num2 = (float)ChartWidth / 1.2f / (1f + (float)trackBar1.get_Value() / 60f) / (1f + (float)trackBar1.get_Value() / 60f);
			if (chkMagnify.get_Checked())
			{
				num2 /= 5f;
			}
			((Control)lblScale).set_Text(string.Format("{0,1:f2}\"", (float)ChartWidth / num2));
			float num3 = ((ScrollBar)Hbar).get_Value();
			float num4 = ((ScrollBar)Vbar).get_Value();
			if (Settings.Default.GraphicsSmoothed)
			{
				formGraphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			Font font = new Font("Arial", 7f);
			Font font2 = new Font("Arial", 8f);
			new Font("Arial", 9f, FontStyle.Bold);
			new Font("Arial", 12f, FontStyle.Bold);
			Cursor.set_Current(Cursors.get_WaitCursor());
			SetMulticolorPens(flag || Printer);
			Pen pen;
			Brush brush;
			Brush brush2;
			Brush brush3;
			if (flag)
			{
				pen = new Pen(Brushes.Black, 1f);
				new Pen(Brushes.Black, 2f);
				brush = Brushes.Black;
				brush2 = Brushes.White;
				brush3 = Brushes.Black;
			}
			else
			{
				pen = new Pen(Brushes.WhiteSmoke, 1f);
				new Pen(Brushes.WhiteSmoke, 2f);
				brush = Brushes.White;
				brush2 = Brushes.Black;
				brush3 = Brushes.Yellow;
			}
			Pen pen2 = new Pen(Brushes.WhiteSmoke, 1f);
			if (flag)
			{
				formGraphics.Clear(Color.White);
				formGraphics.DrawRectangle(pen, 0, 0, ChartWidth - 1, ChartHeight - 1);
			}
			else
			{
				formGraphics.Clear(Color.Black);
			}
			for (int i = 0; i < DEvents.Count; i++)
			{
				if ((int)chkEvents.GetItemCheckState(i + 1) == 1)
				{
					double pA = DEvents[i].PA;
					double num5 = DEvents[i].PA + DEvents[i].CCT;
					double limbSlope = DEvents[i].LimbSlope;
					double num6 = DEvents[i].RV * (DEvents[i].T2 - DEvents[i].T1) / Math.Abs(Math.Cos((pA - num5) / (180.0 / Math.PI)) / Math.Cos((pA + limbSlope - num5) / (180.0 / Math.PI)));
					double num7 = DEvents[i].RV * (double)num / Math.Abs(Math.Cos((pA - num5) / (180.0 / Math.PI)) / Math.Cos((pA + limbSlope - num5) / (180.0 / Math.PI)));
					int num8 = 1;
					if (Math.Abs(DEvents[i].CCT) > 90.0)
					{
						num8 = -1;
					}
					float num9 = (float)((double)(num3 + (float)ChartWidth / 2f) - (double)num2 * num6 * (double)num8 * Math.Sin(pA / (180.0 / Math.PI)));
					float num10 = (float)((double)(num4 + (float)ChartHeight / 2f) - (double)num2 * num6 * (double)num8 * Math.Cos(pA / (180.0 / Math.PI)));
					float num11 = (float)((double)(num3 + (float)ChartWidth / 2f) - (double)num2 * (num6 + num7) * (double)num8 * Math.Sin(pA / (180.0 / Math.PI))) - num9;
					float num12 = (float)((double)(num4 + (float)ChartHeight / 2f) - (double)num2 * (num6 + num7) * (double)num8 * Math.Cos(pA / (180.0 / Math.PI))) - num10;
					if (chkVisual.get_Checked())
					{
						num11 *= 4f;
						num12 *= 4f;
					}
					double num13 = pA + limbSlope;
					float num14 = (float)((double)num9 - (double)ChartWidth * Math.Sin((num13 + 90.0) / (180.0 / Math.PI)));
					float num15 = (float)((double)num10 - (double)ChartWidth * Math.Cos((num13 + 90.0) / (180.0 / Math.PI)));
					float num16 = (float)((double)num9 - (double)ChartWidth * Math.Sin((num13 - 90.0) / (180.0 / Math.PI)));
					float num17 = (float)((double)num10 - (double)ChartWidth * Math.Cos((num13 - 90.0) / (180.0 / Math.PI)));
					pen2 = PenChords[i % 30];
					pen2.DashPattern = new float[2] { 2f, 6f };
					formGraphics.DrawLine(pen2, num3 + (float)ChartWidth / 2f, num4 + (float)ChartHeight / 2f, num9, num10);
					pen2.DashPattern = new float[2] { 1000f, 1f };
					formGraphics.DrawString(i.ToString(), font2, brush, num9, num10);
					formGraphics.DrawLine(PenChords[i % 30], num14, num15, num16, num17);
					if (chkShowUncertainties.get_Checked())
					{
						ArrayList obj = new ArrayList
						{
							new PointF(num14 + num11, num15 + num12),
							new PointF(num14 - num11, num15 - num12),
							new PointF(num16 - num11, num17 - num12),
							new PointF(num16 + num11, num17 + num12)
						};
						PointF[] points = (PointF[])obj.ToArray(obj[0]!.GetType());
						formGraphics.FillPolygon(BrushChords[i % 30], points);
					}
				}
			}
			formGraphics.FillEllipse(brush3, num3 + (float)ChartWidth / 2f - 5f, num4 + (float)ChartHeight / 2f - 5f, 10f, 10f);
			formGraphics.FillEllipse(brush2, num3 + (float)ChartWidth / 2f - (float)((double)num2 * SepSoln * Math.Sin(PASoln / (180.0 / Math.PI))) - 4f, num4 + (float)ChartHeight / 2f - (float)((double)num2 * SepSoln * Math.Cos(PASoln / (180.0 / Math.PI))) - 4f, 8f, 8f);
			formGraphics.FillEllipse(brush3, num3 + (float)ChartWidth / 2f - (float)((double)num2 * SepSoln * Math.Sin(PASoln / (180.0 / Math.PI))) - 3f, num4 + (float)ChartHeight / 2f - (float)((double)num2 * SepSoln * Math.Cos(PASoln / (180.0 / Math.PI))) - 3f, 6f, 6f);
			formGraphics.DrawString(((Control)txtStarID).get_Text().Trim().PadRight(44) + "Plot width = " + string.Format("{0,1:f2}\"", (float)ChartWidth / num2), font2, brush, 5f, 5f);
			formGraphics.DrawString(MeanDate, font2, brush, 5f, 18f);
			formGraphics.DrawString("Occult " + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version, font, brush3, 5f, ChartHeight - 12);
			formGraphics.Dispose();
			int num18 = (int)Math.Floor((double)DEvents.Count / 5.0);
			int height = 12 * DEvents.Count + 4 * num18 + 5;
			((Control)picLegend).set_Height(height);
			int width;
			((Control)picLegend).set_Width(width = 220);
			Bitmap image = new Bitmap(width, height);
			Graphics graphics = Graphics.FromImage(image);
			if (PenChords[0] == null)
			{
				SetMulticolorPens(flag);
			}
			if (Settings.Default.GraphicsSmoothed)
			{
				graphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			graphics.Clear(Color.White);
			for (int j = 0; j < DEvents.Count; j++)
			{
				graphics.DrawString(string.Format("{0,1}, {1,1:f2}, ", j, DEvents[j].T1) + Utilities.ProperCase(DEvents[j].Observer), font2, Brushes.Black, 75f, (float)((double)(12 * j) + 4.0 * Math.Floor((double)j / 5.0)));
				graphics.DrawLine(PenChords[j % 30], 5, 7 + j * 12 + 4 * (int)Math.Floor((double)j / 5.0), 70, 7 + j * 12 + 4 * (int)Math.Floor((double)j / 5.0));
				graphics.DrawLine(PenChords[j % 30], 5, 6 + j * 12 + 4 * (int)Math.Floor((double)j / 5.0), 70, 6 + j * 12 + 4 * (int)Math.Floor((double)j / 5.0));
			}
			picLegend.set_Image((Image)image);
			graphics.Dispose();
			IsPlotting = false;
		}

		private static void SetMulticolorPens(bool BW)
		{
			if (!BW)
			{
				PenChords[0] = new Pen(Color.Aqua, LineThickness);
				PenChords[1] = new Pen(Color.YellowGreen, LineThickness);
				PenChords[2] = new Pen(Color.LightCoral, LineThickness);
				PenChords[3] = new Pen(Color.Green, LineThickness);
				PenChords[4] = new Pen(Color.Orange, LineThickness);
				PenChords[5] = new Pen(Color.Red, LineThickness);
				PenChords[6] = new Pen(Color.CadetBlue, LineThickness);
				PenChords[7] = new Pen(Color.HotPink, LineThickness);
				PenChords[8] = new Pen(Color.RoyalBlue, LineThickness);
				PenChords[9] = new Pen(Color.Crimson, LineThickness);
				PenChords[10] = new Pen(Color.Aquamarine, LineThickness);
				PenChords[11] = new Pen(Color.BlueViolet, LineThickness);
				PenChords[12] = new Pen(Color.Brown, LineThickness);
				PenChords[13] = new Pen(Color.Chartreuse, LineThickness);
				PenChords[14] = new Pen(Color.Coral, LineThickness);
				PenChords[15] = new Pen(Color.CornflowerBlue, LineThickness);
				PenChords[16] = new Pen(Color.Fuchsia, LineThickness);
				PenChords[17] = new Pen(Color.Goldenrod, LineThickness);
				PenChords[18] = new Pen(Color.Gold, LineThickness);
				PenChords[19] = new Pen(Color.IndianRed, LineThickness);
				PenChords[20] = new Pen(Color.Khaki, LineThickness);
				PenChords[21] = new Pen(Color.RoyalBlue, LineThickness);
				PenChords[22] = new Pen(Color.Purple, LineThickness);
				PenChords[23] = new Pen(Color.MediumVioletRed, LineThickness);
				PenChords[24] = new Pen(Color.MidnightBlue, LineThickness);
				PenChords[25] = new Pen(Color.OrangeRed, LineThickness);
				PenChords[26] = new Pen(Color.SlateBlue, LineThickness);
				PenChords[27] = new Pen(Color.Teal, LineThickness);
				PenChords[28] = new Pen(Color.Yellow, LineThickness);
				PenChords[29] = new Pen(Color.Olive, LineThickness);
				BrushChords[0] = new SolidBrush(Color.FromArgb(80, Color.Aqua));
				BrushChords[1] = new SolidBrush(Color.FromArgb(80, Color.YellowGreen));
				BrushChords[2] = new SolidBrush(Color.FromArgb(80, Color.LightCoral));
				BrushChords[3] = new SolidBrush(Color.FromArgb(80, Color.Green));
				BrushChords[4] = new SolidBrush(Color.FromArgb(80, Color.Orange));
				BrushChords[5] = new SolidBrush(Color.FromArgb(80, Color.Red));
				BrushChords[6] = new SolidBrush(Color.FromArgb(80, Color.CadetBlue));
				BrushChords[7] = new SolidBrush(Color.FromArgb(80, Color.HotPink));
				BrushChords[8] = new SolidBrush(Color.FromArgb(80, Color.RoyalBlue));
				BrushChords[9] = new SolidBrush(Color.FromArgb(80, Color.Crimson));
				BrushChords[10] = new SolidBrush(Color.FromArgb(80, Color.Aquamarine));
				BrushChords[11] = new SolidBrush(Color.FromArgb(80, Color.BlueViolet));
				BrushChords[12] = new SolidBrush(Color.FromArgb(80, Color.Brown));
				BrushChords[13] = new SolidBrush(Color.FromArgb(80, Color.Chartreuse));
				BrushChords[14] = new SolidBrush(Color.FromArgb(80, Color.Coral));
				BrushChords[15] = new SolidBrush(Color.FromArgb(80, Color.CornflowerBlue));
				BrushChords[16] = new SolidBrush(Color.FromArgb(80, Color.Fuchsia));
				BrushChords[17] = new SolidBrush(Color.FromArgb(80, Color.Goldenrod));
				BrushChords[18] = new SolidBrush(Color.FromArgb(80, Color.Gold));
				BrushChords[19] = new SolidBrush(Color.FromArgb(80, Color.IndianRed));
				BrushChords[20] = new SolidBrush(Color.FromArgb(80, Color.Khaki));
				BrushChords[21] = new SolidBrush(Color.FromArgb(80, Color.RoyalBlue));
				BrushChords[22] = new SolidBrush(Color.FromArgb(80, Color.Purple));
				BrushChords[23] = new SolidBrush(Color.FromArgb(80, Color.MediumVioletRed));
				BrushChords[24] = new SolidBrush(Color.FromArgb(80, Color.MidnightBlue));
				BrushChords[25] = new SolidBrush(Color.FromArgb(80, Color.OrangeRed));
				BrushChords[26] = new SolidBrush(Color.FromArgb(80, Color.SlateBlue));
				BrushChords[27] = new SolidBrush(Color.FromArgb(80, Color.Teal));
				BrushChords[28] = new SolidBrush(Color.FromArgb(80, Color.Yellow));
				BrushChords[29] = new SolidBrush(Color.FromArgb(80, Color.Olive));
			}
			else
			{
				for (int i = 0; i < 30; i++)
				{
					PenChords[i] = new Pen(Color.Black, LineThickness);
					BrushChords[i] = new SolidBrush(Color.FromArgb(20, Color.Black));
				}
			}
		}

		private void trackBar1_Scroll(object sender, EventArgs e)
		{
			cmdPlot_Click(sender, e);
		}

		private void chkEvents_ItemCheck(object sender, ItemCheckEventArgs e)
		{
			//IL_005e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0068: Expected O, but got Unknown
			if (((ListControl)chkEvents).get_SelectedIndex() >= 1)
			{
				int selectedIndex = ((ListControl)chkEvents).get_SelectedIndex();
				DEvents[((ListControl)chkEvents).get_SelectedIndex() - 1].IsChecked = !DEvents[((ListControl)chkEvents).get_SelectedIndex() - 1].IsChecked;
				((Control)this).BeginInvoke((Delegate)(MethodInvoker)delegate
				{
					DrawLimbPlot();
				});
				DrawLimbPlot();
				((ListControl)chkEvents).set_SelectedIndex(selectedIndex);
			}
		}

		private void copyLimbPlotToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picLimbs.get_Image());
		}

		private void saveLimbPlotToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.SaveGraphic(picLimbs.get_Image(), "DoublePlot " + ((Control)txtStarID).get_Text().Trim(), Utilities.AppPath + "\\Observations\\Doubles");
			Output.SaveGraphic(picLegend.get_Image(), "DoublePlot " + ((Control)txtStarID).get_Text().Trim() + " Legend", Utilities.AppPath + "\\Observations\\Doubles");
		}

		private void plotLimbInBWToolStripMenuItem_Click(object sender, EventArgs e)
		{
			plotLimbInBWToolStripMenuItem.set_Checked(!plotLimbInBWToolStripMenuItem.get_Checked());
			Settings.Default.DoubleSolvePlotBW = plotLimbInBWToolStripMenuItem.get_Checked();
			DrawLimbPlot();
		}

		private void cmdListEquivalents_Click(object sender, EventArgs e)
		{
			string text = ((Control)txtStarID).get_Text();
			Cursor.set_Current(Cursors.get_WaitCursor());
			Vizier_Sesame.GetXZ80Equivalents(text.Substring(0, 1), text.Substring(1));
			Cursor.set_Current(Cursors.get_Default());
		}

		private void saveEntireFormAsAnImageToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Application.DoEvents();
			Bitmap bitmap = new Bitmap(((Control)this).get_Bounds().Width, ((Control)this).get_Bounds().Height);
			Graphics.FromImage(bitmap).CopyFromScreen(((Form)this).get_Location().X, ((Form)this).get_Location().Y, 0, 0, bitmap.Size);
			string fileRootName = "DoubleStar " + ((Control)txtStarID).get_Text().Trim();
			Output.SaveGraphic(bitmap, fileRootName, Utilities.AppPath + "\\Observations\\Doubles");
		}

		private void chkMagnify_CheckedChanged(object sender, EventArgs e)
		{
			DrawLimbPlot();
		}

		private void Vbar_Scroll(object sender, ScrollEventArgs e)
		{
			if (!Updating)
			{
				Updating = true;
				DrawLimbPlot();
				Updating = false;
			}
		}

		private void Hbar_Scroll(object sender, ScrollEventArgs e)
		{
			if (!Updating)
			{
				Updating = true;
				DrawLimbPlot();
				Updating = false;
			}
		}

		private void readObservationFilesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ReadMultipleObservationFiles();
		}

		private void cmdReadMultipleFiles_Click(object sender, EventArgs e)
		{
			ReadMultipleObservationFiles();
		}

		private void ReadMultipleObservationFiles()
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_0044: Unknown result type (might be due to invalid IL or missing references)
			//IL_004a: Invalid comparison between Unknown and I4
			OpenFileDialog val = new OpenFileDialog();
			((FileDialog)val).set_Title("Select Double Star files to read");
			((FileDialog)val).set_InitialDirectory(Utilities.AppPath + "\\Observations\\Doubles");
			((FileDialog)val).set_FileName("*.txt");
			((FileDialog)val).set_Filter("Text files (*.txt)|*.txt|All files (*.*)|*.*");
			val.set_Multiselect(true);
			if ((int)((CommonDialog)val).ShowDialog() != 1)
			{
				return;
			}
			string[] fileNames = ((FileDialog)val).get_FileNames();
			for (int i = 0; i < fileNames.Length; i++)
			{
				string inLine;
				using (StreamReader streamReader = new StreamReader(fileNames[i]))
				{
					inLine = streamReader.ReadToEnd();
				}
				PlaceTextInReport(inLine, MultipleLines: true);
				EncodeLine(((ObjectCollection)chkEvents.get_Items()).get_Count(), Edit: false, Checked: true);
			}
			if (DEvents.Count > 1)
			{
				DrawLimbPlot();
			}
		}

		private void cmdGetLimbSlope_Click(object sender, EventArgs e)
		{
			GetLimbSlope();
		}

		private void GetLimbSlope()
		{
			if (!((L == 0.0) & (B == 0.0) & (AA == 0.0)))
			{
				ReductionProfile.l = L;
				ReductionProfile.b = B;
				ReductionProfile.AA_Min = AA - 1.0;
				ReductionProfile.AA_Max = AA + 1.0;
				ReductionProfile.AAforSlope = AA;
				ReductionProfile.h_Max = 3.5;
				ReductionProfile.h_Min = -3.5;
				ReductionProfile.Show_ReductionProfile(-3);
			}
		}

		private void chkEvents_DoubleClick(object sender, EventArgs e)
		{
			if (((ListControl)chkEvents).get_SelectedIndex() >= 0)
			{
				if (!chkEvents.GetItemChecked(((ListControl)chkEvents).get_SelectedIndex()))
				{
					chkEvents.SetItemCheckState(((ListControl)chkEvents).get_SelectedIndex(), (CheckState)1);
				}
				else
				{
					chkEvents.SetItemCheckState(((ListControl)chkEvents).get_SelectedIndex(), (CheckState)0);
				}
			}
		}

		private void cmdReadSolutionFile_Click(object sender, EventArgs e)
		{
			ReadSolutionFile();
		}

		private void chkShowUncertainties_CheckedChanged(object sender, EventArgs e)
		{
			DrawLimbPlot();
		}

		private void chkVisual_CheckedChanged(object sender, EventArgs e)
		{
			DrawLimbPlot();
		}

		private void copySolutionToolStripMenuItem_Click(object sender, EventArgs e)
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendLine(((Control)txtStarID).get_Text());
			stringBuilder.AppendLine(((Control)lblSep).get_Text());
			stringBuilder.AppendLine(((Control)lblPA).get_Text());
			if (((Control)lblRMS).get_Visible())
			{
				stringBuilder.AppendLine(((Control)lblRMS).get_Text());
			}
			stringBuilder.AppendLine(((Control)lblMeanDate).get_Text());
			stringBuilder.AppendLine("Number of observations: " + (((ObjectCollection)chkEvents.get_Items()).get_Count() - 1));
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
			//IL_1c75: Unknown result type (might be due to invalid IL or missing references)
			//IL_1c7f: Expected O, but got Unknown
			//IL_3775: Unknown result type (might be due to invalid IL or missing references)
			//IL_377f: Expected O, but got Unknown
			//IL_3806: Unknown result type (might be due to invalid IL or missing references)
			//IL_3810: Expected O, but got Unknown
			//IL_3e2a: Unknown result type (might be due to invalid IL or missing references)
			//IL_3e34: Expected O, but got Unknown
			txtT1 = new TextBox();
			txtRV = new TextBox();
			txtCCT = new TextBox();
			txtPA = new TextBox();
			txtT2 = new TextBox();
			label2 = new Label();
			label3 = new Label();
			label4 = new Label();
			label5 = new Label();
			label6 = new Label();
			cmdAdd = new Button();
			cmdReplace = new Button();
			lblRMS = new Label();
			lblPA = new Label();
			lblSep = new Label();
			cmdDelete = new Button();
			menuStrip1 = new MenuStrip();
			fileToolStripMenuItem = new ToolStripMenuItem();
			newToolStripMenuItem1 = new ToolStripMenuItem();
			readObservationFilesToolStripMenuItem = new ToolStripMenuItem();
			openToolStripMenuItem1 = new ToolStripMenuItem();
			saveToolStripMenuItem1 = new ToolStripMenuItem();
			toolStripSeparator3 = new ToolStripSeparator();
			saveEntireFormAsAnImageToolStripMenuItem = new ToolStripMenuItem();
			withObservationsToolStripMenuItem = new ToolStripMenuItem();
			plotLimbInBWToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			copySolutionToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			copyLimbPlotToolStripMenuItem = new ToolStripMenuItem();
			printLimbPlotToolStripMenuItem = new ToolStripMenuItem();
			saveLimbPlotToolStripMenuItem = new ToolStripMenuItem();
			magnitudeCalculatorToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			toolStripMenuItem1 = new ToolStripMenuItem();
			label1 = new Label();
			txtDay = new TextBox();
			label7 = new Label();
			txtMonth = new TextBox();
			label8 = new Label();
			txtYear = new TextBox();
			label9 = new Label();
			txtStarID = new TextBox();
			label10 = new Label();
			label11 = new Label();
			lblBestFit = new Label();
			txtSlope = new TextBox();
			label12 = new Label();
			cmdPaste = new Button();
			chkEvents = new CheckedListBox();
			cmdSwapT = new Button();
			groupBox1 = new GroupBox();
			label31 = new Label();
			txtScale = new TextBox();
			label28 = new Label();
			label29 = new Label();
			label30 = new Label();
			lbl31 = new Label();
			txtL = new TextBox();
			txtAA = new TextBox();
			txtB = new TextBox();
			cmdGetLimbSlope = new Button();
			label27 = new Label();
			label24 = new Label();
			label25 = new Label();
			label26 = new Label();
			txtCA = new TextBox();
			txtAlt = new TextBox();
			txtIllum = new TextBox();
			label22 = new Label();
			txtObserver = new TextBox();
			label19 = new Label();
			txtY1 = new TextBox();
			label20 = new Label();
			txtM1 = new TextBox();
			label21 = new Label();
			txtD1 = new TextBox();
			groupBox2 = new GroupBox();
			cmdListEquivalents = new Button();
			cmdShowWDS_IF = new Button();
			label13 = new Label();
			cmdPlot = new Button();
			trackBar1 = new TrackBar();
			lblScale = new Label();
			label14 = new Label();
			label15 = new Label();
			label16 = new Label();
			label17 = new Label();
			label18 = new Label();
			label23 = new Label();
			PanelLegend = new Panel();
			picLegend = new PictureBox();
			picLimbs = new PictureBox();
			chkMagnify = new CheckBox();
			Vbar = new VScrollBar();
			Hbar = new HScrollBar();
			cmdReadMultipleFiles = new Button();
			cmdReadSolutionFile = new Button();
			chkShowUncertainties = new CheckBox();
			chkVisual = new CheckBox();
			lblMeanDate = new Label();
			panel1 = new Panel();
			lblUncertainty = new Label();
			((Control)menuStrip1).SuspendLayout();
			((Control)groupBox1).SuspendLayout();
			((Control)groupBox2).SuspendLayout();
			((ISupportInitialize)trackBar1).BeginInit();
			((Control)PanelLegend).SuspendLayout();
			((ISupportInitialize)picLegend).BeginInit();
			((ISupportInitialize)picLimbs).BeginInit();
			((Control)panel1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)txtT1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtT1).set_Location(new Point(9, 45));
			((Control)txtT1).set_Name("txtT1");
			((Control)txtT1).set_Size(new Size(48, 20));
			((Control)txtT1).set_TabIndex(2);
			((Control)txtT1).add_Enter((EventHandler)txtT1_Enter);
			((Control)txtRV).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtRV).set_Location(new Point(186, 45));
			((Control)txtRV).set_Name("txtRV");
			((Control)txtRV).set_Size(new Size(48, 20));
			((Control)txtRV).set_TabIndex(9);
			((Control)txtRV).add_Enter((EventHandler)txtRV_Enter);
			((Control)txtCCT).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtCCT).set_Location(new Point(245, 45));
			((Control)txtCCT).set_Name("txtCCT");
			((Control)txtCCT).set_Size(new Size(41, 20));
			((Control)txtCCT).set_TabIndex(11);
			((Control)txtCCT).add_Enter((EventHandler)txtCCT_Enter);
			((Control)txtPA).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtPA).set_Location(new Point(127, 45));
			((Control)txtPA).set_Name("txtPA");
			((Control)txtPA).set_Size(new Size(48, 20));
			((Control)txtPA).set_TabIndex(7);
			((Control)txtPA).add_Enter((EventHandler)txtPA_Enter);
			((Control)txtT2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtT2).set_Location(new Point(68, 45));
			((Control)txtT2).set_Name("txtT2");
			((Control)txtT2).set_Size(new Size(48, 20));
			((Control)txtT2).set_TabIndex(5);
			((Control)txtT2).add_Enter((EventHandler)txtT2_Enter);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(8, 16));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(51, 13));
			((Control)label2).set_TabIndex(0);
			((Control)label2).set_Text("T1 (secs)");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(67, 16));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(51, 13));
			((Control)label3).set_TabIndex(3);
			((Control)label3).set_Text("T2 (secs)");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(202, 29));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(16, 13));
			((Control)label4).set_TabIndex(8);
			((Control)label4).set_Text("rv");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(254, 29));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(22, 13));
			((Control)label5).set_TabIndex(10);
			((Control)label5).set_Text("cct");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(141, 29));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(21, 13));
			((Control)label6).set_TabIndex(6);
			((Control)label6).set_Text("PA");
			((Control)cmdAdd).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdAdd).set_Location(new Point(17, 188));
			((Control)cmdAdd).set_Name("cmdAdd");
			((Control)cmdAdd).set_Size(new Size(62, 28));
			((Control)cmdAdd).set_TabIndex(39);
			((Control)cmdAdd).set_Text("Add");
			((ButtonBase)cmdAdd).set_UseVisualStyleBackColor(true);
			((Control)cmdAdd).add_Click((EventHandler)cmdAdd_Click);
			((Control)cmdReplace).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdReplace).set_Location(new Point(102, 188));
			((Control)cmdReplace).set_Name("cmdReplace");
			((Control)cmdReplace).set_Size(new Size(62, 28));
			((Control)cmdReplace).set_TabIndex(40);
			((Control)cmdReplace).set_Text("Replace");
			((ButtonBase)cmdReplace).set_UseVisualStyleBackColor(true);
			((Control)cmdReplace).add_Click((EventHandler)cmdReplace_Click);
			((Control)lblRMS).set_AutoSize(true);
			((Control)lblRMS).set_Location(new Point(219, 436));
			((Control)lblRMS).set_Name("lblRMS");
			((Control)lblRMS).set_Size(new Size(92, 13));
			((Control)lblRMS).set_TabIndex(9);
			((Control)lblRMS).set_Text("Residuals: RMS =");
			((Control)lblPA).set_AutoSize(true);
			((Control)lblPA).set_Location(new Point(246, 420));
			((Control)lblPA).set_Name("lblPA");
			((Control)lblPA).set_Size(new Size(30, 13));
			((Control)lblPA).set_TabIndex(8);
			((Control)lblPA).set_Text("PA =");
			((Control)lblSep).set_AutoSize(true);
			((Control)lblSep).set_Location(new Point(71, 420));
			((Control)lblSep).set_Name("lblSep");
			((Control)lblSep).set_Size(new Size(35, 13));
			((Control)lblSep).set_TabIndex(7);
			((Control)lblSep).set_Text("Sep =");
			((Control)cmdDelete).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDelete).set_Location(new Point(187, 188));
			((Control)cmdDelete).set_Name("cmdDelete");
			((Control)cmdDelete).set_Size(new Size(62, 28));
			((Control)cmdDelete).set_TabIndex(41);
			((Control)cmdDelete).set_Text("Delete");
			((ButtonBase)cmdDelete).set_UseVisualStyleBackColor(true);
			((Control)cmdDelete).add_Click((EventHandler)cmdDelete_Click);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)fileToolStripMenuItem,
				(ToolStripItem)withObservationsToolStripMenuItem,
				(ToolStripItem)magnitudeCalculatorToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem,
				(ToolStripItem)toolStripMenuItem1
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(921, 24));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)fileToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)newToolStripMenuItem1,
				(ToolStripItem)readObservationFilesToolStripMenuItem,
				(ToolStripItem)openToolStripMenuItem1,
				(ToolStripItem)saveToolStripMenuItem1,
				(ToolStripItem)toolStripSeparator3,
				(ToolStripItem)saveEntireFormAsAnImageToolStripMenuItem
			});
			((ToolStripItem)fileToolStripMenuItem).set_Font(new Font("Segoe UI", 9f));
			((ToolStripItem)fileToolStripMenuItem).set_Name("fileToolStripMenuItem");
			((ToolStripItem)fileToolStripMenuItem).set_Size(new Size(46, 20));
			((ToolStripItem)fileToolStripMenuItem).set_Text("File...");
			((ToolStripItem)newToolStripMenuItem1).set_Image((Image)Resources.NewDocumentHS);
			((ToolStripItem)newToolStripMenuItem1).set_Name("newToolStripMenuItem1");
			((ToolStripItem)newToolStripMenuItem1).set_Size(new Size(253, 22));
			((ToolStripItem)newToolStripMenuItem1).set_Text("New");
			((ToolStripItem)newToolStripMenuItem1).add_Click((EventHandler)newToolStripMenuItem1_Click_1);
			((ToolStripItem)readObservationFilesToolStripMenuItem).set_Image((Image)Resources.openfolderHS);
			((ToolStripItem)readObservationFilesToolStripMenuItem).set_Name("readObservationFilesToolStripMenuItem");
			((ToolStripItem)readObservationFilesToolStripMenuItem).set_Size(new Size(253, 22));
			((ToolStripItem)readObservationFilesToolStripMenuItem).set_Text("Import from observer file(s)");
			((ToolStripItem)readObservationFilesToolStripMenuItem).add_Click((EventHandler)readObservationFilesToolStripMenuItem_Click);
			((ToolStripItem)openToolStripMenuItem1).set_Image((Image)Resources.openfolderHS);
			((ToolStripItem)openToolStripMenuItem1).set_Name("openToolStripMenuItem1");
			((ToolStripItem)openToolStripMenuItem1).set_Size(new Size(253, 22));
			((ToolStripItem)openToolStripMenuItem1).set_Text("Open solution (with observations)");
			((ToolStripItem)openToolStripMenuItem1).add_Click((EventHandler)openToolStripMenuItem1_Click);
			((ToolStripItem)saveToolStripMenuItem1).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem1).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem1).set_Name("saveToolStripMenuItem1");
			((ToolStripItem)saveToolStripMenuItem1).set_Size(new Size(253, 22));
			((ToolStripItem)saveToolStripMenuItem1).set_Text("Save solution (with observations)");
			((ToolStripItem)saveToolStripMenuItem1).add_Click((EventHandler)saveToolStripMenuItem1_Click);
			((ToolStripItem)toolStripSeparator3).set_Name("toolStripSeparator3");
			((ToolStripItem)toolStripSeparator3).set_Size(new Size(250, 6));
			((ToolStripItem)saveEntireFormAsAnImageToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)saveEntireFormAsAnImageToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveEntireFormAsAnImageToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveEntireFormAsAnImageToolStripMenuItem).set_Name("saveEntireFormAsAnImageToolStripMenuItem");
			((ToolStripItem)saveEntireFormAsAnImageToolStripMenuItem).set_Size(new Size(253, 22));
			((ToolStripItem)saveEntireFormAsAnImageToolStripMenuItem).set_Text("Save entire form as an image");
			((ToolStripItem)saveEntireFormAsAnImageToolStripMenuItem).add_Click((EventHandler)saveEntireFormAsAnImageToolStripMenuItem_Click);
			((ToolStripDropDownItem)withObservationsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[9]
			{
				(ToolStripItem)plotLimbInBWToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)copySolutionToolStripMenuItem,
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)copyLimbPlotToolStripMenuItem,
				(ToolStripItem)printLimbPlotToolStripMenuItem,
				(ToolStripItem)saveLimbPlotToolStripMenuItem
			});
			((ToolStripItem)withObservationsToolStripMenuItem).set_Name("withObservationsToolStripMenuItem");
			((ToolStripItem)withObservationsToolStripMenuItem).set_Size(new Size(135, 20));
			((ToolStripItem)withObservationsToolStripMenuItem).set_Text("with Observations ...   ");
			((ToolStripItem)plotLimbInBWToolStripMenuItem).set_Image((Image)Resources.ColorHS);
			((ToolStripItem)plotLimbInBWToolStripMenuItem).set_Name("plotLimbInBWToolStripMenuItem");
			((ToolStripItem)plotLimbInBWToolStripMenuItem).set_Size(new Size(172, 22));
			((ToolStripItem)plotLimbInBWToolStripMenuItem).set_Text("Plot limb in B&&W");
			((ToolStripItem)plotLimbInBWToolStripMenuItem).add_Click((EventHandler)plotLimbInBWToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(169, 6));
			((ToolStripItem)copySolutionToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copySolutionToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copySolutionToolStripMenuItem).set_Name("copySolutionToolStripMenuItem");
			((ToolStripItem)copySolutionToolStripMenuItem).set_Size(new Size(172, 22));
			((ToolStripItem)copySolutionToolStripMenuItem).set_Text("Copy solution");
			((ToolStripItem)copySolutionToolStripMenuItem).add_Click((EventHandler)copySolutionToolStripMenuItem_Click);
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(172, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("Copy observations");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(172, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("Print observations");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(169, 6));
			((ToolStripItem)copyLimbPlotToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyLimbPlotToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyLimbPlotToolStripMenuItem).set_Name("copyLimbPlotToolStripMenuItem");
			((ToolStripItem)copyLimbPlotToolStripMenuItem).set_Size(new Size(172, 22));
			((ToolStripItem)copyLimbPlotToolStripMenuItem).set_Text("Copy limb plot");
			((ToolStripItem)copyLimbPlotToolStripMenuItem).add_Click((EventHandler)copyLimbPlotToolStripMenuItem_Click);
			((ToolStripItem)printLimbPlotToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printLimbPlotToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printLimbPlotToolStripMenuItem).set_Name("printLimbPlotToolStripMenuItem");
			((ToolStripItem)printLimbPlotToolStripMenuItem).set_Size(new Size(172, 22));
			((ToolStripItem)printLimbPlotToolStripMenuItem).set_Text("Print limb plot");
			((ToolStripItem)printLimbPlotToolStripMenuItem).add_Click((EventHandler)printLimbPlotToolStripMenuItem_Click);
			((ToolStripItem)saveLimbPlotToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveLimbPlotToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveLimbPlotToolStripMenuItem).set_Name("saveLimbPlotToolStripMenuItem");
			((ToolStripItem)saveLimbPlotToolStripMenuItem).set_Size(new Size(172, 22));
			((ToolStripItem)saveLimbPlotToolStripMenuItem).set_Text("Save limb plot");
			((ToolStripItem)saveLimbPlotToolStripMenuItem).add_Click((EventHandler)saveLimbPlotToolStripMenuItem_Click);
			((ToolStripItem)magnitudeCalculatorToolStripMenuItem).set_Image((Image)Resources.CalculatorHS);
			((ToolStripItem)magnitudeCalculatorToolStripMenuItem).set_Name("magnitudeCalculatorToolStripMenuItem");
			((ToolStripItem)magnitudeCalculatorToolStripMenuItem).set_Size(new Size(160, 20));
			((ToolStripItem)magnitudeCalculatorToolStripMenuItem).set_Text("Magnitude calculator    ");
			((ToolStripItem)magnitudeCalculatorToolStripMenuItem).add_Click((EventHandler)magnitudeCalculatorToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click_1);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(53, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((ToolStripItem)toolStripMenuItem1).set_Name("toolStripMenuItem1");
			((ToolStripItem)toolStripMenuItem1).set_Size(new Size(12, 20));
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(92, 22));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(26, 13));
			((Control)label1).set_TabIndex(4);
			((Control)label1).set_Text("Day");
			((Control)txtDay).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtDay).set_Location(new Point(92, 38));
			((Control)txtDay).set_Name("txtDay");
			((Control)txtDay).set_Size(new Size(26, 20));
			((Control)txtDay).set_TabIndex(5);
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(63, 22));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(25, 13));
			((Control)label7).set_TabIndex(2);
			((Control)label7).set_Text("Mth");
			((Control)txtMonth).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMonth).set_Location(new Point(63, 38));
			((Control)txtMonth).set_Name("txtMonth");
			((Control)txtMonth).set_Size(new Size(23, 20));
			((Control)txtMonth).set_TabIndex(3);
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(22, 22));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(29, 13));
			((Control)label8).set_TabIndex(0);
			((Control)label8).set_Text("Year");
			((Control)txtYear).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtYear).set_Location(new Point(17, 38));
			((Control)txtYear).set_Name("txtYear");
			((Control)txtYear).set_Size(new Size(39, 20));
			((Control)txtYear).set_TabIndex(1);
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(157, 22));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(40, 13));
			((Control)label9).set_TabIndex(6);
			((Control)label9).set_Text("Star ID");
			((Control)txtStarID).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtStarID).set_Location(new Point(146, 38));
			((Control)txtStarID).set_Name("txtStarID");
			((Control)txtStarID).set_Size(new Size(62, 20));
			((Control)txtStarID).set_TabIndex(7);
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label10).set_Location(new Point(68, 29));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(48, 13));
			((Control)label10).set_TabIndex(4);
			((Control)label10).set_Text("( fainter )");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label11).set_Location(new Point(6, 29));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(54, 13));
			((Control)label11).set_TabIndex(1);
			((Control)label11).set_Text("( brighter )");
			((Control)lblBestFit).set_AutoSize(true);
			((Control)lblBestFit).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Underline, GraphicsUnit.Point, 0));
			((Control)lblBestFit).set_Location(new Point(9, 401));
			((Control)lblBestFit).set_Name("lblBestFit");
			((Control)lblBestFit).set_Size(new Size(452, 13));
			((Control)lblBestFit).set_TabIndex(6);
			((Control)lblBestFit).set_Text("Solution is based on only 2 observations. Change times by a small amount to estimate the error.");
			((Control)lblBestFit).set_Visible(false);
			((Control)txtSlope).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtSlope).set_Location(new Point(348, 45));
			((Control)txtSlope).set_Name("txtSlope");
			((Control)txtSlope).set_Size(new Size(41, 20));
			((Control)txtSlope).set_TabIndex(15);
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(352, 16));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(32, 26));
			((Control)label12).set_TabIndex(14);
			((Control)label12).set_Text("limb\r\nslope");
			label12.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)cmdPaste).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdPaste).set_Location(new Point(121, 37));
			((Control)cmdPaste).set_Name("cmdPaste");
			((Control)cmdPaste).set_Size(new Size(81, 42));
			((Control)cmdPaste).set_TabIndex(1);
			((Control)cmdPaste).set_Text("Paste an observation");
			((ButtonBase)cmdPaste).set_UseVisualStyleBackColor(true);
			((Control)cmdPaste).add_Click((EventHandler)cmdPaste_Click);
			((Control)chkEvents).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)chkEvents).set_FormattingEnabled(true);
			((ListBox)chkEvents).set_HorizontalExtent(1400);
			((ListBox)chkEvents).set_HorizontalScrollbar(true);
			((Control)chkEvents).set_Location(new Point(18, 482));
			((Control)chkEvents).set_Name("chkEvents");
			((Control)chkEvents).set_Size(new Size(694, 154));
			((Control)chkEvents).set_TabIndex(12);
			chkEvents.add_ItemCheck(new ItemCheckEventHandler(chkEvents_ItemCheck));
			((ListBox)chkEvents).add_SelectedIndexChanged((EventHandler)chkEvents_SelectedIndexChanged);
			((Control)chkEvents).add_DoubleClick((EventHandler)chkEvents_DoubleClick);
			((Control)cmdSwapT).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSwapT).set_Location(new Point(22, 71));
			((Control)cmdSwapT).set_Name("cmdSwapT");
			((Control)cmdSwapT).set_Size(new Size(81, 21));
			((Control)cmdSwapT).set_TabIndex(16);
			((Control)cmdSwapT).set_Text("Swap T1, T2");
			((ButtonBase)cmdSwapT).set_UseVisualStyleBackColor(true);
			((Control)cmdSwapT).add_Click((EventHandler)cmdSwapT_Click);
			((Control)groupBox1).get_Controls().Add((Control)(object)label31);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtScale);
			((Control)groupBox1).get_Controls().Add((Control)(object)label28);
			((Control)groupBox1).get_Controls().Add((Control)(object)label29);
			((Control)groupBox1).get_Controls().Add((Control)(object)label30);
			((Control)groupBox1).get_Controls().Add((Control)(object)lbl31);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtL);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtAA);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtB);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdGetLimbSlope);
			((Control)groupBox1).get_Controls().Add((Control)(object)label27);
			((Control)groupBox1).get_Controls().Add((Control)(object)label24);
			((Control)groupBox1).get_Controls().Add((Control)(object)label25);
			((Control)groupBox1).get_Controls().Add((Control)(object)label26);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtCA);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtAlt);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtIllum);
			((Control)groupBox1).get_Controls().Add((Control)(object)label22);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtObserver);
			((Control)groupBox1).get_Controls().Add((Control)(object)label19);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtY1);
			((Control)groupBox1).get_Controls().Add((Control)(object)label20);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtM1);
			((Control)groupBox1).get_Controls().Add((Control)(object)label21);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtD1);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdSwapT);
			((Control)groupBox1).get_Controls().Add((Control)(object)label12);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtSlope);
			((Control)groupBox1).get_Controls().Add((Control)(object)label11);
			((Control)groupBox1).get_Controls().Add((Control)(object)label10);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdDelete);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdReplace);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdAdd);
			((Control)groupBox1).get_Controls().Add((Control)(object)label6);
			((Control)groupBox1).get_Controls().Add((Control)(object)label5);
			((Control)groupBox1).get_Controls().Add((Control)(object)label4);
			((Control)groupBox1).get_Controls().Add((Control)(object)label3);
			((Control)groupBox1).get_Controls().Add((Control)(object)label2);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtT2);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtPA);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtCCT);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtRV);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtT1);
			((Control)groupBox1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox1).set_Location(new Point(18, 168));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(397, 227));
			((Control)groupBox1).set_TabIndex(5);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Details of an observation");
			((Control)label31).set_AutoSize(true);
			((Control)label31).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label31).set_Location(new Point(300, 29));
			((Control)label31).set_Name("label31");
			((Control)label31).set_Size(new Size(34, 13));
			((Control)label31).set_TabIndex(12);
			((Control)label31).set_Text("Scale");
			((Control)txtScale).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtScale).set_Location(new Point(297, 45));
			((Control)txtScale).set_Name("txtScale");
			((Control)txtScale).set_Size(new Size(40, 20));
			((Control)txtScale).set_TabIndex(13);
			((Control)label28).set_AutoSize(true);
			((Control)label28).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label28).set_Location(new Point(45, 127));
			((Control)label28).set_Name("label28");
			((Control)label28).set_Size(new Size(52, 13));
			((Control)label28).set_TabIndex(25);
			((Control)label28).set_Text("Librations");
			((Control)label29).set_AutoSize(true);
			((Control)label29).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label29).set_Location(new Point(156, 127));
			((Control)label29).set_Name("label29");
			((Control)label29).set_Size(new Size(9, 13));
			((Control)label29).set_TabIndex(28);
			((Control)label29).set_Text("l");
			((Control)label30).set_AutoSize(true);
			((Control)label30).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label30).set_Location(new Point(318, 127));
			((Control)label30).set_Name("label30");
			((Control)label30).set_Size(new Size(21, 13));
			((Control)label30).set_TabIndex(36);
			((Control)label30).set_Text("AA");
			((Control)lbl31).set_AutoSize(true);
			((Control)lbl31).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lbl31).set_Location(new Point(241, 126));
			((Control)lbl31).set_Name("lbl31");
			((Control)lbl31).set_Size(new Size(13, 13));
			((Control)lbl31).set_TabIndex(32);
			((Control)lbl31).set_Text("b");
			((Control)txtL).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtL).set_Location(new Point(167, 123));
			((Control)txtL).set_Name("txtL");
			((Control)txtL).set_Size(new Size(33, 20));
			((Control)txtL).set_TabIndex(27);
			((Control)txtAA).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAA).set_Location(new Point(342, 123));
			((Control)txtAA).set_Name("txtAA");
			((Control)txtAA).set_Size(new Size(45, 20));
			((Control)txtAA).set_TabIndex(35);
			((Control)txtB).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtB).set_Location(new Point(257, 123));
			((Control)txtB).set_Name("txtB");
			((Control)txtB).set_Size(new Size(33, 20));
			((Control)txtB).set_TabIndex(31);
			((Control)cmdGetLimbSlope).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdGetLimbSlope).set_Location(new Point(272, 188));
			((Control)cmdGetLimbSlope).set_Name("cmdGetLimbSlope");
			((Control)cmdGetLimbSlope).set_Size(new Size(110, 28));
			((Control)cmdGetLimbSlope).set_TabIndex(42);
			((Control)cmdGetLimbSlope).set_Text("Review limb slope");
			((ButtonBase)cmdGetLimbSlope).set_UseVisualStyleBackColor(true);
			((Control)cmdGetLimbSlope).add_Click((EventHandler)cmdGetLimbSlope_Click);
			((Control)label27).set_AutoSize(true);
			((Control)label27).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label27).set_Location(new Point(45, 151));
			((Control)label27).set_Name("label27");
			((Control)label27).set_Size(new Size(76, 13));
			((Control)label27).set_TabIndex(26);
			((Control)label27).set_Text("Circumstances");
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label24).set_Location(new Point(145, 151));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(21, 13));
			((Control)label24).set_TabIndex(30);
			((Control)label24).set_Text("CA");
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label25).set_Location(new Point(317, 151));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(19, 13));
			((Control)label25).set_TabIndex(38);
			((Control)label25).set_Text("Alt");
			((Control)label26).set_AutoSize(true);
			((Control)label26).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label26).set_Location(new Point(220, 150));
			((Control)label26).set_Name("label26");
			((Control)label26).set_Size(new Size(36, 13));
			((Control)label26).set_TabIndex(34);
			((Control)label26).set_Text("%Illum");
			((Control)txtCA).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtCA).set_Location(new Point(167, 147));
			((Control)txtCA).set_Name("txtCA");
			((Control)txtCA).set_Size(new Size(41, 20));
			((Control)txtCA).set_TabIndex(29);
			((Control)txtAlt).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAlt).set_Location(new Point(342, 147));
			((Control)txtAlt).set_Name("txtAlt");
			((Control)txtAlt).set_Size(new Size(33, 20));
			((Control)txtAlt).set_TabIndex(37);
			((Control)txtIllum).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtIllum).set_Location(new Point(257, 147));
			((Control)txtIllum).set_Name("txtIllum");
			((Control)txtIllum).set_Size(new Size(38, 20));
			((Control)txtIllum).set_TabIndex(33);
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label22).set_Location(new Point(268, 77));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(50, 13));
			((Control)label22).set_TabIndex(23);
			((Control)label22).set_Text("Observer");
			((Control)txtObserver).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtObserver).set_Location(new Point(264, 93));
			((Control)txtObserver).set_Name("txtObserver");
			((Control)txtObserver).set_Size(new Size(123, 20));
			((Control)txtObserver).set_TabIndex(24);
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label19).set_Location(new Point(153, 77));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(29, 13));
			((Control)label19).set_TabIndex(17);
			((Control)label19).set_Text("Year");
			((Control)txtY1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtY1).set_Location(new Point(148, 93));
			((Control)txtY1).set_Name("txtY1");
			((Control)txtY1).set_Size(new Size(39, 20));
			((Control)txtY1).set_TabIndex(18);
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label20).set_Location(new Point(194, 77));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(25, 13));
			((Control)label20).set_TabIndex(19);
			((Control)label20).set_Text("Mth");
			((Control)txtM1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtM1).set_Location(new Point(194, 93));
			((Control)txtM1).set_Name("txtM1");
			((Control)txtM1).set_Size(new Size(23, 20));
			((Control)txtM1).set_TabIndex(20);
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label21).set_Location(new Point(223, 77));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(26, 13));
			((Control)label21).set_TabIndex(21);
			((Control)label21).set_Text("Day");
			((Control)txtD1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtD1).set_Location(new Point(223, 93));
			((Control)txtD1).set_Name("txtD1");
			((Control)txtD1).set_Size(new Size(26, 20));
			((Control)txtD1).set_TabIndex(22);
			((Control)groupBox2).get_Controls().Add((Control)(object)cmdListEquivalents);
			((Control)groupBox2).get_Controls().Add((Control)(object)label9);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtStarID);
			((Control)groupBox2).get_Controls().Add((Control)(object)label8);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtYear);
			((Control)groupBox2).get_Controls().Add((Control)(object)label7);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtMonth);
			((Control)groupBox2).get_Controls().Add((Control)(object)label1);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtDay);
			((Control)groupBox2).get_Controls().Add((Control)(object)cmdShowWDS_IF);
			((Control)groupBox2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox2).set_Location(new Point(18, 92));
			((Control)groupBox2).set_Name("groupBox2");
			((Control)groupBox2).set_Size(new Size(397, 70));
			((Control)groupBox2).set_TabIndex(4);
			groupBox2.set_TabStop(false);
			((Control)groupBox2).set_Text("Event");
			((Control)cmdListEquivalents).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdListEquivalents).set_Location(new Point(322, 20));
			((Control)cmdListEquivalents).set_Name("cmdListEquivalents");
			((Control)cmdListEquivalents).set_Size(new Size(70, 38));
			((Control)cmdListEquivalents).set_TabIndex(9);
			((Control)cmdListEquivalents).set_Text("List Equivalents");
			((ButtonBase)cmdListEquivalents).set_UseVisualStyleBackColor(true);
			((Control)cmdListEquivalents).add_Click((EventHandler)cmdListEquivalents_Click);
			((Control)cmdShowWDS_IF).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdShowWDS_IF).set_Location(new Point(240, 20));
			((Control)cmdShowWDS_IF).set_Name("cmdShowWDS_IF");
			((Control)cmdShowWDS_IF).set_Size(new Size(70, 38));
			((Control)cmdShowWDS_IF).set_TabIndex(8);
			((Control)cmdShowWDS_IF).set_Text("Show WDS && IF");
			((ButtonBase)cmdShowWDS_IF).set_UseVisualStyleBackColor(true);
			((Control)cmdShowWDS_IF).add_Click((EventHandler)cmdShowWDS_IF_Click);
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label13).set_Location(new Point(18, 466));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(199, 13));
			((Control)label13).set_TabIndex(10);
			((Control)label13).set_Text("Checked events are used for the solution");
			((Control)cmdPlot).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdPlot).set_Location(new Point(300, 37));
			((Control)cmdPlot).set_Name("cmdPlot");
			((Control)cmdPlot).set_Size(new Size(81, 42));
			((Control)cmdPlot).set_TabIndex(3);
			((Control)cmdPlot).set_Text("Solve \r\n&& Plot");
			((ButtonBase)cmdPlot).set_UseVisualStyleBackColor(true);
			((Control)cmdPlot).add_Click((EventHandler)cmdPlot_Click);
			trackBar1.set_LargeChange(10);
			((Control)trackBar1).set_Location(new Point(462, 31));
			trackBar1.set_Maximum(50);
			trackBar1.set_Minimum(-50);
			((Control)trackBar1).set_Name("trackBar1");
			trackBar1.set_Orientation((Orientation)1);
			((Control)trackBar1).set_Size(new Size(45, 396));
			((Control)trackBar1).set_TabIndex(15);
			trackBar1.set_TickFrequency(5);
			trackBar1.add_Scroll((EventHandler)trackBar1_Scroll);
			((Control)lblScale).set_AutoSize(true);
			((Control)lblScale).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblScale).set_Location(new Point(661, 449));
			((Control)lblScale).set_Name("lblScale");
			((Control)lblScale).set_Size(new Size(76, 13));
			((Control)lblScale).set_TabIndex(20);
			((Control)lblScale).set_Text("---    2\"    ---");
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label14).set_Location(new Point(499, 449));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(38, 13));
			((Control)label14).set_TabIndex(19);
			((Control)label14).set_Text("<------");
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label15).set_Location(new Point(865, 449));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(34, 13));
			((Control)label15).set_TabIndex(21);
			((Control)label15).set_Text("----->");
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Font(new Font("Microsoft Sans Serif", 14f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label16).set_Location(new Point(543, 4));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(263, 24));
			((Control)label16).set_TabIndex(16);
			((Control)label16).set_Text("Limb plot, with current solution");
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label17).set_Location(new Point(789, 464));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(62, 17));
			((Control)label17).set_TabIndex(22);
			((Control)label17).set_Text("Legend");
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Location(new Point(445, 24));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(53, 13));
			((Control)label18).set_TabIndex(14);
			((Control)label18).set_Text("Plot scale");
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label23).set_Location(new Point(286, 464));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(159, 17));
			((Control)label23).set_TabIndex(11);
			((Control)label23).set_Text("O b s e r v a t i o n s");
			((ScrollableControl)PanelLegend).set_AutoScroll(true);
			((ScrollableControl)PanelLegend).set_AutoScrollMargin(new Size(10, 10));
			((ScrollableControl)PanelLegend).set_AutoScrollMinSize(new Size(10, 100));
			((Control)PanelLegend).set_BackColor(Color.White);
			PanelLegend.set_BorderStyle((BorderStyle)2);
			((Control)PanelLegend).get_Controls().Add((Control)(object)picLegend);
			((Control)PanelLegend).set_Location(new Point(722, 482));
			((Control)PanelLegend).set_Name("PanelLegend");
			((Control)PanelLegend).set_Size(new Size(196, 154));
			((Control)PanelLegend).set_TabIndex(23);
			((Control)picLegend).set_BackColor(Color.White);
			((Control)picLegend).set_Location(new Point(3, 3));
			((Control)picLegend).set_Name("picLegend");
			((Control)picLegend).set_Size(new Size(124, 154));
			picLegend.set_TabIndex(43);
			picLegend.set_TabStop(false);
			((Control)picLimbs).set_BackColor(Color.Black);
			((Control)picLimbs).set_Location(new Point(499, 29));
			((Control)picLimbs).set_Name("picLimbs");
			((Control)picLimbs).set_Size(new Size(400, 400));
			picLimbs.set_TabIndex(39);
			picLimbs.set_TabStop(false);
			((Control)chkMagnify).set_AutoSize(true);
			chkMagnify.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)chkMagnify).set_Location(new Point(391, 52));
			((Control)chkMagnify).set_Name("chkMagnify");
			((Control)chkMagnify).set_Size(new Size(65, 30));
			((Control)chkMagnify).set_TabIndex(13);
			((Control)chkMagnify).set_Text("Magnify\r\nscale x5");
			((ButtonBase)chkMagnify).set_TextAlign(ContentAlignment.MiddleCenter);
			((ButtonBase)chkMagnify).set_UseVisualStyleBackColor(true);
			chkMagnify.add_CheckedChanged((EventHandler)chkMagnify_CheckedChanged);
			((ScrollBar)Vbar).set_LargeChange(1);
			((Control)Vbar).set_Location(new Point(902, 29));
			((ScrollBar)Vbar).set_Maximum(200);
			((ScrollBar)Vbar).set_Minimum(-200);
			((Control)Vbar).set_Name("Vbar");
			((Control)Vbar).set_Size(new Size(15, 400));
			((Control)Vbar).set_TabIndex(18);
			((ScrollBar)Vbar).add_Scroll(new ScrollEventHandler(Vbar_Scroll));
			((ScrollBar)Hbar).set_LargeChange(1);
			((Control)Hbar).set_Location(new Point(499, 432));
			((ScrollBar)Hbar).set_Maximum(200);
			((ScrollBar)Hbar).set_Minimum(-200);
			((Control)Hbar).set_Name("Hbar");
			((Control)Hbar).set_Size(new Size(400, 14));
			((Control)Hbar).set_TabIndex(17);
			((ScrollBar)Hbar).add_Scroll(new ScrollEventHandler(Hbar_Scroll));
			((Control)cmdReadMultipleFiles).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdReadMultipleFiles).set_Location(new Point(15, 31));
			((Control)cmdReadMultipleFiles).set_Name("cmdReadMultipleFiles");
			((Control)cmdReadMultipleFiles).set_Size(new Size(91, 55));
			((Control)cmdReadMultipleFiles).set_TabIndex(0);
			((Control)cmdReadMultipleFiles).set_Text("Import from observation file(s)");
			((ButtonBase)cmdReadMultipleFiles).set_UseVisualStyleBackColor(true);
			((Control)cmdReadMultipleFiles).add_Click((EventHandler)cmdReadMultipleFiles_Click);
			((Control)cmdReadSolutionFile).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdReadSolutionFile).set_Location(new Point(217, 31));
			((Control)cmdReadSolutionFile).set_Name("cmdReadSolutionFile");
			((Control)cmdReadSolutionFile).set_Size(new Size(68, 55));
			((Control)cmdReadSolutionFile).set_TabIndex(2);
			((Control)cmdReadSolutionFile).set_Text("Read a solution file");
			((ButtonBase)cmdReadSolutionFile).set_UseVisualStyleBackColor(true);
			((Control)cmdReadSolutionFile).add_Click((EventHandler)cmdReadSolutionFile_Click);
			((Control)chkShowUncertainties).set_AutoSize(true);
			((Control)chkShowUncertainties).set_Location(new Point(5, -2));
			((Control)chkShowUncertainties).set_Name("chkShowUncertainties");
			((Control)chkShowUncertainties).set_Size(new Size(86, 30));
			((Control)chkShowUncertainties).set_TabIndex(40);
			((Control)chkShowUncertainties).set_Text("Plot nominal \r\nuncertainties");
			((ButtonBase)chkShowUncertainties).set_UseVisualStyleBackColor(true);
			chkShowUncertainties.add_CheckedChanged((EventHandler)chkShowUncertainties_CheckedChanged);
			((Control)chkVisual).set_AutoSize(true);
			((Control)chkVisual).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkVisual).set_Location(new Point(5, 27));
			((Control)chkVisual).set_Name("chkVisual");
			((Control)chkVisual).set_Size(new Size(91, 17));
			((Control)chkVisual).set_TabIndex(41);
			((Control)chkVisual).set_Text("Visual uncerts");
			((ButtonBase)chkVisual).set_UseVisualStyleBackColor(true);
			chkVisual.add_CheckedChanged((EventHandler)chkVisual_CheckedChanged);
			((Control)lblMeanDate).set_AutoSize(true);
			((Control)lblMeanDate).set_Location(new Point(43, 436));
			((Control)lblMeanDate).set_Name("lblMeanDate");
			((Control)lblMeanDate).set_Size(new Size(63, 13));
			((Control)lblMeanDate).set_TabIndex(42);
			((Control)lblMeanDate).set_Text("Mean Date:");
			panel1.set_BorderStyle((BorderStyle)1);
			((Control)panel1).get_Controls().Add((Control)(object)chkVisual);
			((Control)panel1).get_Controls().Add((Control)(object)chkShowUncertainties);
			((Control)panel1).set_Location(new Point(369, 418));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(94, 47));
			((Control)panel1).set_TabIndex(43);
			((Control)lblUncertainty).set_AutoSize(true);
			((Control)lblUncertainty).set_BackColor(Color.Red);
			((Control)lblUncertainty).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblUncertainty).set_ForeColor(Color.Yellow);
			((Control)lblUncertainty).set_Location(new Point(19, 452));
			((Control)lblUncertainty).set_Name("lblUncertainty");
			((Control)lblUncertainty).set_Size(new Size(292, 13));
			((Control)lblUncertainty).set_TabIndex(44);
			((Control)lblUncertainty).set_Text(" Unreliable solution; the difference in limb slopes is too small. ");
			((Control)lblUncertainty).set_Visible(false);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(921, 639));
			((Control)this).get_Controls().Add((Control)(object)lblUncertainty);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)lblMeanDate);
			((Control)this).get_Controls().Add((Control)(object)cmdReadSolutionFile);
			((Control)this).get_Controls().Add((Control)(object)cmdReadMultipleFiles);
			((Control)this).get_Controls().Add((Control)(object)Hbar);
			((Control)this).get_Controls().Add((Control)(object)Vbar);
			((Control)this).get_Controls().Add((Control)(object)chkMagnify);
			((Control)this).get_Controls().Add((Control)(object)PanelLegend);
			((Control)this).get_Controls().Add((Control)(object)label23);
			((Control)this).get_Controls().Add((Control)(object)label18);
			((Control)this).get_Controls().Add((Control)(object)label17);
			((Control)this).get_Controls().Add((Control)(object)label16);
			((Control)this).get_Controls().Add((Control)(object)label15);
			((Control)this).get_Controls().Add((Control)(object)label14);
			((Control)this).get_Controls().Add((Control)(object)lblScale);
			((Control)this).get_Controls().Add((Control)(object)cmdPlot);
			((Control)this).get_Controls().Add((Control)(object)picLimbs);
			((Control)this).get_Controls().Add((Control)(object)label13);
			((Control)this).get_Controls().Add((Control)(object)groupBox2);
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Control)this).get_Controls().Add((Control)(object)chkEvents);
			((Control)this).get_Controls().Add((Control)(object)cmdPaste);
			((Control)this).get_Controls().Add((Control)(object)lblBestFit);
			((Control)this).get_Controls().Add((Control)(object)lblSep);
			((Control)this).get_Controls().Add((Control)(object)lblPA);
			((Control)this).get_Controls().Add((Control)(object)lblRMS);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)trackBar1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationLunarDoubleSolve", true, (DataSourceUpdateMode)1));
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Location(Settings.Default.LocationLunarDoubleSolve);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Control)this).set_Name("DoublesSolve");
			((Control)this).set_Text("Solve for double star PA and separation");
			((Form)this).add_Load((EventHandler)DoublesSolve_Load);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((Control)groupBox2).ResumeLayout(false);
			((Control)groupBox2).PerformLayout();
			((ISupportInitialize)trackBar1).EndInit();
			((Control)PanelLegend).ResumeLayout(false);
			((ISupportInitialize)picLegend).EndInit();
			((ISupportInitialize)picLimbs).EndInit();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
