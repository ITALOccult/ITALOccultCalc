using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Printing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Lunar_Observations
{
	public class AnalyseForStarPosition : Form
	{
		internal ArrayList HistoryFiles = new ArrayList();

		internal ArrayList StarList = new ArrayList();

		private static List<AnalysisStarData> Parameters = new List<AnalysisStarData>();

		private string[] Header = new string[3] { "Radius residuals", "R.A. residuals", "Declination residuals" };

		private readonly string AppPath;

		private readonly string ZCNameFile;

		private const double Radian = 180.0 / Math.PI;

		private bool FormCreated;

		private bool ChangingStar;

		private bool PlottingStar;

		private bool SolvingStar;

		private bool BWflag;

		private bool ValidSolution;

		private string StarID = "";

		private double dRA;

		private double dDec;

		private double dRA2000;

		private double dDec2000;

		private double eRA2000;

		private double eDec2000;

		private double eRA;

		private double eDec;

		private double dPMRA;

		private double dPMDec;

		private double ePMRA;

		private double ePMDec;

		private double dDeltaT;

		private double RMS;

		private float TStart = 1800f;

		private IContainer components;

		private Panel panelStar;

		private TextBox txtPlanet;

		private Label label15;

		private Button cmdRead;

		private ProgressBar pbarStar;

		private TextBox txtXZ;

		private ComboBox cmbNames;

		private Label label6;

		private Label label5;

		private Label label4;

		private Label label3;

		private TextBox txtZC;

		private TextBox txtSAO;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem nameOfStarListToolStripMenuItem;

		private ToolStripMenuItem allNamesToolStripMenuItem;

		private ToolStripMenuItem properNamesToolStripMenuItem;

		private ToolStripMenuItem bayerLettersToolStripMenuItem;

		private ToolStripMenuItem magnitudeBrighterThan2ToolStripMenuItem;

		private ToolStripMenuItem magnitudeBrighterThan3ToolStripMenuItem;

		private ToolStripMenuItem magnitudeBrighterThan4ToolStripMenuItem;

		private ListBox lstResiduals;

		private Button cmdSolve;

		private Label lbldRA;

		private Label label1;

		private Label lbldDec;

		private Label lblPMDec;

		private Label lblPM;

		private Label lblPMRA;

		private Label label2;

		private Label lbldeltaT;

		private CheckBox chkPE;

		private CheckBox chkIncludePosition;

		private GroupBox groupBox1;

		private GroupBox groupBox2;

		private Label label7;

		private Label label8;

		private GroupBox groupBox3;

		private PictureBox picStar;

		private CheckBox chk1900;

		private ToolStripMenuItem withImageToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private Label lblError;

		private Label label9;

		private Label lblEventCount;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private CheckBox chkDonly;

		private CheckBox chkGrazes;

		private CheckBox chkRonly;

		private CheckBox chkIterate;

		private CheckBox chkHighConfidence;

		private CheckBox chk1950;

		private CheckBox chkNonVisual;

		private ToolStripMenuItem withResidualListToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem1;

		private ToolStripMenuItem printToolStripMenuItem1;

		private ToolStripMenuItem saveToolStripMenuItem1;

		private ToolStripMenuItem withSolutionToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem2;

		private ToolStripMenuItem printToolStripMenuItem2;

		private ToolStripMenuItem saveToolStripMenuItem2;

		private CheckBox chkPlotCorrected;

		private CheckBox chkHipEpoch;

		private CheckBox chkExpand;

		public AnalyseForStarPosition()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
			ZCNameFile = AppPath + "\\Resource Files\\ZCNames.dat";
		}

		private void AnalyseForStarPosition_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			string[] files = Directory.GetFiles(AppPath + "\\Resource Files\\", "RArchive Observations*.*");
			foreach (string path in files)
			{
				HistoryFiles.Add(Path.GetFileName(path));
			}
			FormCreated = true;
			CreateStarList(2);
			AnalyseForStarPosition_Resize(sender, e);
		}

		private void allNamesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			CreateStarList(2);
		}

		private void properNamesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			CreateStarList(0);
		}

		private void bayerLettersToolStripMenuItem_Click(object sender, EventArgs e)
		{
			CreateStarList(1);
		}

		private void magnitudeBrighterThan2ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			CreateStarList(3);
		}

		private void magnitudeBrighterThan3ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			CreateStarList(4);
		}

		private void magnitudeBrighterThan4ToolStripMenuItem_Click(object sender, EventArgs e)
		{
			CreateStarList(5);
		}

		private void CreateStarList(int ListType)
		{
			if (!FormCreated)
			{
				return;
			}
			Cursor.set_Current(Cursors.get_WaitCursor());
			StarList.Clear();
			cmbNames.get_Items().Clear();
			cmbNames.set_Sorted(ListType != 1 && ListType != 2);
			StarList.Add(0);
			cmbNames.get_Items().Add((object)"");
			StreamReader streamReader = new StreamReader(ZCNameFile);
			do
			{
				string? text = streamReader.ReadLine();
				int num = int.Parse(text!.Substring(0, 4));
				string text2 = text!.Substring(5).Trim();
				int num2 = text2.IndexOf("=");
				int num3 = text2.IndexOf("=", num2 + 2);
				if (num3 > num2)
				{
					text2 = text2.Substring(0, num3 - 1);
				}
				int result;
				switch (ListType)
				{
				case 0:
					if (num2 > 0 && !int.TryParse(text2.Substring(0, 2), out result))
					{
						int index = cmbNames.get_Items().Add((object)text2);
						StarList.Insert(index, num);
					}
					break;
				case 1:
					if (num2 > 0)
					{
						if (!int.TryParse(text2.Substring(num2 + 1, 2), out result))
						{
							int index = cmbNames.get_Items().Add((object)text2.Substring(num2 + 1));
							StarList.Insert(index, num);
						}
					}
					else if (!int.TryParse(text2.Substring(0, 2), out result))
					{
						int index = cmbNames.get_Items().Add((object)text2.Trim());
						StarList.Insert(index, num);
					}
					break;
				case 2:
				{
					int index = cmbNames.get_Items().Add((object)text2);
					StarList.Insert(index, num);
					break;
				}
				case 3:
					if (ZCMagnitude(num) < 2.0)
					{
						int index = cmbNames.get_Items().Add((object)text2);
						StarList.Insert(index, num);
					}
					break;
				case 4:
					if (ZCMagnitude(num) < 3.0)
					{
						int index = cmbNames.get_Items().Add((object)text2);
						StarList.Insert(index, num);
					}
					break;
				case 5:
					if (ZCMagnitude(num) < 4.0)
					{
						int index = cmbNames.get_Items().Add((object)text2);
						StarList.Insert(index, num);
					}
					break;
				}
			}
			while (!streamReader.EndOfStream);
			streamReader.Close();
			Cursor.set_Current(Cursors.get_Default());
			((ListControl)cmbNames).set_SelectedIndex(1);
		}

		private double ZCMagnitude(int ZC)
		{
			XZ80Q.Get_ZC_Star(ZC);
			return XZ80Q.Mv;
		}

		private void SetcmbNames(int ZC)
		{
			for (int i = 1; i < cmbNames.get_Items().get_Count(); i++)
			{
				if (ZC == (int)StarList[i])
				{
					((ListControl)cmbNames).set_SelectedIndex(i);
					return;
				}
			}
			((ListControl)cmbNames).set_SelectedIndex(0);
		}

		private void txtXZ_TextChanged(object sender, EventArgs e)
		{
			if (!ChangingStar)
			{
				ChangingStar = true;
				if (!int.TryParse(((Control)txtXZ).get_Text(), out var result))
				{
					result = 0;
				}
				XZ80Q.Get_XZ_Star(result);
				((Control)txtZC).set_Text(XZ80Q.ZC.ToString());
				((Control)txtSAO).set_Text(XZ80Q.SAO.ToString());
				((Control)txtPlanet).set_Text("0");
				SetcmbNames(XZ80Q.ZC);
				ChangingStar = false;
			}
		}

		private void txtSAO_TextChanged(object sender, EventArgs e)
		{
			if (!ChangingStar)
			{
				ChangingStar = true;
				if (!int.TryParse(((Control)txtSAO).get_Text(), out var result))
				{
					result = 0;
				}
				if (XZ80Q.Get_SAO_Star(result))
				{
					((Control)txtZC).set_Text(XZ80Q.ZC.ToString());
					((Control)txtXZ).set_Text(XZ80Q.XZ.ToString());
				}
				else
				{
					TextBox obj = txtZC;
					string text;
					((Control)txtXZ).set_Text(text = "0");
					((Control)obj).set_Text(text);
				}
				((Control)txtPlanet).set_Text("0");
				SetcmbNames(XZ80Q.ZC);
				ChangingStar = false;
			}
		}

		private void txtZC_TextChanged(object sender, EventArgs e)
		{
			if (!ChangingStar)
			{
				ChangingStar = true;
				if (!int.TryParse(((Control)txtZC).get_Text(), out var result))
				{
					result = 0;
				}
				XZ80Q.Get_ZC_Star(result);
				((Control)txtSAO).set_Text(XZ80Q.SAO.ToString());
				((Control)txtXZ).set_Text(XZ80Q.XZ.ToString());
				((Control)txtPlanet).set_Text("0");
				SetcmbNames(XZ80Q.ZC);
				ChangingStar = false;
			}
		}

		private void txtPlanet_TextChanged(object sender, EventArgs e)
		{
			if (!ChangingStar)
			{
				ChangingStar = true;
				if (!int.TryParse(((Control)txtPlanet).get_Text(), out var result))
				{
					result = 0;
				}
				else
				{
					TextBox obj = txtZC;
					TextBox obj2 = txtSAO;
					string text;
					((Control)txtXZ).set_Text(text = "0");
					string text2;
					((Control)obj2).set_Text(text2 = text);
					((Control)obj).set_Text(text2);
					((ListControl)cmbNames).set_SelectedIndex(0);
				}
				ChangingStar = false;
			}
		}

		private void cmbNames_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (!ChangingStar)
			{
				ChangingStar = true;
				XZ80Q.Get_ZC_Star((int)StarList[((ListControl)cmbNames).get_SelectedIndex()]);
				((Control)txtZC).set_Text(XZ80Q.ZC.ToString());
				((Control)txtSAO).set_Text(XZ80Q.SAO.ToString());
				((Control)txtXZ).set_Text(XZ80Q.XZ.ToString());
				ChangingStar = false;
			}
		}

		private void cmdRead_Click(object sender, EventArgs e)
		{
			if (!int.TryParse(((Control)txtZC).get_Text(), out var result))
			{
				result = 0;
			}
			if (!int.TryParse(((Control)txtSAO).get_Text(), out var result2))
			{
				result2 = 0;
			}
			if (!int.TryParse(((Control)txtXZ).get_Text(), out var result3))
			{
				result3 = 0;
			}
			if (!int.TryParse(((Control)txtPlanet).get_Text(), out var result4))
			{
				result4 = 0;
			}
			StarID = "";
			if (result > 0)
			{
				StarID += string.Format("R{0,1:f0}_", result);
			}
			if (result2 > 0)
			{
				StarID += string.Format("S{0,1:f0}_", result2);
			}
			if (result3 > 0)
			{
				StarID += string.Format("X{0,1:f0}", result3);
			}
			pbarStar.set_Value(0);
			pbarStar.set_Maximum(100);
			((Control)pbarStar).set_Visible(true);
			lstResiduals.get_Items().Clear();
			lstResiduals.get_Items().Add((object)"   #  Year       Res    dRes   sin(PA)  cos(PA)");
			Parameters.Clear();
			for (int i = 0; i < HistoryFiles.Count; i++)
			{
				pbarStar.set_Value(100 * i / HistoryFiles.Count);
				Application.DoEvents();
				using StreamReader streamReader = new StreamReader(AppPath + "\\Resource Files\\" + HistoryFiles[i]!.ToString());
				do
				{
					string text = streamReader.ReadLine();
					if (text.Length < 140 || !int.TryParse(text.Substring(28, 6), out var result5))
					{
						continue;
					}
					bool flag = false;
					if (text.Substring(27, 1) == "R" && result > 0)
					{
						flag = result5 == result;
					}
					if (text.Substring(27, 1) == "S" && result2 > 0)
					{
						flag = result5 == result2;
					}
					if (text.Substring(27, 1) == "X" && result3 > 0)
					{
						flag = result5 == result3;
					}
					if (text.Substring(27, 1) == "P" && result4 > 0)
					{
						flag = result5 / 1000 == result4;
					}
					if (flag)
					{
						AnalysisStarData analysisStarData = new AnalysisStarData();
						if (analysisStarData.ParseReductionLine(text))
						{
							Parameters.Add(analysisStarData);
							lstResiduals.get_Items().Add((object)(string.Format("{0,4:f0} ", Parameters.Count) + analysisStarData.ToString()));
						}
					}
				}
				while (!streamReader.EndOfStream);
			}
			((Control)pbarStar).set_Visible(false);
			SolveStar();
		}

		private void cmdSolve_Click(object sender, EventArgs e)
		{
			SolveStar();
		}

		private void SolveStar()
		{
			if (SolvingStar)
			{
				return;
			}
			SolvingStar = true;
			double[,] array = new double[8, 8];
			double[,] array2 = new double[8, 8];
			double[,] array3 = new double[5000, 8];
			double num = 0.0;
			double num2 = 3000.0;
			double num3 = -3000.0;
			double num4 = 0.0;
			if (chkHipEpoch.get_Checked())
			{
				num4 = -8.75;
			}
			((Control)lblError).set_Visible(false);
			int num5 = 3;
			if (!chkIterate.get_Checked())
			{
				num5 = 0;
			}
			for (int i = 0; i <= num5; i++)
			{
				if (i == 0)
				{
					dRA = (dDec = (dPMRA = (dPMDec = (dDeltaT = 0.0))));
				}
				int num6 = 0;
				for (int j = 0; j < Parameters.Count; j++)
				{
					if (Parameters[j].T < -200.0 || (chk1900.get_Checked() & (Parameters[j].T < -100.0)) || (chk1950.get_Checked() & (Parameters[j].T < -50.0)) || (chkDonly.get_Checked() & (Parameters[j].rdT > 0.0)) || (chkRonly.get_Checked() & (Parameters[j].rdT < 0.0)) || (chkGrazes.get_Checked() & (Math.Abs(Parameters[j].rdT) > 0.03)) || (chkHighConfidence.get_Checked() & (Parameters[j].Certainty != 1)) || (chkNonVisual.get_Checked() & Parameters[j].PEInvolved))
					{
						continue;
					}
					if (chkIncludePosition.get_Checked())
					{
						array3[num6, 0] = Parameters[j].SinPA;
						array3[num6, 1] = Parameters[j].CosPA;
					}
					else
					{
						array3[num6, 0] = (array3[num6, 1] = 0.0);
					}
					array3[num6, 2] = Parameters[j].SinPA * (Parameters[j].T - num4);
					array3[num6, 3] = Parameters[j].CosPA * (Parameters[j].T - num4);
					if (chkPE.get_Checked() & Parameters[j].PEInvolved)
					{
						array3[num6, 4] = Parameters[j].rdT;
					}
					else
					{
						array3[num6, 4] = 0.0;
					}
					array3[num6, 5] = 0.0;
					array3[num6, 6] = 0.0;
					array3[num6, 7] = Parameters[j].Residual - ResidualCorrection(j, num4);
					if (((i == 0) & (Math.Abs(array3[num6, 7]) < 0.5)) | (Math.Abs(array3[num6, 7]) < 2.0 * RMS))
					{
						num6++;
						if (Parameters[j].T < num2)
						{
							num2 = Parameters[j].T - num4;
						}
						if (Parameters[j].T > num3)
						{
							num3 = Parameters[j].T - num4;
						}
					}
				}
				TStart = (float)(10.0 * Math.Floor(num2 / 10.0));
				((Control)lblEventCount).set_Text(num6.ToString());
				if (num6 < 10 || num3 - num2 < 20.0)
				{
					((Control)lblError).set_Visible(true);
					SolvingStar = false;
					ValidSolution = false;
					PlotResults();
					return;
				}
				((Control)lblError).set_Visible(false);
				num = 0.0;
				for (int j = 0; j < num6; j++)
				{
					num += array3[j, 7];
				}
				num /= (double)num6;
				double num7 = 0.0;
				for (int j = 0; j < num6; j++)
				{
					num7 += Math.Pow(array3[j, 7] - num, 2.0);
				}
				RMS = Math.Sqrt(num7 / (double)num6);
				for (int k = 0; k < 7; k++)
				{
					for (int l = k; l <= 7; l++)
					{
						array[k, l] = 0.0;
					}
				}
				for (int j = 0; j < num6; j++)
				{
					for (int k = 0; k < 7; k++)
					{
						for (int l = k; l <= 7; l++)
						{
							array[k, l] += array3[j, k] * array3[j, l];
						}
					}
				}
				for (int k = 1; k < 7; k++)
				{
					for (int l = 0; l < k; l++)
					{
						array[k, l] = array[l, k];
					}
				}
				for (int k = 0; k <= 6; k++)
				{
					for (int l = 0; l <= 6; l++)
					{
						if (k == l)
						{
							array2[k, l] = 1.0;
						}
						else
						{
							array2[k, l] = 0.0;
						}
					}
				}
				for (int m = 0; m <= 6; m++)
				{
					if (array[m, m] == 0.0)
					{
						continue;
					}
					double num8 = array[m, m];
					for (int l = 0; l <= 6; l++)
					{
						array[m, l] /= num8;
						array2[m, l] /= num8;
					}
					for (int k = 0; k <= 6; k++)
					{
						num8 = array[k, m];
						if (k != m)
						{
							for (int l = 0; l <= 6; l++)
							{
								array[k, l] -= array[m, l] * num8;
								array2[k, l] -= array2[m, l] * num8;
							}
						}
					}
				}
				dRA += array2[0, 0] * array[0, 7] + array2[0, 1] * array[1, 7] + array2[0, 2] * array[2, 7] + array2[0, 3] * array[3, 7] + array2[0, 4] * array[4, 7] + array2[0, 5] * array[5, 7] + array2[0, 6] * array[6, 7];
				dDec += array2[1, 0] * array[0, 7] + array2[1, 1] * array[1, 7] + array2[1, 2] * array[2, 7] + array2[1, 3] * array[3, 7] + array2[1, 4] * array[4, 7] + array2[1, 5] * array[5, 7] + array2[1, 6] * array[6, 7];
				dPMRA += array2[2, 0] * array[0, 7] + array2[2, 1] * array[1, 7] + array2[2, 2] * array[2, 7] + array2[2, 3] * array[3, 7] + array2[2, 4] * array[4, 7] + array2[2, 5] * array[5, 7] + array2[2, 6] * array[6, 7];
				dPMDec += array2[3, 0] * array[0, 7] + array2[3, 1] * array[1, 7] + array2[3, 2] * array[2, 7] + array2[3, 3] * array[3, 7] + array2[3, 4] * array[4, 7] + array2[3, 5] * array[5, 7] + array2[3, 6] * array[6, 7];
				dDeltaT += array2[4, 0] * array[0, 7] + array2[4, 1] * array[1, 7] + array2[4, 2] * array[2, 7] + array2[4, 3] * array[3, 7] + array2[4, 4] * array[4, 7] + array2[4, 5] * array[5, 7] + array2[4, 6] * array[6, 7];
			}
			dRA2000 = dRA;
			dDec2000 = dDec;
			eRA2000 = (eRA = RMS * Math.Sqrt(Math.Abs(array2[0, 0])));
			eDec2000 = (eDec = RMS * Math.Sqrt(Math.Abs(array2[1, 1])));
			ePMRA = RMS * Math.Sqrt(Math.Abs(array2[2, 2]));
			ePMDec = RMS * Math.Sqrt(Math.Abs(array2[3, 3]));
			if (chkHipEpoch.get_Checked())
			{
				dRA2000 -= dPMRA * num4;
				dDec2000 -= dPMDec * num4;
				eRA2000 = Math.Sqrt(eRA * eRA + num4 * ePMRA * num4 * ePMRA);
				eDec2000 = Math.Sqrt(eDec * eDec + num4 * ePMDec * num4 * ePMDec);
			}
			((Control)lbldRA).set_Text(string.Format("{0,5:f3}\" +/-{1,5:f3}\"", dRA2000, eRA2000));
			((Control)lbldDec).set_Text(string.Format("{0,5:f3}\" +/-{1,5:f3}\"", dDec2000, eDec2000));
			((Control)lblPMRA).set_Text(string.Format("{0,5:f4}\" +/-{1,5:f4}\"", dPMRA, RMS * Math.Sqrt(Math.Abs(array2[2, 2]))));
			((Control)lblPMDec).set_Text(string.Format("{0,5:f4}\" +/-{1,5:f4}\"", dPMDec, RMS * Math.Sqrt(Math.Abs(array2[3, 3]))));
			((Control)lbldeltaT).set_Text(string.Format("{0,5:f2}sec +/-{1,5:f2}sec\"", 0.0 - dDeltaT, RMS * Math.Sqrt(Math.Abs(array2[4, 4]))));
			Label obj = lbldRA;
			Label obj2 = lbldDec;
			bool @checked;
			((Control)label1).set_Visible(@checked = chkIncludePosition.get_Checked());
			bool visible;
			((Control)obj2).set_Visible(visible = @checked);
			((Control)obj).set_Visible(visible);
			Label obj3 = lbldeltaT;
			((Control)label2).set_Visible(visible = chkPE.get_Checked());
			((Control)obj3).set_Visible(visible);
			SolvingStar = false;
			ValidSolution = true;
			PlotResults();
		}

		private double ResidualCorrection(int LineNo, double TBase)
		{
			return dRA * Parameters[LineNo].SinPA + dDec * Parameters[LineNo].CosPA + dPMRA * Parameters[LineNo].SinPA * (Parameters[LineNo].T - TBase) + dPMDec * Parameters[LineNo].CosPA * (Parameters[LineNo].T - TBase) + dDeltaT * Parameters[LineNo].rdT;
		}

		private void PlotResults()
		{
			int width = ((Control)picStar).get_Width();
			int height = ((Control)picStar).get_Height();
			Bitmap image = new Bitmap(width, height);
			Graphics graphics = Graphics.FromImage(image);
			if (Settings.Default.GraphicsSmoothed)
			{
				graphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			graphics.Clear(Color.White);
			DrawSolution(graphics, width, height, Printer: false);
			picStar.set_Image((Image)image);
			graphics.Dispose();
		}

		internal void PrintEvent()
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Unknown result type (might be due to invalid IL or missing references)
			//IL_001e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0025: Unknown result type (might be due to invalid IL or missing references)
			//IL_002b: Invalid comparison between Unknown and I4
			PrintDocument printDocument = new PrintDocument();
			PrintDialog val = new PrintDialog();
			val.set_UseEXDialog(true);
			printDocument.DefaultPageSettings.Landscape = false;
			val.set_Document(printDocument);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				printDocument.PrintPage += PrintTheEvent;
				printDocument.Print();
			}
		}

		internal void PrintTheEvent(object sender, PrintPageEventArgs e)
		{
			Graphics graphics = e.Graphics;
			int chartHeight = (int)(0.92 * (double)e.PageBounds.Height);
			int chartWidth = (int)(0.92 * (double)e.PageBounds.Width);
			DrawSolution(graphics, chartWidth, chartHeight, Printer: true);
		}

		private void DrawSolution(Graphics formGraphics, int ChartWidth, int ChartHeight, bool Printer)
		{
			if (PlottingStar)
			{
				return;
			}
			PlottingStar = true;
			float[] array = new float[3];
			string text = "";
			string text2 = "";
			double num = 0.0;
			if (chkHipEpoch.get_Checked())
			{
				num = -8.75;
			}
			bool @checked = chkExpand.get_Checked();
			string text3 = "0.5";
			float num2 = 1f;
			if (@checked)
			{
				text3 = "0.1";
				num2 = 5f;
			}
			Font font = new Font("Times New Roman", 8f);
			new Font("Courier New", 8f, FontStyle.Underline);
			Font font2 = new Font("MSSansSerif", 10f, FontStyle.Bold);
			new Font("MSSansSerif", 18f, FontStyle.Bold);
			Pen pen;
			Pen pen2;
			Brush brush;
			if (BWflag || Printer)
			{
				pen = new Pen(Color.Black, 1f);
				pen2 = new Pen(Color.Black, 1f);
				brush = Brushes.Black;
			}
			else
			{
				pen = new Pen(Color.LightGray, 1f);
				pen2 = new Pen(Color.Cyan, 1f);
				brush = Brushes.White;
			}
			Pen pen3 = new Pen(Color.Green, 1f);
			Pen pen4 = new Pen(Color.Red, 1f);
			new Pen(Color.Blue, 1f);
			pen2.DashPattern = new float[2] { 3f, 4f };
			if (BWflag || Printer)
			{
				formGraphics.Clear(Color.White);
			}
			else
			{
				formGraphics.Clear(Color.Black);
			}
			float num3 = (float)ChartWidth / 95f;
			float num4 = (float)ChartWidth - num3;
			array[0] = (float)ChartHeight / 5f;
			array[1] = (float)ChartHeight / 2f;
			array[2] = (float)ChartHeight - array[0];
			float num5 = (float)ChartHeight / 8f;
			float num6 = (num4 - num3) / 220f;
			float num7 = 200f * num6 + num3;
			text = cmbNames.get_SelectedItem().ToString();
			text2 = StarID.Replace("_", " = ");
			if (text.Length > 0)
			{
				text2 = text2 + " = " + text;
			}
			float num8 = formGraphics.MeasureString(text2, font2).Width / 2f;
			formGraphics.DrawString(text2, font2, brush, (float)ChartWidth / 2f - num8, 10f);
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append("Corrections to XZ80Q: PM RA " + ((Control)lblPMRA).get_Text() + " Dec " + ((Control)lblPMDec).get_Text());
			if (chkIncludePosition.get_Checked())
			{
				stringBuilder.Append(": RA " + ((Control)lbldRA).get_Text() + " Dec " + ((Control)lbldDec).get_Text());
			}
			if (chkPE.get_Checked())
			{
				stringBuilder.Append(": PE " + ((Control)lbldeltaT).get_Text());
			}
			num8 = formGraphics.MeasureString(stringBuilder.ToString(), font).Width / 2f;
			formGraphics.DrawString(stringBuilder.ToString(), font, brush, (float)ChartWidth / 2f - num8, 30f);
			for (int i = 0; i <= 2; i++)
			{
				formGraphics.DrawRectangle(pen, num3, array[i] - num5, num4 - num3, 2f * num5);
				formGraphics.DrawLine(pen, num3, array[i], num4, array[i]);
				for (int j = 10; j < 250; j += 10)
				{
					if (j % 50 == 0)
					{
						formGraphics.DrawLine(pen, num3 + (float)j * num6, array[i] + 5f, num3 + (float)j * num6, array[i] - 5f);
						string text4 = (1800 + j).ToString();
						num8 = formGraphics.MeasureString(text4, font).Width / 2f;
						formGraphics.DrawString(text4, font, brush, num3 + (float)j * num6 - num8, array[i] + num5 - 14f);
					}
					else
					{
						formGraphics.DrawLine(pen, num3 + (float)j * num6, array[i] + 2f, num3 + (float)j * num6, array[i] - 2f);
					}
				}
				formGraphics.DrawLine(pen, num4, array[i] - 0.5f * num5, num4 - 10f, array[i] - 0.5f * num5);
				formGraphics.DrawLine(pen, num4, array[i] + 0.5f * num5, num4 - 10f, array[i] + 0.5f * num5);
				formGraphics.DrawLine(pen, num3, array[i] - 0.5f * num5, num3 + 10f, array[i] - 0.5f * num5);
				formGraphics.DrawString("+" + text3 + "\"", font, brush, num3 + 12f, array[i] - 0.5f * num5 - 6f);
				formGraphics.DrawLine(pen, num3, array[i] + 0.5f * num5, num3 + 10f, array[i] + 0.5f * num5);
				formGraphics.DrawString("-" + text3 + "\"", font, brush, num3 + 12f, array[i] + 0.5f * num5 - 6f);
				string text5 = "";
				if (chkPlotCorrected.get_Checked())
				{
					text5 = " corrected";
				}
				formGraphics.DrawString(Header[i] + text5, font, brush, num3 + 5f, array[i] - num5 + 2f);
				float num9 = 0f;
				float num10 = 0f;
				for (int k = 0; k < Parameters.Count; k++)
				{
					if ((chkHighConfidence.get_Checked() & (Parameters[k].Certainty != 1)) || ((chkRonly.get_Checked() & (Parameters[k].rdT < 0.0)) | (chkDonly.get_Checked() & (Parameters[k].rdT > 0.0))) || (chkNonVisual.get_Checked() & Parameters[k].PEInvolved))
					{
						continue;
					}
					double num11 = Parameters[k].Residual;
					if (chkPlotCorrected.get_Checked())
					{
						num11 -= ResidualCorrection(k, num);
					}
					if (Math.Abs(num11) >= 1.0)
					{
						continue;
					}
					switch (i)
					{
					case 1:
						if (Math.Abs(Parameters[k].SinPA) < 0.6)
						{
							continue;
						}
						num11 *= Parameters[k].SinPA;
						break;
					case 2:
						if (Math.Abs(Parameters[k].CosPA) < 0.6)
						{
							continue;
						}
						num11 *= Parameters[k].CosPA;
						break;
					}
					if (Math.Abs((double)num2 * num11) < 1.0)
					{
						double value = Parameters[k].Residual - ResidualCorrection(k, num);
						double t = Parameters[k].T;
						num9 = (float)((double)num7 + t * (double)num6);
						num10 = (float)((double)array[i] - (double)(num2 * num5) * num11);
						Pen pen5 = pen3;
						if ((Math.Abs(value) > 2.0 * RMS) | (chk1900.get_Checked() && t < -100.0) | (chk1950.get_Checked() & (Parameters[k].T < -50.0)) | (chkGrazes.get_Checked() & (Math.Abs(Parameters[k].rdT) > 0.03)))
						{
							pen5 = pen4;
						}
						formGraphics.DrawEllipse(pen5, num9 - 1f, num10 - 1f, 2f, 2f);
					}
				}
				if (!chkPlotCorrected.get_Checked() && ValidSolution)
				{
					switch (i)
					{
					case 1:
						formGraphics.DrawLine(pen2, num7 + TStart * num6, array[i] - num2 * num5 * (float)(dRA + dPMRA * ((double)TStart - num)), num4, array[i] - num2 * num5 * (float)(dRA + dPMRA * (double)(float)(20.0 - num)));
						break;
					case 2:
						formGraphics.DrawLine(pen2, num7 + TStart * num6, array[i] - num2 * num5 * (float)(dDec + dPMDec * ((double)TStart - num)), num4, array[i] - num2 * num5 * (float)(dDec + dPMDec * (double)(float)(20.0 - num)));
						break;
					}
				}
			}
			PlottingStar = false;
		}

		private void chkPE_CheckedChanged(object sender, EventArgs e)
		{
			SolveStar();
		}

		private void chkIncludePosition_CheckedChanged(object sender, EventArgs e)
		{
			SolveStar();
		}

		private void chk1900_CheckedChanged(object sender, EventArgs e)
		{
			if (chk1900.get_Checked() && chk1950.get_Checked())
			{
				chk1950.set_Checked(false);
			}
			SolveStar();
		}

		private void chk1950_CheckedChanged(object sender, EventArgs e)
		{
			if (chk1950.get_Checked() && chk1900.get_Checked())
			{
				chk1900.set_Checked(false);
			}
			SolveStar();
		}

		private void chkDonly_CheckedChanged(object sender, EventArgs e)
		{
			if (chkDonly.get_Checked() && chkRonly.get_Checked())
			{
				chkRonly.set_Checked(false);
			}
			SolveStar();
		}

		private void chkRonly_CheckedChanged(object sender, EventArgs e)
		{
			if (chkRonly.get_Checked() && chkDonly.get_Checked())
			{
				chkDonly.set_Checked(false);
			}
			SolveStar();
		}

		private void chkGrazes_CheckedChanged(object sender, EventArgs e)
		{
			SolveStar();
		}

		private void chkHighConfidence_CheckedChanged(object sender, EventArgs e)
		{
			SolveStar();
		}

		private void chkIterate_CheckedChanged(object sender, EventArgs e)
		{
			SolveStar();
		}

		private void chkNonVisual_CheckedChanged(object sender, EventArgs e)
		{
			SolveStar();
		}

		private void chkHipEpoch_CheckedChanged(object sender, EventArgs e)
		{
			SolveStar();
		}

		private void chkPlotCorrected_CheckedChanged(object sender, EventArgs e)
		{
			PlotResults();
		}

		private void chkExpand_CheckedChanged(object sender, EventArgs e)
		{
			PlotResults();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picStar.get_Image());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.SaveGraphic(picStar.get_Image(), StarID + " Residuals", Utilities.AppPath + "\\Observations\\Results");
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			PrintEvent();
		}

		private void AnalyseForStarPosition_Resize(object sender, EventArgs e)
		{
			if (((Control)this).get_Width() > 600)
			{
				((Control)picStar).set_Width(((Control)this).get_Width() - 400);
			}
			if (((Control)this).get_Height() > 550)
			{
				((Control)picStar).set_Height(((Control)this).get_Height() - 86);
				((Control)lstResiduals).set_Height(((Control)this).get_Height() - 520);
			}
			PlotResults();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Solve star position");
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void copyToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(GetList());
		}

		private void printToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			Output.PrintText(GetList());
		}

		private void saveToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			Output.SavePredictionText(GetList(), StarID + "_Data", Utilities.AppPath + "\\Observations\\Results");
		}

		private string GetText()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(StarID + "\r\n\r\n");
			stringBuilder.Append("Corrections\r\n");
			stringBuilder.Append("PM in RA : " + ((Control)lblPMRA).get_Text() + "\r\n");
			stringBuilder.Append("PM in Dec: " + ((Control)lblPMDec).get_Text() + "\r\n");
			if (chkIncludePosition.get_Checked())
			{
				stringBuilder.Append("Posn - RA: " + ((Control)lbldRA).get_Text() + "\r\n");
				stringBuilder.Append("Posn -Dec: " + ((Control)lbldDec).get_Text() + "\r\n");
			}
			if (chkPE.get_Checked())
			{
				stringBuilder.Append("PE       : " + ((Control)lbldeltaT).get_Text() + "\r\n");
			}
			stringBuilder.Append("\r\nSolution based on " + ((Control)lblEventCount).get_Text() + " events\r\n");
			if (chkIterate.get_Checked())
			{
				stringBuilder.Append("Iterated solution\r\n");
			}
			return stringBuilder.ToString();
		}

		private string GetList()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(GetText());
			stringBuilder.Append("\r\n\r\nEvent list\r\n");
			for (int i = 0; i < lstResiduals.get_Items().get_Count(); i++)
			{
				stringBuilder.Append(lstResiduals.get_Items().get_Item(i).ToString() + "\r\n");
			}
			return stringBuilder.ToString();
		}

		private void copyToolStripMenuItem2_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(GetText());
		}

		private void printToolStripMenuItem2_Click(object sender, EventArgs e)
		{
			Output.PrintText(GetText());
		}

		private void saveToolStripMenuItem2_Click(object sender, EventArgs e)
		{
			Output.SavePredictionText(GetText(), StarID + "_Soln", Utilities.AppPath + "\\Observations\\Results");
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
			panelStar = new Panel();
			txtPlanet = new TextBox();
			label15 = new Label();
			pbarStar = new ProgressBar();
			txtXZ = new TextBox();
			cmbNames = new ComboBox();
			label6 = new Label();
			label5 = new Label();
			label4 = new Label();
			label3 = new Label();
			txtZC = new TextBox();
			txtSAO = new TextBox();
			cmdRead = new Button();
			menuStrip1 = new MenuStrip();
			withSolutionToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem2 = new ToolStripMenuItem();
			printToolStripMenuItem2 = new ToolStripMenuItem();
			saveToolStripMenuItem2 = new ToolStripMenuItem();
			withResidualListToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem1 = new ToolStripMenuItem();
			printToolStripMenuItem1 = new ToolStripMenuItem();
			saveToolStripMenuItem1 = new ToolStripMenuItem();
			withImageToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			nameOfStarListToolStripMenuItem = new ToolStripMenuItem();
			allNamesToolStripMenuItem = new ToolStripMenuItem();
			properNamesToolStripMenuItem = new ToolStripMenuItem();
			bayerLettersToolStripMenuItem = new ToolStripMenuItem();
			magnitudeBrighterThan2ToolStripMenuItem = new ToolStripMenuItem();
			magnitudeBrighterThan3ToolStripMenuItem = new ToolStripMenuItem();
			magnitudeBrighterThan4ToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			lstResiduals = new ListBox();
			cmdSolve = new Button();
			lbldRA = new Label();
			label1 = new Label();
			lbldDec = new Label();
			lblPMDec = new Label();
			lblPM = new Label();
			lblPMRA = new Label();
			label2 = new Label();
			lbldeltaT = new Label();
			chkPE = new CheckBox();
			chkIncludePosition = new CheckBox();
			groupBox1 = new GroupBox();
			groupBox2 = new GroupBox();
			chkExpand = new CheckBox();
			chkPlotCorrected = new CheckBox();
			label9 = new Label();
			lblEventCount = new Label();
			label7 = new Label();
			label8 = new Label();
			lblError = new Label();
			groupBox3 = new GroupBox();
			chkHipEpoch = new CheckBox();
			chkNonVisual = new CheckBox();
			chk1950 = new CheckBox();
			chkHighConfidence = new CheckBox();
			chkIterate = new CheckBox();
			chkRonly = new CheckBox();
			chkGrazes = new CheckBox();
			chkDonly = new CheckBox();
			chk1900 = new CheckBox();
			picStar = new PictureBox();
			((Control)panelStar).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((Control)groupBox1).SuspendLayout();
			((Control)groupBox2).SuspendLayout();
			((Control)groupBox3).SuspendLayout();
			((ISupportInitialize)picStar).BeginInit();
			((Control)this).SuspendLayout();
			((Control)panelStar).get_Controls().Add((Control)(object)txtPlanet);
			((Control)panelStar).get_Controls().Add((Control)(object)label15);
			((Control)panelStar).get_Controls().Add((Control)(object)pbarStar);
			((Control)panelStar).get_Controls().Add((Control)(object)txtXZ);
			((Control)panelStar).get_Controls().Add((Control)(object)cmbNames);
			((Control)panelStar).get_Controls().Add((Control)(object)label6);
			((Control)panelStar).get_Controls().Add((Control)(object)label5);
			((Control)panelStar).get_Controls().Add((Control)(object)label4);
			((Control)panelStar).get_Controls().Add((Control)(object)label3);
			((Control)panelStar).get_Controls().Add((Control)(object)txtZC);
			((Control)panelStar).get_Controls().Add((Control)(object)txtSAO);
			((Control)panelStar).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)panelStar).set_Location(new Point(82, 19));
			((Control)panelStar).set_Name("panelStar");
			((Control)panelStar).set_Size(new Size(197, 103));
			((Control)panelStar).set_TabIndex(0);
			((Control)txtPlanet).set_Location(new Point(164, 73));
			((Control)txtPlanet).set_Name("txtPlanet");
			((Control)txtPlanet).set_Size(new Size(19, 20));
			((Control)txtPlanet).set_TabIndex(10);
			((Control)txtPlanet).add_TextChanged((EventHandler)txtPlanet_TextChanged);
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Location(new Point(156, 57));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(37, 13));
			((Control)label15).set_TabIndex(6);
			((Control)label15).set_Text("Planet");
			((Control)pbarStar).set_Location(new Point(4, 47));
			((Control)pbarStar).set_Name("pbarStar");
			((Control)pbarStar).set_Size(new Size(186, 8));
			((Control)pbarStar).set_TabIndex(2);
			((Control)pbarStar).set_Visible(false);
			((Control)txtXZ).set_Location(new Point(110, 73));
			((Control)txtXZ).set_Name("txtXZ");
			((Control)txtXZ).set_Size(new Size(46, 20));
			((Control)txtXZ).set_TabIndex(9);
			((Control)txtXZ).add_TextChanged((EventHandler)txtXZ_TextChanged);
			cmbNames.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbNames).set_FormattingEnabled(true);
			((Control)cmbNames).set_Location(new Point(4, 22));
			cmbNames.set_MaxDropDownItems(20);
			((Control)cmbNames).set_Name("cmbNames");
			((Control)cmbNames).set_Size(new Size(186, 21));
			((Control)cmbNames).set_TabIndex(1);
			cmbNames.add_SelectedIndexChanged((EventHandler)cmbNames_SelectedIndexChanged);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(44, 7));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(67, 13));
			((Control)label6).set_TabIndex(0);
			((Control)label6).set_Text("Name of star");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(18, 58));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(21, 13));
			((Control)label5).set_TabIndex(3);
			((Control)label5).set_Text("ZC");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(63, 58));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(29, 13));
			((Control)label4).set_TabIndex(4);
			((Control)label4).set_Text("SAO");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(120, 58));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(21, 13));
			((Control)label3).set_TabIndex(5);
			((Control)label3).set_Text("XZ");
			((Control)txtZC).set_Location(new Point(8, 73));
			((Control)txtZC).set_Name("txtZC");
			((Control)txtZC).set_Size(new Size(40, 20));
			((Control)txtZC).set_TabIndex(7);
			((Control)txtZC).add_TextChanged((EventHandler)txtZC_TextChanged);
			((Control)txtSAO).set_Location(new Point(56, 73));
			((Control)txtSAO).set_Name("txtSAO");
			((Control)txtSAO).set_Size(new Size(46, 20));
			((Control)txtSAO).set_TabIndex(8);
			((Control)txtSAO).add_TextChanged((EventHandler)txtSAO_TextChanged);
			((Control)cmdRead).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdRead).set_Location(new Point(138, 128));
			((Control)cmdRead).set_Name("cmdRead");
			((Control)cmdRead).set_Size(new Size(76, 30));
			((Control)cmdRead).set_TabIndex(1);
			((Control)cmdRead).set_Text("Read");
			((ButtonBase)cmdRead).set_UseVisualStyleBackColor(true);
			((Control)cmdRead).add_Click((EventHandler)cmdRead_Click);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)withSolutionToolStripMenuItem,
				(ToolStripItem)withResidualListToolStripMenuItem,
				(ToolStripItem)withImageToolStripMenuItem,
				(ToolStripItem)nameOfStarListToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(865, 24));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withSolutionToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)copyToolStripMenuItem2,
				(ToolStripItem)printToolStripMenuItem2,
				(ToolStripItem)saveToolStripMenuItem2
			});
			((ToolStripItem)withSolutionToolStripMenuItem).set_Name("withSolutionToolStripMenuItem");
			((ToolStripItem)withSolutionToolStripMenuItem).set_Size(new Size(110, 20));
			((ToolStripItem)withSolutionToolStripMenuItem).set_Text("with Solution....   ");
			((ToolStripItem)copyToolStripMenuItem2).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem2).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem2).set_Name("copyToolStripMenuItem2");
			((ToolStripItem)copyToolStripMenuItem2).set_Size(new Size(102, 22));
			((ToolStripItem)copyToolStripMenuItem2).set_Text("Copy");
			((ToolStripItem)copyToolStripMenuItem2).add_Click((EventHandler)copyToolStripMenuItem2_Click);
			((ToolStripItem)printToolStripMenuItem2).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem2).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem2).set_Name("printToolStripMenuItem2");
			((ToolStripItem)printToolStripMenuItem2).set_Size(new Size(102, 22));
			((ToolStripItem)printToolStripMenuItem2).set_Text("Print");
			((ToolStripItem)printToolStripMenuItem2).add_Click((EventHandler)printToolStripMenuItem2_Click);
			((ToolStripItem)saveToolStripMenuItem2).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem2).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem2).set_Name("saveToolStripMenuItem2");
			((ToolStripItem)saveToolStripMenuItem2).set_Size(new Size(102, 22));
			((ToolStripItem)saveToolStripMenuItem2).set_Text("Save");
			((ToolStripItem)saveToolStripMenuItem2).add_Click((EventHandler)saveToolStripMenuItem2_Click);
			((ToolStripDropDownItem)withResidualListToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)copyToolStripMenuItem1,
				(ToolStripItem)printToolStripMenuItem1,
				(ToolStripItem)saveToolStripMenuItem1
			});
			((ToolStripItem)withResidualListToolStripMenuItem).set_Name("withResidualListToolStripMenuItem");
			((ToolStripItem)withResidualListToolStripMenuItem).set_Size(new Size(128, 20));
			((ToolStripItem)withResidualListToolStripMenuItem).set_Text("with Residual list...    ");
			((ToolStripItem)copyToolStripMenuItem1).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem1).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem1).set_Name("copyToolStripMenuItem1");
			((ToolStripItem)copyToolStripMenuItem1).set_Size(new Size(102, 22));
			((ToolStripItem)copyToolStripMenuItem1).set_Text("Copy");
			((ToolStripItem)copyToolStripMenuItem1).add_Click((EventHandler)copyToolStripMenuItem1_Click);
			((ToolStripItem)printToolStripMenuItem1).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem1).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem1).set_Name("printToolStripMenuItem1");
			((ToolStripItem)printToolStripMenuItem1).set_Size(new Size(102, 22));
			((ToolStripItem)printToolStripMenuItem1).set_Text("Print");
			((ToolStripItem)printToolStripMenuItem1).add_Click((EventHandler)printToolStripMenuItem1_Click);
			((ToolStripItem)saveToolStripMenuItem1).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem1).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem1).set_Name("saveToolStripMenuItem1");
			((ToolStripItem)saveToolStripMenuItem1).set_Size(new Size(102, 22));
			((ToolStripItem)saveToolStripMenuItem1).set_Text("Save");
			((ToolStripItem)saveToolStripMenuItem1).add_Click((EventHandler)saveToolStripMenuItem1_Click);
			((ToolStripDropDownItem)withImageToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withImageToolStripMenuItem).set_Name("withImageToolStripMenuItem");
			((ToolStripItem)withImageToolStripMenuItem).set_Size(new Size(96, 20));
			((ToolStripItem)withImageToolStripMenuItem).set_Text("with Image...   ");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(102, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(102, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(102, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripDropDownItem)nameOfStarListToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)allNamesToolStripMenuItem,
				(ToolStripItem)properNamesToolStripMenuItem,
				(ToolStripItem)bayerLettersToolStripMenuItem,
				(ToolStripItem)magnitudeBrighterThan2ToolStripMenuItem,
				(ToolStripItem)magnitudeBrighterThan3ToolStripMenuItem,
				(ToolStripItem)magnitudeBrighterThan4ToolStripMenuItem
			});
			((ToolStripItem)nameOfStarListToolStripMenuItem).set_Name("nameOfStarListToolStripMenuItem");
			((ToolStripItem)nameOfStarListToolStripMenuItem).set_Size(new Size(133, 20));
			((ToolStripItem)nameOfStarListToolStripMenuItem).set_Text("'Name of Star' list....   ");
			((ToolStripItem)allNamesToolStripMenuItem).set_Name("allNamesToolStripMenuItem");
			((ToolStripItem)allNamesToolStripMenuItem).set_Size(new Size(213, 22));
			((ToolStripItem)allNamesToolStripMenuItem).set_Text("All names");
			((ToolStripItem)allNamesToolStripMenuItem).add_Click((EventHandler)allNamesToolStripMenuItem_Click);
			((ToolStripItem)properNamesToolStripMenuItem).set_Name("properNamesToolStripMenuItem");
			((ToolStripItem)properNamesToolStripMenuItem).set_Size(new Size(213, 22));
			((ToolStripItem)properNamesToolStripMenuItem).set_Text("Proper names");
			((ToolStripItem)properNamesToolStripMenuItem).add_Click((EventHandler)properNamesToolStripMenuItem_Click);
			((ToolStripItem)bayerLettersToolStripMenuItem).set_Name("bayerLettersToolStripMenuItem");
			((ToolStripItem)bayerLettersToolStripMenuItem).set_Size(new Size(213, 22));
			((ToolStripItem)bayerLettersToolStripMenuItem).set_Text("Bayer letters");
			((ToolStripItem)bayerLettersToolStripMenuItem).add_Click((EventHandler)bayerLettersToolStripMenuItem_Click);
			((ToolStripItem)magnitudeBrighterThan2ToolStripMenuItem).set_Name("magnitudeBrighterThan2ToolStripMenuItem");
			((ToolStripItem)magnitudeBrighterThan2ToolStripMenuItem).set_Size(new Size(213, 22));
			((ToolStripItem)magnitudeBrighterThan2ToolStripMenuItem).set_Text("Magnitude brighter than 2");
			((ToolStripItem)magnitudeBrighterThan2ToolStripMenuItem).add_Click((EventHandler)magnitudeBrighterThan2ToolStripMenuItem_Click);
			((ToolStripItem)magnitudeBrighterThan3ToolStripMenuItem).set_Name("magnitudeBrighterThan3ToolStripMenuItem");
			((ToolStripItem)magnitudeBrighterThan3ToolStripMenuItem).set_Size(new Size(213, 22));
			((ToolStripItem)magnitudeBrighterThan3ToolStripMenuItem).set_Text("Magnitude brighter than 3");
			((ToolStripItem)magnitudeBrighterThan3ToolStripMenuItem).add_Click((EventHandler)magnitudeBrighterThan3ToolStripMenuItem_Click);
			((ToolStripItem)magnitudeBrighterThan4ToolStripMenuItem).set_Name("magnitudeBrighterThan4ToolStripMenuItem");
			((ToolStripItem)magnitudeBrighterThan4ToolStripMenuItem).set_Size(new Size(213, 22));
			((ToolStripItem)magnitudeBrighterThan4ToolStripMenuItem).set_Text("Magnitude brighter than 4");
			((ToolStripItem)magnitudeBrighterThan4ToolStripMenuItem).add_Click((EventHandler)magnitudeBrighterThan4ToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(62, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit   ");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)lstResiduals).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstResiduals).set_FormattingEnabled(true);
			lstResiduals.set_ItemHeight(14);
			((Control)lstResiduals).set_Location(new Point(5, 474));
			((Control)lstResiduals).set_Name("lstResiduals");
			((Control)lstResiduals).set_Size(new Size(361, 130));
			((Control)lstResiduals).set_TabIndex(4);
			((Control)cmdSolve).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSolve).set_Location(new Point(288, 50));
			((Control)cmdSolve).set_Name("cmdSolve");
			((Control)cmdSolve).set_Size(new Size(59, 30));
			((Control)cmdSolve).set_TabIndex(10);
			((Control)cmdSolve).set_Text("Solve");
			((ButtonBase)cmdSolve).set_UseVisualStyleBackColor(true);
			((Control)cmdSolve).add_Click((EventHandler)cmdSolve_Click);
			((Control)lbldRA).set_AutoSize(true);
			((Control)lbldRA).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lbldRA).set_Location(new Point(222, 49));
			((Control)lbldRA).set_Name("lbldRA");
			((Control)lbldRA).set_Size(new Size(28, 13));
			((Control)lbldRA).set_TabIndex(6);
			((Control)lbldRA).set_Text("dRA");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(222, 36));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(98, 13));
			((Control)label1).set_TabIndex(5);
			((Control)label1).set_Text("Position (J2000)");
			((Control)lbldDec).set_AutoSize(true);
			((Control)lbldDec).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lbldDec).set_Location(new Point(222, 62));
			((Control)lbldDec).set_Name("lbldDec");
			((Control)lbldDec).set_Size(new Size(33, 13));
			((Control)lbldDec).set_TabIndex(7);
			((Control)lbldDec).set_Text("dDec");
			((Control)lblPMDec).set_AutoSize(true);
			((Control)lblPMDec).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblPMDec).set_Location(new Point(49, 62));
			((Control)lblPMDec).set_Name("lblPMDec");
			((Control)lblPMDec).set_Size(new Size(41, 13));
			((Control)lblPMDec).set_TabIndex(4);
			((Control)lblPMDec).set_Text("pmDec");
			((Control)lblPM).set_AutoSize(true);
			((Control)lblPM).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblPM).set_Location(new Point(49, 36));
			((Control)lblPM).set_Name("lblPM");
			((Control)lblPM).set_Size(new Size(68, 13));
			((Control)lblPM).set_TabIndex(0);
			((Control)lblPM).set_Text("Annual PM");
			((Control)lblPMRA).set_AutoSize(true);
			((Control)lblPMRA).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblPMRA).set_Location(new Point(49, 49));
			((Control)lblPMRA).set_Name("lblPMRA");
			((Control)lblPMRA).set_Size(new Size(36, 13));
			((Control)lblPMRA).set_TabIndex(2);
			((Control)lblPMRA).set_Text("pmRA");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(156, 83));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(56, 13));
			((Control)label2).set_TabIndex(10);
			((Control)label2).set_Text("PE corrn");
			((Control)lbldeltaT).set_AutoSize(true);
			((Control)lbldeltaT).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lbldeltaT).set_Location(new Point(217, 83));
			((Control)lbldeltaT).set_Name("lbldeltaT");
			((Control)lbldeltaT).set_Size(new Size(71, 13));
			((Control)lbldeltaT).set_TabIndex(11);
			((Control)lbldeltaT).set_Text("PE correction");
			((Control)chkPE).set_AutoSize(true);
			((Control)chkPE).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkPE).set_Location(new Point(156, 87));
			((Control)chkPE).set_Name("chkPE");
			((Control)chkPE).set_Size(new Size(128, 17));
			((Control)chkPE).set_TabIndex(9);
			((Control)chkPE).set_Text("Include PE correction");
			((ButtonBase)chkPE).set_UseVisualStyleBackColor(true);
			chkPE.add_CheckedChanged((EventHandler)chkPE_CheckedChanged);
			((Control)chkIncludePosition).set_AutoSize(true);
			((Control)chkIncludePosition).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkIncludePosition).set_Location(new Point(156, 70));
			((Control)chkIncludePosition).set_Name("chkIncludePosition");
			((Control)chkIncludePosition).set_Size(new Size(100, 17));
			((Control)chkIncludePosition).set_TabIndex(8);
			((Control)chkIncludePosition).set_Text("Include position");
			((ButtonBase)chkIncludePosition).set_UseVisualStyleBackColor(true);
			chkIncludePosition.add_CheckedChanged((EventHandler)chkIncludePosition_CheckedChanged);
			((Control)groupBox1).get_Controls().Add((Control)(object)panelStar);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdRead);
			((Control)groupBox1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox1).set_Location(new Point(5, 27));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(361, 184));
			((Control)groupBox1).set_TabIndex(1);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Get residuals");
			((Control)groupBox2).get_Controls().Add((Control)(object)chkExpand);
			((Control)groupBox2).get_Controls().Add((Control)(object)chkPlotCorrected);
			((Control)groupBox2).get_Controls().Add((Control)(object)label9);
			((Control)groupBox2).get_Controls().Add((Control)(object)lblEventCount);
			((Control)groupBox2).get_Controls().Add((Control)(object)label7);
			((Control)groupBox2).get_Controls().Add((Control)(object)label8);
			((Control)groupBox2).get_Controls().Add((Control)(object)lblPM);
			((Control)groupBox2).get_Controls().Add((Control)(object)lblPMRA);
			((Control)groupBox2).get_Controls().Add((Control)(object)lblPMDec);
			((Control)groupBox2).get_Controls().Add((Control)(object)lbldDec);
			((Control)groupBox2).get_Controls().Add((Control)(object)label2);
			((Control)groupBox2).get_Controls().Add((Control)(object)lbldeltaT);
			((Control)groupBox2).get_Controls().Add((Control)(object)lbldRA);
			((Control)groupBox2).get_Controls().Add((Control)(object)label1);
			((Control)groupBox2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox2).set_Location(new Point(5, 357));
			((Control)groupBox2).set_Name("groupBox2");
			((Control)groupBox2).set_Size(new Size(361, 109));
			((Control)groupBox2).set_TabIndex(3);
			groupBox2.set_TabStop(false);
			((Control)groupBox2).set_Text("Solution");
			((Control)chkExpand).set_AutoSize(true);
			((Control)chkExpand).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkExpand).set_Location(new Point(206, 16));
			((Control)chkExpand).set_Name("chkExpand");
			((Control)chkExpand).set_Size(new Size(96, 17));
			((Control)chkExpand).set_TabIndex(12);
			((Control)chkExpand).set_Text("Expand plot x5");
			((ButtonBase)chkExpand).set_UseVisualStyleBackColor(true);
			chkExpand.add_CheckedChanged((EventHandler)chkExpand_CheckedChanged);
			((Control)chkPlotCorrected).set_AutoSize(true);
			((Control)chkPlotCorrected).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkPlotCorrected).set_Location(new Point(16, 16));
			((Control)chkPlotCorrected).set_Name("chkPlotCorrected");
			((Control)chkPlotCorrected).set_Size(new Size(150, 17));
			((Control)chkPlotCorrected).set_TabIndex(11);
			((Control)chkPlotCorrected).set_Text("Plot residuals as corrected");
			((ButtonBase)chkPlotCorrected).set_UseVisualStyleBackColor(true);
			chkPlotCorrected.add_CheckedChanged((EventHandler)chkPlotCorrected_CheckedChanged);
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(6, 83));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(77, 13));
			((Control)label9).set_TabIndex(8);
			((Control)label9).set_Text("Events used");
			((Control)lblEventCount).set_AutoSize(true);
			((Control)lblEventCount).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblEventCount).set_Location(new Point(83, 83));
			((Control)lblEventCount).set_Name("lblEventCount");
			((Control)lblEventCount).set_Size(new Size(13, 13));
			((Control)lblEventCount).set_TabIndex(9);
			((Control)lblEventCount).set_Text("0");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(13, 62));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(30, 13));
			((Control)label7).set_TabIndex(3);
			((Control)label7).set_Text("Dec");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(19, 49));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(24, 13));
			((Control)label8).set_TabIndex(1);
			((Control)label8).set_Text("RA");
			((Control)lblError).set_AutoSize(true);
			((Control)lblError).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblError).set_ForeColor(Color.Red);
			((Control)lblError).set_Location(new Point(23, 458));
			((Control)lblError).set_Name("lblError");
			((Control)lblError).set_Size(new Size(315, 17));
			((Control)lblError).set_TabIndex(5);
			((Control)lblError).set_Text("Insufficient points, or date range too short");
			((Control)lblError).set_Visible(false);
			((Control)groupBox3).get_Controls().Add((Control)(object)chkHipEpoch);
			((Control)groupBox3).get_Controls().Add((Control)(object)chkNonVisual);
			((Control)groupBox3).get_Controls().Add((Control)(object)chk1950);
			((Control)groupBox3).get_Controls().Add((Control)(object)chkHighConfidence);
			((Control)groupBox3).get_Controls().Add((Control)(object)chkIterate);
			((Control)groupBox3).get_Controls().Add((Control)(object)chkRonly);
			((Control)groupBox3).get_Controls().Add((Control)(object)chkGrazes);
			((Control)groupBox3).get_Controls().Add((Control)(object)chkDonly);
			((Control)groupBox3).get_Controls().Add((Control)(object)chk1900);
			((Control)groupBox3).get_Controls().Add((Control)(object)chkIncludePosition);
			((Control)groupBox3).get_Controls().Add((Control)(object)chkPE);
			((Control)groupBox3).get_Controls().Add((Control)(object)cmdSolve);
			((Control)groupBox3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox3).set_Location(new Point(5, 219));
			((Control)groupBox3).set_Name("groupBox3");
			((Control)groupBox3).set_Size(new Size(361, 130));
			((Control)groupBox3).set_TabIndex(2);
			groupBox3.set_TabStop(false);
			((Control)groupBox3).set_Text("Solution parameters");
			((Control)chkHipEpoch).set_AutoSize(true);
			((Control)chkHipEpoch).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkHipEpoch).set_Location(new Point(16, 104));
			((Control)chkHipEpoch).set_Name("chkHipEpoch");
			((Control)chkHipEpoch).set_Size(new Size(127, 17));
			((Control)chkHipEpoch).set_TabIndex(11);
			((Control)chkHipEpoch).set_Text("Use 1991.25 as base");
			((ButtonBase)chkHipEpoch).set_UseVisualStyleBackColor(true);
			chkHipEpoch.add_CheckedChanged((EventHandler)chkHipEpoch_CheckedChanged);
			((Control)chkNonVisual).set_AutoSize(true);
			((Control)chkNonVisual).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkNonVisual).set_Location(new Point(16, 87));
			((Control)chkNonVisual).set_Name("chkNonVisual");
			((Control)chkNonVisual).set_Size(new Size(116, 17));
			((Control)chkNonVisual).set_TabIndex(4);
			((Control)chkNonVisual).set_Text("use non-visual only");
			((ButtonBase)chkNonVisual).set_UseVisualStyleBackColor(true);
			chkNonVisual.add_CheckedChanged((EventHandler)chkNonVisual_CheckedChanged);
			((Control)chk1950).set_AutoSize(true);
			((Control)chk1950).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chk1950).set_Location(new Point(156, 53));
			((Control)chk1950).set_Name("chk1950");
			((Control)chk1950).set_Size(new Size(98, 17));
			((Control)chk1950).set_TabIndex(7);
			((Control)chk1950).set_Text("Only after 1950");
			((ButtonBase)chk1950).set_UseVisualStyleBackColor(true);
			chk1950.add_CheckedChanged((EventHandler)chk1950_CheckedChanged);
			((Control)chkHighConfidence).set_AutoSize(true);
			chkHighConfidence.set_Checked(true);
			chkHighConfidence.set_CheckState((CheckState)1);
			((Control)chkHighConfidence).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkHighConfidence).set_Location(new Point(16, 70));
			((Control)chkHighConfidence).set_Name("chkHighConfidence");
			((Control)chkHighConfidence).set_Size(new Size(126, 17));
			((Control)chkHighConfidence).set_TabIndex(3);
			((Control)chkHighConfidence).set_Text("only High confidence");
			((ButtonBase)chkHighConfidence).set_UseVisualStyleBackColor(true);
			chkHighConfidence.add_CheckedChanged((EventHandler)chkHighConfidence_CheckedChanged);
			((Control)chkIterate).set_AutoSize(true);
			chkIterate.set_Checked(true);
			chkIterate.set_CheckState((CheckState)1);
			((Control)chkIterate).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkIterate).set_Location(new Point(156, 19));
			((Control)chkIterate).set_Name("chkIterate");
			((Control)chkIterate).set_Size(new Size(56, 17));
			((Control)chkIterate).set_TabIndex(5);
			((Control)chkIterate).set_Text("Iterate");
			((ButtonBase)chkIterate).set_UseVisualStyleBackColor(true);
			chkIterate.add_CheckedChanged((EventHandler)chkIterate_CheckedChanged);
			((Control)chkRonly).set_AutoSize(true);
			((Control)chkRonly).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkRonly).set_Location(new Point(16, 36));
			((Control)chkRonly).set_Name("chkRonly");
			((Control)chkRonly).set_Size(new Size(83, 17));
			((Control)chkRonly).set_TabIndex(1);
			((Control)chkRonly).set_Text("use R's only");
			((ButtonBase)chkRonly).set_UseVisualStyleBackColor(true);
			chkRonly.add_CheckedChanged((EventHandler)chkRonly_CheckedChanged);
			((Control)chkGrazes).set_AutoSize(true);
			((Control)chkGrazes).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkGrazes).set_Location(new Point(16, 53));
			((Control)chkGrazes).set_Name("chkGrazes");
			((Control)chkGrazes).set_Size(new Size(99, 17));
			((Control)chkGrazes).set_TabIndex(2);
			((Control)chkGrazes).set_Text("use grazes only");
			((ButtonBase)chkGrazes).set_UseVisualStyleBackColor(true);
			chkGrazes.add_CheckedChanged((EventHandler)chkGrazes_CheckedChanged);
			((Control)chkDonly).set_AutoSize(true);
			((Control)chkDonly).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkDonly).set_Location(new Point(16, 19));
			((Control)chkDonly).set_Name("chkDonly");
			((Control)chkDonly).set_Size(new Size(83, 17));
			((Control)chkDonly).set_TabIndex(0);
			((Control)chkDonly).set_Text("use D's only");
			((ButtonBase)chkDonly).set_UseVisualStyleBackColor(true);
			chkDonly.add_CheckedChanged((EventHandler)chkDonly_CheckedChanged);
			((Control)chk1900).set_AutoSize(true);
			((Control)chk1900).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chk1900).set_Location(new Point(156, 36));
			((Control)chk1900).set_Name("chk1900");
			((Control)chk1900).set_Size(new Size(98, 17));
			((Control)chk1900).set_TabIndex(6);
			((Control)chk1900).set_Text("Only after 1900");
			((ButtonBase)chk1900).set_UseVisualStyleBackColor(true);
			chk1900.add_CheckedChanged((EventHandler)chk1900_CheckedChanged);
			((Control)picStar).set_BackColor(Color.Black);
			((Control)picStar).set_Location(new Point(373, 37));
			((Control)picStar).set_Name("picStar");
			((Control)picStar).set_Size(new Size(488, 579));
			picStar.set_TabIndex(40);
			picStar.set_TabStop(false);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(865, 620));
			((Control)this).get_Controls().Add((Control)(object)lblError);
			((Control)this).get_Controls().Add((Control)(object)picStar);
			((Control)this).get_Controls().Add((Control)(object)groupBox3);
			((Control)this).get_Controls().Add((Control)(object)groupBox2);
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Control)this).get_Controls().Add((Control)(object)lstResiduals);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("AnalyseForStarPosition");
			((Control)this).set_Text("Analyse for star position");
			((Form)this).add_Load((EventHandler)AnalyseForStarPosition_Load);
			((Control)this).add_Resize((EventHandler)AnalyseForStarPosition_Resize);
			((Control)panelStar).ResumeLayout(false);
			((Control)panelStar).PerformLayout();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox2).ResumeLayout(false);
			((Control)groupBox2).PerformLayout();
			((Control)groupBox3).ResumeLayout(false);
			((Control)groupBox3).PerformLayout();
			((ISupportInitialize)picStar).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
