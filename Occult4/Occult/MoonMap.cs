using System;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Printing;
using System.IO;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class MoonMap : Form
	{
		private bool FormCreated;

		private static string AppPath;

		private const double Radian = 180.0 / Math.PI;

		private static int NumOfEventsToPlot = 1;

		internal static bool MultiSite = false;

		private bool SwapBrightDark;

		private IContainer components;

		private MenuStrip menuStrip1;

		private PictureBox picMoon;

		private ToolStripMenuItem withMapToolStripMenuItem;

		private ToolStripMenuItem bWToolStripMenuItem;

		private ToolStripMenuItem mirrorImageToolStripMenuItem;

		private ToolStripMenuItem showCraterNamesToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ContextMenuStrip contextMenuStrip1;

		private ToolStripMenuItem plotSelectedStarOnlyToolStripMenuItem;

		private ToolStripMenuItem plot10StarsFromSelectedToolStripMenuItem;

		private ToolStripMenuItem plot20StarsFromSelectedToolStripMenuItem;

		private ToolStripMenuItem plotAllStarsForTheDayToolStripMenuItem;

		private Label label1;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem showPathTagsToolStripMenuItem;

		private ToolStripMenuItem southAtTopToolStripMenuItem;

		private ToolStripMenuItem plot5StarsStartingAtSelectedToolStripMenuItem;

		public MoonMap()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
			bWToolStripMenuItem.set_Checked(Settings.Default.BWFlag);
			if (bWToolStripMenuItem.get_Checked())
			{
				((ToolStripItem)bWToolStripMenuItem).set_Text("Plot in Colour");
			}
			mirrorImageToolStripMenuItem.set_Checked(Settings.Default.MoonMapMirror);
			((Control)this).set_Width(Settings.Default.MoonMapWidth);
			((Control)this).set_Height(Settings.Default.MoonMapHeight);
		}

		private void MoonMap_Resize(object sender, EventArgs e)
		{
			if (!((((Control)this).get_Width() < 100) | (((Control)this).get_Height() < 100)))
			{
				((Control)picMoon).set_Width(((Control)this).get_Width() - 8);
				((Control)picMoon).set_Height(((Control)this).get_Height() - 61);
				if (FormCreated)
				{
					DrawMoon();
				}
			}
		}

		internal void DrawMoon()
		{
			int width = ((Control)picMoon).get_Width();
			int height = ((Control)picMoon).get_Height();
			Bitmap image = new Bitmap(width, height);
			Graphics graphics = Graphics.FromImage(image);
			PlotMoon(graphics, width, height, Printer: false, MultiSite);
			picMoon.set_Image((Image)image);
			graphics.Dispose();
		}

		public void PrintMoon()
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0022: Unknown result type (might be due to invalid IL or missing references)
			//IL_0029: Unknown result type (might be due to invalid IL or missing references)
			//IL_002f: Invalid comparison between Unknown and I4
			PrintDocument printDocument = new PrintDocument();
			PrintDialog val = new PrintDialog();
			val.set_UseEXDialog(true);
			printDocument.DefaultPageSettings.Landscape = MultiSite;
			val.set_Document(printDocument);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				printDocument.PrintPage += PrintMoonMap;
				printDocument.Print();
			}
		}

		public void PrintPreviewMoon()
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_000c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_002f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0036: Unknown result type (might be due to invalid IL or missing references)
			//IL_003c: Invalid comparison between Unknown and I4
			//IL_0051: Unknown result type (might be due to invalid IL or missing references)
			PrintPreviewDialog val = new PrintPreviewDialog();
			PrintDocument printDocument = new PrintDocument();
			PrintDialog val2 = new PrintDialog();
			val2.set_UseEXDialog(true);
			printDocument.DefaultPageSettings.Landscape = MultiSite;
			val.set_Document(printDocument);
			val2.set_Document(printDocument);
			if ((int)((CommonDialog)val2).ShowDialog() == 1)
			{
				printDocument.PrintPage += PrintMoonMap;
				((Form)val).ShowDialog();
			}
		}

		private void PrintMoonMap(object sender, PrintPageEventArgs e)
		{
			Graphics graphics = e.Graphics;
			int num;
			int num2;
			if (MultiSite)
			{
				num = (int)(0.92 * (double)e.PageBounds.Height);
				num2 = (int)(0.92 * (double)e.PageBounds.Width);
			}
			else
			{
				num = (int)(0.92 * (double)e.PageBounds.Height);
				num2 = (int)(0.92 * (double)e.PageBounds.Width);
			}
			PlotMoon(graphics, num2, num, Printer: true, MultiSite);
		}

		private static void PlotMoon(Graphics formGraphics, float ChartWidth, float ChartHeight, bool Printer, bool MultipleSites)
		{
			float num = 0f;
			float num2 = 0f;
			float x = 0f;
			float y = 0f;
			float x2 = 0f;
			float y2 = 0f;
			int num3 = 1;
			bool @checked = LunarOccultations.LunarPrediction.chkFilterOutput.get_Checked();
			bool flag = @checked & Settings.Default.LunarFilter_TimeRange;
			double num4 = (double)Settings.Default.LunarFilter_Tstart;
			double num5 = (double)Settings.Default.LunarFilter_Tend;
			if (num5 < num4)
			{
				num5 += 24.0;
			}
			bool flag2 = @checked & Settings.Default.LunarFilter_NoDaytime;
			bool flag3 = @checked & Settings.Default.LunarFilter_NoBrightLimb;
			bool flag4 = @checked & Settings.Default.LunarFilter_ExcludeGrazeMessage;
			bool flag5 = true;
			if (Settings.Default.GraphicsSmoothed)
			{
				formGraphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			if (MultipleSites)
			{
				((Control)LunarOccultations.MoonMap.contextMenuStrip1).set_Enabled(false);
				((Control)LunarOccultations.MoonMap.label1).set_Visible(false);
			}
			else
			{
				((Control)LunarOccultations.MoonMap.contextMenuStrip1).set_Enabled(true);
				((Control)LunarOccultations.MoonMap.label1).set_Visible(true);
			}
			float num6 = ChartWidth / 2f;
			float num7 = ChartHeight / 2f;
			float num8 = ChartWidth / 2.5f;
			if (num7 < num6)
			{
				num8 = ChartHeight / 2.5f;
			}
			if (MultipleSites)
			{
				num8 /= 1.2f;
			}
			if (Printer & !MultiSite)
			{
				num8 = ChartWidth / 2.6f;
				num7 = 1.1f * num8;
			}
			int num9 = 1;
			int num10 = 1;
			if (Settings.Default.MoonMapMirror)
			{
				num9 = -1;
			}
			if (Settings.Default.MoonMapSouthAtTop)
			{
				num10 = -1;
				num9 = -num9;
			}
			Font font = new Font("Ariel", 7f);
			Font font2 = new Font("Ariel", 10f);
			Brush brush;
			Brush brush2;
			Brush brush3;
			Brush brush4;
			Pen pen;
			Pen pen2;
			Pen pen3;
			Pen pen4;
			Pen pen5;
			Pen pen6;
			SolidBrush brush5;
			Brush brush6;
			Brush brush7;
			if (Printer | Settings.Default.BWFlag)
			{
				brush = new SolidBrush(Color.FromArgb(255, 255, 255, 255));
				brush2 = new SolidBrush(Color.FromArgb(255, 128, 128, 128));
				brush3 = new SolidBrush(Color.FromArgb(255, 235, 235, 235));
				brush4 = new SolidBrush(Color.FromArgb(25, 235, 235, 235));
				pen = new Pen(Color.FromArgb(150, 10, 10, 10));
				pen2 = new Pen(Color.FromArgb(150, 10, 10, 10));
				pen3 = new Pen(Color.FromArgb(120, 10, 10, 10), 1f);
				pen4 = new Pen(Color.FromArgb(150, 10, 10, 10), 1.5f);
				pen5 = new Pen(Brushes.Black);
				pen6 = new Pen(Brushes.Black);
				brush5 = new SolidBrush(Color.Black);
				brush6 = Brushes.Black;
				brush7 = Brushes.White;
			}
			else
			{
				brush = new SolidBrush(Color.FromArgb(245, 255, 255, 200));
				brush2 = new SolidBrush(Color.FromArgb(155, 128, 128, 128));
				brush3 = new SolidBrush(Color.FromArgb(50, 160, 140, 0));
				brush4 = new SolidBrush(Color.FromArgb(100, 100, 120, 0));
				pen = new Pen(Color.FromArgb(100, 10, 10, 10));
				pen2 = new Pen(Color.FromArgb(250, 50, 50, 50));
				pen3 = new Pen(Color.FromArgb(200, 80, 80), 1f);
				pen4 = new Pen(Color.FromArgb(50, 50, 50), 2f);
				new Pen(Color.White);
				pen5 = new Pen(Color.FromArgb(250, 250, 250));
				pen6 = new Pen(Color.FromArgb(128, 128, 128));
				brush5 = new SolidBrush(Color.White);
				brush6 = Brushes.ForestGreen;
				brush7 = Brushes.LightBlue;
			}
			pen6.DashPattern = new float[2] { 4f, 4f };
			if (Printer | Settings.Default.BWFlag)
			{
				formGraphics.Clear(Color.FromArgb(255, 240, 240, 240));
			}
			else
			{
				formGraphics.Clear(Color.FromArgb(255, 25, 25, 25));
			}
			double aA;
			double pA;
			double num11;
			double elongation;
			double lL;
			double lB;
			if (MultipleSites)
			{
				aA = LunarOccultations.MultiPrediction[0].AA;
				pA = LunarOccultations.MultiPrediction[0].PA;
				num11 = LunarOccultations.MultiPrediction[0].PABrightLimb + aA - pA;
				elongation = LunarOccultations.MultiPrediction[0].Elongation;
				lL = LunarOccultations.MultiPrediction[0].LL;
				lB = LunarOccultations.MultiPrediction[0].LB;
				num3 = 1;
				if (LunarOccultations.MultiPrediction[0].WaxFlag == "-")
				{
					num3 = -1;
				}
			}
			else
			{
				aA = LunarOccultations.Prediction[LunarOccultations.MoonMapSelectedStar].AA;
				pA = LunarOccultations.Prediction[LunarOccultations.MoonMapSelectedStar].PA;
				num11 = LunarOccultations.Prediction[LunarOccultations.MoonMapSelectedStar].PABrightLimb + aA - pA;
				elongation = LunarOccultations.Prediction[LunarOccultations.MoonMapSelectedStar].Elongation;
				lL = LunarOccultations.Prediction[LunarOccultations.MoonMapSelectedStar].LL;
				lB = LunarOccultations.Prediction[LunarOccultations.MoonMapSelectedStar].LB;
				num3 = 1;
				if (LunarOccultations.Prediction[LunarOccultations.MoonMapSelectedStar].WaxFlag == "-")
				{
					num3 = -1;
				}
			}
			double num12 = pA - aA;
			if (num12 < -180.0)
			{
				num12 += 360.0;
			}
			if (num12 > 180.0)
			{
				num12 -= 360.0;
			}
			double num13 = Math.Sin(num12 / (180.0 / Math.PI));
			double num14 = Math.Cos(num12 / (180.0 / Math.PI));
			formGraphics.FillEllipse(brush2, num6 - num8, num7 - num8, 2f * num8, 2f * num8);
			PointF[] array = new PointF[300];
			int num15 = 0;
			for (double num16 = num11 - 90.0; num16 <= num11 + 90.1; num16 += 1.0)
			{
				double num17 = num16 + num12;
				num = (float)((double)num6 + (double)num9 * ((double)(0f - num8) * Math.Sin(num17 / (180.0 / Math.PI))));
				num2 = (float)((double)num7 - (double)((float)num10 * num8) * Math.Cos(num17 / (180.0 / Math.PI)));
				array[num15] = new PointF(num, num2);
				num15++;
			}
			for (int i = 0; i <= 180; i += 2)
			{
				double num18 = (double)num8 * Math.Sin((double)i / (180.0 / Math.PI)) * Math.Cos(elongation / (180.0 / Math.PI));
				double num19 = (double)num8 * Math.Cos((double)i / (180.0 / Math.PI));
				double num20 = (0.0 - num11 - num12 - 90.0) / (180.0 / Math.PI);
				num = (float)((double)num6 + (double)num9 * (num18 * Math.Cos(num20) + num19 * Math.Sin(num20)));
				num2 = (float)((double)num7 - (double)num10 * (num19 * Math.Cos(num20) - num18 * Math.Sin(num20)));
				array[num15] = new PointF(num, num2);
				num15++;
			}
			Array.Resize(ref array, num15);
			GraphicsPath graphicsPath = new GraphicsPath();
			graphicsPath.AddClosedCurve(array);
			formGraphics.FillPath(brush, graphicsPath);
			formGraphics.DrawLine(pen6, (float)((double)num6 - (double)((float)num9 * num8) * num13), (float)((double)num7 - (double)((float)num10 * num8) * num14), (float)((double)num6 - (double)num9 * 1.1 * (double)num8 * num13), (float)((double)num7 - (double)num10 * 1.1 * (double)num8 * num14));
			formGraphics.DrawLine(pen6, (float)((double)num6 + (double)((float)num9 * num8) * num13), (float)((double)num7 + (double)((float)num10 * num8) * num14), (float)((double)num6 + (double)num9 * 1.1 * (double)num8 * num13), (float)((double)num7 + (double)num10 * 1.1 * (double)num8 * num14));
			StreamReader streamReader = new StreamReader(AppPath + "\\Resource Files\\Maria.dat");
			Pen pen7;
			do
			{
				string? text = streamReader.ReadLine();
				int num21 = int.Parse(text!.Substring(0, 3));
				float num22 = float.Parse(text!.Substring(5, 5));
				float num23 = float.Parse(text!.Substring(12));
				num22 = (float)(((double)num22 - lL) / (180.0 / Math.PI));
				num23 = (float)(((double)num23 - lB * Math.Cos(num22)) / (180.0 / Math.PI));
				pen7 = pen4;
				Brush brush8 = brush4;
				double num24 = (double)num22 * (180.0 / Math.PI) + elongation * (double)num3;
				if (num24 > 180.0)
				{
					num24 -= 360.0;
				}
				if (num24 < -180.0)
				{
					num24 += 360.0;
				}
				if (Math.Abs(num24) > 90.0)
				{
					pen7 = pen3;
				}
				if (num22 == 0f)
				{
					num22 = 1E-07f;
				}
				double num25 = Math.Atan2(Math.Tan(num23), Math.Sin(num22));
				double num26 = Math.Cos(num22) * Math.Cos(num23);
				double num27 = Math.Cos(num25);
				double num28 = Math.Sin(num25);
				double num29 = (double)num8 * Math.Sqrt(1.0 - num26 * num26) * num27;
				double num30 = (double)num8 * Math.Sqrt(1.0 - num26 * num26) * num28;
				num = (float)((double)num6 + (double)num9 * (num29 * num14 - num30 * num13));
				num2 = (float)((double)num7 - (double)num10 * (num29 * num13 + num30 * num14));
				if (num21 == 1)
				{
					if (!flag5)
					{
						formGraphics.DrawLine(pen7, x, y, x2, y2);
					}
					flag5 = false;
					x2 = (x = num);
					y2 = (y = num2);
				}
				else
				{
					formGraphics.DrawLine(pen7, x, y, num, num2);
					x = num;
					y = num2;
				}
			}
			while (!streamReader.EndOfStream);
			formGraphics.DrawLine(pen7, x, y, x2, y2);
			streamReader.Close();
			string path = AppPath + "\\Resource Files\\Crater.bin";
			int num31 = 27;
			FileStream fileStream = new FileStream(path, FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			for (int j = 0; j < fileStream.Length / num31; j++)
			{
				float num22 = (float)((double)((float)binaryReader.ReadInt16() / 10f) - lL);
				float num23 = (float)((double)((float)binaryReader.ReadInt16() / 10f) - lB);
				float num32 = (float)binaryReader.ReadInt16() / 10f;
				char[] value = binaryReader.ReadChars(20);
				binaryReader.ReadChar();
				float num33 = num32 / 3476f * num8;
				string text2 = Utilities.ProperCase(new string(value).Trim());
				if (!(text2.Substring(0, 4).ToUpper() != "MARE" && num32 < 250f))
				{
					continue;
				}
				pen7 = pen2;
				Brush brush8 = brush4;
				Brush brush9 = brush7;
				double num24 = (double)num22 + elongation * (double)num3;
				if (num24 > 180.0)
				{
					num24 -= 360.0;
				}
				if (num24 < -180.0)
				{
					num24 += 360.0;
				}
				if (Math.Abs(num24) > 90.0)
				{
					pen7 = pen;
					brush8 = brush3;
					brush9 = brush6;
				}
				num22 = (float)((double)num22 / (180.0 / Math.PI));
				num23 = (float)((double)num23 / (180.0 / Math.PI));
				double num26 = Math.Cos(num22) * Math.Cos(num23);
				double num34 = (double)num8 * Math.Sqrt(1.0 - num26 * num26);
				if (num26 > 0.02)
				{
					array = new PointF[19];
					GraphicsPath graphicsPath2 = new GraphicsPath();
					if (num22 == 0f)
					{
						num22 = 1E-07f;
					}
					double num35 = Math.Atan2(Math.Tan(num23), Math.Sin(num22));
					double num27 = Math.Cos(num35);
					double num28 = Math.Sin(num35);
					int num36 = 0;
					for (double num37 = 0.0; num37 < 361.0; num37 += 20.0)
					{
						double num38 = num26 * (double)num33 * Math.Sin(num37 / (180.0 / Math.PI));
						double num39 = (double)num33 * Math.Cos(num37 / (180.0 / Math.PI));
						double num40 = num27 * num38 + num28 * num39;
						double num41 = num28 * num38 - num27 * num39;
						double num29 = num34 * num27 + num40;
						double num30 = num34 * num28 + num41;
						num = (float)((double)num6 + (double)num9 * (num29 * num14 - num30 * num13));
						num2 = (float)((double)num7 - (double)num10 * (num29 * num13 + num30 * num14));
						array[num36] = new PointF(num, num2);
						num36++;
					}
					graphicsPath2.AddClosedCurve(array);
					formGraphics.FillPath(brush8, graphicsPath2);
					for (int k = 0; k < array.Length - 1; k += 2)
					{
						formGraphics.DrawLine(pen7, array[k], array[k + 1]);
					}
					if (Settings.Default.MoonMapNames && (num32 > 120f || (num32 > 80f && num23 > 0f)))
					{
						formGraphics.DrawString(text2, font, brush9, (float)((double)num6 + (double)num9 * num34 * (num27 * num14 - num28 * num13)), (float)((double)num7 - (double)num10 * num34 * (num27 * num13 + num28 * num14)));
					}
				}
			}
			fileStream.Close();
			if (MultipleSites)
			{
				for (int l = 0; l < LunarOccultations.MultiPrediction.Count; l++)
				{
					if ((LunarOccultations.MultiPrediction[l].EventPhase.ToUpper() != "D ") & (LunarOccultations.MultiPrediction[l].EventPhase.ToUpper() != "R "))
					{
						continue;
					}
					num = (float)((double)num6 - (double)num9 * 1.002 * (double)num8 * Math.Sin(LunarOccultations.MultiPrediction[l].PA / (180.0 / Math.PI)));
					num2 = (float)((double)num7 - (double)num10 * 1.002 * (double)num8 * Math.Cos(LunarOccultations.MultiPrediction[l].PA / (180.0 / Math.PI)));
					formGraphics.FillEllipse(brush5, num - 1f, num2 - 1f, 2f, 2f);
					double num42 = (LunarOccultations.MultiPrediction[l].PA + LunarOccultations.MultiPrediction[l].CCT) / (180.0 / Math.PI);
					double num43 = LunarOccultations.MultiPrediction[l].RV * (double)num8 / Math.Cos(LunarOccultations.MultiPrediction[l].CCT / (180.0 / Math.PI));
					float num44 = (float)((double)(-num9) * num43 * Math.Sin(num42));
					float num45 = (float)((double)(-num10) * num43 * Math.Cos(num42));
					formGraphics.DrawLine(pen5, num, num2, num + num44, num2 + num45);
					if (LunarOccultations.MoonMap.showPathTagsToolStripMenuItem.get_Checked())
					{
						float num46 = 10 * (l % 3);
						if (num44 < 0f)
						{
							num46 -= 35f;
						}
						formGraphics.DrawString(LunarOccultations.MultiPrediction[l].Tag.ToString(), font, brush6, num + num44 + num46, num2 + num45 - 5f);
					}
				}
			}
			else
			{
				int num47 = 0;
				double jD = LunarOccultations.Prediction[LunarOccultations.MoonMapSelectedStar].JD;
				for (int m = LunarOccultations.MoonMapSelectedStar; m < LunarOccultations.Prediction.Count && !(LunarOccultations.Prediction[m].JD - jD > 0.5); m++)
				{
					if (@checked)
					{
						if (flag)
						{
							double num48 = LunarOccultations.Prediction[m].EventUT();
							if (num48 < num4)
							{
								num48 += 24.0;
							}
							if (num48 > num5)
							{
								continue;
							}
						}
						if ((flag2 && LunarOccultations.Prediction[m].SunAlt > -6.0) || (flag3 && LunarOccultations.Prediction[m].CA < 0.0) || (flag4 && LunarOccultations.Prediction[m].GrazeLatitude2 < 100.0))
						{
							continue;
						}
					}
					num = (float)((double)num6 - (double)num9 * 1.002 * (double)num8 * Math.Sin(LunarOccultations.Prediction[m].PA / (180.0 / Math.PI)) - 3.0);
					num2 = (float)((double)num7 - (double)num10 * 1.002 * (double)num8 * Math.Cos(LunarOccultations.Prediction[m].PA / (180.0 / Math.PI)) - 3.0);
					formGraphics.FillEllipse(brush5, num, num2, 6f, 6f);
					string text3 = LunarOccultations.Prediction[m].EventTime() + $" ({LunarOccultations.Prediction[m].Mv:F1})";
					if (num > num6)
					{
						formGraphics.DrawString(string.Format("{0,2:f0},", num47 + 1) + text3, font, brush5, num + 8f, num2);
					}
					else
					{
						float width = formGraphics.MeasureString(string.Format("{0,2:f0},", num47 + 1) + text3, font).Width;
						formGraphics.DrawString(string.Format("{0,2:f0},", num47 + 1) + text3, font, brush5, num - 4f - width, num2);
					}
					if (LunarOccultations.MoonMap.showPathTagsToolStripMenuItem.get_Checked())
					{
						double num42 = (LunarOccultations.Prediction[m].PA + LunarOccultations.Prediction[m].CCT) / (180.0 / Math.PI);
						double num43 = LunarOccultations.Prediction[m].RV * (double)num8 / Math.Cos(LunarOccultations.Prediction[m].CCT / (180.0 / Math.PI));
						num += 2f;
						num2 += 2f;
						float num44 = (float)((double)(-num9) * num43 * Math.Sin(num42));
						float num45 = (float)((double)(-num10) * num43 * Math.Cos(num42));
						pen5.DashPattern = new float[2] { 2f, 4f };
						formGraphics.DrawLine(pen5, num, num2, num + num44, num2 + num45);
					}
					num47++;
					if (num47 >= NumOfEventsToPlot)
					{
						break;
					}
				}
			}
			formGraphics.DrawString("S", font2, brush6, num6 - 5f, num7 - 10f + (float)num10 * (num7 - 15f));
			formGraphics.DrawString("N", font2, brush6, num6 - 5f, num7 - 10f - (float)num10 * (num7 - 15f));
			formGraphics.DrawString("E", font2, brush6, num6 - 10f - (float)num9 * (num6 - 15f), num7 - 5f);
			formGraphics.DrawString("W", font2, brush6, num6 - 10f + (float)num9 * (num6 - 15f), num7 - 5f);
			if (!(Printer && !MultipleSites))
			{
				return;
			}
			float num49 = 2.3f * num8;
			Font font3 = new Font("Courier New", 8f);
			int num50 = 0;
			int num51 = 0;
			double jD2 = LunarOccultations.Prediction[LunarOccultations.MoonMapSelectedStar].JD;
			formGraphics.DrawString("           day  Time   P   Star  Sp  Mag  Mag    % Elon Sun  Moon   CA   PA  VA  AA", font3, brush6, 100f, num49 - 24f);
			formGraphics.DrawString("     y   m  d  h  m  s      No  D     v    r V  ill     Alt Alt Az   o    o   o   o", font3, brush6, 100f, num49 - 12f);
			for (int n = LunarOccultations.MoonMapSelectedStar; n < LunarOccultations.Prediction.Count; n++)
			{
				if (@checked)
				{
					if (flag)
					{
						double num52 = LunarOccultations.Prediction[n].EventUT();
						if (num52 < num4)
						{
							num52 += 24.0;
						}
						if (num52 > num5)
						{
							continue;
						}
					}
					if ((flag2 && LunarOccultations.Prediction[n].SunAlt > -6.0) || (flag3 && LunarOccultations.Prediction[n].CA < 0.0) || (flag4 && LunarOccultations.Prediction[n].GrazeLatitude2 < 100.0))
					{
						continue;
					}
				}
				formGraphics.DrawString(string.Format("{0,2:f0}, ", num50 + 1) + LunarOccultations.Prediction[n].FormatLine(ShortLine: true), font3, brush6, 100f, num49 + (float)(12 * num51));
				num50++;
				num51++;
				if (LunarOccultations.Prediction[n].DoubleDetailsExist)
				{
					formGraphics.DrawString(LunarOccultations.Prediction[n].DoubleDetails, font3, brush6, 120f, num49 + (float)(12 * num51));
					num51++;
					if (LunarOccultations.Prediction[n].DoubleWantedExists)
					{
						formGraphics.DrawString(LunarOccultations.Prediction[n].DoubleObservationsWanted, font3, brush6, 120f, num49 + (float)(12 * num51));
						num51++;
					}
				}
				if (num50 >= NumOfEventsToPlot || LunarOccultations.Prediction[n].JD - jD2 > 0.5)
				{
					break;
				}
			}
		}

		private static void SwapPens(ref Pen A1, ref Pen A2)
		{
			Pen pen = A1;
			A1 = A2;
			A2 = pen;
		}

		private static void SwapBrushes(ref Brush A1, ref Brush A2)
		{
			Brush brush = A1;
			A1 = A2;
			A2 = brush;
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void bWToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.BWFlag = !Settings.Default.BWFlag;
			bWToolStripMenuItem.set_Checked(Settings.Default.BWFlag);
			if (bWToolStripMenuItem.get_Checked())
			{
				((ToolStripItem)bWToolStripMenuItem).set_Text("Plot in Colour");
			}
			else
			{
				((ToolStripItem)bWToolStripMenuItem).set_Text("Plot in B&&W");
			}
			if (FormCreated)
			{
				DrawMoon();
			}
		}

		private void mirrorImageToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.MoonMapMirror = !Settings.Default.MoonMapMirror;
			mirrorImageToolStripMenuItem.set_Checked(Settings.Default.MoonMapMirror);
			if (FormCreated)
			{
				DrawMoon();
			}
		}

		private void showCraterNamesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.MoonMapNames = !Settings.Default.MoonMapNames;
			showCraterNamesToolStripMenuItem.set_Checked(Settings.Default.MoonMapNames);
			if (FormCreated)
			{
				DrawMoon();
			}
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picMoon.get_Image());
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			PrintPreviewMoon();
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			PrintMoon();
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_LunarPredictions = Output.SaveGraphic(picMoon.get_Image(), "Moon map", Settings.Default.Save_LunarPredictions);
		}

		private void MoonMap_Load(object sender, EventArgs e)
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
			FormCreated = true;
		}

		private void plotSelectedStarOnlyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			NumOfEventsToPlot = 1;
			DrawMoon();
		}

		private void plot5StarsStartingAtSelectedToolStripMenuItem_Click(object sender, EventArgs e)
		{
			NumOfEventsToPlot = 5;
			DrawMoon();
		}

		private void plot10StarsFromSelectedToolStripMenuItem_Click(object sender, EventArgs e)
		{
			NumOfEventsToPlot = 10;
			DrawMoon();
		}

		private void plot20StarsFromSelectedToolStripMenuItem_Click(object sender, EventArgs e)
		{
			NumOfEventsToPlot = 20;
			DrawMoon();
		}

		private void plotAllStarsForTheDayToolStripMenuItem_Click(object sender, EventArgs e)
		{
			NumOfEventsToPlot = 1000;
			DrawMoon();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Lunar Occultations - MoonMap");
		}

		private void showPathTagsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (FormCreated)
			{
				DrawMoon();
			}
		}

		private void southAtTopToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.MoonMapSouthAtTop = southAtTopToolStripMenuItem.get_Checked();
			if (FormCreated)
			{
				DrawMoon();
			}
		}

		private void MoonMap_FormClosing(object sender, FormClosingEventArgs e)
		{
			Settings.Default.MoonMapWidth = ((Control)this).get_Width();
			Settings.Default.MoonMapHeight = ((Control)this).get_Height();
		}

		private void MoonMap_FormClosed(object sender, FormClosedEventArgs e)
		{
			((Component)this).Dispose();
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
			//IL_00ac: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b6: Expected O, but got Unknown
			//IL_00b7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c1: Expected O, but got Unknown
			//IL_00c2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00cc: Expected O, but got Unknown
			//IL_00cd: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d7: Expected O, but got Unknown
			//IL_00d8: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e2: Expected O, but got Unknown
			//IL_00e3: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ed: Expected O, but got Unknown
			//IL_00ee: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f8: Expected O, but got Unknown
			//IL_00f9: Unknown result type (might be due to invalid IL or missing references)
			//IL_0103: Expected O, but got Unknown
			//IL_0a92: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a9c: Expected O, but got Unknown
			//IL_0aee: Unknown result type (might be due to invalid IL or missing references)
			//IL_0af8: Expected O, but got Unknown
			//IL_0b00: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b0a: Expected O, but got Unknown
			components = new Container();
			menuStrip1 = new MenuStrip();
			withMapToolStripMenuItem = new ToolStripMenuItem();
			bWToolStripMenuItem = new ToolStripMenuItem();
			southAtTopToolStripMenuItem = new ToolStripMenuItem();
			mirrorImageToolStripMenuItem = new ToolStripMenuItem();
			showCraterNamesToolStripMenuItem = new ToolStripMenuItem();
			showPathTagsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			contextMenuStrip1 = new ContextMenuStrip(components);
			plotSelectedStarOnlyToolStripMenuItem = new ToolStripMenuItem();
			plot5StarsStartingAtSelectedToolStripMenuItem = new ToolStripMenuItem();
			plot10StarsFromSelectedToolStripMenuItem = new ToolStripMenuItem();
			plot20StarsFromSelectedToolStripMenuItem = new ToolStripMenuItem();
			plotAllStarsForTheDayToolStripMenuItem = new ToolStripMenuItem();
			label1 = new Label();
			picMoon = new PictureBox();
			((Control)menuStrip1).SuspendLayout();
			((Control)contextMenuStrip1).SuspendLayout();
			((ISupportInitialize)picMoon).BeginInit();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withMapToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(600, 24));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withMapToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[10]
			{
				(ToolStripItem)bWToolStripMenuItem,
				(ToolStripItem)southAtTopToolStripMenuItem,
				(ToolStripItem)mirrorImageToolStripMenuItem,
				(ToolStripItem)showCraterNamesToolStripMenuItem,
				(ToolStripItem)showPathTagsToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withMapToolStripMenuItem).set_Name("withMapToolStripMenuItem");
			((ToolStripItem)withMapToolStripMenuItem).set_Size(new Size(96, 20));
			((ToolStripItem)withMapToolStripMenuItem).set_Text("with Map...      ");
			((ToolStripItem)bWToolStripMenuItem).set_Image((Image)Resources.ColorHS);
			((ToolStripItem)bWToolStripMenuItem).set_Name("bWToolStripMenuItem");
			((ToolStripItem)bWToolStripMenuItem).set_Size(new Size(268, 22));
			((ToolStripItem)bWToolStripMenuItem).set_Text("Plot in B&&W");
			((ToolStripItem)bWToolStripMenuItem).add_Click((EventHandler)bWToolStripMenuItem_Click);
			southAtTopToolStripMenuItem.set_Checked(Settings.Default.MoonMapSouthAtTop);
			southAtTopToolStripMenuItem.set_CheckOnClick(true);
			((ToolStripItem)southAtTopToolStripMenuItem).set_Image((Image)Resources.FillDownHS);
			((ToolStripItem)southAtTopToolStripMenuItem).set_Name("southAtTopToolStripMenuItem");
			((ToolStripItem)southAtTopToolStripMenuItem).set_Size(new Size(268, 22));
			((ToolStripItem)southAtTopToolStripMenuItem).set_Text("South at top  [rotate 180 deg.]");
			((ToolStripItem)southAtTopToolStripMenuItem).add_Click((EventHandler)southAtTopToolStripMenuItem_Click);
			((ToolStripItem)mirrorImageToolStripMenuItem).set_Image((Image)Resources.Edit_RedoHS);
			((ToolStripItem)mirrorImageToolStripMenuItem).set_Name("mirrorImageToolStripMenuItem");
			mirrorImageToolStripMenuItem.set_ShortcutKeys((Keys)131149);
			((ToolStripItem)mirrorImageToolStripMenuItem).set_Size(new Size(268, 22));
			((ToolStripItem)mirrorImageToolStripMenuItem).set_Text("Mirror image [East <> West]");
			((ToolStripItem)mirrorImageToolStripMenuItem).add_Click((EventHandler)mirrorImageToolStripMenuItem_Click);
			((ToolStripItem)showCraterNamesToolStripMenuItem).set_Name("showCraterNamesToolStripMenuItem");
			showCraterNamesToolStripMenuItem.set_ShortcutKeys((Keys)131150);
			((ToolStripItem)showCraterNamesToolStripMenuItem).set_Size(new Size(268, 22));
			((ToolStripItem)showCraterNamesToolStripMenuItem).set_Text("Show crater names");
			((ToolStripItem)showCraterNamesToolStripMenuItem).add_Click((EventHandler)showCraterNamesToolStripMenuItem_Click);
			showPathTagsToolStripMenuItem.set_Checked(Settings.Default.MoonMapTagPaths);
			showPathTagsToolStripMenuItem.set_CheckOnClick(true);
			showPathTagsToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)showPathTagsToolStripMenuItem).set_Name("showPathTagsToolStripMenuItem");
			((ToolStripItem)showPathTagsToolStripMenuItem).set_Size(new Size(268, 22));
			((ToolStripItem)showPathTagsToolStripMenuItem).set_Text("Show star paths");
			((ToolStripItem)showPathTagsToolStripMenuItem).add_Click((EventHandler)showPathTagsToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(265, 6));
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(268, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(268, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(268, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(268, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(69, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help   ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("&Exit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((ToolStrip)contextMenuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)plotSelectedStarOnlyToolStripMenuItem,
				(ToolStripItem)plot5StarsStartingAtSelectedToolStripMenuItem,
				(ToolStripItem)plot10StarsFromSelectedToolStripMenuItem,
				(ToolStripItem)plot20StarsFromSelectedToolStripMenuItem,
				(ToolStripItem)plotAllStarsForTheDayToolStripMenuItem
			});
			((Control)contextMenuStrip1).set_Name("contextMenuStrip1");
			((Control)contextMenuStrip1).set_Size(new Size(330, 114));
			((ToolStripItem)plotSelectedStarOnlyToolStripMenuItem).set_Name("plotSelectedStarOnlyToolStripMenuItem");
			((ToolStripItem)plotSelectedStarOnlyToolStripMenuItem).set_Size(new Size(329, 22));
			((ToolStripItem)plotSelectedStarOnlyToolStripMenuItem).set_Text("Plot the selected star only");
			((ToolStripItem)plotSelectedStarOnlyToolStripMenuItem).add_Click((EventHandler)plotSelectedStarOnlyToolStripMenuItem_Click);
			((ToolStripItem)plot5StarsStartingAtSelectedToolStripMenuItem).set_Name("plot5StarsStartingAtSelectedToolStripMenuItem");
			((ToolStripItem)plot5StarsStartingAtSelectedToolStripMenuItem).set_Size(new Size(329, 22));
			((ToolStripItem)plot5StarsStartingAtSelectedToolStripMenuItem).set_Text("Plot 5 stars, starting at selected");
			((ToolStripItem)plot5StarsStartingAtSelectedToolStripMenuItem).add_Click((EventHandler)plot5StarsStartingAtSelectedToolStripMenuItem_Click);
			((ToolStripItem)plot10StarsFromSelectedToolStripMenuItem).set_Name("plot10StarsFromSelectedToolStripMenuItem");
			((ToolStripItem)plot10StarsFromSelectedToolStripMenuItem).set_Size(new Size(329, 22));
			((ToolStripItem)plot10StarsFromSelectedToolStripMenuItem).set_Text("Plot 10 stars, starting at selected");
			((ToolStripItem)plot10StarsFromSelectedToolStripMenuItem).add_Click((EventHandler)plot10StarsFromSelectedToolStripMenuItem_Click);
			((ToolStripItem)plot20StarsFromSelectedToolStripMenuItem).set_Name("plot20StarsFromSelectedToolStripMenuItem");
			((ToolStripItem)plot20StarsFromSelectedToolStripMenuItem).set_Size(new Size(329, 22));
			((ToolStripItem)plot20StarsFromSelectedToolStripMenuItem).set_Text("Plot 20 stars, starting at selected");
			((ToolStripItem)plot20StarsFromSelectedToolStripMenuItem).add_Click((EventHandler)plot20StarsFromSelectedToolStripMenuItem_Click);
			((ToolStripItem)plotAllStarsForTheDayToolStripMenuItem).set_Name("plotAllStarsForTheDayToolStripMenuItem");
			((ToolStripItem)plotAllStarsForTheDayToolStripMenuItem).set_Size(new Size(329, 22));
			((ToolStripItem)plotAllStarsForTheDayToolStripMenuItem).set_Text("Plot all stars in next 12 hours, starting at selected");
			((ToolStripItem)plotAllStarsForTheDayToolStripMenuItem).add_Click((EventHandler)plotAllStarsForTheDayToolStripMenuItem_Click);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(263, 10));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(166, 12));
			((Control)label1).set_TabIndex(3);
			((Control)label1).set_Text("right-click to set number of stars plotted");
			((Control)picMoon).set_ContextMenuStrip(contextMenuStrip1);
			((Control)picMoon).set_Location(new Point(0, 27));
			((Control)picMoon).set_Name("picMoon");
			((Control)picMoon).set_Size(new Size(600, 500));
			picMoon.set_TabIndex(1);
			picMoon.set_TabStop(false);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(600, 527));
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)picMoon);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationMoonMap", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationMoonMap);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("MoonMap");
			((Control)this).set_Text("Moon map");
			((Form)this).add_FormClosing(new FormClosingEventHandler(MoonMap_FormClosing));
			((Form)this).add_FormClosed(new FormClosedEventHandler(MoonMap_FormClosed));
			((Form)this).add_Load((EventHandler)MoonMap_Load);
			((Control)this).add_Resize((EventHandler)MoonMap_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)contextMenuStrip1).ResumeLayout(false);
			((ISupportInitialize)picMoon).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
