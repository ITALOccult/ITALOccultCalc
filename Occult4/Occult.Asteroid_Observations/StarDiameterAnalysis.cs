using System;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Printing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Asteroid_Observations
{
	public class StarDiameterAnalysis
	{
		internal static double[] SaturationFactor = new double[21]
		{
			1.0, 0.999, 0.995, 0.988, 0.978, 0.965, 0.949, 0.929, 0.906, 0.879,
			0.846, 0.809, 0.766, 0.717, 0.66, 0.596, 0.522, 0.434, 0.33, 0.199,
			0.0
		};

		internal static float[] Theoretical = new float[6000];

		internal static float[] Observed = new float[600];

		private static int NumObserved = 0;

		private static float Toffset = 0f;

		private static float PlotScale = 1f;

		internal static double SaturationLevel;

		internal static string DiameterSolutions = "";

		private static int PlotAll_IndividualChartWidth;

		private static int PlotAll_IndividualChartHeight;

		internal static float NumOfSecsDisplayed = 1f;

		private static bool EventTypeForPrinter = true;

		internal static float VideoStepIntervalMsec;

		internal static float ChartXMiddle;

		internal static float TimeAxisScale;

		public static StarDiameterAnalyser StarDiameter_Analyser;

		internal static StarDiameterPaste StarDiameter_Paste = new StarDiameterPaste();

		internal static StarDiameterPlotAll StarDiameter_PlotAll;

		public static void ShowStarDiameterAnalyser()
		{
			try
			{
				((Control)StarDiameter_Analyser).Show();
			}
			catch
			{
				StarDiameter_Analyser = new StarDiameterAnalyser();
				((Control)StarDiameter_Analyser).Show();
			}
		}

		public static void ShowStarDiameterPlotAll()
		{
			try
			{
				((Control)StarDiameter_PlotAll).Show();
			}
			catch
			{
				StarDiameter_PlotAll = new StarDiameterPlotAll();
				((Control)StarDiameter_PlotAll).Show();
			}
		}

		internal static void GenerateTheoreticalLightCurve(double StarDiaMAS, double StarMotionMASperSec, bool Devent)
		{
			for (int i = 0; i < 6000; i++)
			{
				Theoretical[i] = 0f;
			}
			if (Devent)
			{
				for (int j = 0; j < 3000; j++)
				{
					Theoretical[j] = 1f;
				}
			}
			else
			{
				for (int k = 3001; k < 6000; k++)
				{
					Theoretical[k] = 1f;
				}
			}
			Theoretical[3000] = 0.5f;
			int num = (int)(StarDiaMAS / StarMotionMASperSec / 2.0 / (double)VideoStepIntervalMsec * 1000.0) * 10;
			double num2 = StarMotionMASperSec / StarDiaMAS * 2.0 * (double)VideoStepIntervalMsec / 1000.0 / 10.0;
			double num3 = 2f * SegmentArea_LimbDarkened(1E-05);
			for (int l = -num - 100; l <= num + 100; l++)
			{
				double num4 = (double)l * num2;
				double num5;
				if (num4 < -1.0)
				{
					num5 = 1.0;
				}
				else if (num4 > 1.0)
				{
					num5 = 0.0;
				}
				else if (!StarDiameter_Analyser.chkLimbDarkening.get_Checked())
				{
					double num6 = 2.0 * Math.Acos(num4);
					num5 = (num6 - Math.Sin(num6)) / 2.0 / Math.PI;
				}
				else
				{
					num5 = (double)SegmentArea_LimbDarkened(num4) / num3;
				}
				if (Devent)
				{
					Theoretical[3000 + l] = (float)num5;
				}
				else
				{
					Theoretical[3000 - l] = (float)num5;
				}
			}
			double num7 = SaturationAdjusted(1.0);
			if (num7 < 1.0)
			{
				for (int m = -num - 100; m <= num + 100; m++)
				{
					Theoretical[3000 + m] = (float)((double)SaturationAdjusted(Theoretical[3000 + m]) / num7);
				}
			}
		}

		internal static float SegmentArea_LimbDarkened(double Dist)
		{
			int num = 30;
			double[] array = new double[num + 1];
			double[] array2 = new double[num];
			double[] array3 = new double[num];
			double[] array4 = new double[34]
			{
				0.6, 0.65, 0.69, 0.73, 0.77, 0.82, 0.86, 0.9, 0.92, 0.94,
				0.96, 0.98, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
				1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
				1.0, 1.0, 1.0, 1.0
			};
			double num2 = 0.0;
			for (int i = 0; i < 30; i++)
			{
				double num3 = 1.0 - (double)i / (double)num;
				double num4;
				if (Dist > num3)
				{
					num4 = 0.0;
				}
				else if (Dist < 0.0 - num3)
				{
					num4 = num3 * num3;
				}
				else
				{
					double num5 = 2.0 * Math.Acos(Dist / num3);
					num4 = (num5 - Math.Sin(num5)) / 2.0 / Math.PI * num3 * num3;
				}
				array[i] = num4;
			}
			for (int j = 0; j < num; j++)
			{
				array2[j] = array[j] - array[j + 1];
			}
			for (int k = 0; k < num; k++)
			{
				array3[k] = array2[k] * array4[k];
			}
			for (int l = 0; l < num; l++)
			{
				num2 += array3[l];
			}
			_ = -0.1;
			return (float)num2;
		}

		internal static float SaturationAdjusted(double Height)
		{
			double num = 1.0;
			double num2 = SaturationLevel / Height;
			if (num2 < 1.0)
			{
				double num3 = 20.0 * (1.0 - num2);
				int num4 = (int)Math.Floor(num3);
				int num5 = num4 + 1;
				double num6 = num3 - (double)num4;
				double num7 = SaturationFactor[num4];
				double num8 = SaturationFactor[num5];
				num = num7 * (1.0 - num6) + num8 * num6;
			}
			return (float)(Height * num);
		}

		internal static void PlotEventOnScreen(bool Devent)
		{
			SetPlotParameters(Devent);
			int width;
			int height;
			if (Devent)
			{
				width = ((Control)StarDiameter_Analyser.picD).get_Width();
				height = ((Control)StarDiameter_Analyser.picD).get_Height();
			}
			else
			{
				width = ((Control)StarDiameter_Analyser.picR).get_Width();
				height = ((Control)StarDiameter_Analyser.picR).get_Height();
			}
			Bitmap image = new Bitmap(width, height);
			Graphics graphics = Graphics.FromImage(image);
			if (Settings.Default.GraphicsSmoothed)
			{
				graphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			graphics.Clear(Color.White);
			PlotCurves(graphics, width, height, Devent, Printer: false);
			if (Devent)
			{
				StarDiameter_Analyser.picD.set_Image((Image)image);
			}
			else
			{
				StarDiameter_Analyser.picR.set_Image((Image)image);
			}
			graphics.Dispose();
			((Control)StarDiameter_Analyser).Focus();
		}

		internal static void PlotAll_Plot()
		{
			Font font = new Font("Courier New", 10f, FontStyle.Regular);
			int num = ((Control)StarDiameter_Analyser.picResD).get_Width() + 5;
			int num2 = 2 * num + 20;
			DiameterSolutions = "";
			ShowStarDiameterPlotAll();
			if (StarDiameter_Analyser.optLarge.get_Checked())
			{
				((Control)StarDiameter_PlotAll).set_Width((int)((double)Screen.GetWorkingArea((Control)(object)StarDiameter_PlotAll).Width * 0.9));
			}
			else
			{
				((Control)StarDiameter_PlotAll).set_Width(720);
			}
			PlotAll_IndividualChartWidth = (int)((double)(((Control)StarDiameter_PlotAll.picPlotAll).get_Width() - 20) / 3.4);
			PlotAll_IndividualChartHeight = (int)((double)(2 * PlotAll_IndividualChartWidth) / 3.0);
			StarDiameterPlotAll starDiameter_PlotAll = StarDiameter_PlotAll;
			((Control)starDiameter_PlotAll).set_Width(((Control)starDiameter_PlotAll).get_Width() + 100);
			int count = StarDiameter_Analyser.lstObservers.get_Items().get_Count();
			int num3 = (PlotAll_IndividualChartHeight + 10) * count + 40;
			if (num3 < ((Control)StarDiameter_PlotAll).get_Height() - 50)
			{
				num3 = ((Control)StarDiameter_PlotAll).get_Height() - 50;
			}
			((Control)StarDiameter_PlotAll.panelAll).set_Height(((Control)StarDiameter_PlotAll).get_Height() - 70);
			((Control)StarDiameter_PlotAll.picPlotAll).set_Height(num3);
			Bitmap image = new Bitmap(((Control)StarDiameter_PlotAll.picPlotAll).get_Width(), ((Control)StarDiameter_PlotAll.picPlotAll).get_Height());
			Graphics graphics = Graphics.FromImage(image);
			if (Settings.Default.GraphicsSmoothed)
			{
				graphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			Bitmap image2 = new Bitmap(PlotAll_IndividualChartWidth, PlotAll_IndividualChartHeight);
			Graphics graphics2 = Graphics.FromImage(image2);
			if (Settings.Default.GraphicsSmoothed)
			{
				graphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			((Control)StarDiameter_Analyser.lstObservers).set_Enabled(false);
			for (int i = 0; i < count; i++)
			{
				StarDiameter_Analyser.SetFieldsforObserverRecord(i);
				SetPlotParameters(DEvent: true);
				graphics2.Clear(Color.White);
				PlotCurves(graphics2, PlotAll_IndividualChartWidth, PlotAll_IndividualChartHeight, Devent: true, Printer: false);
				StarDiameter_Analyser.DrawHistogram(Dcurve: true, GetHistogramData: true);
				graphics.DrawImageUnscaled(image2, 5, i * (PlotAll_IndividualChartHeight + 10) + 40);
				graphics.DrawImageUnscaled(StarDiameter_Analyser.picResD.get_Image(), 2 * PlotAll_IndividualChartWidth + 20, i * (PlotAll_IndividualChartHeight + 10) + 40);
				graphics.DrawImageUnscaled(StarDiameter_Analyser.picNormD.get_Image(), 2 * PlotAll_IndividualChartWidth + 20 + num, i * (PlotAll_IndividualChartHeight + 10) + 40);
				graphics2.Clear(Color.White);
				SetPlotParameters(DEvent: false);
				PlotCurves(graphics2, PlotAll_IndividualChartWidth, PlotAll_IndividualChartHeight, Devent: false, Printer: false);
				StarDiameter_Analyser.DrawHistogram(Dcurve: false, GetHistogramData: true);
				graphics.DrawImageUnscaled(image2, PlotAll_IndividualChartWidth + 15, i * (PlotAll_IndividualChartHeight + 10) + 40);
				graphics.DrawImageUnscaled(StarDiameter_Analyser.picResR.get_Image(), 2 * PlotAll_IndividualChartWidth + 20, i * (PlotAll_IndividualChartHeight + 10) + 40 + ((Control)StarDiameter_Analyser.picResD).get_Height() + 5);
				graphics.DrawImageUnscaled(StarDiameter_Analyser.picNormR.get_Image(), 2 * PlotAll_IndividualChartWidth + 20 + num, i * (PlotAll_IndividualChartHeight + 10) + 40 + ((Control)StarDiameter_Analyser.picResD).get_Height() + 5);
				try
				{
					graphics.DrawString(StarDiameter_Analyser.lstObservers.get_Items().get_Item(i).ToString()!.Substring(12), font, Brushes.Black, 2 * PlotAll_IndividualChartWidth + num2, i * (PlotAll_IndividualChartHeight + 10) + 40);
					graphics.DrawString("D at frame " + ((Control)StarDiameter_Analyser.lblMidD).get_Text(), font, Brushes.Black, 2 * PlotAll_IndividualChartWidth + num2, i * (PlotAll_IndividualChartHeight + 10) + 20 + 40);
					graphics.DrawString("R at frame " + ((Control)StarDiameter_Analyser.lblMidR).get_Text(), font, Brushes.Black, 2 * PlotAll_IndividualChartWidth + num2, i * (PlotAll_IndividualChartHeight + 10) + 33 + 40);
					if (StarDiameter_Analyser.updnWeightD.get_Value() > 0m)
					{
						graphics.DrawString("Dia from D " + string.Format("{0,1:f2}", StarDiameter_Analyser.updnDiaD.get_Value()), font, Brushes.Black, 2 * PlotAll_IndividualChartWidth + num2, i * (PlotAll_IndividualChartHeight + 10) + 63 + 40);
					}
					if (StarDiameter_Analyser.updnWeightR.get_Value() > 0m)
					{
						graphics.DrawString("Dia from R " + string.Format("{0,1:f2}", StarDiameter_Analyser.updnDiaR.get_Value()), font, Brushes.Black, 2 * PlotAll_IndividualChartWidth + num2, i * (PlotAll_IndividualChartHeight + 10) + 76 + 40);
					}
				}
				catch
				{
				}
				if ((StarDiameter_Analyser.updnWeightD.get_Value() > 0m) | (StarDiameter_Analyser.updnWeightR.get_Value() > 0m))
				{
					DiameterSolutions = DiameterSolutions + "\r\n" + StarDiameter_Analyser.lstObservers.get_Items().get_Item(i).ToString() + "\r\n";
					if (StarDiameter_Analyser.updnWeightD.get_Value() > 0m)
					{
						DiameterSolutions = DiameterSolutions + "Dia from D " + string.Format("{0,1:f2}", StarDiameter_Analyser.updnDiaD.get_Value()) + "\r\n";
					}
					if (StarDiameter_Analyser.updnWeightR.get_Value() > 0m)
					{
						DiameterSolutions = DiameterSolutions + "Dia from R " + string.Format("{0,1:f2}", StarDiameter_Analyser.updnDiaR.get_Value()) + "\r\n";
					}
				}
			}
			DiameterSolutions = "Star diameter = " + ((Control)StarDiameter_Analyser.txtStarSolution).get_Text() + " +/- " + ((Control)StarDiameter_Analyser.txtStarUncertainty).get_Text() + " mas, using " + ((Control)StarDiameter_Analyser.txtCurveNos).get_Text() + " light curves\r\n" + DiameterSolutions;
			graphics.DrawString("Star diameter = " + ((Control)StarDiameter_Analyser.txtStarSolution).get_Text() + " +/- " + ((Control)StarDiameter_Analyser.txtStarUncertainty).get_Text() + " mas, using " + ((Control)StarDiameter_Analyser.txtCurveNos).get_Text() + " light curves", font, Brushes.Black, 25f, 5f);
			graphics.DrawString("Raw S.Dvn", font, Brushes.Black, 2 * PlotAll_IndividualChartWidth + 20, 20f);
			graphics.DrawString("Norm S.Dvn", font, Brushes.Black, 2 * PlotAll_IndividualChartWidth + 20 + num, 20f);
			graphics2.Dispose();
			((Control)StarDiameter_Analyser.lstObservers).set_Enabled(true);
			((Control)StarDiameter_Analyser).Focus();
			StarDiameter_PlotAll.picPlotAll.set_Image((Image)image);
			graphics.Dispose();
			((Control)StarDiameter_PlotAll).Focus();
		}

		private static void SetPlotParameters(bool DEvent)
		{
			double.TryParse(((Control)StarDiameter_Analyser.txtFrameRate).get_Text(), out var result);
			if (result == 0.0)
			{
				result = 25.0;
			}
			VideoStepIntervalMsec = (float)(1000.0 / result);
			NumOfSecsDisplayed = (float)StarDiameter_Analyser.updnSecsToDisplay.get_Value();
			PlotScale = (float)StarDiameter_Analyser.updnPlotScale.get_Value();
			double result2;
			if (DEvent)
			{
				if (!double.TryParse(((Control)StarDiameter_Analyser.txtMotionD).get_Text(), out result2))
				{
					result2 = 0.1;
				}
				GenerateTheoreticalLightCurve((double)StarDiameter_Analyser.updnDiaD.get_Value(), result2, Devent: true);
				for (int i = 0; i < StarDiameterAnalyser.Dcount; i++)
				{
					Observed[i] = StarDiameterAnalyser.ObservedD[i];
				}
				NumObserved = StarDiameterAnalyser.Dcount;
				Toffset = (float)StarDiameter_Analyser.updnDAdjust.get_Value();
			}
			else
			{
				if (!double.TryParse(((Control)StarDiameter_Analyser.txtMotionR).get_Text(), out result2))
				{
					result2 = 0.1;
				}
				GenerateTheoreticalLightCurve((double)StarDiameter_Analyser.updnDiaR.get_Value(), result2, Devent: false);
				for (int j = 0; j < StarDiameterAnalyser.Rcount; j++)
				{
					Observed[j] = StarDiameterAnalyser.ObservedR[j];
				}
				NumObserved = StarDiameterAnalyser.Rcount;
				Toffset = (float)StarDiameter_Analyser.updnRAdjust.get_Value();
			}
		}

		internal static void PrintEvent(bool Devent)
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Unknown result type (might be due to invalid IL or missing references)
			//IL_001e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0025: Unknown result type (might be due to invalid IL or missing references)
			//IL_002b: Invalid comparison between Unknown and I4
			PrintDocument printDocument = new PrintDocument();
			PrintDialog val = new PrintDialog();
			val.set_UseEXDialog(true);
			printDocument.DefaultPageSettings.Landscape = true;
			val.set_Document(printDocument);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				EventTypeForPrinter = Devent;
				printDocument.PrintPage += PrintTheEvent;
				printDocument.Print();
			}
		}

		internal static void PrintTheEvent(object sender, PrintPageEventArgs e)
		{
			Graphics graphics = e.Graphics;
			int num = (int)(0.92 * (double)e.PageBounds.Height);
			int num2 = (int)(0.92 * (double)e.PageBounds.Width);
			if (num < num2)
			{
				num2 = num;
			}
			else
			{
				num = num2;
			}
			PlotCurves(graphics, num2, num, EventTypeForPrinter, Printer: true);
		}

		internal static void PlotCurves(Graphics formGraphics, int ChartWidth, int ChartHeight, bool Devent, bool Printer)
		{
			int num = 2;
			formGraphics.Clear(Color.White);
			Pen pen = new Pen(Color.Black, 1f);
			Pen pen2 = new Pen(Color.Red, 1f);
			Brush darkBlue = Brushes.DarkBlue;
			Pen pen3 = new Pen(Color.Green);
			Pen pen4 = new Pen(Color.Black);
			Pen pen5 = new Pen(Color.Green);
			Brush lightYellow = Brushes.LightYellow;
			pen3.DashPattern = new float[2] { 3f, 10f };
			pen5.DashPattern = new float[2] { 3f, 10f };
			pen4.DashPattern = new float[2] { 5f, 10f };
			float num2 = (float)(-ChartHeight) / 1.4f;
			TimeAxisScale = (float)ChartWidth / NumOfSecsDisplayed;
			float num3 = -1.2f * num2;
			ChartXMiddle = (float)ChartWidth / 2f;
			float x = 0f;
			float y = 0f;
			if (StarDiameter_Analyser.chkShowRegions.get_Checked())
			{
				if (Devent)
				{
					float num4 = ChartXMiddle + ((float)int.Parse(((Control)StarDiameter_Analyser.txtMeas1D).get_Text()) - Toffset - (float)(NumObserved / 2)) * TimeAxisScale * VideoStepIntervalMsec / 1000f;
					float num5 = ChartXMiddle + ((float)int.Parse(((Control)StarDiameter_Analyser.txtMeas2D).get_Text()) - Toffset - (float)(NumObserved / 2)) * TimeAxisScale * VideoStepIntervalMsec / 1000f;
					formGraphics.FillRectangle(lightYellow, num4, 0f, num5 - num4, ChartHeight);
					num4 = ChartXMiddle + ((float)int.Parse(((Control)StarDiameter_Analyser.txtTop1D).get_Text()) - Toffset - (float)(NumObserved / 2)) * TimeAxisScale * VideoStepIntervalMsec / 1000f;
					formGraphics.DrawLine(pen3, num4, 0f, num4, ChartHeight);
					num4 = ChartXMiddle + ((float)int.Parse(((Control)StarDiameter_Analyser.txtTop2D).get_Text()) - Toffset - (float)(NumObserved / 2)) * TimeAxisScale * VideoStepIntervalMsec / 1000f;
					formGraphics.DrawLine(pen3, num4, 0f, num4, ChartHeight);
					num4 = ChartXMiddle + ((float)int.Parse(((Control)StarDiameter_Analyser.txtBottom1D).get_Text()) - Toffset - (float)(NumObserved / 2)) * TimeAxisScale * VideoStepIntervalMsec / 1000f;
					formGraphics.DrawLine(pen5, num4, 0f, num4, ChartHeight);
					num4 = ChartXMiddle + ((float)int.Parse(((Control)StarDiameter_Analyser.txtBottom2D).get_Text()) - Toffset - (float)(NumObserved / 2)) * TimeAxisScale * VideoStepIntervalMsec / 1000f;
					formGraphics.DrawLine(pen5, num4, 0f, num4, ChartHeight);
				}
				else
				{
					float num4 = ChartXMiddle + ((float)int.Parse(((Control)StarDiameter_Analyser.txtMeas1R).get_Text()) - Toffset - (float)(NumObserved / 2)) * TimeAxisScale * VideoStepIntervalMsec / 1000f;
					float num5 = ChartXMiddle + ((float)int.Parse(((Control)StarDiameter_Analyser.txtMeas2R).get_Text()) - Toffset - (float)(NumObserved / 2)) * TimeAxisScale * VideoStepIntervalMsec / 1000f;
					formGraphics.FillRectangle(lightYellow, num4, 0f, num5 - num4, ChartHeight);
					num4 = ChartXMiddle + ((float)int.Parse(((Control)StarDiameter_Analyser.txtTop1R).get_Text()) - Toffset - (float)(NumObserved / 2)) * TimeAxisScale * VideoStepIntervalMsec / 1000f;
					formGraphics.DrawLine(pen3, num4, 0f, num4, ChartHeight);
					num4 = ChartXMiddle + ((float)int.Parse(((Control)StarDiameter_Analyser.txtTop2R).get_Text()) - Toffset - (float)(NumObserved / 2)) * TimeAxisScale * VideoStepIntervalMsec / 1000f;
					formGraphics.DrawLine(pen3, num4, 0f, num4, ChartHeight);
					num4 = ChartXMiddle + ((float)int.Parse(((Control)StarDiameter_Analyser.txtBottom1R).get_Text()) - Toffset - (float)(NumObserved / 2)) * TimeAxisScale * VideoStepIntervalMsec / 1000f;
					formGraphics.DrawLine(pen5, num4, 0f, num4, ChartHeight);
					num4 = ChartXMiddle + ((float)int.Parse(((Control)StarDiameter_Analyser.txtBottom2R).get_Text()) - Toffset - (float)(NumObserved / 2)) * TimeAxisScale * VideoStepIntervalMsec / 1000f;
					formGraphics.DrawLine(pen5, num4, 0f, num4, ChartHeight);
				}
			}
			formGraphics.DrawRectangle(pen, 0, 0, ChartWidth - 1, ChartHeight - 1);
			formGraphics.DrawLine(pen2, 0f, num3, ChartWidth, num3);
			for (float num6 = 0f - NumOfSecsDisplayed; num6 < NumOfSecsDisplayed; num6 += 0.2f)
			{
				num = 2;
				if (((double)Math.Abs(num6 % 1f) < 0.005) | ((double)Math.Abs(num6 % 1f) > 0.995))
				{
					num = 5;
				}
				formGraphics.DrawLine(pen2, ChartXMiddle + num6 * TimeAxisScale, num3 + (float)num, ChartXMiddle + num6 * TimeAxisScale, num3 - (float)num);
			}
			if (StarDiameter_Analyser.chkLightCurve.get_Checked())
			{
				for (int i = 0; i < 6000; i++)
				{
					float num4 = ChartXMiddle + (float)(i - 3000) / 10f * TimeAxisScale * VideoStepIntervalMsec / 1000f;
					float num7 = Theoretical[i] * num2 * PlotScale + num3;
					if (i > 0)
					{
						formGraphics.DrawLine(pen, x, y, num4, num7);
					}
					x = num4;
					y = num7;
				}
			}
			for (int j = 0; j < NumObserved; j++)
			{
				float num4 = ChartXMiddle + ((float)j - Toffset - (float)(NumObserved / 2)) * TimeAxisScale * VideoStepIntervalMsec / 1000f;
				float num7 = Observed[j] * num2 * PlotScale + num3;
				formGraphics.FillEllipse(darkBlue, num4 - 2f, num7 - 2f, 4f, 4f);
			}
		}

		internal static int LocalPlotPointFromMap_X(float x)
		{
			return (int)(1000f * (x - ChartXMiddle) / TimeAxisScale / VideoStepIntervalMsec);
		}
	}
}
