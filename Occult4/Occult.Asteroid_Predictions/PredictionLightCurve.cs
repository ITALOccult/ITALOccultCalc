using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Asteroid_Predictions
{
	public class PredictionLightCurve : Form
	{
		internal bool DiameterChanging;

		private bool IsPlotting;

		private float[] OccBrightnessAcross = new float[201];

		private float[] OccBrightnessAlong = new float[201];

		private const decimal Convert_ParallaxToKM_At_1AU = 725.270943802154m;

		private decimal km_at_1AU_mas = 1.378795m;

		private PredictionLightCurveModel LCM = new PredictionLightCurveModel();

		private const int ArraySize = 201;

		private const int ArrayOffset = 100;

		private int[,] StarModel = new int[201, 201];

		private bool[,] AsteroidModel = new bool[201, 201];

		private int FullIllumination;

		public string ObjectID = "";

		private int xMax = -100;

		private int yMax = -100;

		private int xMin = 100;

		private int yMin = 100;

		internal double PAofPathDirection = 90.0;

		private double Star_Mag;

		private float Duration = 1f;

		internal float TopoKmRatio = 1f;

		private IContainer components;

		private PictureBox picAcrossPath;

		private PictureBox picAlongPath;

		private TrackBar TBarAcross;

		private Label label1;

		private Label label2;

		internal NumericUpDown updnStarDiameter;

		private Label label3;

		internal NumericUpDown updnAsteroidDia_km;

		internal NumericUpDown updnAsteroidDiameter_mas;

		private Label label4;

		private Label label6;

		internal NumericUpDown updnParallax;

		private GroupBox grpStar;

		internal NumericUpDown updnStarMag;

		private Label label7;

		private GroupBox grpAsteroid;

		internal NumericUpDown updnAsteroidMag;

		private Label label8;

		private Panel pnlCurves;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem acrosspathLightCurveToolStripMenuItem;

		private ToolStripMenuItem alongpathLightCurveToolStripMenuItem;

		private ToolStripMenuItem bothLightCurvesToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem acrossPathToolStripMenuItem;

		private ToolStripMenuItem alongpathLightCurveToolStripMenuItem1;

		private ToolStripMenuItem bothLightCurvesToolStripMenuItem1;

		internal Label lblEvent;

		private CheckBox chkLightLevels;

		private CheckBox chkKm_Time;

		private GroupBox grpDurn;

		private NumericUpDown updnLimbDarkening;

		private Label label11;

		private Label label12;

		private Panel panel1;

		internal Label lblPathPA;

		private Label label15;

		internal NumericUpDown updnPA;

		internal NumericUpDown updnMinor_mas;

		internal NumericUpDown updnMinor_km;

		private Label label16;

		private Label label14;

		private Label label13;

		private Label label17;

		private Panel panel2;

		private Label label18;

		internal RadioButton optRectangle;

		internal RadioButton optEllipse;

		private Label label5;

		private Label label19;

		internal NumericUpDown updn_masPERsec;

		private Label label20;

		private Label lblDuration;

		internal CheckBox chkTopoDistances;

		private Label label10;

		private Label label9;

		private CheckBox chkMagLevels;

		private Panel panel3;

		internal TrackBar trackOpacity;

		private Label label22;

		private Label label23;

		private Label label24;

		private Label label25;

		private Label label26;

		private Label label27;

		private Panel pnlOpacity;

		private ListBox listBox1;

		private Label label21;

		private Panel panel4;

		public PredictionLightCurve()
		{
			InitializeComponent();
			if (!Utilities.IsInVisualStudio)
			{
				((Control)this).set_Height(514);
			}
			Plot(GenerateModel: true);
			ReSize();
		}

		private void TBarAcross_Scroll(object sender, EventArgs e)
		{
			Plot(GenerateModel: false);
		}

		private void updnAsteroidDia_km_ValueChanged(object sender, EventArgs e)
		{
			if (!DiameterChanging)
			{
				DiameterChanging = true;
				try
				{
					updnAsteroidDiameter_mas.set_Value(Math.Round(updnAsteroidDia_km.get_Value() * km_at_1AU_mas * updnParallax.get_Value() / 8.79414383618253m * 10m) / 10m);
				}
				catch
				{
				}
				if (updnMinor_mas.get_Value() > updnAsteroidDiameter_mas.get_Value())
				{
					updnMinor_mas.set_Value(updnAsteroidDiameter_mas.get_Value());
					updnMinor_km.set_Value(updnAsteroidDia_km.get_Value());
				}
				if (updnAsteroidDia_km.get_Value() <= 10m)
				{
					updnAsteroidDia_km.set_Increment(0.1m);
				}
				else if (updnAsteroidDia_km.get_Value() <= 20m)
				{
					updnAsteroidDia_km.set_Increment(0.5m);
				}
				else
				{
					updnAsteroidDia_km.set_Increment(1m);
				}
				DiameterChanging = false;
				SetDuration();
				Plot(GenerateModel: true);
			}
		}

		private void updnParallax_ValueChanged(object sender, EventArgs e)
		{
			if (!DiameterChanging)
			{
				DiameterChanging = true;
				updnAsteroidDiameter_mas.set_Value(Math.Round(updnAsteroidDia_km.get_Value() * km_at_1AU_mas * updnParallax.get_Value() / 8.79414383618253m * 10m) / 10m);
				DiameterChanging = false;
				Plot(GenerateModel: true);
			}
		}

		private void updnAsteroidDiameter_mas_ValueChanged(object sender, EventArgs e)
		{
			if (!DiameterChanging)
			{
				DiameterChanging = true;
				updnAsteroidDia_km.set_Value(updnAsteroidDiameter_mas.get_Value() / (km_at_1AU_mas * updnParallax.get_Value() / 8.79414383618253m));
				if (updnMinor_mas.get_Value() > updnAsteroidDiameter_mas.get_Value())
				{
					updnMinor_mas.set_Value(updnAsteroidDiameter_mas.get_Value());
					updnMinor_km.set_Value(updnAsteroidDia_km.get_Value());
				}
				DiameterChanging = false;
				if (updnAsteroidDiameter_mas.get_Value() <= 2m)
				{
					updnAsteroidDiameter_mas.set_Increment(0.1m);
				}
				else if (updnAsteroidDiameter_mas.get_Value() <= 10m)
				{
					updnAsteroidDiameter_mas.set_Increment(0.5m);
				}
				else
				{
					updnAsteroidDiameter_mas.set_Increment(1m);
				}
				SetDuration();
				Plot(GenerateModel: true);
			}
		}

		private void updnPA_ValueChanged(object sender, EventArgs e)
		{
			Plot(GenerateModel: true);
		}

		private void updnMinor_mas_ValueChanged(object sender, EventArgs e)
		{
			if (!DiameterChanging)
			{
				DiameterChanging = true;
				updnMinor_km.set_Value(updnMinor_mas.get_Value() / (km_at_1AU_mas * updnParallax.get_Value() / 8.79414383618253m));
				if (updnMinor_mas.get_Value() > updnAsteroidDiameter_mas.get_Value())
				{
					updnAsteroidDiameter_mas.set_Value(updnMinor_mas.get_Value());
					updnAsteroidDia_km.set_Value(updnMinor_km.get_Value());
				}
				DiameterChanging = false;
				if (updnMinor_mas.get_Value() <= 2m)
				{
					updnMinor_mas.set_Increment(0.1m);
				}
				else if (updnMinor_mas.get_Value() <= 10m)
				{
					updnMinor_mas.set_Increment(0.5m);
				}
				else
				{
					updnMinor_mas.set_Increment(1m);
				}
				SetDuration();
				Plot(GenerateModel: true);
			}
		}

		private void updnMinor_km_ValueChanged(object sender, EventArgs e)
		{
			if (!DiameterChanging)
			{
				DiameterChanging = true;
				try
				{
					updnMinor_mas.set_Value(Math.Round(updnMinor_km.get_Value() * km_at_1AU_mas * updnParallax.get_Value() / 8.79414383618253m * 10m) / 10m);
				}
				catch
				{
				}
				if (updnMinor_mas.get_Value() > updnAsteroidDiameter_mas.get_Value())
				{
					updnAsteroidDiameter_mas.set_Value(updnMinor_mas.get_Value());
					updnAsteroidDia_km.set_Value(updnMinor_km.get_Value());
				}
				DiameterChanging = false;
				if (updnMinor_km.get_Value() <= 2m)
				{
					updnMinor_km.set_Increment(0.1m);
				}
				else if (updnMinor_km.get_Value() <= 10m)
				{
					updnMinor_km.set_Increment(0.5m);
				}
				else
				{
					updnMinor_km.set_Increment(1m);
				}
				SetDuration();
				Plot(GenerateModel: true);
			}
		}

		internal void Plot(bool GenerateModel)
		{
			if (!IsPlotting)
			{
				IsPlotting = true;
				_ = (double)updnStarDiameter.get_Value() / 2.0;
				_ = 0.0;
				_ = (double)updnAsteroidDiameter_mas.get_Value() / 2.0;
				Star_Mag = (double)updnStarMag.get_Value();
				double num = (double)updnAsteroidMag.get_Value();
				PredictionLightCurveModel.BrightnessRatio_Asteroid_over_Star = (float)Math.Pow(10.0, (Star_Mag - num) / 2.5);
				if (GenerateModel)
				{
					CreateLightCurveModel();
				}
				LCM.LightCurve(ref OccBrightnessAcross, Along: false, 0);
				PlotAcrossAlongPath();
				IsPlotting = false;
			}
		}

		internal void PlotAcrossAlongPath()
		{
			Pen pen = new Pen(Color.Black);
			Pen pen2 = new Pen(ColorTranslator.FromHtml("#18000080"), 1f);
			Pen pen3 = new Pen(ColorTranslator.FromHtml("#200080FF"), 3f);
			Pen pen4 = new Pen(ColorTranslator.FromHtml("#30005000"), 1f);
			Pen pen5 = new Pen(ColorTranslator.FromHtml("#20FF0000"), 1f);
			_ = new float[2] { 3f, 25f };
			Pen pen6 = new Pen(Brushes.Black, 1f);
			Pen pen7 = new Pen(Brushes.Red, 1f);
			Font font = new Font("Courier New", 8f);
			Font font2 = new Font("Arial", 8f, FontStyle.Bold);
			Font font3 = new Font("Arial", 7f);
			Font font4 = new Font("Arial", 7f, FontStyle.Bold);
			float num = (float)updnAsteroidDiameter_mas.get_Value() / ((float)updnStarDiameter.get_Value() + (float)updnAsteroidDiameter_mas.get_Value());
			float num2 = 85f * num / (float)updnAsteroidDia_km.get_Value() * 2f;
			if (chkTopoDistances.get_Checked())
			{
				num2 /= TopoKmRatio;
			}
			float num3 = (float)updnAsteroidDia_km.get_Value() / num / 2f / 0.8f;
			float num4 = 0.2f;
			float num5 = (float)((Control)picAcrossPath).get_Width() / 450f;
			if (num3 > 200f * num5)
			{
				num4 = 60f;
			}
			else if (num3 > 150f * num5)
			{
				num4 = 50f;
			}
			else if (num3 > 120f * num5)
			{
				num4 = 40f;
			}
			else if (num3 > 90f * num5)
			{
				num4 = 30f;
			}
			else if (num3 > 60f * num5)
			{
				num4 = 20f;
			}
			else if (num3 > 30f * num5)
			{
				num4 = 10f;
			}
			else if (num3 > 24f * num5)
			{
				num4 = 8f;
			}
			else if (num3 > 15f * num5)
			{
				num4 = 5f;
			}
			else if (num3 > 6f * num5)
			{
				num4 = 2f;
			}
			else if (num3 > 2.5f * num5)
			{
				num4 = 1f;
			}
			else if (num3 > 1f * num5)
			{
				num4 = 0.5f;
			}
			float num6 = Duration / 85f * 50f;
			float num7 = 100f / num6;
			float num8 = 0.05f;
			if (num6 > 200f * num5)
			{
				num8 = 60f;
			}
			else if (num6 > 150f * num5)
			{
				num8 = 50f;
			}
			else if (num6 > 120f * num5)
			{
				num8 = 40f;
			}
			else if (num6 > 90f * num5)
			{
				num8 = 30f;
			}
			else if (num6 > 60f * num5)
			{
				num8 = 20f;
			}
			else if (num6 > 30f * num5)
			{
				num8 = 10f;
			}
			else if (num6 > 24f * num5)
			{
				num8 = 8f;
			}
			else if (num6 > 15f * num5)
			{
				num8 = 5f;
			}
			else if (num6 > 6f * num5)
			{
				num8 = 2f;
			}
			else if (num6 > 3f * num5)
			{
				num8 = 1f;
			}
			else if ((double)num6 > 1.5 * (double)num5)
			{
				num8 = 0.5f;
			}
			else if ((double)num6 > 0.6 * (double)num5)
			{
				num8 = 0.2f;
			}
			else if ((double)num6 > 0.3 * (double)num5)
			{
				num8 = 0.1f;
			}
			else if ((double)num6 > 0.15 * (double)num5)
			{
				num8 = 0.05f;
			}
			float num9 = 0.9f;
			SizeF sizeF = default(SizeF);
			float[] OccBrightness = new float[201];
			int value = TBarAcross.get_Value();
			LCM.LightCurve(ref OccBrightness, Along: true, value);
			for (int i = 0; i <= 1; i++)
			{
				int height;
				int width;
				if (i == 0)
				{
					height = ((Control)picAcrossPath).get_Height();
					width = ((Control)picAcrossPath).get_Width();
				}
				else
				{
					height = ((Control)picAlongPath).get_Height();
					width = ((Control)picAlongPath).get_Width();
				}
				Bitmap image = new Bitmap(width, height);
				height--;
				width--;
				Graphics graphics = Graphics.FromImage(image);
				graphics.SmoothingMode = SmoothingMode.AntiAlias;
				graphics.Clear(Color.Beige);
				graphics.FillRectangle(Brushes.Moccasin, 0f, 0f, width, 0.05f * (float)height);
				graphics.FillRectangle(Brushes.Moccasin, 0f, 0.95f * (float)height, width, 0.5f * (float)height);
				pen.Color = Color.Black;
				graphics.DrawRectangle(pen, 0, 0, width, height);
				float num10 = (float)width / 200f;
				float num11 = num9 * (float)height;
				float num12 = 0.2f;
				if (num11 > 350f)
				{
					num12 = 0.1f;
				}
				for (float num13 = 0.9f; num13 > 0f; num13 -= num12)
				{
					float num14 = 0.05f * (float)height + (1f - num13) * num11;
					if (chkLightLevels.get_Checked())
					{
						graphics.DrawLine(pen4, 0f, num14, width, num14);
					}
					string text = $"{num13:.0}";
					graphics.DrawString(y: num14 - graphics.MeasureString(text, font).Height / 2f, s: text, font: font, brush: Brushes.Black, x: 2f);
					graphics.DrawLine(pen6, 0f, num14, 4f, num14);
				}
				double[] array = new double[7] { 0.2, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0 };
				foreach (double num15 in array)
				{
					if (!((num11 < 350f) & (num15 % 1.0 != 0.0)))
					{
						float num14 = 0.05f * (float)height + (float)(1.0 - Math.Pow(10.0, (0.0 - num15) / 2.5)) * num11;
						if (chkMagLevels.get_Checked())
						{
							graphics.DrawLine(pen5, 0f, num14, width, num14);
						}
						string text = ((!(num11 < 350f)) ? string.Format("{0,0:f1}", num15) : string.Format("{0,0:f0}", num15));
						sizeF = graphics.MeasureString(text, font);
						graphics.DrawString(text, font, Brushes.Black, (float)(width - 4) - sizeF.Width, num14 - sizeF.Height / 2f);
					}
				}
				string text2 = string.Format("dia: ✶={0,1:f0}mas", updnStarDiameter.get_Value());
				string text3 = string.Format("  ({0})={1,1:f0}x{2,1:f0}mas", ObjectID, updnAsteroidDiameter_mas.get_Value(), updnMinor_mas.get_Value());
				string text4 = $"Light drop";
				_ = OccBrightnessAcross[100];
				string text5 = "0";
				if ((double)(100f - OccBrightnessAcross[100] * 100f) > 98.5)
				{
					text5 = "1";
				}
				string text6 = string.Format("  {0,1:f" + text5 + "}%, to mag {1,1:f1}", 100f - OccBrightnessAcross[100] * 100f, Star_Mag - 2.5 * Math.Log10(OccBrightnessAcross[100]));
				if (i == 0)
				{
					for (float num16 = 0f; num16 <= 400f; num16 += num4)
					{
						float num17 = 100f + num16 * num2;
						float num18 = 100f - num16 * num2;
						if (num17 < 0f)
						{
							break;
						}
						if (chkKm_Time.get_Checked())
						{
							if (num16 == 0f)
							{
								graphics.DrawLine(pen3, num17 * num10, 0f, num17 * num10, height);
							}
							else
							{
								graphics.DrawLine(pen2, num17 * num10, 0f, num17 * num10, height);
								graphics.DrawLine(pen2, num18 * num10, 0f, num18 * num10, height);
							}
						}
						graphics.DrawLine(pen6, num17 * num10, 0f, num17 * num10, 4f);
						graphics.DrawLine(pen6, num18 * num10, 0f, num18 * num10, 4f);
						graphics.DrawLine(pen6, num17 * num10, height, num17 * num10, height - 4);
						graphics.DrawLine(pen6, num18 * num10, height, num18 * num10, height - 4);
						string text = ((!(num4 >= 1f)) ? string.Format("{0,1:f1}km", num16) : $"{num16:0}km");
						sizeF = graphics.MeasureString(text, font);
						graphics.DrawString(text, font, Brushes.Black, num17 * num10 - sizeF.Width / 2f, 2f);
						graphics.DrawString(text, font, Brushes.Black, num18 * num10 - sizeF.Width / 2f, 2f);
						graphics.DrawString(text, font, Brushes.Black, num17 * num10 - sizeF.Width / 2f, (float)(height - 3) - sizeF.Height);
						graphics.DrawString(text, font, Brushes.Black, num18 * num10 - sizeF.Width / 2f, (float)(height - 3) - sizeF.Height);
					}
					graphics.DrawString(x: 100f * num10 - graphics.MeasureString(text2, font3).Width + 2f, s: text2 + text3, font: font3, brush: Brushes.Black, y: 20f);
					graphics.DrawString(x: 100f * num10 - graphics.MeasureString(text4, font3).Width + 2f, s: text4 + text6, font: font3, brush: Brushes.Black, y: 30f);
					graphics.DrawString("LEFT", font2, Brushes.Black, 30f, 0.95f * (float)height - 23f);
					graphics.DrawString(x: (float)(width - 40) - graphics.MeasureString("RIGHT", font2).Width, s: "RIGHT", font: font2, brush: Brushes.Black, y: 0.95f * (float)height - 23f);
				}
				else
				{
					for (float num19 = 0f; num19 <= 400f; num19 += num8)
					{
						float num20 = 100f + num19 * num7;
						float num21 = 100f - num19 * num7;
						if (num20 < 0f)
						{
							break;
						}
						if (chkKm_Time.get_Checked())
						{
							if (num19 == 0f)
							{
								graphics.DrawLine(pen3, num20 * num10, 0f, num20 * num10, height);
							}
							else
							{
								graphics.DrawLine(pen2, num20 * num10, 0f, num20 * num10, height);
								graphics.DrawLine(pen2, num21 * num10, 0f, num21 * num10, height);
							}
						}
						graphics.DrawLine(pen6, num20 * num10, 0f, num20 * num10, 4f);
						graphics.DrawLine(pen6, num21 * num10, 0f, num21 * num10, 4f);
						graphics.DrawLine(pen6, num20 * num10, height, num20 * num10, height - 4);
						graphics.DrawLine(pen6, num21 * num10, height, num21 * num10, height - 4);
						string text = ((!(num8 >= 1f)) ? ((!(num8 >= 0.5f)) ? string.Format("{0,1:f2}s", num19) : string.Format("{0,0:f1}s", num19)) : $"{num19:0}s");
						sizeF = graphics.MeasureString(text, font);
						graphics.DrawString(text, font, Brushes.Black, num20 * num10 - sizeF.Width / 2f, 2f);
						graphics.DrawString(text, font, Brushes.Black, num20 * num10 - sizeF.Width / 2f, (float)(height - 3) - sizeF.Height);
						text = ((!(num8 >= 1f)) ? ((!(num8 >= 0.5f)) ? string.Format("{0,1:f2}s", 0f - num19) : string.Format("{0,0:f1}s", 0f - num19)) : $"{0f - num19:0}s");
						sizeF = graphics.MeasureString(text, font);
						graphics.DrawString(text, font, Brushes.Black, num21 * num10 - sizeF.Width / 2f, 2f);
						graphics.DrawString(text, font, Brushes.Black, num21 * num10 - sizeF.Width / 2f, (float)(height - 3) - sizeF.Height);
					}
					graphics.DrawString(x: 100f * num10 - graphics.MeasureString(text2, font3).Width, s: text2 + text3, font: font3, brush: Brushes.Black, y: 20f);
				}
				ArrayList arrayList = new ArrayList();
				for (int k = 0; k <= 200; k++)
				{
					float x5 = (float)k * num10;
					float num14 = ((i != 0) ? (0.05f * (float)height + (1f - OccBrightness[k]) * num11) : (0.05f * (float)height + (1f - OccBrightnessAcross[k]) * num11));
					arrayList.Add(new PointF(x5, num14));
				}
				PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
				try
				{
					graphics.DrawLines(pen6, points);
				}
				catch
				{
				}
				if (i == 0)
				{
					float x5 = (float)(TBarAcross.get_Value() + 100) * num10;
					graphics.DrawLine(pen7, x5, 1f, x5, height - 1);
					string text = ((!(num2 > 8f)) ? string.Format("{0,1:f0}", Math.Abs((float)TBarAcross.get_Value() / num2)) : string.Format("{0,1:f1}", Math.Abs((float)TBarAcross.get_Value() / num2)));
					sizeF = graphics.MeasureString(text, font);
					graphics.FillRectangle(Brushes.Moccasin, x5 - sizeF.Width / 2f + 2f, 0.95f * (float)height - 12f, sizeF.Width - 3f, sizeF.Height - 3f);
					graphics.DrawString(text, font4, Brushes.DarkBlue, x5 - sizeF.Width / 2f + 2f, 0.95f * (float)height - 12f);
				}
				if (i == 0)
				{
					picAcrossPath.set_Image((Image)image);
				}
				else
				{
					picAlongPath.set_Image((Image)image);
				}
				graphics.Dispose();
			}
		}

		private void updnStarMag_ValueChanged(object sender, EventArgs e)
		{
			Plot(GenerateModel: false);
		}

		private void updnAsteroidMag_ValueChanged(object sender, EventArgs e)
		{
			Plot(GenerateModel: false);
		}

		private void updnStarDiameter_ValueChanged(object sender, EventArgs e)
		{
			SetDuration();
			Plot(GenerateModel: true);
			if (updnStarDiameter.get_Value() <= 2m)
			{
				updnStarDiameter.set_Increment(0.1m);
			}
			else if (updnStarDiameter.get_Value() <= 10m)
			{
				updnStarDiameter.set_Increment(0.5m);
			}
			else
			{
				updnStarDiameter.set_Increment(1m);
			}
		}

		private void acrosspathLightCurveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_002d: Unknown result type (might be due to invalid IL or missing references)
			try
			{
				Output.SaveGraphic(picAcrossPath.get_Image(), "Test_Across_Path", Utilities.AppPath + "/Generated Files");
			}
			catch
			{
				MessageBox.Show("Error - Across Path file not saved");
			}
		}

		private void alongpathLightCurveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_002d: Unknown result type (might be due to invalid IL or missing references)
			try
			{
				Output.SaveGraphic(picAlongPath.get_Image(), "Test_Along_Path", Utilities.AppPath + "/Generated Files");
			}
			catch
			{
				MessageBox.Show("Error - Along path file not saved");
			}
		}

		private void bothLightCurvesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_008e: Unknown result type (might be due to invalid IL or missing references)
			if (Utilities.ScreenSizeTooLargeForImageSaves())
			{
				return;
			}
			Bitmap bitmap = new Bitmap(((Control)pnlCurves).get_Width(), ((Control)pnlCurves).get_Height());
			Graphics.FromImage(bitmap).CopyFromScreen(((Form)this).get_Location().X + ((Control)pnlCurves).get_Left(), ((Form)this).get_Location().Y + ((Control)pnlCurves).get_Top(), 0, 0, bitmap.Size);
			try
			{
				Output.SaveGraphic(bitmap, "Test_Across_Along_Paths", Utilities.AppPath + "/Generated Files");
			}
			catch
			{
				MessageBox.Show("Error - file not saved");
			}
		}

		private void acrossPathToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picAcrossPath.get_Image());
		}

		private void alongpathLightCurveToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picAlongPath.get_Image());
		}

		private void bothLightCurvesToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			//IL_0079: Unknown result type (might be due to invalid IL or missing references)
			if (Utilities.ScreenSizeTooLargeForImageSaves())
			{
				return;
			}
			Bitmap bitmap = new Bitmap(((Control)pnlCurves).get_Width(), ((Control)pnlCurves).get_Height());
			Graphics.FromImage(bitmap).CopyFromScreen(((Form)this).get_Location().X + ((Control)pnlCurves).get_Left(), ((Form)this).get_Location().Y + ((Control)pnlCurves).get_Top(), 0, 0, bitmap.Size);
			try
			{
				Clipboard.SetImage((Image)bitmap);
			}
			catch
			{
				MessageBox.Show("Error - image not copied");
			}
		}

		private void PredictionLightCurve_Resize(object sender, EventArgs e)
		{
			ReSize();
		}

		internal void ReSize()
		{
			int num = 10;
			((Control)pnlCurves).set_Width(((Control)this).get_Width() - 34);
			((Control)pnlCurves).set_Height(((Control)this).get_Height() - 250);
			if (((Control)this).get_Height() < 400)
			{
				((Control)this).set_Height(400);
			}
			int num2 = (((Control)this).get_Width() - 12 - ((Control)grpAsteroid).get_Width() - ((Control)grpDurn).get_Width() - ((Control)grpStar).get_Width()) / 4;
			if (num2 <= 2)
			{
				num2 = 2;
			}
			((Control)grpDurn).set_Left(num2);
			((Control)grpStar).set_Left(((Control)grpDurn).get_Right() + num2);
			((Control)grpAsteroid).set_Left(((Control)grpStar).get_Right() + num2);
			((Control)picAcrossPath).set_Left(num);
			GroupBox obj = grpDurn;
			GroupBox obj2 = grpAsteroid;
			int num3;
			((Control)grpStar).set_Top(num3 = ((Control)this).get_Height() - ((Control)grpAsteroid).get_Height() - 46);
			int top;
			((Control)obj2).set_Top(top = num3);
			((Control)obj).set_Top(top);
			((Control)picAlongPath).set_Left(((Control)pnlCurves).get_Width() / 2 + num);
			PictureBox obj3 = picAcrossPath;
			((Control)picAlongPath).set_Width(top = ((Control)pnlCurves).get_Width() / 2 - 2 * num);
			((Control)obj3).set_Width(top);
			PictureBox obj4 = picAcrossPath;
			((Control)picAlongPath).set_Height(top = ((Control)pnlCurves).get_Height() - 50);
			((Control)obj4).set_Height(top);
			((Control)TBarAcross).set_Left(((Control)picAcrossPath).get_Left() - 9);
			((Control)TBarAcross).set_Top(((Control)picAcrossPath).get_Top() + ((Control)picAcrossPath).get_Height() - 8);
			((Control)TBarAcross).set_Width(((Control)picAcrossPath).get_Width() + 18);
			((Control)label1).set_Left(((Control)picAcrossPath).get_Left() + (((Control)picAcrossPath).get_Width() - ((Control)label1).get_Width()) / 2);
			((Control)label2).set_Left(((Control)picAlongPath).get_Left() + (((Control)picAlongPath).get_Width() - ((Control)label2).get_Width()) / 2);
			((Control)lblEvent).set_Left((((Control)this).get_Width() - ((Control)lblEvent).get_Width()) / 2);
			((Control)pnlOpacity).set_Left(((Control)this).get_Width() - ((Control)pnlOpacity).get_Width() - 10);
			Plot(GenerateModel: false);
		}

		private void chkLightLevels_CheckedChanged(object sender, EventArgs e)
		{
			Plot(GenerateModel: false);
		}

		private void updnLimbDarkening_ValueChanged(object sender, EventArgs e)
		{
			Plot(GenerateModel: true);
		}

		private void optEllipse_CheckedChanged(object sender, EventArgs e)
		{
			Plot(GenerateModel: true);
		}

		private void optRectangle_CheckedChanged(object sender, EventArgs e)
		{
			Plot(GenerateModel: true);
		}

		private void updn_masPERsec_ValueChanged(object sender, EventArgs e)
		{
			SetDuration();
			Plot(GenerateModel: false);
		}

		internal void SetDuration()
		{
			Duration = ((float)updnStarDiameter.get_Value() + (float)updnAsteroidDiameter_mas.get_Value()) / (float)updn_masPERsec.get_Value();
			((Control)lblDuration).set_Text(string.Format("Maximum duration  {0,1:f1} secs", Duration));
		}

		private void chkTopoDistances_CheckedChanged(object sender, EventArgs e)
		{
			Plot(GenerateModel: false);
		}

		private void chkMagLevels_CheckedChanged(object sender, EventArgs e)
		{
			Plot(GenerateModel: false);
		}

		private void updnAsteroidDia_km_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnAsteroidDia_km).Select(0, 10);
		}

		private void updnAsteroidDiameter_mas_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnAsteroidDiameter_mas).Select(0, 10);
		}

		private void updnMinor_km_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnMinor_km).Select(0, 10);
		}

		private void updnMinor_mas_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnMinor_mas).Select(0, 10);
		}

		private void updnPA_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnPA).Select(0, 10);
		}

		private void updnStarDiameter_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnStarDiameter).Select(0, 10);
		}

		private void updnLimbDarkening_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnLimbDarkening).Select(0, 10);
		}

		private void updnStarMag_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnStarMag).Select(0, 10);
		}

		private void updn_masPERsec_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updn_masPERsec).Select(0, 10);
		}

		private void updnParallax_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnParallax).Select(0, 10);
		}

		private void updnAsteroidMag_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnAsteroidMag).Select(0, 10);
		}

		private void trackOpacity_ValueChanged(object sender, EventArgs e)
		{
			((Control)pnlOpacity).set_BackColor(Color.LightSeaGreen);
			((Form)this).set_Opacity((double)trackOpacity.get_Value() / 100.0);
			if (trackOpacity.get_Value() > 60)
			{
				Color lightSeaGreen;
				((ToolStrip)menuStrip1).set_BackColor(lightSeaGreen = Color.LightSeaGreen);
				((Control)this).set_BackColor(lightSeaGreen);
				((Control)trackOpacity).set_BackColor(Color.LightYellow);
			}
			else
			{
				MenuStrip obj = menuStrip1;
				Color color;
				((Control)trackOpacity).set_BackColor(color = Color.FromArgb(255, 255, 150 + trackOpacity.get_Value(), 0));
				Color lightSeaGreen;
				((ToolStrip)obj).set_BackColor(lightSeaGreen = color);
				((Control)this).set_BackColor(lightSeaGreen);
				((Control)trackOpacity).set_BackColor(Color.LightCyan);
			}
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void chkKm_Time_CheckedChanged(object sender, EventArgs e)
		{
			Plot(GenerateModel: false);
		}

		private void updnDuration_ValueChanged(object sender, EventArgs e)
		{
			Plot(GenerateModel: false);
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"LightCurve simulator");
		}

		private void CreateLightCurveModel()
		{
			double starDia_mas = (double)updnStarDiameter.get_Value();
			double limb_Darkening = (double)updnLimbDarkening.get_Value();
			double asteroidMajorAxis_mas = (double)updnAsteroidDiameter_mas.get_Value();
			double asteroidMinorAxis_mas = (double)updnMinor_mas.get_Value();
			double num = (double)updnPA.get_Value();
			bool @checked = optRectangle.get_Checked();
			double num2 = num - PAofPathDirection - 90.0;
			if (num2 > 180.0)
			{
				num2 -= 180.0;
			}
			if (num2 < 0.0)
			{
				num2 += 180.0;
			}
			LCM.CreateLightCurveArrays(starDia_mas, limb_Darkening, asteroidMajorAxis_mas, asteroidMinorAxis_mas, num2, @checked);
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
			//IL_06b7: Unknown result type (might be due to invalid IL or missing references)
			//IL_06c1: Expected O, but got Unknown
			//IL_0a22: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a2c: Expected O, but got Unknown
			//IL_0c70: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c7a: Expected O, but got Unknown
			//IL_0e20: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e2a: Expected O, but got Unknown
			//IL_0fea: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ff4: Expected O, but got Unknown
			//IL_121a: Unknown result type (might be due to invalid IL or missing references)
			//IL_1224: Expected O, but got Unknown
			//IL_19d0: Unknown result type (might be due to invalid IL or missing references)
			//IL_19da: Expected O, but got Unknown
			//IL_1c16: Unknown result type (might be due to invalid IL or missing references)
			//IL_1c20: Expected O, but got Unknown
			//IL_2bb2: Unknown result type (might be due to invalid IL or missing references)
			//IL_2bbc: Expected O, but got Unknown
			TBarAcross = new TrackBar();
			label1 = new Label();
			label2 = new Label();
			label3 = new Label();
			updnAsteroidDia_km = new NumericUpDown();
			updnAsteroidDiameter_mas = new NumericUpDown();
			label4 = new Label();
			label6 = new Label();
			updnParallax = new NumericUpDown();
			grpStar = new GroupBox();
			updnStarMag = new NumericUpDown();
			label11 = new Label();
			updnLimbDarkening = new NumericUpDown();
			label7 = new Label();
			updnStarDiameter = new NumericUpDown();
			grpAsteroid = new GroupBox();
			updnAsteroidMag = new NumericUpDown();
			label8 = new Label();
			panel1 = new Panel();
			label10 = new Label();
			label9 = new Label();
			panel2 = new Panel();
			label18 = new Label();
			optRectangle = new RadioButton();
			optEllipse = new RadioButton();
			lblPathPA = new Label();
			label15 = new Label();
			updnPA = new NumericUpDown();
			updnMinor_mas = new NumericUpDown();
			updnMinor_km = new NumericUpDown();
			label16 = new Label();
			label14 = new Label();
			label13 = new Label();
			label17 = new Label();
			label12 = new Label();
			label5 = new Label();
			picAlongPath = new PictureBox();
			picAcrossPath = new PictureBox();
			pnlCurves = new Panel();
			menuStrip1 = new MenuStrip();
			copyToolStripMenuItem = new ToolStripMenuItem();
			acrossPathToolStripMenuItem = new ToolStripMenuItem();
			alongpathLightCurveToolStripMenuItem1 = new ToolStripMenuItem();
			bothLightCurvesToolStripMenuItem1 = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			acrosspathLightCurveToolStripMenuItem = new ToolStripMenuItem();
			alongpathLightCurveToolStripMenuItem = new ToolStripMenuItem();
			bothLightCurvesToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			lblEvent = new Label();
			chkLightLevels = new CheckBox();
			chkKm_Time = new CheckBox();
			grpDurn = new GroupBox();
			chkMagLevels = new CheckBox();
			chkTopoDistances = new CheckBox();
			lblDuration = new Label();
			label19 = new Label();
			updn_masPERsec = new NumericUpDown();
			label20 = new Label();
			panel3 = new Panel();
			trackOpacity = new TrackBar();
			label22 = new Label();
			label23 = new Label();
			label24 = new Label();
			label25 = new Label();
			label26 = new Label();
			label27 = new Label();
			pnlOpacity = new Panel();
			label21 = new Label();
			listBox1 = new ListBox();
			panel4 = new Panel();
			((ISupportInitialize)TBarAcross).BeginInit();
			((ISupportInitialize)updnAsteroidDia_km).BeginInit();
			((ISupportInitialize)updnAsteroidDiameter_mas).BeginInit();
			((ISupportInitialize)updnParallax).BeginInit();
			((Control)grpStar).SuspendLayout();
			((ISupportInitialize)updnStarMag).BeginInit();
			((ISupportInitialize)updnLimbDarkening).BeginInit();
			((ISupportInitialize)updnStarDiameter).BeginInit();
			((Control)grpAsteroid).SuspendLayout();
			((ISupportInitialize)updnAsteroidMag).BeginInit();
			((Control)panel1).SuspendLayout();
			((Control)panel2).SuspendLayout();
			((ISupportInitialize)updnPA).BeginInit();
			((ISupportInitialize)updnMinor_mas).BeginInit();
			((ISupportInitialize)updnMinor_km).BeginInit();
			((ISupportInitialize)picAlongPath).BeginInit();
			((ISupportInitialize)picAcrossPath).BeginInit();
			((Control)pnlCurves).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((Control)grpDurn).SuspendLayout();
			((ISupportInitialize)updn_masPERsec).BeginInit();
			((Control)panel3).SuspendLayout();
			((ISupportInitialize)trackOpacity).BeginInit();
			((Control)pnlOpacity).SuspendLayout();
			((Control)panel4).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)TBarAcross).set_AutoSize(false);
			((Control)TBarAcross).set_BackColor(SystemColors.Control);
			TBarAcross.set_LargeChange(1);
			((Control)TBarAcross).set_Location(new Point(16, 291));
			TBarAcross.set_Maximum(100);
			TBarAcross.set_Minimum(-100);
			((Control)TBarAcross).set_Name("TBarAcross");
			((Control)TBarAcross).set_Size(new Size(351, 20));
			((Control)TBarAcross).set_TabIndex(2);
			TBarAcross.set_TickFrequency(10);
			TBarAcross.set_TickStyle((TickStyle)1);
			TBarAcross.add_Scroll((EventHandler)TBarAcross_Scroll);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_ForeColor(Color.DarkBlue);
			((Control)label1).set_Location(new Point(139, 2));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(106, 20));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text("Across-path");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label2).set_ForeColor(Color.DarkBlue);
			((Control)label2).set_Location(new Point(415, 2));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(293, 20));
			((Control)label2).set_TabIndex(1);
			((Control)label2).set_Text("Along-path, at Across-path location");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(9, 22));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(77, 13));
			((Control)label3).set_TabIndex(0);
			((Control)label3).set_Text("Diameter (mas)");
			((Control)updnAsteroidDia_km).set_BackColor(SystemColors.Window);
			((Control)updnAsteroidDia_km).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "LCsim_Major", true, (DataSourceUpdateMode)1));
			updnAsteroidDia_km.set_DecimalPlaces(1);
			((Control)updnAsteroidDia_km).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnAsteroidDia_km.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnAsteroidDia_km).set_Location(new Point(68, 33));
			updnAsteroidDia_km.set_Maximum(new decimal(new int[4] { 1000, 0, 0, 0 }));
			updnAsteroidDia_km.set_Minimum(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnAsteroidDia_km).set_Name("updnAsteroidDia_km");
			((Control)updnAsteroidDia_km).set_Size(new Size(58, 20));
			((Control)updnAsteroidDia_km).set_TabIndex(4);
			((UpDownBase)updnAsteroidDia_km).set_TextAlign((HorizontalAlignment)2);
			updnAsteroidDia_km.set_Value(Settings.Default.LCsim_Major);
			updnAsteroidDia_km.add_ValueChanged((EventHandler)updnAsteroidDia_km_ValueChanged);
			((Control)updnAsteroidDia_km).add_Enter((EventHandler)updnAsteroidDia_km_Enter);
			updnAsteroidDiameter_mas.set_DecimalPlaces(1);
			((Control)updnAsteroidDiameter_mas).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnAsteroidDiameter_mas.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnAsteroidDiameter_mas).set_Location(new Point(144, 33));
			updnAsteroidDiameter_mas.set_Maximum(new decimal(new int[4] { 400, 0, 0, 0 }));
			updnAsteroidDiameter_mas.set_Minimum(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnAsteroidDiameter_mas).set_Name("updnAsteroidDiameter_mas");
			((Control)updnAsteroidDiameter_mas).set_Size(new Size(45, 20));
			((Control)updnAsteroidDiameter_mas).set_TabIndex(5);
			((UpDownBase)updnAsteroidDiameter_mas).set_TextAlign((HorizontalAlignment)2);
			updnAsteroidDiameter_mas.set_Value(new decimal(new int[4] { 30, 0, 0, 0 }));
			updnAsteroidDiameter_mas.add_ValueChanged((EventHandler)updnAsteroidDiameter_mas_ValueChanged);
			((Control)updnAsteroidDiameter_mas).add_Enter((EventHandler)updnAsteroidDiameter_mas_Enter);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(145, 18));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(29, 13));
			((Control)label4).set_TabIndex(2);
			((Control)label4).set_Text("mas");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(223, 65));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(76, 13));
			((Control)label6).set_TabIndex(1);
			((Control)label6).set_Text("Parallax (asec)");
			((Control)updnParallax).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "LCsim_Parallax_asec", true, (DataSourceUpdateMode)1));
			updnParallax.set_DecimalPlaces(3);
			((Control)updnParallax).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnParallax.set_Increment(new decimal(new int[4] { 5, 0, 0, 131072 }));
			((Control)updnParallax).set_Location(new Point(300, 61));
			updnParallax.set_Maximum(new decimal(new int[4] { 40, 0, 0, 0 }));
			updnParallax.set_Minimum(new decimal(new int[4] { 1, 0, 0, 131072 }));
			((Control)updnParallax).set_Name("updnParallax");
			((Control)updnParallax).set_Size(new Size(60, 20));
			((Control)updnParallax).set_TabIndex(2);
			updnParallax.set_Value(Settings.Default.LCsim_Parallax_asec);
			updnParallax.add_ValueChanged((EventHandler)updnParallax_ValueChanged);
			((Control)updnParallax).add_Enter((EventHandler)updnParallax_Enter);
			((Control)grpStar).set_BackColor(Color.FromArgb(192, 255, 255));
			((Control)grpStar).get_Controls().Add((Control)(object)panel4);
			((Control)grpStar).get_Controls().Add((Control)(object)updnStarMag);
			((Control)grpStar).get_Controls().Add((Control)(object)label7);
			((Control)grpStar).get_Controls().Add((Control)(object)label3);
			((Control)grpStar).get_Controls().Add((Control)(object)updnStarDiameter);
			((Control)grpStar).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpStar).set_Location(new Point(213, 383));
			((Control)grpStar).set_Name("grpStar");
			((Control)grpStar).set_Size(new Size(145, 142));
			((Control)grpStar).set_TabIndex(2);
			grpStar.set_TabStop(false);
			((Control)grpStar).set_Text("Star details");
			((Control)updnStarMag).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "LCsim_MvStar", true, (DataSourceUpdateMode)1));
			updnStarMag.set_DecimalPlaces(1);
			((Control)updnStarMag).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnStarMag.set_Increment(new decimal(new int[4] { 2, 0, 0, 65536 }));
			((Control)updnStarMag).set_Location(new Point(88, 46));
			updnStarMag.set_Maximum(new decimal(new int[4] { 20, 0, 0, 0 }));
			updnStarMag.set_Minimum(new decimal(new int[4] { 1, 0, 0, -2147483648 }));
			((Control)updnStarMag).set_Name("updnStarMag");
			((Control)updnStarMag).set_Size(new Size(46, 20));
			((Control)updnStarMag).set_TabIndex(5);
			updnStarMag.set_Value(Settings.Default.LCsim_MvStar);
			updnStarMag.add_ValueChanged((EventHandler)updnStarMag_ValueChanged);
			((Control)updnStarMag).add_Enter((EventHandler)updnStarMag_Enter);
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label11).set_Location(new Point(1, 6));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(81, 13));
			((Control)label11).set_TabIndex(2);
			((Control)label11).set_Text("Limb Darkening");
			((Control)updnLimbDarkening).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "LCsim_Limb", true, (DataSourceUpdateMode)1));
			updnLimbDarkening.set_DecimalPlaces(2);
			((Control)updnLimbDarkening).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnLimbDarkening.set_Increment(new decimal(new int[4] { 25, 0, 0, 196608 }));
			((Control)updnLimbDarkening).set_Location(new Point(82, 2));
			updnLimbDarkening.set_Maximum(new decimal(new int[4] { 98, 0, 0, 131072 }));
			updnLimbDarkening.set_Minimum(new decimal(new int[4] { 1, 0, 0, -2147418112 }));
			((Control)updnLimbDarkening).set_Name("updnLimbDarkening");
			((Control)updnLimbDarkening).set_Size(new Size(46, 20));
			((Control)updnLimbDarkening).set_TabIndex(3);
			updnLimbDarkening.set_Value(Settings.Default.LCsim_Limb);
			updnLimbDarkening.add_ValueChanged((EventHandler)updnLimbDarkening_ValueChanged);
			((Control)updnLimbDarkening).add_Enter((EventHandler)updnLimbDarkening_Enter);
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(29, 50));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(57, 13));
			((Control)label7).set_TabIndex(4);
			((Control)label7).set_Text("Magnitude");
			((Control)updnStarDiameter).set_BackColor(SystemColors.Window);
			((Control)updnStarDiameter).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "LCsim_DiaStar", true, (DataSourceUpdateMode)1));
			updnStarDiameter.set_DecimalPlaces(2);
			((Control)updnStarDiameter).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnStarDiameter.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnStarDiameter).set_Location(new Point(88, 18));
			updnStarDiameter.set_Minimum(new decimal(new int[4] { 1, 0, 0, 131072 }));
			((Control)updnStarDiameter).set_Name("updnStarDiameter");
			((Control)updnStarDiameter).set_Size(new Size(46, 20));
			((Control)updnStarDiameter).set_TabIndex(1);
			updnStarDiameter.set_Value(Settings.Default.LCsim_DiaStar);
			updnStarDiameter.add_ValueChanged((EventHandler)updnStarDiameter_ValueChanged);
			((Control)updnStarDiameter).add_Enter((EventHandler)updnStarDiameter_Enter);
			((Control)grpAsteroid).set_BackColor(Color.FromArgb(255, 224, 192));
			((Control)grpAsteroid).get_Controls().Add((Control)(object)label6);
			((Control)grpAsteroid).get_Controls().Add((Control)(object)updnParallax);
			((Control)grpAsteroid).get_Controls().Add((Control)(object)updnAsteroidMag);
			((Control)grpAsteroid).get_Controls().Add((Control)(object)label8);
			((Control)grpAsteroid).get_Controls().Add((Control)(object)panel1);
			((Control)grpAsteroid).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpAsteroid).set_Location(new Point(388, 383));
			((Control)grpAsteroid).set_Name("grpAsteroid");
			((Control)grpAsteroid).set_Size(new Size(373, 142));
			((Control)grpAsteroid).set_TabIndex(3);
			grpAsteroid.set_TabStop(false);
			((Control)grpAsteroid).set_Text("Asteroid details");
			((Control)updnAsteroidMag).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "LCsim_MvAst", true, (DataSourceUpdateMode)1));
			updnAsteroidMag.set_DecimalPlaces(1);
			((Control)updnAsteroidMag).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnAsteroidMag.set_Increment(new decimal(new int[4] { 2, 0, 0, 65536 }));
			((Control)updnAsteroidMag).set_Location(new Point(300, 99));
			updnAsteroidMag.set_Maximum(new decimal(new int[4] { 30, 0, 0, 0 }));
			updnAsteroidMag.set_Minimum(new decimal(new int[4] { 2, 0, 0, -2147483648 }));
			((Control)updnAsteroidMag).set_Name("updnAsteroidMag");
			((Control)updnAsteroidMag).set_Size(new Size(46, 20));
			((Control)updnAsteroidMag).set_TabIndex(4);
			updnAsteroidMag.set_Value(Settings.Default.LCsim_MvAst);
			updnAsteroidMag.add_ValueChanged((EventHandler)updnAsteroidMag_ValueChanged);
			((Control)updnAsteroidMag).add_Enter((EventHandler)updnAsteroidMag_Enter);
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(236, 103));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(57, 13));
			((Control)label8).set_TabIndex(3);
			((Control)label8).set_Text("Magnitude");
			((Control)panel1).set_BackColor(Color.Beige);
			panel1.set_BorderStyle((BorderStyle)2);
			((Control)panel1).get_Controls().Add((Control)(object)label10);
			((Control)panel1).get_Controls().Add((Control)(object)label9);
			((Control)panel1).get_Controls().Add((Control)(object)panel2);
			((Control)panel1).get_Controls().Add((Control)(object)lblPathPA);
			((Control)panel1).get_Controls().Add((Control)(object)label15);
			((Control)panel1).get_Controls().Add((Control)(object)updnPA);
			((Control)panel1).get_Controls().Add((Control)(object)updnMinor_mas);
			((Control)panel1).get_Controls().Add((Control)(object)updnMinor_km);
			((Control)panel1).get_Controls().Add((Control)(object)label16);
			((Control)panel1).get_Controls().Add((Control)(object)updnAsteroidDiameter_mas);
			((Control)panel1).get_Controls().Add((Control)(object)label4);
			((Control)panel1).get_Controls().Add((Control)(object)label14);
			((Control)panel1).get_Controls().Add((Control)(object)label13);
			((Control)panel1).get_Controls().Add((Control)(object)label17);
			((Control)panel1).get_Controls().Add((Control)(object)label12);
			((Control)panel1).get_Controls().Add((Control)(object)label5);
			((Control)panel1).get_Controls().Add((Control)(object)updnAsteroidDia_km);
			((Control)panel1).set_Location(new Point(3, 16));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(211, 122));
			((Control)panel1).set_TabIndex(0);
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(128, 59));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(14, 13));
			((Control)label10).set_TabIndex(16);
			((Control)label10).set_Text("=");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(128, 37));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(14, 13));
			((Control)label9).set_TabIndex(15);
			((Control)label9).set_Text("=");
			((Control)panel2).set_BackColor(Color.LightYellow);
			((Control)panel2).get_Controls().Add((Control)(object)label18);
			((Control)panel2).get_Controls().Add((Control)(object)optRectangle);
			((Control)panel2).get_Controls().Add((Control)(object)optEllipse);
			((Control)panel2).set_Location(new Point(-1, -1));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(209, 20));
			((Control)panel2).set_TabIndex(0);
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label18).set_Location(new Point(16, 3));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(36, 13));
			((Control)label18).set_TabIndex(0);
			((Control)label18).set_Text("Model");
			((Control)optRectangle).set_AutoSize(true);
			((Control)optRectangle).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optRectangle).set_Location(new Point(119, 2));
			((Control)optRectangle).set_Name("optRectangle");
			((Control)optRectangle).set_Size(new Size(74, 17));
			((Control)optRectangle).set_TabIndex(2);
			((Control)optRectangle).set_Text("Rectangle");
			((ButtonBase)optRectangle).set_UseVisualStyleBackColor(true);
			optRectangle.add_CheckedChanged((EventHandler)optRectangle_CheckedChanged);
			((Control)optEllipse).set_AutoSize(true);
			optEllipse.set_Checked(true);
			((Control)optEllipse).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optEllipse).set_Location(new Point(64, 1));
			((Control)optEllipse).set_Name("optEllipse");
			((Control)optEllipse).set_Size(new Size(55, 17));
			((Control)optEllipse).set_TabIndex(1);
			optEllipse.set_TabStop(true);
			((Control)optEllipse).set_Text("Ellipse");
			((ButtonBase)optEllipse).set_UseVisualStyleBackColor(true);
			optEllipse.add_CheckedChanged((EventHandler)optEllipse_CheckedChanged);
			((Control)lblPathPA).set_AutoSize(true);
			((Control)lblPathPA).set_Location(new Point(149, 102));
			((Control)lblPathPA).set_Name("lblPathPA");
			((Control)lblPathPA).set_Size(new Size(26, 13));
			((Control)lblPathPA).set_TabIndex(14);
			((Control)lblPathPA).set_Text("90°");
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label15).set_Location(new Point(134, 89));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(57, 13));
			((Control)label15).set_TabIndex(13);
			((Control)label15).set_Text("PA of path");
			((Control)updnPA).set_BackColor(SystemColors.Window);
			((Control)updnPA).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "LCsim_PA", true, (DataSourceUpdateMode)1));
			((Control)updnPA).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnPA.set_Increment(new decimal(new int[4] { 2, 0, 0, 0 }));
			((Control)updnPA).set_Location(new Point(68, 93));
			updnPA.set_Maximum(new decimal(new int[4] { 359, 0, 0, 0 }));
			((Control)updnPA).set_Name("updnPA");
			((Control)updnPA).set_Size(new Size(46, 20));
			((Control)updnPA).set_TabIndex(12);
			((UpDownBase)updnPA).set_TextAlign((HorizontalAlignment)2);
			updnPA.set_Value(Settings.Default.LCsim_PA);
			updnPA.add_ValueChanged((EventHandler)updnPA_ValueChanged);
			((Control)updnPA).add_Enter((EventHandler)updnPA_Enter);
			updnMinor_mas.set_DecimalPlaces(1);
			((Control)updnMinor_mas).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnMinor_mas.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnMinor_mas).set_Location(new Point(144, 56));
			updnMinor_mas.set_Maximum(new decimal(new int[4] { 400, 0, 0, 0 }));
			updnMinor_mas.set_Minimum(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnMinor_mas).set_Name("updnMinor_mas");
			((Control)updnMinor_mas).set_Size(new Size(45, 20));
			((Control)updnMinor_mas).set_TabIndex(8);
			((UpDownBase)updnMinor_mas).set_TextAlign((HorizontalAlignment)2);
			updnMinor_mas.set_Value(new decimal(new int[4] { 30, 0, 0, 0 }));
			updnMinor_mas.add_ValueChanged((EventHandler)updnMinor_mas_ValueChanged);
			((Control)updnMinor_mas).add_Enter((EventHandler)updnMinor_mas_Enter);
			((Control)updnMinor_km).set_BackColor(SystemColors.Window);
			((Control)updnMinor_km).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "LCsim_Minor", true, (DataSourceUpdateMode)1));
			updnMinor_km.set_DecimalPlaces(1);
			((Control)updnMinor_km).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnMinor_km.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnMinor_km).set_Location(new Point(68, 56));
			updnMinor_km.set_Maximum(new decimal(new int[4] { 1000, 0, 0, 0 }));
			updnMinor_km.set_Minimum(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnMinor_km).set_Name("updnMinor_km");
			((Control)updnMinor_km).set_Size(new Size(58, 20));
			((Control)updnMinor_km).set_TabIndex(7);
			((UpDownBase)updnMinor_km).set_TextAlign((HorizontalAlignment)2);
			updnMinor_km.set_Value(Settings.Default.LCsim_Minor);
			updnMinor_km.add_ValueChanged((EventHandler)updnMinor_km_ValueChanged);
			((Control)updnMinor_km).add_Enter((EventHandler)updnMinor_km_Enter);
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label16).set_ForeColor(Color.FromArgb(192, 64, 0));
			((Control)label16).set_Location(new Point(80, 78));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(87, 13));
			((Control)label16).set_TabIndex(9);
			((Control)label16).set_Text("Sky-plane values");
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label14).set_Location(new Point(41, 96));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(25, 13));
			((Control)label14).set_TabIndex(11);
			((Control)label14).set_Text("PA°");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label13).set_Location(new Point(13, 59));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(54, 13));
			((Control)label13).set_TabIndex(6);
			((Control)label13).set_Text("Minor axis");
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label17).set_Location(new Point(9, 89));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(33, 26));
			((Control)label17).set_TabIndex(10);
			((Control)label17).set_Text("Major\r\naxis");
			label17.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(12, 36));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(54, 13));
			((Control)label12).set_TabIndex(3);
			((Control)label12).set_Text("Major axis");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(77, 18));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(23, 13));
			((Control)label5).set_TabIndex(1);
			((Control)label5).set_Text("km");
			((Control)picAlongPath).set_Location(new Point(394, 28));
			((Control)picAlongPath).set_Name("picAlongPath");
			((Control)picAlongPath).set_Size(new Size(334, 271));
			picAlongPath.set_TabIndex(1);
			picAlongPath.set_TabStop(false);
			((Control)picAcrossPath).set_Location(new Point(25, 28));
			((Control)picAcrossPath).set_Name("picAcrossPath");
			((Control)picAcrossPath).set_Size(new Size(334, 271));
			picAcrossPath.set_TabIndex(0);
			picAcrossPath.set_TabStop(false);
			pnlCurves.set_BorderStyle((BorderStyle)2);
			((Control)pnlCurves).get_Controls().Add((Control)(object)label2);
			((Control)pnlCurves).get_Controls().Add((Control)(object)label1);
			((Control)pnlCurves).get_Controls().Add((Control)(object)picAlongPath);
			((Control)pnlCurves).get_Controls().Add((Control)(object)picAcrossPath);
			((Control)pnlCurves).get_Controls().Add((Control)(object)TBarAcross);
			((Control)pnlCurves).set_Location(new Point(9, 57));
			((Control)pnlCurves).set_Name("pnlCurves");
			((Control)pnlCurves).set_Size(new Size(756, 320));
			((Control)pnlCurves).set_TabIndex(0);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(774, 24));
			((Control)menuStrip1).set_TabIndex(24);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)copyToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)acrossPathToolStripMenuItem,
				(ToolStripItem)alongpathLightCurveToolStripMenuItem1,
				(ToolStripItem)bothLightCurvesToolStripMenuItem1
			});
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.CopyItem_32x);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(87, 20));
			((ToolStripItem)copyToolStripMenuItem).set_Text("Copy        ");
			((ToolStripItem)acrossPathToolStripMenuItem).set_Image((Image)Resources.arrow_Sync_16xLG);
			((ToolStripItem)acrossPathToolStripMenuItem).set_Name("acrossPathToolStripMenuItem");
			((ToolStripItem)acrossPathToolStripMenuItem).set_Size(new Size(197, 22));
			((ToolStripItem)acrossPathToolStripMenuItem).set_Text("Across-path light curve");
			((ToolStripItem)acrossPathToolStripMenuItem).add_Click((EventHandler)acrossPathToolStripMenuItem_Click);
			((ToolStripItem)alongpathLightCurveToolStripMenuItem1).set_Image((Image)Resources.arrow_Sync_updn_16xLG);
			((ToolStripItem)alongpathLightCurveToolStripMenuItem1).set_Name("alongpathLightCurveToolStripMenuItem1");
			((ToolStripItem)alongpathLightCurveToolStripMenuItem1).set_Size(new Size(197, 22));
			((ToolStripItem)alongpathLightCurveToolStripMenuItem1).set_Text("Along-path light curve");
			((ToolStripItem)alongpathLightCurveToolStripMenuItem1).add_Click((EventHandler)alongpathLightCurveToolStripMenuItem1_Click);
			((ToolStripItem)bothLightCurvesToolStripMenuItem1).set_Name("bothLightCurvesToolStripMenuItem1");
			((ToolStripItem)bothLightCurvesToolStripMenuItem1).set_Size(new Size(197, 22));
			((ToolStripItem)bothLightCurvesToolStripMenuItem1).set_Text("Both light curves");
			((ToolStripItem)bothLightCurvesToolStripMenuItem1).add_Click((EventHandler)bothLightCurvesToolStripMenuItem1_Click);
			((ToolStripDropDownItem)saveToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)acrosspathLightCurveToolStripMenuItem,
				(ToolStripItem)alongpathLightCurveToolStripMenuItem,
				(ToolStripItem)bothLightCurvesToolStripMenuItem
			});
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(116, 20));
			((ToolStripItem)saveToolStripMenuItem).set_Text("Save....               ");
			((ToolStripItem)acrosspathLightCurveToolStripMenuItem).set_Image((Image)Resources.arrow_Sync_16xLG);
			((ToolStripItem)acrosspathLightCurveToolStripMenuItem).set_Name("acrosspathLightCurveToolStripMenuItem");
			((ToolStripItem)acrosspathLightCurveToolStripMenuItem).set_Size(new Size(197, 22));
			((ToolStripItem)acrosspathLightCurveToolStripMenuItem).set_Text("Across-path light curve");
			((ToolStripItem)acrosspathLightCurveToolStripMenuItem).add_Click((EventHandler)acrosspathLightCurveToolStripMenuItem_Click);
			((ToolStripItem)alongpathLightCurveToolStripMenuItem).set_Image((Image)Resources.arrow_Sync_updn_16xLG);
			((ToolStripItem)alongpathLightCurveToolStripMenuItem).set_Name("alongpathLightCurveToolStripMenuItem");
			((ToolStripItem)alongpathLightCurveToolStripMenuItem).set_Size(new Size(197, 22));
			((ToolStripItem)alongpathLightCurveToolStripMenuItem).set_Text("Along-path light curve");
			((ToolStripItem)alongpathLightCurveToolStripMenuItem).add_Click((EventHandler)alongpathLightCurveToolStripMenuItem_Click);
			((ToolStripItem)bothLightCurvesToolStripMenuItem).set_Name("bothLightCurvesToolStripMenuItem");
			((ToolStripItem)bothLightCurvesToolStripMenuItem).set_Size(new Size(197, 22));
			((ToolStripItem)bothLightCurvesToolStripMenuItem).set_Text("Both light curves");
			((ToolStripItem)bothLightCurvesToolStripMenuItem).add_Click((EventHandler)bothLightCurvesToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(105, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text(" Exit                ");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(108, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help                ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((Control)lblEvent).set_AutoSize(true);
			((Control)lblEvent).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblEvent).set_ForeColor(Color.DarkGreen);
			((Control)lblEvent).set_Location(new Point(32, 32));
			((Control)lblEvent).set_Name("lblEvent");
			((Control)lblEvent).set_Size(new Size(142, 20));
			((Control)lblEvent).set_TabIndex(25);
			((Control)lblEvent).set_Text("Event light curve");
			((Control)chkLightLevels).set_AutoSize(true);
			((Control)chkLightLevels).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkLightLevels).set_Location(new Point(9, 70));
			((Control)chkLightLevels).set_Name("chkLightLevels");
			((Control)chkLightLevels).set_Size(new Size(126, 17));
			((Control)chkLightLevels).set_TabIndex(4);
			((Control)chkLightLevels).set_Text("show Light level lines");
			((ButtonBase)chkLightLevels).set_UseVisualStyleBackColor(true);
			chkLightLevels.add_CheckedChanged((EventHandler)chkLightLevels_CheckedChanged);
			((Control)chkKm_Time).set_AutoSize(true);
			((Control)chkKm_Time).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkKm_Time).set_Location(new Point(9, 104));
			((Control)chkKm_Time).set_Name("chkKm_Time");
			((Control)chkKm_Time).set_Size(new Size(155, 17));
			((Control)chkKm_Time).set_TabIndex(6);
			((Control)chkKm_Time).set_Text("show Distance && Time lines");
			((ButtonBase)chkKm_Time).set_UseVisualStyleBackColor(true);
			chkKm_Time.add_CheckedChanged((EventHandler)chkKm_Time_CheckedChanged);
			((Control)grpDurn).set_BackColor(Color.FromArgb(192, 255, 192));
			((Control)grpDurn).get_Controls().Add((Control)(object)chkMagLevels);
			((Control)grpDurn).get_Controls().Add((Control)(object)chkTopoDistances);
			((Control)grpDurn).get_Controls().Add((Control)(object)lblDuration);
			((Control)grpDurn).get_Controls().Add((Control)(object)label19);
			((Control)grpDurn).get_Controls().Add((Control)(object)updn_masPERsec);
			((Control)grpDurn).get_Controls().Add((Control)(object)label20);
			((Control)grpDurn).get_Controls().Add((Control)(object)chkKm_Time);
			((Control)grpDurn).get_Controls().Add((Control)(object)chkLightLevels);
			((Control)grpDurn).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpDurn).set_Location(new Point(9, 383));
			((Control)grpDurn).set_Name("grpDurn");
			((Control)grpDurn).set_Size(new Size(187, 142));
			((Control)grpDurn).set_TabIndex(1);
			grpDurn.set_TabStop(false);
			((Control)grpDurn).set_Text("Plot parameters");
			((Control)chkMagLevels).set_AutoSize(true);
			((Control)chkMagLevels).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkMagLevels).set_Location(new Point(9, 87));
			((Control)chkMagLevels).set_Name("chkMagLevels");
			((Control)chkMagLevels).set_Size(new Size(128, 17));
			((Control)chkMagLevels).set_TabIndex(5);
			((Control)chkMagLevels).set_Text("show Magnitude lines");
			((ButtonBase)chkMagLevels).set_UseVisualStyleBackColor(true);
			chkMagLevels.add_CheckedChanged((EventHandler)chkMagLevels_CheckedChanged);
			((Control)chkTopoDistances).set_AutoSize(true);
			((Control)chkTopoDistances).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkTopoDistances).set_Location(new Point(9, 121));
			((Control)chkTopoDistances).set_Name("chkTopoDistances");
			((Control)chkTopoDistances).set_Size(new Size(145, 17));
			((Control)chkTopoDistances).set_TabIndex(7);
			((Control)chkTopoDistances).set_Text("distances as Topocentric");
			((ButtonBase)chkTopoDistances).set_UseVisualStyleBackColor(true);
			chkTopoDistances.add_CheckedChanged((EventHandler)chkTopoDistances_CheckedChanged);
			((Control)lblDuration).set_AutoSize(true);
			((Control)lblDuration).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblDuration).set_Location(new Point(21, 48));
			((Control)lblDuration).set_Name("lblDuration");
			((Control)lblDuration).set_Size(new Size(141, 13));
			((Control)lblDuration).set_TabIndex(3);
			((Control)lblDuration).set_Text("Maximum duration   xxx secs");
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label19).set_Location(new Point(2, 29));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(79, 13));
			((Control)label19).set_TabIndex(0);
			((Control)label19).set_Text("Asteroid motion");
			((Control)updn_masPERsec).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "LCsim_Motion", true, (DataSourceUpdateMode)1));
			updn_masPERsec.set_DecimalPlaces(2);
			((Control)updn_masPERsec).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updn_masPERsec.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updn_masPERsec).set_Location(new Point(83, 25));
			updn_masPERsec.set_Minimum(new decimal(new int[4] { 2, 0, 0, 65536 }));
			((Control)updn_masPERsec).set_Name("updn_masPERsec");
			((Control)updn_masPERsec).set_Size(new Size(49, 20));
			((Control)updn_masPERsec).set_TabIndex(1);
			((UpDownBase)updn_masPERsec).set_TextAlign((HorizontalAlignment)2);
			updn_masPERsec.set_Value(Settings.Default.LCsim_Motion);
			updn_masPERsec.add_ValueChanged((EventHandler)updn_masPERsec_ValueChanged);
			((Control)updn_masPERsec).add_Enter((EventHandler)updn_masPERsec_Enter);
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label20).set_Location(new Point(133, 29));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(48, 13));
			((Control)label20).set_TabIndex(2);
			((Control)label20).set_Text("mas/sec");
			panel3.set_BorderStyle((BorderStyle)1);
			((Control)panel3).get_Controls().Add((Control)(object)trackOpacity);
			((Control)panel3).set_Location(new Point(51, 0));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(151, 23));
			((Control)panel3).set_TabIndex(70);
			((Control)trackOpacity).set_AutoSize(false);
			((Control)trackOpacity).set_BackColor(Color.LightYellow);
			((Control)trackOpacity).set_Location(new Point(0, 0));
			trackOpacity.set_Maximum(100);
			trackOpacity.set_Minimum(20);
			((Control)trackOpacity).set_Name("trackOpacity");
			((Control)trackOpacity).set_Size(new Size(149, 23));
			trackOpacity.set_SmallChange(5);
			((Control)trackOpacity).set_TabIndex(39);
			trackOpacity.set_TickFrequency(10);
			trackOpacity.set_Value(100);
			trackOpacity.add_ValueChanged((EventHandler)trackOpacity_ValueChanged);
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label22).set_Location(new Point(4, 1));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(50, 13));
			((Control)label22).set_TabIndex(63);
			((Control)label22).set_Text("Opacity");
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label23).set_Location(new Point(53, 21));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(26, 12));
			((Control)label23).set_TabIndex(68);
			((Control)label23).set_Text("20%");
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label24).set_Location(new Point(117, 21));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(26, 12));
			((Control)label24).set_TabIndex(67);
			((Control)label24).set_Text("60%");
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label25).set_Location(new Point(148, 21));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(26, 12));
			((Control)label25).set_TabIndex(66);
			((Control)label25).set_Text("80%");
			((Control)label26).set_AutoSize(true);
			((Control)label26).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label26).set_Location(new Point(178, 21));
			((Control)label26).set_Name("label26");
			((Control)label26).set_Size(new Size(32, 12));
			((Control)label26).set_TabIndex(65);
			((Control)label26).set_Text("100%");
			((Control)label27).set_AutoSize(true);
			((Control)label27).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label27).set_Location(new Point(85, 21));
			((Control)label27).set_Name("label27");
			((Control)label27).set_Size(new Size(26, 12));
			((Control)label27).set_TabIndex(64);
			((Control)label27).set_Text("40%");
			pnlOpacity.set_BorderStyle((BorderStyle)2);
			((Control)pnlOpacity).get_Controls().Add((Control)(object)panel3);
			((Control)pnlOpacity).get_Controls().Add((Control)(object)label22);
			((Control)pnlOpacity).get_Controls().Add((Control)(object)label23);
			((Control)pnlOpacity).get_Controls().Add((Control)(object)label24);
			((Control)pnlOpacity).get_Controls().Add((Control)(object)label25);
			((Control)pnlOpacity).get_Controls().Add((Control)(object)label26);
			((Control)pnlOpacity).get_Controls().Add((Control)(object)label27);
			((Control)pnlOpacity).set_Location(new Point(559, -1));
			((Control)pnlOpacity).set_Name("pnlOpacity");
			((Control)pnlOpacity).set_Size(new Size(214, 35));
			((Control)pnlOpacity).set_TabIndex(71);
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label21).set_Location(new Point(3, 26));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(65, 26));
			((Control)label21).set_TabIndex(6);
			((Control)label21).set_Text("Appx values\r\nby class");
			label21.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)listBox1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)listBox1).set_FormattingEnabled(true);
			listBox1.get_Items().AddRange(new object[7] { "O  0.05", "B  0.2", "A  0.5", "F  0.6", "G  0.7", "K  0.7", "M 0.6" });
			((Control)listBox1).set_Location(new Point(68, 24));
			((Control)listBox1).set_Name("listBox1");
			((Control)listBox1).set_Size(new Size(60, 30));
			((Control)listBox1).set_TabIndex(7);
			((Control)panel4).set_BackColor(Color.FromArgb(128, 255, 255));
			panel4.set_BorderStyle((BorderStyle)2);
			((Control)panel4).get_Controls().Add((Control)(object)listBox1);
			((Control)panel4).get_Controls().Add((Control)(object)label21);
			((Control)panel4).get_Controls().Add((Control)(object)label11);
			((Control)panel4).get_Controls().Add((Control)(object)updnLimbDarkening);
			((Control)panel4).set_Location(new Point(4, 76));
			((Control)panel4).set_Name("panel4");
			((Control)panel4).set_Size(new Size(135, 59));
			((Control)panel4).set_TabIndex(8);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(774, 532));
			((Control)this).get_Controls().Add((Control)(object)pnlOpacity);
			((Control)this).get_Controls().Add((Control)(object)grpDurn);
			((Control)this).get_Controls().Add((Control)(object)grpAsteroid);
			((Control)this).get_Controls().Add((Control)(object)grpStar);
			((Control)this).get_Controls().Add((Control)(object)lblEvent);
			((Control)this).get_Controls().Add((Control)(object)pnlCurves);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MinimumSize(new Size(500, 400));
			((Control)this).set_Name("PredictionLightCurve");
			((Control)this).set_Text("PredictionLightCurve");
			((Control)this).add_Resize((EventHandler)PredictionLightCurve_Resize);
			((ISupportInitialize)TBarAcross).EndInit();
			((ISupportInitialize)updnAsteroidDia_km).EndInit();
			((ISupportInitialize)updnAsteroidDiameter_mas).EndInit();
			((ISupportInitialize)updnParallax).EndInit();
			((Control)grpStar).ResumeLayout(false);
			((Control)grpStar).PerformLayout();
			((ISupportInitialize)updnStarMag).EndInit();
			((ISupportInitialize)updnLimbDarkening).EndInit();
			((ISupportInitialize)updnStarDiameter).EndInit();
			((Control)grpAsteroid).ResumeLayout(false);
			((Control)grpAsteroid).PerformLayout();
			((ISupportInitialize)updnAsteroidMag).EndInit();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((ISupportInitialize)updnPA).EndInit();
			((ISupportInitialize)updnMinor_mas).EndInit();
			((ISupportInitialize)updnMinor_km).EndInit();
			((ISupportInitialize)picAlongPath).EndInit();
			((ISupportInitialize)picAcrossPath).EndInit();
			((Control)pnlCurves).ResumeLayout(false);
			((Control)pnlCurves).PerformLayout();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)grpDurn).ResumeLayout(false);
			((Control)grpDurn).PerformLayout();
			((ISupportInitialize)updn_masPERsec).EndInit();
			((Control)panel3).ResumeLayout(false);
			((ISupportInitialize)trackOpacity).EndInit();
			((Control)pnlOpacity).ResumeLayout(false);
			((Control)pnlOpacity).PerformLayout();
			((Control)panel4).ResumeLayout(false);
			((Control)panel4).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
