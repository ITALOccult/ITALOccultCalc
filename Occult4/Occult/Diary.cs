using System;
using System.ComponentModel;
using System.Drawing;
using System.Globalization;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class Diary : Form
	{
		private readonly string AppPath;

		private const double Radian = 180.0 / Math.PI;

		private const double twoPi = Math.PI * 2.0;

		private static string[,] outmonth = new string[13, 52];

		private static int[] outcount = new int[13];

		private static double T;

		private static double eventTime;

		private static double deltaTday;

		private static int dT;

		private static int YearOfDiary;

		private static string eventType;

		private static string eventType2;

		private static bool CancelFlag = false;

		private IContainer components;

		private Label label1;

		private NumericUpDown updnYear;

		private MenuStrip menuStrip1;

		private ListBox lstDiary;

		private ToolStripMenuItem withPredictionToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem eToolStripMenuItem;

		private ProgressBar progressBar1;

		private Button cmdCompute;

		private Button cmdCancel;

		private ToolStripMenuItem printPToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		public Diary()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
		}

		private void Diary_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			updnYear.set_Value((decimal)DateTime.Now.Year);
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
		}

		private void Diary_Resize(object sender, EventArgs e)
		{
			if (!((((Control)this).get_Height() < 200) | (((Control)this).get_Width() < 100)))
			{
				((Control)lstDiary).set_Height(((Control)this).get_Height() - 110);
				((Control)lstDiary).set_Width(((Control)this).get_Width() - 35);
			}
		}

		private string CollectEvents()
		{
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstDiary.get_Items().get_Count();
			for (int i = 0; i < count; i++)
			{
				stringBuilder.AppendLine(lstDiary.get_Items().get_Item(i).ToString());
			}
			stringBuilder.AppendLine("");
			return stringBuilder.ToString();
		}

		private void eToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents());
		}

		private void printPToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintPreviewText(CollectEvents());
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(CollectEvents());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_EphemerisData = Output.SaveAppendPredictionText(CollectEvents(), "Diary of events for " + updnYear.get_Value(), Settings.Default.Save_EphemerisData);
		}

		private void ComputeDiary()
		{
			double[,] array = new double[10, 3];
			double[,] array2 = new double[10, 3];
			double[,] array3 = new double[10, 3];
			double[,] array4 = new double[10, 3];
			double[,] array5 = new double[15, 3];
			double[,] array6 = new double[15, 3];
			double[,] array7 = new double[3, 3];
			double[,] array8 = new double[3, 3];
			double[] array9 = new double[2];
			double[] array10 = new double[2];
			double[] array11 = new double[2];
			double[] array12 = new double[15];
			double[] array13 = new double[5];
			double[] array14 = new double[5];
			double[] array15 = new double[5];
			double[] array16 = new double[5];
			double[] array17 = new double[5];
			double[] array18 = new double[5];
			double[] array19 = new double[5];
			string[] array20 = new string[15]
			{
				"", "Mercury ", "Venus ", "Sun ", "Mars ", "Jupiter ", "Saturn ", "Uranus ", "Neptune ", "Pluto ",
				"Aldebaran ", "Pollux ", "Regulus ", "Spica ", "Antares "
			};
			YearOfDiary = (int)updnYear.get_Value();
			double num = Utilities.JD_from_Date(YearOfDiary, 1, 1.0) - 12.0;
			double num2 = Utilities.JD_from_Date(YearOfDiary + 1, 1, 1.0) + 8.0 - num;
			deltaTday = Utilities.delta_T((int)updnYear.get_Value(), 7, 1.0) / 86400.0;
			eventType2 = "";
			progressBar1.set_Value(0);
			progressBar1.set_Maximum((int)num2);
			((Control)progressBar1).set_Visible(true);
			for (int i = 1; i <= 12; i++)
			{
				outcount[i] = 0;
				for (dT = 0; dT <= 50; dT++)
				{
					outmonth[i, dT] = "";
				}
			}
			for (int j = 0; (double)j <= num2; j += 4)
			{
				T = num + (double)j;
				progressBar1.set_Value(j);
				Application.DoEvents();
				if (CancelFlag)
				{
					break;
				}
				double num3 = Utilities.MeanEclipticOfDate(T, Use1976Value: true);
				double Dec;
				double Parallax;
				double num4;
				for (int k = 1; k <= 9; k++)
				{
					int planetNo = k;
					if (k == 3)
					{
						planetNo = 0;
					}
					Utilities.PlanetGeocentric(T + 4.0, planetNo, 1E-05, 0, physicalFlag: false, out var _, out var _, out var _, out array[k, 2], out array3[k, 2], out array4[k, 2], out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _);
					num4 = Math.Cos(array3[k, 2]) * Math.Sin(array[k, 2]) * Math.Cos(num3) + Math.Sin(array3[k, 2]) * Math.Sin(num3);
					Dec = Math.Cos(array3[k, 2]) * Math.Cos(array[k, 2]);
					Parallax = (0.0 - Math.Cos(array3[k, 2])) * Math.Sin(array[k, 2]) * Math.Sin(num3) + Math.Sin(array3[k, 2]) * Math.Cos(num3);
					array5[k, 2] = Math.Atan2(num4, Dec);
					if (array5[k, 2] < 0.0)
					{
						array5[k, 2] += Math.PI * 2.0;
					}
					array6[k, 2] = Math.Atan(Parallax / Math.Sqrt(num4 * num4 + Dec * Dec));
					array2[k, 1] = array[k, 2] - array[k, 1];
					if (array2[k, 1] > Math.PI)
					{
						array2[k, 1] -= Math.PI * 2.0;
					}
					if (array2[k, 1] < -Math.PI)
					{
						array2[k, 1] += Math.PI * 2.0;
					}
				}
				for (int k = 1; k < 3; k++)
				{
					num4 = Math.Cos(array3[3, 2]) * Math.Sin(array[3, 2] - array[k, 2]);
					Dec = Math.Sin(array3[3, 2]) * Math.Cos(array3[k, 2]) - Math.Cos(array3[3, 2]) * Math.Sin(array3[k, 2]) * Math.Cos(array[3, 2] - array[k, 2]);
					Parallax = Math.Sin(array3[3, 2]) * Math.Sin(array3[k, 2]) + Math.Cos(array3[3, 2]) * Math.Cos(array3[k, 2]) * Math.Cos(array[3, 2] - array[k, 2]);
					array7[k, 2] = Math.Atan(Math.Sqrt(num4 * num4 + Dec * Dec) / Parallax);
					array8[k, 1] = array7[k, 2] - array7[k, 1];
				}
				array9[1] = array4[3, 2] - array4[3, 1];
				array10[1] = array4[4, 2] - array4[4, 1];
				array11[1] = array3[3, 2] - array3[3, 1];
				for (int k = 1; k < 10; k++)
				{
					array12[k] = array5[k, 2] - array5[k, 1];
					if (array12[k] < Math.PI)
					{
						array12[k] += Math.PI * 2.0;
					}
					if (array12[k] > Math.PI)
					{
						array12[k] -= Math.PI * 2.0;
					}
					array12[k] /= 4.0;
				}
				for (dT = 1; dT < 5; dT++)
				{
					Utilities.QuickMoon(T + (double)dT, out array13[dT], out array14[dT], out array16[dT], out array18[dT], out array19[dT]);
				}
				for (dT = 0; dT < 4; dT++)
				{
					array17[1 + dT] = array16[1 + dT] - array16[dT];
					array15[1 + dT] = array14[1 + dT] - array14[dT];
				}
				num4 = 1.396389 * (T - 2451545.0) / 36525.0 / (180.0 / Math.PI);
				array5[10, 2] = 1.2180507551008197 + num4;
				array5[11, 2] = 1.9760453381064262 + num4;
				array5[12, 2] = 2.6150387371165658 + num4;
				array5[13, 2] = 3.5577043794902377 + num4;
				array5[14, 2] = 4.359176936286762 + num4;
				array6[10, 2] = -0.09540711456333102;
				array6[11, 2] = 0.1166760616019476;
				array6[12, 2] = 0.008119711503249123;
				array6[13, 2] = -0.03585721352357034;
				array6[14, 2] = -0.07976056943175973;
				if (j > 8)
				{
					for (int k = 1; k < 10; k++)
					{
						if (k == 3)
						{
							k++;
						}
						if (Math.Sign(array2[k, 0]) != Math.Sign(array2[k, 1]))
						{
							double num5 = array2[k, 0] / (array2[k, 0] - array2[k, 1]);
							eventTime = (num5 - 0.5) * 4.0;
							eventType = array20[k] + "stationary";
							AddToDiary();
						}
					}
					if (Math.Sign(array9[0]) != Math.Sign(array9[1]))
					{
						double num5 = array9[0] / (array9[0] - array9[1]);
						eventTime = (num5 - 0.5) * 4.0;
						if (array9[0] < 0.0)
						{
							eventType = "Earth at perihelion";
						}
						else
						{
							eventType = "Earth at aphelion";
						}
						AddToDiary();
					}
					if (Math.Sign(array3[3, 1]) != Math.Sign(array3[3, 2]))
					{
						double num5 = array3[3, 1] / (array3[3, 1] - array3[3, 2]);
						eventTime = num5 * 4.0;
						eventType = "Equinox";
						AddToDiary();
					}
					if (Math.Sign(array11[0]) != Math.Sign(array11[1]))
					{
						double num5 = array11[0] / (array11[0] - array11[1]);
						eventTime = (num5 - 0.5) * 4.0;
						eventType = "Solstice";
						AddToDiary();
					}
					for (int k = 1; k < 3; k++)
					{
						if ((Math.Sign(array8[k, 0]) != Math.Sign(array8[k, 1])) & (array7[k, 1] > 0.15))
						{
							double num5 = array8[k, 0] / (array8[k, 0] - array8[k, 1]);
							eventTime = (num5 - 0.5) * 4.0;
							eventType = array20[k] + "greatest elong ";
							if (Math.Sin(array[k, 2] - array[3, 2]) > 0.0)
							{
								eventType += "E(";
							}
							else
							{
								eventType += "W(";
							}
							eventType = eventType + string.Format("{0,1:F0}", 180.0 / Math.PI * array7[k, 1]) + ")";
							AddToDiary();
						}
					}
					if ((Math.Sign(array10[0]) != Math.Sign(array10[1])) & (array10[0] < 0.0))
					{
						double num5 = array10[0] / (array10[0] - array10[1]);
						eventTime = (num5 - 0.5) * 4.0;
						eventType = "Mars nearest to Earth";
						AddToDiary();
					}
					for (int k = 1; k < 10; k++)
					{
						if (k == 3)
						{
							k++;
						}
						double num6 = 180.0 / Math.PI * NormalisedDifference(array5[k, 1], array5[3, 1] + Math.PI);
						double num7 = 180.0 / Math.PI * NormalisedDifference(array5[k, 2], array5[3, 2] + Math.PI);
						double num5;
						if ((Math.Sign(num6 - 180.0) != Math.Sign(num7 - 180.0)) & (Math.Abs(num6 - num7) < 50.0))
						{
							num5 = (num6 - 180.0) / (num6 - num7);
							eventTime = num5 * 4.0;
							eventType = array20[k] + "at opposition";
							AddToDiary();
							continue;
						}
						num6 = 180.0 / Math.PI * NormalisedDifference(array5[k, 1], array5[3, 1]);
						num7 = 180.0 / Math.PI * NormalisedDifference(array5[k, 2], array5[3, 2]);
						if (!((Math.Sign(num6 - 180.0) != Math.Sign(num7 - 180.0)) & (Math.Abs(num6 - num7) < 50.0)))
						{
							continue;
						}
						num5 = (num6 - 180.0) / (num6 - num7);
						eventTime = num5 * 4.0;
						eventType = array20[k];
						if (k < 3)
						{
							if (array4[k, 1] < 1.0)
							{
								eventType += "inferior";
							}
							else
							{
								eventType += "superior";
							}
						}
						else
						{
							eventType += "at";
						}
						eventType += " conjunction";
						AddToDiary();
					}
					for (int k = 1; k < 9; k++)
					{
						if (k == 3)
						{
							k++;
						}
						for (int l = k + 1; l <= 14; l++)
						{
							if (l == 3)
							{
								l++;
							}
							double num6 = 180.0 / Math.PI * NormalisedDifference(array5[k, 1], array5[l, 1]);
							double num7 = 180.0 / Math.PI * NormalisedDifference(array5[k, 2], array5[l, 2]);
							if ((Math.Sign(num6 - 180.0) != Math.Sign(num7 - 180.0)) & (Math.Abs(num6 - num7) < 50.0))
							{
								double num8 = array6[k, 1] - array6[l, 1];
								double num9 = array6[k, 2] - array6[l, 2];
								double num5 = (num6 - 180.0) / (num6 - num7);
								double value = 180.0 / Math.PI * ((1.0 - num5) * num8 + num5 * num9);
								if (Math.Abs(value) < 6.0)
								{
									string text = value.ToString("#0.0N of ;#0.0S of ");
									eventTime = num5 * 4.0;
									eventType = array20[k] + text + array20[l];
									AddToDiary();
								}
							}
						}
					}
					for (dT = 0; dT <= 3; dT++)
					{
						if (Math.Sign(array17[dT]) != Math.Sign(array17[dT + 1]))
						{
							double num5 = array17[dT] / (array17[dT] - array17[dT + 1]);
							eventTime = (double)dT + num5 - 0.5;
							if (array17[dT] > 0.0)
							{
								eventType = "Moon at perigee";
							}
							else
							{
								eventType = "Moon at apogee";
							}
							AddToDiary();
						}
					}
					for (dT = 0; dT <= 3; dT++)
					{
						if (Math.Sign(array15[dT]) != Math.Sign(array15[dT + 1]))
						{
							double num5 = array15[dT] / (array15[dT] - array15[dT + 1]);
							eventTime = (double)dT + num5 - 0.5;
							if (array14[dT] > 0.0)
							{
								eventType = "Moon furthest North";
							}
							else
							{
								eventType = "Moon furthest South";
							}
							Utilities.QuickMoon(T + eventTime, out num4, out Dec, out Parallax, out var _, out var _);
							eventType += $" ({Dec * (180.0 / Math.PI):F1})";
							AddToDiary();
						}
					}
					for (dT = 0; dT <= 3; dT++)
					{
						for (int m = 0; m <= 3; m++)
						{
							double num6 = 180.0 / Math.PI * NormalisedDifference(array18[dT] - Math.PI * (double)m / 2.0, array5[3, 1] + (double)dT * array12[3]);
							double num7 = 180.0 / Math.PI * NormalisedDifference(array18[dT + 1] - Math.PI * (double)m / 2.0, array5[3, 1] + (double)(dT + 1) * array12[3]);
							if (!((Math.Sign(num6 - 180.0) != Math.Sign(num7 - 180.0)) & (Math.Abs(num6 - num7) < 50.0)))
							{
								continue;
							}
							double num5 = (num6 - 180.0) / (num6 - num7);
							eventTime = (double)dT + num5;
							switch (m)
							{
							case 0:
								eventType = "NEW MOON";
								if (Math.Abs((1.0 - num5) * array19[dT] + num5 * array19[dT + 1]) < 0.026)
								{
									eventType2 = "Eclipse";
								}
								break;
							case 1:
								eventType = "FIRST QUARTER";
								break;
							case 2:
								eventType = "FULL MOON";
								if (Math.Abs((1.0 - num5) * array19[dT] + num5 * array19[dT + 1]) < 0.026)
								{
									eventType2 = "Eclipse";
								}
								break;
							case 3:
								eventType = "LAST QUARTER";
								break;
							}
							AddToDiary();
						}
					}
					for (dT = 0; dT <= 3; dT++)
					{
						for (int k = 1; k <= 14; k++)
						{
							if (k == 3)
							{
								k++;
							}
							double num6 = 180.0 / Math.PI * NormalisedDifference(array18[dT], array5[k, 1] + (double)dT * array12[k]);
							double num7 = 180.0 / Math.PI * NormalisedDifference(array18[dT + 1], array5[k, 1] + (double)(dT + 1) * array12[k]);
							if (!((Math.Sign(num6 - 180.0) != Math.Sign(num7 - 180.0)) & (Math.Abs(num6 - num7) < 50.0)))
							{
								continue;
							}
							double num5 = (num6 - 180.0) / (num6 - num7);
							eventTime = (double)dT + num5;
							double value = 180.0 / Math.PI * ((1.0 - ((double)dT + num5) / 4.0) * array6[k, 1] + ((double)dT + num5) / 4.0 * array6[k, 2] - ((1.0 - num5) * array19[dT] + num5 * array19[dT + 1]));
							if (Math.Abs(value) < 6.0)
							{
								eventType = array20[k] + value.ToString("#0.0N;#0.0S") + " of Moon";
								if (Math.Abs(value) < 1.272 * array16[dT] * (180.0 / Math.PI))
								{
									eventType2 = "Occn";
								}
								AddToDiary();
							}
						}
					}
				}
				for (int k = 1; k <= 14; k++)
				{
					array5[k, 0] = array5[k, 1];
					array5[k, 1] = array5[k, 2];
					array6[k, 0] = array6[k, 1];
					array6[k, 1] = array6[k, 2];
					if (k < 10)
					{
						array[k, 1] = array[k, 2];
						array3[k, 1] = array3[k, 2];
						array4[k, 1] = array4[k, 2];
						array2[k, 0] = array2[k, 1];
						if (k < 3)
						{
							array7[k, 0] = array7[k, 1];
							array7[k, 1] = array7[k, 2];
							array8[k, 0] = array8[k, 1];
						}
					}
				}
				array9[0] = array9[1];
				array10[0] = array10[1];
				array11[0] = array11[1];
				array13[0] = array13[4];
				array14[0] = array14[4];
				array16[0] = array16[4];
				array18[0] = array18[4];
				array19[0] = array19[4];
				array17[0] = array17[4];
				array15[0] = array15[4];
			}
			((Control)progressBar1).set_Visible(false);
			lstDiary.get_Items().Add((object)("".PadLeft(20) + "Diary of Astronomical Phenomena " + YearOfDiary));
			lstDiary.get_Items().Add((object)"");
			for (int n = 1; n < 13; n++)
			{
				lstDiary.get_Items().Add((object)("".PadLeft(33) + CultureInfo.CurrentCulture.DateTimeFormat.GetMonthName(n)));
				lstDiary.get_Items().Add((object)(" d  h" + "".PadLeft(34) + "d  h"));
				int num10 = (outcount[n] - 1) / 2;
				for (int num11 = 0; num11 <= num10; num11++)
				{
					lstDiary.get_Items().Add((object)(outmonth[n, num11] + "".PadRight(38 - outmonth[n, num11].Length) + outmonth[n, num11 + num10 + 1]));
				}
				lstDiary.get_Items().Add((object)"");
			}
			CancelFlag = false;
		}

		private void AddToDiary()
		{
			double day = 0.0;
			int Year = 0;
			int Month = 0;
			eventTime -= deltaTday;
			double num = eventTime - Math.Floor(eventTime);
			Utilities.Date_from_JD(T + Math.Floor(eventTime), out Year, out Month, out day);
			if (eventType2.Length > 0)
			{
				eventType2 = "".PadRight(28 - eventType.Length - eventType2.Length) + eventType2;
			}
			string text = day.ToString().PadLeft(2) + Convert.ToString((int)(24.0 * num)).PadLeft(3) + "  " + eventType + eventType2;
			if (Year == YearOfDiary)
			{
				int num2 = outcount[Month] - 1;
				int num3;
				if (num2 < 0)
				{
					num3 = 0;
				}
				else if (string.Compare(text, outmonth[Month, num2]) >= 0)
				{
					num3 = num2 + 1;
				}
				else
				{
					for (num3 = num2; num3 >= 0; num3--)
					{
						outmonth[Month, num3 + 1] = outmonth[Month, num3];
						if (num3 > 0 && string.Compare(text, outmonth[Month, num3 - 1]) > 0)
						{
							break;
						}
					}
					if (num3 < 0)
					{
						num3 = 0;
					}
				}
				outmonth[Month, num3] = text;
				outcount[Month]++;
			}
			eventType2 = "";
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			CancelFlag = true;
		}

		private void cmdCompute_Click(object sender, EventArgs e)
		{
			lstDiary.get_Items().Clear();
			((Control)cmdCompute).set_Visible(false);
			ComputeDiary();
			((Control)cmdCompute).set_Visible(true);
		}

		private double NormalisedDifference(double A, double B)
		{
			double num;
			for (num = A - B + Math.PI; num < 0.0; num += Math.PI * 2.0)
			{
			}
			while (num > Math.PI * 2.0)
			{
				num -= Math.PI * 2.0;
			}
			return num;
		}

		private void updnYear_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnYear).Select(0, 10);
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Diary");
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
			//IL_081c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0826: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(Diary));
			label1 = new Label();
			updnYear = new NumericUpDown();
			menuStrip1 = new MenuStrip();
			withPredictionToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			eToolStripMenuItem = new ToolStripMenuItem();
			lstDiary = new ListBox();
			progressBar1 = new ProgressBar();
			cmdCompute = new Button();
			cmdCancel = new Button();
			((ISupportInitialize)updnYear).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)label1).set_Anchor((AnchorStyles)1);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(89, 43));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(29, 13));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text("Year");
			((Control)updnYear).set_Anchor((AnchorStyles)1);
			((Control)updnYear).set_Location(new Point(124, 39));
			updnYear.set_Maximum(new decimal(new int[4] { 17000, 0, 0, 0 }));
			updnYear.set_Minimum(new decimal(new int[4] { 13000, 0, 0, -2147483648 }));
			((Control)updnYear).set_Name("updnYear");
			((Control)updnYear).set_Size(new Size(55, 20));
			((Control)updnYear).set_TabIndex(1);
			((Control)updnYear).add_Enter((EventHandler)updnYear_Enter);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withPredictionToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)eToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(567, 24));
			((Control)menuStrip1).set_TabIndex(6);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withPredictionToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withPredictionToolStripMenuItem).set_Name("withPredictionToolStripMenuItem");
			((ToolStripItem)withPredictionToolStripMenuItem).set_Size(new Size(114, 20));
			((ToolStripItem)withPredictionToolStripMenuItem).set_Text("with Prediction...  ");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printPToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPToolStripMenuItem).set_Name("printPToolStripMenuItem");
			((ToolStripItem)printPToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)printPToolStripMenuItem).set_Text("Print Preview");
			((ToolStripItem)printPToolStripMenuItem).add_Click((EventHandler)printPToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(69, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help   ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)eToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)eToolStripMenuItem).set_Name("eToolStripMenuItem");
			((ToolStripItem)eToolStripMenuItem).set_Size(new Size(53, 20));
			((ToolStripItem)eToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)eToolStripMenuItem).add_Click((EventHandler)eToolStripMenuItem_Click);
			((Control)lstDiary).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstDiary).set_FormattingEnabled(true);
			lstDiary.set_ItemHeight(14);
			((Control)lstDiary).set_Location(new Point(12, 68));
			((Control)lstDiary).set_Name("lstDiary");
			((Control)lstDiary).set_Size(new Size(542, 480));
			((Control)lstDiary).set_TabIndex(4);
			((Control)progressBar1).set_Anchor((AnchorStyles)1);
			((Control)progressBar1).set_Location(new Point(205, 44));
			((Control)progressBar1).set_Name("progressBar1");
			((Control)progressBar1).set_Size(new Size(156, 10));
			((Control)progressBar1).set_TabIndex(5);
			((Control)progressBar1).set_Visible(false);
			((Control)cmdCompute).set_Anchor((AnchorStyles)1);
			((Control)cmdCompute).set_Location(new Point(392, 36));
			((Control)cmdCompute).set_Name("cmdCompute");
			((Control)cmdCompute).set_Size(new Size(70, 26));
			((Control)cmdCompute).set_TabIndex(2);
			((Control)cmdCompute).set_Text("Compute");
			((ButtonBase)cmdCompute).set_UseVisualStyleBackColor(true);
			((Control)cmdCompute).add_Click((EventHandler)cmdCompute_Click);
			((Control)cmdCancel).set_Anchor((AnchorStyles)1);
			((Control)cmdCancel).set_Location(new Point(392, 36));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(70, 26));
			((Control)cmdCancel).set_TabIndex(3);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(567, 559));
			((Control)this).get_Controls().Add((Control)(object)progressBar1);
			((Control)this).get_Controls().Add((Control)(object)lstDiary);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)updnYear);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)cmdCompute);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationEphemDiary", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationEphemDiary);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MinimumSize(new Size(560, 560));
			((Control)this).set_Name("Diary");
			((Control)this).set_Text("Diary of Astronomical Events");
			((Form)this).add_Load((EventHandler)Diary_Load);
			((Control)this).add_Resize((EventHandler)Diary_Resize);
			((ISupportInitialize)updnYear).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
