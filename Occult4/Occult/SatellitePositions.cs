using System;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class SatellitePositions : Form
	{
		private readonly string AppPath;

		private const double Radian = 180.0 / Math.PI;

		private static bool CancelFlag = false;

		private static double JDStart = 0.0;

		private static double Duration = 0.0;

		private static double Step = 1.0;

		private IContainer components;

		private RadioButton radMars;

		private CheckBox chkM1;

		private CheckBox chkM2;

		private RadioButton radJupiter;

		private RadioButton radSaturn;

		private RadioButton radUranus;

		private RadioButton radNeptune;

		private RadioButton radPluto;

		private CheckBox chkP1;

		private CheckBox chkP2;

		private CheckBox chkP3;

		private CheckBox chkN1;

		private CheckBox chkN2;

		private CheckBox chkJ1;

		private CheckBox chkJ2;

		private CheckBox chkJ3;

		private CheckBox chkJ4;

		private CheckBox chkJ5;

		private CheckBox chkJ6;

		private CheckBox chkJ7;

		private CheckBox chkJ8;

		private CheckBox chkJ9;

		private CheckBox chkJ10;

		private CheckBox chkJ11;

		private CheckBox chkJ12;

		private CheckBox chkJ13;

		private CheckBox chkJ14;

		private CheckBox chkJ15;

		private CheckBox chkJ16;

		private CheckBox chkS9;

		private CheckBox chkS8;

		private CheckBox chkS7;

		private CheckBox chkS6;

		private CheckBox chkS5;

		private CheckBox chkS4;

		private CheckBox chkS3;

		private CheckBox chkS2;

		private CheckBox chkS1;

		private CheckBox chkU9;

		private CheckBox chkU8;

		private CheckBox chkU7;

		private CheckBox chkU6;

		private CheckBox chkU5;

		private CheckBox chkU4;

		private CheckBox chkU3;

		private CheckBox chkU2;

		private CheckBox chkU1;

		private CheckBox chkU15;

		private CheckBox chkU14;

		private CheckBox chkU13;

		private CheckBox chkU12;

		private CheckBox chkU11;

		private CheckBox chkU10;

		private ListBox lstPrediction;

		private NumericUpDown updnHour;

		private NumericUpDown updnIntervalHours;

		private NumericUpDown updnDay;

		private NumericUpDown updnMonth;

		private NumericUpDown updnYear;

		private Label label1;

		private NumericUpDown updnDurationDays;

		private Label label2;

		private Label label3;

		private Label label4;

		private Label label5;

		private Label label6;

		private Label label7;

		private Label label8;

		private Label label9;

		private GroupBox groupBox1;

		private Button cmdCompute;

		private Button cmdCancel;

		private MenuStrip menuStrip2;

		private ToolStripMenuItem withPredictionsToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private GroupBox groupBox2;

		private Label label10;

		private NumericUpDown updnDurationHours;

		private Label label11;

		private NumericUpDown updnIntervalDays;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private CheckBox chkP4;

		private CheckBox chkP5;

		private CheckBox chkHighPrecision;

		private CheckBox chkS14;

		private CheckBox chkS13;

		private CheckBox chkS12;

		private RadioButton optTT;

		private RadioButton optUT;

		private Label label12;

		private Label label16;

		private Label label17;

		private Label label22;

		private Label label15;

		private Label lbl120;

		private Label label14;

		private Label label13;

		private Button cmdClearJupiter;

		private Button cmdSetSaturn;

		private Button cmdClearSaturn;

		private Button cmdSetUranus;

		private Button cmdClearUranus;

		private Button cmdClearPluto;

		private Button cmdSetJupiter;

		public SatellitePositions()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
		}

		private void SatellitePositions_Load(object sender, EventArgs e)
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
			updnYear.set_Value((decimal)DateTime.Now.ToUniversalTime().Year);
			updnMonth.set_Value((decimal)DateTime.Now.ToUniversalTime().Month);
			updnDay.set_Value((decimal)DateTime.Now.ToUniversalTime().Day);
			updnHour.set_Value((decimal)DateTime.Now.ToUniversalTime().Hour);
		}

		private void SatellitePositions_Resize(object sender, EventArgs e)
		{
			if (!((((Control)this).get_Height() < 500) | (((Control)this).get_Width() < 760)))
			{
				((Control)lstPrediction).set_Height(((Control)this).get_Height() - 291);
				((Control)lstPrediction).set_Width(((Control)this).get_Width() - 314);
			}
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private string CollectEvents()
		{
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstPrediction.get_Items().get_Count();
			for (int i = 0; i < count; i++)
			{
				stringBuilder.AppendLine(lstPrediction.get_Items().get_Item(i).ToString());
			}
			stringBuilder.AppendLine("");
			return stringBuilder.ToString();
		}

		private void cmdCompute_Click(object sender, EventArgs e)
		{
			Compute();
		}

		private void Compute()
		{
			JDStart = Utilities.JD_from_Date((int)updnYear.get_Value(), (int)updnMonth.get_Value(), (double)updnDay.get_Value()) + (double)updnHour.get_Value() / 24.0;
			Step = (double)updnIntervalDays.get_Value() + (double)updnIntervalHours.get_Value() / 24.0;
			Duration = (double)updnDurationDays.get_Value() + (double)updnDurationHours.get_Value() / 24.0;
			((Control)cmdCompute).set_Visible(false);
			CancelFlag = false;
			lstPrediction.get_Items().Clear();
			ControlLoop();
			((Control)cmdCompute).set_Visible(true);
			CancelFlag = false;
		}

		private void ControlLoop()
		{
			if (radMars.get_Checked())
			{
				if (chkM1.get_Checked())
				{
					Compute(4, 1);
				}
				if (!CancelFlag && chkM2.get_Checked())
				{
					Compute(4, 2);
				}
			}
			else if (radJupiter.get_Checked())
			{
				if (chkJ1.get_Checked())
				{
					Compute(5, 1);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkJ2.get_Checked())
				{
					Compute(5, 2);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkJ3.get_Checked())
				{
					Compute(5, 3);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkJ4.get_Checked())
				{
					Compute(5, 4);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkJ5.get_Checked())
				{
					Compute(5, 5);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkJ6.get_Checked())
				{
					Compute(5, 6);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkJ7.get_Checked())
				{
					Compute(5, 7);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkJ8.get_Checked())
				{
					Compute(5, 8);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkJ9.get_Checked())
				{
					Compute(5, 9);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkJ10.get_Checked())
				{
					Compute(5, 10);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkJ11.get_Checked())
				{
					Compute(5, 11);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkJ12.get_Checked())
				{
					Compute(5, 12);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkJ13.get_Checked())
				{
					Compute(5, 13);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkJ14.get_Checked())
				{
					Compute(5, 14);
				}
				if (!CancelFlag)
				{
					if (chkJ15.get_Checked())
					{
						Compute(5, 15);
					}
					if (!CancelFlag && chkJ16.get_Checked())
					{
						Compute(5, 16);
					}
				}
			}
			else if (radSaturn.get_Checked())
			{
				if (chkS1.get_Checked())
				{
					Compute(6, 1);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkS2.get_Checked())
				{
					Compute(6, 2);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkS3.get_Checked())
				{
					Compute(6, 3);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkS4.get_Checked())
				{
					Compute(6, 4);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkS5.get_Checked())
				{
					Compute(6, 5);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkS6.get_Checked())
				{
					Compute(6, 6);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkS7.get_Checked())
				{
					Compute(6, 7);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkS8.get_Checked())
				{
					Compute(6, 8);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkS9.get_Checked())
				{
					Compute(6, 9);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkS12.get_Checked())
				{
					Compute(6, 12);
				}
				if (!CancelFlag)
				{
					if (chkS13.get_Checked())
					{
						Compute(6, 13);
					}
					if (!CancelFlag && chkS14.get_Checked())
					{
						Compute(6, 14);
					}
				}
			}
			else if (radUranus.get_Checked())
			{
				if (chkU1.get_Checked())
				{
					Compute(7, 1);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkU2.get_Checked())
				{
					Compute(7, 2);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkU3.get_Checked())
				{
					Compute(7, 3);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkU4.get_Checked())
				{
					Compute(7, 4);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkU5.get_Checked())
				{
					Compute(7, 5);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkU6.get_Checked())
				{
					Compute(7, 6);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkU7.get_Checked())
				{
					Compute(7, 7);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkU8.get_Checked())
				{
					Compute(7, 8);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkU9.get_Checked())
				{
					Compute(7, 9);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkU10.get_Checked())
				{
					Compute(7, 10);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkU11.get_Checked())
				{
					Compute(7, 11);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkU12.get_Checked())
				{
					Compute(7, 12);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkU13.get_Checked())
				{
					Compute(7, 13);
				}
				if (!CancelFlag)
				{
					if (chkU14.get_Checked())
					{
						Compute(7, 14);
					}
					if (!CancelFlag && chkU15.get_Checked())
					{
						Compute(7, 15);
					}
				}
			}
			else if (radNeptune.get_Checked())
			{
				if (chkN1.get_Checked())
				{
					Compute(8, 1);
				}
				if (!CancelFlag && chkN2.get_Checked())
				{
					Compute(8, 2);
				}
			}
			else
			{
				if (!radPluto.get_Checked())
				{
					return;
				}
				if (chkP1.get_Checked())
				{
					Compute(9, 1);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkP2.get_Checked())
				{
					Compute(9, 2);
				}
				if (CancelFlag)
				{
					return;
				}
				if (chkP3.get_Checked())
				{
					Compute(9, 3);
				}
				if (!CancelFlag)
				{
					if (chkP4.get_Checked())
					{
						Compute(9, 4);
					}
					if (!CancelFlag && chkP5.get_Checked())
					{
						Compute(9, 5);
					}
				}
			}
		}

		private void Compute(int Planet, int Moon)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double num = 0.0;
			double dec_OfOrigin = 0.0;
			float MoonDiaKm = 0f;
			float Mag = 0f;
			string MoonName = "";
			bool @checked = chkHighPrecision.get_Checked();
			int num2 = 0;
			if (@checked)
			{
				num2 = 2;
			}
			for (double num3 = 0.0; num3 <= Duration; num3 += Step)
			{
				Application.DoEvents();
				if (CancelFlag)
				{
					return;
				}
				double num4 = JDStart + num3;
				Satellites.SatelliteCoordinates(num4, Planet, Moon, ref MoonName, ref MoonDiaKm, ref RA, ref Dec, ref Mag);
				if (num3 == 0.0)
				{
					lstPrediction.get_Items().Add((object)MoonName);
					if (@checked)
					{
						lstPrediction.get_Items().Add((object)"Year Mth Dy  hr    h  m   s        o  '   \"      Mag   Sep\"    PA      \"/min  PA");
					}
					else
					{
						lstPrediction.get_Items().Add((object)"Year Mth Dy  hr    h  m   s      o  '   \"    Mag   Sep\"   PA     \"/min  PA");
					}
				}
				StringBuilder stringBuilder = new StringBuilder();
				double num5 = Math.Floor(num4 - 0.5) + 0.50001;
				double num6 = 24.0 * (num4 - num5);
				stringBuilder.Append(Utilities.Date_from_JD(num5, 0));
				stringBuilder.AppendFormat(" {0,4:F1}  ", num6);
				stringBuilder.Append(Utilities.DEGtoDMS(RA * (180.0 / Math.PI) / 15.0, 2, 2 + num2, MinutesOnly: false) + "  ");
				stringBuilder.Append(Utilities.DEGtoDMS(Dec * (180.0 / Math.PI), 3, 1 + num2, MinutesOnly: false));
				stringBuilder.AppendFormat("  {0,4:F1}", Mag);
				if (@checked)
				{
					stringBuilder.AppendFormat(" {0,8:F2}", Satellites.SeparationPlanet * (180.0 / Math.PI) * 3600.0);
					stringBuilder.AppendFormat(" {0,6:F2}", Satellites.PAPlanet * (180.0 / Math.PI));
				}
				else
				{
					stringBuilder.AppendFormat(" {0,7:F1}", Satellites.SeparationPlanet * (180.0 / Math.PI) * 3600.0);
					stringBuilder.AppendFormat(" {0,5:F1}", Satellites.PAPlanet * (180.0 / Math.PI));
				}
				if (num != 0.0)
				{
					Utilities.Distance(num, dec_OfOrigin, RA, Dec, out var Distance, out var PA_atOrigin);
					stringBuilder.AppendFormat(" {0,7:F2}", Distance * (180.0 / Math.PI) * 60.0 / Step / 24.0);
					stringBuilder.AppendFormat(" {0,5:F1}", PA_atOrigin * (180.0 / Math.PI));
				}
				lstPrediction.get_Items().Add((object)stringBuilder.ToString());
				num = RA;
				dec_OfOrigin = Dec;
			}
			lstPrediction.get_Items().Add((object)"");
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			CancelFlag = true;
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents());
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintPreviewText(CollectEvents());
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(CollectEvents());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_EphemerisData = Output.SaveAppendPredictionText(CollectEvents(), "Satellite Positions", Settings.Default.Save_EphemerisData);
		}

		private void updnYear_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnYear).Select(0, 10);
		}

		private void updnMonth_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnMonth).Select(0, 10);
		}

		private void updnDay_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnDay).Select(0, 10);
		}

		private void updnHour_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnHour).Select(0, 10);
		}

		private void updnInterval_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnIntervalHours).Select(0, 10);
		}

		private void updnDuration_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnDurationDays).Select(0, 10);
		}

		private void cmdSetJupiter_Click(object sender, EventArgs e)
		{
			CheckBox obj = chkJ1;
			CheckBox obj2 = chkJ2;
			CheckBox obj3 = chkJ3;
			CheckBox obj4 = chkJ4;
			CheckBox obj5 = chkJ5;
			CheckBox obj6 = chkJ6;
			CheckBox obj7 = chkJ7;
			CheckBox obj8 = chkJ8;
			CheckBox obj9 = chkJ9;
			CheckBox obj10 = chkJ10;
			CheckBox obj11 = chkJ11;
			CheckBox obj12 = chkJ12;
			CheckBox obj13 = chkJ13;
			CheckBox obj14 = chkJ14;
			CheckBox obj15 = chkJ15;
			bool flag;
			chkJ16.set_Checked(flag = true);
			bool flag2;
			obj15.set_Checked(flag2 = flag);
			bool flag3;
			obj14.set_Checked(flag3 = flag2);
			bool flag4;
			obj13.set_Checked(flag4 = flag3);
			bool flag5;
			obj12.set_Checked(flag5 = flag4);
			bool flag6;
			obj11.set_Checked(flag6 = flag5);
			bool flag7;
			obj10.set_Checked(flag7 = flag6);
			bool flag8;
			obj9.set_Checked(flag8 = flag7);
			bool flag9;
			obj8.set_Checked(flag9 = flag8);
			bool flag10;
			obj7.set_Checked(flag10 = flag9);
			bool flag11;
			obj6.set_Checked(flag11 = flag10);
			bool flag12;
			obj5.set_Checked(flag12 = flag11);
			bool flag13;
			obj4.set_Checked(flag13 = flag12);
			bool flag14;
			obj3.set_Checked(flag14 = flag13);
			bool @checked;
			obj2.set_Checked(@checked = flag14);
			obj.set_Checked(@checked);
		}

		private void cmdClearJupiter_Click(object sender, EventArgs e)
		{
			CheckBox obj = chkJ1;
			CheckBox obj2 = chkJ2;
			CheckBox obj3 = chkJ3;
			CheckBox obj4 = chkJ4;
			CheckBox obj5 = chkJ5;
			CheckBox obj6 = chkJ6;
			CheckBox obj7 = chkJ7;
			CheckBox obj8 = chkJ8;
			CheckBox obj9 = chkJ9;
			CheckBox obj10 = chkJ10;
			CheckBox obj11 = chkJ11;
			CheckBox obj12 = chkJ12;
			CheckBox obj13 = chkJ13;
			CheckBox obj14 = chkJ14;
			CheckBox obj15 = chkJ15;
			bool flag;
			chkJ16.set_Checked(flag = false);
			bool flag2;
			obj15.set_Checked(flag2 = flag);
			bool flag3;
			obj14.set_Checked(flag3 = flag2);
			bool flag4;
			obj13.set_Checked(flag4 = flag3);
			bool flag5;
			obj12.set_Checked(flag5 = flag4);
			bool flag6;
			obj11.set_Checked(flag6 = flag5);
			bool flag7;
			obj10.set_Checked(flag7 = flag6);
			bool flag8;
			obj9.set_Checked(flag8 = flag7);
			bool flag9;
			obj8.set_Checked(flag9 = flag8);
			bool flag10;
			obj7.set_Checked(flag10 = flag9);
			bool flag11;
			obj6.set_Checked(flag11 = flag10);
			bool flag12;
			obj5.set_Checked(flag12 = flag11);
			bool flag13;
			obj4.set_Checked(flag13 = flag12);
			bool flag14;
			obj3.set_Checked(flag14 = flag13);
			bool @checked;
			obj2.set_Checked(@checked = flag14);
			obj.set_Checked(@checked);
		}

		private void cmdSetSaturn_Click(object sender, EventArgs e)
		{
			CheckBox obj = chkS1;
			CheckBox obj2 = chkS2;
			CheckBox obj3 = chkS3;
			CheckBox obj4 = chkS4;
			CheckBox obj5 = chkS5;
			CheckBox obj6 = chkS6;
			CheckBox obj7 = chkS7;
			CheckBox obj8 = chkS8;
			CheckBox obj9 = chkS9;
			CheckBox obj10 = chkS12;
			CheckBox obj11 = chkS13;
			bool flag;
			chkS14.set_Checked(flag = true);
			bool flag2;
			obj11.set_Checked(flag2 = flag);
			bool flag3;
			obj10.set_Checked(flag3 = flag2);
			bool flag4;
			obj9.set_Checked(flag4 = flag3);
			bool flag5;
			obj8.set_Checked(flag5 = flag4);
			bool flag6;
			obj7.set_Checked(flag6 = flag5);
			bool flag7;
			obj6.set_Checked(flag7 = flag6);
			bool flag8;
			obj5.set_Checked(flag8 = flag7);
			bool flag9;
			obj4.set_Checked(flag9 = flag8);
			bool flag10;
			obj3.set_Checked(flag10 = flag9);
			bool @checked;
			obj2.set_Checked(@checked = flag10);
			obj.set_Checked(@checked);
		}

		private void cmdClearSaturn_Click(object sender, EventArgs e)
		{
			CheckBox obj = chkS1;
			CheckBox obj2 = chkS2;
			CheckBox obj3 = chkS3;
			CheckBox obj4 = chkS4;
			CheckBox obj5 = chkS5;
			CheckBox obj6 = chkS6;
			CheckBox obj7 = chkS7;
			CheckBox obj8 = chkS8;
			CheckBox obj9 = chkS9;
			CheckBox obj10 = chkS12;
			CheckBox obj11 = chkS13;
			bool flag;
			chkS14.set_Checked(flag = false);
			bool flag2;
			obj11.set_Checked(flag2 = flag);
			bool flag3;
			obj10.set_Checked(flag3 = flag2);
			bool flag4;
			obj9.set_Checked(flag4 = flag3);
			bool flag5;
			obj8.set_Checked(flag5 = flag4);
			bool flag6;
			obj7.set_Checked(flag6 = flag5);
			bool flag7;
			obj6.set_Checked(flag7 = flag6);
			bool flag8;
			obj5.set_Checked(flag8 = flag7);
			bool flag9;
			obj4.set_Checked(flag9 = flag8);
			bool flag10;
			obj3.set_Checked(flag10 = flag9);
			bool @checked;
			obj2.set_Checked(@checked = flag10);
			obj.set_Checked(@checked);
		}

		private void cmdSetUranus_Click(object sender, EventArgs e)
		{
			CheckBox obj = chkU1;
			CheckBox obj2 = chkU2;
			CheckBox obj3 = chkU3;
			CheckBox obj4 = chkU4;
			CheckBox obj5 = chkU5;
			CheckBox obj6 = chkU6;
			CheckBox obj7 = chkU7;
			CheckBox obj8 = chkU8;
			CheckBox obj9 = chkU9;
			CheckBox obj10 = chkU10;
			CheckBox obj11 = chkU11;
			CheckBox obj12 = chkU12;
			CheckBox obj13 = chkU13;
			CheckBox obj14 = chkU14;
			bool flag;
			chkU15.set_Checked(flag = true);
			bool flag2;
			obj14.set_Checked(flag2 = flag);
			bool flag3;
			obj13.set_Checked(flag3 = flag2);
			bool flag4;
			obj12.set_Checked(flag4 = flag3);
			bool flag5;
			obj11.set_Checked(flag5 = flag4);
			bool flag6;
			obj10.set_Checked(flag6 = flag5);
			bool flag7;
			obj9.set_Checked(flag7 = flag6);
			bool flag8;
			obj8.set_Checked(flag8 = flag7);
			bool flag9;
			obj7.set_Checked(flag9 = flag8);
			bool flag10;
			obj6.set_Checked(flag10 = flag9);
			bool flag11;
			obj5.set_Checked(flag11 = flag10);
			bool flag12;
			obj4.set_Checked(flag12 = flag11);
			bool flag13;
			obj3.set_Checked(flag13 = flag12);
			bool @checked;
			obj2.set_Checked(@checked = flag13);
			obj.set_Checked(@checked);
		}

		private void cmdClearUranus_Click(object sender, EventArgs e)
		{
			CheckBox obj = chkU1;
			CheckBox obj2 = chkU2;
			CheckBox obj3 = chkU3;
			CheckBox obj4 = chkU4;
			CheckBox obj5 = chkU5;
			CheckBox obj6 = chkU6;
			CheckBox obj7 = chkU7;
			CheckBox obj8 = chkU8;
			CheckBox obj9 = chkU9;
			CheckBox obj10 = chkU10;
			CheckBox obj11 = chkU11;
			CheckBox obj12 = chkU12;
			CheckBox obj13 = chkU13;
			CheckBox obj14 = chkU14;
			bool flag;
			chkU15.set_Checked(flag = false);
			bool flag2;
			obj14.set_Checked(flag2 = flag);
			bool flag3;
			obj13.set_Checked(flag3 = flag2);
			bool flag4;
			obj12.set_Checked(flag4 = flag3);
			bool flag5;
			obj11.set_Checked(flag5 = flag4);
			bool flag6;
			obj10.set_Checked(flag6 = flag5);
			bool flag7;
			obj9.set_Checked(flag7 = flag6);
			bool flag8;
			obj8.set_Checked(flag8 = flag7);
			bool flag9;
			obj7.set_Checked(flag9 = flag8);
			bool flag10;
			obj6.set_Checked(flag10 = flag9);
			bool flag11;
			obj5.set_Checked(flag11 = flag10);
			bool flag12;
			obj4.set_Checked(flag12 = flag11);
			bool flag13;
			obj3.set_Checked(flag13 = flag12);
			bool @checked;
			obj2.set_Checked(@checked = flag13);
			obj.set_Checked(@checked);
		}

		private void cmdClearPluto_Click(object sender, EventArgs e)
		{
			CheckBox obj = chkP1;
			CheckBox obj2 = chkP2;
			CheckBox obj3 = chkP3;
			CheckBox obj4 = chkP4;
			bool flag;
			chkP5.set_Checked(flag = false);
			bool flag2;
			obj4.set_Checked(flag2 = flag);
			bool flag3;
			obj3.set_Checked(flag3 = flag2);
			bool @checked;
			obj2.set_Checked(@checked = flag3);
			obj.set_Checked(@checked);
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Satellite positions");
		}

		private void chkHighPrecision_CheckedChanged(object sender, EventArgs e)
		{
			Compute();
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
			//IL_41c7: Unknown result type (might be due to invalid IL or missing references)
			//IL_41d1: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(SatellitePositions));
			radMars = new RadioButton();
			chkM1 = new CheckBox();
			chkM2 = new CheckBox();
			radJupiter = new RadioButton();
			radSaturn = new RadioButton();
			radUranus = new RadioButton();
			radNeptune = new RadioButton();
			radPluto = new RadioButton();
			chkP1 = new CheckBox();
			chkP2 = new CheckBox();
			chkP3 = new CheckBox();
			chkN1 = new CheckBox();
			chkN2 = new CheckBox();
			chkJ1 = new CheckBox();
			chkJ2 = new CheckBox();
			chkJ3 = new CheckBox();
			chkJ4 = new CheckBox();
			chkJ5 = new CheckBox();
			chkJ6 = new CheckBox();
			chkJ7 = new CheckBox();
			chkJ8 = new CheckBox();
			chkJ9 = new CheckBox();
			chkJ10 = new CheckBox();
			chkJ11 = new CheckBox();
			chkJ12 = new CheckBox();
			chkJ13 = new CheckBox();
			chkJ14 = new CheckBox();
			chkJ15 = new CheckBox();
			chkJ16 = new CheckBox();
			chkS9 = new CheckBox();
			chkS8 = new CheckBox();
			chkS7 = new CheckBox();
			chkS6 = new CheckBox();
			chkS5 = new CheckBox();
			chkS4 = new CheckBox();
			chkS3 = new CheckBox();
			chkS2 = new CheckBox();
			chkS1 = new CheckBox();
			chkU9 = new CheckBox();
			chkU8 = new CheckBox();
			chkU7 = new CheckBox();
			chkU6 = new CheckBox();
			chkU5 = new CheckBox();
			chkU4 = new CheckBox();
			chkU3 = new CheckBox();
			chkU2 = new CheckBox();
			chkU1 = new CheckBox();
			chkU15 = new CheckBox();
			chkU14 = new CheckBox();
			chkU13 = new CheckBox();
			chkU12 = new CheckBox();
			chkU11 = new CheckBox();
			chkU10 = new CheckBox();
			lstPrediction = new ListBox();
			updnHour = new NumericUpDown();
			updnIntervalHours = new NumericUpDown();
			updnDay = new NumericUpDown();
			updnMonth = new NumericUpDown();
			updnYear = new NumericUpDown();
			label1 = new Label();
			updnDurationDays = new NumericUpDown();
			label2 = new Label();
			label3 = new Label();
			label4 = new Label();
			label5 = new Label();
			label6 = new Label();
			label7 = new Label();
			label8 = new Label();
			label9 = new Label();
			groupBox1 = new GroupBox();
			label16 = new Label();
			label17 = new Label();
			label22 = new Label();
			label15 = new Label();
			lbl120 = new Label();
			label14 = new Label();
			label13 = new Label();
			cmdClearJupiter = new Button();
			cmdSetSaturn = new Button();
			cmdClearSaturn = new Button();
			cmdSetUranus = new Button();
			cmdClearUranus = new Button();
			cmdClearPluto = new Button();
			cmdSetJupiter = new Button();
			chkS14 = new CheckBox();
			chkS13 = new CheckBox();
			chkS12 = new CheckBox();
			chkP5 = new CheckBox();
			chkP4 = new CheckBox();
			cmdCompute = new Button();
			cmdCancel = new Button();
			menuStrip2 = new MenuStrip();
			withPredictionsToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			groupBox2 = new GroupBox();
			label12 = new Label();
			optTT = new RadioButton();
			optUT = new RadioButton();
			label11 = new Label();
			updnIntervalDays = new NumericUpDown();
			label10 = new Label();
			updnDurationHours = new NumericUpDown();
			chkHighPrecision = new CheckBox();
			((ISupportInitialize)updnHour).BeginInit();
			((ISupportInitialize)updnIntervalHours).BeginInit();
			((ISupportInitialize)updnDay).BeginInit();
			((ISupportInitialize)updnMonth).BeginInit();
			((ISupportInitialize)updnYear).BeginInit();
			((ISupportInitialize)updnDurationDays).BeginInit();
			((Control)groupBox1).SuspendLayout();
			((Control)menuStrip2).SuspendLayout();
			((Control)groupBox2).SuspendLayout();
			((ISupportInitialize)updnIntervalDays).BeginInit();
			((ISupportInitialize)updnDurationHours).BeginInit();
			((Control)this).SuspendLayout();
			((Control)radMars).set_AutoSize(true);
			((Control)radMars).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)radMars).set_Location(new Point(8, 20));
			((Control)radMars).set_Name("radMars");
			((Control)radMars).set_Size(new Size(52, 17));
			((Control)radMars).set_TabIndex(0);
			((Control)radMars).set_Text("Mars");
			((ButtonBase)radMars).set_UseVisualStyleBackColor(true);
			((Control)chkM1).set_AutoSize(true);
			chkM1.set_Checked(true);
			chkM1.set_CheckState((CheckState)1);
			((Control)chkM1).set_Location(new Point(141, 20));
			((Control)chkM1).set_Name("chkM1");
			((Control)chkM1).set_Size(new Size(68, 17));
			((Control)chkM1).set_TabIndex(1);
			((Control)chkM1).set_Text("I Phobos");
			((ButtonBase)chkM1).set_UseVisualStyleBackColor(true);
			((Control)chkM2).set_AutoSize(true);
			chkM2.set_Checked(true);
			chkM2.set_CheckState((CheckState)1);
			((Control)chkM2).set_Location(new Point(220, 20));
			((Control)chkM2).set_Name("chkM2");
			((Control)chkM2).set_Size(new Size(70, 17));
			((Control)chkM2).set_TabIndex(2);
			((Control)chkM2).set_Text("II Deimos");
			((ButtonBase)chkM2).set_UseVisualStyleBackColor(true);
			((Control)radJupiter).set_AutoSize(true);
			radJupiter.set_Checked(true);
			((Control)radJupiter).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)radJupiter).set_Location(new Point(8, 51));
			((Control)radJupiter).set_Name("radJupiter");
			((Control)radJupiter).set_Size(new Size(63, 17));
			((Control)radJupiter).set_TabIndex(3);
			radJupiter.set_TabStop(true);
			((Control)radJupiter).set_Text("Jupiter");
			((ButtonBase)radJupiter).set_UseVisualStyleBackColor(true);
			((Control)radSaturn).set_AutoSize(true);
			((Control)radSaturn).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)radSaturn).set_Location(new Point(8, 93));
			((Control)radSaturn).set_Name("radSaturn");
			((Control)radSaturn).set_Size(new Size(62, 17));
			((Control)radSaturn).set_TabIndex(20);
			((Control)radSaturn).set_Text("Saturn");
			((ButtonBase)radSaturn).set_UseVisualStyleBackColor(true);
			((Control)radUranus).set_AutoSize(true);
			((Control)radUranus).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)radUranus).set_Location(new Point(8, 132));
			((Control)radUranus).set_Name("radUranus");
			((Control)radUranus).set_Size(new Size(65, 17));
			((Control)radUranus).set_TabIndex(33);
			((Control)radUranus).set_Text("Uranus");
			((ButtonBase)radUranus).set_UseVisualStyleBackColor(true);
			((Control)radNeptune).set_AutoSize(true);
			((Control)radNeptune).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)radNeptune).set_Location(new Point(8, 167));
			((Control)radNeptune).set_Name("radNeptune");
			((Control)radNeptune).set_Size(new Size(73, 17));
			((Control)radNeptune).set_TabIndex(49);
			((Control)radNeptune).set_Text("Neptune");
			((ButtonBase)radNeptune).set_UseVisualStyleBackColor(true);
			((Control)radPluto).set_AutoSize(true);
			((Control)radPluto).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)radPluto).set_Location(new Point(8, 190));
			((Control)radPluto).set_Name("radPluto");
			((Control)radPluto).set_Size(new Size(54, 17));
			((Control)radPluto).set_TabIndex(52);
			((Control)radPluto).set_Text("Pluto");
			((ButtonBase)radPluto).set_UseVisualStyleBackColor(true);
			((Control)chkP1).set_AutoSize(true);
			chkP1.set_Checked(true);
			chkP1.set_CheckState((CheckState)1);
			((Control)chkP1).set_Location(new Point(141, 190));
			((Control)chkP1).set_Name("chkP1");
			((Control)chkP1).set_Size(new Size(66, 17));
			((Control)chkP1).set_TabIndex(53);
			((Control)chkP1).set_Text("I Charon");
			((ButtonBase)chkP1).set_UseVisualStyleBackColor(true);
			((Control)chkP2).set_AutoSize(true);
			((Control)chkP2).set_Location(new Point(220, 190));
			((Control)chkP2).set_Name("chkP2");
			((Control)chkP2).set_Size(new Size(50, 17));
			((Control)chkP2).set_TabIndex(54);
			((Control)chkP2).set_Text("II Nix");
			((ButtonBase)chkP2).set_UseVisualStyleBackColor(true);
			((Control)chkP3).set_AutoSize(true);
			((Control)chkP3).set_Location(new Point(313, 190));
			((Control)chkP3).set_Name("chkP3");
			((Control)chkP3).set_Size(new Size(66, 17));
			((Control)chkP3).set_TabIndex(55);
			((Control)chkP3).set_Text("III Hydra");
			((ButtonBase)chkP3).set_UseVisualStyleBackColor(true);
			((Control)chkN1).set_AutoSize(true);
			chkN1.set_Checked(true);
			chkN1.set_CheckState((CheckState)1);
			((Control)chkN1).set_Location(new Point(141, 167));
			((Control)chkN1).set_Name("chkN1");
			((Control)chkN1).set_Size(new Size(59, 17));
			((Control)chkN1).set_TabIndex(50);
			((Control)chkN1).set_Text("I Triton");
			((ButtonBase)chkN1).set_UseVisualStyleBackColor(true);
			((Control)chkN2).set_AutoSize(true);
			((Control)chkN2).set_Location(new Point(220, 167));
			((Control)chkN2).set_Name("chkN2");
			((Control)chkN2).set_Size(new Size(66, 17));
			((Control)chkN2).set_TabIndex(51);
			((Control)chkN2).set_Text("II Nereid");
			((ButtonBase)chkN2).set_UseVisualStyleBackColor(true);
			((Control)chkJ1).set_AutoSize(true);
			chkJ1.set_Checked(true);
			chkJ1.set_CheckState((CheckState)1);
			((Control)chkJ1).set_Location(new Point(141, 45));
			((Control)chkJ1).set_Name("chkJ1");
			((Control)chkJ1).set_Size(new Size(41, 17));
			((Control)chkJ1).set_TabIndex(4);
			((Control)chkJ1).set_Text("I Io");
			((ButtonBase)chkJ1).set_UseVisualStyleBackColor(true);
			((Control)chkJ2).set_AutoSize(true);
			chkJ2.set_Checked(true);
			chkJ2.set_CheckState((CheckState)1);
			((Control)chkJ2).set_Location(new Point(220, 45));
			((Control)chkJ2).set_Name("chkJ2");
			((Control)chkJ2).set_Size(new Size(69, 17));
			((Control)chkJ2).set_TabIndex(5);
			((Control)chkJ2).set_Text("II Europa");
			((ButtonBase)chkJ2).set_UseVisualStyleBackColor(true);
			((Control)chkJ3).set_AutoSize(true);
			chkJ3.set_Checked(true);
			chkJ3.set_CheckState((CheckState)1);
			((Control)chkJ3).set_Location(new Point(313, 45));
			((Control)chkJ3).set_Name("chkJ3");
			((Control)chkJ3).set_Size(new Size(89, 17));
			((Control)chkJ3).set_TabIndex(6);
			((Control)chkJ3).set_Text("III Ganymede");
			((ButtonBase)chkJ3).set_UseVisualStyleBackColor(true);
			((Control)chkJ4).set_AutoSize(true);
			chkJ4.set_Checked(true);
			chkJ4.set_CheckState((CheckState)1);
			((Control)chkJ4).set_Location(new Point(402, 45));
			((Control)chkJ4).set_Name("chkJ4");
			((Control)chkJ4).set_Size(new Size(72, 17));
			((Control)chkJ4).set_TabIndex(7);
			((Control)chkJ4).set_Text("IV Callisto");
			((ButtonBase)chkJ4).set_UseVisualStyleBackColor(true);
			((Control)chkJ5).set_AutoSize(true);
			((Control)chkJ5).set_Location(new Point(481, 45));
			((Control)chkJ5).set_Name("chkJ5");
			((Control)chkJ5).set_Size(new Size(80, 17));
			((Control)chkJ5).set_TabIndex(8);
			((Control)chkJ5).set_Text("V Amalthea");
			((ButtonBase)chkJ5).set_UseVisualStyleBackColor(true);
			((Control)chkJ6).set_AutoSize(true);
			((Control)chkJ6).set_Location(new Point(567, 45));
			((Control)chkJ6).set_Name("chkJ6");
			((Control)chkJ6).set_Size(new Size(73, 17));
			((Control)chkJ6).set_TabIndex(9);
			((Control)chkJ6).set_Text("VI Himalia");
			((ButtonBase)chkJ6).set_UseVisualStyleBackColor(true);
			((Control)chkJ7).set_AutoSize(true);
			((Control)chkJ7).set_Location(new Point(648, 45));
			((Control)chkJ7).set_Name("chkJ7");
			((Control)chkJ7).set_Size(new Size(66, 17));
			((Control)chkJ7).set_TabIndex(10);
			((Control)chkJ7).set_Text("VII Elara");
			((ButtonBase)chkJ7).set_UseVisualStyleBackColor(true);
			((Control)chkJ8).set_AutoSize(true);
			((Control)chkJ8).set_Location(new Point(733, 45));
			((Control)chkJ8).set_Name("chkJ8");
			((Control)chkJ8).set_Size(new Size(89, 17));
			((Control)chkJ8).set_TabIndex(11);
			((Control)chkJ8).set_Text("VIII Pasiphae");
			((ButtonBase)chkJ8).set_UseVisualStyleBackColor(true);
			((Control)chkJ9).set_AutoSize(true);
			((Control)chkJ9).set_Location(new Point(141, 60));
			((Control)chkJ9).set_Name("chkJ9");
			((Control)chkJ9).set_Size(new Size(72, 17));
			((Control)chkJ9).set_TabIndex(12);
			((Control)chkJ9).set_Text("IX Sinope");
			((ButtonBase)chkJ9).set_UseVisualStyleBackColor(true);
			((Control)chkJ10).set_AutoSize(true);
			((Control)chkJ10).set_Location(new Point(220, 60));
			((Control)chkJ10).set_Name("chkJ10");
			((Control)chkJ10).set_Size(new Size(75, 17));
			((Control)chkJ10).set_TabIndex(13);
			((Control)chkJ10).set_Text("X Lysithea");
			((ButtonBase)chkJ10).set_UseVisualStyleBackColor(true);
			((Control)chkJ11).set_AutoSize(true);
			((Control)chkJ11).set_Location(new Point(313, 60));
			((Control)chkJ11).set_Name("chkJ11");
			((Control)chkJ11).set_Size(new Size(69, 17));
			((Control)chkJ11).set_TabIndex(14);
			((Control)chkJ11).set_Text("XI Carme");
			((ButtonBase)chkJ11).set_UseVisualStyleBackColor(true);
			((Control)chkJ12).set_AutoSize(true);
			((Control)chkJ12).set_Location(new Point(402, 60));
			((Control)chkJ12).set_Name("chkJ12");
			((Control)chkJ12).set_Size(new Size(79, 17));
			((Control)chkJ12).set_TabIndex(15);
			((Control)chkJ12).set_Text("XII Ananke");
			((ButtonBase)chkJ12).set_UseVisualStyleBackColor(true);
			((Control)chkJ13).set_AutoSize(true);
			((Control)chkJ13).set_Location(new Point(481, 60));
			((Control)chkJ13).set_Name("chkJ13");
			((Control)chkJ13).set_Size(new Size(69, 17));
			((Control)chkJ13).set_TabIndex(16);
			((Control)chkJ13).set_Text("XIII Leda");
			((ButtonBase)chkJ13).set_UseVisualStyleBackColor(true);
			((Control)chkJ14).set_AutoSize(true);
			((Control)chkJ14).set_Location(new Point(567, 60));
			((Control)chkJ14).set_Name("chkJ14");
			((Control)chkJ14).set_Size(new Size(77, 17));
			((Control)chkJ14).set_TabIndex(17);
			((Control)chkJ14).set_Text("XIV Thebe");
			((ButtonBase)chkJ14).set_UseVisualStyleBackColor(true);
			((Control)chkJ15).set_AutoSize(true);
			((Control)chkJ15).set_Location(new Point(648, 60));
			((Control)chkJ15).set_Name("chkJ15");
			((Control)chkJ15).set_Size(new Size(85, 17));
			((Control)chkJ15).set_TabIndex(18);
			((Control)chkJ15).set_Text("XV Adrastea");
			((ButtonBase)chkJ15).set_UseVisualStyleBackColor(true);
			((Control)chkJ16).set_AutoSize(true);
			((Control)chkJ16).set_Location(new Point(733, 60));
			((Control)chkJ16).set_Name("chkJ16");
			((Control)chkJ16).set_Size(new Size(71, 17));
			((Control)chkJ16).set_TabIndex(19);
			((Control)chkJ16).set_Text("XVI Metis");
			((ButtonBase)chkJ16).set_UseVisualStyleBackColor(true);
			((Control)chkS9).set_AutoSize(true);
			((Control)chkS9).set_Location(new Point(141, 103));
			((Control)chkS9).set_Name("chkS9");
			((Control)chkS9).set_Size(new Size(76, 17));
			((Control)chkS9).set_TabIndex(29);
			((Control)chkS9).set_Text("IX Phoebe");
			((ButtonBase)chkS9).set_UseVisualStyleBackColor(true);
			((Control)chkS8).set_AutoSize(true);
			chkS8.set_Checked(true);
			chkS8.set_CheckState((CheckState)1);
			((Control)chkS8).set_Location(new Point(733, 88));
			((Control)chkS8).set_Name("chkS8");
			((Control)chkS8).set_Size(new Size(80, 17));
			((Control)chkS8).set_TabIndex(28);
			((Control)chkS8).set_Text("VIII Iapetus");
			((ButtonBase)chkS8).set_UseVisualStyleBackColor(true);
			((Control)chkS7).set_AutoSize(true);
			((Control)chkS7).set_Location(new Point(648, 88));
			((Control)chkS7).set_Name("chkS7");
			((Control)chkS7).set_Size(new Size(84, 17));
			((Control)chkS7).set_TabIndex(27);
			((Control)chkS7).set_Text("VII Hyperion");
			((ButtonBase)chkS7).set_UseVisualStyleBackColor(true);
			((Control)chkS6).set_AutoSize(true);
			chkS6.set_Checked(true);
			chkS6.set_CheckState((CheckState)1);
			((Control)chkS6).set_Location(new Point(567, 88));
			((Control)chkS6).set_Name("chkS6");
			((Control)chkS6).set_Size(new Size(63, 17));
			((Control)chkS6).set_TabIndex(26);
			((Control)chkS6).set_Text("VI Titan");
			((ButtonBase)chkS6).set_UseVisualStyleBackColor(true);
			((Control)chkS5).set_AutoSize(true);
			chkS5.set_Checked(true);
			chkS5.set_CheckState((CheckState)1);
			((Control)chkS5).set_Location(new Point(481, 88));
			((Control)chkS5).set_Name("chkS5");
			((Control)chkS5).set_Size(new Size(62, 17));
			((Control)chkS5).set_TabIndex(25);
			((Control)chkS5).set_Text("V Rhea");
			((ButtonBase)chkS5).set_UseVisualStyleBackColor(true);
			((Control)chkS4).set_AutoSize(true);
			chkS4.set_Checked(true);
			chkS4.set_CheckState((CheckState)1);
			((Control)chkS4).set_Location(new Point(402, 88));
			((Control)chkS4).set_Name("chkS4");
			((Control)chkS4).set_Size(new Size(67, 17));
			((Control)chkS4).set_TabIndex(24);
			((Control)chkS4).set_Text("IV Dione");
			((ButtonBase)chkS4).set_UseVisualStyleBackColor(true);
			((Control)chkS3).set_AutoSize(true);
			chkS3.set_Checked(true);
			chkS3.set_CheckState((CheckState)1);
			((Control)chkS3).set_Location(new Point(313, 88));
			((Control)chkS3).set_Name("chkS3");
			((Control)chkS3).set_Size(new Size(70, 17));
			((Control)chkS3).set_TabIndex(23);
			((Control)chkS3).set_Text("III Tethys");
			((ButtonBase)chkS3).set_UseVisualStyleBackColor(true);
			((Control)chkS2).set_AutoSize(true);
			chkS2.set_Checked(true);
			chkS2.set_CheckState((CheckState)1);
			((Control)chkS2).set_Location(new Point(220, 88));
			((Control)chkS2).set_Name("chkS2");
			((Control)chkS2).set_Size(new Size(85, 17));
			((Control)chkS2).set_TabIndex(22);
			((Control)chkS2).set_Text("II Enceladus");
			((ButtonBase)chkS2).set_UseVisualStyleBackColor(true);
			((Control)chkS1).set_AutoSize(true);
			((Control)chkS1).set_Location(new Point(141, 88));
			((Control)chkS1).set_Name("chkS1");
			((Control)chkS1).set_Size(new Size(62, 17));
			((Control)chkS1).set_TabIndex(21);
			((Control)chkS1).set_Text("I Mimas");
			((ButtonBase)chkS1).set_UseVisualStyleBackColor(true);
			((Control)chkU9).set_AutoSize(true);
			((Control)chkU9).set_Location(new Point(141, 142));
			((Control)chkU9).set_Name("chkU9");
			((Control)chkU9).set_Size(new Size(79, 17));
			((Control)chkU9).set_TabIndex(42);
			((Control)chkU9).set_Text("IX Cressida");
			((ButtonBase)chkU9).set_UseVisualStyleBackColor(true);
			((Control)chkU8).set_AutoSize(true);
			((Control)chkU8).set_Location(new Point(733, 127));
			((Control)chkU8).set_Name("chkU8");
			((Control)chkU8).set_Size(new Size(78, 17));
			((Control)chkU8).set_TabIndex(41);
			((Control)chkU8).set_Text("VIII Bianca");
			((ButtonBase)chkU8).set_UseVisualStyleBackColor(true);
			((Control)chkU7).set_AutoSize(true);
			((Control)chkU7).set_Location(new Point(648, 127));
			((Control)chkU7).set_Name("chkU7");
			((Control)chkU7).set_Size(new Size(78, 17));
			((Control)chkU7).set_TabIndex(40);
			((Control)chkU7).set_Text("VII Ophelia");
			((ButtonBase)chkU7).set_UseVisualStyleBackColor(true);
			((Control)chkU6).set_AutoSize(true);
			((Control)chkU6).set_Location(new Point(567, 127));
			((Control)chkU6).set_Name("chkU6");
			((Control)chkU6).set_Size(new Size(77, 17));
			((Control)chkU6).set_TabIndex(39);
			((Control)chkU6).set_Text("VI Cordelia");
			((ButtonBase)chkU6).set_UseVisualStyleBackColor(true);
			((Control)chkU5).set_AutoSize(true);
			((Control)chkU5).set_Location(new Point(481, 127));
			((Control)chkU5).set_Name("chkU5");
			((Control)chkU5).set_Size(new Size(74, 17));
			((Control)chkU5).set_TabIndex(38);
			((Control)chkU5).set_Text("V Miranda");
			((ButtonBase)chkU5).set_UseVisualStyleBackColor(true);
			((Control)chkU4).set_AutoSize(true);
			chkU4.set_Checked(true);
			chkU4.set_CheckState((CheckState)1);
			((Control)chkU4).set_Location(new Point(402, 127));
			((Control)chkU4).set_Name("chkU4");
			((Control)chkU4).set_Size(new Size(74, 17));
			((Control)chkU4).set_TabIndex(37);
			((Control)chkU4).set_Text("IV Oberon");
			((ButtonBase)chkU4).set_UseVisualStyleBackColor(true);
			((Control)chkU3).set_AutoSize(true);
			chkU3.set_Checked(true);
			chkU3.set_CheckState((CheckState)1);
			((Control)chkU3).set_Location(new Point(313, 127));
			((Control)chkU3).set_Name("chkU3");
			((Control)chkU3).set_Size(new Size(70, 17));
			((Control)chkU3).set_TabIndex(36);
			((Control)chkU3).set_Text("III Titania");
			((ButtonBase)chkU3).set_UseVisualStyleBackColor(true);
			((Control)chkU2).set_AutoSize(true);
			chkU2.set_Checked(true);
			chkU2.set_CheckState((CheckState)1);
			((Control)chkU2).set_Location(new Point(220, 127));
			((Control)chkU2).set_Name("chkU2");
			((Control)chkU2).set_Size(new Size(70, 17));
			((Control)chkU2).set_TabIndex(35);
			((Control)chkU2).set_Text("II Umbriel");
			((ButtonBase)chkU2).set_UseVisualStyleBackColor(true);
			((Control)chkU1).set_AutoSize(true);
			chkU1.set_Checked(true);
			chkU1.set_CheckState((CheckState)1);
			((Control)chkU1).set_Location(new Point(141, 127));
			((Control)chkU1).set_Name("chkU1");
			((Control)chkU1).set_Size(new Size(52, 17));
			((Control)chkU1).set_TabIndex(34);
			((Control)chkU1).set_Text("I Ariel");
			((ButtonBase)chkU1).set_UseVisualStyleBackColor(true);
			((Control)chkU15).set_AutoSize(true);
			((Control)chkU15).set_Location(new Point(648, 142));
			((Control)chkU15).set_Name("chkU15");
			((Control)chkU15).set_Size(new Size(68, 17));
			((Control)chkU15).set_TabIndex(48);
			((Control)chkU15).set_Text("XV Puck");
			((ButtonBase)chkU15).set_UseVisualStyleBackColor(true);
			((Control)chkU14).set_AutoSize(true);
			((Control)chkU14).set_Location(new Point(567, 142));
			((Control)chkU14).set_Name("chkU14");
			((Control)chkU14).set_Size(new Size(81, 17));
			((Control)chkU14).set_TabIndex(47);
			((Control)chkU14).set_Text("XIV Belinda");
			((ButtonBase)chkU14).set_UseVisualStyleBackColor(true);
			((Control)chkU13).set_AutoSize(true);
			((Control)chkU13).set_Location(new Point(481, 142));
			((Control)chkU13).set_Name("chkU13");
			((Control)chkU13).set_Size(new Size(86, 17));
			((Control)chkU13).set_TabIndex(46);
			((Control)chkU13).set_Text("XIII Rosalind");
			((ButtonBase)chkU13).set_UseVisualStyleBackColor(true);
			((Control)chkU12).set_AutoSize(true);
			((Control)chkU12).set_Location(new Point(402, 142));
			((Control)chkU12).set_Name("chkU12");
			((Control)chkU12).set_Size(new Size(69, 17));
			((Control)chkU12).set_TabIndex(45);
			((Control)chkU12).set_Text("XII Portia");
			((ButtonBase)chkU12).set_UseVisualStyleBackColor(true);
			((Control)chkU11).set_AutoSize(true);
			((Control)chkU11).set_Location(new Point(313, 142));
			((Control)chkU11).set_Name("chkU11");
			((Control)chkU11).set_Size(new Size(63, 17));
			((Control)chkU11).set_TabIndex(44);
			((Control)chkU11).set_Text("XI Juliet");
			((ButtonBase)chkU11).set_UseVisualStyleBackColor(true);
			((Control)chkU10).set_AutoSize(true);
			((Control)chkU10).set_Location(new Point(220, 142));
			((Control)chkU10).set_Name("chkU10");
			((Control)chkU10).set_Size(new Size(93, 17));
			((Control)chkU10).set_TabIndex(43);
			((Control)chkU10).set_Text("X Desdemona");
			((ButtonBase)chkU10).set_UseVisualStyleBackColor(true);
			((Control)lstPrediction).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstPrediction).set_FormattingEnabled(true);
			lstPrediction.set_ItemHeight(14);
			((Control)lstPrediction).set_Location(new Point(280, 246));
			((Control)lstPrediction).set_Name("lstPrediction");
			((Control)lstPrediction).set_Size(new Size(600, 284));
			((Control)lstPrediction).set_TabIndex(2);
			updnHour.set_DecimalPlaces(1);
			((Control)updnHour).set_Location(new Point(210, 29));
			updnHour.set_Maximum(new decimal(new int[4] { 24, 0, 0, 0 }));
			((Control)updnHour).set_Name("updnHour");
			((Control)updnHour).set_Size(new Size(42, 20));
			((Control)updnHour).set_TabIndex(8);
			((Control)updnHour).add_Enter((EventHandler)updnHour_Enter);
			updnIntervalHours.set_DecimalPlaces(1);
			((Control)updnIntervalHours).set_Location(new Point(131, 97));
			updnIntervalHours.set_Maximum(new decimal(new int[4] { 24, 0, 0, 0 }));
			((Control)updnIntervalHours).set_Name("updnIntervalHours");
			((Control)updnIntervalHours).set_Size(new Size(42, 20));
			((Control)updnIntervalHours).set_TabIndex(11);
			updnIntervalHours.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnIntervalHours).add_Enter((EventHandler)updnInterval_Enter);
			((Control)updnDay).set_Location(new Point(171, 29));
			updnDay.set_Maximum(new decimal(new int[4] { 31, 0, 0, 0 }));
			updnDay.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnDay).set_Name("updnDay");
			((Control)updnDay).set_Size(new Size(33, 20));
			((Control)updnDay).set_TabIndex(7);
			updnDay.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnDay).add_Enter((EventHandler)updnDay_Enter);
			((Control)updnMonth).set_Location(new Point(131, 29));
			updnMonth.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnMonth.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnMonth).set_Name("updnMonth");
			((Control)updnMonth).set_Size(new Size(34, 20));
			((Control)updnMonth).set_TabIndex(6);
			updnMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnMonth).add_Enter((EventHandler)updnMonth_Enter);
			((Control)updnYear).set_Location(new Point(79, 29));
			updnYear.set_Maximum(new decimal(new int[4] { 4000, 0, 0, 0 }));
			updnYear.set_Minimum(new decimal(new int[4] { 2000, 0, 0, -2147483648 }));
			((Control)updnYear).set_Name("updnYear");
			((Control)updnYear).set_Size(new Size(46, 20));
			((Control)updnYear).set_TabIndex(5);
			updnYear.set_Value(new decimal(new int[4] { 2006, 0, 0, 0 }));
			((Control)updnYear).add_Enter((EventHandler)updnYear_Enter);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(4, 31));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(63, 13));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text("Start date");
			((Control)updnDurationDays).set_Location(new Point(78, 136));
			updnDurationDays.set_Maximum(new decimal(new int[4] { 9999, 0, 0, 0 }));
			((Control)updnDurationDays).set_Name("updnDurationDays");
			((Control)updnDurationDays).set_Size(new Size(46, 20));
			((Control)updnDurationDays).set_TabIndex(14);
			((Control)updnDurationDays).add_Enter((EventHandler)updnDuration_Enter);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(79, 16));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(29, 13));
			((Control)label2).set_TabIndex(1);
			((Control)label2).set_Text("Year");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(128, 16));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(37, 13));
			((Control)label3).set_TabIndex(2);
			((Control)label3).set_Text("Month");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(169, 16));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(26, 13));
			((Control)label4).set_TabIndex(3);
			((Control)label4).set_Text("Day");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(208, 16));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(30, 13));
			((Control)label5).set_TabIndex(4);
			((Control)label5).set_Text("Hour");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(128, 84));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(35, 13));
			((Control)label6).set_TabIndex(10);
			((Control)label6).set_Text("Hours");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(17, 101));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(50, 13));
			((Control)label7).set_TabIndex(9);
			((Control)label7).set_Text("Interval");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(76, 123));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(31, 13));
			((Control)label8).set_TabIndex(13);
			((Control)label8).set_Text("Days");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(12, 139));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(55, 13));
			((Control)label9).set_TabIndex(12);
			((Control)label9).set_Text("Duration");
			((Control)groupBox1).set_Anchor((AnchorStyles)1);
			((Control)groupBox1).get_Controls().Add((Control)(object)label16);
			((Control)groupBox1).get_Controls().Add((Control)(object)label17);
			((Control)groupBox1).get_Controls().Add((Control)(object)label22);
			((Control)groupBox1).get_Controls().Add((Control)(object)label15);
			((Control)groupBox1).get_Controls().Add((Control)(object)lbl120);
			((Control)groupBox1).get_Controls().Add((Control)(object)label14);
			((Control)groupBox1).get_Controls().Add((Control)(object)label13);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdClearJupiter);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdSetSaturn);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdClearSaturn);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdSetUranus);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdClearUranus);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdClearPluto);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdSetJupiter);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkS14);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkS13);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkS12);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkP5);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkP4);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkU15);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkU14);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkU13);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkU12);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkU11);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkU10);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkU9);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkU8);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkU7);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkU6);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkU5);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkU4);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkU3);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkU2);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkU1);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkS9);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkS8);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkS7);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkS6);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkS5);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkS4);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkS3);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkS2);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkS1);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkJ16);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkJ15);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkJ14);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkJ13);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkJ12);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkJ11);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkJ10);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkJ9);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkJ8);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkJ7);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkJ6);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkJ5);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkJ4);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkJ3);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkJ2);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkJ1);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkN2);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkN1);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkP3);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkP2);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkP1);
			((Control)groupBox1).get_Controls().Add((Control)(object)radPluto);
			((Control)groupBox1).get_Controls().Add((Control)(object)radNeptune);
			((Control)groupBox1).get_Controls().Add((Control)(object)radUranus);
			((Control)groupBox1).get_Controls().Add((Control)(object)radSaturn);
			((Control)groupBox1).get_Controls().Add((Control)(object)radJupiter);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkM2);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkM1);
			((Control)groupBox1).get_Controls().Add((Control)(object)radMars);
			((Control)groupBox1).set_Location(new Point(15, 28));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(853, 211));
			((Control)groupBox1).set_TabIndex(0);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("1.  Select satellites for the prediction");
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Location(new Point(88, 192));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(44, 13));
			((Control)label16).set_TabIndex(72);
			((Control)label16).set_Text("Clear all");
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Location(new Point(88, 105));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(44, 13));
			((Control)label17).set_TabIndex(71);
			((Control)label17).set_Text("Clear all");
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Location(new Point(88, 90));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(36, 13));
			((Control)label22).set_TabIndex(70);
			((Control)label22).set_Text("Set all");
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Location(new Point(88, 144));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(44, 13));
			((Control)label15).set_TabIndex(69);
			((Control)label15).set_Text("Clear all");
			((Control)lbl120).set_AutoSize(true);
			((Control)lbl120).set_Location(new Point(88, 129));
			((Control)lbl120).set_Name("lbl120");
			((Control)lbl120).set_Size(new Size(36, 13));
			((Control)lbl120).set_TabIndex(68);
			((Control)lbl120).set_Text("Set all");
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Location(new Point(88, 62));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(44, 13));
			((Control)label14).set_TabIndex(67);
			((Control)label14).set_Text("Clear all");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Location(new Point(88, 47));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(36, 13));
			((Control)label13).set_TabIndex(66);
			((Control)label13).set_Text("Set all");
			((Control)cmdClearJupiter).set_BackColor(Color.LightPink);
			((ButtonBase)cmdClearJupiter).set_FlatStyle((FlatStyle)1);
			((Control)cmdClearJupiter).set_ForeColor(SystemColors.ButtonFace);
			((Control)cmdClearJupiter).set_Location(new Point(75, 63));
			((Control)cmdClearJupiter).set_Name("cmdClearJupiter");
			((Control)cmdClearJupiter).set_Size(new Size(13, 11));
			((Control)cmdClearJupiter).set_TabIndex(65);
			((ButtonBase)cmdClearJupiter).set_UseVisualStyleBackColor(false);
			((Control)cmdClearJupiter).add_Click((EventHandler)cmdClearJupiter_Click);
			((Control)cmdSetSaturn).set_BackColor(Color.PaleGreen);
			((ButtonBase)cmdSetSaturn).set_FlatStyle((FlatStyle)1);
			((Control)cmdSetSaturn).set_ForeColor(SystemColors.ButtonFace);
			((Control)cmdSetSaturn).set_Location(new Point(75, 91));
			((Control)cmdSetSaturn).set_Name("cmdSetSaturn");
			((Control)cmdSetSaturn).set_Size(new Size(13, 11));
			((Control)cmdSetSaturn).set_TabIndex(64);
			((ButtonBase)cmdSetSaturn).set_UseVisualStyleBackColor(false);
			((Control)cmdSetSaturn).add_Click((EventHandler)cmdSetSaturn_Click);
			((Control)cmdClearSaturn).set_BackColor(Color.LightPink);
			((ButtonBase)cmdClearSaturn).set_FlatStyle((FlatStyle)1);
			((Control)cmdClearSaturn).set_ForeColor(SystemColors.ButtonFace);
			((Control)cmdClearSaturn).set_Location(new Point(75, 106));
			((Control)cmdClearSaturn).set_Name("cmdClearSaturn");
			((Control)cmdClearSaturn).set_Size(new Size(13, 11));
			((Control)cmdClearSaturn).set_TabIndex(63);
			((ButtonBase)cmdClearSaturn).set_UseVisualStyleBackColor(false);
			((Control)cmdClearSaturn).add_Click((EventHandler)cmdClearSaturn_Click);
			((Control)cmdSetUranus).set_BackColor(Color.PaleGreen);
			((ButtonBase)cmdSetUranus).set_FlatStyle((FlatStyle)1);
			((Control)cmdSetUranus).set_ForeColor(SystemColors.ButtonFace);
			((Control)cmdSetUranus).set_Location(new Point(75, 130));
			((Control)cmdSetUranus).set_Name("cmdSetUranus");
			((Control)cmdSetUranus).set_Size(new Size(13, 11));
			((Control)cmdSetUranus).set_TabIndex(62);
			((ButtonBase)cmdSetUranus).set_UseVisualStyleBackColor(false);
			((Control)cmdSetUranus).add_Click((EventHandler)cmdSetUranus_Click);
			((Control)cmdClearUranus).set_BackColor(Color.LightPink);
			((ButtonBase)cmdClearUranus).set_FlatStyle((FlatStyle)1);
			((Control)cmdClearUranus).set_ForeColor(SystemColors.ButtonFace);
			((Control)cmdClearUranus).set_Location(new Point(75, 145));
			((Control)cmdClearUranus).set_Name("cmdClearUranus");
			((Control)cmdClearUranus).set_Size(new Size(13, 11));
			((Control)cmdClearUranus).set_TabIndex(61);
			((ButtonBase)cmdClearUranus).set_UseVisualStyleBackColor(false);
			((Control)cmdClearUranus).add_Click((EventHandler)cmdClearUranus_Click);
			((Control)cmdClearPluto).set_BackColor(Color.LightPink);
			((ButtonBase)cmdClearPluto).set_FlatStyle((FlatStyle)1);
			((Control)cmdClearPluto).set_Location(new Point(75, 193));
			((Control)cmdClearPluto).set_Name("cmdClearPluto");
			((Control)cmdClearPluto).set_Size(new Size(13, 11));
			((Control)cmdClearPluto).set_TabIndex(59);
			((ButtonBase)cmdClearPluto).set_UseVisualStyleBackColor(false);
			((Control)cmdClearPluto).add_Click((EventHandler)cmdClearPluto_Click);
			((Control)cmdSetJupiter).set_BackColor(Color.PaleGreen);
			((ButtonBase)cmdSetJupiter).set_FlatStyle((FlatStyle)1);
			((Control)cmdSetJupiter).set_ForeColor(SystemColors.ButtonFace);
			((Control)cmdSetJupiter).set_Location(new Point(75, 48));
			((Control)cmdSetJupiter).set_Name("cmdSetJupiter");
			((Control)cmdSetJupiter).set_Size(new Size(13, 11));
			((Control)cmdSetJupiter).set_TabIndex(58);
			((ButtonBase)cmdSetJupiter).set_UseVisualStyleBackColor(false);
			((Control)cmdSetJupiter).add_Click((EventHandler)cmdSetJupiter_Click);
			((Control)chkS14).set_AutoSize(true);
			((Control)chkS14).set_Location(new Point(402, 103));
			((Control)chkS14).set_Name("chkS14");
			((Control)chkS14).set_Size(new Size(83, 17));
			((Control)chkS14).set_TabIndex(32);
			((Control)chkS14).set_Text("XIV Calypso");
			((ButtonBase)chkS14).set_UseVisualStyleBackColor(true);
			((Control)chkS13).set_AutoSize(true);
			((Control)chkS13).set_Location(new Point(313, 103));
			((Control)chkS13).set_Name("chkS13");
			((Control)chkS13).set_Size(new Size(80, 17));
			((Control)chkS13).set_TabIndex(31);
			((Control)chkS13).set_Text("XIII Telesto");
			((ButtonBase)chkS13).set_UseVisualStyleBackColor(true);
			((Control)chkS12).set_AutoSize(true);
			((Control)chkS12).set_Location(new Point(220, 103));
			((Control)chkS12).set_Name("chkS12");
			((Control)chkS12).set_Size(new Size(76, 17));
			((Control)chkS12).set_TabIndex(30);
			((Control)chkS12).set_Text("XII Helene");
			((ButtonBase)chkS12).set_UseVisualStyleBackColor(true);
			((Control)chkP5).set_AutoSize(true);
			((Control)chkP5).set_Location(new Point(481, 190));
			((Control)chkP5).set_Name("chkP5");
			((Control)chkP5).set_Size(new Size(56, 17));
			((Control)chkP5).set_TabIndex(57);
			((Control)chkP5).set_Text("V Styx");
			((ButtonBase)chkP5).set_UseVisualStyleBackColor(true);
			((Control)chkP4).set_AutoSize(true);
			((Control)chkP4).set_Location(new Point(402, 190));
			((Control)chkP4).set_Name("chkP4");
			((Control)chkP4).set_Size(new Size(81, 17));
			((Control)chkP4).set_TabIndex(56);
			((Control)chkP4).set_Text("IV Kerberos");
			((ButtonBase)chkP4).set_UseVisualStyleBackColor(true);
			((Control)cmdCompute).set_Anchor((AnchorStyles)4);
			((Control)cmdCompute).set_Location(new Point(80, 463));
			((Control)cmdCompute).set_Name("cmdCompute");
			((Control)cmdCompute).set_Size(new Size(88, 44));
			((Control)cmdCompute).set_TabIndex(72);
			((Control)cmdCompute).set_Text("Compute");
			((ButtonBase)cmdCompute).set_UseVisualStyleBackColor(true);
			((Control)cmdCompute).add_Click((EventHandler)cmdCompute_Click);
			((Control)cmdCancel).set_Anchor((AnchorStyles)4);
			((Control)cmdCancel).set_Location(new Point(80, 463));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(88, 44));
			((Control)cmdCancel).set_TabIndex(73);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((ToolStrip)menuStrip2).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withPredictionsToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip2).set_Location(new Point(0, 0));
			((Control)menuStrip2).set_Name("menuStrip2");
			((Control)menuStrip2).set_Size(new Size(890, 24));
			((Control)menuStrip2).set_TabIndex(3);
			((Control)menuStrip2).set_Text("menuStrip2");
			((ToolStripDropDownItem)withPredictionsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withPredictionsToolStripMenuItem).set_Name("withPredictionsToolStripMenuItem");
			((ToolStripItem)withPredictionsToolStripMenuItem).set_Size(new Size(119, 20));
			((ToolStripItem)withPredictionsToolStripMenuItem).set_Text("with Predictions...  ");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
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
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)groupBox2).set_Anchor((AnchorStyles)4);
			((Control)groupBox2).get_Controls().Add((Control)(object)label12);
			((Control)groupBox2).get_Controls().Add((Control)(object)optTT);
			((Control)groupBox2).get_Controls().Add((Control)(object)optUT);
			((Control)groupBox2).get_Controls().Add((Control)(object)label11);
			((Control)groupBox2).get_Controls().Add((Control)(object)updnIntervalDays);
			((Control)groupBox2).get_Controls().Add((Control)(object)label10);
			((Control)groupBox2).get_Controls().Add((Control)(object)updnDurationHours);
			((Control)groupBox2).get_Controls().Add((Control)(object)label9);
			((Control)groupBox2).get_Controls().Add((Control)(object)label8);
			((Control)groupBox2).get_Controls().Add((Control)(object)label7);
			((Control)groupBox2).get_Controls().Add((Control)(object)label6);
			((Control)groupBox2).get_Controls().Add((Control)(object)label5);
			((Control)groupBox2).get_Controls().Add((Control)(object)label4);
			((Control)groupBox2).get_Controls().Add((Control)(object)label3);
			((Control)groupBox2).get_Controls().Add((Control)(object)label2);
			((Control)groupBox2).get_Controls().Add((Control)(object)updnDurationDays);
			((Control)groupBox2).get_Controls().Add((Control)(object)label1);
			((Control)groupBox2).get_Controls().Add((Control)(object)updnHour);
			((Control)groupBox2).get_Controls().Add((Control)(object)updnIntervalHours);
			((Control)groupBox2).get_Controls().Add((Control)(object)updnDay);
			((Control)groupBox2).get_Controls().Add((Control)(object)updnMonth);
			((Control)groupBox2).get_Controls().Add((Control)(object)updnYear);
			((Control)groupBox2).set_Location(new Point(5, 246));
			((Control)groupBox2).set_Name("groupBox2");
			((Control)groupBox2).set_Size(new Size(264, 174));
			((Control)groupBox2).set_TabIndex(1);
			groupBox2.set_TabStop(false);
			((Control)groupBox2).set_Text("2.  Set dates for the prediction");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Location(new Point(97, 59));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(75, 13));
			((Control)label12).set_TabIndex(21);
			((Control)label12).set_Text("Time base is : ");
			((Control)optTT).set_AutoSize(true);
			((Control)optTT).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optTT).set_Location(new Point(214, 58));
			((Control)optTT).set_Name("optTT");
			((Control)optTT).set_Size(new Size(41, 17));
			((Control)optTT).set_TabIndex(20);
			((Control)optTT).set_Text("TT");
			((ButtonBase)optTT).set_UseVisualStyleBackColor(true);
			((Control)optUT).set_AutoSize(true);
			optUT.set_Checked(true);
			((Control)optUT).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optUT).set_Location(new Point(172, 58));
			((Control)optUT).set_Name("optUT");
			((Control)optUT).set_Size(new Size(42, 17));
			((Control)optUT).set_TabIndex(19);
			optUT.set_TabStop(true);
			((Control)optUT).set_Text("UT");
			((ButtonBase)optUT).set_UseVisualStyleBackColor(true);
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Location(new Point(77, 84));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(31, 13));
			((Control)label11).set_TabIndex(17);
			((Control)label11).set_Text("Days");
			((Control)updnIntervalDays).set_Location(new Point(79, 97));
			((Control)updnIntervalDays).set_Name("updnIntervalDays");
			((Control)updnIntervalDays).set_Size(new Size(46, 20));
			((Control)updnIntervalDays).set_TabIndex(18);
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(128, 123));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(35, 13));
			((Control)label10).set_TabIndex(16);
			((Control)label10).set_Text("Hours");
			updnDurationHours.set_DecimalPlaces(1);
			((Control)updnDurationHours).set_Location(new Point(131, 136));
			updnDurationHours.set_Maximum(new decimal(new int[4] { 23, 0, 0, 0 }));
			((Control)updnDurationHours).set_Name("updnDurationHours");
			((Control)updnDurationHours).set_Size(new Size(42, 20));
			((Control)updnDurationHours).set_TabIndex(15);
			((Control)chkHighPrecision).set_AutoSize(true);
			((Control)chkHighPrecision).set_Location(new Point(19, 429));
			((Control)chkHighPrecision).set_Name("chkHighPrecision");
			((Control)chkHighPrecision).set_Size(new Size(94, 17));
			((Control)chkHighPrecision).set_TabIndex(74);
			((Control)chkHighPrecision).set_Text("High Precision");
			((ButtonBase)chkHighPrecision).set_UseVisualStyleBackColor(true);
			chkHighPrecision.add_CheckedChanged((EventHandler)chkHighPrecision_CheckedChanged);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(890, 539));
			((Control)this).get_Controls().Add((Control)(object)chkHighPrecision);
			((Control)this).get_Controls().Add((Control)(object)groupBox2);
			((Control)this).get_Controls().Add((Control)(object)cmdCompute);
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Control)this).get_Controls().Add((Control)(object)lstPrediction);
			((Control)this).get_Controls().Add((Control)(object)menuStrip2);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationSatellitePositions", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationSatellitePositions);
			((Control)this).set_MinimumSize(new Size(800, 500));
			((Control)this).set_Name("SatellitePositions");
			((Control)this).set_Text("Positions of satellites of the major Planets, and of Pluto");
			((Form)this).add_Load((EventHandler)SatellitePositions_Load);
			((Control)this).add_Resize((EventHandler)SatellitePositions_Resize);
			((ISupportInitialize)updnHour).EndInit();
			((ISupportInitialize)updnIntervalHours).EndInit();
			((ISupportInitialize)updnDay).EndInit();
			((ISupportInitialize)updnMonth).EndInit();
			((ISupportInitialize)updnYear).EndInit();
			((ISupportInitialize)updnDurationDays).EndInit();
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((Control)menuStrip2).ResumeLayout(false);
			((Control)menuStrip2).PerformLayout();
			((Control)groupBox2).ResumeLayout(false);
			((Control)groupBox2).PerformLayout();
			((ISupportInitialize)updnIntervalDays).EndInit();
			((ISupportInitialize)updnDurationHours).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
