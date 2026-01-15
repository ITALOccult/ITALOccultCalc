using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using System.Windows.Forms;
using GifCreator;
using Occult.Properties;

namespace Occult.File_Actions
{
	public class CloudMap : Form
	{
		private bool IsLoaded;

		private string LastURL = "";

		private int tau_Value;

		private string INITstring = "          ";

		private int INITyr;

		private int INITmth;

		private int INITday;

		private int INIThr;

		private int StartYr;

		private int StartMth;

		private int StartDay;

		private string StartDate;

		private DateTime InitialDate;

		private int CurrentImageDisplayed;

		private IContainer components;

		private PictureBox picCloud;

		private GroupBox groupBox1;

		private RadioButton optAsia;

		private RadioButton optEurope;

		private RadioButton optNthAmerica;

		private RadioButton optSthAmerica;

		private RadioButton optAfrica;

		private RadioButton optAustralasia;

		private RadioButton optWorld;

		private GroupBox groupBox2;

		private RadioButton optTotalCloud;

		private RadioButton optLowCloud;

		private RadioButton optHighCloud;

		private RadioButton optMiddleCloud;

		private RadioButton optTemperature;

		private RadioButton optExtinction;

		private GroupBox grpDateTime;

		private RadioButton opt0;

		private Label lblDate4;

		private RadioButton opt75;

		private RadioButton opt78;

		private RadioButton opt81;

		private RadioButton opt84;

		private RadioButton opt87;

		private RadioButton opt90;

		private RadioButton opt93;

		private RadioButton opt72;

		private Label lblDate3;

		private RadioButton opt51;

		private RadioButton opt54;

		private RadioButton opt57;

		private RadioButton opt60;

		private RadioButton opt63;

		private RadioButton opt66;

		private RadioButton opt69;

		private RadioButton opt48;

		private Label lblDate2;

		private RadioButton opt27;

		private RadioButton opt30;

		private RadioButton opt33;

		private RadioButton opt36;

		private RadioButton opt39;

		private RadioButton opt42;

		private RadioButton opt45;

		private RadioButton opt24;

		private Label lblDate1;

		private RadioButton opt3;

		private RadioButton opt6;

		private RadioButton opt9;

		private RadioButton opt12;

		private RadioButton opt15;

		private RadioButton opt18;

		private RadioButton opt21;

		private Label lblDate16;

		private Label lblDate15;

		private RadioButton opt348;

		private RadioButton opt360;

		private RadioButton opt372;

		private RadioButton opt336;

		private Label lblDate14;

		private Label lblDate13;

		private Label lblDate12;

		private RadioButton opt276;

		private RadioButton opt288;

		private RadioButton opt300;

		private RadioButton opt312;

		private RadioButton opt324;

		private RadioButton opt264;

		private Label lblDate10;

		private Label lblDate9;

		private Label lblDate8;

		private RadioButton opt204;

		private RadioButton opt216;

		private RadioButton opt228;

		private RadioButton opt240;

		private RadioButton opt252;

		private RadioButton opt192;

		private Label lblDate7;

		private RadioButton opt171;

		private RadioButton opt174;

		private RadioButton opt177;

		private RadioButton opt180;

		private RadioButton opt168;

		private Label lblDate6;

		private RadioButton opt147;

		private RadioButton opt150;

		private RadioButton opt153;

		private RadioButton opt156;

		private RadioButton opt159;

		private RadioButton opt162;

		private RadioButton opt165;

		private RadioButton opt144;

		private Label lblDate5;

		private RadioButton opt123;

		private RadioButton opt126;

		private RadioButton opt129;

		private RadioButton opt132;

		private RadioButton opt135;

		private RadioButton opt138;

		private RadioButton opt141;

		private RadioButton opt120;

		private Label lblDate11;

		private RadioButton opt99;

		private RadioButton opt102;

		private RadioButton opt105;

		private RadioButton opt108;

		private RadioButton opt111;

		private RadioButton opt114;

		private RadioButton opt117;

		private RadioButton opt96;

		private RadioButton optWind;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withMapToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private RadioButton opt183;

		private RadioButton opt186;

		private RadioButton opt189;

		private PictureBox picCloud_2;

		private PictureBox picCloud_3;

		private PictureBox picCloud1;

		private PictureBox picCloud_1;

		private CheckBox chkAnimated;

		private Timer timer1;

		private TrackBar TrackSpeed;

		private ToolStripMenuItem saveAnimationAsAnimatedGIFToolStripMenuItem;

		private Panel Panel;

		private Label label1;

		private PictureBox picCloudDisplay;

		private Label label3;

		private Label label2;

		public CloudMap()
		{
			InitializeComponent();
		}

		private void CloudMap_Load(object sender, EventArgs e)
		{
			//IL_0838: Unknown result type (might be due to invalid IL or missing references)
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			if (Screen.GetWorkingArea((Control)(object)this).Height < 880)
			{
				((Control)this).set_Height(Screen.GetWorkingArea((Control)(object)this).Height - 40);
			}
			int cloudMapRegion = Settings.Default.CloudMapRegion;
			optWorld.set_Checked(cloudMapRegion == 0);
			optAsia.set_Checked(cloudMapRegion == 1);
			optEurope.set_Checked(cloudMapRegion == 2);
			optNthAmerica.set_Checked(cloudMapRegion == 3);
			optAustralasia.set_Checked(cloudMapRegion == 4);
			optAfrica.set_Checked(cloudMapRegion == 5);
			optSthAmerica.set_Checked(cloudMapRegion == 6);
			IsLoaded = true;
			StartYr = DateTime.Now.ToUniversalTime().Year;
			StartMth = DateTime.Now.ToUniversalTime().Month;
			StartDay = DateTime.Now.ToUniversalTime().Day;
			InitialDate = new DateTime(StartYr, StartMth, StartDay);
			string startDate;
			((Control)lblDate1).set_Text(startDate = Utilities.ShortMonths[DateTime.Now.ToUniversalTime().Month] + DateTime.Now.ToUniversalTime().Day.ToString().PadLeft(3));
			StartDate = startDate;
			((Control)lblDate2).set_Text(Utilities.ShortMonths[DateTime.Now.AddDays(1.0).ToUniversalTime().Month] + DateTime.Now.AddDays(1.0).ToUniversalTime().Day.ToString().PadLeft(3));
			((Control)lblDate3).set_Text(Utilities.ShortMonths[DateTime.Now.AddDays(2.0).ToUniversalTime().Month] + DateTime.Now.AddDays(2.0).ToUniversalTime().Day.ToString().PadLeft(3));
			((Control)lblDate4).set_Text(Utilities.ShortMonths[DateTime.Now.AddDays(3.0).ToUniversalTime().Month] + DateTime.Now.AddDays(3.0).ToUniversalTime().Day.ToString().PadLeft(3));
			((Control)lblDate5).set_Text(Utilities.ShortMonths[DateTime.Now.AddDays(4.0).ToUniversalTime().Month] + DateTime.Now.AddDays(4.0).ToUniversalTime().Day.ToString().PadLeft(3));
			((Control)lblDate6).set_Text(Utilities.ShortMonths[DateTime.Now.AddDays(5.0).ToUniversalTime().Month] + DateTime.Now.AddDays(5.0).ToUniversalTime().Day.ToString().PadLeft(3));
			((Control)lblDate7).set_Text(Utilities.ShortMonths[DateTime.Now.AddDays(6.0).ToUniversalTime().Month] + DateTime.Now.AddDays(6.0).ToUniversalTime().Day.ToString().PadLeft(3));
			((Control)lblDate8).set_Text(Utilities.ShortMonths[DateTime.Now.AddDays(7.0).ToUniversalTime().Month] + DateTime.Now.AddDays(7.0).ToUniversalTime().Day.ToString().PadLeft(3));
			((Control)lblDate9).set_Text(Utilities.ShortMonths[DateTime.Now.AddDays(8.0).ToUniversalTime().Month] + DateTime.Now.AddDays(8.0).ToUniversalTime().Day.ToString().PadLeft(3));
			((Control)lblDate10).set_Text(Utilities.ShortMonths[DateTime.Now.AddDays(9.0).ToUniversalTime().Month] + DateTime.Now.AddDays(9.0).ToUniversalTime().Day.ToString().PadLeft(3));
			((Control)lblDate11).set_Text(Utilities.ShortMonths[DateTime.Now.AddDays(10.0).ToUniversalTime().Month] + DateTime.Now.AddDays(10.0).ToUniversalTime().Day.ToString().PadLeft(3));
			((Control)lblDate12).set_Text(Utilities.ShortMonths[DateTime.Now.AddDays(11.0).ToUniversalTime().Month] + DateTime.Now.AddDays(11.0).ToUniversalTime().Day.ToString().PadLeft(3));
			((Control)lblDate13).set_Text(Utilities.ShortMonths[DateTime.Now.AddDays(12.0).ToUniversalTime().Month] + DateTime.Now.AddDays(12.0).ToUniversalTime().Day.ToString().PadLeft(3));
			((Control)lblDate14).set_Text(Utilities.ShortMonths[DateTime.Now.AddDays(13.0).ToUniversalTime().Month] + DateTime.Now.AddDays(13.0).ToUniversalTime().Day.ToString().PadLeft(3));
			((Control)lblDate15).set_Text(Utilities.ShortMonths[DateTime.Now.AddDays(14.0).ToUniversalTime().Month] + DateTime.Now.AddDays(14.0).ToUniversalTime().Day.ToString().PadLeft(3));
			((Control)lblDate16).set_Text(Utilities.ShortMonths[DateTime.Now.AddDays(15.0).ToUniversalTime().Month] + DateTime.Now.AddDays(15.0).ToUniversalTime().Day.ToString().PadLeft(3));
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
				return;
			}
			http.DownloadHTTP(Settings.Default.SevenTimer + "/dip/server", "weachart.flag", Utilities.AppPath + "\\Downloaded Files\\WeachartINIT.txt", unzip: false, gunzip: false, ShowMessages: false);
			if (File.Exists(Utilities.AppPath + "\\DownLoaded Files\\WeachartINIT.txt"))
			{
				using StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\DownLoaded Files\\WeachartINIT.txt");
				INITstring = streamReader.ReadLine()!.PadRight(10);
			}
			else
			{
				INITstring = "          ";
			}
			if (!int.TryParse(INITstring.Substring(0, 4), out INITyr))
			{
				INITyr = 0;
			}
			if (!int.TryParse(INITstring.Substring(4, 2), out INITmth))
			{
				INITmth = 0;
			}
			if (!int.TryParse(INITstring.Substring(6, 2), out INITday))
			{
				INITday = 0;
			}
			if (!int.TryParse(INITstring.Substring(8, 2), out INIThr))
			{
				INIThr = 0;
			}
			if ((INIThr == 6) | (INIThr == 18))
			{
				SetTimeLabels(SixHrs: true);
			}
			else
			{
				SetTimeLabels(SixHrs: false);
			}
			PictureBox obj = picCloud_1;
			PictureBox obj2 = picCloud_2;
			PictureBox obj3 = picCloud_3;
			int top;
			((Control)picCloud1).set_Top(top = ((Control)picCloud).get_Top());
			int num;
			((Control)obj3).set_Top(num = top);
			int top2;
			((Control)obj2).set_Top(top2 = num);
			((Control)obj).set_Top(top2);
			PictureBox obj4 = picCloud_1;
			PictureBox obj5 = picCloud_2;
			PictureBox obj6 = picCloud_3;
			((Control)picCloud1).set_Left(top = ((Control)picCloud).get_Left());
			((Control)obj6).set_Left(num = top);
			((Control)obj5).set_Left(top2 = num);
			((Control)obj4).set_Left(top2);
			((Control)grpDateTime).set_Text("Date and time  [ UTC ]" + "".PadRight(60) + string.Format("Starting point for prediction :  {0,4:f0} ", INITyr) + Utilities.ShortMonths[INITmth] + string.Format(" {0,1:f0}, at {1,1:f0} hrs UT", INITday, INIThr));
		}

		public void SetRequestDate(DateTime UTRequestDate)
		{
			TimeSpan timeSpan = UTRequestDate.Subtract(InitialDate);
			int num = 24 * timeSpan.Days + timeSpan.Hours;
			num = 3 * (num / 3);
			if (num < 0 || num > 372)
			{
				num = 0;
			}
			IsLoaded = false;
			opt0.set_Checked(num >= 0);
			opt3.set_Checked(num >= 3);
			opt6.set_Checked(num >= 6);
			opt9.set_Checked(num >= 9);
			opt12.set_Checked(num >= 12);
			opt15.set_Checked(num >= 15);
			opt18.set_Checked(num >= 18);
			opt21.set_Checked(num >= 21);
			opt24.set_Checked(num >= 24);
			opt27.set_Checked(num >= 27);
			opt30.set_Checked(num >= 30);
			opt33.set_Checked(num >= 33);
			opt36.set_Checked(num >= 36);
			opt39.set_Checked(num >= 39);
			opt42.set_Checked(num >= 42);
			opt45.set_Checked(num >= 45);
			opt48.set_Checked(num >= 48);
			opt51.set_Checked(num >= 51);
			opt54.set_Checked(num >= 54);
			opt57.set_Checked(num >= 57);
			opt60.set_Checked(num >= 60);
			opt63.set_Checked(num >= 63);
			opt66.set_Checked(num >= 66);
			opt69.set_Checked(num >= 69);
			opt72.set_Checked(num >= 72);
			opt75.set_Checked(num >= 75);
			opt78.set_Checked(num >= 78);
			opt81.set_Checked(num >= 81);
			opt84.set_Checked(num >= 84);
			opt87.set_Checked(num >= 87);
			opt90.set_Checked(num >= 90);
			opt93.set_Checked(num >= 93);
			opt96.set_Checked(num >= 96);
			opt99.set_Checked(num >= 99);
			opt102.set_Checked(num >= 102);
			opt105.set_Checked(num >= 105);
			opt108.set_Checked(num >= 108);
			opt111.set_Checked(num >= 111);
			opt114.set_Checked(num >= 114);
			opt117.set_Checked(num >= 117);
			opt120.set_Checked(num >= 120);
			opt123.set_Checked(num >= 123);
			opt126.set_Checked(num >= 126);
			opt129.set_Checked(num >= 129);
			opt132.set_Checked(num >= 132);
			opt135.set_Checked(num >= 135);
			opt138.set_Checked(num >= 138);
			opt141.set_Checked(num >= 141);
			opt144.set_Checked(num >= 144);
			opt147.set_Checked(num >= 147);
			opt150.set_Checked(num >= 150);
			opt153.set_Checked(num >= 153);
			opt156.set_Checked(num >= 156);
			opt159.set_Checked(num >= 159);
			opt162.set_Checked(num >= 162);
			opt165.set_Checked(num >= 165);
			opt168.set_Checked(num >= 168);
			opt171.set_Checked(num >= 171);
			opt174.set_Checked(num >= 174);
			if (((Control)opt177).get_Visible())
			{
				opt177.set_Checked(num >= 177);
			}
			opt180.set_Checked(num >= 180);
			if (((Control)opt183).get_Visible())
			{
				opt183.set_Checked(num >= 183);
			}
			if (((Control)opt186).get_Visible())
			{
				opt186.set_Checked(num >= 186);
			}
			num = 12 * (num / 12);
			opt192.set_Checked(num >= 192);
			opt204.set_Checked(num >= 204);
			opt216.set_Checked(num >= 216);
			opt228.set_Checked(num >= 228);
			opt240.set_Checked(num >= 240);
			opt252.set_Checked(num >= 252);
			opt264.set_Checked(num >= 264);
			opt276.set_Checked(num >= 276);
			opt288.set_Checked(num >= 288);
			opt300.set_Checked(num >= 300);
			opt312.set_Checked(num >= 312);
			opt324.set_Checked(num >= 324);
			opt336.set_Checked(num >= 336);
			opt348.set_Checked(num >= 348);
			opt360.set_Checked(num >= 360);
			opt372.set_Checked(num >= 372);
			IsLoaded = true;
			Application.DoEvents();
			GetCloudMap();
		}

		private void GetCloudMap()
		{
			//IL_0201: Unknown result type (might be due to invalid IL or missing references)
			//IL_0337: Unknown result type (might be due to invalid IL or missing references)
			if (!IsLoaded)
			{
				return;
			}
			string[] obj = new string[7] { "world", "china", "europe", "namerica", "oceania", "safrica", "samerica" };
			string text = "tcdcclm";
			string text2 = obj[Settings.Default.CloudMapRegion];
			if (optTotalCloud.get_Checked())
			{
				text = "tcdcclm";
			}
			else if (optHighCloud.get_Checked())
			{
				text = "tcdchcl";
			}
			else if (optMiddleCloud.get_Checked())
			{
				text = "tcdcmcl";
			}
			else if (optLowCloud.get_Checked())
			{
				text = "tcdclcl";
			}
			else if (optExtinction.get_Checked())
			{
				text = "trans";
			}
			else if (optTemperature.get_Checked())
			{
				text = "tmp2m";
			}
			else if (optWind.get_Checked())
			{
				text = "wind10m";
			}
			tau_Value = Get_tau_value();
			string text3 = "00z";
			switch (INIThr)
			{
			case 0:
				text3 = "00z";
				break;
			case 6:
				text3 = "06z";
				if (tau_Value <= 186)
				{
					tau_Value -= 6;
				}
				break;
			case 12:
				text3 = "12z";
				tau_Value -= 12;
				break;
			case 18:
				text3 = "18z";
				if (tau_Value < 180)
				{
					tau_Value -= 18;
				}
				else
				{
					tau_Value -= 12;
				}
				break;
			default:
				text3 = "00z";
				break;
			}
			if (StartDay != INITday)
			{
				tau_Value += 24;
			}
			if (chkAnimated.get_Checked() & (tau_Value < 12))
			{
				tau_Value = 12;
			}
			if (chkAnimated.get_Checked() & (tau_Value > 172))
			{
				MessageBox.Show("Animation is not enabled for dates more than 7 days in the future", "No animation", (MessageBoxButtons)0, (MessageBoxIcon)16);
				chkAnimated.set_Checked(false);
			}
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			Application.DoEvents();
			for (int i = -9; i <= 3; i += 3)
			{
				if (!chkAnimated.get_Checked() && i != 0)
				{
					continue;
				}
				string text4 = (tau_Value + i).ToString().PadLeft(2, '0');
				string text5 = string.Format(Settings.Default.SevenTimer + "/img/chart/" + text3 + "/" + text2 + "/" + text + "_" + text4 + ".png");
				if (LastURL != text5)
				{
					try
					{
						if (i == -9)
						{
							picCloud_3.Load(text5);
						}
						if (i == -6)
						{
							picCloud_2.Load(text5);
						}
						if (i == -3)
						{
							picCloud_1.Load(text5);
						}
						if (i == 0)
						{
							picCloud.Load(text5);
						}
						if (i == 3)
						{
							picCloud1.Load(text5);
						}
					}
					catch
					{
						MessageBox.Show("Requested weather map is not available.", "No map");
					}
				}
				LastURL = text5;
				SetMapDisplayed(i);
			}
			try
			{
				((Control)picCloudDisplay).set_Width(picCloud.get_Image().Width);
				((Control)picCloudDisplay).set_Height(picCloud.get_Image().Height);
				((Control)picCloudDisplay).set_Left((((Control)Panel).get_Width() - ((Control)picCloud).get_Width()) / 2 - 10);
				((Control)picCloudDisplay).set_Top(((Control)picCloud).get_Top());
			}
			catch
			{
			}
			((Control)this).set_Cursor(Cursors.get_Default());
		}

		private void SetMapDisplayed(int Toffset)
		{
			if (Toffset == -9)
			{
				picCloudDisplay.set_Image(picCloud_3.get_Image());
			}
			if (Toffset == -6)
			{
				picCloudDisplay.set_Image(picCloud_2.get_Image());
			}
			if (Toffset == -3)
			{
				picCloudDisplay.set_Image(picCloud_1.get_Image());
			}
			if (Toffset == 0)
			{
				picCloudDisplay.set_Image(picCloud.get_Image());
			}
			if (Toffset >= 3)
			{
				picCloudDisplay.set_Image(picCloud1.get_Image());
			}
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picCloud.get_Image());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			string text = "";
			if (optTotalCloud.get_Checked())
			{
				text = " Total cloud ";
			}
			else if (optHighCloud.get_Checked())
			{
				text = " High cloud ";
			}
			else if (optMiddleCloud.get_Checked())
			{
				text = " Middle cloud ";
			}
			else if (optLowCloud.get_Checked())
			{
				text = " Low cloud ";
			}
			else if (optExtinction.get_Checked())
			{
				text = " Extinction ";
			}
			else if (optTemperature.get_Checked())
			{
				text = " Temp ";
			}
			else if (optWind.get_Checked())
			{
				text = " Wind ";
			}
			Settings.Default.Save_CloudMap = Output.SaveGraphic(picCloud.get_Image(), text + StartDate + " + " + tau_Value + "hrs", Settings.Default.Save_CloudMap);
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void optWorld_CheckedChanged(object sender, EventArgs e)
		{
			SetRegion();
		}

		private void optAsia_CheckedChanged(object sender, EventArgs e)
		{
			SetRegion();
		}

		private void optEurope_CheckedChanged(object sender, EventArgs e)
		{
			SetRegion();
		}

		private void optNthAmerica_CheckedChanged(object sender, EventArgs e)
		{
			SetRegion();
		}

		private void optAustralasia_CheckedChanged(object sender, EventArgs e)
		{
			SetRegion();
		}

		private void optAfrica_CheckedChanged(object sender, EventArgs e)
		{
			SetRegion();
		}

		private void optSthAmerica_CheckedChanged(object sender, EventArgs e)
		{
			SetRegion();
		}

		private void SetRegion()
		{
			if (IsLoaded)
			{
				int cloudMapRegion = 0;
				if (optWorld.get_Checked())
				{
					cloudMapRegion = 0;
				}
				else if (optAsia.get_Checked())
				{
					cloudMapRegion = 1;
				}
				else if (optEurope.get_Checked())
				{
					cloudMapRegion = 2;
				}
				else if (optNthAmerica.get_Checked())
				{
					cloudMapRegion = 3;
				}
				else if (optAustralasia.get_Checked())
				{
					cloudMapRegion = 4;
				}
				else if (optAfrica.get_Checked())
				{
					cloudMapRegion = 5;
				}
				else if (optSthAmerica.get_Checked())
				{
					cloudMapRegion = 6;
				}
				Settings.Default.CloudMapRegion = cloudMapRegion;
				GetCloudMap();
			}
		}

		private void optTotalCloud_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void optHighCloud_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void optMiddleCloud_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void optLowCloud_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void optExtinction_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void optTemperature_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private int Get_tau_value()
		{
			if (opt0.get_Checked())
			{
				return 0;
			}
			if (opt0.get_Checked())
			{
				return 0;
			}
			if (opt3.get_Checked())
			{
				return 3;
			}
			if (opt6.get_Checked())
			{
				return 6;
			}
			if (opt9.get_Checked())
			{
				return 9;
			}
			if (opt12.get_Checked())
			{
				return 12;
			}
			if (opt15.get_Checked())
			{
				return 15;
			}
			if (opt18.get_Checked())
			{
				return 18;
			}
			if (opt21.get_Checked())
			{
				return 21;
			}
			if (opt24.get_Checked())
			{
				return 24;
			}
			if (opt27.get_Checked())
			{
				return 27;
			}
			if (opt30.get_Checked())
			{
				return 30;
			}
			if (opt33.get_Checked())
			{
				return 33;
			}
			if (opt36.get_Checked())
			{
				return 36;
			}
			if (opt39.get_Checked())
			{
				return 39;
			}
			if (opt42.get_Checked())
			{
				return 42;
			}
			if (opt45.get_Checked())
			{
				return 45;
			}
			if (opt48.get_Checked())
			{
				return 48;
			}
			if (opt51.get_Checked())
			{
				return 51;
			}
			if (opt54.get_Checked())
			{
				return 54;
			}
			if (opt57.get_Checked())
			{
				return 57;
			}
			if (opt60.get_Checked())
			{
				return 60;
			}
			if (opt63.get_Checked())
			{
				return 63;
			}
			if (opt66.get_Checked())
			{
				return 66;
			}
			if (opt69.get_Checked())
			{
				return 69;
			}
			if (opt72.get_Checked())
			{
				return 72;
			}
			if (opt75.get_Checked())
			{
				return 75;
			}
			if (opt78.get_Checked())
			{
				return 78;
			}
			if (opt81.get_Checked())
			{
				return 81;
			}
			if (opt84.get_Checked())
			{
				return 84;
			}
			if (opt87.get_Checked())
			{
				return 87;
			}
			if (opt90.get_Checked())
			{
				return 90;
			}
			if (opt93.get_Checked())
			{
				return 93;
			}
			if (opt96.get_Checked())
			{
				return 96;
			}
			if (opt99.get_Checked())
			{
				return 99;
			}
			if (opt102.get_Checked())
			{
				return 102;
			}
			if (opt105.get_Checked())
			{
				return 105;
			}
			if (opt108.get_Checked())
			{
				return 108;
			}
			if (opt111.get_Checked())
			{
				return 111;
			}
			if (opt114.get_Checked())
			{
				return 114;
			}
			if (opt117.get_Checked())
			{
				return 117;
			}
			if (opt120.get_Checked())
			{
				return 120;
			}
			if (opt123.get_Checked())
			{
				return 123;
			}
			if (opt126.get_Checked())
			{
				return 126;
			}
			if (opt129.get_Checked())
			{
				return 129;
			}
			if (opt132.get_Checked())
			{
				return 132;
			}
			if (opt135.get_Checked())
			{
				return 135;
			}
			if (opt138.get_Checked())
			{
				return 138;
			}
			if (opt141.get_Checked())
			{
				return 141;
			}
			if (opt144.get_Checked())
			{
				return 144;
			}
			if (opt147.get_Checked())
			{
				return 147;
			}
			if (opt150.get_Checked())
			{
				return 150;
			}
			if (opt153.get_Checked())
			{
				return 153;
			}
			if (opt156.get_Checked())
			{
				return 156;
			}
			if (opt159.get_Checked())
			{
				return 159;
			}
			if (opt162.get_Checked())
			{
				return 162;
			}
			if (opt165.get_Checked())
			{
				return 165;
			}
			if (opt168.get_Checked())
			{
				return 168;
			}
			if (opt171.get_Checked())
			{
				return 171;
			}
			if (opt174.get_Checked())
			{
				return 174;
			}
			if (opt177.get_Checked())
			{
				return 177;
			}
			if (opt180.get_Checked())
			{
				return 180;
			}
			if (opt183.get_Checked())
			{
				return 183;
			}
			if (opt186.get_Checked())
			{
				return 186;
			}
			if (opt189.get_Checked())
			{
				return 189;
			}
			if (opt192.get_Checked())
			{
				return 192;
			}
			if (opt204.get_Checked())
			{
				return 204;
			}
			if (opt216.get_Checked())
			{
				return 216;
			}
			if (opt228.get_Checked())
			{
				return 228;
			}
			if (opt240.get_Checked())
			{
				return 240;
			}
			if (opt252.get_Checked())
			{
				return 252;
			}
			if (opt264.get_Checked())
			{
				return 264;
			}
			if (opt276.get_Checked())
			{
				return 276;
			}
			if (opt288.get_Checked())
			{
				return 288;
			}
			if (opt300.get_Checked())
			{
				return 300;
			}
			if (opt312.get_Checked())
			{
				return 312;
			}
			if (opt324.get_Checked())
			{
				return 324;
			}
			if (opt336.get_Checked())
			{
				return 336;
			}
			if (opt348.get_Checked())
			{
				return 348;
			}
			if (opt360.get_Checked())
			{
				return 360;
			}
			if (opt372.get_Checked())
			{
				return 372;
			}
			return 0;
		}

		private void opt0_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt3_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt6_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt9_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt12_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt15_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt18_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt21_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt24_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt27_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt30_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt33_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt36_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt39_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt42_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt45_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt48_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt51_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt54_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt57_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt60_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt63_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt66_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt69_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt72_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt75_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt78_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt81_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt84_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt87_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt90_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt93_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt96_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt99_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt102_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt105_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt108_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt111_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt114_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt117_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt120_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt123_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt126_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt129_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt132_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt135_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt138_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt141_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt144_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt147_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt150_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt153_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt156_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt159_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt162_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt165_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt168_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt171_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt174_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt177_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt180_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt183_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt186_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt189_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt192_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt204_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt216_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt228_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt240_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt252_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt264_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt276_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt288_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt300_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt312_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt324_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt336_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt348_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt360_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void opt372_CheckedChanged(object sender, EventArgs e)
		{
			GetCloudMap();
		}

		private void SetTimeLabels(bool SixHrs)
		{
			string text = "0 hr";
			if (SixHrs)
			{
				text = "6 hr";
			}
			RadioButton obj = opt192;
			RadioButton obj2 = opt216;
			RadioButton obj3 = opt240;
			RadioButton obj4 = opt264;
			RadioButton obj5 = opt288;
			RadioButton obj6 = opt312;
			RadioButton obj7 = opt336;
			string text2;
			((Control)opt360).set_Text(text2 = text);
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
			text = "12 hr";
			if (SixHrs)
			{
				text = "18 hr";
			}
			RadioButton obj8 = opt204;
			RadioButton obj9 = opt228;
			RadioButton obj10 = opt252;
			RadioButton obj11 = opt276;
			RadioButton obj12 = opt300;
			RadioButton obj13 = opt324;
			RadioButton obj14 = opt348;
			((Control)opt372).set_Text(text2 = text);
			((Control)obj14).set_Text(text3 = text2);
			((Control)obj13).set_Text(text4 = text3);
			((Control)obj12).set_Text(text5 = text4);
			((Control)obj11).set_Text(text6 = text5);
			((Control)obj10).set_Text(text7 = text6);
			((Control)obj9).set_Text(text8 = text7);
			((Control)obj8).set_Text(text8);
			if (INIThr == 0)
			{
				((Control)opt180).set_Text(text);
				RadioButton obj15 = opt183;
				RadioButton obj16 = opt186;
				bool flag;
				((Control)opt189).set_Visible(flag = false);
				bool visible;
				((Control)obj16).set_Visible(visible = flag);
				((Control)obj15).set_Visible(visible);
			}
			if (INIThr == 6)
			{
				if (INITday == StartDay)
				{
					RadioButton obj17 = opt0;
					bool visible;
					((Control)opt3).set_Enabled(visible = false);
					((Control)obj17).set_Enabled(visible);
				}
				((Control)opt189).set_Visible(false);
			}
			else if (INIThr == 12)
			{
				if (INITday == StartDay)
				{
					RadioButton obj18 = opt0;
					RadioButton obj19 = opt3;
					RadioButton obj20 = opt6;
					bool flag2;
					((Control)opt9).set_Enabled(flag2 = false);
					bool flag;
					((Control)obj20).set_Enabled(flag = flag2);
					bool visible;
					((Control)obj19).set_Enabled(visible = flag);
					((Control)obj18).set_Enabled(visible);
				}
			}
			else if (INIThr == 18)
			{
				if (INITday != StartDay)
				{
					((Control)opt180).set_Text(text);
				}
				bool flag2;
				bool flag;
				bool visible;
				if (INITday == StartDay)
				{
					RadioButton obj21 = opt0;
					RadioButton obj22 = opt3;
					RadioButton obj23 = opt6;
					RadioButton obj24 = opt9;
					RadioButton obj25 = opt12;
					bool flag3;
					((Control)opt15).set_Enabled(flag3 = false);
					bool flag4;
					((Control)obj25).set_Enabled(flag4 = flag3);
					((Control)obj24).set_Enabled(flag2 = flag4);
					((Control)obj23).set_Enabled(flag = flag2);
					((Control)obj22).set_Enabled(visible = flag);
					((Control)obj21).set_Enabled(visible);
				}
				RadioButton obj26 = opt177;
				RadioButton obj27 = opt183;
				RadioButton obj28 = opt186;
				((Control)opt189).set_Visible(flag2 = false);
				((Control)obj28).set_Visible(flag = flag2);
				((Control)obj27).set_Visible(visible = flag);
				((Control)obj26).set_Visible(visible);
			}
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Weather maps");
		}

		private void CloudMap_FormClosed(object sender, FormClosedEventArgs e)
		{
			((Component)this).Dispose();
		}

		private void chkAnimated_CheckedChanged(object sender, EventArgs e)
		{
			timer1.set_Enabled(chkAnimated.get_Checked());
			((ToolStripItem)saveAnimationAsAnimatedGIFToolStripMenuItem).set_Enabled(chkAnimated.get_Checked());
			GetCloudMap();
		}

		private void timer1_Tick(object sender, EventArgs e)
		{
			CurrentImageDisplayed++;
			if (CurrentImageDisplayed > 6)
			{
				CurrentImageDisplayed = 0;
			}
			SetMapDisplayed(3 * CurrentImageDisplayed - 9);
			timer1.set_Interval((TrackSpeed.get_Maximum() + TrackSpeed.get_Minimum() - TrackSpeed.get_Value()) * 100);
		}

		private void saveAnimationAsAnimatedGIFToolStripMenuItem_Click(object sender, EventArgs e)
		{
			List<string> list = new List<string>();
			int num = 0;
			Settings.Default.GIFRepeats = 0m;
			Settings.Default.GIFdelay = (TrackSpeed.get_Maximum() + TrackSpeed.get_Minimum() - TrackSpeed.get_Value()) / 10;
			string path = Utilities.AppPath + "\\Predictions\\AnimatedGIF";
			if (!Directory.Exists(path))
			{
				Directory.CreateDirectory(path);
			}
			string[] files = Directory.GetFiles(path, "*.gif");
			foreach (string path2 in files)
			{
				try
				{
					File.Delete(path2);
				}
				catch
				{
				}
			}
			for (int j = 0; j <= 6; j++)
			{
				string text = Utilities.AppPath + "\\Predictions\\AnimatedGIF\\" + num + ".gif";
				try
				{
					switch (j)
					{
					case 0:
						picCloud_3.get_Image().Save(text, ImageFormat.Gif);
						break;
					case 1:
						picCloud_2.get_Image().Save(text, ImageFormat.Gif);
						break;
					case 2:
						picCloud_1.get_Image().Save(text, ImageFormat.Gif);
						break;
					case 3:
						picCloud.get_Image().Save(text, ImageFormat.Gif);
						break;
					case 4:
						picCloud1.get_Image().Save(text, ImageFormat.Gif);
						break;
					case 5:
						picCloud1.get_Image().Save(text, ImageFormat.Gif);
						break;
					case 6:
						picCloud1.get_Image().Save(text, ImageFormat.Gif);
						break;
					}
					list.Add(text);
				}
				catch
				{
				}
				num++;
			}
			global::GifCreator.GifCreator.Create_Animated_Gif(list, "Cloud.gif");
		}

		private void CloudMap_Resize(object sender, EventArgs e)
		{
			((Control)Panel).set_Height(((Control)this).get_Height() - 68);
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
			//IL_0581: Unknown result type (might be due to invalid IL or missing references)
			//IL_058b: Expected O, but got Unknown
			//IL_058c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0596: Expected O, but got Unknown
			//IL_0597: Unknown result type (might be due to invalid IL or missing references)
			//IL_05a1: Expected O, but got Unknown
			//IL_05a2: Unknown result type (might be due to invalid IL or missing references)
			//IL_05ac: Expected O, but got Unknown
			//IL_05ad: Unknown result type (might be due to invalid IL or missing references)
			//IL_05b7: Expected O, but got Unknown
			//IL_05b8: Unknown result type (might be due to invalid IL or missing references)
			//IL_05c2: Expected O, but got Unknown
			//IL_05c3: Unknown result type (might be due to invalid IL or missing references)
			//IL_05cd: Expected O, but got Unknown
			//IL_05ce: Unknown result type (might be due to invalid IL or missing references)
			//IL_05d8: Expected O, but got Unknown
			//IL_4fdb: Unknown result type (might be due to invalid IL or missing references)
			//IL_4fe5: Expected O, but got Unknown
			//IL_5558: Unknown result type (might be due to invalid IL or missing references)
			//IL_5562: Expected O, but got Unknown
			components = new Container();
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(CloudMap));
			picCloud = new PictureBox();
			groupBox1 = new GroupBox();
			optAsia = new RadioButton();
			optEurope = new RadioButton();
			optNthAmerica = new RadioButton();
			optSthAmerica = new RadioButton();
			optAfrica = new RadioButton();
			optAustralasia = new RadioButton();
			optWorld = new RadioButton();
			groupBox2 = new GroupBox();
			optWind = new RadioButton();
			optTemperature = new RadioButton();
			optExtinction = new RadioButton();
			optTotalCloud = new RadioButton();
			optLowCloud = new RadioButton();
			optHighCloud = new RadioButton();
			optMiddleCloud = new RadioButton();
			grpDateTime = new GroupBox();
			opt189 = new RadioButton();
			opt183 = new RadioButton();
			opt186 = new RadioButton();
			lblDate16 = new Label();
			lblDate15 = new Label();
			opt348 = new RadioButton();
			opt360 = new RadioButton();
			lblDate11 = new Label();
			opt372 = new RadioButton();
			opt336 = new RadioButton();
			lblDate14 = new Label();
			lblDate13 = new Label();
			lblDate12 = new Label();
			opt276 = new RadioButton();
			opt288 = new RadioButton();
			opt300 = new RadioButton();
			opt312 = new RadioButton();
			opt324 = new RadioButton();
			opt264 = new RadioButton();
			lblDate10 = new Label();
			lblDate9 = new Label();
			lblDate8 = new Label();
			opt204 = new RadioButton();
			opt216 = new RadioButton();
			opt228 = new RadioButton();
			opt240 = new RadioButton();
			opt252 = new RadioButton();
			opt192 = new RadioButton();
			lblDate7 = new Label();
			opt171 = new RadioButton();
			opt174 = new RadioButton();
			opt177 = new RadioButton();
			opt180 = new RadioButton();
			opt168 = new RadioButton();
			lblDate6 = new Label();
			opt147 = new RadioButton();
			opt150 = new RadioButton();
			opt153 = new RadioButton();
			opt156 = new RadioButton();
			opt159 = new RadioButton();
			opt162 = new RadioButton();
			opt165 = new RadioButton();
			opt144 = new RadioButton();
			lblDate5 = new Label();
			opt123 = new RadioButton();
			opt126 = new RadioButton();
			opt129 = new RadioButton();
			opt132 = new RadioButton();
			opt135 = new RadioButton();
			opt138 = new RadioButton();
			opt141 = new RadioButton();
			opt120 = new RadioButton();
			opt99 = new RadioButton();
			opt102 = new RadioButton();
			opt105 = new RadioButton();
			opt108 = new RadioButton();
			opt111 = new RadioButton();
			opt114 = new RadioButton();
			opt117 = new RadioButton();
			opt96 = new RadioButton();
			lblDate4 = new Label();
			opt75 = new RadioButton();
			opt78 = new RadioButton();
			opt81 = new RadioButton();
			opt84 = new RadioButton();
			opt87 = new RadioButton();
			opt90 = new RadioButton();
			opt93 = new RadioButton();
			opt72 = new RadioButton();
			lblDate3 = new Label();
			opt51 = new RadioButton();
			opt54 = new RadioButton();
			opt57 = new RadioButton();
			opt60 = new RadioButton();
			opt63 = new RadioButton();
			opt66 = new RadioButton();
			opt69 = new RadioButton();
			opt48 = new RadioButton();
			lblDate2 = new Label();
			opt27 = new RadioButton();
			opt30 = new RadioButton();
			opt33 = new RadioButton();
			opt36 = new RadioButton();
			opt39 = new RadioButton();
			opt42 = new RadioButton();
			opt45 = new RadioButton();
			opt24 = new RadioButton();
			lblDate1 = new Label();
			opt3 = new RadioButton();
			opt6 = new RadioButton();
			opt9 = new RadioButton();
			opt12 = new RadioButton();
			opt15 = new RadioButton();
			opt18 = new RadioButton();
			opt21 = new RadioButton();
			opt0 = new RadioButton();
			menuStrip1 = new MenuStrip();
			withMapToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			saveAnimationAsAnimatedGIFToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			picCloud1 = new PictureBox();
			picCloud_1 = new PictureBox();
			picCloud_2 = new PictureBox();
			picCloud_3 = new PictureBox();
			timer1 = new Timer(components);
			TrackSpeed = new TrackBar();
			chkAnimated = new CheckBox();
			Panel = new Panel();
			picCloudDisplay = new PictureBox();
			label1 = new Label();
			label2 = new Label();
			label3 = new Label();
			((ISupportInitialize)picCloud).BeginInit();
			((Control)groupBox1).SuspendLayout();
			((Control)groupBox2).SuspendLayout();
			((Control)grpDateTime).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((ISupportInitialize)picCloud1).BeginInit();
			((ISupportInitialize)picCloud_1).BeginInit();
			((ISupportInitialize)picCloud_2).BeginInit();
			((ISupportInitialize)picCloud_3).BeginInit();
			((ISupportInitialize)TrackSpeed).BeginInit();
			((Control)Panel).SuspendLayout();
			((ISupportInitialize)picCloudDisplay).BeginInit();
			((Control)this).SuspendLayout();
			((Control)picCloud).set_BackColor(Color.Black);
			((Control)picCloud).set_Location(new Point(9, 193));
			((Control)picCloud).set_Name("picCloud");
			((Control)picCloud).set_Size(new Size(810, 602));
			picCloud.set_TabIndex(1);
			picCloud.set_TabStop(false);
			((Control)picCloud).set_Visible(false);
			((Control)groupBox1).get_Controls().Add((Control)(object)optAsia);
			((Control)groupBox1).get_Controls().Add((Control)(object)optEurope);
			((Control)groupBox1).get_Controls().Add((Control)(object)optNthAmerica);
			((Control)groupBox1).get_Controls().Add((Control)(object)optSthAmerica);
			((Control)groupBox1).get_Controls().Add((Control)(object)optAfrica);
			((Control)groupBox1).get_Controls().Add((Control)(object)optAustralasia);
			((Control)groupBox1).get_Controls().Add((Control)(object)optWorld);
			((Control)groupBox1).set_Location(new Point(9, 3));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(104, 155));
			((Control)groupBox1).set_TabIndex(2);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Region");
			((Control)optAsia).set_AutoSize(true);
			((Control)optAsia).set_Location(new Point(8, 38));
			((Control)optAsia).set_Name("optAsia");
			((Control)optAsia).set_Size(new Size(45, 17));
			((Control)optAsia).set_TabIndex(6);
			((Control)optAsia).set_Text("Asia");
			((ButtonBase)optAsia).set_UseVisualStyleBackColor(true);
			optAsia.add_CheckedChanged((EventHandler)optAsia_CheckedChanged);
			((Control)optEurope).set_AutoSize(true);
			((Control)optEurope).set_Location(new Point(8, 76));
			((Control)optEurope).set_Name("optEurope");
			((Control)optEurope).set_Size(new Size(59, 17));
			((Control)optEurope).set_TabIndex(5);
			((Control)optEurope).set_Text("Europe");
			((ButtonBase)optEurope).set_UseVisualStyleBackColor(true);
			optEurope.add_CheckedChanged((EventHandler)optEurope_CheckedChanged);
			((Control)optNthAmerica).set_AutoSize(true);
			((Control)optNthAmerica).set_Location(new Point(8, 95));
			((Control)optNthAmerica).set_Name("optNthAmerica");
			((Control)optNthAmerica).set_Size(new Size(92, 17));
			((Control)optNthAmerica).set_TabIndex(4);
			((Control)optNthAmerica).set_Text("North America");
			((ButtonBase)optNthAmerica).set_UseVisualStyleBackColor(true);
			optNthAmerica.add_CheckedChanged((EventHandler)optNthAmerica_CheckedChanged);
			((Control)optSthAmerica).set_AutoSize(true);
			((Control)optSthAmerica).set_Location(new Point(8, 114));
			((Control)optSthAmerica).set_Name("optSthAmerica");
			((Control)optSthAmerica).set_Size(new Size(94, 17));
			((Control)optSthAmerica).set_TabIndex(3);
			((Control)optSthAmerica).set_Text("South America");
			((ButtonBase)optSthAmerica).set_UseVisualStyleBackColor(true);
			optSthAmerica.add_CheckedChanged((EventHandler)optSthAmerica_CheckedChanged);
			((Control)optAfrica).set_AutoSize(true);
			((Control)optAfrica).set_Location(new Point(8, 19));
			((Control)optAfrica).set_Name("optAfrica");
			((Control)optAfrica).set_Size(new Size(52, 17));
			((Control)optAfrica).set_TabIndex(2);
			((Control)optAfrica).set_Text("Africa");
			((ButtonBase)optAfrica).set_UseVisualStyleBackColor(true);
			optAfrica.add_CheckedChanged((EventHandler)optAfrica_CheckedChanged);
			((Control)optAustralasia).set_AutoSize(true);
			((Control)optAustralasia).set_Location(new Point(8, 57));
			((Control)optAustralasia).set_Name("optAustralasia");
			((Control)optAustralasia).set_Size(new Size(76, 17));
			((Control)optAustralasia).set_TabIndex(1);
			((Control)optAustralasia).set_Text("Australasia");
			((ButtonBase)optAustralasia).set_UseVisualStyleBackColor(true);
			optAustralasia.add_CheckedChanged((EventHandler)optAustralasia_CheckedChanged);
			((Control)optWorld).set_AutoSize(true);
			optWorld.set_Checked(true);
			((Control)optWorld).set_Location(new Point(8, 133));
			((Control)optWorld).set_Name("optWorld");
			((Control)optWorld).set_Size(new Size(53, 17));
			((Control)optWorld).set_TabIndex(0);
			optWorld.set_TabStop(true);
			((Control)optWorld).set_Text("World");
			((ButtonBase)optWorld).set_UseVisualStyleBackColor(true);
			optWorld.add_CheckedChanged((EventHandler)optWorld_CheckedChanged);
			((Control)groupBox2).get_Controls().Add((Control)(object)optWind);
			((Control)groupBox2).get_Controls().Add((Control)(object)optTemperature);
			((Control)groupBox2).get_Controls().Add((Control)(object)optExtinction);
			((Control)groupBox2).get_Controls().Add((Control)(object)optTotalCloud);
			((Control)groupBox2).get_Controls().Add((Control)(object)optLowCloud);
			((Control)groupBox2).get_Controls().Add((Control)(object)optHighCloud);
			((Control)groupBox2).get_Controls().Add((Control)(object)optMiddleCloud);
			((Control)groupBox2).set_Location(new Point(123, 3));
			((Control)groupBox2).set_Name("groupBox2");
			((Control)groupBox2).set_Size(new Size(97, 155));
			((Control)groupBox2).set_TabIndex(3);
			groupBox2.set_TabStop(false);
			((Control)groupBox2).set_Text("Map type");
			((Control)optWind).set_AutoSize(true);
			((Control)optWind).set_Location(new Point(8, 133));
			((Control)optWind).set_Name("optWind");
			((Control)optWind).set_Size(new Size(50, 17));
			((Control)optWind).set_TabIndex(7);
			((Control)optWind).set_Text("Wind");
			((ButtonBase)optWind).set_UseVisualStyleBackColor(true);
			((Control)optTemperature).set_AutoSize(true);
			((Control)optTemperature).set_Location(new Point(8, 114));
			((Control)optTemperature).set_Name("optTemperature");
			((Control)optTemperature).set_Size(new Size(85, 17));
			((Control)optTemperature).set_TabIndex(6);
			((Control)optTemperature).set_Text("Temperature");
			((ButtonBase)optTemperature).set_UseVisualStyleBackColor(true);
			optTemperature.add_CheckedChanged((EventHandler)optTemperature_CheckedChanged);
			((Control)optExtinction).set_AutoSize(true);
			((Control)optExtinction).set_Location(new Point(8, 95));
			((Control)optExtinction).set_Name("optExtinction");
			((Control)optExtinction).set_Size(new Size(71, 17));
			((Control)optExtinction).set_TabIndex(5);
			((Control)optExtinction).set_Text("Extinction");
			((ButtonBase)optExtinction).set_UseVisualStyleBackColor(true);
			optExtinction.add_CheckedChanged((EventHandler)optExtinction_CheckedChanged);
			((Control)optTotalCloud).set_AutoSize(true);
			optTotalCloud.set_Checked(true);
			((Control)optTotalCloud).set_Location(new Point(8, 19));
			((Control)optTotalCloud).set_Name("optTotalCloud");
			((Control)optTotalCloud).set_Size(new Size(78, 17));
			((Control)optTotalCloud).set_TabIndex(4);
			optTotalCloud.set_TabStop(true);
			((Control)optTotalCloud).set_Text("Total cloud");
			((ButtonBase)optTotalCloud).set_UseVisualStyleBackColor(true);
			optTotalCloud.add_CheckedChanged((EventHandler)optTotalCloud_CheckedChanged);
			((Control)optLowCloud).set_AutoSize(true);
			((Control)optLowCloud).set_Location(new Point(8, 76));
			((Control)optLowCloud).set_Name("optLowCloud");
			((Control)optLowCloud).set_Size(new Size(74, 17));
			((Control)optLowCloud).set_TabIndex(3);
			((Control)optLowCloud).set_Text("Low cloud");
			((ButtonBase)optLowCloud).set_UseVisualStyleBackColor(true);
			optLowCloud.add_CheckedChanged((EventHandler)optLowCloud_CheckedChanged);
			((Control)optHighCloud).set_AutoSize(true);
			((Control)optHighCloud).set_Location(new Point(8, 38));
			((Control)optHighCloud).set_Name("optHighCloud");
			((Control)optHighCloud).set_Size(new Size(76, 17));
			((Control)optHighCloud).set_TabIndex(2);
			((Control)optHighCloud).set_Text("High cloud");
			((ButtonBase)optHighCloud).set_UseVisualStyleBackColor(true);
			optHighCloud.add_CheckedChanged((EventHandler)optHighCloud_CheckedChanged);
			((Control)optMiddleCloud).set_AutoSize(true);
			((Control)optMiddleCloud).set_Location(new Point(8, 57));
			((Control)optMiddleCloud).set_Name("optMiddleCloud");
			((Control)optMiddleCloud).set_Size(new Size(85, 17));
			((Control)optMiddleCloud).set_TabIndex(1);
			((Control)optMiddleCloud).set_Text("Middle cloud");
			((ButtonBase)optMiddleCloud).set_UseVisualStyleBackColor(true);
			optMiddleCloud.add_CheckedChanged((EventHandler)optMiddleCloud_CheckedChanged);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt189);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt183);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt186);
			((Control)grpDateTime).get_Controls().Add((Control)(object)lblDate16);
			((Control)grpDateTime).get_Controls().Add((Control)(object)lblDate15);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt348);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt360);
			((Control)grpDateTime).get_Controls().Add((Control)(object)lblDate11);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt372);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt336);
			((Control)grpDateTime).get_Controls().Add((Control)(object)lblDate14);
			((Control)grpDateTime).get_Controls().Add((Control)(object)lblDate13);
			((Control)grpDateTime).get_Controls().Add((Control)(object)lblDate12);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt276);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt288);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt300);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt312);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt324);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt264);
			((Control)grpDateTime).get_Controls().Add((Control)(object)lblDate10);
			((Control)grpDateTime).get_Controls().Add((Control)(object)lblDate9);
			((Control)grpDateTime).get_Controls().Add((Control)(object)lblDate8);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt204);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt216);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt228);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt240);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt252);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt192);
			((Control)grpDateTime).get_Controls().Add((Control)(object)lblDate7);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt171);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt174);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt177);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt180);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt168);
			((Control)grpDateTime).get_Controls().Add((Control)(object)lblDate6);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt147);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt150);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt153);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt156);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt159);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt162);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt165);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt144);
			((Control)grpDateTime).get_Controls().Add((Control)(object)lblDate5);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt123);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt126);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt129);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt132);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt135);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt138);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt141);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt120);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt99);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt102);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt105);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt108);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt111);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt114);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt117);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt96);
			((Control)grpDateTime).get_Controls().Add((Control)(object)lblDate4);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt75);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt78);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt81);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt84);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt87);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt90);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt93);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt72);
			((Control)grpDateTime).get_Controls().Add((Control)(object)lblDate3);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt51);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt54);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt57);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt60);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt63);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt66);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt69);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt48);
			((Control)grpDateTime).get_Controls().Add((Control)(object)lblDate2);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt27);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt30);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt33);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt36);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt39);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt42);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt45);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt24);
			((Control)grpDateTime).get_Controls().Add((Control)(object)lblDate1);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt3);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt6);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt9);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt12);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt15);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt18);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt21);
			((Control)grpDateTime).get_Controls().Add((Control)(object)opt0);
			((Control)grpDateTime).set_Location(new Point(230, 3));
			((Control)grpDateTime).set_Name("grpDateTime");
			((Control)grpDateTime).set_Size(new Size(594, 179));
			((Control)grpDateTime).set_TabIndex(4);
			grpDateTime.set_TabStop(false);
			((Control)grpDateTime).set_Text("Date and time  [ UTC ]");
			((Control)opt189).set_AutoSize(true);
			((Control)opt189).set_Location(new Point(373, 151));
			((Control)opt189).set_Name("opt189");
			((Control)opt189).set_Size(new Size(49, 17));
			((Control)opt189).set_TabIndex(102);
			((Control)opt189).set_Text("21 hr");
			((ButtonBase)opt189).set_UseVisualStyleBackColor(true);
			opt189.add_CheckedChanged((EventHandler)opt189_CheckedChanged);
			((Control)opt183).set_AutoSize(true);
			((Control)opt183).set_Location(new Point(373, 118));
			((Control)opt183).set_Name("opt183");
			((Control)opt183).set_Size(new Size(49, 17));
			((Control)opt183).set_TabIndex(101);
			((Control)opt183).set_Text("15 hr");
			((ButtonBase)opt183).set_UseVisualStyleBackColor(true);
			opt183.add_CheckedChanged((EventHandler)opt183_CheckedChanged);
			((Control)opt186).set_AutoSize(true);
			((Control)opt186).set_Location(new Point(373, 135));
			((Control)opt186).set_Name("opt186");
			((Control)opt186).set_Size(new Size(49, 17));
			((Control)opt186).set_TabIndex(100);
			((Control)opt186).set_Text("18 hr");
			((ButtonBase)opt186).set_UseVisualStyleBackColor(true);
			opt186.add_CheckedChanged((EventHandler)opt186_CheckedChanged);
			((Control)lblDate16).set_AutoSize(true);
			((Control)lblDate16).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblDate16).set_Location(new Point(539, 71));
			((Control)lblDate16).set_Name("lblDate16");
			((Control)lblDate16).set_Size(new Size(48, 13));
			((Control)lblDate16).set_TabIndex(99);
			((Control)lblDate16).set_Text("Date16");
			((Control)lblDate15).set_AutoSize(true);
			((Control)lblDate15).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblDate15).set_Location(new Point(539, 20));
			((Control)lblDate15).set_Name("lblDate15");
			((Control)lblDate15).set_Size(new Size(48, 13));
			((Control)lblDate15).set_TabIndex(98);
			((Control)lblDate15).set_Text("Date15");
			((Control)opt348).set_AutoSize(true);
			((Control)opt348).set_Location(new Point(542, 50));
			((Control)opt348).set_Name("opt348");
			((Control)opt348).set_Size(new Size(49, 17));
			((Control)opt348).set_TabIndex(97);
			((Control)opt348).set_Text("12 hr");
			((ButtonBase)opt348).set_UseVisualStyleBackColor(true);
			opt348.add_CheckedChanged((EventHandler)opt348_CheckedChanged);
			((Control)opt360).set_AutoSize(true);
			((Control)opt360).set_Location(new Point(542, 84));
			((Control)opt360).set_Name("opt360");
			((Control)opt360).set_Size(new Size(43, 17));
			((Control)opt360).set_TabIndex(96);
			((Control)opt360).set_Text("0 hr");
			((ButtonBase)opt360).set_UseVisualStyleBackColor(true);
			opt360.add_CheckedChanged((EventHandler)opt360_CheckedChanged);
			((Control)lblDate11).set_AutoSize(true);
			((Control)lblDate11).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblDate11).set_Location(new Point(430, 122));
			((Control)lblDate11).set_Name("lblDate11");
			((Control)lblDate11).set_Size(new Size(48, 13));
			((Control)lblDate11).set_TabIndex(44);
			((Control)lblDate11).set_Text("Date11");
			((Control)opt372).set_AutoSize(true);
			((Control)opt372).set_Location(new Point(542, 101));
			((Control)opt372).set_Name("opt372");
			((Control)opt372).set_Size(new Size(49, 17));
			((Control)opt372).set_TabIndex(95);
			((Control)opt372).set_Text("12 hr");
			((ButtonBase)opt372).set_UseVisualStyleBackColor(true);
			opt372.add_CheckedChanged((EventHandler)opt372_CheckedChanged);
			((Control)opt336).set_AutoSize(true);
			((Control)opt336).set_Location(new Point(542, 33));
			((Control)opt336).set_Name("opt336");
			((Control)opt336).set_Size(new Size(43, 17));
			((Control)opt336).set_TabIndex(92);
			((Control)opt336).set_Text("0 hr");
			((ButtonBase)opt336).set_UseVisualStyleBackColor(true);
			opt336.add_CheckedChanged((EventHandler)opt336_CheckedChanged);
			((Control)lblDate14).set_AutoSize(true);
			((Control)lblDate14).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblDate14).set_Location(new Point(487, 122));
			((Control)lblDate14).set_Name("lblDate14");
			((Control)lblDate14).set_Size(new Size(48, 13));
			((Control)lblDate14).set_TabIndex(91);
			((Control)lblDate14).set_Text("Date14");
			((Control)lblDate13).set_AutoSize(true);
			((Control)lblDate13).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblDate13).set_Location(new Point(487, 71));
			((Control)lblDate13).set_Name("lblDate13");
			((Control)lblDate13).set_Size(new Size(48, 13));
			((Control)lblDate13).set_TabIndex(90);
			((Control)lblDate13).set_Text("Date13");
			((Control)lblDate12).set_AutoSize(true);
			((Control)lblDate12).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblDate12).set_Location(new Point(487, 20));
			((Control)lblDate12).set_Name("lblDate12");
			((Control)lblDate12).set_Size(new Size(48, 13));
			((Control)lblDate12).set_TabIndex(89);
			((Control)lblDate12).set_Text("Date12");
			((Control)opt276).set_AutoSize(true);
			((Control)opt276).set_Location(new Point(490, 50));
			((Control)opt276).set_Name("opt276");
			((Control)opt276).set_Size(new Size(49, 17));
			((Control)opt276).set_TabIndex(88);
			((Control)opt276).set_Text("12 hr");
			((ButtonBase)opt276).set_UseVisualStyleBackColor(true);
			opt276.add_CheckedChanged((EventHandler)opt276_CheckedChanged);
			((Control)opt288).set_AutoSize(true);
			((Control)opt288).set_Location(new Point(490, 84));
			((Control)opt288).set_Name("opt288");
			((Control)opt288).set_Size(new Size(43, 17));
			((Control)opt288).set_TabIndex(87);
			((Control)opt288).set_Text("0 hr");
			((ButtonBase)opt288).set_UseVisualStyleBackColor(true);
			opt288.add_CheckedChanged((EventHandler)opt288_CheckedChanged);
			((Control)opt300).set_AutoSize(true);
			((Control)opt300).set_Location(new Point(490, 101));
			((Control)opt300).set_Name("opt300");
			((Control)opt300).set_Size(new Size(49, 17));
			((Control)opt300).set_TabIndex(86);
			((Control)opt300).set_Text("12 hr");
			((ButtonBase)opt300).set_UseVisualStyleBackColor(true);
			opt300.add_CheckedChanged((EventHandler)opt300_CheckedChanged);
			((Control)opt312).set_AutoSize(true);
			((Control)opt312).set_Location(new Point(490, 135));
			((Control)opt312).set_Name("opt312");
			((Control)opt312).set_Size(new Size(43, 17));
			((Control)opt312).set_TabIndex(85);
			((Control)opt312).set_Text("0 hr");
			((ButtonBase)opt312).set_UseVisualStyleBackColor(true);
			opt312.add_CheckedChanged((EventHandler)opt312_CheckedChanged);
			((Control)opt324).set_AutoSize(true);
			((Control)opt324).set_Location(new Point(490, 152));
			((Control)opt324).set_Name("opt324");
			((Control)opt324).set_Size(new Size(49, 17));
			((Control)opt324).set_TabIndex(84);
			((Control)opt324).set_Text("12 hr");
			((ButtonBase)opt324).set_UseVisualStyleBackColor(true);
			opt324.add_CheckedChanged((EventHandler)opt324_CheckedChanged);
			((Control)opt264).set_AutoSize(true);
			((Control)opt264).set_Location(new Point(490, 33));
			((Control)opt264).set_Name("opt264");
			((Control)opt264).set_Size(new Size(43, 17));
			((Control)opt264).set_TabIndex(83);
			((Control)opt264).set_Text("0 hr");
			((ButtonBase)opt264).set_UseVisualStyleBackColor(true);
			opt264.add_CheckedChanged((EventHandler)opt264_CheckedChanged);
			((Control)lblDate10).set_AutoSize(true);
			((Control)lblDate10).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblDate10).set_Location(new Point(430, 71));
			((Control)lblDate10).set_Name("lblDate10");
			((Control)lblDate10).set_Size(new Size(48, 13));
			((Control)lblDate10).set_TabIndex(82);
			((Control)lblDate10).set_Text("Date10");
			((Control)lblDate9).set_AutoSize(true);
			((Control)lblDate9).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblDate9).set_Location(new Point(430, 20));
			((Control)lblDate9).set_Name("lblDate9");
			((Control)lblDate9).set_Size(new Size(41, 13));
			((Control)lblDate9).set_TabIndex(81);
			((Control)lblDate9).set_Text("Date9");
			((Control)lblDate8).set_AutoSize(true);
			((Control)lblDate8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblDate8).set_Location(new Point(370, 20));
			((Control)lblDate8).set_Name("lblDate8");
			((Control)lblDate8).set_Size(new Size(41, 13));
			((Control)lblDate8).set_TabIndex(80);
			((Control)lblDate8).set_Text("Date8");
			((Control)opt204).set_AutoSize(true);
			((Control)opt204).set_Location(new Point(438, 50));
			((Control)opt204).set_Name("opt204");
			((Control)opt204).set_Size(new Size(49, 17));
			((Control)opt204).set_TabIndex(79);
			((Control)opt204).set_Text("12 hr");
			((ButtonBase)opt204).set_UseVisualStyleBackColor(true);
			opt204.add_CheckedChanged((EventHandler)opt204_CheckedChanged);
			((Control)opt216).set_AutoSize(true);
			((Control)opt216).set_Location(new Point(438, 84));
			((Control)opt216).set_Name("opt216");
			((Control)opt216).set_Size(new Size(43, 17));
			((Control)opt216).set_TabIndex(78);
			((Control)opt216).set_Text("0 hr");
			((ButtonBase)opt216).set_UseVisualStyleBackColor(true);
			opt216.add_CheckedChanged((EventHandler)opt216_CheckedChanged);
			((Control)opt228).set_AutoSize(true);
			((Control)opt228).set_Location(new Point(438, 101));
			((Control)opt228).set_Name("opt228");
			((Control)opt228).set_Size(new Size(49, 17));
			((Control)opt228).set_TabIndex(77);
			((Control)opt228).set_Text("12 hr");
			((ButtonBase)opt228).set_UseVisualStyleBackColor(true);
			opt228.add_CheckedChanged((EventHandler)opt228_CheckedChanged);
			((Control)opt240).set_AutoSize(true);
			((Control)opt240).set_Location(new Point(438, 135));
			((Control)opt240).set_Name("opt240");
			((Control)opt240).set_Size(new Size(43, 17));
			((Control)opt240).set_TabIndex(76);
			((Control)opt240).set_Text("0 hr");
			((ButtonBase)opt240).set_UseVisualStyleBackColor(true);
			opt240.add_CheckedChanged((EventHandler)opt240_CheckedChanged);
			((Control)opt252).set_AutoSize(true);
			((Control)opt252).set_Location(new Point(438, 152));
			((Control)opt252).set_Name("opt252");
			((Control)opt252).set_Size(new Size(49, 17));
			((Control)opt252).set_TabIndex(75);
			((Control)opt252).set_Text("12 hr");
			((ButtonBase)opt252).set_UseVisualStyleBackColor(true);
			opt252.add_CheckedChanged((EventHandler)opt252_CheckedChanged);
			((Control)opt192).set_AutoSize(true);
			((Control)opt192).set_Location(new Point(438, 33));
			((Control)opt192).set_Name("opt192");
			((Control)opt192).set_Size(new Size(43, 17));
			((Control)opt192).set_TabIndex(72);
			((Control)opt192).set_Text("0 hr");
			((ButtonBase)opt192).set_UseVisualStyleBackColor(true);
			opt192.add_CheckedChanged((EventHandler)opt192_CheckedChanged);
			((Control)lblDate7).set_AutoSize(true);
			((Control)lblDate7).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblDate7).set_Location(new Point(318, 20));
			((Control)lblDate7).set_Name("lblDate7");
			((Control)lblDate7).set_Size(new Size(41, 13));
			((Control)lblDate7).set_TabIndex(71);
			((Control)lblDate7).set_Text("Date7");
			((Control)opt171).set_AutoSize(true);
			((Control)opt171).set_Location(new Point(373, 50));
			((Control)opt171).set_Name("opt171");
			((Control)opt171).set_Size(new Size(43, 17));
			((Control)opt171).set_TabIndex(70);
			((Control)opt171).set_Text("3 hr");
			((ButtonBase)opt171).set_UseVisualStyleBackColor(true);
			opt171.add_CheckedChanged((EventHandler)opt171_CheckedChanged);
			((Control)opt174).set_AutoSize(true);
			((Control)opt174).set_Location(new Point(373, 67));
			((Control)opt174).set_Name("opt174");
			((Control)opt174).set_Size(new Size(43, 17));
			((Control)opt174).set_TabIndex(69);
			((Control)opt174).set_Text("6 hr");
			((ButtonBase)opt174).set_UseVisualStyleBackColor(true);
			opt174.add_CheckedChanged((EventHandler)opt174_CheckedChanged);
			((Control)opt177).set_AutoSize(true);
			((Control)opt177).set_Location(new Point(373, 84));
			((Control)opt177).set_Name("opt177");
			((Control)opt177).set_Size(new Size(43, 17));
			((Control)opt177).set_TabIndex(68);
			((Control)opt177).set_Text("9 hr");
			((ButtonBase)opt177).set_UseVisualStyleBackColor(true);
			opt177.add_CheckedChanged((EventHandler)opt177_CheckedChanged);
			((Control)opt180).set_AutoSize(true);
			((Control)opt180).set_Location(new Point(373, 101));
			((Control)opt180).set_Name("opt180");
			((Control)opt180).set_Size(new Size(49, 17));
			((Control)opt180).set_TabIndex(67);
			((Control)opt180).set_Text("12 hr");
			((ButtonBase)opt180).set_UseVisualStyleBackColor(true);
			opt180.add_CheckedChanged((EventHandler)opt180_CheckedChanged);
			((Control)opt168).set_AutoSize(true);
			((Control)opt168).set_Location(new Point(373, 33));
			((Control)opt168).set_Name("opt168");
			((Control)opt168).set_Size(new Size(43, 17));
			((Control)opt168).set_TabIndex(63);
			((Control)opt168).set_Text("0 hr");
			((ButtonBase)opt168).set_UseVisualStyleBackColor(true);
			opt168.add_CheckedChanged((EventHandler)opt168_CheckedChanged);
			((Control)lblDate6).set_AutoSize(true);
			((Control)lblDate6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblDate6).set_Location(new Point(266, 20));
			((Control)lblDate6).set_Name("lblDate6");
			((Control)lblDate6).set_Size(new Size(41, 13));
			((Control)lblDate6).set_TabIndex(62);
			((Control)lblDate6).set_Text("Date6");
			((Control)opt147).set_AutoSize(true);
			((Control)opt147).set_Location(new Point(321, 50));
			((Control)opt147).set_Name("opt147");
			((Control)opt147).set_Size(new Size(43, 17));
			((Control)opt147).set_TabIndex(61);
			((Control)opt147).set_Text("3 hr");
			((ButtonBase)opt147).set_UseVisualStyleBackColor(true);
			opt147.add_CheckedChanged((EventHandler)opt147_CheckedChanged);
			((Control)opt150).set_AutoSize(true);
			((Control)opt150).set_Location(new Point(321, 67));
			((Control)opt150).set_Name("opt150");
			((Control)opt150).set_Size(new Size(43, 17));
			((Control)opt150).set_TabIndex(60);
			((Control)opt150).set_Text("6 hr");
			((ButtonBase)opt150).set_UseVisualStyleBackColor(true);
			opt150.add_CheckedChanged((EventHandler)opt150_CheckedChanged);
			((Control)opt153).set_AutoSize(true);
			((Control)opt153).set_Location(new Point(321, 84));
			((Control)opt153).set_Name("opt153");
			((Control)opt153).set_Size(new Size(43, 17));
			((Control)opt153).set_TabIndex(59);
			((Control)opt153).set_Text("9 hr");
			((ButtonBase)opt153).set_UseVisualStyleBackColor(true);
			opt153.add_CheckedChanged((EventHandler)opt153_CheckedChanged);
			((Control)opt156).set_AutoSize(true);
			((Control)opt156).set_Location(new Point(321, 101));
			((Control)opt156).set_Name("opt156");
			((Control)opt156).set_Size(new Size(49, 17));
			((Control)opt156).set_TabIndex(58);
			((Control)opt156).set_Text("12 hr");
			((ButtonBase)opt156).set_UseVisualStyleBackColor(true);
			opt156.add_CheckedChanged((EventHandler)opt156_CheckedChanged);
			((Control)opt159).set_AutoSize(true);
			((Control)opt159).set_Location(new Point(321, 118));
			((Control)opt159).set_Name("opt159");
			((Control)opt159).set_Size(new Size(49, 17));
			((Control)opt159).set_TabIndex(57);
			((Control)opt159).set_Text("15 hr");
			((ButtonBase)opt159).set_UseVisualStyleBackColor(true);
			opt159.add_CheckedChanged((EventHandler)opt159_CheckedChanged);
			((Control)opt162).set_AutoSize(true);
			((Control)opt162).set_Location(new Point(321, 135));
			((Control)opt162).set_Name("opt162");
			((Control)opt162).set_Size(new Size(49, 17));
			((Control)opt162).set_TabIndex(56);
			((Control)opt162).set_Text("18 hr");
			((ButtonBase)opt162).set_UseVisualStyleBackColor(true);
			opt162.add_CheckedChanged((EventHandler)opt162_CheckedChanged);
			((Control)opt165).set_AutoSize(true);
			((Control)opt165).set_Location(new Point(321, 152));
			((Control)opt165).set_Name("opt165");
			((Control)opt165).set_Size(new Size(49, 17));
			((Control)opt165).set_TabIndex(55);
			((Control)opt165).set_Text("21 hr");
			((ButtonBase)opt165).set_UseVisualStyleBackColor(true);
			opt165.add_CheckedChanged((EventHandler)opt165_CheckedChanged);
			((Control)opt144).set_AutoSize(true);
			((Control)opt144).set_Location(new Point(321, 33));
			((Control)opt144).set_Name("opt144");
			((Control)opt144).set_Size(new Size(43, 17));
			((Control)opt144).set_TabIndex(54);
			((Control)opt144).set_Text("0 hr");
			((ButtonBase)opt144).set_UseVisualStyleBackColor(true);
			opt144.add_CheckedChanged((EventHandler)opt144_CheckedChanged);
			((Control)lblDate5).set_AutoSize(true);
			((Control)lblDate5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblDate5).set_Location(new Point(214, 20));
			((Control)lblDate5).set_Name("lblDate5");
			((Control)lblDate5).set_Size(new Size(41, 13));
			((Control)lblDate5).set_TabIndex(53);
			((Control)lblDate5).set_Text("Date5");
			((Control)opt123).set_AutoSize(true);
			((Control)opt123).set_Location(new Point(269, 50));
			((Control)opt123).set_Name("opt123");
			((Control)opt123).set_Size(new Size(43, 17));
			((Control)opt123).set_TabIndex(52);
			((Control)opt123).set_Text("3 hr");
			((ButtonBase)opt123).set_UseVisualStyleBackColor(true);
			opt123.add_CheckedChanged((EventHandler)opt123_CheckedChanged);
			((Control)opt126).set_AutoSize(true);
			((Control)opt126).set_Location(new Point(269, 67));
			((Control)opt126).set_Name("opt126");
			((Control)opt126).set_Size(new Size(43, 17));
			((Control)opt126).set_TabIndex(51);
			((Control)opt126).set_Text("6 hr");
			((ButtonBase)opt126).set_UseVisualStyleBackColor(true);
			opt126.add_CheckedChanged((EventHandler)opt126_CheckedChanged);
			((Control)opt129).set_AutoSize(true);
			((Control)opt129).set_Location(new Point(269, 84));
			((Control)opt129).set_Name("opt129");
			((Control)opt129).set_Size(new Size(43, 17));
			((Control)opt129).set_TabIndex(50);
			((Control)opt129).set_Text("9 hr");
			((ButtonBase)opt129).set_UseVisualStyleBackColor(true);
			opt129.add_CheckedChanged((EventHandler)opt129_CheckedChanged);
			((Control)opt132).set_AutoSize(true);
			((Control)opt132).set_Location(new Point(269, 101));
			((Control)opt132).set_Name("opt132");
			((Control)opt132).set_Size(new Size(49, 17));
			((Control)opt132).set_TabIndex(49);
			((Control)opt132).set_Text("12 hr");
			((ButtonBase)opt132).set_UseVisualStyleBackColor(true);
			opt132.add_CheckedChanged((EventHandler)opt132_CheckedChanged);
			((Control)opt135).set_AutoSize(true);
			((Control)opt135).set_Location(new Point(269, 118));
			((Control)opt135).set_Name("opt135");
			((Control)opt135).set_Size(new Size(49, 17));
			((Control)opt135).set_TabIndex(48);
			((Control)opt135).set_Text("15 hr");
			((ButtonBase)opt135).set_UseVisualStyleBackColor(true);
			opt135.add_CheckedChanged((EventHandler)opt135_CheckedChanged);
			((Control)opt138).set_AutoSize(true);
			((Control)opt138).set_Location(new Point(269, 135));
			((Control)opt138).set_Name("opt138");
			((Control)opt138).set_Size(new Size(49, 17));
			((Control)opt138).set_TabIndex(47);
			((Control)opt138).set_Text("18 hr");
			((ButtonBase)opt138).set_UseVisualStyleBackColor(true);
			opt138.add_CheckedChanged((EventHandler)opt138_CheckedChanged);
			((Control)opt141).set_AutoSize(true);
			((Control)opt141).set_Location(new Point(269, 152));
			((Control)opt141).set_Name("opt141");
			((Control)opt141).set_Size(new Size(49, 17));
			((Control)opt141).set_TabIndex(46);
			((Control)opt141).set_Text("21 hr");
			((ButtonBase)opt141).set_UseVisualStyleBackColor(true);
			opt141.add_CheckedChanged((EventHandler)opt141_CheckedChanged);
			((Control)opt120).set_AutoSize(true);
			((Control)opt120).set_Location(new Point(269, 33));
			((Control)opt120).set_Name("opt120");
			((Control)opt120).set_Size(new Size(43, 17));
			((Control)opt120).set_TabIndex(45);
			((Control)opt120).set_Text("0 hr");
			((ButtonBase)opt120).set_UseVisualStyleBackColor(true);
			opt120.add_CheckedChanged((EventHandler)opt120_CheckedChanged);
			((Control)opt99).set_AutoSize(true);
			((Control)opt99).set_Location(new Point(217, 50));
			((Control)opt99).set_Name("opt99");
			((Control)opt99).set_Size(new Size(43, 17));
			((Control)opt99).set_TabIndex(43);
			((Control)opt99).set_Text("3 hr");
			((ButtonBase)opt99).set_UseVisualStyleBackColor(true);
			opt99.add_CheckedChanged((EventHandler)opt99_CheckedChanged);
			((Control)opt102).set_AutoSize(true);
			((Control)opt102).set_Location(new Point(217, 67));
			((Control)opt102).set_Name("opt102");
			((Control)opt102).set_Size(new Size(43, 17));
			((Control)opt102).set_TabIndex(42);
			((Control)opt102).set_Text("6 hr");
			((ButtonBase)opt102).set_UseVisualStyleBackColor(true);
			opt102.add_CheckedChanged((EventHandler)opt102_CheckedChanged);
			((Control)opt105).set_AutoSize(true);
			((Control)opt105).set_Location(new Point(217, 84));
			((Control)opt105).set_Name("opt105");
			((Control)opt105).set_Size(new Size(43, 17));
			((Control)opt105).set_TabIndex(41);
			((Control)opt105).set_Text("9 hr");
			((ButtonBase)opt105).set_UseVisualStyleBackColor(true);
			opt105.add_CheckedChanged((EventHandler)opt105_CheckedChanged);
			((Control)opt108).set_AutoSize(true);
			((Control)opt108).set_Location(new Point(217, 101));
			((Control)opt108).set_Name("opt108");
			((Control)opt108).set_Size(new Size(49, 17));
			((Control)opt108).set_TabIndex(40);
			((Control)opt108).set_Text("12 hr");
			((ButtonBase)opt108).set_UseVisualStyleBackColor(true);
			opt108.add_CheckedChanged((EventHandler)opt108_CheckedChanged);
			((Control)opt111).set_AutoSize(true);
			((Control)opt111).set_Location(new Point(217, 118));
			((Control)opt111).set_Name("opt111");
			((Control)opt111).set_Size(new Size(49, 17));
			((Control)opt111).set_TabIndex(39);
			((Control)opt111).set_Text("15 hr");
			((ButtonBase)opt111).set_UseVisualStyleBackColor(true);
			opt111.add_CheckedChanged((EventHandler)opt111_CheckedChanged);
			((Control)opt114).set_AutoSize(true);
			((Control)opt114).set_Location(new Point(217, 135));
			((Control)opt114).set_Name("opt114");
			((Control)opt114).set_Size(new Size(49, 17));
			((Control)opt114).set_TabIndex(38);
			((Control)opt114).set_Text("18 hr");
			((ButtonBase)opt114).set_UseVisualStyleBackColor(true);
			opt114.add_CheckedChanged((EventHandler)opt114_CheckedChanged);
			((Control)opt117).set_AutoSize(true);
			((Control)opt117).set_Location(new Point(217, 152));
			((Control)opt117).set_Name("opt117");
			((Control)opt117).set_Size(new Size(49, 17));
			((Control)opt117).set_TabIndex(37);
			((Control)opt117).set_Text("21 hr");
			((ButtonBase)opt117).set_UseVisualStyleBackColor(true);
			opt117.add_CheckedChanged((EventHandler)opt117_CheckedChanged);
			((Control)opt96).set_AutoSize(true);
			((Control)opt96).set_Location(new Point(217, 33));
			((Control)opt96).set_Name("opt96");
			((Control)opt96).set_Size(new Size(43, 17));
			((Control)opt96).set_TabIndex(36);
			((Control)opt96).set_Text("0 hr");
			((ButtonBase)opt96).set_UseVisualStyleBackColor(true);
			opt96.add_CheckedChanged((EventHandler)opt96_CheckedChanged);
			((Control)lblDate4).set_AutoSize(true);
			((Control)lblDate4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblDate4).set_Location(new Point(162, 20));
			((Control)lblDate4).set_Name("lblDate4");
			((Control)lblDate4).set_Size(new Size(41, 13));
			((Control)lblDate4).set_TabIndex(35);
			((Control)lblDate4).set_Text("Date4");
			((Control)opt75).set_AutoSize(true);
			((Control)opt75).set_Location(new Point(165, 50));
			((Control)opt75).set_Name("opt75");
			((Control)opt75).set_Size(new Size(43, 17));
			((Control)opt75).set_TabIndex(34);
			((Control)opt75).set_Text("3 hr");
			((ButtonBase)opt75).set_UseVisualStyleBackColor(true);
			opt75.add_CheckedChanged((EventHandler)opt75_CheckedChanged);
			((Control)opt78).set_AutoSize(true);
			((Control)opt78).set_Location(new Point(165, 67));
			((Control)opt78).set_Name("opt78");
			((Control)opt78).set_Size(new Size(43, 17));
			((Control)opt78).set_TabIndex(33);
			((Control)opt78).set_Text("6 hr");
			((ButtonBase)opt78).set_UseVisualStyleBackColor(true);
			opt78.add_CheckedChanged((EventHandler)opt78_CheckedChanged);
			((Control)opt81).set_AutoSize(true);
			((Control)opt81).set_Location(new Point(165, 84));
			((Control)opt81).set_Name("opt81");
			((Control)opt81).set_Size(new Size(43, 17));
			((Control)opt81).set_TabIndex(32);
			((Control)opt81).set_Text("9 hr");
			((ButtonBase)opt81).set_UseVisualStyleBackColor(true);
			opt81.add_CheckedChanged((EventHandler)opt81_CheckedChanged);
			((Control)opt84).set_AutoSize(true);
			((Control)opt84).set_Location(new Point(165, 101));
			((Control)opt84).set_Name("opt84");
			((Control)opt84).set_Size(new Size(49, 17));
			((Control)opt84).set_TabIndex(31);
			((Control)opt84).set_Text("12 hr");
			((ButtonBase)opt84).set_UseVisualStyleBackColor(true);
			opt84.add_CheckedChanged((EventHandler)opt84_CheckedChanged);
			((Control)opt87).set_AutoSize(true);
			((Control)opt87).set_Location(new Point(165, 118));
			((Control)opt87).set_Name("opt87");
			((Control)opt87).set_Size(new Size(49, 17));
			((Control)opt87).set_TabIndex(30);
			((Control)opt87).set_Text("15 hr");
			((ButtonBase)opt87).set_UseVisualStyleBackColor(true);
			opt87.add_CheckedChanged((EventHandler)opt87_CheckedChanged);
			((Control)opt90).set_AutoSize(true);
			((Control)opt90).set_Location(new Point(165, 135));
			((Control)opt90).set_Name("opt90");
			((Control)opt90).set_Size(new Size(49, 17));
			((Control)opt90).set_TabIndex(29);
			((Control)opt90).set_Text("18 hr");
			((ButtonBase)opt90).set_UseVisualStyleBackColor(true);
			opt90.add_CheckedChanged((EventHandler)opt90_CheckedChanged);
			((Control)opt93).set_AutoSize(true);
			((Control)opt93).set_Location(new Point(165, 152));
			((Control)opt93).set_Name("opt93");
			((Control)opt93).set_Size(new Size(49, 17));
			((Control)opt93).set_TabIndex(28);
			((Control)opt93).set_Text("21 hr");
			((ButtonBase)opt93).set_UseVisualStyleBackColor(true);
			opt93.add_CheckedChanged((EventHandler)opt93_CheckedChanged);
			((Control)opt72).set_AutoSize(true);
			((Control)opt72).set_Location(new Point(165, 33));
			((Control)opt72).set_Name("opt72");
			((Control)opt72).set_Size(new Size(43, 17));
			((Control)opt72).set_TabIndex(27);
			((Control)opt72).set_Text("0 hr");
			((ButtonBase)opt72).set_UseVisualStyleBackColor(true);
			opt72.add_CheckedChanged((EventHandler)opt72_CheckedChanged);
			((Control)lblDate3).set_AutoSize(true);
			((Control)lblDate3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblDate3).set_Location(new Point(110, 20));
			((Control)lblDate3).set_Name("lblDate3");
			((Control)lblDate3).set_Size(new Size(41, 13));
			((Control)lblDate3).set_TabIndex(26);
			((Control)lblDate3).set_Text("Date3");
			((Control)opt51).set_AutoSize(true);
			((Control)opt51).set_Location(new Point(113, 50));
			((Control)opt51).set_Name("opt51");
			((Control)opt51).set_Size(new Size(43, 17));
			((Control)opt51).set_TabIndex(25);
			((Control)opt51).set_Text("3 hr");
			((ButtonBase)opt51).set_UseVisualStyleBackColor(true);
			opt51.add_CheckedChanged((EventHandler)opt51_CheckedChanged);
			((Control)opt54).set_AutoSize(true);
			((Control)opt54).set_Location(new Point(113, 67));
			((Control)opt54).set_Name("opt54");
			((Control)opt54).set_Size(new Size(43, 17));
			((Control)opt54).set_TabIndex(24);
			((Control)opt54).set_Text("6 hr");
			((ButtonBase)opt54).set_UseVisualStyleBackColor(true);
			opt54.add_CheckedChanged((EventHandler)opt54_CheckedChanged);
			((Control)opt57).set_AutoSize(true);
			((Control)opt57).set_Location(new Point(113, 84));
			((Control)opt57).set_Name("opt57");
			((Control)opt57).set_Size(new Size(43, 17));
			((Control)opt57).set_TabIndex(23);
			((Control)opt57).set_Text("9 hr");
			((ButtonBase)opt57).set_UseVisualStyleBackColor(true);
			opt57.add_CheckedChanged((EventHandler)opt57_CheckedChanged);
			((Control)opt60).set_AutoSize(true);
			((Control)opt60).set_Location(new Point(113, 101));
			((Control)opt60).set_Name("opt60");
			((Control)opt60).set_Size(new Size(49, 17));
			((Control)opt60).set_TabIndex(22);
			((Control)opt60).set_Text("12 hr");
			((ButtonBase)opt60).set_UseVisualStyleBackColor(true);
			opt60.add_CheckedChanged((EventHandler)opt60_CheckedChanged);
			((Control)opt63).set_AutoSize(true);
			((Control)opt63).set_Location(new Point(113, 118));
			((Control)opt63).set_Name("opt63");
			((Control)opt63).set_Size(new Size(49, 17));
			((Control)opt63).set_TabIndex(21);
			((Control)opt63).set_Text("15 hr");
			((ButtonBase)opt63).set_UseVisualStyleBackColor(true);
			opt63.add_CheckedChanged((EventHandler)opt63_CheckedChanged);
			((Control)opt66).set_AutoSize(true);
			((Control)opt66).set_Location(new Point(113, 135));
			((Control)opt66).set_Name("opt66");
			((Control)opt66).set_Size(new Size(49, 17));
			((Control)opt66).set_TabIndex(20);
			((Control)opt66).set_Text("18 hr");
			((ButtonBase)opt66).set_UseVisualStyleBackColor(true);
			opt66.add_CheckedChanged((EventHandler)opt66_CheckedChanged);
			((Control)opt69).set_AutoSize(true);
			((Control)opt69).set_Location(new Point(113, 152));
			((Control)opt69).set_Name("opt69");
			((Control)opt69).set_Size(new Size(49, 17));
			((Control)opt69).set_TabIndex(19);
			((Control)opt69).set_Text("21 hr");
			((ButtonBase)opt69).set_UseVisualStyleBackColor(true);
			opt69.add_CheckedChanged((EventHandler)opt69_CheckedChanged);
			((Control)opt48).set_AutoSize(true);
			((Control)opt48).set_Location(new Point(113, 33));
			((Control)opt48).set_Name("opt48");
			((Control)opt48).set_Size(new Size(43, 17));
			((Control)opt48).set_TabIndex(18);
			((Control)opt48).set_Text("0 hr");
			((ButtonBase)opt48).set_UseVisualStyleBackColor(true);
			opt48.add_CheckedChanged((EventHandler)opt48_CheckedChanged);
			((Control)lblDate2).set_AutoSize(true);
			((Control)lblDate2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblDate2).set_Location(new Point(58, 20));
			((Control)lblDate2).set_Name("lblDate2");
			((Control)lblDate2).set_Size(new Size(41, 13));
			((Control)lblDate2).set_TabIndex(17);
			((Control)lblDate2).set_Text("Date2");
			((Control)opt27).set_AutoSize(true);
			((Control)opt27).set_Location(new Point(61, 50));
			((Control)opt27).set_Name("opt27");
			((Control)opt27).set_Size(new Size(43, 17));
			((Control)opt27).set_TabIndex(16);
			((Control)opt27).set_Text("3 hr");
			((ButtonBase)opt27).set_UseVisualStyleBackColor(true);
			opt27.add_CheckedChanged((EventHandler)opt27_CheckedChanged);
			((Control)opt30).set_AutoSize(true);
			((Control)opt30).set_Location(new Point(61, 67));
			((Control)opt30).set_Name("opt30");
			((Control)opt30).set_Size(new Size(43, 17));
			((Control)opt30).set_TabIndex(15);
			((Control)opt30).set_Text("6 hr");
			((ButtonBase)opt30).set_UseVisualStyleBackColor(true);
			opt30.add_CheckedChanged((EventHandler)opt30_CheckedChanged);
			((Control)opt33).set_AutoSize(true);
			((Control)opt33).set_Location(new Point(61, 84));
			((Control)opt33).set_Name("opt33");
			((Control)opt33).set_Size(new Size(43, 17));
			((Control)opt33).set_TabIndex(14);
			((Control)opt33).set_Text("9 hr");
			((ButtonBase)opt33).set_UseVisualStyleBackColor(true);
			opt33.add_CheckedChanged((EventHandler)opt33_CheckedChanged);
			((Control)opt36).set_AutoSize(true);
			((Control)opt36).set_Location(new Point(61, 101));
			((Control)opt36).set_Name("opt36");
			((Control)opt36).set_Size(new Size(49, 17));
			((Control)opt36).set_TabIndex(13);
			((Control)opt36).set_Text("12 hr");
			((ButtonBase)opt36).set_UseVisualStyleBackColor(true);
			opt36.add_CheckedChanged((EventHandler)opt36_CheckedChanged);
			((Control)opt39).set_AutoSize(true);
			((Control)opt39).set_Location(new Point(61, 118));
			((Control)opt39).set_Name("opt39");
			((Control)opt39).set_Size(new Size(49, 17));
			((Control)opt39).set_TabIndex(12);
			((Control)opt39).set_Text("15 hr");
			((ButtonBase)opt39).set_UseVisualStyleBackColor(true);
			opt39.add_CheckedChanged((EventHandler)opt39_CheckedChanged);
			((Control)opt42).set_AutoSize(true);
			((Control)opt42).set_Location(new Point(61, 135));
			((Control)opt42).set_Name("opt42");
			((Control)opt42).set_Size(new Size(49, 17));
			((Control)opt42).set_TabIndex(11);
			((Control)opt42).set_Text("18 hr");
			((ButtonBase)opt42).set_UseVisualStyleBackColor(true);
			opt42.add_CheckedChanged((EventHandler)opt42_CheckedChanged);
			((Control)opt45).set_AutoSize(true);
			((Control)opt45).set_Location(new Point(61, 152));
			((Control)opt45).set_Name("opt45");
			((Control)opt45).set_Size(new Size(49, 17));
			((Control)opt45).set_TabIndex(10);
			((Control)opt45).set_Text("21 hr");
			((ButtonBase)opt45).set_UseVisualStyleBackColor(true);
			opt45.add_CheckedChanged((EventHandler)opt45_CheckedChanged);
			((Control)opt24).set_AutoSize(true);
			((Control)opt24).set_Location(new Point(61, 33));
			((Control)opt24).set_Name("opt24");
			((Control)opt24).set_Size(new Size(43, 17));
			((Control)opt24).set_TabIndex(9);
			((Control)opt24).set_Text("0 hr");
			((ButtonBase)opt24).set_UseVisualStyleBackColor(true);
			opt24.add_CheckedChanged((EventHandler)opt24_CheckedChanged);
			((Control)lblDate1).set_AutoSize(true);
			((Control)lblDate1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblDate1).set_Location(new Point(6, 20));
			((Control)lblDate1).set_Name("lblDate1");
			((Control)lblDate1).set_Size(new Size(41, 13));
			((Control)lblDate1).set_TabIndex(8);
			((Control)lblDate1).set_Text("Date1");
			((Control)opt3).set_AutoSize(true);
			((Control)opt3).set_Location(new Point(9, 50));
			((Control)opt3).set_Name("opt3");
			((Control)opt3).set_Size(new Size(43, 17));
			((Control)opt3).set_TabIndex(7);
			((Control)opt3).set_Text("3 hr");
			((ButtonBase)opt3).set_UseVisualStyleBackColor(true);
			opt3.add_CheckedChanged((EventHandler)opt3_CheckedChanged);
			((Control)opt6).set_AutoSize(true);
			((Control)opt6).set_Location(new Point(9, 67));
			((Control)opt6).set_Name("opt6");
			((Control)opt6).set_Size(new Size(43, 17));
			((Control)opt6).set_TabIndex(6);
			((Control)opt6).set_Text("6 hr");
			((ButtonBase)opt6).set_UseVisualStyleBackColor(true);
			opt6.add_CheckedChanged((EventHandler)opt6_CheckedChanged);
			((Control)opt9).set_AutoSize(true);
			((Control)opt9).set_Location(new Point(9, 84));
			((Control)opt9).set_Name("opt9");
			((Control)opt9).set_Size(new Size(43, 17));
			((Control)opt9).set_TabIndex(5);
			((Control)opt9).set_Text("9 hr");
			((ButtonBase)opt9).set_UseVisualStyleBackColor(true);
			opt9.add_CheckedChanged((EventHandler)opt9_CheckedChanged);
			((Control)opt12).set_AutoSize(true);
			((Control)opt12).set_Location(new Point(9, 101));
			((Control)opt12).set_Name("opt12");
			((Control)opt12).set_Size(new Size(49, 17));
			((Control)opt12).set_TabIndex(4);
			((Control)opt12).set_Text("12 hr");
			((ButtonBase)opt12).set_UseVisualStyleBackColor(true);
			opt12.add_CheckedChanged((EventHandler)opt12_CheckedChanged);
			((Control)opt15).set_AutoSize(true);
			((Control)opt15).set_Location(new Point(9, 118));
			((Control)opt15).set_Name("opt15");
			((Control)opt15).set_Size(new Size(49, 17));
			((Control)opt15).set_TabIndex(3);
			((Control)opt15).set_Text("15 hr");
			((ButtonBase)opt15).set_UseVisualStyleBackColor(true);
			opt15.add_CheckedChanged((EventHandler)opt15_CheckedChanged);
			((Control)opt18).set_AutoSize(true);
			((Control)opt18).set_Location(new Point(9, 135));
			((Control)opt18).set_Name("opt18");
			((Control)opt18).set_Size(new Size(49, 17));
			((Control)opt18).set_TabIndex(2);
			((Control)opt18).set_Text("18 hr");
			((ButtonBase)opt18).set_UseVisualStyleBackColor(true);
			opt18.add_CheckedChanged((EventHandler)opt18_CheckedChanged);
			((Control)opt21).set_AutoSize(true);
			((Control)opt21).set_Location(new Point(9, 152));
			((Control)opt21).set_Name("opt21");
			((Control)opt21).set_Size(new Size(49, 17));
			((Control)opt21).set_TabIndex(1);
			((Control)opt21).set_Text("21 hr");
			((ButtonBase)opt21).set_UseVisualStyleBackColor(true);
			opt21.add_CheckedChanged((EventHandler)opt21_CheckedChanged);
			((Control)opt0).set_AutoSize(true);
			opt0.set_Checked(true);
			((Control)opt0).set_Location(new Point(9, 33));
			((Control)opt0).set_Name("opt0");
			((Control)opt0).set_Size(new Size(43, 17));
			((Control)opt0).set_TabIndex(0);
			opt0.set_TabStop(true);
			((Control)opt0).set_Text("0 hr");
			((ButtonBase)opt0).set_UseVisualStyleBackColor(true);
			opt0.add_CheckedChanged((EventHandler)opt0_CheckedChanged);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withMapToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(864, 24));
			((Control)menuStrip1).set_TabIndex(5);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withMapToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem,
				(ToolStripItem)saveAnimationAsAnimatedGIFToolStripMenuItem
			});
			((ToolStripItem)withMapToolStripMenuItem).set_Name("withMapToolStripMenuItem");
			((ToolStripItem)withMapToolStripMenuItem).set_Size(new Size(96, 20));
			((ToolStripItem)withMapToolStripMenuItem).set_Text("with Map...      ");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(242, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(242, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)saveAnimationAsAnimatedGIFToolStripMenuItem).set_Name("saveAnimationAsAnimatedGIFToolStripMenuItem");
			((ToolStripItem)saveAnimationAsAnimatedGIFToolStripMenuItem).set_Size(new Size(242, 22));
			((ToolStripItem)saveAnimationAsAnimatedGIFToolStripMenuItem).set_Text("Save animation as animated GIF");
			((ToolStripItem)saveAnimationAsAnimatedGIFToolStripMenuItem).add_Click((EventHandler)saveAnimationAsAnimatedGIFToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(53, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)picCloud1).set_BackColor(Color.Black);
			((Control)picCloud1).set_Location(new Point(9, 193));
			((Control)picCloud1).set_Name("picCloud1");
			((Control)picCloud1).set_Size(new Size(810, 602));
			picCloud1.set_TabIndex(7);
			picCloud1.set_TabStop(false);
			((Control)picCloud1).set_Visible(false);
			((Control)picCloud_1).set_BackColor(Color.Black);
			((Control)picCloud_1).set_Location(new Point(9, 193));
			((Control)picCloud_1).set_Name("picCloud_1");
			((Control)picCloud_1).set_Size(new Size(810, 602));
			picCloud_1.set_TabIndex(8);
			picCloud_1.set_TabStop(false);
			((Control)picCloud_1).set_Visible(false);
			((Control)picCloud_2).set_BackColor(Color.Black);
			((Control)picCloud_2).set_Location(new Point(9, 193));
			((Control)picCloud_2).set_Name("picCloud_2");
			((Control)picCloud_2).set_Size(new Size(810, 602));
			picCloud_2.set_TabIndex(9);
			picCloud_2.set_TabStop(false);
			((Control)picCloud_2).set_Visible(false);
			((Control)picCloud_3).set_BackColor(Color.Black);
			((Control)picCloud_3).set_Location(new Point(9, 193));
			((Control)picCloud_3).set_Name("picCloud_3");
			((Control)picCloud_3).set_Size(new Size(810, 602));
			picCloud_3.set_TabIndex(10);
			picCloud_3.set_TabStop(false);
			((Control)picCloud_3).set_Visible(false);
			timer1.set_Interval(400);
			timer1.add_Tick((EventHandler)timer1_Tick);
			((Control)TrackSpeed).set_AutoSize(false);
			((Control)TrackSpeed).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "WeatherMapInterval", true, (DataSourceUpdateMode)1));
			((Control)TrackSpeed).set_Location(new Point(123, 159));
			TrackSpeed.set_Maximum(20);
			TrackSpeed.set_Minimum(2);
			((Control)TrackSpeed).set_Name("TrackSpeed");
			((Control)TrackSpeed).set_Size(new Size(97, 29));
			TrackSpeed.set_SmallChange(2);
			((Control)TrackSpeed).set_TabIndex(12);
			TrackSpeed.set_TickFrequency(2);
			TrackSpeed.set_TickStyle((TickStyle)1);
			TrackSpeed.set_Value(Settings.Default.WeatherMapInterval);
			((Control)chkAnimated).set_AutoSize(true);
			((Control)chkAnimated).set_Location(new Point(9, 167));
			((Control)chkAnimated).set_Name("chkAnimated");
			((Control)chkAnimated).set_Size(new Size(72, 17));
			((Control)chkAnimated).set_TabIndex(11);
			((Control)chkAnimated).set_Text("Animation");
			((ButtonBase)chkAnimated).set_UseVisualStyleBackColor(true);
			chkAnimated.add_CheckedChanged((EventHandler)chkAnimated_CheckedChanged);
			((ScrollableControl)Panel).set_AutoScroll(true);
			Panel.set_BorderStyle((BorderStyle)2);
			((Control)Panel).get_Controls().Add((Control)(object)label3);
			((Control)Panel).get_Controls().Add((Control)(object)label2);
			((Control)Panel).get_Controls().Add((Control)(object)picCloudDisplay);
			((Control)Panel).get_Controls().Add((Control)(object)label1);
			((Control)Panel).get_Controls().Add((Control)(object)TrackSpeed);
			((Control)Panel).get_Controls().Add((Control)(object)picCloud);
			((Control)Panel).get_Controls().Add((Control)(object)chkAnimated);
			((Control)Panel).get_Controls().Add((Control)(object)picCloud_3);
			((Control)Panel).get_Controls().Add((Control)(object)picCloud_2);
			((Control)Panel).get_Controls().Add((Control)(object)picCloud_1);
			((Control)Panel).get_Controls().Add((Control)(object)picCloud1);
			((Control)Panel).get_Controls().Add((Control)(object)grpDateTime);
			((Control)Panel).get_Controls().Add((Control)(object)groupBox2);
			((Control)Panel).get_Controls().Add((Control)(object)groupBox1);
			((Control)Panel).set_Location(new Point(7, 26));
			((Control)Panel).set_Name("Panel");
			((Control)Panel).set_Size(new Size(852, 801));
			((Control)Panel).set_TabIndex(13);
			((Control)picCloudDisplay).set_BackColor(Color.Black);
			((Control)picCloudDisplay).set_Location(new Point(9, 193));
			((Control)picCloudDisplay).set_Name("picCloudDisplay");
			((Control)picCloudDisplay).set_Size(new Size(810, 602));
			picCloudDisplay.set_TabIndex(14);
			picCloudDisplay.set_TabStop(false);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(96, 169));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(31, 12));
			((Control)label1).set_TabIndex(13);
			((Control)label1).set_Text("Speed");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(204, 181));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(24, 12));
			((Control)label2).set_TabIndex(15);
			((Control)label2).set_Text("Fast");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(112, 181));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(26, 12));
			((Control)label3).set_TabIndex(16);
			((Control)label3).set_Text("Slow");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(864, 832));
			((Control)this).get_Controls().Add((Control)(object)Panel);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MaximumSize(new Size(881, 868));
			((Control)this).set_MinimumSize(new Size(880, 36));
			((Control)this).set_Name("CloudMap");
			((Control)this).set_Text("Weather Maps      [ from 7Timer Weachart ]");
			((Form)this).add_Load((EventHandler)CloudMap_Load);
			((Form)this).add_FormClosed(new FormClosedEventHandler(CloudMap_FormClosed));
			((Control)this).add_Resize((EventHandler)CloudMap_Resize);
			((ISupportInitialize)picCloud).EndInit();
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((Control)groupBox2).ResumeLayout(false);
			((Control)groupBox2).PerformLayout();
			((Control)grpDateTime).ResumeLayout(false);
			((Control)grpDateTime).PerformLayout();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((ISupportInitialize)picCloud1).EndInit();
			((ISupportInitialize)picCloud_1).EndInit();
			((ISupportInitialize)picCloud_2).EndInit();
			((ISupportInitialize)picCloud_3).EndInit();
			((ISupportInitialize)TrackSpeed).EndInit();
			((Control)Panel).ResumeLayout(false);
			((Control)Panel).PerformLayout();
			((ISupportInitialize)picCloudDisplay).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
