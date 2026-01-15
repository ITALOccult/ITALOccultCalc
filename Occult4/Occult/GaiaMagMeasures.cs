using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class GaiaMagMeasures : Form
	{
		private List<GaiaMags> G_mags = new List<GaiaMags>();

		private GaiaMags G;

		private string Query = "";

		internal double PA;

		internal double Lat;

		internal float Oblate = 1f;

		private IContainer components;

		private Button cmdQueryTapVizieR;

		private Label label1;

		internal TextBox txtAsteroid;

		private Label label2;

		private Button cmdReadDownload;

		private ListBox lstMags;

		private Label label3;

		private Button cmdCopyAsCSV;

		private Button cmdCopy;

		private Label label4;

		private Label label5;

		private Label label6;

		private Button cmdHelp;

		internal TextBox txtDec;

		internal TextBox txtRA;

		internal TextBox txtPoleRA;

		internal TextBox txtPoleDec;

		internal TextBox txtOblate;

		internal TextBox txtLat1;

		internal TextBox txtPA1;

		private Label label7;

		private Label label8;

		private Label label9;

		private Label label10;

		private Label label11;

		private Label label12;

		private Label label14;

		private PictureBox picShape;

		private Panel panel1;

		private Label label13;

		private Label label15;

		public GaiaMagMeasures()
		{
			InitializeComponent();
		}

		private void GaiaMagMeasures_Load(object sender, EventArgs e)
		{
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
		}

		internal void CreateQuery()
		{
			Query = "-- output format : csv\r\nSELECT \"I/359/ssoobs\".MPC,  2455197.5 + \"I/359/ssoobs\".EpochUTC,  \"I/359/ssoobs\".Gmag\r\nFROM \"I/359/ssoobs\"\r\nWHERE \"I/359/ssoobs\".MPC=" + ((Control)txtAsteroid).get_Text().Trim();
		}

		private void cmdQueryTapVizieR_Click(object sender, EventArgs e)
		{
			CreateQuery();
			try
			{
				Clipboard.SetText(Query);
			}
			catch
			{
				try
				{
					Clipboard.SetText(Query);
				}
				catch
				{
				}
			}
			Process.Start("https://tapvizier.cds.unistra.fr/adql/?I/359");
		}

		private void cmdReadDownload_Click(object sender, EventArgs e)
		{
			//IL_000b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0011: Expected O, but got Unknown
			//IL_0060: Unknown result type (might be due to invalid IL or missing references)
			//IL_0066: Invalid comparison between Unknown and I4
			G_mags.Clear();
			OpenFileDialog val = new OpenFileDialog();
			((FileDialog)val).set_Title("Specify file to READ asteroidal observations from.");
			((FileDialog)val).set_Filter("Text files (*.csv)|*.csv|All files (*.*)|*.*");
			((FileDialog)val).set_FilterIndex(0);
			val.set_Multiselect(false);
			((FileDialog)val).set_FileName(Settings.Default.GaiaMags_File);
			try
			{
				((FileDialog)val).set_InitialDirectory(Path.GetDirectoryName(Settings.Default.GaiaMags_File));
			}
			catch
			{
			}
			if ((int)((CommonDialog)val).ShowDialog() != 1)
			{
				return;
			}
			string fileName = ((FileDialog)val).get_FileName();
			Settings.Default.GaiaMags_File = fileName;
			using (StreamReader streamReader = new StreamReader(fileName))
			{
				streamReader.ReadLine();
				do
				{
					string[] array = streamReader.ReadLine()!.Split(new char[1] { ',' });
					G = new GaiaMags();
					G.AstNum = int.Parse(array[0]);
					G.JD = double.Parse(array[1]);
					double.TryParse(array[2], out var result);
					if (result < 4.0)
					{
						result = 25.0;
					}
					G.MagG = result;
					G_mags.Add(G);
				}
				while (!streamReader.EndOfStream);
			}
			G_mags.Sort();
			for (int num = G_mags.Count - 1; num > 0; num--)
			{
				if (Math.Abs(G_mags[num].JD - G_mags[num - 1].JD) < 0.1)
				{
					G_mags.RemoveAt(num);
				}
			}
			lstMags.get_Items().Clear();
			lstMags.get_Items().Add((object)"      Date      Gaia   Horiz  Horiz-Gaia");
			for (int i = 0; i < G_mags.Count; i++)
			{
				if (G_mags[i].MagG < 21.0)
				{
					http.GetHorizonsAsteroidJ2000Ephemeris(((Control)txtAsteroid).get_Text().Trim(), G_mags[i].JD, 1.0, UseTTnotUT: false, UseOldHorizonsOrbit: false, "", out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var Mag);
					G_mags[i].MagEphem = Mag;
					lstMags.get_Items().Add((object)G_mags[i].ToString());
				}
			}
		}

		private void cmdCopyAsCSV_Click(object sender, EventArgs e)
		{
			string text = "Date,JD,Gaia,Horiz,Horiz-Gaia\r\n";
			for (int i = 0; i < G_mags.Count; i++)
			{
				if (G_mags[i].MagG < 21.0)
				{
					text = text + Utilities.Date_from_JD(G_mags[i].JD, 2) + "," + string.Format("{0,1:f5},{1,1:f2},{2,1:f2},{3,1:f2}\r\n", G_mags[i].JD, G_mags[i].MagG, G_mags[i].MagEphem, G_mags[i].MagEphem - G_mags[i].MagG);
				}
			}
			Clipboard.SetText(text);
		}

		private void cmdCopy_Click(object sender, EventArgs e)
		{
			string text = "";
			for (int i = 0; i < lstMags.get_Items().get_Count(); i++)
			{
				text = text + lstMags.get_Items().get_Item(i).ToString() + "\r\n";
			}
			Clipboard.SetText(text);
		}

		private void cmdHelp_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Gaia Mags");
		}

		private void label2_Click(object sender, EventArgs e)
		{
		}

		private void txtAsteroid_TextChanged(object sender, EventArgs e)
		{
		}

		private void label4_Click(object sender, EventArgs e)
		{
		}

		private void label1_Click(object sender, EventArgs e)
		{
		}

		internal void PlotAsteroid()
		{
			Bitmap image = new Bitmap(200, 200);
			Graphics graphics = Graphics.FromImage(image);
			graphics.Clear(Color.FloralWhite);
			Pen pen = new Pen(Brushes.Black, 1f);
			Font font = new Font("MSSansSerif", 10f, FontStyle.Bold);
			Font font2 = new Font("MSSansSerif", 8f);
			Brush black = Brushes.Black;
			float x = 0f;
			float y = 0f;
			float num;
			float num2 = (num = (float)((Control)picShape).get_Width() / 2.4f) * Oblate;
			double num3 = (double)num * Math.Sin(Lat / (180.0 / Math.PI));
			double num4 = (double)num2 * Math.Cos(Lat / (180.0 / Math.PI));
			float num5 = (float)Math.Sqrt(num3 * num3 + num4 * num4);
			float num6 = (float)((Control)picShape).get_Width() / 2f;
			float num7 = (float)((Control)picShape).get_Height() / 2f;
			double num8 = Math.Sin(PA / (180.0 / Math.PI));
			double num9 = Math.Cos(PA / (180.0 / Math.PI));
			for (float num10 = 0f; num10 < 361f; num10 += 1f)
			{
				num3 = (double)num * Math.Sin((double)num10 / (180.0 / Math.PI));
				num4 = (double)num5 * Math.Cos((double)num10 / (180.0 / Math.PI));
				float num11 = num6 + (float)(num3 * num9 + num4 * num8);
				float num12 = num7 + (float)((0.0 - num3) * num8 + num4 * num9);
				if (num10 > 0f)
				{
					graphics.DrawLine(pen, x, y, num11, num12);
				}
				x = num11;
				y = num12;
			}
			graphics.DrawString("N", font, black, num6 - 7f, 1f);
			graphics.DrawString("E", font, black, 1f, num7 - 7f);
			string s = string.Format("Axis ratio {0,1:f2}:1", num / num5);
			graphics.DrawString(s, font2, black, 2f, 160f);
			picShape.set_Image((Image)image);
			graphics.Dispose();
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
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(GaiaMagMeasures));
			cmdQueryTapVizieR = new Button();
			txtAsteroid = new TextBox();
			label1 = new Label();
			label2 = new Label();
			cmdReadDownload = new Button();
			lstMags = new ListBox();
			label3 = new Label();
			cmdCopyAsCSV = new Button();
			cmdCopy = new Button();
			label4 = new Label();
			label5 = new Label();
			label6 = new Label();
			cmdHelp = new Button();
			txtPoleRA = new TextBox();
			txtPoleDec = new TextBox();
			txtOblate = new TextBox();
			txtDec = new TextBox();
			txtRA = new TextBox();
			txtLat1 = new TextBox();
			txtPA1 = new TextBox();
			label7 = new Label();
			label8 = new Label();
			label9 = new Label();
			label10 = new Label();
			label11 = new Label();
			label12 = new Label();
			label14 = new Label();
			picShape = new PictureBox();
			panel1 = new Panel();
			label13 = new Label();
			label15 = new Label();
			((ISupportInitialize)picShape).BeginInit();
			((Control)panel1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)cmdQueryTapVizieR).set_BackColor(Color.PaleGreen);
			((Control)cmdQueryTapVizieR).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdQueryTapVizieR).set_Location(new Point(92, 50));
			((Control)cmdQueryTapVizieR).set_Name("cmdQueryTapVizieR");
			((Control)cmdQueryTapVizieR).set_Size(new Size(117, 54));
			((Control)cmdQueryTapVizieR).set_TabIndex(0);
			((Control)cmdQueryTapVizieR).set_Text("Create list of magnitudes from TapVizieR");
			((ButtonBase)cmdQueryTapVizieR).set_UseVisualStyleBackColor(false);
			((Control)cmdQueryTapVizieR).add_Click((EventHandler)cmdQueryTapVizieR_Click);
			((Control)txtAsteroid).set_Location(new Point(118, 20));
			((Control)txtAsteroid).set_Name("txtAsteroid");
			((TextBoxBase)txtAsteroid).set_ReadOnly(true);
			((Control)txtAsteroid).set_Size(new Size(64, 20));
			((Control)txtAsteroid).set_TabIndex(1);
			txtAsteroid.set_TextAlign((HorizontalAlignment)2);
			((Control)txtAsteroid).add_TextChanged((EventHandler)txtAsteroid_TextChanged);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(33, 24));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(83, 13));
			((Control)label1).set_TabIndex(2);
			((Control)label1).set_Text("Asteroid number");
			((Control)label1).add_Click((EventHandler)label1_Click);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_BackColor(Color.AntiqueWhite);
			label2.set_BorderStyle((BorderStyle)2);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(3, 118));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(295, 262));
			((Control)label2).set_TabIndex(3);
			((Control)label2).set_Text(componentResourceManager.GetString("label2.Text"));
			((Control)label2).add_Click((EventHandler)label2_Click);
			((Control)cmdReadDownload).set_BackColor(Color.MediumSpringGreen);
			((Control)cmdReadDownload).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdReadDownload).set_Location(new Point(85, 398));
			((Control)cmdReadDownload).set_Name("cmdReadDownload");
			((Control)cmdReadDownload).set_Size(new Size(131, 38));
			((Control)cmdReadDownload).set_TabIndex(4);
			((Control)cmdReadDownload).set_Text("Read downloaded file");
			((ButtonBase)cmdReadDownload).set_UseVisualStyleBackColor(false);
			((Control)cmdReadDownload).add_Click((EventHandler)cmdReadDownload_Click);
			((Control)lstMags).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstMags).set_FormattingEnabled(true);
			((Control)lstMags).set_Location(new Point(321, 77));
			((Control)lstMags).set_Name("lstMags");
			lstMags.set_ScrollAlwaysVisible(true);
			((Control)lstMags).set_Size(new Size(287, 303));
			((Control)lstMags).set_TabIndex(5);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(318, 56));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(292, 17));
			((Control)label3).set_TabIndex(6);
			((Control)label3).set_Text("Gaia mags, Horizon mags, && difference");
			((Control)cmdCopyAsCSV).set_BackColor(Color.Gold);
			((Control)cmdCopyAsCSV).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdCopyAsCSV).set_ForeColor(Color.Maroon);
			((Control)cmdCopyAsCSV).set_Location(new Point(321, 398));
			((Control)cmdCopyAsCSV).set_Name("cmdCopyAsCSV");
			((Control)cmdCopyAsCSV).set_Size(new Size(166, 38));
			((Control)cmdCopyAsCSV).set_TabIndex(7);
			((Control)cmdCopyAsCSV).set_Text("Copy list in csv format,\r\nready to Paste into Excel");
			((ButtonBase)cmdCopyAsCSV).set_UseVisualStyleBackColor(false);
			((Control)cmdCopyAsCSV).add_Click((EventHandler)cmdCopyAsCSV_Click);
			((Control)cmdCopy).set_BackColor(Color.Khaki);
			((Control)cmdCopy).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdCopy).set_ForeColor(Color.DarkRed);
			((Control)cmdCopy).set_Location(new Point(518, 398));
			((Control)cmdCopy).set_Name("cmdCopy");
			((Control)cmdCopy).set_Size(new Size(90, 38));
			((Control)cmdCopy).set_TabIndex(8);
			((Control)cmdCopy).set_Text("Copy list as text");
			((ButtonBase)cmdCopy).set_UseVisualStyleBackColor(false);
			((Control)cmdCopy).add_Click((EventHandler)cmdCopy_Click);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label4).set_ForeColor(Color.DarkBlue);
			((Control)label4).set_Location(new Point(32, 69));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(55, 17));
			((Control)label4).set_TabIndex(9);
			((Control)label4).set_Text("Step 1");
			((Control)label4).add_Click((EventHandler)label4_Click);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label5).set_ForeColor(Color.DarkBlue);
			((Control)label5).set_Location(new Point(25, 409));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(55, 17));
			((Control)label5).set_TabIndex(10);
			((Control)label5).set_Text("Step 2");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label6).set_ForeColor(Color.DarkBlue);
			((Control)label6).set_Location(new Point(260, 409));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(55, 17));
			((Control)label6).set_TabIndex(11);
			((Control)label6).set_Text("Step 3");
			((Control)cmdHelp).set_BackColor(Color.LightSkyBlue);
			((Control)cmdHelp).set_Font(new Font("Microsoft Sans Serif", 11f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((ButtonBase)cmdHelp).set_Image((Image)Resources.help);
			((ButtonBase)cmdHelp).set_ImageAlign(ContentAlignment.MiddleLeft);
			((Control)cmdHelp).set_Location(new Point(526, 5));
			((Control)cmdHelp).set_Name("cmdHelp");
			((Control)cmdHelp).set_Size(new Size(82, 35));
			((Control)cmdHelp).set_TabIndex(12);
			((Control)cmdHelp).set_Text("Help");
			((ButtonBase)cmdHelp).set_TextAlign(ContentAlignment.MiddleRight);
			((ButtonBase)cmdHelp).set_UseVisualStyleBackColor(false);
			((Control)cmdHelp).add_Click((EventHandler)cmdHelp_Click);
			((Control)txtPoleRA).set_Location(new Point(51, 163));
			((Control)txtPoleRA).set_Name("txtPoleRA");
			((TextBoxBase)txtPoleRA).set_ReadOnly(true);
			((Control)txtPoleRA).set_Size(new Size(41, 20));
			((Control)txtPoleRA).set_TabIndex(13);
			((Control)txtPoleDec).set_Location(new Point(103, 163));
			((Control)txtPoleDec).set_Name("txtPoleDec");
			((TextBoxBase)txtPoleDec).set_ReadOnly(true);
			((Control)txtPoleDec).set_Size(new Size(41, 20));
			((Control)txtPoleDec).set_TabIndex(14);
			((Control)txtOblate).set_Location(new Point(165, 163));
			((Control)txtOblate).set_Name("txtOblate");
			((TextBoxBase)txtOblate).set_ReadOnly(true);
			((Control)txtOblate).set_Size(new Size(41, 20));
			((Control)txtOblate).set_TabIndex(15);
			((Control)txtDec).set_Location(new Point(103, 137));
			((Control)txtDec).set_Name("txtDec");
			((TextBoxBase)txtDec).set_ReadOnly(true);
			((Control)txtDec).set_Size(new Size(41, 20));
			((Control)txtDec).set_TabIndex(17);
			((Control)txtRA).set_Location(new Point(51, 137));
			((Control)txtRA).set_Name("txtRA");
			((TextBoxBase)txtRA).set_ReadOnly(true);
			((Control)txtRA).set_Size(new Size(41, 20));
			((Control)txtRA).set_TabIndex(16);
			((Control)txtLat1).set_Location(new Point(105, 215));
			((Control)txtLat1).set_Name("txtLat1");
			((TextBoxBase)txtLat1).set_ReadOnly(true);
			((Control)txtLat1).set_Size(new Size(36, 20));
			((Control)txtLat1).set_TabIndex(19);
			((Control)txtPA1).set_Location(new Point(52, 215));
			((Control)txtPA1).set_Name("txtPA1");
			((TextBoxBase)txtPA1).set_ReadOnly(true);
			((Control)txtPA1).set_Size(new Size(38, 20));
			((Control)txtPA1).set_TabIndex(18);
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(4, 141));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(45, 13));
			((Control)label7).set_TabIndex(20);
			((Control)label7).set_Text("Asteroid");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(21, 167));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(28, 13));
			((Control)label8).set_TabIndex(21);
			((Control)label8).set_Text("Pole");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(57, 122));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(28, 13));
			((Control)label9).set_TabIndex(22);
			((Control)label9).set_Text("R.A.");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(110, 121));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(27, 13));
			((Control)label10).set_TabIndex(23);
			((Control)label10).set_Text("Dec");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Location(new Point(155, 148));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(60, 13));
			((Control)label11).set_TabIndex(24);
			((Control)label11).set_Text("Oblateness");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Location(new Point(44, 202));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(55, 13));
			((Control)label12).set_TabIndex(27);
			((Control)label12).set_Text("PA of Axis");
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Location(new Point(101, 202));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(45, 13));
			((Control)label14).set_TabIndex(29);
			((Control)label14).set_Text("Latitude");
			((Control)picShape).set_BackColor(Color.FloralWhite);
			picShape.set_BorderStyle((BorderStyle)2);
			((Control)picShape).set_Location(new Point(27, 252));
			((Control)picShape).set_Name("picShape");
			((Control)picShape).set_Size(new Size(180, 180));
			picShape.set_TabIndex(31);
			picShape.set_TabStop(false);
			((Control)panel1).set_BackColor(Color.Honeydew);
			((Control)panel1).get_Controls().Add((Control)(object)label13);
			((Control)panel1).get_Controls().Add((Control)(object)picShape);
			((Control)panel1).get_Controls().Add((Control)(object)label14);
			((Control)panel1).get_Controls().Add((Control)(object)label12);
			((Control)panel1).get_Controls().Add((Control)(object)label11);
			((Control)panel1).get_Controls().Add((Control)(object)label10);
			((Control)panel1).get_Controls().Add((Control)(object)label9);
			((Control)panel1).get_Controls().Add((Control)(object)label8);
			((Control)panel1).get_Controls().Add((Control)(object)label7);
			((Control)panel1).get_Controls().Add((Control)(object)txtLat1);
			((Control)panel1).get_Controls().Add((Control)(object)txtPA1);
			((Control)panel1).get_Controls().Add((Control)(object)txtDec);
			((Control)panel1).get_Controls().Add((Control)(object)txtRA);
			((Control)panel1).get_Controls().Add((Control)(object)txtOblate);
			((Control)panel1).get_Controls().Add((Control)(object)txtPoleDec);
			((Control)panel1).get_Controls().Add((Control)(object)txtPoleRA);
			((Control)panel1).set_Location(new Point(626, 0));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(235, 445));
			((Control)panel1).set_TabIndex(33);
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label13).set_ForeColor(Color.Red);
			((Control)label13).set_Location(new Point(14, 10));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(206, 100));
			((Control)label13).set_TabIndex(33);
			((Control)label13).set_Text("Possible asteroid profile.\r\nThis plot may have\r\npoor reliability\r\n\r\nRead the Help topic !!\r\n");
			label13.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label15).set_ForeColor(Color.Red);
			((Control)label15).set_Location(new Point(207, 4));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(306, 40));
			((Control)label15).set_TabIndex(34);
			((Control)label15).set_Text("Read the Help topic !!\r\nThe data provided is hard to interpret\r\n");
			label15.set_TextAlign(ContentAlignment.MiddleCenter);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(860, 442));
			((Control)this).get_Controls().Add((Control)(object)label15);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)cmdHelp);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)cmdCopy);
			((Control)this).get_Controls().Add((Control)(object)cmdCopyAsCSV);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)lstMags);
			((Control)this).get_Controls().Add((Control)(object)cmdReadDownload);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)txtAsteroid);
			((Control)this).get_Controls().Add((Control)(object)cmdQueryTapVizieR);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Control)this).set_Name("GaiaMagMeasures");
			((Control)this).set_Text("Gaia magnitudes compared to Horizons.    Possible low-reliability profile");
			((Form)this).add_Load((EventHandler)GaiaMagMeasures_Load);
			((ISupportInitialize)picShape).EndInit();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
