using System;
using System.ComponentModel;
using System.Diagnostics;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Star_Catalogues
{
	public class Star_Diameter_from_Gaia : Form
	{
		private IContainer components;

		private Label label2;

		private Label label1;

		internal TextBox txtGaiaID;

		private Button cmdQueryTapVizieR;

		private Button cmdParamsup;

		private Label label3;

		private Label label4;

		private Button cmdHelp;

		public Star_Diameter_from_Gaia()
		{
			InitializeComponent();
		}

		private void cmdQueryTapVizieR_Click(object sender, EventArgs e)
		{
			string text = "SELECT Rad AS \"Solar radius, GSP-Aeneas 'best'\",round(9.304*X.plx*Rad/1000,3) AS \"Star diameter mas, GSP-Aeneas 'best'\",round(9.304*X.plx*(\"B_Rad\"-Rad)/1000,3) AS plus,round(9.304*X.plx*(\"b_Rad\"-Rad)/1000,3) AS Minus,\r\n\"Rad-Flame\" AS \"Solar radius FLAME\",round(9.304*X.plx*\"Rad-Flame\"/1000,3) AS \"Star diameter mas, FLAME\",round(9.304*X.plx*(\"B_Rad-Flame\"-\"Rad-Flame\")/1000,3) AS plus,round(9.304*X.plx*(\"b_Rad-Flame\"-\"Rad-Flame\")/1000,3) AS Minus\r\nFROM \"I/355/paramp\"\r\nJOIN \"I/355/gaiadr3\" as X using (Source)\r\nWHERE \"I/355/paramp\".Source = " + ((Control)txtGaiaID).get_Text().Trim();
			try
			{
				Clipboard.SetText(text);
			}
			catch
			{
				try
				{
					Clipboard.SetText(text);
				}
				catch
				{
				}
			}
			Process.Start("https://tapvizier.cds.unistra.fr/adql/?I/359");
		}

		private void cmdParamsup_Click(object sender, EventArgs e)
		{
			string text = "SELECT Rad AS \"Solar radius, GSP-Aeneas MARCS\",round(9.304*X.plx*Rad/1000,3) AS \"Star diameter mas, GSP-Aeneas MARCS\",round(9.304*X.plx*(\"B_Rad\"-Rad)/1000,3) AS \"plus\",round(9.304*X.plx*(\"b_Rad\"-Rad)/1000,3) AS \"minus\",\r\n\"Rad-Phx\" AS \"Solar radius GSP-Aeneas PHOENIX\",round(9.304*X.plx*\"Rad-Phx\"/1000,3) AS \"Star diameter mas, GSP-Phx\",round(9.304*X.plx*(\"B_Rad-Phx\"-\"Rad-Phx\")/1000,3) AS \"plus\",round(9.304*X.plx*(\"b_Rad-Phx\"-\"Rad-Phx\")/1000,3) AS \"minus\",\r\n\"Rad-OB\" AS \"Solar radius GSP-Aeneas OB\",round(9.304*X.plx*\"Rad-OB\"/1000,3) AS \"Star diameter mas, GSP-OB\",round(9.304*X.plx*(\"B_Rad-OB\"-\"Rad-OB\")/1000,3) AS \"plus\",round(9.304*X.plx*(\"b_Rad-OB\"-\"Rad-OB\")/1000,3) AS \"minus\",\r\n\"Rad-A\" AS \"Solar radius GSP-Aeneas A\",round(9.304*X.plx*\"Rad-A\"/1000,3) AS \"Star diameter mas, GSP-A\",round(9.304*X.plx*(\"B_Rad-A\"-\"Rad-A\")/1000,3) AS \"plus\",round(9.304*X.plx*(\"b_Rad-A\"-\"Rad-A\")/1000,3) AS \"minus\",\r\n\"Rad-Flame\" AS \"Solar radius FLAME bright\",round(9.304*X.plx*\"Rad-Flame\"/1000,3) AS \"Star diameter mas, Flame bright\",round(9.304*X.plx*(\"B_Rad-Flame\"-\"Rad-Flame\")/1000,3) AS \"plus\",round(9.304*X.plx*(\"b_Rad-Flame\"-\"Rad-Flame\")/1000,3) AS \"minus\"\r\nFROM \"I/355/paramsup\"\r\nJOIN \"I/355/gaiadr3\" as X using (Source)\r\nWHERE \"I/355/paramsup\".Source = " + ((Control)txtGaiaID).get_Text().Trim();
			try
			{
				Clipboard.SetText(text);
			}
			catch
			{
				try
				{
					Clipboard.SetText(text);
				}
				catch
				{
				}
			}
			Process.Start("https://tapvizier.cds.unistra.fr/adql/?I/359");
		}

		private void cmdHelp_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Gaia star diameters");
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
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(Star_Diameter_from_Gaia));
			label2 = new Label();
			label1 = new Label();
			txtGaiaID = new TextBox();
			cmdQueryTapVizieR = new Button();
			cmdParamsup = new Button();
			label3 = new Label();
			label4 = new Label();
			cmdHelp = new Button();
			((Control)this).SuspendLayout();
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_BackColor(Color.AntiqueWhite);
			label2.set_BorderStyle((BorderStyle)2);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(6, 192));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(285, 223));
			((Control)label2).set_TabIndex(13);
			((Control)label2).set_Text(componentResourceManager.GetString("label2.Text"));
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(2, 18));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(72, 13));
			((Control)label1).set_TabIndex(12);
			((Control)label1).set_Text("Gaia star id");
			((Control)txtGaiaID).set_Location(new Point(76, 14));
			((Control)txtGaiaID).set_Name("txtGaiaID");
			((Control)txtGaiaID).set_Size(new Size(131, 20));
			((Control)txtGaiaID).set_TabIndex(11);
			txtGaiaID.set_TextAlign((HorizontalAlignment)2);
			((Control)cmdQueryTapVizieR).set_BackColor(Color.LightGreen);
			((Control)cmdQueryTapVizieR).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdQueryTapVizieR).set_Location(new Point(2, 45));
			((Control)cmdQueryTapVizieR).set_Name("cmdQueryTapVizieR");
			((Control)cmdQueryTapVizieR).set_Size(new Size(115, 44));
			((Control)cmdQueryTapVizieR).set_TabIndex(10);
			((Control)cmdQueryTapVizieR).set_Text("Diameters from\r\nGaia 'paramp'");
			((ButtonBase)cmdQueryTapVizieR).set_UseVisualStyleBackColor(false);
			((Control)cmdQueryTapVizieR).add_Click((EventHandler)cmdQueryTapVizieR_Click);
			((Control)cmdParamsup).set_BackColor(Color.PaleTurquoise);
			((Control)cmdParamsup).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdParamsup).set_Location(new Point(2, 118));
			((Control)cmdParamsup).set_Name("cmdParamsup");
			((Control)cmdParamsup).set_Size(new Size(115, 40));
			((Control)cmdParamsup).set_TabIndex(14);
			((Control)cmdParamsup).set_Text("Diameters from\r\nGaia 'paramsup'");
			((ButtonBase)cmdParamsup).set_UseVisualStyleBackColor(false);
			((Control)cmdParamsup).add_Click((EventHandler)cmdParamsup_Click);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_BackColor(Color.Honeydew);
			label3.set_BorderStyle((BorderStyle)2);
			((Control)label3).set_Location(new Point(119, 47));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(170, 41));
			((Control)label3).set_TabIndex(15);
			((Control)label3).set_Text("Retrieves diameter estimates from:\r\n*   GSP-Phot Aeneas - 'best', &&\r\n*   FLAME");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_BackColor(Color.LightCyan);
			label4.set_BorderStyle((BorderStyle)2);
			((Control)label4).set_Location(new Point(119, 98));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(171, 80));
			((Control)label4).set_TabIndex(16);
			((Control)label4).set_Text("Retrieves diameter estimates from:\r\n*   GSP-Phot Aeneas - MARCS, \r\n*   GSP-Phot Aeneas - PHOENIX, \r\n*   GSP-Phot Aeneas - OB, \r\n*   GSP-Phot Aeneas - A, \r\n*   FLAME (bright sources)");
			((Control)cmdHelp).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((ButtonBase)cmdHelp).set_Image((Image)Resources.help);
			((ButtonBase)cmdHelp).set_ImageAlign(ContentAlignment.MiddleLeft);
			((Control)cmdHelp).set_Location(new Point(217, 5));
			((Control)cmdHelp).set_Name("cmdHelp");
			((Control)cmdHelp).set_Size(new Size(77, 34));
			((Control)cmdHelp).set_TabIndex(17);
			((Control)cmdHelp).set_Text("Help");
			((ButtonBase)cmdHelp).set_TextAlign(ContentAlignment.MiddleRight);
			((ButtonBase)cmdHelp).set_UseVisualStyleBackColor(true);
			((Control)cmdHelp).add_Click((EventHandler)cmdHelp_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(297, 418));
			((Control)this).get_Controls().Add((Control)(object)cmdHelp);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)cmdParamsup);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)txtGaiaID);
			((Control)this).get_Controls().Add((Control)(object)cmdQueryTapVizieR);
			((Control)this).set_Name("Star_Diameter_from_Gaia");
			((Control)this).set_Text("Star diameter from Gaia");
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
