using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using GaiaDoubles;
using Occult.Properties;

namespace Occult.Star_Catalogues
{
	public class GaiaDoubles_Display : Form
	{
		private string[] Headers = new string[26]
		{
			"  RA  ", "  Dec ", " Gmag", " Semimajor", " Period(d)", " Epoch(d)", " Ecc", " Inc", " Periastron", " Node",
			" A", " B", " F", " G", " EclP(d)", " DurnP(d)", " EclS(d)", " DurnS(d)", " pmRA", " dpmRA",
			" ddpmRA", " pmDec", " dpmDec", " ddpmDec", " ID", " Model"
		};

		private IContainer components;

		private Label label1;

		private Label label2;

		private Label label3;

		private Label label4;

		private Label label5;

		private Label label6;

		private Button cmdDisplay;

		private ListBox listOut;

		private Label label7;

		private Button cmdHelp;

		internal TextBox txtRAh;

		internal TextBox txtDecS;

		internal TextBox txtDecM;

		internal TextBox txtDecD;

		internal TextBox txtRAs;

		internal TextBox txtRAm;

		private Button cmdCopy;

		public GaiaDoubles_Display()
		{
			InitializeComponent();
		}

		private void cmdDisplay_Click(object sender, EventArgs e)
		{
			DisplayDoubles_Within15arcmin();
		}

		internal void DisplayDoubles_Within15arcmin()
		{
			double result = 0.0;
			double result2 = 0.0;
			double result3 = 0.0;
			double result4 = 0.0;
			double result5 = 0.0;
			double result6 = 0.0;
			double num = 0.0;
			double num2 = 0.0;
			string text = "";
			string text2 = "";
			if (!global::GaiaDoubles.GaiaDoubles.Initialise_Index())
			{
				return;
			}
			double.TryParse(((Control)txtRAh).get_Text(), out result);
			double.TryParse(((Control)txtRAm).get_Text(), out result2);
			double.TryParse(((Control)txtRAs).get_Text(), out result3);
			num = 15.0 * (result + result2 / 60.0 + result3 / 3600.0);
			text = Utilities.DEGtoDMS(num / 15.0, 2, 2, MinutesOnly: false);
			double.TryParse(((Control)txtDecD).get_Text().Replace("-", ""), out result4);
			double.TryParse(((Control)txtDecM).get_Text(), out result5);
			double.TryParse(((Control)txtDecS).get_Text(), out result6);
			num2 = result4 + result5 / 60.0 + result6 / 3600.0;
			if (((Control)txtDecD).get_Text().Contains("-"))
			{
				num2 = 0.0 - num2;
			}
			text2 = Utilities.DEGtoDMS(num2, 3, 1, MinutesOnly: false);
			int num3 = (int)(num * 4.0) - 1;
			if (num3 < 0)
			{
				num3 = 0;
			}
			int num4 = num3 + 3;
			if (num4 > 1440)
			{
				num4 = 1440;
			}
			FileStream fileStream = new FileStream(Utilities.AppPath + "\\Resource Files\\Gaia\\Gaia_Doubles.csv", FileMode.Open, FileAccess.Read);
			int num5 = (int)(global::GaiaDoubles.GaiaDoubles.Index[num4] - global::GaiaDoubles.GaiaDoubles.Index[num3]);
			byte[] array = new byte[num5 + 1];
			fileStream.Seek(global::GaiaDoubles.GaiaDoubles.Index[num3], SeekOrigin.Begin);
			fileStream.Read(array, 0, num5 - 2);
			string[] array2 = Encoding.UTF8.GetString(array, 0, num5 - 2).Split(new char[1] { '\n' });
			listOut.get_Items().Clear();
			listOut.get_Items().Add((object)("Gaia doubles within 15 arc mins of  " + text + ", " + text2 + "      Epoch 2016.0 = JD 2457388.405023"));
			double num6 = 0.25 / Math.Cos(num2 / (180.0 / Math.PI));
			for (int i = 0; i < array2.Length; i++)
			{
				string[] array3 = array2[i].Split(new char[1] { ',' });
				double.TryParse(array3[0], out var result7);
				double.TryParse(array3[1], out var result8);
				if ((Math.Abs(num2 - result8) < 0.25) & (Math.Abs(num - result7) < num6))
				{
					GaiaDoubles.Elements.SelectiveOut(array2[i], DetailsOnly: false, out var Line, out var Line2);
					listOut.get_Items().Add((object)"");
					if ((Math.Abs(num2 - result8) < 0.001) & (Math.Abs(num - result7) < 0.001))
					{
						listOut.get_Items().Add((object)"***   Probable match   ***");
					}
					listOut.get_Items().Add((object)Line);
					listOut.get_Items().Add((object)Line2);
				}
			}
			if (listOut.get_Items().get_Count() == 1)
			{
				listOut.get_Items().Add((object)"No Gaia double stars were found within 15 arc mins of the position");
			}
		}

		private void GaiaDoubles_Display_Create_Resize(object sender, EventArgs e)
		{
			if (((Control)this).get_Width() < 300)
			{
				((Control)this).set_Width(300);
			}
			if (((Control)this).get_Height() < 240)
			{
				((Control)this).set_Height(240);
			}
			((Control)listOut).set_Width(((Control)this).get_Width() - 30);
			((Control)listOut).set_Height(((Control)this).get_Height() - 100);
		}

		private void cmdHelp_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Gaia doubles");
		}

		private void txtRAh_KeyPress(object sender, KeyPressEventArgs e)
		{
			e.set_Handled(!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()));
		}

		private void txtRAm_KeyPress(object sender, KeyPressEventArgs e)
		{
			e.set_Handled(!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()));
		}

		private void txtRAs_KeyPress(object sender, KeyPressEventArgs e)
		{
			e.set_Handled(!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()) && e.get_KeyChar() != '.');
		}

		private void txtDecD_KeyPress(object sender, KeyPressEventArgs e)
		{
			e.set_Handled(!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()) && e.get_KeyChar() != '-');
		}

		private void txtDecM_KeyPress(object sender, KeyPressEventArgs e)
		{
			e.set_Handled(!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()));
		}

		private void txtDecS_KeyPress(object sender, KeyPressEventArgs e)
		{
			e.set_Handled(!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()) && e.get_KeyChar() != '.');
		}

		private void cmdCopy_Click(object sender, EventArgs e)
		{
			string text = "";
			for (int i = 0; i < listOut.get_Items().get_Count(); i++)
			{
				text = text + listOut.get_Items().get_Item(i).ToString() + "\r\n";
			}
			Clipboard.SetText(text);
		}

		private void txtRAh_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtRAh).SelectAll();
		}

		private void txtRAm_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtRAm).SelectAll();
		}

		private void txtRAs_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtRAs).SelectAll();
		}

		private void txtDecD_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtDecD).SelectAll();
		}

		private void txtDecM_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtDecM).SelectAll();
		}

		private void txtDecS_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtDecS).SelectAll();
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
			//IL_0139: Unknown result type (might be due to invalid IL or missing references)
			//IL_0143: Expected O, but got Unknown
			//IL_01bf: Unknown result type (might be due to invalid IL or missing references)
			//IL_01c9: Expected O, but got Unknown
			//IL_0245: Unknown result type (might be due to invalid IL or missing references)
			//IL_024f: Expected O, but got Unknown
			//IL_02cb: Unknown result type (might be due to invalid IL or missing references)
			//IL_02d5: Expected O, but got Unknown
			//IL_0350: Unknown result type (might be due to invalid IL or missing references)
			//IL_035a: Expected O, but got Unknown
			//IL_03d5: Unknown result type (might be due to invalid IL or missing references)
			//IL_03df: Expected O, but got Unknown
			txtRAh = new TextBox();
			txtDecS = new TextBox();
			txtDecM = new TextBox();
			txtDecD = new TextBox();
			txtRAs = new TextBox();
			txtRAm = new TextBox();
			label1 = new Label();
			label2 = new Label();
			label3 = new Label();
			label4 = new Label();
			label5 = new Label();
			label6 = new Label();
			cmdDisplay = new Button();
			listOut = new ListBox();
			label7 = new Label();
			cmdCopy = new Button();
			cmdHelp = new Button();
			((Control)this).SuspendLayout();
			((Control)txtRAh).set_Location(new Point(110, 20));
			((Control)txtRAh).set_Name("txtRAh");
			((Control)txtRAh).set_Size(new Size(28, 20));
			((Control)txtRAh).set_TabIndex(3);
			((Control)txtRAh).set_Text("1");
			((Control)txtRAh).add_Enter((EventHandler)txtRAh_Enter);
			((Control)txtRAh).add_KeyPress(new KeyPressEventHandler(txtRAh_KeyPress));
			((Control)txtDecS).set_Location(new Point(293, 20));
			((Control)txtDecS).set_Name("txtDecS");
			((Control)txtDecS).set_Size(new Size(28, 20));
			((Control)txtDecS).set_TabIndex(11);
			((Control)txtDecS).set_Text("12.3");
			((Control)txtDecS).add_Enter((EventHandler)txtDecS_Enter);
			((Control)txtDecS).add_KeyPress(new KeyPressEventHandler(txtDecS_KeyPress));
			((Control)txtDecM).set_Location(new Point(259, 20));
			((Control)txtDecM).set_Name("txtDecM");
			((Control)txtDecM).set_Size(new Size(28, 20));
			((Control)txtDecM).set_TabIndex(10);
			((Control)txtDecM).set_Text("11");
			((Control)txtDecM).add_Enter((EventHandler)txtDecM_Enter);
			((Control)txtDecM).add_KeyPress(new KeyPressEventHandler(txtDecM_KeyPress));
			((Control)txtDecD).set_Location(new Point(225, 20));
			((Control)txtDecD).set_Name("txtDecD");
			((Control)txtDecD).set_Size(new Size(28, 20));
			((Control)txtDecD).set_TabIndex(9);
			((Control)txtDecD).set_Text("-10");
			((Control)txtDecD).add_Enter((EventHandler)txtDecD_Enter);
			((Control)txtDecD).add_KeyPress(new KeyPressEventHandler(txtDecD_KeyPress));
			((Control)txtRAs).set_Location(new Point(178, 20));
			((Control)txtRAs).set_Name("txtRAs");
			((Control)txtRAs).set_Size(new Size(33, 20));
			((Control)txtRAs).set_TabIndex(5);
			((Control)txtRAs).set_Text("3.45");
			((Control)txtRAs).add_Enter((EventHandler)txtRAs_Enter);
			((Control)txtRAs).add_KeyPress(new KeyPressEventHandler(txtRAs_KeyPress));
			((Control)txtRAm).set_Location(new Point(144, 20));
			((Control)txtRAm).set_Name("txtRAm");
			((Control)txtRAm).set_Size(new Size(28, 20));
			((Control)txtRAm).set_TabIndex(4);
			((Control)txtRAm).set_Text("2");
			((Control)txtRAm).add_Enter((EventHandler)txtRAm_Enter);
			((Control)txtRAm).add_KeyPress(new KeyPressEventHandler(txtRAm_KeyPress));
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(116, 4));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(18, 13));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text("Hr");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(293, 4));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(26, 13));
			((Control)label2).set_TabIndex(8);
			((Control)label2).set_Text("Sec");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(261, 4));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(24, 13));
			((Control)label3).set_TabIndex(7);
			((Control)label3).set_Text("Min");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(228, 4));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(27, 13));
			((Control)label4).set_TabIndex(6);
			((Control)label4).set_Text("Deg");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(178, 4));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(26, 13));
			((Control)label5).set_TabIndex(2);
			((Control)label5).set_Text("Sec");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(146, 4));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(24, 13));
			((Control)label6).set_TabIndex(1);
			((Control)label6).set_Text("Min");
			((Control)cmdDisplay).set_BackColor(Color.Moccasin);
			((Control)cmdDisplay).set_Location(new Point(344, 10));
			((Control)cmdDisplay).set_Name("cmdDisplay");
			((Control)cmdDisplay).set_Size(new Size(78, 30));
			((Control)cmdDisplay).set_TabIndex(12);
			((Control)cmdDisplay).set_Text("Display");
			((ButtonBase)cmdDisplay).set_UseVisualStyleBackColor(false);
			((Control)cmdDisplay).add_Click((EventHandler)cmdDisplay_Click);
			((Control)listOut).set_Font(new Font("Cascadia Code", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)listOut).set_FormattingEnabled(true);
			listOut.set_HorizontalScrollbar(true);
			listOut.set_ItemHeight(16);
			((Control)listOut).set_Location(new Point(9, 47));
			((Control)listOut).set_Name("listOut");
			((Control)listOut).set_Size(new Size(767, 164));
			((Control)listOut).set_TabIndex(13);
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(10, 22));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(93, 13));
			((Control)label7).set_TabIndex(14);
			((Control)label7).set_Text("Star coords J2000");
			((Control)cmdCopy).set_BackColor(Color.Honeydew);
			((Control)cmdCopy).set_Location(new Point(496, 10));
			((Control)cmdCopy).set_Name("cmdCopy");
			((Control)cmdCopy).set_Size(new Size(78, 30));
			((Control)cmdCopy).set_TabIndex(16);
			((Control)cmdCopy).set_Text("Copy list");
			((ButtonBase)cmdCopy).set_UseVisualStyleBackColor(false);
			((Control)cmdCopy).add_Click((EventHandler)cmdCopy_Click);
			((Control)cmdHelp).set_BackColor(Color.SkyBlue);
			((Control)cmdHelp).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((ButtonBase)cmdHelp).set_Image((Image)Resources.Help16x16);
			((ButtonBase)cmdHelp).set_ImageAlign(ContentAlignment.MiddleLeft);
			((Control)cmdHelp).set_Location(new Point(691, 10));
			((Control)cmdHelp).set_Name("cmdHelp");
			((Control)cmdHelp).set_Size(new Size(78, 30));
			((Control)cmdHelp).set_TabIndex(15);
			((Control)cmdHelp).set_Text("Help");
			((ButtonBase)cmdHelp).set_UseVisualStyleBackColor(false);
			((Control)cmdHelp).add_Click((EventHandler)cmdHelp_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(781, 215));
			((Control)this).get_Controls().Add((Control)(object)cmdCopy);
			((Control)this).get_Controls().Add((Control)(object)cmdHelp);
			((Control)this).get_Controls().Add((Control)(object)label7);
			((Control)this).get_Controls().Add((Control)(object)listOut);
			((Control)this).get_Controls().Add((Control)(object)cmdDisplay);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)txtRAm);
			((Control)this).get_Controls().Add((Control)(object)txtRAs);
			((Control)this).get_Controls().Add((Control)(object)txtDecD);
			((Control)this).get_Controls().Add((Control)(object)txtDecM);
			((Control)this).get_Controls().Add((Control)(object)txtDecS);
			((Control)this).get_Controls().Add((Control)(object)txtRAh);
			((Control)this).set_Name("GaiaDoubles_Display");
			((Control)this).set_Text("Gaia double stars -  within 15 arc-mins of the coordinates");
			((Control)this).add_Resize((EventHandler)GaiaDoubles_Display_Create_Resize);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
