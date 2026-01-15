using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using System.Xml;
using Occult.Properties;
using Occult.SesameService;

namespace Occult.Lunar_Observations
{
	public class ChooseStar : Form
	{
		private const double Radian = 180.0 / Math.PI;

		private IContainer components;

		private ListBox lstID;

		private Button cmdCancel;

		private Label lblEvent;

		private Label label1;

		private Label label3;

		private Button cmdAbort;

		private Label label5;

		private Label label2;

		private ListBox lstIDGSC;

		private Label lblCoords;

		private Button cmdIdentify;

		private TextBox txtNumber;

		private TextBox txtField;

		private GroupBox groupBox1;

		public ChooseStar()
		{
			InitializeComponent();
		}

		private void ChooseStar_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(50);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Width - ((Control)this).get_Width() - 50);
			}
			if (LunarObservations.EventCodeForIdentification == "D")
			{
				((Control)lblEvent).set_Text("Disappear");
			}
			else if (LunarObservations.EventCodeForIdentification == "R")
			{
				((Control)lblEvent).set_Text("Reappear");
			}
			else if (LunarObservations.EventCodeForIdentification == "B")
			{
				((Control)lblEvent).set_Text("Blink");
			}
			else if (LunarObservations.EventCodeForIdentification == "F")
			{
				((Control)lblEvent).set_Text("Flash");
			}
			else
			{
				((Control)lblEvent).set_Text("Miscellaneous event");
			}
			lstID.get_Items().Clear();
			for (int i = 0; i < LunarObservations.StarList.Count; i++)
			{
				lstID.get_Items().Add((object)LunarObservations.StarList[i].ToString());
			}
		}

		private void lstID_MouseDoubleClick(object sender, MouseEventArgs e)
		{
			LunarObservations.IDLine = ((ListControl)lstID).get_SelectedIndex();
			((Form)this).Close();
			((Form)this).set_DialogResult((DialogResult)1);
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			LunarObservations.IDLine = -1;
			((Form)this).Close();
			((Form)this).set_DialogResult((DialogResult)2);
		}

		private void cmdAbort_Click(object sender, EventArgs e)
		{
			LunarObservations.IDLine = -1;
			((Form)this).Close();
			((Form)this).set_DialogResult((DialogResult)3);
		}

		private void cmdIdentify_Click(object sender, EventArgs e)
		{
			XmlDocument xmlDocument = new XmlDocument();
			Cursor.set_Current(Cursors.get_WaitCursor());
			try
			{
				Occult.SesameService.SesameService sesameService = new Occult.SesameService.SesameService();
				string name = "GSC" + ((Control)txtField).get_Text() + "-" + ((Control)txtNumber).get_Text();
				string xml = sesameService.sesame(name, "xi");
				xmlDocument.LoadXml(xml);
			}
			catch (Exception)
			{
			}
			if (xmlDocument != null)
			{
				try
				{
					XmlNode xmlNode = xmlDocument.SelectSingleNode("//jpos");
					((Control)lblCoords).set_Text("(J2000)    RA: " + xmlNode.InnerText.Insert(xmlNode.InnerText.IndexOf(".") + 4, "    Dec: "));
					if (double.TryParse(xmlDocument.SelectSingleNode("//jradeg")!.InnerText.ToString(), out var result) & double.TryParse(xmlDocument.SelectSingleNode("//jdedeg")!.InnerText.ToString(), out var result2))
					{
						string text = Utilities.AppPath + "\\Resource Files\\XZ80.dat";
						List<PossibleStars> list = new List<PossibleStars>();
						list.Clear();
						int num = (int)Math.Floor(result - 0.1);
						int num2 = (int)Math.Ceiling(result + 0.1);
						if (num < 1)
						{
							num += 360;
						}
						if (num2 < 1)
						{
							num2 += 360;
						}
						if (num >= 360)
						{
							num -= 360;
						}
						if (num2 >= 360)
						{
							num2 -= 360;
						}
						LunarOccultations.InitialiseStarCatIndexArray(text);
						FileStream fileStream = new FileStream(text, FileMode.Open, FileAccess.Read);
						BinaryReader readXZ = new BinaryReader(fileStream);
						int num3 = LunarOccultations.XZIndex[num];
						int num4 = LunarOccultations.XZIndex[num2];
						int num5 = LunarOccultations.XZIndex[360];
						int num6 = num3;
						int num7 = num4;
						if (num4 < num3)
						{
							num6 = 1;
							num7 = num5;
						}
						fileStream.Seek(num6 * 35, SeekOrigin.Begin);
						for (int i = num6; i <= num7; i++)
						{
							if (num4 < num3 && i == num4)
							{
								i = num3;
								fileStream.Seek(i * 35, SeekOrigin.Begin);
							}
							XZ80Q.ReadStarEntry(fileStream, readXZ, i);
							double rA_rad = XZ80Q.RA_rad;
							double dec_rad = XZ80Q.Dec_rad;
							Utilities.Distance(result / (180.0 / Math.PI), result2 / (180.0 / Math.PI), rA_rad, dec_rad, out var Distance, out var _);
							Distance *= 648000.0 / Math.PI;
							if (Math.Abs(Distance) < 20.0)
							{
								PossibleStars possibleStars = new PossibleStars();
								possibleStars.Mag = XZ80Q.Mv;
								possibleStars.PA = 0.0;
								possibleStars.Residual = Distance;
								possibleStars.XZ = XZ80Q.XZ;
								if (XZ80Q.ZC > 0)
								{
									possibleStars.Number = "R" + XZ80Q.ZC;
								}
								else if (XZ80Q.SAO > 0)
								{
									possibleStars.Number = "S" + XZ80Q.SAO;
								}
								else
								{
									possibleStars.Number = "X" + XZ80Q.XZ;
								}
								list.Add(possibleStars);
							}
						}
						fileStream.Close();
						PossibleStars.SortFlag = 0;
						list.Sort();
						lstIDGSC.get_Items().Clear();
						for (int j = 0; j < list.Count; j++)
						{
							lstIDGSC.get_Items().Add((object)list[j].ToString());
						}
					}
				}
				catch
				{
				}
			}
			Cursor.set_Current(Cursors.get_Default());
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
			//IL_0134: Unknown result type (might be due to invalid IL or missing references)
			//IL_013e: Expected O, but got Unknown
			//IL_083a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0844: Expected O, but got Unknown
			lstID = new ListBox();
			cmdCancel = new Button();
			lblEvent = new Label();
			label1 = new Label();
			label3 = new Label();
			cmdAbort = new Button();
			label5 = new Label();
			label2 = new Label();
			lstIDGSC = new ListBox();
			lblCoords = new Label();
			cmdIdentify = new Button();
			txtNumber = new TextBox();
			txtField = new TextBox();
			groupBox1 = new GroupBox();
			((Control)groupBox1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)lstID).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstID).set_FormattingEnabled(true);
			lstID.set_ItemHeight(14);
			((Control)lstID).set_Location(new Point(8, 60));
			((Control)lstID).set_Name("lstID");
			((Control)lstID).set_Size(new Size(234, 102));
			((Control)lstID).set_TabIndex(0);
			((Control)lstID).add_MouseDoubleClick(new MouseEventHandler(lstID_MouseDoubleClick));
			((Control)cmdCancel).set_Location(new Point(44, 173));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(73, 30));
			((Control)cmdCancel).set_TabIndex(1);
			((Control)cmdCancel).set_Text("Not listed");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)lblEvent).set_AutoSize(true);
			((Control)lblEvent).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblEvent).set_Location(new Point(12, 6));
			((Control)lblEvent).set_Name("lblEvent");
			((Control)lblEvent).set_Size(new Size(41, 13));
			((Control)lblEvent).set_TabIndex(2);
			((Control)lblEvent).set_Text("label1");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(9, 44));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(196, 14));
			((Control)label1).set_TabIndex(3);
			((Control)label1).set_Text("Star No.  Mag   P.A.   Res.");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(66, 24));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(118, 12));
			((Control)label3).set_TabIndex(5);
			((Control)label3).set_Text("Double-click to select a star");
			((Control)cmdAbort).set_Location(new Point(134, 173));
			((Control)cmdAbort).set_Name("cmdAbort");
			((Control)cmdAbort).set_Size(new Size(73, 30));
			((Control)cmdAbort).set_TabIndex(6);
			((Control)cmdAbort).set_Text("Abort");
			((ButtonBase)cmdAbort).set_UseVisualStyleBackColor(true);
			((Control)cmdAbort).add_Click((EventHandler)cmdAbort_Click);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(16, 73));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(224, 14));
			((Control)label5).set_TabIndex(14);
			((Control)label5).set_Text("Star No.  Mag   [--]   Diff.(\")");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(3, 25));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(67, 13));
			((Control)label2).set_TabIndex(13);
			((Control)label2).set_Text("GSC number");
			((Control)lstIDGSC).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstIDGSC).set_FormattingEnabled(true);
			lstIDGSC.set_ItemHeight(14);
			((Control)lstIDGSC).set_Location(new Point(16, 88));
			((Control)lstIDGSC).set_Name("lstIDGSC");
			((Control)lstIDGSC).set_Size(new Size(213, 46));
			((Control)lstIDGSC).set_TabIndex(12);
			((Control)lblCoords).set_AutoSize(true);
			((Control)lblCoords).set_Location(new Point(3, 51));
			((Control)lblCoords).set_Name("lblCoords");
			((Control)lblCoords).set_Size(new Size(101, 13));
			((Control)lblCoords).set_TabIndex(11);
			((Control)lblCoords).set_Text("Coordinates (J2000)");
			((Control)cmdIdentify).set_Location(new Point(160, 20));
			((Control)cmdIdentify).set_Name("cmdIdentify");
			((Control)cmdIdentify).set_Size(new Size(76, 23));
			((Control)cmdIdentify).set_TabIndex(10);
			((Control)cmdIdentify).set_Text("Match to XZ");
			((ButtonBase)cmdIdentify).set_UseVisualStyleBackColor(true);
			((Control)cmdIdentify).add_Click((EventHandler)cmdIdentify_Click);
			((Control)txtNumber).set_Location(new Point(117, 21));
			((Control)txtNumber).set_Name("txtNumber");
			((Control)txtNumber).set_Size(new Size(34, 20));
			((Control)txtNumber).set_TabIndex(9);
			((Control)txtField).set_Location(new Point(72, 21));
			((Control)txtField).set_Name("txtField");
			((Control)txtField).set_Size(new Size(39, 20));
			((Control)txtField).set_TabIndex(8);
			((Control)groupBox1).get_Controls().Add((Control)(object)label5);
			((Control)groupBox1).get_Controls().Add((Control)(object)label2);
			((Control)groupBox1).get_Controls().Add((Control)(object)lstIDGSC);
			((Control)groupBox1).get_Controls().Add((Control)(object)lblCoords);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdIdentify);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtNumber);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtField);
			((Control)groupBox1).set_Location(new Point(3, 232));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(244, 138));
			((Control)groupBox1).set_TabIndex(15);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Match a GSC number");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(250, 373));
			((Form)this).set_ControlBox(false);
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Control)this).get_Controls().Add((Control)(object)cmdAbort);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)lblEvent);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_Controls().Add((Control)(object)lstID);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationLunarChooseStar", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Location(Settings.Default.LocationLunarChooseStar);
			((Control)this).set_Name("ChooseStar");
			((Form)this).set_ShowInTaskbar(false);
			((Form)this).set_StartPosition((FormStartPosition)0);
			((Control)this).set_Text("Select star");
			((Form)this).add_Load((EventHandler)ChooseStar_Load);
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
