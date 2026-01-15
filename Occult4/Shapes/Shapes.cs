using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using System.Windows.Forms;
using GifCreator;
using Occult;
using Occult.Asteroid_Observations;
using Occult.Properties;

namespace Shapes
{
	public class Shapes : Form
	{
		private IContainer components;

		private Button cmdModelList;

		private ListBox lstAsteroids;

		private RadioButton optModel1;

		private RadioButton optModel4;

		private RadioButton optModel3;

		private RadioButton optModel2;

		private RadioButton optModel6;

		private RadioButton optModel5;

		private PictureBox picAst;

		private Button cmdPlot;

		private Label label1;

		private Label label2;

		private NumericUpDown updnYear;

		private NumericUpDown updnMonth;

		private NumericUpDown updnDay;

		private NumericUpDown updnHour;

		private NumericUpDown updnMinute;

		private Label label3;

		private Label label4;

		private Label label5;

		private Label label6;

		private Label label7;

		private CheckBox chkAnimate;

		private Panel panel1;

		private GroupBox groupBox1;

		private RadioButton optLightBlue;

		private RadioButton optWhite;

		private RadioButton optBlack;

		private Label label8;

		private Label label9;

		private Label lblVol;

		private Label lblSurf;

		private Label label10;

		private Button cmdDownloadAll;

		private PictureBox picResize;

		private Label label11;

		private Label label12;

		private Label lblPlotWidth;

		private GroupBox groupBox3;

		private RadioButton optEarth;

		private RadioButton optSky;

		private Label lblQuality;

		private Label lblVersion;

		private Label lblComment;

		private Label lblCreated;

		private Label lblModified;

		private TrackBar trackTransparency;

		private CheckBox chk300;

		private CheckBox chk600;

		private Label label13;

		private GroupBox groupBox2;

		private RadioButton optV3;

		private RadioButton optV2;

		private Label lblJD;

		private CheckBox chkAxesOfRotation;

		private RadioButton optModel8;

		private RadioButton optModel7;

		private RadioButton optModel10;

		private RadioButton optModel9;

		private RadioButton optModel12;

		private RadioButton optModel11;

		private Button cmdManuallyAdd;

		private Button cmdAll;

		private Button cmdHelp;

		private CheckBox chkAnimatedGif;

		private Label lblPAaxis;

		public Shapes()
		{
			InitializeComponent();
		}

		private void Form1_Load(object sender, EventArgs e)
		{
			((Control)cmdManuallyAdd).set_Visible(Utilities.IsInVisualStudio);
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			GetShapeModelData.InitialiseDAMITShapeModels();
			GetShapeModelData.Initialise_ISAM_ShapeModels();
			FillList();
			if (lstAsteroids.get_Items().get_Count() > 0)
			{
				((ListControl)lstAsteroids).set_SelectedIndex(0);
			}
			updnYear.set_Value((decimal)DateTime.Now.Year);
			updnMonth.set_Value((decimal)DateTime.Now.Month);
			updnDay.set_Value((decimal)DateTime.Now.Day);
			((Control)this).set_Cursor(Cursors.get_Default());
		}

		private void cmdModelList_Click(object sender, EventArgs e)
		{
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			GetShapeModelData.CreateDAMITModelList();
			GetShapeModelData.FillDAMITAsteroidModels();
			GetShapeModelData.Fill_ISAM_AsteroidModels();
			FillList();
			((Control)this).set_Cursor(Cursors.get_Default());
		}

		private void FillList()
		{
			int num = 0;
			for (int i = 0; i < GetShapeModelData.DAMITModelsByAsteroidNumber.Count; i++)
			{
				if (GetShapeModelData.DAMITModelsByAsteroidNumber[i].AsteroidNumber > num)
				{
					lstAsteroids.get_Items().Add((object)GetShapeModelData.DAMITModelsByAsteroidNumber[i].AsteroidNumber.ToString().PadLeft(6));
					num = GetShapeModelData.DAMITModelsByAsteroidNumber[i].AsteroidNumber;
				}
			}
			num = 0;
			Model_Parameters.Sort_0bysteroidNumber_1byModelNumberNumeric_2forISAM = 0;
			GetShapeModelData.ISAMmodelsByAsteroidNumber.Sort();
			for (int j = 0; j < GetShapeModelData.ISAMmodelsByAsteroidNumber.Count; j++)
			{
				if (GetShapeModelData.ISAMmodelsByAsteroidNumber[j].AsteroidNumber > num)
				{
					lstAsteroids.get_Items().Add((object)GetShapeModelData.ISAMmodelsByAsteroidNumber[j].AsteroidNumber.ToString().PadLeft(6));
					num = GetShapeModelData.ISAMmodelsByAsteroidNumber[j].AsteroidNumber;
				}
			}
			for (int num2 = lstAsteroids.get_Items().get_Count() - 1; num2 > 0; num2--)
			{
				if (lstAsteroids.get_Items().get_Item(num2).ToString() == lstAsteroids.get_Items().get_Item(num2 - 1).ToString())
				{
					lstAsteroids.get_Items().RemoveAt(num2);
				}
			}
		}

		private void lstAsteroids_SelectedIndexChanged(object sender, EventArgs e)
		{
			RadioButton obj = optModel1;
			RadioButton obj2 = optModel2;
			RadioButton obj3 = optModel3;
			RadioButton obj4 = optModel4;
			RadioButton obj5 = optModel5;
			bool flag;
			((Control)optModel6).set_Enabled(flag = false);
			bool flag2;
			((Control)obj5).set_Enabled(flag2 = flag);
			bool flag3;
			((Control)obj4).set_Enabled(flag3 = flag2);
			bool flag4;
			((Control)obj3).set_Enabled(flag4 = flag3);
			bool enabled;
			((Control)obj2).set_Enabled(enabled = flag4);
			((Control)obj).set_Enabled(enabled);
			RadioButton obj6 = optModel1;
			RadioButton obj7 = optModel2;
			RadioButton obj8 = optModel3;
			RadioButton obj9 = optModel4;
			RadioButton obj10 = optModel5;
			string text;
			((Control)optModel6).set_Text(text = "-");
			string text2;
			((Control)obj10).set_Text(text2 = text);
			string text3;
			((Control)obj9).set_Text(text3 = text2);
			string text4;
			((Control)obj8).set_Text(text4 = text3);
			string text5;
			((Control)obj7).set_Text(text5 = text4);
			((Control)obj6).set_Text(text5);
			RadioButton obj11 = optModel7;
			RadioButton obj12 = optModel8;
			RadioButton obj13 = optModel9;
			RadioButton obj14 = optModel10;
			RadioButton obj15 = optModel11;
			((Control)optModel12).set_Enabled(flag = false);
			((Control)obj15).set_Enabled(flag2 = flag);
			((Control)obj14).set_Enabled(flag3 = flag2);
			((Control)obj13).set_Enabled(flag4 = flag3);
			((Control)obj12).set_Enabled(enabled = flag4);
			((Control)obj11).set_Enabled(enabled);
			RadioButton obj16 = optModel7;
			RadioButton obj17 = optModel8;
			RadioButton obj18 = optModel9;
			RadioButton obj19 = optModel10;
			RadioButton obj20 = optModel11;
			((Control)optModel12).set_Text(text = "-");
			((Control)obj20).set_Text(text2 = text);
			((Control)obj19).set_Text(text3 = text2);
			((Control)obj18).set_Text(text4 = text3);
			((Control)obj17).set_Text(text5 = text4);
			((Control)obj16).set_Text(text5);
			int num = int.Parse(lstAsteroids.get_Items().get_Item(((ListControl)lstAsteroids).get_SelectedIndex()).ToString());
			DrawShapeModel.SingleAst = new List<Model_Parameters>();
			for (int i = 0; i < GetShapeModelData.DAMITModelsByAsteroidNumber.Count; i++)
			{
				if (GetShapeModelData.DAMITModelsByAsteroidNumber[i].AsteroidNumber == num)
				{
					DrawShapeModel.SingleAst.Add(GetShapeModelData.DAMITModelsByAsteroidNumber[i]);
				}
				if (GetShapeModelData.DAMITModelsByAsteroidNumber[i].AsteroidNumber > num)
				{
					break;
				}
			}
			Model_Parameters.Sort_0bysteroidNumber_1byModelNumberNumeric_2forISAM = 0;
			GetShapeModelData.ISAMmodelsByAsteroidNumber.Sort();
			for (int j = 0; j < GetShapeModelData.ISAMmodelsByAsteroidNumber.Count; j++)
			{
				if (GetShapeModelData.ISAMmodelsByAsteroidNumber[j].AsteroidNumber == num)
				{
					DrawShapeModel.SingleAst.Add(GetShapeModelData.ISAMmodelsByAsteroidNumber[j]);
				}
				if (GetShapeModelData.ISAMmodelsByAsteroidNumber[j].AsteroidNumber > num)
				{
					break;
				}
			}
			for (int k = 0; k < DrawShapeModel.SingleAst.Count; k++)
			{
				if (k == 0)
				{
					((Control)optModel1).set_Enabled(true);
					((Control)optModel1).set_Text(DrawShapeModel.SingleAst[k].ModelNumber_String.ToString());
				}
				if (k == 1)
				{
					((Control)optModel2).set_Enabled(true);
					((Control)optModel2).set_Text(DrawShapeModel.SingleAst[k].ModelNumber_String.ToString());
				}
				if (k == 2)
				{
					((Control)optModel3).set_Enabled(true);
					((Control)optModel3).set_Text(DrawShapeModel.SingleAst[k].ModelNumber_String.ToString());
				}
				if (k == 3)
				{
					((Control)optModel4).set_Enabled(true);
					((Control)optModel4).set_Text(DrawShapeModel.SingleAst[k].ModelNumber_String.ToString());
				}
				if (k == 4)
				{
					((Control)optModel5).set_Enabled(true);
					((Control)optModel5).set_Text(DrawShapeModel.SingleAst[k].ModelNumber_String.ToString());
				}
				if (k == 5)
				{
					((Control)optModel6).set_Enabled(true);
					((Control)optModel6).set_Text(DrawShapeModel.SingleAst[k].ModelNumber_String.ToString());
				}
				if (k == 6)
				{
					((Control)optModel7).set_Enabled(true);
					((Control)optModel7).set_Text(DrawShapeModel.SingleAst[k].ModelNumber_String.ToString());
				}
				if (k == 7)
				{
					((Control)optModel8).set_Enabled(true);
					((Control)optModel8).set_Text(DrawShapeModel.SingleAst[k].ModelNumber_String.ToString());
				}
				if (k == 8)
				{
					((Control)optModel9).set_Enabled(true);
					((Control)optModel9).set_Text(DrawShapeModel.SingleAst[k].ModelNumber_String.ToString());
				}
				if (k == 9)
				{
					((Control)optModel10).set_Enabled(true);
					((Control)optModel10).set_Text(DrawShapeModel.SingleAst[k].ModelNumber_String.ToString());
				}
				if (k == 10)
				{
					((Control)optModel11).set_Enabled(true);
					((Control)optModel11).set_Text(DrawShapeModel.SingleAst[k].ModelNumber_String.ToString());
				}
				if (k == 11)
				{
					((Control)optModel12).set_Enabled(true);
					((Control)optModel12).set_Text(DrawShapeModel.SingleAst[k].ModelNumber_String.ToString());
				}
			}
			optModel1.set_Checked(true);
		}

		private void cmdPlot_Click(object sender, EventArgs e)
		{
			PlotModel();
		}

		private void PlotModel()
		{
			//IL_0023: Unknown result type (might be due to invalid IL or missing references)
			//IL_0553: Unknown result type (might be due to invalid IL or missing references)
			List<string> list = new List<string>();
			bool flag = false;
			if (((ListControl)lstAsteroids).get_SelectedIndex() < 0)
			{
				MessageBox.Show("You must select an asteroid", "No asteroid", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			Application.DoEvents();
			int asteroidNumber = int.Parse(lstAsteroids.get_Items().get_Item(((ListControl)lstAsteroids).get_SelectedIndex()).ToString());
			int Current = 0;
			if (optModel1.get_Checked() & ((Control)optModel1).get_Text().Contains("A"))
			{
				DisplayShapeModels.Find_ISAM_ModelEntry(((Control)optModel1).get_Text(), out Current);
			}
			else if (optModel2.get_Checked() & ((Control)optModel2).get_Text().Contains("A"))
			{
				DisplayShapeModels.Find_ISAM_ModelEntry(((Control)optModel2).get_Text(), out Current);
			}
			else if (optModel3.get_Checked() & ((Control)optModel3).get_Text().Contains("A"))
			{
				DisplayShapeModels.Find_ISAM_ModelEntry(((Control)optModel3).get_Text(), out Current);
			}
			else if (optModel4.get_Checked() & ((Control)optModel4).get_Text().Contains("A"))
			{
				DisplayShapeModels.Find_ISAM_ModelEntry(((Control)optModel4).get_Text(), out Current);
			}
			else if (optModel5.get_Checked() & ((Control)optModel5).get_Text().Contains("A"))
			{
				DisplayShapeModels.Find_ISAM_ModelEntry(((Control)optModel5).get_Text(), out Current);
			}
			else if (optModel6.get_Checked() & ((Control)optModel6).get_Text().Contains("A"))
			{
				DisplayShapeModels.Find_ISAM_ModelEntry(((Control)optModel6).get_Text(), out Current);
			}
			else if (optModel7.get_Checked() & ((Control)optModel6).get_Text().Contains("A"))
			{
				DisplayShapeModels.Find_ISAM_ModelEntry(((Control)optModel7).get_Text(), out Current);
			}
			else if (optModel8.get_Checked() & ((Control)optModel6).get_Text().Contains("A"))
			{
				DisplayShapeModels.Find_ISAM_ModelEntry(((Control)optModel8).get_Text(), out Current);
			}
			else if (optModel9.get_Checked() & ((Control)optModel6).get_Text().Contains("A"))
			{
				DisplayShapeModels.Find_ISAM_ModelEntry(((Control)optModel9).get_Text(), out Current);
			}
			else if (optModel10.get_Checked() & ((Control)optModel6).get_Text().Contains("A"))
			{
				DisplayShapeModels.Find_ISAM_ModelEntry(((Control)optModel10).get_Text(), out Current);
			}
			else if (optModel11.get_Checked() & ((Control)optModel6).get_Text().Contains("A"))
			{
				DisplayShapeModels.Find_ISAM_ModelEntry(((Control)optModel11).get_Text(), out Current);
			}
			else if (optModel12.get_Checked() & ((Control)optModel6).get_Text().Contains("A"))
			{
				DisplayShapeModels.Find_ISAM_ModelEntry(((Control)optModel12).get_Text(), out Current);
			}
			if (optModel1.get_Checked())
			{
				flag = DrawShapeModel.PopulateDataElementsForTheShapeModel(((Control)optModel1).get_Text(), Current);
			}
			else if (optModel2.get_Checked())
			{
				flag = DrawShapeModel.PopulateDataElementsForTheShapeModel(((Control)optModel2).get_Text(), Current);
			}
			else if (optModel3.get_Checked())
			{
				flag = DrawShapeModel.PopulateDataElementsForTheShapeModel(((Control)optModel3).get_Text(), Current);
			}
			else if (optModel4.get_Checked())
			{
				flag = DrawShapeModel.PopulateDataElementsForTheShapeModel(((Control)optModel4).get_Text(), Current);
			}
			else if (optModel5.get_Checked())
			{
				flag = DrawShapeModel.PopulateDataElementsForTheShapeModel(((Control)optModel5).get_Text(), Current);
			}
			else if (optModel6.get_Checked())
			{
				flag = DrawShapeModel.PopulateDataElementsForTheShapeModel(((Control)optModel6).get_Text(), Current);
			}
			else if (optModel7.get_Checked())
			{
				flag = DrawShapeModel.PopulateDataElementsForTheShapeModel(((Control)optModel7).get_Text(), Current);
			}
			else if (optModel8.get_Checked())
			{
				flag = DrawShapeModel.PopulateDataElementsForTheShapeModel(((Control)optModel8).get_Text(), Current);
			}
			else if (optModel9.get_Checked())
			{
				flag = DrawShapeModel.PopulateDataElementsForTheShapeModel(((Control)optModel9).get_Text(), Current);
			}
			else if (optModel10.get_Checked())
			{
				flag = DrawShapeModel.PopulateDataElementsForTheShapeModel(((Control)optModel10).get_Text(), Current);
			}
			else if (optModel11.get_Checked())
			{
				flag = DrawShapeModel.PopulateDataElementsForTheShapeModel(((Control)optModel11).get_Text(), Current);
			}
			else if (optModel12.get_Checked())
			{
				flag = DrawShapeModel.PopulateDataElementsForTheShapeModel(((Control)optModel12).get_Text(), Current);
			}
			((Control)this).set_Cursor(Cursors.get_Default());
			if (!flag)
			{
				return;
			}
			double num = Utilities.JD_from_Date((int)updnYear.get_Value(), (int)updnMonth.get_Value(), (double)updnDay.get_Value() + (double)updnHour.get_Value() / 24.0 + (double)updnMinute.get_Value() / 1440.0);
			if (!DrawShapeModel.GetDataToOrientTheModel(num, asteroidNumber))
			{
				MessageBox.Show("The model cannot be drawn without knowing where the asteroid is located", "No orbital elements", (MessageBoxButtons)0, (MessageBoxIcon)64);
				return;
			}
			((Control)lblQuality).set_Text("Quality = " + DrawShapeModel.Quality);
			((Control)lblVersion).set_Text("Version: " + DrawShapeModel.Version);
			((Control)lblComment).set_Text("Comment:  " + DrawShapeModel.Comment);
			((Control)lblCreated).set_Text("Created: " + DrawShapeModel.Created);
			((Control)lblModified).set_Text("Modified: " + DrawShapeModel.Modified);
			((Control)lblPlotWidth).set_Text(((Control)picAst).get_Width().ToString());
			DrawShapeModel.OrientTheVertices(num, 0.0, 0.0);
			double num2 = 0.0;
			if (chkAnimate.get_Checked())
			{
				num2 = 360.0;
			}
			if (chkAnimate.get_Checked() & chkAnimatedGif.get_Checked())
			{
				string[] files = Directory.GetFiles(Utilities.AppPath + "\\Predictions\\AnimatedGIF", "*.gif");
				foreach (string path in files)
				{
					try
					{
						File.Delete(path);
					}
					catch
					{
					}
				}
				Settings.Default.AnimationInterval = 1;
			}
			bool @checked = optEarth.get_Checked();
			Color background = Color.Black;
			if (optWhite.get_Checked())
			{
				background = Color.White;
			}
			else if (optLightBlue.get_Checked())
			{
				background = Color.FromArgb(255, 220, 255, 255);
			}
			int num3 = 10000;
			for (double num4 = 0.0; num4 <= num2; num4 += 1.0)
			{
				double num5 = DrawShapeModel.Period / 24.0 / 360.0 * num4;
				DrawShapeModel.OrientTheVertices(num + num5, 0.0, 0.0);
				Bitmap image = new Bitmap(((Control)picAst).get_Width(), ((Control)picAst).get_Height());
				Graphics graphics = Graphics.FromImage(image);
				DrawShapeModel.PlotShapeModel(graphics, ((Control)picAst).get_Width(), ((Control)picAst).get_Height(), BlackWhiteModel: false, PlotModelDark: false, @checked, background, ShowFaceEdges: false, chkAxesOfRotation.get_Checked(), optV2.get_Checked());
				picAst.set_Image((Image)image);
				graphics.Dispose();
				Application.DoEvents();
				if (chkAnimate.get_Checked() & chkAnimatedGif.get_Checked())
				{
					string text = Utilities.AppPath + "\\Predictions\\AnimatedGIF\\" + num3 + ".gif";
					try
					{
						picAst.get_Image().Save(text, ImageFormat.Gif);
						list.Add(text);
						num3++;
					}
					catch
					{
					}
				}
				if (num4 == 0.0)
				{
					((Control)lblVol).set_Text(string.Format("{0,5:f3} = {1,5:f1}", DrawShapeModel.Volume_Equiv_Radius_OfModel, DrawShapeModel.Vol_Equiv_Radius_OnPlot * 2.0));
					((Control)lblSurf).set_Text(string.Format("{0,5:f3} = {1,5:f1}", DrawShapeModel.Surface_Equiv_Radius_OfModel, DrawShapeModel.Surf_Equiv_Radius_OnPlot * 2.0));
					StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\generated Files\\SurfVol.txt", append: true);
					streamWriter.WriteLine(string.Format("{0,5:f3}", DrawShapeModel.Surface_Equiv_Radius_OfModel / DrawShapeModel.Volume_Equiv_Radius_OfModel));
					streamWriter.Close();
				}
				Application.DoEvents();
			}
			((Control)lblPAaxis).set_Text(string.Format("PA {0,1:f1}°, Alt {1,1:f1}°", DrawShapeModel.PAofAxis, DrawShapeModel.AngleOfAxisAboveSkyPlane));
			if (chkAnimate.get_Checked() & chkAnimatedGif.get_Checked())
			{
				string text2 = "(" + asteroidNumber + ") " + updnYear.get_Value() + updnMonth.get_Value().ToString().PadLeft(2, '_') + updnDay.get_Value().ToString().PadLeft(2, '_') + updnHour.get_Value().ToString().PadLeft(2, '_') + updnMinute.get_Value().ToString().PadLeft(2, '_');
				global::GifCreator.GifCreator.Create_Animated_Gif(list, Utilities.AppPath + "\\Asteroids\\" + text2 + ".gif");
			}
		}

		private void cmdDownloadAll_Click(object sender, EventArgs e)
		{
			GetShapeModelData.DownloadAllDAMITModels();
		}

		private void trackTransparency_Scroll(object sender, EventArgs e)
		{
			((Form)this).set_Opacity((double)trackTransparency.get_Value() / 100.0);
		}

		private void updnYear_ValueChanged(object sender, EventArgs e)
		{
			DisplayJD();
		}

		private void updnMonth_ValueChanged(object sender, EventArgs e)
		{
			DisplayJD();
		}

		private void updnDay_ValueChanged(object sender, EventArgs e)
		{
			DisplayJD();
		}

		private void DisplayJD()
		{
			((Control)lblJD).set_Text("JD at 12hrs = " + (long)Utilities.JD_from_Date((int)updnYear.get_Value(), (int)updnMonth.get_Value(), (double)updnDay.get_Value() + 0.5));
			PlotModel();
		}

		private void chk300_CheckedChanged(object sender, EventArgs e)
		{
			SetNewSize();
			PlotModel();
		}

		private void chk600_CheckedChanged(object sender, EventArgs e)
		{
			SetNewSize();
			PlotModel();
		}

		private void optBlack_CheckedChanged(object sender, EventArgs e)
		{
			PlotModel();
		}

		private void optWhite_CheckedChanged(object sender, EventArgs e)
		{
			PlotModel();
		}

		private void optLightBlue_CheckedChanged(object sender, EventArgs e)
		{
			PlotModel();
		}

		private void optSky_CheckedChanged(object sender, EventArgs e)
		{
			PlotModel();
		}

		private void optEarth_CheckedChanged(object sender, EventArgs e)
		{
			PlotModel();
		}

		private void optModel1_CheckedChanged(object sender, EventArgs e)
		{
			PlotModel();
		}

		private void panel1_Paint(object sender, PaintEventArgs e)
		{
			PlotModel();
		}

		private void optModel3_CheckedChanged(object sender, EventArgs e)
		{
			PlotModel();
		}

		private void optModel4_CheckedChanged(object sender, EventArgs e)
		{
			PlotModel();
		}

		private void Shapes_Resize(object sender, EventArgs e)
		{
			SetNewSize();
		}

		private void SetNewSize()
		{
			if (chk300.get_Checked())
			{
				PictureBox obj = picAst;
				int width;
				((Control)picAst).set_Height(width = 300);
				((Control)obj).set_Width(width);
			}
			else if (chk600.get_Checked())
			{
				PictureBox obj2 = picAst;
				int width;
				((Control)picAst).set_Height(width = 600);
				((Control)obj2).set_Width(width);
			}
			else
			{
				if (((Control)this).get_Width() > 350)
				{
					((Control)picAst).set_Width(((Control)this).get_Width() - 362);
				}
				if (((Control)this).get_Height() > 700)
				{
					((Control)picAst).set_Height(((Control)this).get_Height() - 107);
				}
			}
			((Control)picResize).set_Left(((Control)this).get_Width() - 41);
			((Control)picResize).set_Top(((Control)this).get_Height() - 64);
			PlotModel();
		}

		private void optV2_CheckedChanged(object sender, EventArgs e)
		{
			PlotModel();
		}

		private void optV3_CheckedChanged(object sender, EventArgs e)
		{
			PlotModel();
		}

		private void optModel2_CheckedChanged(object sender, EventArgs e)
		{
			PlotModel();
		}

		private void optModel3_CheckedChanged_1(object sender, EventArgs e)
		{
			PlotModel();
		}

		private void optModel4_CheckedChanged_1(object sender, EventArgs e)
		{
			PlotModel();
		}

		private void optModel5_CheckedChanged(object sender, EventArgs e)
		{
			PlotModel();
		}

		private void optModel6_CheckedChanged(object sender, EventArgs e)
		{
			PlotModel();
		}

		private void optModelI1_CheckedChanged(object sender, EventArgs e)
		{
			PlotModel();
		}

		private void optModelI2_CheckedChanged(object sender, EventArgs e)
		{
			PlotModel();
		}

		private void optModelI3_CheckedChanged(object sender, EventArgs e)
		{
			PlotModel();
		}

		private void optModelI4_CheckedChanged(object sender, EventArgs e)
		{
			PlotModel();
		}

		private void optModelI5_CheckedChanged(object sender, EventArgs e)
		{
			PlotModel();
		}

		private void optModelI6_CheckedChanged(object sender, EventArgs e)
		{
			PlotModel();
		}

		private void cmdManuallyAdd_Click(object sender, EventArgs e)
		{
			GetShapeModelData.ManuallyAddToISAM();
		}

		private void cmdAll_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < lstAsteroids.get_Items().get_Count(); i++)
			{
				((ListControl)lstAsteroids).set_SelectedIndex(i);
				PlotModel();
			}
		}

		private void cmdHelp_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"ShapeModelDemo");
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
			//IL_10d6: Unknown result type (might be due to invalid IL or missing references)
			//IL_10e0: Expected O, but got Unknown
			cmdModelList = new Button();
			lstAsteroids = new ListBox();
			optModel1 = new RadioButton();
			optModel4 = new RadioButton();
			optModel3 = new RadioButton();
			optModel2 = new RadioButton();
			optModel6 = new RadioButton();
			optModel5 = new RadioButton();
			cmdPlot = new Button();
			label1 = new Label();
			label2 = new Label();
			updnYear = new NumericUpDown();
			updnMonth = new NumericUpDown();
			updnDay = new NumericUpDown();
			updnHour = new NumericUpDown();
			updnMinute = new NumericUpDown();
			label3 = new Label();
			label4 = new Label();
			label5 = new Label();
			label6 = new Label();
			label7 = new Label();
			chkAnimate = new CheckBox();
			panel1 = new Panel();
			optModel8 = new RadioButton();
			optModel7 = new RadioButton();
			optModel10 = new RadioButton();
			optModel9 = new RadioButton();
			optModel12 = new RadioButton();
			optModel11 = new RadioButton();
			groupBox1 = new GroupBox();
			optLightBlue = new RadioButton();
			optWhite = new RadioButton();
			optBlack = new RadioButton();
			label8 = new Label();
			label9 = new Label();
			lblVol = new Label();
			lblSurf = new Label();
			label10 = new Label();
			cmdDownloadAll = new Button();
			picResize = new PictureBox();
			picAst = new PictureBox();
			label11 = new Label();
			label12 = new Label();
			lblPlotWidth = new Label();
			groupBox3 = new GroupBox();
			optEarth = new RadioButton();
			optSky = new RadioButton();
			lblQuality = new Label();
			lblVersion = new Label();
			lblComment = new Label();
			lblCreated = new Label();
			lblModified = new Label();
			trackTransparency = new TrackBar();
			chk300 = new CheckBox();
			chk600 = new CheckBox();
			label13 = new Label();
			groupBox2 = new GroupBox();
			optV3 = new RadioButton();
			optV2 = new RadioButton();
			lblJD = new Label();
			chkAxesOfRotation = new CheckBox();
			cmdManuallyAdd = new Button();
			cmdAll = new Button();
			cmdHelp = new Button();
			chkAnimatedGif = new CheckBox();
			lblPAaxis = new Label();
			((ISupportInitialize)updnYear).BeginInit();
			((ISupportInitialize)updnMonth).BeginInit();
			((ISupportInitialize)updnDay).BeginInit();
			((ISupportInitialize)updnHour).BeginInit();
			((ISupportInitialize)updnMinute).BeginInit();
			((Control)panel1).SuspendLayout();
			((Control)groupBox1).SuspendLayout();
			((ISupportInitialize)picResize).BeginInit();
			((ISupportInitialize)picAst).BeginInit();
			((Control)groupBox3).SuspendLayout();
			((ISupportInitialize)trackTransparency).BeginInit();
			((Control)groupBox2).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)cmdModelList).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdModelList).set_Location(new Point(2, 616));
			((Control)cmdModelList).set_Name("cmdModelList");
			((Control)cmdModelList).set_Size(new Size(91, 45));
			((Control)cmdModelList).set_TabIndex(1);
			((Control)cmdModelList).set_Text("Update list \r\nof models ");
			((ButtonBase)cmdModelList).set_UseVisualStyleBackColor(true);
			((Control)cmdModelList).add_Click((EventHandler)cmdModelList_Click);
			((Control)lstAsteroids).set_Font(new Font("Courier New", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstAsteroids).set_FormattingEnabled(true);
			lstAsteroids.set_ItemHeight(15);
			((Control)lstAsteroids).set_Location(new Point(2, 33));
			((Control)lstAsteroids).set_Name("lstAsteroids");
			lstAsteroids.set_ScrollAlwaysVisible(true);
			((Control)lstAsteroids).set_Size(new Size(83, 409));
			lstAsteroids.set_Sorted(true);
			((Control)lstAsteroids).set_TabIndex(3);
			lstAsteroids.add_SelectedIndexChanged((EventHandler)lstAsteroids_SelectedIndexChanged);
			((Control)optModel1).set_AutoSize(true);
			((Control)optModel1).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optModel1).set_Location(new Point(5, 25));
			((Control)optModel1).set_Name("optModel1");
			((Control)optModel1).set_Size(new Size(28, 16));
			((Control)optModel1).set_TabIndex(4);
			optModel1.set_TabStop(true);
			((Control)optModel1).set_Text("1");
			((ButtonBase)optModel1).set_UseVisualStyleBackColor(true);
			optModel1.add_CheckedChanged((EventHandler)optModel1_CheckedChanged);
			((Control)optModel4).set_AutoSize(true);
			((Control)optModel4).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optModel4).set_Location(new Point(5, 94));
			((Control)optModel4).set_Name("optModel4");
			((Control)optModel4).set_Size(new Size(28, 16));
			((Control)optModel4).set_TabIndex(5);
			optModel4.set_TabStop(true);
			((Control)optModel4).set_Text("4");
			((ButtonBase)optModel4).set_UseVisualStyleBackColor(true);
			optModel4.add_CheckedChanged((EventHandler)optModel4_CheckedChanged_1);
			((Control)optModel3).set_AutoSize(true);
			((Control)optModel3).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optModel3).set_Location(new Point(5, 71));
			((Control)optModel3).set_Name("optModel3");
			((Control)optModel3).set_Size(new Size(28, 16));
			((Control)optModel3).set_TabIndex(6);
			optModel3.set_TabStop(true);
			((Control)optModel3).set_Text("3");
			((ButtonBase)optModel3).set_UseVisualStyleBackColor(true);
			optModel3.add_CheckedChanged((EventHandler)optModel3_CheckedChanged_1);
			((Control)optModel2).set_AutoSize(true);
			((Control)optModel2).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optModel2).set_Location(new Point(5, 48));
			((Control)optModel2).set_Name("optModel2");
			((Control)optModel2).set_Size(new Size(28, 16));
			((Control)optModel2).set_TabIndex(7);
			optModel2.set_TabStop(true);
			((Control)optModel2).set_Text("2");
			((ButtonBase)optModel2).set_UseVisualStyleBackColor(true);
			optModel2.add_CheckedChanged((EventHandler)optModel2_CheckedChanged);
			((Control)optModel6).set_AutoSize(true);
			((Control)optModel6).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optModel6).set_Location(new Point(5, 140));
			((Control)optModel6).set_Name("optModel6");
			((Control)optModel6).set_Size(new Size(28, 16));
			((Control)optModel6).set_TabIndex(8);
			optModel6.set_TabStop(true);
			((Control)optModel6).set_Text("6");
			((ButtonBase)optModel6).set_UseVisualStyleBackColor(true);
			optModel6.add_CheckedChanged((EventHandler)optModel6_CheckedChanged);
			((Control)optModel5).set_AutoSize(true);
			((Control)optModel5).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optModel5).set_Location(new Point(5, 117));
			((Control)optModel5).set_Name("optModel5");
			((Control)optModel5).set_Size(new Size(28, 16));
			((Control)optModel5).set_TabIndex(9);
			optModel5.set_TabStop(true);
			((Control)optModel5).set_Text("5");
			((ButtonBase)optModel5).set_UseVisualStyleBackColor(true);
			optModel5.add_CheckedChanged((EventHandler)optModel5_CheckedChanged);
			((Control)cmdPlot).set_Location(new Point(144, 326));
			((Control)cmdPlot).set_Name("cmdPlot");
			((Control)cmdPlot).set_Size(new Size(128, 29));
			((Control)cmdPlot).set_TabIndex(13);
			((Control)cmdPlot).set_Text("Plot");
			((ButtonBase)cmdPlot).set_UseVisualStyleBackColor(true);
			((Control)cmdPlot).add_Click((EventHandler)cmdPlot_Click);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(3, 17));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(77, 13));
			((Control)label1).set_TabIndex(14);
			((Control)label1).set_Text("Select asteroid");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(9, 6));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(79, 15));
			((Control)label2).set_TabIndex(15);
			((Control)label2).set_Text("Select model");
			((Control)updnYear).set_Location(new Point(99, 67));
			updnYear.set_Maximum(new decimal(new int[4] { 2100, 0, 0, 0 }));
			updnYear.set_Minimum(new decimal(new int[4] { 1900, 0, 0, 0 }));
			((Control)updnYear).set_Name("updnYear");
			((Control)updnYear).set_Size(new Size(55, 20));
			((Control)updnYear).set_TabIndex(16);
			updnYear.set_Value(new decimal(new int[4] { 2020, 0, 0, 0 }));
			updnYear.add_ValueChanged((EventHandler)updnYear_ValueChanged);
			((Control)updnMonth).set_Location(new Point(167, 67));
			updnMonth.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnMonth.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnMonth).set_Name("updnMonth");
			((Control)updnMonth).set_Size(new Size(45, 20));
			((Control)updnMonth).set_TabIndex(17);
			updnMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnMonth.add_ValueChanged((EventHandler)updnMonth_ValueChanged);
			((Control)updnDay).set_Location(new Point(232, 67));
			updnDay.set_Maximum(new decimal(new int[4] { 31, 0, 0, 0 }));
			updnDay.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnDay).set_Name("updnDay");
			((Control)updnDay).set_Size(new Size(40, 20));
			((Control)updnDay).set_TabIndex(18);
			updnDay.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnDay.add_ValueChanged((EventHandler)updnDay_ValueChanged);
			((Control)updnHour).set_Location(new Point(99, 125));
			updnHour.set_Maximum(new decimal(new int[4] { 23, 0, 0, 0 }));
			((Control)updnHour).set_Name("updnHour");
			((Control)updnHour).set_Size(new Size(41, 20));
			((Control)updnHour).set_TabIndex(19);
			updnHour.set_Value(new decimal(new int[4] { 12, 0, 0, 0 }));
			((Control)updnMinute).set_Location(new Point(165, 124));
			updnMinute.set_Maximum(new decimal(new int[4] { 599, 0, 0, 65536 }));
			((Control)updnMinute).set_Name("updnMinute");
			((Control)updnMinute).set_Size(new Size(59, 20));
			((Control)updnMinute).set_TabIndex(20);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(102, 52));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(29, 13));
			((Control)label3).set_TabIndex(21);
			((Control)label3).set_Text("Year");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(170, 52));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(37, 13));
			((Control)label4).set_TabIndex(22);
			((Control)label4).set_Text("Month");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(235, 52));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(26, 13));
			((Control)label5).set_TabIndex(23);
			((Control)label5).set_Text("Day");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(102, 110));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(30, 13));
			((Control)label6).set_TabIndex(24);
			((Control)label6).set_Text("Hour");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(168, 109));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(39, 13));
			((Control)label7).set_TabIndex(25);
			((Control)label7).set_Text("Minute");
			((Control)chkAnimate).set_AutoSize(true);
			((Control)chkAnimate).set_Location(new Point(100, 153));
			((Control)chkAnimate).set_Name("chkAnimate");
			((Control)chkAnimate).set_Size(new Size(230, 17));
			((Control)chkAnimate).set_TabIndex(26);
			((Control)chkAnimate).set_Text("Display 1- degree animation for one rotation");
			((ButtonBase)chkAnimate).set_UseVisualStyleBackColor(true);
			((Control)panel1).get_Controls().Add((Control)(object)optModel8);
			((Control)panel1).get_Controls().Add((Control)(object)optModel7);
			((Control)panel1).get_Controls().Add((Control)(object)optModel10);
			((Control)panel1).get_Controls().Add((Control)(object)optModel9);
			((Control)panel1).get_Controls().Add((Control)(object)optModel12);
			((Control)panel1).get_Controls().Add((Control)(object)optModel11);
			((Control)panel1).get_Controls().Add((Control)(object)optModel2);
			((Control)panel1).get_Controls().Add((Control)(object)optModel1);
			((Control)panel1).get_Controls().Add((Control)(object)optModel4);
			((Control)panel1).get_Controls().Add((Control)(object)optModel3);
			((Control)panel1).get_Controls().Add((Control)(object)optModel6);
			((Control)panel1).get_Controls().Add((Control)(object)optModel5);
			((Control)panel1).get_Controls().Add((Control)(object)label2);
			((Control)panel1).set_Location(new Point(1, 446));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(118, 168));
			((Control)panel1).set_TabIndex(27);
			((Control)panel1).add_Paint(new PaintEventHandler(panel1_Paint));
			((Control)optModel8).set_AutoSize(true);
			((Control)optModel8).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optModel8).set_Location(new Point(62, 48));
			((Control)optModel8).set_Name("optModel8");
			((Control)optModel8).set_Size(new Size(43, 16));
			((Control)optModel8).set_TabIndex(19);
			optModel8.set_TabStop(true);
			((Control)optModel8).set_Text("A1-2");
			((ButtonBase)optModel8).set_UseVisualStyleBackColor(true);
			optModel8.add_CheckedChanged((EventHandler)optModelI2_CheckedChanged);
			((Control)optModel7).set_AutoSize(true);
			((Control)optModel7).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optModel7).set_Location(new Point(62, 25));
			((Control)optModel7).set_Name("optModel7");
			((Control)optModel7).set_Size(new Size(43, 16));
			((Control)optModel7).set_TabIndex(16);
			optModel7.set_TabStop(true);
			((Control)optModel7).set_Text("A1-1");
			((ButtonBase)optModel7).set_UseVisualStyleBackColor(true);
			optModel7.add_CheckedChanged((EventHandler)optModelI1_CheckedChanged);
			((Control)optModel10).set_AutoSize(true);
			((Control)optModel10).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optModel10).set_Location(new Point(62, 94));
			((Control)optModel10).set_Name("optModel10");
			((Control)optModel10).set_Size(new Size(43, 16));
			((Control)optModel10).set_TabIndex(17);
			optModel10.set_TabStop(true);
			((Control)optModel10).set_Text("A1-4");
			((ButtonBase)optModel10).set_UseVisualStyleBackColor(true);
			optModel10.add_CheckedChanged((EventHandler)optModelI4_CheckedChanged);
			((Control)optModel9).set_AutoSize(true);
			((Control)optModel9).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optModel9).set_Location(new Point(62, 71));
			((Control)optModel9).set_Name("optModel9");
			((Control)optModel9).set_Size(new Size(43, 16));
			((Control)optModel9).set_TabIndex(18);
			optModel9.set_TabStop(true);
			((Control)optModel9).set_Text("A1-3");
			((ButtonBase)optModel9).set_UseVisualStyleBackColor(true);
			optModel9.add_CheckedChanged((EventHandler)optModelI3_CheckedChanged);
			((Control)optModel12).set_AutoSize(true);
			((Control)optModel12).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optModel12).set_Location(new Point(62, 140));
			((Control)optModel12).set_Name("optModel12");
			((Control)optModel12).set_Size(new Size(43, 16));
			((Control)optModel12).set_TabIndex(20);
			optModel12.set_TabStop(true);
			((Control)optModel12).set_Text("A1-6");
			((ButtonBase)optModel12).set_UseVisualStyleBackColor(true);
			optModel12.add_CheckedChanged((EventHandler)optModelI6_CheckedChanged);
			((Control)optModel11).set_AutoSize(true);
			((Control)optModel11).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optModel11).set_Location(new Point(62, 117));
			((Control)optModel11).set_Name("optModel11");
			((Control)optModel11).set_Size(new Size(43, 16));
			((Control)optModel11).set_TabIndex(21);
			optModel11.set_TabStop(true);
			((Control)optModel11).set_Text("A1-5");
			((ButtonBase)optModel11).set_UseVisualStyleBackColor(true);
			optModel11.add_CheckedChanged((EventHandler)optModelI5_CheckedChanged);
			((Control)groupBox1).get_Controls().Add((Control)(object)optLightBlue);
			((Control)groupBox1).get_Controls().Add((Control)(object)optWhite);
			((Control)groupBox1).get_Controls().Add((Control)(object)optBlack);
			((Control)groupBox1).set_Location(new Point(100, 189));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(210, 48));
			((Control)groupBox1).set_TabIndex(28);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Background");
			((Control)optLightBlue).set_AutoSize(true);
			((Control)optLightBlue).set_Location(new Point(129, 21));
			((Control)optLightBlue).set_Name("optLightBlue");
			((Control)optLightBlue).set_Size(new Size(69, 17));
			((Control)optLightBlue).set_TabIndex(2);
			optLightBlue.set_TabStop(true);
			((Control)optLightBlue).set_Text("LightBlue");
			((ButtonBase)optLightBlue).set_UseVisualStyleBackColor(true);
			optLightBlue.add_CheckedChanged((EventHandler)optLightBlue_CheckedChanged);
			((Control)optWhite).set_AutoSize(true);
			((Control)optWhite).set_Location(new Point(68, 21));
			((Control)optWhite).set_Name("optWhite");
			((Control)optWhite).set_Size(new Size(53, 17));
			((Control)optWhite).set_TabIndex(1);
			optWhite.set_TabStop(true);
			((Control)optWhite).set_Text("White");
			((ButtonBase)optWhite).set_UseVisualStyleBackColor(true);
			optWhite.add_CheckedChanged((EventHandler)optWhite_CheckedChanged);
			((Control)optBlack).set_AutoSize(true);
			optBlack.set_Checked(true);
			((Control)optBlack).set_Location(new Point(8, 21));
			((Control)optBlack).set_Name("optBlack");
			((Control)optBlack).set_Size(new Size(52, 17));
			((Control)optBlack).set_TabIndex(0);
			optBlack.set_TabStop(true);
			((Control)optBlack).set_Text("Black");
			((ButtonBase)optBlack).set_UseVisualStyleBackColor(true);
			optBlack.add_CheckedChanged((EventHandler)optBlack_CheckedChanged);
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(96, 413));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(137, 13));
			((Control)label8).set_TabIndex(29);
			((Control)label8).set_Text("Volume-equivalent diameter");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(94, 431));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(139, 13));
			((Control)label9).set_TabIndex(30);
			((Control)label9).set_Text("Surface-equivalent diameter");
			((Control)lblVol).set_AutoSize(true);
			((Control)lblVol).set_Location(new Point(239, 413));
			((Control)lblVol).set_Name("lblVol");
			((Control)lblVol).set_Size(new Size(52, 13));
			((Control)lblVol).set_TabIndex(31);
			((Control)lblVol).set_Text("1.000 = 0");
			((Control)lblSurf).set_AutoSize(true);
			((Control)lblSurf).set_Location(new Point(239, 431));
			((Control)lblSurf).set_Name("lblSurf");
			((Control)lblSurf).set_Size(new Size(52, 13));
			((Control)lblSurf).set_TabIndex(32);
			((Control)lblSurf).set_Text("1.000 = 0");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(223, 384));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(51, 26));
			((Control)label10).set_TabIndex(33);
			((Control)label10).set_Text("... of plot \r\nwidth");
			label10.set_TextAlign(ContentAlignment.BottomRight);
			((Control)cmdDownloadAll).set_Location(new Point(139, 617));
			((Control)cmdDownloadAll).set_Name("cmdDownloadAll");
			((Control)cmdDownloadAll).set_Size(new Size(112, 43));
			((Control)cmdDownloadAll).set_TabIndex(34);
			((Control)cmdDownloadAll).set_Text("Download all models");
			((ButtonBase)cmdDownloadAll).set_UseVisualStyleBackColor(true);
			((Control)cmdDownloadAll).add_Click((EventHandler)cmdDownloadAll_Click);
			picResize.set_Image((Image)Resources.ResizeGrip_26x);
			((Control)picResize).set_Location(new Point(921, 643));
			((Control)picResize).set_Name("picResize");
			((Control)picResize).set_Size(new Size(26, 26));
			picResize.set_TabIndex(35);
			picResize.set_TabStop(false);
			((Control)picAst).set_BackColor(Color.Black);
			picAst.set_BorderStyle((BorderStyle)1);
			((Control)picAst).set_Location(new Point(334, 33));
			((Control)picAst).set_Name("picAst");
			((Control)picAst).set_Size(new Size(600, 600));
			picAst.set_TabIndex(10);
			picAst.set_TabStop(false);
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Location(new Point(279, 397));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(33, 13));
			((Control)label11).set_TabIndex(36);
			((Control)label11).set_Text("pixels");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Location(new Point(96, 369));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(88, 13));
			((Control)label12).set_TabIndex(37);
			((Control)label12).set_Text("Plot width (pixels)");
			((Control)lblPlotWidth).set_AutoSize(true);
			((Control)lblPlotWidth).set_Location(new Point(191, 369));
			((Control)lblPlotWidth).set_Name("lblPlotWidth");
			((Control)lblPlotWidth).set_Size(new Size(13, 13));
			((Control)lblPlotWidth).set_TabIndex(38);
			((Control)lblPlotWidth).set_Text("0");
			((Control)groupBox3).get_Controls().Add((Control)(object)optEarth);
			((Control)groupBox3).get_Controls().Add((Control)(object)optSky);
			((Control)groupBox3).set_Location(new Point(100, 241));
			((Control)groupBox3).set_Name("groupBox3");
			((Control)groupBox3).set_Size(new Size(210, 49));
			((Control)groupBox3).set_TabIndex(39);
			groupBox3.set_TabStop(false);
			((Control)groupBox3).set_Text("Projection");
			((Control)optEarth).set_AutoSize(true);
			((Control)optEarth).set_Location(new Point(100, 20));
			((Control)optEarth).set_Name("optEarth");
			((Control)optEarth).set_Size(new Size(80, 17));
			((Control)optEarth).set_TabIndex(1);
			optEarth.set_TabStop(true);
			((Control)optEarth).set_Text("Earth Plane");
			((ButtonBase)optEarth).set_UseVisualStyleBackColor(true);
			optEarth.add_CheckedChanged((EventHandler)optEarth_CheckedChanged);
			((Control)optSky).set_AutoSize(true);
			optSky.set_Checked(true);
			((Control)optSky).set_Location(new Point(9, 20));
			((Control)optSky).set_Name("optSky");
			((Control)optSky).set_Size(new Size(72, 17));
			((Control)optSky).set_TabIndex(0);
			optSky.set_TabStop(true);
			((Control)optSky).set_Text("Sky plane");
			((ButtonBase)optSky).set_UseVisualStyleBackColor(true);
			optSky.add_CheckedChanged((EventHandler)optSky_CheckedChanged);
			((Control)lblQuality).set_AutoSize(true);
			((Control)lblQuality).set_Location(new Point(120, 460));
			((Control)lblQuality).set_Name("lblQuality");
			((Control)lblQuality).set_Size(new Size(51, 13));
			((Control)lblQuality).set_TabIndex(40);
			((Control)lblQuality).set_Text("Quality = ");
			((Control)lblVersion).set_AutoSize(true);
			((Control)lblVersion).set_Location(new Point(120, 481));
			((Control)lblVersion).set_Name("lblVersion");
			((Control)lblVersion).set_Size(new Size(54, 13));
			((Control)lblVersion).set_TabIndex(41);
			((Control)lblVersion).set_Text("Version = ");
			((Control)lblComment).set_Location(new Point(120, 502));
			((Control)lblComment).set_Name("lblComment");
			((Control)lblComment).set_Size(new Size(214, 42));
			((Control)lblComment).set_TabIndex(42);
			((Control)lblComment).set_Text("Comment =");
			((Control)lblCreated).set_AutoSize(true);
			((Control)lblCreated).set_Location(new Point(120, 546));
			((Control)lblCreated).set_Name("lblCreated");
			((Control)lblCreated).set_Size(new Size(47, 13));
			((Control)lblCreated).set_TabIndex(43);
			((Control)lblCreated).set_Text("Created ");
			((Control)lblModified).set_AutoSize(true);
			((Control)lblModified).set_Location(new Point(120, 567));
			((Control)lblModified).set_Name("lblModified");
			((Control)lblModified).set_Size(new Size(50, 13));
			((Control)lblModified).set_TabIndex(44);
			((Control)lblModified).set_Text("Modified ");
			((Control)trackTransparency).set_AutoSize(false);
			((Control)trackTransparency).set_Location(new Point(114, 12));
			trackTransparency.set_Maximum(100);
			trackTransparency.set_Minimum(50);
			((Control)trackTransparency).set_Name("trackTransparency");
			((Control)trackTransparency).set_Size(new Size(170, 25));
			((Control)trackTransparency).set_TabIndex(45);
			trackTransparency.set_TickFrequency(5);
			trackTransparency.set_Value(100);
			trackTransparency.add_Scroll((EventHandler)trackTransparency_Scroll);
			((Control)chk300).set_AutoSize(true);
			((Control)chk300).set_Location(new Point(120, 589));
			((Control)chk300).set_Name("chk300");
			((Control)chk300).set_Size(new Size(73, 17));
			((Control)chk300).set_TabIndex(46);
			((Control)chk300).set_Text("300 pixels");
			((ButtonBase)chk300).set_UseVisualStyleBackColor(true);
			chk300.add_CheckedChanged((EventHandler)chk300_CheckedChanged);
			((Control)chk600).set_AutoSize(true);
			((Control)chk600).set_Location(new Point(189, 589));
			((Control)chk600).set_Name("chk600");
			((Control)chk600).set_Size(new Size(73, 17));
			((Control)chk600).set_TabIndex(47);
			((Control)chk600).set_Text("600 pixels");
			((ButtonBase)chk600).set_UseVisualStyleBackColor(true);
			chk600.add_CheckedChanged((EventHandler)chk600_CheckedChanged);
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Location(new Point(165, 4));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(69, 13));
			((Control)label13).set_TabIndex(48);
			((Control)label13).set_Text("Form Opacity");
			((Control)groupBox2).get_Controls().Add((Control)(object)optV3);
			((Control)groupBox2).get_Controls().Add((Control)(object)optV2);
			((Control)groupBox2).set_Location(new Point(260, 539));
			((Control)groupBox2).set_Name("groupBox2");
			((Control)groupBox2).set_Size(new Size(57, 57));
			((Control)groupBox2).set_TabIndex(49);
			groupBox2.set_TabStop(false);
			((Control)groupBox2).set_Text("DAMIT");
			((Control)optV3).set_AutoSize(true);
			optV3.set_Checked(true);
			((Control)optV3).set_Location(new Point(6, 35));
			((Control)optV3).set_Name("optV3");
			((Control)optV3).set_Size(new Size(38, 17));
			((Control)optV3).set_TabIndex(1);
			optV3.set_TabStop(true);
			((Control)optV3).set_Text("V3");
			((ButtonBase)optV3).set_UseVisualStyleBackColor(true);
			optV3.add_CheckedChanged((EventHandler)optV3_CheckedChanged);
			((Control)optV2).set_AutoSize(true);
			((Control)optV2).set_Location(new Point(6, 16));
			((Control)optV2).set_Name("optV2");
			((Control)optV2).set_Size(new Size(38, 17));
			((Control)optV2).set_TabIndex(0);
			((Control)optV2).set_Text("V2");
			((ButtonBase)optV2).set_UseVisualStyleBackColor(true);
			optV2.add_CheckedChanged((EventHandler)optV2_CheckedChanged);
			((Control)lblJD).set_AutoSize(true);
			((Control)lblJD).set_Location(new Point(206, 95));
			((Control)lblJD).set_Name("lblJD");
			((Control)lblJD).set_Size(new Size(74, 13));
			((Control)lblJD).set_TabIndex(50);
			((Control)lblJD).set_Text("JD = 2450520");
			((Control)chkAxesOfRotation).set_AutoSize(true);
			chkAxesOfRotation.set_Checked(true);
			chkAxesOfRotation.set_CheckState((CheckState)1);
			((Control)chkAxesOfRotation).set_Location(new Point(96, 296));
			((Control)chkAxesOfRotation).set_Name("chkAxesOfRotation");
			((Control)chkAxesOfRotation).set_Size(new Size(136, 17));
			((Control)chkAxesOfRotation).set_TabIndex(51);
			((Control)chkAxesOfRotation).set_Text("Include axes of rotation");
			((ButtonBase)chkAxesOfRotation).set_UseVisualStyleBackColor(true);
			((Control)cmdManuallyAdd).set_Location(new Point(271, 636));
			((Control)cmdManuallyAdd).set_Name("cmdManuallyAdd");
			((Control)cmdManuallyAdd).set_Size(new Size(100, 24));
			((Control)cmdManuallyAdd).set_TabIndex(52);
			((Control)cmdManuallyAdd).set_Text("Manually add model");
			((ButtonBase)cmdManuallyAdd).set_UseVisualStyleBackColor(true);
			((Control)cmdManuallyAdd).add_Click((EventHandler)cmdManuallyAdd_Click);
			((Control)cmdAll).set_Location(new Point(382, 7));
			((Control)cmdAll).set_Name("cmdAll");
			((Control)cmdAll).set_Size(new Size(213, 20));
			((Control)cmdAll).set_TabIndex(55);
			((Control)cmdAll).set_Text("Generate file of Surface/Volume ratios");
			((ButtonBase)cmdAll).set_UseVisualStyleBackColor(true);
			((Control)cmdAll).add_Click((EventHandler)cmdAll_Click);
			((Control)cmdHelp).set_BackColor(SystemColors.Control);
			((ButtonBase)cmdHelp).set_Image((Image)Resources.help);
			((Control)cmdHelp).set_Location(new Point(783, 1));
			((Control)cmdHelp).set_Name("cmdHelp");
			((Control)cmdHelp).set_Size(new Size(30, 28));
			((Control)cmdHelp).set_TabIndex(56);
			((ButtonBase)cmdHelp).set_UseVisualStyleBackColor(false);
			((Control)cmdHelp).add_Click((EventHandler)cmdHelp_Click);
			((Control)chkAnimatedGif).set_AutoSize(true);
			((Control)chkAnimatedGif).set_Location(new Point(119, 168));
			((Control)chkAnimatedGif).set_Name("chkAnimatedGif");
			((Control)chkAnimatedGif).set_Size(new Size(131, 17));
			((Control)chkAnimatedGif).set_TabIndex(57);
			((Control)chkAnimatedGif).set_Text("Save as animated GIF");
			((ButtonBase)chkAnimatedGif).set_UseVisualStyleBackColor(true);
			((Control)lblPAaxis).set_AutoSize(true);
			((Control)lblPAaxis).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblPAaxis).set_Location(new Point(232, 298));
			((Control)lblPAaxis).set_Name("lblPAaxis");
			((Control)lblPAaxis).set_Size(new Size(31, 12));
			((Control)lblPAaxis).set_TabIndex(58);
			((Control)lblPAaxis).set_Text("PA  0°");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(946, 668));
			((Control)this).get_Controls().Add((Control)(object)lblPAaxis);
			((Control)this).get_Controls().Add((Control)(object)chkAnimatedGif);
			((Control)this).get_Controls().Add((Control)(object)cmdHelp);
			((Control)this).get_Controls().Add((Control)(object)cmdAll);
			((Control)this).get_Controls().Add((Control)(object)cmdManuallyAdd);
			((Control)this).get_Controls().Add((Control)(object)chkAxesOfRotation);
			((Control)this).get_Controls().Add((Control)(object)lblJD);
			((Control)this).get_Controls().Add((Control)(object)groupBox2);
			((Control)this).get_Controls().Add((Control)(object)label13);
			((Control)this).get_Controls().Add((Control)(object)chk600);
			((Control)this).get_Controls().Add((Control)(object)chk300);
			((Control)this).get_Controls().Add((Control)(object)trackTransparency);
			((Control)this).get_Controls().Add((Control)(object)lblModified);
			((Control)this).get_Controls().Add((Control)(object)lblCreated);
			((Control)this).get_Controls().Add((Control)(object)lblComment);
			((Control)this).get_Controls().Add((Control)(object)lblVersion);
			((Control)this).get_Controls().Add((Control)(object)lblQuality);
			((Control)this).get_Controls().Add((Control)(object)groupBox3);
			((Control)this).get_Controls().Add((Control)(object)lblPlotWidth);
			((Control)this).get_Controls().Add((Control)(object)label12);
			((Control)this).get_Controls().Add((Control)(object)label11);
			((Control)this).get_Controls().Add((Control)(object)picResize);
			((Control)this).get_Controls().Add((Control)(object)cmdDownloadAll);
			((Control)this).get_Controls().Add((Control)(object)label10);
			((Control)this).get_Controls().Add((Control)(object)lblSurf);
			((Control)this).get_Controls().Add((Control)(object)lblVol);
			((Control)this).get_Controls().Add((Control)(object)label9);
			((Control)this).get_Controls().Add((Control)(object)label8);
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)chkAnimate);
			((Control)this).get_Controls().Add((Control)(object)label7);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)updnMinute);
			((Control)this).get_Controls().Add((Control)(object)updnHour);
			((Control)this).get_Controls().Add((Control)(object)updnDay);
			((Control)this).get_Controls().Add((Control)(object)updnMonth);
			((Control)this).get_Controls().Add((Control)(object)updnYear);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)cmdPlot);
			((Control)this).get_Controls().Add((Control)(object)picAst);
			((Control)this).get_Controls().Add((Control)(object)lstAsteroids);
			((Control)this).get_Controls().Add((Control)(object)cmdModelList);
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)this).set_Name("Shapes");
			((Control)this).set_Text("Test for for for plotting shape models obtained from the  DAMIT v3 system");
			((Form)this).add_Load((EventHandler)Form1_Load);
			((Control)this).add_Resize((EventHandler)Shapes_Resize);
			((ISupportInitialize)updnYear).EndInit();
			((ISupportInitialize)updnMonth).EndInit();
			((ISupportInitialize)updnDay).EndInit();
			((ISupportInitialize)updnHour).EndInit();
			((ISupportInitialize)updnMinute).EndInit();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((ISupportInitialize)picResize).EndInit();
			((ISupportInitialize)picAst).EndInit();
			((Control)groupBox3).ResumeLayout(false);
			((Control)groupBox3).PerformLayout();
			((ISupportInitialize)trackTransparency).EndInit();
			((Control)groupBox2).ResumeLayout(false);
			((Control)groupBox2).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
