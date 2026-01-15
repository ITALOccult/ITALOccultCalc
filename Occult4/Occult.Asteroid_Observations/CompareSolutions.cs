using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Asteroid_Observations
{
	public class CompareSolutions : Form
	{
		private int CurrentExportFileRecord;

		public bool UpdateEllipse;

		public bool UpdateFlags;

		public bool UpdateShapes;

		public bool UpdateDoubles;

		public bool UpdateSatellites;

		private IContainer components;

		private Label label2;

		private Label lblExX;

		private Label label4;

		private Label lblExY;

		private Label label6;

		private Label label7;

		private Label lblEvY;

		private Label lblEvX;

		private Label lblXe;

		private Label lblYe;

		private Label lblPAe;

		private Label lblXh;

		private Label lblYh;

		private Label lblPAh;

		private Label lblXeD;

		private Label lblYeD;

		private Label lblXhD;

		private Label lblYhD;

		private ListBox lstExportShape;

		private ListBox lstHistoryShape;

		private Label labela;

		private Label labelb;

		private Label labelc;

		private Label labeld;

		private Button cmdOK;

		private Button cmdCancel;

		private Label label10;

		private ListBox lstExportDouble;

		private ListBox lstHistoryDouble;

		private Label label11;

		private Label label12;

		private ListBox lstHistorySatellite;

		private ListBox lstExportSatellite;

		private PictureBox picXeYes;

		private PictureBox picXeNo;

		private PictureBox picPAeNo;

		private PictureBox picPAeYes;

		private PictureBox picYeDNo;

		private PictureBox picYeDYes;

		private PictureBox picYeNo;

		private PictureBox picYeYes;

		private PictureBox picXeDNo;

		private PictureBox picXeDYes;

		private Label label14;

		private Label label15;

		private Label label16;

		private Label label17;

		private Label label19;

		private Label label20;

		private Label label31;

		private Label label32;

		private Label label33;

		private Label label34;

		private PictureBox picXhDNo;

		private PictureBox picXhDYes;

		private PictureBox picYhNo;

		private PictureBox picYhYes;

		private PictureBox picYhDNo;

		private PictureBox picYhDYes;

		private PictureBox picPAhNo;

		private PictureBox picPAhYes;

		private PictureBox picXhNo;

		private PictureBox picXhYes;

		private Label label35;

		private Label label36;

		private PictureBox picSepeNo;

		private PictureBox picSepeYes;

		private PictureBox picDbPAeNo;

		private PictureBox picDbPAeYes;

		private Label label37;

		private Label label38;

		private PictureBox picSephNo;

		private PictureBox picSephYes;

		private PictureBox picDbPAhNo;

		private PictureBox picDbPAhYes;

		private GroupBox groupBox1;

		private GroupBox groupBox2;

		private Label label13;

		private PictureBox picSMhNo;

		private PictureBox picSMhYes;

		private Label label1;

		private PictureBox picSMeNo;

		private PictureBox picSMeYes;

		private Label label21;

		private PictureBox picADhNo;

		private PictureBox picADhYes;

		private Label label18;

		private PictureBox picADeNo;

		private PictureBox picADeYes;

		private PictureBox picCeNo;

		private PictureBox picCeYes;

		private PictureBox picChNo;

		private PictureBox picChYes;

		private Label label23;

		private Label label22;

		private Label label25;

		private Label label24;

		private PictureBox picMhNo;

		private PictureBox picMhYes;

		private PictureBox picMeNo;

		private PictureBox picMeYes;

		private Label label26;

		private Label label27;

		private CheckBox chkEllipse;

		private CheckBox chkSolutions;

		private CheckBox chkShapeModels;

		private CheckBox chkDoubles;

		private CheckBox chkSatellite;

		private Label lblExport;

		private Label lblImport;

		private Label label28;

		private Label label29;

		private Label label30;

		private Label lblExportLastEdit;

		private Label lblEditorLastEdit;

		private Label lblAsteroid;

		private Label lblFh;

		private Label lblFith;

		private Label lblFe;

		private Label lblFite;

		public CompareSolutions(int EditRecord, bool ConfirmImport)
		{
			InitializeComponent();
			CurrentExportFileRecord = EditRecord;
			((Control)picXeNo).set_Location(((Control)picXeYes).get_Location());
			((Control)picYeNo).set_Location(((Control)picYeYes).get_Location());
			((Control)picXeDNo).set_Location(((Control)picXeDYes).get_Location());
			((Control)picYeDNo).set_Location(((Control)picYeDYes).get_Location());
			((Control)picPAeNo).set_Location(((Control)picPAeYes).get_Location());
			((Control)picMeNo).set_Location(((Control)picMeYes).get_Location());
			((Control)picSepeNo).set_Location(((Control)picSepeYes).get_Location());
			((Control)picDbPAeNo).set_Location(((Control)picDbPAeYes).get_Location());
			((Control)picSMeNo).set_Location(((Control)picSMeYes).get_Location());
			((Control)picADeNo).set_Location(((Control)picADeYes).get_Location());
			((Control)picCeNo).set_Location(((Control)picCeYes).get_Location());
			((Control)picXhNo).set_Location(((Control)picXhYes).get_Location());
			((Control)picYhNo).set_Location(((Control)picYhYes).get_Location());
			((Control)picXhDNo).set_Location(((Control)picXhDYes).get_Location());
			((Control)picYhDNo).set_Location(((Control)picYhDYes).get_Location());
			((Control)picPAhNo).set_Location(((Control)picPAhYes).get_Location());
			((Control)picMhNo).set_Location(((Control)picMhYes).get_Location());
			((Control)picSephNo).set_Location(((Control)picSephYes).get_Location());
			((Control)picDbPAhNo).set_Location(((Control)picDbPAhYes).get_Location());
			((Control)picSMhNo).set_Location(((Control)picSMhYes).get_Location());
			((Control)picADhNo).set_Location(((Control)picADhYes).get_Location());
			((Control)picChNo).set_Location(((Control)picChYes).get_Location());
			((Control)lblImport).set_Location(((Control)lblExport).get_Location());
			CheckBox obj = chkEllipse;
			CheckBox obj2 = chkSolutions;
			CheckBox obj3 = chkShapeModels;
			CheckBox obj4 = chkDoubles;
			CheckBox obj5 = chkSatellite;
			Label obj6 = lblImport;
			bool flag;
			((Control)label28).set_Visible(flag = ConfirmImport);
			bool flag2;
			((Control)obj6).set_Visible(flag2 = flag);
			bool flag3;
			((Control)obj5).set_Visible(flag3 = flag2);
			bool flag4;
			((Control)obj4).set_Visible(flag4 = flag3);
			bool flag5;
			((Control)obj3).set_Visible(flag5 = flag4);
			bool visible;
			((Control)obj2).set_Visible(visible = flag5);
			((Control)obj).set_Visible(visible);
			CheckBox obj7 = chkShapeModels;
			CheckBox obj8 = chkDoubles;
			chkSatellite.set_Checked(flag5 = false);
			obj8.set_Checked(visible = flag5);
			obj7.set_Checked(visible);
			((Control)lblExport).set_Visible(!ConfirmImport);
			((Control)this).set_Text("Compare solutions for (" + EventDetails.AsteroidNumber + ") " + EventDetails.AsteroidID);
			((Control)lblAsteroid).set_Text("(" + EventDetails.AsteroidNumber + ") " + EventDetails.AsteroidID);
			((Control)lblAsteroid).set_Left((((Control)this).get_Width() - ((Control)lblAsteroid).get_Width()) / 2);
		}

		private void CompareSolutions_Load(object sender, EventArgs e)
		{
			SetValues();
		}

		private void SetValues()
		{
			string Summary = "";
			int SolutionCount = 0;
			string X = "";
			string Y = "";
			string PA_Ellipse = "";
			string X_Dia = "";
			string Y_Dia = "";
			string Quality = "";
			string eX = "";
			string eY = "";
			string eMajor = "";
			string eMinor = "";
			string ePA = "";
			bool UsedAssumedDiameter = false;
			bool FlagForReview = false;
			string text = "Soln    separation             PA";
			lstExportDouble.get_Items().Add((object)text);
			lstHistoryDouble.get_Items().Add((object)text);
			string text2 = "Model         diameter range     Phase    Fit quality";
			lstExportShape.get_Items().Add((object)text2);
			lstHistoryShape.get_Items().Add((object)text2);
			string text3 = "Separation       PA      dimensions      PA    Identifier";
			lstExportSatellite.get_Items().Add((object)text3);
			lstHistorySatellite.get_Items().Add((object)text3);
			for (int i = 0; i < AllEdits.All_Edits[CurrentExportFileRecord].Edits.Count; i++)
			{
				if (AllEdits.All_Edits[CurrentExportFileRecord].Edits[i].Contains(Tags.TagStart[5]))
				{
					AllEvents.Parse_SolveFlags(AllEdits.All_Edits[CurrentExportFileRecord].Edits[i], out var Solve_X, out var Solve_Y, out var Solve_Major, out var Solve_Minor, out var Solve_PA, out var Solve_Circular, out var Inc_Miss, out var Solve_CompanionSep, out var Solve_CompanionPA);
					((Control)picXeNo).set_Visible(!Solve_X);
					((Control)picYeNo).set_Visible(!Solve_Y);
					((Control)picXeDNo).set_Visible(!Solve_Major);
					((Control)picYeDNo).set_Visible(!Solve_Minor);
					((Control)picPAeNo).set_Visible(!Solve_PA);
					((Control)picMeNo).set_Visible(!Inc_Miss);
					((Control)picCeNo).set_Visible(!Solve_Circular);
					((Control)picSepeNo).set_Visible(!Solve_CompanionSep);
					((Control)picDbPAeNo).set_Visible(!Solve_CompanionPA);
				}
				double CentreOfMass_Offset_X;
				double CentreOfMass_Offset_Y;
				if (AllEdits.All_Edits[CurrentExportFileRecord].Edits[i].Contains(Tags.TagStart[6]))
				{
					AllEvents.Parse_EllipticFit(AllEdits.All_Edits[CurrentExportFileRecord].Edits[i], out X, out Y, out X_Dia, out Y_Dia, out PA_Ellipse, out Quality, out UsedAssumedDiameter, out FlagForReview, out CentreOfMass_Offset_X, out CentreOfMass_Offset_Y);
					((Control)lblFe).set_Text(Quality);
					((Control)lblXe).set_Text(X);
					((Control)lblYe).set_Text(Y);
					((Control)lblXeD).set_Text(X_Dia);
					((Control)lblYeD).set_Text(Y_Dia);
					((Control)lblPAe).set_Text(PA_Ellipse);
					chkShapeModels.set_Checked(FlagForReview);
				}
				if (AllEdits.All_Edits[CurrentExportFileRecord].Edits[i].Contains(Tags.TagStart[7]))
				{
					AllEvents.Parse_EllipticUncertainty(AllEdits.All_Edits[CurrentExportFileRecord].Edits[i], out eX, out eY, out eMajor, out eMinor, out ePA);
				}
				if (AllEdits.All_Edits[CurrentExportFileRecord].Edits[i].Contains(Tags.TagStart[9]))
				{
					AllEvents.Parse_ShapeModelFit(AllEdits.All_Edits[CurrentExportFileRecord].Edits[i], out Summary);
					lstExportShape.get_Items().Add((object)Summary);
				}
				if (AllEdits.All_Edits[CurrentExportFileRecord].Edits[i].Contains(Tags.TagStart[10]))
				{
					AllEvents.Parse_Satellite(AllEdits.All_Edits[CurrentExportFileRecord].Edits[i], out Summary, out CentreOfMass_Offset_Y, out CentreOfMass_Offset_X);
					lstExportSatellite.get_Items().Add((object)Summary);
				}
				if (AllEdits.All_Edits[CurrentExportFileRecord].Edits[i].Contains(Tags.TagStart[17]))
				{
					AllEvents.Parse_DoubleStarSolution(AllEdits.All_Edits[CurrentExportFileRecord].Edits[i], ref SolutionCount, out Summary);
					lstExportDouble.get_Items().Add((object)Summary);
				}
				if (AllEdits.All_Edits[CurrentExportFileRecord].Edits[i].Contains(Tags.TagStart[26]))
				{
					AllEvents.Parse_LastEdited(AllEdits.All_Edits[CurrentExportFileRecord].Edits[i], out Summary);
					((Control)lblExportLastEdit).set_Text(Summary);
				}
			}
			((Control)lblEditorLastEdit).set_Text(EventDetails.YearEdited + " " + Utilities.ShortMonths[EventDetails.MonthEdited] + " " + EventDetails.DayEdited.ToString().PadLeft(2, '0'));
			((Control)lblXe).set_Text(X + "  ± " + eX);
			((Control)lblYe).set_Text(Y + "  ± " + eY);
			((Control)lblXeD).set_Text(X_Dia + "  ± " + eMajor);
			((Control)lblYeD).set_Text(Y_Dia + "  ± " + eMinor);
			((Control)lblPAe).set_Text(PA_Ellipse + "  ± " + ePA);
			((Control)picSMeNo).set_Visible(!FlagForReview);
			((Control)picADeNo).set_Visible(!UsedAssumedDiameter);
			((Control)lblFh).set_Text(EventDetails.Quality.ToString());
			((Control)lblXh).set_Text(string.Format("{0,2:f1}  ± {1,2:f1}", EventDetails.X, EventDetails.Sdev_X));
			((Control)lblYh).set_Text(string.Format("{0,2:f1}  ± {1,2:f1}", EventDetails.Y, EventDetails.Sdev_Y));
			((Control)lblXhD).set_Text(string.Format("{0,2:f1}  ± {1,2:f1}", EventDetails.X_Dia, EventDetails.Sdev_Major));
			((Control)lblYhD).set_Text(string.Format("{0,2:f1}  ± {1,2:f1}", EventDetails.Y_Dia, EventDetails.Sdev_Minor));
			((Control)lblPAh).set_Text(string.Format("{0,2:f1}  ± {1,2:f1}", EventDetails.PA_Ellipse, EventDetails.Sdev_PA_Ellipse));
			((Control)picSMhNo).set_Visible(!EventDetails.AstrometryShapeModelCentered);
			((Control)picADhNo).set_Visible(!EventDetails.UsedAssumedDiameter);
			((Control)picXhNo).set_Visible(!EventDetails.Solve_X);
			((Control)picYhNo).set_Visible(!EventDetails.Solve_Y);
			((Control)picXhDNo).set_Visible(!EventDetails.Solve_Major);
			((Control)picYhDNo).set_Visible(!EventDetails.Solve_Minor);
			((Control)picPAhNo).set_Visible(!EventDetails.Solve_PA);
			((Control)picMhNo).set_Visible(!EventDetails.Inc_Miss);
			((Control)picChNo).set_Visible(!EventDetails.Solve_Circular);
			((Control)picSephNo).set_Visible(!EventDetails.Solve_CompanionSep);
			((Control)picDbPAhNo).set_Visible(!EventDetails.Solve_CompanionPA);
			for (int j = 0; j < EventDetails.ShapeData.Count; j++)
			{
				lstHistoryShape.get_Items().Add((object)EventDetails.ShapeData[j].Summary());
			}
			for (int k = 0; k < EventDetails.NumberOfDoubleSolutions; k++)
			{
				lstHistoryDouble.get_Items().Add((object)EventDetails.Doubles[k].ToString());
			}
			if (EventDetails.AsteroidHasSatellite)
			{
				lstHistorySatellite.get_Items().Add((object)(string.Format("{0,2:f1}", EventDetails.Doubles[4].Sep_Companion).PadLeft(6) + "mas" + string.Format("{0,2:f1}", EventDetails.Doubles[4].PA_Companion).PadLeft(11) + "°, " + string.Format("{0,2:f1}", EventDetails.Satellites[Data_and_Plots.SelectedSatellite].MajorAxisSatellite).PadLeft(5) + " x " + string.Format("{0,2:f1}", EventDetails.Satellites[Data_and_Plots.SelectedSatellite].MinorAxisSatellite).PadLeft(5) + "km" + string.Format("{0,2:f1}", EventDetails.Satellites[Data_and_Plots.SelectedSatellite].PAAxisSatellite).PadLeft(6) + "°, " + EventDetails.Satellites[Data_and_Plots.SelectedSatellite].CompanionIAUname));
			}
			if (((Control)lblFe).get_Text() != ((Control)lblFh).get_Text())
			{
				Label obj = lblFite;
				Color red;
				((Control)lblFith).set_ForeColor(red = Color.Red);
				((Control)obj).set_ForeColor(red);
			}
			if (((Control)lblXe).get_Text() != ((Control)lblXh).get_Text())
			{
				Label obj2 = lblExX;
				Color red;
				((Control)lblEvX).set_ForeColor(red = Color.Red);
				((Control)obj2).set_ForeColor(red);
			}
			if (((Control)lblYe).get_Text() != ((Control)lblYh).get_Text())
			{
				Label obj3 = lblExY;
				Color red;
				((Control)lblEvY).set_ForeColor(red = Color.Red);
				((Control)obj3).set_ForeColor(red);
			}
			if (((Control)lblXeD).get_Text() != ((Control)lblXhD).get_Text())
			{
				Label obj4 = labela;
				Color red;
				((Control)labelc).set_ForeColor(red = Color.Red);
				((Control)obj4).set_ForeColor(red);
			}
			if (((Control)lblYeD).get_Text() != ((Control)lblYhD).get_Text())
			{
				Label obj5 = labelb;
				Color red;
				((Control)labeld).set_ForeColor(red = Color.Red);
				((Control)obj5).set_ForeColor(red);
			}
			if (((Control)lblPAe).get_Text() != ((Control)lblPAh).get_Text())
			{
				Label obj6 = label6;
				Color red;
				((Control)label7).set_ForeColor(red = Color.Red);
				((Control)obj6).set_ForeColor(red);
			}
			if (((Control)picSMeNo).get_Visible() != ((Control)picSMhNo).get_Visible())
			{
				Label obj7 = label1;
				Color red;
				((Control)label13).set_ForeColor(red = Color.Red);
				((Control)obj7).set_ForeColor(red);
			}
			if (((Control)picCeNo).get_Visible() != ((Control)picChNo).get_Visible())
			{
				Label obj8 = label22;
				Color red;
				((Control)label23).set_ForeColor(red = Color.Red);
				((Control)obj8).set_ForeColor(red);
			}
			if (((Control)picADeNo).get_Visible() != ((Control)picADhNo).get_Visible())
			{
				Label obj9 = label18;
				Color red;
				((Control)label21).set_ForeColor(red = Color.Red);
				((Control)obj9).set_ForeColor(red);
			}
			if (((Control)picXeNo).get_Visible() != ((Control)picXhNo).get_Visible())
			{
				Label obj10 = label14;
				Color red;
				((Control)label34).set_ForeColor(red = Color.Red);
				((Control)obj10).set_ForeColor(red);
			}
			if (((Control)picYeNo).get_Visible() != ((Control)picYhNo).get_Visible())
			{
				Label obj11 = label19;
				Color red;
				((Control)label20).set_ForeColor(red = Color.Red);
				((Control)obj11).set_ForeColor(red);
			}
			if (((Control)picXeDNo).get_Visible() != ((Control)picXhDNo).get_Visible())
			{
				Label obj12 = label17;
				Color red;
				((Control)label31).set_ForeColor(red = Color.Red);
				((Control)obj12).set_ForeColor(red);
			}
			if (((Control)picYeDNo).get_Visible() != ((Control)picYhDNo).get_Visible())
			{
				Label obj13 = label16;
				Color red;
				((Control)label32).set_ForeColor(red = Color.Red);
				((Control)obj13).set_ForeColor(red);
			}
			if (((Control)picPAeNo).get_Visible() != ((Control)picPAhNo).get_Visible())
			{
				Label obj14 = label15;
				Color red;
				((Control)label33).set_ForeColor(red = Color.Red);
				((Control)obj14).set_ForeColor(red);
			}
			if (((Control)picMeNo).get_Visible() != ((Control)picMhNo).get_Visible())
			{
				Label obj15 = label24;
				Color red;
				((Control)label25).set_ForeColor(red = Color.Red);
				((Control)obj15).set_ForeColor(red);
			}
			int count = lstExportShape.get_Items().get_Count();
			int count2 = lstHistoryShape.get_Items().get_Count();
			if (count > 1 || count2 > 1)
			{
				if (count != count2)
				{
					ListBox obj16 = lstExportShape;
					Color red;
					((Control)lstHistoryShape).set_ForeColor(red = Color.Red);
					((Control)obj16).set_ForeColor(red);
				}
				else
				{
					bool flag = true;
					for (int l = 1; l < count; l++)
					{
						if (lstExportShape.get_Items().get_Item(l).ToString() != lstHistoryShape.get_Items().get_Item(l).ToString())
						{
							flag = false;
						}
					}
					if (!flag)
					{
						ListBox obj17 = lstExportShape;
						Color red;
						((Control)lstHistoryShape).set_ForeColor(red = Color.Red);
						((Control)obj17).set_ForeColor(red);
					}
				}
				chkShapeModels.set_Checked(((Control)lstHistoryShape).get_ForeColor() == Color.Red);
			}
			count = lstExportDouble.get_Items().get_Count();
			count2 = lstHistoryDouble.get_Items().get_Count();
			if (count > 1 || count2 > 1)
			{
				if (count != count2)
				{
					ListBox obj18 = lstExportDouble;
					Color red;
					((Control)lstHistoryDouble).set_ForeColor(red = Color.Red);
					((Control)obj18).set_ForeColor(red);
				}
				else
				{
					bool flag2 = true;
					for (int m = 1; m < count; m++)
					{
						if (lstExportDouble.get_Items().get_Item(m).ToString() != lstHistoryDouble.get_Items().get_Item(m).ToString())
						{
							flag2 = false;
						}
					}
					if (!flag2)
					{
						ListBox obj19 = lstExportDouble;
						Color red;
						((Control)lstHistoryDouble).set_ForeColor(red = Color.Red);
						((Control)obj19).set_ForeColor(red);
					}
				}
				chkDoubles.set_Checked(((Control)lstHistoryDouble).get_ForeColor() == Color.Red);
			}
			count = lstExportSatellite.get_Items().get_Count();
			count2 = lstHistorySatellite.get_Items().get_Count();
			if (count > 1 || count2 > 1)
			{
				if (count != count2)
				{
					ListBox obj20 = lstExportSatellite;
					Color red;
					((Control)lstHistorySatellite).set_ForeColor(red = Color.Red);
					((Control)obj20).set_ForeColor(red);
				}
				else if (lstExportSatellite.get_Items().get_Item(1).ToString() != lstHistorySatellite.get_Items().get_Item(1).ToString())
				{
					ListBox obj21 = lstExportSatellite;
					Color red;
					((Control)lstHistorySatellite).set_ForeColor(red = Color.Red);
					((Control)obj21).set_ForeColor(red);
				}
				chkSatellite.set_Checked(((Control)lstHistorySatellite).get_ForeColor() == Color.Red);
			}
		}

		private void cmdOK_Click(object sender, EventArgs e)
		{
			UpdateEllipse = chkEllipse.get_Checked();
			UpdateFlags = chkSolutions.get_Checked();
			UpdateShapes = chkShapeModels.get_Checked();
			UpdateDoubles = chkDoubles.get_Checked();
			UpdateSatellites = chkSatellite.get_Checked();
			((Form)this).Close();
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
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
			//IL_0319: Unknown result type (might be due to invalid IL or missing references)
			//IL_0323: Expected O, but got Unknown
			//IL_0324: Unknown result type (might be due to invalid IL or missing references)
			//IL_032e: Expected O, but got Unknown
			//IL_032f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0339: Expected O, but got Unknown
			//IL_033a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0344: Expected O, but got Unknown
			//IL_0345: Unknown result type (might be due to invalid IL or missing references)
			//IL_034f: Expected O, but got Unknown
			//IL_0350: Unknown result type (might be due to invalid IL or missing references)
			//IL_035a: Expected O, but got Unknown
			//IL_035b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0365: Expected O, but got Unknown
			//IL_0366: Unknown result type (might be due to invalid IL or missing references)
			//IL_0370: Expected O, but got Unknown
			//IL_0371: Unknown result type (might be due to invalid IL or missing references)
			//IL_037b: Expected O, but got Unknown
			//IL_037c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0386: Expected O, but got Unknown
			//IL_0387: Unknown result type (might be due to invalid IL or missing references)
			//IL_0391: Expected O, but got Unknown
			//IL_0392: Unknown result type (might be due to invalid IL or missing references)
			//IL_039c: Expected O, but got Unknown
			//IL_039d: Unknown result type (might be due to invalid IL or missing references)
			//IL_03a7: Expected O, but got Unknown
			//IL_03a8: Unknown result type (might be due to invalid IL or missing references)
			//IL_03b2: Expected O, but got Unknown
			//IL_03b3: Unknown result type (might be due to invalid IL or missing references)
			//IL_03bd: Expected O, but got Unknown
			//IL_03be: Unknown result type (might be due to invalid IL or missing references)
			//IL_03c8: Expected O, but got Unknown
			//IL_03c9: Unknown result type (might be due to invalid IL or missing references)
			//IL_03d3: Expected O, but got Unknown
			//IL_03d4: Unknown result type (might be due to invalid IL or missing references)
			//IL_03de: Expected O, but got Unknown
			//IL_03df: Unknown result type (might be due to invalid IL or missing references)
			//IL_03e9: Expected O, but got Unknown
			//IL_03ea: Unknown result type (might be due to invalid IL or missing references)
			//IL_03f4: Expected O, but got Unknown
			//IL_03f5: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ff: Expected O, but got Unknown
			//IL_0400: Unknown result type (might be due to invalid IL or missing references)
			//IL_040a: Expected O, but got Unknown
			//IL_040b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0415: Expected O, but got Unknown
			//IL_0416: Unknown result type (might be due to invalid IL or missing references)
			//IL_0420: Expected O, but got Unknown
			//IL_0421: Unknown result type (might be due to invalid IL or missing references)
			//IL_042b: Expected O, but got Unknown
			//IL_042c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0436: Expected O, but got Unknown
			//IL_0437: Unknown result type (might be due to invalid IL or missing references)
			//IL_0441: Expected O, but got Unknown
			//IL_0442: Unknown result type (might be due to invalid IL or missing references)
			//IL_044c: Expected O, but got Unknown
			//IL_044d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0457: Expected O, but got Unknown
			//IL_0458: Unknown result type (might be due to invalid IL or missing references)
			//IL_0462: Expected O, but got Unknown
			//IL_0463: Unknown result type (might be due to invalid IL or missing references)
			//IL_046d: Expected O, but got Unknown
			//IL_046e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0478: Expected O, but got Unknown
			//IL_0479: Unknown result type (might be due to invalid IL or missing references)
			//IL_0483: Expected O, but got Unknown
			//IL_0484: Unknown result type (might be due to invalid IL or missing references)
			//IL_048e: Expected O, but got Unknown
			//IL_048f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0499: Expected O, but got Unknown
			//IL_049a: Unknown result type (might be due to invalid IL or missing references)
			//IL_04a4: Expected O, but got Unknown
			//IL_04a5: Unknown result type (might be due to invalid IL or missing references)
			//IL_04af: Expected O, but got Unknown
			//IL_04b0: Unknown result type (might be due to invalid IL or missing references)
			//IL_04ba: Expected O, but got Unknown
			//IL_04bb: Unknown result type (might be due to invalid IL or missing references)
			//IL_04c5: Expected O, but got Unknown
			//IL_04c6: Unknown result type (might be due to invalid IL or missing references)
			//IL_04d0: Expected O, but got Unknown
			//IL_04d1: Unknown result type (might be due to invalid IL or missing references)
			//IL_04db: Expected O, but got Unknown
			//IL_04dc: Unknown result type (might be due to invalid IL or missing references)
			//IL_04e6: Expected O, but got Unknown
			//IL_04e7: Unknown result type (might be due to invalid IL or missing references)
			//IL_04f1: Expected O, but got Unknown
			//IL_04f2: Unknown result type (might be due to invalid IL or missing references)
			//IL_04fc: Expected O, but got Unknown
			//IL_04fd: Unknown result type (might be due to invalid IL or missing references)
			//IL_0507: Expected O, but got Unknown
			//IL_0508: Unknown result type (might be due to invalid IL or missing references)
			//IL_0512: Expected O, but got Unknown
			//IL_0513: Unknown result type (might be due to invalid IL or missing references)
			//IL_051d: Expected O, but got Unknown
			//IL_051e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0528: Expected O, but got Unknown
			label2 = new Label();
			lblExX = new Label();
			label4 = new Label();
			lblExY = new Label();
			label6 = new Label();
			label7 = new Label();
			lblEvY = new Label();
			lblEvX = new Label();
			lblXe = new Label();
			lblYe = new Label();
			lblPAe = new Label();
			lblXh = new Label();
			lblYh = new Label();
			lblPAh = new Label();
			lblXeD = new Label();
			lblYeD = new Label();
			lblXhD = new Label();
			lblYhD = new Label();
			lstExportShape = new ListBox();
			lstHistoryShape = new ListBox();
			labela = new Label();
			labelb = new Label();
			labelc = new Label();
			labeld = new Label();
			cmdOK = new Button();
			cmdCancel = new Button();
			label10 = new Label();
			lstExportDouble = new ListBox();
			lstHistoryDouble = new ListBox();
			label11 = new Label();
			label12 = new Label();
			lstHistorySatellite = new ListBox();
			lstExportSatellite = new ListBox();
			picXeYes = new PictureBox();
			picXeNo = new PictureBox();
			picPAeNo = new PictureBox();
			picPAeYes = new PictureBox();
			picYeDNo = new PictureBox();
			picYeDYes = new PictureBox();
			picYeNo = new PictureBox();
			picYeYes = new PictureBox();
			picXeDNo = new PictureBox();
			picXeDYes = new PictureBox();
			label14 = new Label();
			label15 = new Label();
			label16 = new Label();
			label17 = new Label();
			label19 = new Label();
			label20 = new Label();
			label31 = new Label();
			label32 = new Label();
			label33 = new Label();
			label34 = new Label();
			picXhDNo = new PictureBox();
			picXhDYes = new PictureBox();
			picYhNo = new PictureBox();
			picYhYes = new PictureBox();
			picYhDNo = new PictureBox();
			picYhDYes = new PictureBox();
			picPAhNo = new PictureBox();
			picPAhYes = new PictureBox();
			picXhNo = new PictureBox();
			picXhYes = new PictureBox();
			label35 = new Label();
			label36 = new Label();
			picSepeNo = new PictureBox();
			picSepeYes = new PictureBox();
			picDbPAeNo = new PictureBox();
			picDbPAeYes = new PictureBox();
			label37 = new Label();
			label38 = new Label();
			picSephNo = new PictureBox();
			picSephYes = new PictureBox();
			picDbPAhNo = new PictureBox();
			picDbPAhYes = new PictureBox();
			groupBox1 = new GroupBox();
			label25 = new Label();
			label24 = new Label();
			picMhNo = new PictureBox();
			picMhYes = new PictureBox();
			picMeNo = new PictureBox();
			picMeYes = new PictureBox();
			groupBox2 = new GroupBox();
			picCeNo = new PictureBox();
			picCeYes = new PictureBox();
			picChNo = new PictureBox();
			picChYes = new PictureBox();
			label23 = new Label();
			label22 = new Label();
			label21 = new Label();
			picADhNo = new PictureBox();
			picADhYes = new PictureBox();
			label18 = new Label();
			picADeNo = new PictureBox();
			picADeYes = new PictureBox();
			label13 = new Label();
			picSMhNo = new PictureBox();
			picSMhYes = new PictureBox();
			label1 = new Label();
			picSMeNo = new PictureBox();
			picSMeYes = new PictureBox();
			label26 = new Label();
			label27 = new Label();
			chkEllipse = new CheckBox();
			chkSolutions = new CheckBox();
			chkShapeModels = new CheckBox();
			chkDoubles = new CheckBox();
			chkSatellite = new CheckBox();
			lblExport = new Label();
			lblImport = new Label();
			label28 = new Label();
			label29 = new Label();
			label30 = new Label();
			lblExportLastEdit = new Label();
			lblEditorLastEdit = new Label();
			lblAsteroid = new Label();
			lblFite = new Label();
			lblFe = new Label();
			lblFith = new Label();
			lblFh = new Label();
			((ISupportInitialize)picXeYes).BeginInit();
			((ISupportInitialize)picXeNo).BeginInit();
			((ISupportInitialize)picPAeNo).BeginInit();
			((ISupportInitialize)picPAeYes).BeginInit();
			((ISupportInitialize)picYeDNo).BeginInit();
			((ISupportInitialize)picYeDYes).BeginInit();
			((ISupportInitialize)picYeNo).BeginInit();
			((ISupportInitialize)picYeYes).BeginInit();
			((ISupportInitialize)picXeDNo).BeginInit();
			((ISupportInitialize)picXeDYes).BeginInit();
			((ISupportInitialize)picXhDNo).BeginInit();
			((ISupportInitialize)picXhDYes).BeginInit();
			((ISupportInitialize)picYhNo).BeginInit();
			((ISupportInitialize)picYhYes).BeginInit();
			((ISupportInitialize)picYhDNo).BeginInit();
			((ISupportInitialize)picYhDYes).BeginInit();
			((ISupportInitialize)picPAhNo).BeginInit();
			((ISupportInitialize)picPAhYes).BeginInit();
			((ISupportInitialize)picXhNo).BeginInit();
			((ISupportInitialize)picXhYes).BeginInit();
			((ISupportInitialize)picSepeNo).BeginInit();
			((ISupportInitialize)picSepeYes).BeginInit();
			((ISupportInitialize)picDbPAeNo).BeginInit();
			((ISupportInitialize)picDbPAeYes).BeginInit();
			((ISupportInitialize)picSephNo).BeginInit();
			((ISupportInitialize)picSephYes).BeginInit();
			((ISupportInitialize)picDbPAhNo).BeginInit();
			((ISupportInitialize)picDbPAhYes).BeginInit();
			((Control)groupBox1).SuspendLayout();
			((ISupportInitialize)picMhNo).BeginInit();
			((ISupportInitialize)picMhYes).BeginInit();
			((ISupportInitialize)picMeNo).BeginInit();
			((ISupportInitialize)picMeYes).BeginInit();
			((Control)groupBox2).SuspendLayout();
			((ISupportInitialize)picCeNo).BeginInit();
			((ISupportInitialize)picCeYes).BeginInit();
			((ISupportInitialize)picChNo).BeginInit();
			((ISupportInitialize)picChYes).BeginInit();
			((ISupportInitialize)picADhNo).BeginInit();
			((ISupportInitialize)picADhYes).BeginInit();
			((ISupportInitialize)picADeNo).BeginInit();
			((ISupportInitialize)picADeYes).BeginInit();
			((ISupportInitialize)picSMhNo).BeginInit();
			((ISupportInitialize)picSMhYes).BeginInit();
			((ISupportInitialize)picSMeNo).BeginInit();
			((ISupportInitialize)picSMeYes).BeginInit();
			((Control)this).SuspendLayout();
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 16f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label2).set_ForeColor(Color.Maroon);
			((Control)label2).set_Location(new Point(70, 14));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(237, 26));
			((Control)label2).set_TabIndex(1);
			((Control)label2).set_Text("Content of Export file");
			((Control)lblExX).set_AutoSize(true);
			((Control)lblExX).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblExX).set_Location(new Point(41, 27));
			((Control)lblExX).set_Name("lblExX");
			((Control)lblExX).set_Size(new Size(25, 15));
			((Control)lblExX).set_TabIndex(2);
			((Control)lblExX).set_Text("X =");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 16f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label4).set_ForeColor(Color.DarkBlue);
			((Control)label4).set_Location(new Point(438, 14));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(262, 26));
			((Control)label4).set_TabIndex(3);
			((Control)label4).set_Text("Event data in the Editor");
			((Control)lblExY).set_AutoSize(true);
			((Control)lblExY).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblExY).set_Location(new Point(175, 27));
			((Control)lblExY).set_Name("lblExY");
			((Control)lblExY).set_Size(new Size(27, 15));
			((Control)lblExY).set_TabIndex(4);
			((Control)lblExY).set_Text("Y = ");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(34, 65));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(35, 15));
			((Control)label6).set_TabIndex(5);
			((Control)label6).set_Text("PA = ");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(431, 65));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(35, 15));
			((Control)label7).set_TabIndex(8);
			((Control)label7).set_Text("PA = ");
			((Control)lblEvY).set_AutoSize(true);
			((Control)lblEvY).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblEvY).set_Location(new Point(572, 27));
			((Control)lblEvY).set_Name("lblEvY");
			((Control)lblEvY).set_Size(new Size(27, 15));
			((Control)lblEvY).set_TabIndex(7);
			((Control)lblEvY).set_Text("Y = ");
			((Control)lblEvX).set_AutoSize(true);
			((Control)lblEvX).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblEvX).set_Location(new Point(438, 27));
			((Control)lblEvX).set_Name("lblEvX");
			((Control)lblEvX).set_Size(new Size(25, 15));
			((Control)lblEvX).set_TabIndex(6);
			((Control)lblEvX).set_Text("X =");
			((Control)lblXe).set_AutoSize(true);
			((Control)lblXe).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblXe).set_Location(new Point(64, 27));
			((Control)lblXe).set_Name("lblXe");
			((Control)lblXe).set_Size(new Size(14, 15));
			((Control)lblXe).set_TabIndex(9);
			((Control)lblXe).set_Text("0");
			((Control)lblYe).set_AutoSize(true);
			((Control)lblYe).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblYe).set_Location(new Point(198, 27));
			((Control)lblYe).set_Name("lblYe");
			((Control)lblYe).set_Size(new Size(14, 15));
			((Control)lblYe).set_TabIndex(10);
			((Control)lblYe).set_Text("0");
			((Control)lblPAe).set_AutoSize(true);
			((Control)lblPAe).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblPAe).set_Location(new Point(64, 65));
			((Control)lblPAe).set_Name("lblPAe");
			((Control)lblPAe).set_Size(new Size(14, 15));
			((Control)lblPAe).set_TabIndex(11);
			((Control)lblPAe).set_Text("0");
			((Control)lblXh).set_AutoSize(true);
			((Control)lblXh).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblXh).set_Location(new Point(461, 27));
			((Control)lblXh).set_Name("lblXh");
			((Control)lblXh).set_Size(new Size(14, 15));
			((Control)lblXh).set_TabIndex(12);
			((Control)lblXh).set_Text("0");
			((Control)lblYh).set_AutoSize(true);
			((Control)lblYh).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblYh).set_Location(new Point(594, 27));
			((Control)lblYh).set_Name("lblYh");
			((Control)lblYh).set_Size(new Size(14, 15));
			((Control)lblYh).set_TabIndex(13);
			((Control)lblYh).set_Text("0");
			((Control)lblPAh).set_AutoSize(true);
			((Control)lblPAh).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblPAh).set_Location(new Point(460, 65));
			((Control)lblPAh).set_Name("lblPAh");
			((Control)lblPAh).set_Size(new Size(14, 15));
			((Control)lblPAh).set_TabIndex(14);
			((Control)lblPAh).set_Text("0");
			((Control)lblXeD).set_AutoSize(true);
			((Control)lblXeD).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblXeD).set_Location(new Point(64, 46));
			((Control)lblXeD).set_Name("lblXeD");
			((Control)lblXeD).set_Size(new Size(14, 15));
			((Control)lblXeD).set_TabIndex(15);
			((Control)lblXeD).set_Text("0");
			((Control)lblYeD).set_AutoSize(true);
			((Control)lblYeD).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblYeD).set_Location(new Point(198, 46));
			((Control)lblYeD).set_Name("lblYeD");
			((Control)lblYeD).set_Size(new Size(14, 15));
			((Control)lblYeD).set_TabIndex(16);
			((Control)lblYeD).set_Text("0");
			((Control)lblXhD).set_AutoSize(true);
			((Control)lblXhD).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblXhD).set_Location(new Point(461, 46));
			((Control)lblXhD).set_Name("lblXhD");
			((Control)lblXhD).set_Size(new Size(14, 15));
			((Control)lblXhD).set_TabIndex(18);
			((Control)lblXhD).set_Text("0");
			((Control)lblYhD).set_AutoSize(true);
			((Control)lblYhD).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblYhD).set_Location(new Point(594, 46));
			((Control)lblYhD).set_Name("lblYhD");
			((Control)lblYhD).set_Size(new Size(14, 15));
			((Control)lblYhD).set_TabIndex(19);
			((Control)lblYhD).set_Text("0");
			((Control)lstExportShape).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstExportShape).set_FormattingEnabled(true);
			lstExportShape.set_HorizontalScrollbar(true);
			((Control)lstExportShape).set_Location(new Point(10, 327));
			((Control)lstExportShape).set_Name("lstExportShape");
			((Control)lstExportShape).set_Size(new Size(374, 108));
			((Control)lstExportShape).set_TabIndex(30);
			((Control)lstHistoryShape).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstHistoryShape).set_FormattingEnabled(true);
			lstHistoryShape.set_HorizontalScrollbar(true);
			((Control)lstHistoryShape).set_Location(new Point(390, 327));
			((Control)lstHistoryShape).set_Name("lstHistoryShape");
			((Control)lstHistoryShape).set_Size(new Size(374, 108));
			((Control)lstHistoryShape).set_TabIndex(31);
			((Control)labela).set_AutoSize(true);
			((Control)labela).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)labela).set_Location(new Point(21, 46));
			((Control)labela).set_Name("labela");
			((Control)labela).set_Size(new Size(45, 15));
			((Control)labela).set_TabIndex(32);
			((Control)labela).set_Text("X dia =");
			((Control)labelb).set_AutoSize(true);
			((Control)labelb).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)labelb).set_Location(new Point(155, 46));
			((Control)labelb).set_Name("labelb");
			((Control)labelb).set_Size(new Size(44, 15));
			((Control)labelb).set_TabIndex(33);
			((Control)labelb).set_Text("Y dia =");
			((Control)labelc).set_AutoSize(true);
			((Control)labelc).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)labelc).set_Location(new Point(418, 46));
			((Control)labelc).set_Name("labelc");
			((Control)labelc).set_Size(new Size(45, 15));
			((Control)labelc).set_TabIndex(34);
			((Control)labelc).set_Text("X dia =");
			((Control)labeld).set_AutoSize(true);
			((Control)labeld).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)labeld).set_Location(new Point(553, 46));
			((Control)labeld).set_Name("labeld");
			((Control)labeld).set_Size(new Size(44, 15));
			((Control)labeld).set_TabIndex(35);
			((Control)labeld).set_Text("Y dia =");
			cmdOK.set_DialogResult((DialogResult)6);
			((Control)cmdOK).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdOK).set_Location(new Point(685, 677));
			((Control)cmdOK).set_Name("cmdOK");
			((Control)cmdOK).set_Size(new Size(79, 33));
			((Control)cmdOK).set_TabIndex(36);
			((Control)cmdOK).set_Text("Yes");
			((ButtonBase)cmdOK).set_UseVisualStyleBackColor(true);
			((Control)cmdOK).add_Click((EventHandler)cmdOK_Click);
			cmdCancel.set_DialogResult((DialogResult)2);
			((Control)cmdCancel).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdCancel).set_Location(new Point(685, 716));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(79, 32));
			((Control)cmdCancel).set_TabIndex(37);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label10).set_Location(new Point(13, 304));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(144, 20));
			((Control)label10).set_TabIndex(38);
			((Control)label10).set_Text("Shape model fits");
			((Control)lstExportDouble).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstExportDouble).set_FormattingEnabled(true);
			lstExportDouble.set_HorizontalScrollbar(true);
			((Control)lstExportDouble).set_Location(new Point(10, 488));
			((Control)lstExportDouble).set_Name("lstExportDouble");
			((Control)lstExportDouble).set_Size(new Size(289, 69));
			((Control)lstExportDouble).set_TabIndex(39);
			((Control)lstHistoryDouble).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstHistoryDouble).set_FormattingEnabled(true);
			lstHistoryDouble.set_HorizontalScrollbar(true);
			((Control)lstHistoryDouble).set_Location(new Point(390, 488));
			((Control)lstHistoryDouble).set_Name("lstHistoryDouble");
			((Control)lstHistoryDouble).set_Size(new Size(289, 69));
			((Control)lstHistoryDouble).set_TabIndex(40);
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label11).set_Location(new Point(13, 465));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(132, 20));
			((Control)label11).set_TabIndex(41);
			((Control)label11).set_Text("Double star fits");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(13, 591));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(96, 20));
			((Control)label12).set_TabIndex(42);
			((Control)label12).set_Text("Satellite fit");
			((Control)lstHistorySatellite).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstHistorySatellite).set_FormattingEnabled(true);
			lstHistorySatellite.set_HorizontalScrollbar(true);
			((Control)lstHistorySatellite).set_Location(new Point(390, 614));
			((Control)lstHistorySatellite).set_Name("lstHistorySatellite");
			((Control)lstHistorySatellite).set_Size(new Size(374, 56));
			((Control)lstHistorySatellite).set_TabIndex(44);
			((Control)lstExportSatellite).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstExportSatellite).set_FormattingEnabled(true);
			lstExportSatellite.set_HorizontalScrollbar(true);
			((Control)lstExportSatellite).set_Location(new Point(8, 614));
			((Control)lstExportSatellite).set_Name("lstExportSatellite");
			((Control)lstExportSatellite).set_Size(new Size(374, 56));
			((Control)lstExportSatellite).set_TabIndex(45);
			picXeYes.set_Image((Image)Resources.check);
			((Control)picXeYes).set_Location(new Point(25, 29));
			((Control)picXeYes).set_Name("picXeYes");
			((Control)picXeYes).set_Size(new Size(18, 18));
			picXeYes.set_SizeMode((PictureBoxSizeMode)1);
			picXeYes.set_TabIndex(47);
			picXeYes.set_TabStop(false);
			picXeNo.set_Image((Image)Resources.error);
			((Control)picXeNo).set_Location(new Point(25, 49));
			((Control)picXeNo).set_Name("picXeNo");
			((Control)picXeNo).set_Size(new Size(18, 18));
			picXeNo.set_SizeMode((PictureBoxSizeMode)1);
			picXeNo.set_TabIndex(48);
			picXeNo.set_TabStop(false);
			((Control)picXeNo).set_Visible(false);
			picPAeNo.set_Image((Image)Resources.error);
			((Control)picPAeNo).set_Location(new Point(266, 49));
			((Control)picPAeNo).set_Name("picPAeNo");
			((Control)picPAeNo).set_Size(new Size(18, 18));
			picPAeNo.set_SizeMode((PictureBoxSizeMode)1);
			picPAeNo.set_TabIndex(60);
			picPAeNo.set_TabStop(false);
			((Control)picPAeNo).set_Visible(false);
			picPAeYes.set_Image((Image)Resources.check);
			((Control)picPAeYes).set_Location(new Point(266, 29));
			((Control)picPAeYes).set_Name("picPAeYes");
			((Control)picPAeYes).set_Size(new Size(18, 18));
			picPAeYes.set_SizeMode((PictureBoxSizeMode)1);
			picPAeYes.set_TabIndex(59);
			picPAeYes.set_TabStop(false);
			picYeDNo.set_Image((Image)Resources.error);
			((Control)picYeDNo).set_Location(new Point(210, 49));
			((Control)picYeDNo).set_Name("picYeDNo");
			((Control)picYeDNo).set_Size(new Size(18, 18));
			picYeDNo.set_SizeMode((PictureBoxSizeMode)1);
			picYeDNo.set_TabIndex(62);
			picYeDNo.set_TabStop(false);
			((Control)picYeDNo).set_Visible(false);
			picYeDYes.set_Image((Image)Resources.check);
			((Control)picYeDYes).set_Location(new Point(210, 29));
			((Control)picYeDYes).set_Name("picYeDYes");
			((Control)picYeDYes).set_Size(new Size(18, 18));
			picYeDYes.set_SizeMode((PictureBoxSizeMode)1);
			picYeDYes.set_TabIndex(61);
			picYeDYes.set_TabStop(false);
			picYeNo.set_Image((Image)Resources.error);
			((Control)picYeNo).set_Location(new Point(73, 49));
			((Control)picYeNo).set_Name("picYeNo");
			((Control)picYeNo).set_Size(new Size(18, 18));
			picYeNo.set_SizeMode((PictureBoxSizeMode)1);
			picYeNo.set_TabIndex(64);
			picYeNo.set_TabStop(false);
			((Control)picYeNo).set_Visible(false);
			picYeYes.set_Image((Image)Resources.check);
			((Control)picYeYes).set_Location(new Point(73, 29));
			((Control)picYeYes).set_Name("picYeYes");
			((Control)picYeYes).set_Size(new Size(18, 18));
			picYeYes.set_SizeMode((PictureBoxSizeMode)1);
			picYeYes.set_TabIndex(63);
			picYeYes.set_TabStop(false);
			picXeDNo.set_Image((Image)Resources.error);
			((Control)picXeDNo).set_Location(new Point(142, 49));
			((Control)picXeDNo).set_Name("picXeDNo");
			((Control)picXeDNo).set_Size(new Size(18, 18));
			picXeDNo.set_SizeMode((PictureBoxSizeMode)1);
			picXeDNo.set_TabIndex(66);
			picXeDNo.set_TabStop(false);
			((Control)picXeDNo).set_Visible(false);
			picXeDYes.set_Image((Image)Resources.check);
			((Control)picXeDYes).set_Location(new Point(142, 29));
			((Control)picXeDYes).set_Name("picXeDYes");
			((Control)picXeDYes).set_Size(new Size(18, 18));
			picXeDYes.set_SizeMode((PictureBoxSizeMode)1);
			picXeDYes.set_TabIndex(65);
			picXeDYes.set_TabStop(false);
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label14).set_Location(new Point(8, 31));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(15, 15));
			((Control)label14).set_TabIndex(67);
			((Control)label14).set_Text("X");
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label15).set_Location(new Point(242, 31));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(22, 15));
			((Control)label15).set_TabIndex(68);
			((Control)label15).set_Text("PA");
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label16).set_Location(new Point(169, 31));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(39, 15));
			((Control)label16).set_TabIndex(69);
			((Control)label16).set_Text("Minor");
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label17).set_Location(new Point(101, 31));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(39, 15));
			((Control)label17).set_TabIndex(70);
			((Control)label17).set_Text("Major");
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label19).set_Location(new Point(57, 31));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(14, 15));
			((Control)label19).set_TabIndex(71);
			((Control)label19).set_Text("Y");
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label20).set_Location(new Point(453, 31));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(14, 15));
			((Control)label20).set_TabIndex(86);
			((Control)label20).set_Text("Y");
			((Control)label31).set_AutoSize(true);
			((Control)label31).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label31).set_Location(new Point(497, 31));
			((Control)label31).set_Name("label31");
			((Control)label31).set_Size(new Size(39, 15));
			((Control)label31).set_TabIndex(85);
			((Control)label31).set_Text("Major");
			((Control)label32).set_AutoSize(true);
			((Control)label32).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label32).set_Location(new Point(565, 31));
			((Control)label32).set_Name("label32");
			((Control)label32).set_Size(new Size(39, 15));
			((Control)label32).set_TabIndex(84);
			((Control)label32).set_Text("Minor");
			((Control)label33).set_AutoSize(true);
			((Control)label33).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label33).set_Location(new Point(638, 31));
			((Control)label33).set_Name("label33");
			((Control)label33).set_Size(new Size(22, 15));
			((Control)label33).set_TabIndex(83);
			((Control)label33).set_Text("PA");
			((Control)label34).set_AutoSize(true);
			((Control)label34).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label34).set_Location(new Point(404, 31));
			((Control)label34).set_Name("label34");
			((Control)label34).set_Size(new Size(15, 15));
			((Control)label34).set_TabIndex(82);
			((Control)label34).set_Text("X");
			picXhDNo.set_Image((Image)Resources.error);
			((Control)picXhDNo).set_Location(new Point(537, 49));
			((Control)picXhDNo).set_Name("picXhDNo");
			((Control)picXhDNo).set_Size(new Size(18, 18));
			picXhDNo.set_SizeMode((PictureBoxSizeMode)1);
			picXhDNo.set_TabIndex(81);
			picXhDNo.set_TabStop(false);
			((Control)picXhDNo).set_Visible(false);
			picXhDYes.set_Image((Image)Resources.check);
			((Control)picXhDYes).set_Location(new Point(538, 29));
			((Control)picXhDYes).set_Name("picXhDYes");
			((Control)picXhDYes).set_Size(new Size(18, 18));
			picXhDYes.set_SizeMode((PictureBoxSizeMode)1);
			picXhDYes.set_TabIndex(80);
			picXhDYes.set_TabStop(false);
			picYhNo.set_Image((Image)Resources.error);
			((Control)picYhNo).set_Location(new Point(468, 49));
			((Control)picYhNo).set_Name("picYhNo");
			((Control)picYhNo).set_Size(new Size(18, 18));
			picYhNo.set_SizeMode((PictureBoxSizeMode)1);
			picYhNo.set_TabIndex(79);
			picYhNo.set_TabStop(false);
			((Control)picYhNo).set_Visible(false);
			picYhYes.set_Image((Image)Resources.check);
			((Control)picYhYes).set_Location(new Point(468, 29));
			((Control)picYhYes).set_Name("picYhYes");
			((Control)picYhYes).set_Size(new Size(18, 18));
			picYhYes.set_SizeMode((PictureBoxSizeMode)1);
			picYhYes.set_TabIndex(78);
			picYhYes.set_TabStop(false);
			picYhDNo.set_Image((Image)Resources.error);
			((Control)picYhDNo).set_Location(new Point(606, 49));
			((Control)picYhDNo).set_Name("picYhDNo");
			((Control)picYhDNo).set_Size(new Size(18, 18));
			picYhDNo.set_SizeMode((PictureBoxSizeMode)1);
			picYhDNo.set_TabIndex(77);
			picYhDNo.set_TabStop(false);
			((Control)picYhDNo).set_Visible(false);
			picYhDYes.set_Image((Image)Resources.check);
			((Control)picYhDYes).set_Location(new Point(606, 29));
			((Control)picYhDYes).set_Name("picYhDYes");
			((Control)picYhDYes).set_Size(new Size(18, 18));
			picYhDYes.set_SizeMode((PictureBoxSizeMode)1);
			picYhDYes.set_TabIndex(76);
			picYhDYes.set_TabStop(false);
			picPAhNo.set_Image((Image)Resources.error);
			((Control)picPAhNo).set_Location(new Point(661, 49));
			((Control)picPAhNo).set_Name("picPAhNo");
			((Control)picPAhNo).set_Size(new Size(18, 18));
			picPAhNo.set_SizeMode((PictureBoxSizeMode)1);
			picPAhNo.set_TabIndex(75);
			picPAhNo.set_TabStop(false);
			((Control)picPAhNo).set_Visible(false);
			picPAhYes.set_Image((Image)Resources.check);
			((Control)picPAhYes).set_Location(new Point(662, 29));
			((Control)picPAhYes).set_Name("picPAhYes");
			((Control)picPAhYes).set_Size(new Size(18, 18));
			picPAhYes.set_SizeMode((PictureBoxSizeMode)1);
			picPAhYes.set_TabIndex(74);
			picPAhYes.set_TabStop(false);
			picXhNo.set_Image((Image)Resources.error);
			((Control)picXhNo).set_Location(new Point(421, 49));
			((Control)picXhNo).set_Name("picXhNo");
			((Control)picXhNo).set_Size(new Size(18, 18));
			picXhNo.set_SizeMode((PictureBoxSizeMode)1);
			picXhNo.set_TabIndex(73);
			picXhNo.set_TabStop(false);
			((Control)picXhNo).set_Visible(false);
			picXhYes.set_Image((Image)Resources.check);
			((Control)picXhYes).set_Location(new Point(421, 29));
			((Control)picXhYes).set_Name("picXhYes");
			((Control)picXhYes).set_Size(new Size(18, 18));
			picXhYes.set_SizeMode((PictureBoxSizeMode)1);
			picXhYes.set_TabIndex(72);
			picXhYes.set_TabStop(false);
			((Control)label35).set_AutoSize(true);
			((Control)label35).set_Location(new Point(313, 510));
			((Control)label35).set_Name("label35");
			((Control)label35).set_Size(new Size(32, 13));
			((Control)label35).set_TabIndex(92);
			((Control)label35).set_Text("Sepn");
			((Control)label36).set_AutoSize(true);
			((Control)label36).set_Location(new Point(323, 533));
			((Control)label36).set_Name("label36");
			((Control)label36).set_Size(new Size(21, 13));
			((Control)label36).set_TabIndex(91);
			((Control)label36).set_Text("PA");
			picSepeNo.set_Image((Image)Resources.error);
			((Control)picSepeNo).set_Location(new Point(369, 508));
			((Control)picSepeNo).set_Name("picSepeNo");
			((Control)picSepeNo).set_Size(new Size(18, 18));
			picSepeNo.set_SizeMode((PictureBoxSizeMode)1);
			picSepeNo.set_TabIndex(90);
			picSepeNo.set_TabStop(false);
			((Control)picSepeNo).set_Visible(false);
			picSepeYes.set_Image((Image)Resources.check);
			((Control)picSepeYes).set_Location(new Point(346, 508));
			((Control)picSepeYes).set_Name("picSepeYes");
			((Control)picSepeYes).set_Size(new Size(18, 18));
			picSepeYes.set_SizeMode((PictureBoxSizeMode)1);
			picSepeYes.set_TabIndex(89);
			picSepeYes.set_TabStop(false);
			picDbPAeNo.set_Image((Image)Resources.error);
			((Control)picDbPAeNo).set_Location(new Point(369, 531));
			((Control)picDbPAeNo).set_Name("picDbPAeNo");
			((Control)picDbPAeNo).set_Size(new Size(18, 18));
			picDbPAeNo.set_SizeMode((PictureBoxSizeMode)1);
			picDbPAeNo.set_TabIndex(88);
			picDbPAeNo.set_TabStop(false);
			((Control)picDbPAeNo).set_Visible(false);
			picDbPAeYes.set_Image((Image)Resources.check);
			((Control)picDbPAeYes).set_Location(new Point(346, 530));
			((Control)picDbPAeYes).set_Name("picDbPAeYes");
			((Control)picDbPAeYes).set_Size(new Size(18, 18));
			picDbPAeYes.set_SizeMode((PictureBoxSizeMode)1);
			picDbPAeYes.set_TabIndex(87);
			picDbPAeYes.set_TabStop(false);
			((Control)label37).set_AutoSize(true);
			((Control)label37).set_Location(new Point(693, 511));
			((Control)label37).set_Name("label37");
			((Control)label37).set_Size(new Size(32, 13));
			((Control)label37).set_TabIndex(98);
			((Control)label37).set_Text("Sepn");
			((Control)label38).set_AutoSize(true);
			((Control)label38).set_Location(new Point(703, 536));
			((Control)label38).set_Name("label38");
			((Control)label38).set_Size(new Size(21, 13));
			((Control)label38).set_TabIndex(97);
			((Control)label38).set_Text("PA");
			picSephNo.set_Image((Image)Resources.error);
			((Control)picSephNo).set_Location(new Point(749, 509));
			((Control)picSephNo).set_Name("picSephNo");
			((Control)picSephNo).set_Size(new Size(18, 18));
			picSephNo.set_SizeMode((PictureBoxSizeMode)1);
			picSephNo.set_TabIndex(96);
			picSephNo.set_TabStop(false);
			((Control)picSephNo).set_Visible(false);
			picSephYes.set_Image((Image)Resources.check);
			((Control)picSephYes).set_Location(new Point(726, 509));
			((Control)picSephYes).set_Name("picSephYes");
			((Control)picSephYes).set_Size(new Size(18, 18));
			picSephYes.set_SizeMode((PictureBoxSizeMode)1);
			picSephYes.set_TabIndex(95);
			picSephYes.set_TabStop(false);
			picDbPAhNo.set_Image((Image)Resources.error);
			((Control)picDbPAhNo).set_Location(new Point(749, 536));
			((Control)picDbPAhNo).set_Name("picDbPAhNo");
			((Control)picDbPAhNo).set_Size(new Size(18, 18));
			picDbPAhNo.set_SizeMode((PictureBoxSizeMode)1);
			picDbPAhNo.set_TabIndex(94);
			picDbPAhNo.set_TabStop(false);
			((Control)picDbPAhNo).set_Visible(false);
			picDbPAhYes.set_Image((Image)Resources.check);
			((Control)picDbPAhYes).set_Location(new Point(726, 533));
			((Control)picDbPAhYes).set_Name("picDbPAhYes");
			((Control)picDbPAhYes).set_Size(new Size(18, 18));
			picDbPAhYes.set_SizeMode((PictureBoxSizeMode)1);
			picDbPAhYes.set_TabIndex(93);
			picDbPAhYes.set_TabStop(false);
			((Control)groupBox1).get_Controls().Add((Control)(object)label25);
			((Control)groupBox1).get_Controls().Add((Control)(object)label24);
			((Control)groupBox1).get_Controls().Add((Control)(object)picMhNo);
			((Control)groupBox1).get_Controls().Add((Control)(object)picMhYes);
			((Control)groupBox1).get_Controls().Add((Control)(object)picMeNo);
			((Control)groupBox1).get_Controls().Add((Control)(object)picMeYes);
			((Control)groupBox1).get_Controls().Add((Control)(object)label20);
			((Control)groupBox1).get_Controls().Add((Control)(object)label31);
			((Control)groupBox1).get_Controls().Add((Control)(object)label32);
			((Control)groupBox1).get_Controls().Add((Control)(object)label33);
			((Control)groupBox1).get_Controls().Add((Control)(object)label34);
			((Control)groupBox1).get_Controls().Add((Control)(object)picXhDNo);
			((Control)groupBox1).get_Controls().Add((Control)(object)picXhDYes);
			((Control)groupBox1).get_Controls().Add((Control)(object)picYhNo);
			((Control)groupBox1).get_Controls().Add((Control)(object)picYhYes);
			((Control)groupBox1).get_Controls().Add((Control)(object)picYhDNo);
			((Control)groupBox1).get_Controls().Add((Control)(object)picYhDYes);
			((Control)groupBox1).get_Controls().Add((Control)(object)picPAhNo);
			((Control)groupBox1).get_Controls().Add((Control)(object)picPAhYes);
			((Control)groupBox1).get_Controls().Add((Control)(object)picXhNo);
			((Control)groupBox1).get_Controls().Add((Control)(object)picXhYes);
			((Control)groupBox1).get_Controls().Add((Control)(object)label19);
			((Control)groupBox1).get_Controls().Add((Control)(object)label17);
			((Control)groupBox1).get_Controls().Add((Control)(object)label16);
			((Control)groupBox1).get_Controls().Add((Control)(object)label15);
			((Control)groupBox1).get_Controls().Add((Control)(object)label14);
			((Control)groupBox1).get_Controls().Add((Control)(object)picXeDNo);
			((Control)groupBox1).get_Controls().Add((Control)(object)picXeDYes);
			((Control)groupBox1).get_Controls().Add((Control)(object)picYeNo);
			((Control)groupBox1).get_Controls().Add((Control)(object)picYeYes);
			((Control)groupBox1).get_Controls().Add((Control)(object)picYeDNo);
			((Control)groupBox1).get_Controls().Add((Control)(object)picYeDYes);
			((Control)groupBox1).get_Controls().Add((Control)(object)picPAeNo);
			((Control)groupBox1).get_Controls().Add((Control)(object)picPAeYes);
			((Control)groupBox1).get_Controls().Add((Control)(object)picXeNo);
			((Control)groupBox1).get_Controls().Add((Control)(object)picXeYes);
			((Control)groupBox1).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox1).set_Location(new Point(10, 212));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(755, 60));
			((Control)groupBox1).set_TabIndex(99);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Solve flags");
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label25).set_Location(new Point(693, 31));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(33, 15));
			((Control)label25).set_TabIndex(92);
			((Control)label25).set_Text("Miss");
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label24).set_Location(new Point(297, 31));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(33, 15));
			((Control)label24).set_TabIndex(91);
			((Control)label24).set_Text("Miss");
			picMhNo.set_Image((Image)Resources.error);
			((Control)picMhNo).set_Location(new Point(728, 49));
			((Control)picMhNo).set_Name("picMhNo");
			((Control)picMhNo).set_Size(new Size(18, 18));
			picMhNo.set_SizeMode((PictureBoxSizeMode)1);
			picMhNo.set_TabIndex(90);
			picMhNo.set_TabStop(false);
			((Control)picMhNo).set_Visible(false);
			picMhYes.set_Image((Image)Resources.check);
			((Control)picMhYes).set_Location(new Point(728, 29));
			((Control)picMhYes).set_Name("picMhYes");
			((Control)picMhYes).set_Size(new Size(18, 18));
			picMhYes.set_SizeMode((PictureBoxSizeMode)1);
			picMhYes.set_TabIndex(89);
			picMhYes.set_TabStop(false);
			picMeNo.set_Image((Image)Resources.error);
			((Control)picMeNo).set_Location(new Point(332, 49));
			((Control)picMeNo).set_Name("picMeNo");
			((Control)picMeNo).set_Size(new Size(18, 18));
			picMeNo.set_SizeMode((PictureBoxSizeMode)1);
			picMeNo.set_TabIndex(88);
			picMeNo.set_TabStop(false);
			((Control)picMeNo).set_Visible(false);
			picMeYes.set_Image((Image)Resources.check);
			((Control)picMeYes).set_Location(new Point(332, 29));
			((Control)picMeYes).set_Name("picMeYes");
			((Control)picMeYes).set_Size(new Size(18, 18));
			picMeYes.set_SizeMode((PictureBoxSizeMode)1);
			picMeYes.set_TabIndex(87);
			picMeYes.set_TabStop(false);
			((Control)groupBox2).get_Controls().Add((Control)(object)lblFh);
			((Control)groupBox2).get_Controls().Add((Control)(object)lblFith);
			((Control)groupBox2).get_Controls().Add((Control)(object)lblFe);
			((Control)groupBox2).get_Controls().Add((Control)(object)lblFite);
			((Control)groupBox2).get_Controls().Add((Control)(object)picCeNo);
			((Control)groupBox2).get_Controls().Add((Control)(object)picCeYes);
			((Control)groupBox2).get_Controls().Add((Control)(object)picChNo);
			((Control)groupBox2).get_Controls().Add((Control)(object)picChYes);
			((Control)groupBox2).get_Controls().Add((Control)(object)label23);
			((Control)groupBox2).get_Controls().Add((Control)(object)label22);
			((Control)groupBox2).get_Controls().Add((Control)(object)label21);
			((Control)groupBox2).get_Controls().Add((Control)(object)picADhNo);
			((Control)groupBox2).get_Controls().Add((Control)(object)picADhYes);
			((Control)groupBox2).get_Controls().Add((Control)(object)label18);
			((Control)groupBox2).get_Controls().Add((Control)(object)picADeNo);
			((Control)groupBox2).get_Controls().Add((Control)(object)picADeYes);
			((Control)groupBox2).get_Controls().Add((Control)(object)label13);
			((Control)groupBox2).get_Controls().Add((Control)(object)picSMhNo);
			((Control)groupBox2).get_Controls().Add((Control)(object)picSMhYes);
			((Control)groupBox2).get_Controls().Add((Control)(object)label1);
			((Control)groupBox2).get_Controls().Add((Control)(object)picSMeNo);
			((Control)groupBox2).get_Controls().Add((Control)(object)picSMeYes);
			((Control)groupBox2).get_Controls().Add((Control)(object)labeld);
			((Control)groupBox2).get_Controls().Add((Control)(object)labelc);
			((Control)groupBox2).get_Controls().Add((Control)(object)labelb);
			((Control)groupBox2).get_Controls().Add((Control)(object)labela);
			((Control)groupBox2).get_Controls().Add((Control)(object)lblYhD);
			((Control)groupBox2).get_Controls().Add((Control)(object)lblXhD);
			((Control)groupBox2).get_Controls().Add((Control)(object)lblYeD);
			((Control)groupBox2).get_Controls().Add((Control)(object)lblXeD);
			((Control)groupBox2).get_Controls().Add((Control)(object)lblPAh);
			((Control)groupBox2).get_Controls().Add((Control)(object)lblYh);
			((Control)groupBox2).get_Controls().Add((Control)(object)lblXh);
			((Control)groupBox2).get_Controls().Add((Control)(object)lblPAe);
			((Control)groupBox2).get_Controls().Add((Control)(object)lblYe);
			((Control)groupBox2).get_Controls().Add((Control)(object)lblXe);
			((Control)groupBox2).get_Controls().Add((Control)(object)label7);
			((Control)groupBox2).get_Controls().Add((Control)(object)lblEvY);
			((Control)groupBox2).get_Controls().Add((Control)(object)lblEvX);
			((Control)groupBox2).get_Controls().Add((Control)(object)label6);
			((Control)groupBox2).get_Controls().Add((Control)(object)lblExY);
			((Control)groupBox2).get_Controls().Add((Control)(object)lblExX);
			((Control)groupBox2).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox2).set_Location(new Point(9, 70));
			((Control)groupBox2).set_Name("groupBox2");
			((Control)groupBox2).set_Size(new Size(755, 107));
			((Control)groupBox2).set_TabIndex(100);
			groupBox2.set_TabStop(false);
			((Control)groupBox2).set_Text("Fit of ellipse");
			picCeNo.set_Image((Image)Resources.error);
			((Control)picCeNo).set_Location(new Point(83, 84));
			((Control)picCeNo).set_Name("picCeNo");
			((Control)picCeNo).set_Size(new Size(18, 18));
			picCeNo.set_SizeMode((PictureBoxSizeMode)1);
			picCeNo.set_TabIndex(110);
			picCeNo.set_TabStop(false);
			((Control)picCeNo).set_Visible(false);
			picCeYes.set_Image((Image)Resources.check);
			((Control)picCeYes).set_Location(new Point(59, 84));
			((Control)picCeYes).set_Name("picCeYes");
			((Control)picCeYes).set_Size(new Size(18, 18));
			picCeYes.set_SizeMode((PictureBoxSizeMode)1);
			picCeYes.set_TabIndex(109);
			picCeYes.set_TabStop(false);
			picChNo.set_Image((Image)Resources.error);
			((Control)picChNo).set_Location(new Point(479, 84));
			((Control)picChNo).set_Name("picChNo");
			((Control)picChNo).set_Size(new Size(18, 18));
			picChNo.set_SizeMode((PictureBoxSizeMode)1);
			picChNo.set_TabIndex(108);
			picChNo.set_TabStop(false);
			((Control)picChNo).set_Visible(false);
			picChYes.set_Image((Image)Resources.check);
			((Control)picChYes).set_Location(new Point(455, 84));
			((Control)picChYes).set_Name("picChYes");
			((Control)picChYes).set_Size(new Size(18, 18));
			picChYes.set_SizeMode((PictureBoxSizeMode)1);
			picChYes.set_TabIndex(107);
			picChYes.set_TabStop(false);
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label23).set_Location(new Point(404, 84));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(49, 15));
			((Control)label23).set_TabIndex(106);
			((Control)label23).set_Text("Circular");
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label22).set_Location(new Point(8, 84));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(49, 15));
			((Control)label22).set_TabIndex(105);
			((Control)label22).set_Text("Circular");
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label21).set_Location(new Point(544, 83));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(141, 15));
			((Control)label21).set_TabIndex(104);
			((Control)label21).set_Text("Uses assumed diameter");
			picADhNo.set_Image((Image)Resources.error);
			((Control)picADhNo).set_Location(new Point(710, 84));
			((Control)picADhNo).set_Name("picADhNo");
			((Control)picADhNo).set_Size(new Size(18, 18));
			picADhNo.set_SizeMode((PictureBoxSizeMode)1);
			picADhNo.set_TabIndex(103);
			picADhNo.set_TabStop(false);
			((Control)picADhNo).set_Visible(false);
			picADhYes.set_Image((Image)Resources.check);
			((Control)picADhYes).set_Location(new Point(687, 84));
			((Control)picADhYes).set_Name("picADhYes");
			((Control)picADhYes).set_Size(new Size(18, 18));
			picADhYes.set_SizeMode((PictureBoxSizeMode)1);
			picADhYes.set_TabIndex(102);
			picADhYes.set_TabStop(false);
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label18).set_Location(new Point(145, 83));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(141, 15));
			((Control)label18).set_TabIndex(101);
			((Control)label18).set_Text("Uses assumed diameter");
			picADeNo.set_Image((Image)Resources.error);
			((Control)picADeNo).set_Location(new Point(311, 84));
			((Control)picADeNo).set_Name("picADeNo");
			((Control)picADeNo).set_Size(new Size(18, 18));
			picADeNo.set_SizeMode((PictureBoxSizeMode)1);
			picADeNo.set_TabIndex(100);
			picADeNo.set_TabStop(false);
			((Control)picADeNo).set_Visible(false);
			picADeYes.set_Image((Image)Resources.check);
			((Control)picADeYes).set_Location(new Point(288, 84));
			((Control)picADeYes).set_Name("picADeYes");
			((Control)picADeYes).set_Size(new Size(18, 18));
			picADeYes.set_SizeMode((PictureBoxSizeMode)1);
			picADeYes.set_TabIndex(99);
			picADeYes.set_TabStop(false);
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label13).set_Location(new Point(553, 65));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(132, 15));
			((Control)label13).set_TabIndex(98);
			((Control)label13).set_Text("Shape model centered");
			picSMhNo.set_Image((Image)Resources.error);
			((Control)picSMhNo).set_Location(new Point(711, 64));
			((Control)picSMhNo).set_Name("picSMhNo");
			((Control)picSMhNo).set_Size(new Size(18, 18));
			picSMhNo.set_SizeMode((PictureBoxSizeMode)1);
			picSMhNo.set_TabIndex(97);
			picSMhNo.set_TabStop(false);
			((Control)picSMhNo).set_Visible(false);
			picSMhYes.set_Image((Image)Resources.check);
			((Control)picSMhYes).set_Location(new Point(687, 64));
			((Control)picSMhYes).set_Name("picSMhYes");
			((Control)picSMhYes).set_Size(new Size(18, 18));
			picSMhYes.set_SizeMode((PictureBoxSizeMode)1);
			picSMhYes.set_TabIndex(96);
			picSMhYes.set_TabStop(false);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(154, 65));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(132, 15));
			((Control)label1).set_TabIndex(95);
			((Control)label1).set_Text("Shape model centered");
			picSMeNo.set_Image((Image)Resources.error);
			((Control)picSMeNo).set_Location(new Point(311, 64));
			((Control)picSMeNo).set_Name("picSMeNo");
			((Control)picSMeNo).set_Size(new Size(18, 18));
			picSMeNo.set_SizeMode((PictureBoxSizeMode)1);
			picSMeNo.set_TabIndex(94);
			picSMeNo.set_TabStop(false);
			((Control)picSMeNo).set_Visible(false);
			picSMeYes.set_Image((Image)Resources.check);
			((Control)picSMeYes).set_Location(new Point(288, 64));
			((Control)picSMeYes).set_Name("picSMeYes");
			((Control)picSMeYes).set_Size(new Size(18, 18));
			picSMeYes.set_SizeMode((PictureBoxSizeMode)1);
			picSMeYes.set_TabIndex(93);
			picSMeYes.set_TabStop(false);
			((Control)label26).set_AutoSize(true);
			((Control)label26).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label26).set_Location(new Point(301, 486));
			((Control)label26).set_Name("label26");
			((Control)label26).set_Size(new Size(88, 17));
			((Control)label26).set_TabIndex(101);
			((Control)label26).set_Text("Solve flags");
			((Control)label27).set_AutoSize(true);
			((Control)label27).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label27).set_Location(new Point(681, 486));
			((Control)label27).set_Name("label27");
			((Control)label27).set_Size(new Size(88, 17));
			((Control)label27).set_TabIndex(102);
			((Control)label27).set_Text("Solve flags");
			((Control)chkEllipse).set_AutoSize(true);
			((Control)chkEllipse).set_BackColor(Color.RoyalBlue);
			chkEllipse.set_Checked(true);
			chkEllipse.set_CheckState((CheckState)1);
			((Control)chkEllipse).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkEllipse).set_ForeColor(Color.Yellow);
			((Control)chkEllipse).set_Location(new Point(310, 178));
			((Control)chkEllipse).set_Name("chkEllipse");
			((Control)chkEllipse).set_Size(new Size(139, 21));
			((Control)chkEllipse).set_TabIndex(103);
			((Control)chkEllipse).set_Text("Apply Ellipse fit");
			((ButtonBase)chkEllipse).set_UseVisualStyleBackColor(false);
			((Control)chkSolutions).set_AutoSize(true);
			((Control)chkSolutions).set_BackColor(Color.RoyalBlue);
			chkSolutions.set_Checked(true);
			chkSolutions.set_CheckState((CheckState)1);
			((Control)chkSolutions).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkSolutions).set_ForeColor(Color.Yellow);
			((Control)chkSolutions).set_Location(new Point(310, 276));
			((Control)chkSolutions).set_Name("chkSolutions");
			((Control)chkSolutions).set_Size(new Size(169, 21));
			((Control)chkSolutions).set_TabIndex(104);
			((Control)chkSolutions).set_Text("Apply solution flags");
			((ButtonBase)chkSolutions).set_UseVisualStyleBackColor(false);
			((Control)chkShapeModels).set_AutoSize(true);
			((Control)chkShapeModels).set_BackColor(Color.RoyalBlue);
			chkShapeModels.set_Checked(true);
			chkShapeModels.set_CheckState((CheckState)1);
			((Control)chkShapeModels).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkShapeModels).set_ForeColor(Color.Yellow);
			((Control)chkShapeModels).set_Location(new Point(310, 438));
			((Control)chkShapeModels).set_Name("chkShapeModels");
			((Control)chkShapeModels).set_Size(new Size(193, 21));
			((Control)chkShapeModels).set_TabIndex(105);
			((Control)chkShapeModels).set_Text("Apply Shape model fits");
			((ButtonBase)chkShapeModels).set_UseVisualStyleBackColor(false);
			((Control)chkDoubles).set_AutoSize(true);
			((Control)chkDoubles).set_BackColor(Color.RoyalBlue);
			chkDoubles.set_Checked(true);
			chkDoubles.set_CheckState((CheckState)1);
			((Control)chkDoubles).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkDoubles).set_ForeColor(Color.Yellow);
			((Control)chkDoubles).set_Location(new Point(310, 559));
			((Control)chkDoubles).set_Name("chkDoubles");
			((Control)chkDoubles).set_Size(new Size(226, 21));
			((Control)chkDoubles).set_TabIndex(106);
			((Control)chkDoubles).set_Text("Apply Double star solutions");
			((ButtonBase)chkDoubles).set_UseVisualStyleBackColor(false);
			((Control)chkSatellite).set_AutoSize(true);
			((Control)chkSatellite).set_BackColor(Color.RoyalBlue);
			chkSatellite.set_Checked(true);
			chkSatellite.set_CheckState((CheckState)1);
			((Control)chkSatellite).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkSatellite).set_ForeColor(Color.Yellow);
			((Control)chkSatellite).set_Location(new Point(310, 670));
			((Control)chkSatellite).set_Name("chkSatellite");
			((Control)chkSatellite).set_Size(new Size(148, 21));
			((Control)chkSatellite).set_TabIndex(107);
			((Control)chkSatellite).set_Text("Apply satellite fit");
			((ButtonBase)chkSatellite).set_UseVisualStyleBackColor(false);
			((Control)lblExport).set_AutoSize(true);
			((Control)lblExport).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblExport).set_Location(new Point(390, 696));
			((Control)lblExport).set_Name("lblExport");
			((Control)lblExport).set_Size(new Size(286, 51));
			((Control)lblExport).set_TabIndex(108);
			((Control)lblExport).set_Text("Do you want to replace the data in the\r\nExport file with the Event data in\r\nthe Editor?");
			((Control)lblImport).set_AutoSize(true);
			((Control)lblImport).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblImport).set_Location(new Point(188, 591));
			((Control)lblImport).set_Name("lblImport");
			((Control)lblImport).set_Size(new Size(291, 51));
			((Control)lblImport).set_TabIndex(109);
			((Control)lblImport).set_Text("Do you want to replace the Event data \r\nin the Editor with the items that have \r\nbeen checked?");
			((Control)label28).set_AutoSize(true);
			((Control)label28).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold | FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label28).set_Location(new Point(11, 696));
			((Control)label28).set_Name("label28");
			((Control)label28).set_Size(new Size(292, 30));
			((Control)label28).set_TabIndex(110);
			((Control)label28).set_Text("Event data in the Editor will only be replaced\r\nby data groups that have been checked");
			((Control)label29).set_AutoSize(true);
			((Control)label29).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label29).set_Location(new Point(73, 41));
			((Control)label29).set_Name("label29");
			((Control)label29).set_Size(new Size(94, 17));
			((Control)label29).set_TabIndex(111);
			((Control)label29).set_Text("Last edited ");
			((Control)label30).set_AutoSize(true);
			((Control)label30).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label30).set_Location(new Point(442, 41));
			((Control)label30).set_Name("label30");
			((Control)label30).set_Size(new Size(94, 17));
			((Control)label30).set_TabIndex(112);
			((Control)label30).set_Text("Last edited ");
			((Control)lblExportLastEdit).set_AutoSize(true);
			((Control)lblExportLastEdit).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblExportLastEdit).set_ForeColor(Color.Maroon);
			((Control)lblExportLastEdit).set_Location(new Point(173, 41));
			((Control)lblExportLastEdit).set_Name("lblExportLastEdit");
			((Control)lblExportLastEdit).set_Size(new Size(38, 17));
			((Control)lblExportLastEdit).set_TabIndex(113);
			((Control)lblExportLastEdit).set_Text("-----");
			((Control)lblEditorLastEdit).set_AutoSize(true);
			((Control)lblEditorLastEdit).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblEditorLastEdit).set_ForeColor(Color.DarkBlue);
			((Control)lblEditorLastEdit).set_Location(new Point(540, 41));
			((Control)lblEditorLastEdit).set_Name("lblEditorLastEdit");
			((Control)lblEditorLastEdit).set_Size(new Size(38, 17));
			((Control)lblEditorLastEdit).set_TabIndex(114);
			((Control)lblEditorLastEdit).set_Text("-----");
			((Control)lblAsteroid).set_AutoSize(true);
			((Control)lblAsteroid).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblAsteroid).set_ForeColor(Color.DarkGreen);
			((Control)lblAsteroid).set_Location(new Point(305, 54));
			((Control)lblAsteroid).set_Name("lblAsteroid");
			((Control)lblAsteroid).set_Size(new Size(76, 17));
			((Control)lblAsteroid).set_TabIndex(115);
			((Control)lblAsteroid).set_Text("(1) Ceres");
			((Control)lblFite).set_AutoSize(true);
			((Control)lblFite).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblFite).set_Location(new Point(131, 12));
			((Control)lblFite).set_Name("lblFite");
			((Control)lblFite).set_Size(new Size(71, 15));
			((Control)lblFite).set_TabIndex(111);
			((Control)lblFite).set_Text("Fit quality = ");
			((Control)lblFe).set_AutoSize(true);
			((Control)lblFe).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblFe).set_Location(new Point(198, 12));
			((Control)lblFe).set_Name("lblFe");
			((Control)lblFe).set_Size(new Size(14, 15));
			((Control)lblFe).set_TabIndex(112);
			((Control)lblFe).set_Text("0");
			((Control)lblFith).set_AutoSize(true);
			((Control)lblFith).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblFith).set_Location(new Point(395, 12));
			((Control)lblFith).set_Name("lblFith");
			((Control)lblFith).set_Size(new Size(71, 15));
			((Control)lblFith).set_TabIndex(113);
			((Control)lblFith).set_Text("Fit quality = ");
			((Control)lblFh).set_AutoSize(true);
			((Control)lblFh).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblFh).set_Location(new Point(461, 12));
			((Control)lblFh).set_Name("lblFh");
			((Control)lblFh).set_Size(new Size(14, 15));
			((Control)lblFh).set_TabIndex(114);
			((Control)lblFh).set_Text("0");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(771, 752));
			((Form)this).set_ControlBox(false);
			((Control)this).get_Controls().Add((Control)(object)lblAsteroid);
			((Control)this).get_Controls().Add((Control)(object)lblEditorLastEdit);
			((Control)this).get_Controls().Add((Control)(object)lblExportLastEdit);
			((Control)this).get_Controls().Add((Control)(object)label30);
			((Control)this).get_Controls().Add((Control)(object)label29);
			((Control)this).get_Controls().Add((Control)(object)label28);
			((Control)this).get_Controls().Add((Control)(object)lblImport);
			((Control)this).get_Controls().Add((Control)(object)lblExport);
			((Control)this).get_Controls().Add((Control)(object)chkSatellite);
			((Control)this).get_Controls().Add((Control)(object)chkDoubles);
			((Control)this).get_Controls().Add((Control)(object)chkShapeModels);
			((Control)this).get_Controls().Add((Control)(object)chkSolutions);
			((Control)this).get_Controls().Add((Control)(object)chkEllipse);
			((Control)this).get_Controls().Add((Control)(object)label27);
			((Control)this).get_Controls().Add((Control)(object)label26);
			((Control)this).get_Controls().Add((Control)(object)groupBox2);
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Control)this).get_Controls().Add((Control)(object)label37);
			((Control)this).get_Controls().Add((Control)(object)label38);
			((Control)this).get_Controls().Add((Control)(object)picSephNo);
			((Control)this).get_Controls().Add((Control)(object)picSephYes);
			((Control)this).get_Controls().Add((Control)(object)picDbPAhNo);
			((Control)this).get_Controls().Add((Control)(object)picDbPAhYes);
			((Control)this).get_Controls().Add((Control)(object)label35);
			((Control)this).get_Controls().Add((Control)(object)label36);
			((Control)this).get_Controls().Add((Control)(object)picSepeNo);
			((Control)this).get_Controls().Add((Control)(object)picSepeYes);
			((Control)this).get_Controls().Add((Control)(object)picDbPAeNo);
			((Control)this).get_Controls().Add((Control)(object)picDbPAeYes);
			((Control)this).get_Controls().Add((Control)(object)lstExportSatellite);
			((Control)this).get_Controls().Add((Control)(object)lstHistorySatellite);
			((Control)this).get_Controls().Add((Control)(object)label12);
			((Control)this).get_Controls().Add((Control)(object)label11);
			((Control)this).get_Controls().Add((Control)(object)lstHistoryDouble);
			((Control)this).get_Controls().Add((Control)(object)lstExportDouble);
			((Control)this).get_Controls().Add((Control)(object)label10);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_Controls().Add((Control)(object)cmdOK);
			((Control)this).get_Controls().Add((Control)(object)lstHistoryShape);
			((Control)this).get_Controls().Add((Control)(object)lstExportShape);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Control)this).set_Name("CompareSolutions");
			((Control)this).set_Text("Compare Solutions for");
			((Form)this).add_Load((EventHandler)CompareSolutions_Load);
			((ISupportInitialize)picXeYes).EndInit();
			((ISupportInitialize)picXeNo).EndInit();
			((ISupportInitialize)picPAeNo).EndInit();
			((ISupportInitialize)picPAeYes).EndInit();
			((ISupportInitialize)picYeDNo).EndInit();
			((ISupportInitialize)picYeDYes).EndInit();
			((ISupportInitialize)picYeNo).EndInit();
			((ISupportInitialize)picYeYes).EndInit();
			((ISupportInitialize)picXeDNo).EndInit();
			((ISupportInitialize)picXeDYes).EndInit();
			((ISupportInitialize)picXhDNo).EndInit();
			((ISupportInitialize)picXhDYes).EndInit();
			((ISupportInitialize)picYhNo).EndInit();
			((ISupportInitialize)picYhYes).EndInit();
			((ISupportInitialize)picYhDNo).EndInit();
			((ISupportInitialize)picYhDYes).EndInit();
			((ISupportInitialize)picPAhNo).EndInit();
			((ISupportInitialize)picPAhYes).EndInit();
			((ISupportInitialize)picXhNo).EndInit();
			((ISupportInitialize)picXhYes).EndInit();
			((ISupportInitialize)picSepeNo).EndInit();
			((ISupportInitialize)picSepeYes).EndInit();
			((ISupportInitialize)picDbPAeNo).EndInit();
			((ISupportInitialize)picDbPAeYes).EndInit();
			((ISupportInitialize)picSephNo).EndInit();
			((ISupportInitialize)picSephYes).EndInit();
			((ISupportInitialize)picDbPAhNo).EndInit();
			((ISupportInitialize)picDbPAhYes).EndInit();
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((ISupportInitialize)picMhNo).EndInit();
			((ISupportInitialize)picMhYes).EndInit();
			((ISupportInitialize)picMeNo).EndInit();
			((ISupportInitialize)picMeYes).EndInit();
			((Control)groupBox2).ResumeLayout(false);
			((Control)groupBox2).PerformLayout();
			((ISupportInitialize)picCeNo).EndInit();
			((ISupportInitialize)picCeYes).EndInit();
			((ISupportInitialize)picChNo).EndInit();
			((ISupportInitialize)picChYes).EndInit();
			((ISupportInitialize)picADhNo).EndInit();
			((ISupportInitialize)picADhYes).EndInit();
			((ISupportInitialize)picADeNo).EndInit();
			((ISupportInitialize)picADeYes).EndInit();
			((ISupportInitialize)picSMhNo).EndInit();
			((ISupportInitialize)picSMhYes).EndInit();
			((ISupportInitialize)picSMeNo).EndInit();
			((ISupportInitialize)picSMeYes).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
