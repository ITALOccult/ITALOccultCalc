using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.MPC_PDS
{
	public class PDS_Headers : Form
	{
		private static DataSet dtSet = new DataSet();

		internal static DataTable HeaderTable = new DataTable("Headers");

		internal DataColumn dtColumn;

		internal static DataRow myDataRow;

		private static List<GridContent> GCList = new List<GridContent>();

		private static GridContent GC;

		private HelpNavigator navigator = (HelpNavigator)(-2147483642);

		private DisplayData DD;

		private static string CurrentFile = "HeaderAsteroids";

		private static bool CurrentFileSaved = true;

		private IContainer components;

		private Button cmdSaveHeader;

		private Button cmdMove;

		private DataGridView dataGridView;

		private Button cmdInsert;

		private Button cmdDelete;

		private GroupBox grpHeader;

		private RadioButton optPlanetSummary;

		private RadioButton optPlanets;

		private RadioButton optTimes;

		private RadioButton optAsteroidSummary;

		private RadioButton optAsteroids;

		private Button cmdLoad;

		private NumericUpDown updnFrom;

		private NumericUpDown updnTo;

		private Label label1;

		private Label label2;

		private GroupBox groupBox1;

		private Panel panel2;

		private RadioButton optSaveEdited;

		private RadioButton optSaveForUse;

		private Panel panel1;

		private RadioButton optOpenEdited;

		private RadioButton optOpenForUse;

		private GroupBox groupBox2;

		private Button cmdDownloadHeaders;

		private Panel panel3;

		private Button cmdDuplicates;

		private GroupBox groupBox3;

		private Label label3;

		private Button cmdHelp;

		private RadioButton optAstrometry;

		private RadioButton optDiameters;

		private Button cmdExit;

		private RadioButton optDoubles;

		private RadioButton optSatellites;

		public PDS_Headers()
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			//IL_02ef: Unknown result type (might be due to invalid IL or missing references)
			//IL_02f5: Expected O, but got Unknown
			InitializeComponent();
			HeaderTable.Columns.Clear();
			dtColumn = new DataColumn();
			dtColumn.DataType = typeof(short);
			dtColumn.ColumnName = "Line";
			dtColumn.Caption = "Line";
			dtColumn.ReadOnly = true;
			dtColumn.Unique = false;
			HeaderTable.Columns.Add(dtColumn);
			dtColumn = new DataColumn();
			dtColumn.DataType = typeof(string);
			dtColumn.ColumnName = "Label";
			dtColumn.Caption = "Field Label";
			dtColumn.ReadOnly = false;
			dtColumn.Unique = false;
			HeaderTable.Columns.Add(dtColumn);
			dtColumn = new DataColumn();
			dtColumn.DataType = typeof(string);
			dtColumn.ColumnName = "Format";
			dtColumn.Caption = "Field Format";
			dtColumn.AutoIncrement = false;
			dtColumn.ReadOnly = false;
			dtColumn.Unique = false;
			HeaderTable.Columns.Add(dtColumn);
			dtColumn = new DataColumn();
			dtColumn.DataType = typeof(string);
			dtColumn.ColumnName = "Precision";
			dtColumn.Caption = "Precis %xx.nf";
			dtColumn.AutoIncrement = false;
			dtColumn.ReadOnly = false;
			dtColumn.Unique = false;
			HeaderTable.Columns.Add(dtColumn);
			dtColumn = new DataColumn();
			dtColumn.DataType = typeof(string);
			dtColumn.ColumnName = "Unit";
			dtColumn.Caption = "Field Unit";
			dtColumn.ReadOnly = false;
			dtColumn.Unique = false;
			HeaderTable.Columns.Add(dtColumn);
			dtColumn = new DataColumn();
			dtColumn.DataType = typeof(string);
			dtColumn.ColumnName = "Description";
			dtColumn.Caption = "Field Description";
			dtColumn.ReadOnly = false;
			dtColumn.Unique = false;
			HeaderTable.Columns.Add(dtColumn);
			dtSet.Tables.Clear();
			dtSet.Tables.Add(HeaderTable);
			BindingSource val = new BindingSource();
			val.set_DataSource((object)dtSet.Tables["Headers"]);
			dataGridView.set_DataSource((object)val);
			dataGridView.set_AllowUserToResizeColumns(true);
			dataGridView.get_Columns().get_Item(0).set_Width(40);
			dataGridView.get_Columns().get_Item(1).set_Width(210);
			dataGridView.get_Columns().get_Item(2).set_Width(125);
			dataGridView.get_Columns().get_Item(3).set_Width(70);
			dataGridView.get_Columns().get_Item(4).set_Width(60);
			dataGridView.get_Columns().get_Item(5).set_Width(5000);
			dataGridView.set_AllowUserToAddRows(true);
			dataGridView.set_AllowUserToDeleteRows(true);
		}

		private void PDS_Headers_Resize(object sender, EventArgs e)
		{
			if (((Control)this).get_Width() > 100)
			{
				((Control)dataGridView).set_Width(((Control)this).get_Width() - 40);
			}
			if (((Control)this).get_Height() > 250)
			{
				((Control)dataGridView).set_Height(((Control)this).get_Height() - 190);
			}
		}

		private void dataGridView_CellFormatting(object sender, DataGridViewCellFormattingEventArgs e)
		{
			if (e.get_RowIndex() < dataGridView.get_Rows().get_Count() - 2 && dataGridView.get_Rows().get_Item(e.get_RowIndex()).get_Cells()
				.get_Item(1)
				.get_Value()
				.ToString()!.Trim() == "")
			{
				((DataGridViewBand)dataGridView.get_Rows().get_Item(e.get_RowIndex() + 1)).get_DefaultCellStyle().set_BackColor(Color.Aqua);
			}
		}

		private void button1_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < dataGridView.get_Rows().get_Count() - 2; i++)
			{
				if (i < dataGridView.get_Rows().get_Count() - 2 && dataGridView.get_Rows().get_Item(i).get_Cells()
					.get_Item(1)
					.get_Value()
					.ToString()!.Trim() == "")
				{
					((DataGridViewBand)dataGridView.get_Rows().get_Item(i + 1)).get_DefaultCellStyle().set_BackColor(Color.Red);
				}
			}
		}

		internal void InitiateGridContent()
		{
			string text = PDS.PDSHeaderFilePath + CurrentFile;
			if (optOpenForUse.get_Checked())
			{
				text += ".txt";
			}
			else if (File.Exists(text + "_Rev.txt"))
			{
				text += "_Rev.txt";
			}
			else
			{
				text += ".txt";
				optOpenForUse.set_Checked(true);
				optOpenEdited.set_Checked(false);
			}
			using (StreamReader streamReader = new StreamReader(text))
			{
				int num = 0;
				GCList.Clear();
				do
				{
					num++;
					string Line = streamReader.ReadLine();
					PDS.AddFormatFieldLocation(ref Line);
					string[] array = Line.Split(new char[1] { ':' });
					int num2 = array.Length;
					GC = new GridContent();
					GC.SequenceNo = num;
					if (num2 > 0)
					{
						GC.Label = array[0];
					}
					if (num2 > 1)
					{
						GC.Format = array[1];
					}
					if (num2 > 2)
					{
						GC.Precision = array[2];
					}
					if (num2 > 3)
					{
						GC.Unit = array[3];
					}
					if (num2 > 4)
					{
						GC.Description = array[4];
					}
					GCList.Add(GC);
				}
				while (!streamReader.EndOfStream);
			}
			FillGrid_FromList();
			CurrentFileSaved = false;
			((Control)cmdSaveHeader).set_Enabled(true);
		}

		internal void FillGrid_FromList()
		{
			HeaderTable.Rows.Clear();
			for (int i = 0; i < GCList.Count; i++)
			{
				myDataRow = HeaderTable.NewRow();
				myDataRow["Line"] = GCList[i].SequenceNo;
				myDataRow["Label"] = GCList[i].Label;
				myDataRow["Format"] = GCList[i].Format;
				if (GCList[i].Format == "ASCII_Real")
				{
					myDataRow["Precision"] = GCList[i].Precision;
				}
				else
				{
					myDataRow["Precision"] = "";
				}
				myDataRow["Unit"] = GCList[i].Unit;
				myDataRow["Description"] = GCList[i].Description;
				HeaderTable.Rows.Add(myDataRow);
			}
			SetFormatCell_Color();
		}

		internal void SetFormatCell_Color()
		{
			for (int i = 0; i < dataGridView.get_Rows().get_Count(); i++)
			{
				try
				{
					if (dataGridView.get_Rows().get_Item(i).get_Cells()
						.get_Item(2)
						.get_Style()
						.get_BackColor() == Color.Aqua)
					{
						dataGridView.get_Rows().get_Item(i).get_Cells()
							.get_Item(3)
							.get_Style()
							.set_BackColor(Color.Aqua);
					}
					else if (dataGridView.get_Rows().get_Item(i).get_Cells()
						.get_Item(2)
						.get_Value()
						.ToString() == "ASCII_Real")
					{
						string text = dataGridView.get_Rows().get_Item(i).get_Cells()
							.get_Item(3)
							.get_Value()
							.ToString();
						bool flag = true;
						if (text.Length < 5)
						{
							flag = false;
						}
						else if (text.Substring(0, 1) != "%")
						{
							flag = false;
						}
						else if (text.Substring(text.Length - 1) != "f")
						{
							flag = false;
						}
						if (flag)
						{
							dataGridView.get_Rows().get_Item(i).get_Cells()
								.get_Item(3)
								.get_Style()
								.set_BackColor(SystemColors.Window);
						}
						else
						{
							dataGridView.get_Rows().get_Item(i).get_Cells()
								.get_Item(3)
								.get_Style()
								.set_BackColor(Color.LightGreen);
						}
					}
					else if (dataGridView.get_Rows().get_Item(i).get_Cells()
						.get_Item(2)
						.get_Value()
						.ToString()!.Length > 0 && i > 0)
					{
						dataGridView.get_Rows().get_Item(i).get_Cells()
							.get_Item(3)
							.get_Style()
							.set_BackColor(Color.MistyRose);
						dataGridView.get_Rows().get_Item(i).get_Cells()
							.get_Item(3)
							.set_Value((object)"");
					}
				}
				catch
				{
				}
			}
		}

		private void dataGridView_CellValueChanged(object sender, DataGridViewCellEventArgs e)
		{
			if ((e.get_ColumnIndex() == 2) | (e.get_ColumnIndex() == 3))
			{
				SetFormatCell_Color();
			}
		}

		internal void FillList_FromGrid()
		{
			GCList.Clear();
			for (int i = 0; i < HeaderTable.Rows.Count; i++)
			{
				GC = new GridContent();
				if (dataGridView.get_Rows().get_Item(i).get_Cells()
					.get_Item(0)
					.get_Value()
					.ToString()!.Length > 0)
				{
					GC.SequenceNo = Convert.ToInt32(dataGridView.get_Rows().get_Item(i).get_Cells()
						.get_Item(0)
						.get_Value());
				}
				else
				{
					GC.SequenceNo = i + 1;
				}
				GC.Label = dataGridView.get_Rows().get_Item(i).get_Cells()
					.get_Item(1)
					.get_Value()
					.ToString();
				GC.Format = dataGridView.get_Rows().get_Item(i).get_Cells()
					.get_Item(2)
					.get_Value()
					.ToString();
				GC.Precision = dataGridView.get_Rows().get_Item(i).get_Cells()
					.get_Item(3)
					.get_Value()
					.ToString();
				GC.Unit = dataGridView.get_Rows().get_Item(i).get_Cells()
					.get_Item(4)
					.get_Value()
					.ToString();
				GC.Description = dataGridView.get_Rows().get_Item(i).get_Cells()
					.get_Item(5)
					.get_Value()
					.ToString()!.Replace(":", " - ");
				GCList.Add(GC);
			}
			SetFormatCell_Color();
		}

		private bool DataSortedByLine()
		{
			//IL_00be: Unknown result type (might be due to invalid IL or missing references)
			for (int i = 0; i < HeaderTable.Rows.Count - 1; i++)
			{
				if (Convert.ToInt32(dataGridView.get_Rows().get_Item(i + 1).get_Cells()
					.get_Item(0)
					.get_Value()) <= Convert.ToInt32(dataGridView.get_Rows().get_Item(i).get_Cells()
					.get_Item(0)
					.get_Value()))
				{
					MessageBox.Show("Table is not sorted by Line\r\n\r\n [For first instance, see line numbered " + dataGridView.get_Rows().get_Item(i).get_Cells()
						.get_Item(0)
						.get_Value()?.ToString() + " at Row " + (i + 1) + "]\r\n\r\nThe data cannot be saved, or lines added, deleted or moved, until it is sorted by Line.", "NotSorted", (MessageBoxButtons)0);
					return false;
				}
			}
			return true;
		}

		private void cmdSaveHeader_Click(object sender, EventArgs e)
		{
			SaveHeaderFile();
		}

		private void SaveHeaderFile()
		{
			if (!DataSortedByLine())
			{
				return;
			}
			string text = PDS.PDSHeaderFilePath + CurrentFile;
			text = ((!optSaveEdited.get_Checked()) ? (text + ".txt") : (text + "_Rev.txt"));
			FillList_FromGrid();
			using (StreamWriter streamWriter = new StreamWriter(text))
			{
				for (int i = 0; i < GCList.Count; i++)
				{
					streamWriter.WriteLine(GCList[i].ToString());
				}
			}
			CurrentFileSaved = true;
		}

		private void cmdInsert_Click(object sender, EventArgs e)
		{
			if (DataSortedByLine())
			{
				InsertInList(dataGridView.get_CurrentCell().get_RowIndex() + 1);
			}
		}

		private void InsertInList(int LineToAdd)
		{
			if (LineToAdd > 0)
			{
				LineToAdd--;
			}
			FillList_FromGrid();
			GC = new GridContent();
			GCList.Insert(LineToAdd, GC);
			RenumberList();
			FillGrid_FromList();
			dataGridView.set_CurrentCell(dataGridView.get_Item(0, LineToAdd));
		}

		internal void cmdDelete_Click(object sender, EventArgs e)
		{
			if (DataSortedByLine())
			{
				DeleteFromList(dataGridView.get_CurrentCell().get_RowIndex());
			}
		}

		private void DeleteFromList(int LineToDelete)
		{
			FillList_FromGrid();
			GCList.RemoveAt(LineToDelete);
			RenumberList();
			FillGrid_FromList();
			dataGridView.set_CurrentCell(dataGridView.get_Item(0, LineToDelete));
		}

		private void cmdMove_Click(object sender, EventArgs e)
		{
			if (!DataSortedByLine())
			{
				return;
			}
			int num = (int)updnFrom.get_Value() - 1;
			int num2 = (int)updnTo.get_Value() - 1;
			int num3 = num2;
			if (num2 != num)
			{
				FillList_FromGrid();
				GCList.Insert(num2, GCList[num]);
				if (num2 < num)
				{
					num++;
				}
				GCList.RemoveAt(num);
				RenumberList();
				FillGrid_FromList();
				dataGridView.set_CurrentCell(dataGridView.get_Item(0, num3));
			}
		}

		private void RenumberList()
		{
			for (int i = 0; i < GCList.Count; i++)
			{
				GCList[i].SequenceNo = i + 1;
			}
		}

		private void cmdLoad_Click(object sender, EventArgs e)
		{
			if (optAsteroids.get_Checked())
			{
				CurrentFile = "HeaderAsteroids";
			}
			else if (optAsteroidSummary.get_Checked())
			{
				CurrentFile = "HeaderAsteroidsSummary";
			}
			else if (optPlanets.get_Checked())
			{
				CurrentFile = "HeaderPlanets";
			}
			else if (optPlanetSummary.get_Checked())
			{
				CurrentFile = "HeaderPlanetsSummary";
			}
			else if (optTimes.get_Checked())
			{
				CurrentFile = "HeaderTimes";
			}
			else if (optAstrometry.get_Checked())
			{
				CurrentFile = "HeaderAstrometry";
			}
			else if (optDiameters.get_Checked())
			{
				CurrentFile = "HeaderDiameters";
			}
			else if (optDoubles.get_Checked())
			{
				CurrentFile = "HeaderDoubles";
			}
			else if (optSatellites.get_Checked())
			{
				CurrentFile = "HeaderSatellites";
			}
			InitiateGridContent();
		}

		private void optOpenForUse_MouseClick(object sender, MouseEventArgs e)
		{
			ManageOpenChoice();
		}

		private void optOpenEdited_MouseClick(object sender, MouseEventArgs e)
		{
			ManageOpenChoice();
		}

		private void ManageOpenChoice()
		{
			if (!optOpenForUse.get_Checked())
			{
				optOpenForUse.set_Checked(true);
				optOpenEdited.set_Checked(false);
			}
			else
			{
				optOpenForUse.set_Checked(false);
				optOpenEdited.set_Checked(true);
			}
			optSaveEdited.set_Checked(true);
			optSaveForUse.set_Checked(false);
		}

		private void optSaveEdited_MouseClick(object sender, MouseEventArgs e)
		{
			ManageSaveChoice();
		}

		private void optSaveForUse_MouseClick(object sender, MouseEventArgs e)
		{
			ManageSaveChoice();
		}

		private void ManageSaveChoice()
		{
			if (!optSaveForUse.get_Checked())
			{
				optSaveForUse.set_Checked(true);
				optSaveEdited.set_Checked(false);
			}
			else
			{
				optSaveForUse.set_Checked(false);
				optSaveEdited.set_Checked(true);
			}
		}

		private void optAsteroids_MouseClick(object sender, MouseEventArgs e)
		{
			if (!optAsteroids.get_Checked() && CheckSaved())
			{
				optAsteroids.set_Checked(true);
				RadioButton obj = optAsteroidSummary;
				RadioButton obj2 = optPlanets;
				RadioButton obj3 = optPlanetSummary;
				RadioButton obj4 = optTimes;
				RadioButton obj5 = optAstrometry;
				RadioButton obj6 = optDiameters;
				RadioButton obj7 = optDoubles;
				bool flag;
				optSatellites.set_Checked(flag = false);
				bool flag2;
				obj7.set_Checked(flag2 = flag);
				bool flag3;
				obj6.set_Checked(flag3 = flag2);
				bool flag4;
				obj5.set_Checked(flag4 = flag3);
				bool flag5;
				obj4.set_Checked(flag5 = flag4);
				bool flag6;
				obj3.set_Checked(flag6 = flag5);
				bool @checked;
				obj2.set_Checked(@checked = flag6);
				obj.set_Checked(@checked);
				optSaveEdited.set_Checked(true);
				optSaveForUse.set_Checked(false);
			}
		}

		private void optAsteroidSummary_MouseClick(object sender, MouseEventArgs e)
		{
			if (!optAsteroidSummary.get_Checked() && CheckSaved())
			{
				optAsteroidSummary.set_Checked(true);
				RadioButton obj = optAsteroids;
				RadioButton obj2 = optPlanets;
				RadioButton obj3 = optPlanetSummary;
				RadioButton obj4 = optTimes;
				RadioButton obj5 = optAstrometry;
				RadioButton obj6 = optDiameters;
				RadioButton obj7 = optDoubles;
				bool flag;
				optSatellites.set_Checked(flag = false);
				bool flag2;
				obj7.set_Checked(flag2 = flag);
				bool flag3;
				obj6.set_Checked(flag3 = flag2);
				bool flag4;
				obj5.set_Checked(flag4 = flag3);
				bool flag5;
				obj4.set_Checked(flag5 = flag4);
				bool flag6;
				obj3.set_Checked(flag6 = flag5);
				bool @checked;
				obj2.set_Checked(@checked = flag6);
				obj.set_Checked(@checked);
				optSaveEdited.set_Checked(true);
				optSaveForUse.set_Checked(false);
			}
		}

		private void optPlanets_MouseClick(object sender, MouseEventArgs e)
		{
			if (!optPlanets.get_Checked() && CheckSaved())
			{
				optPlanets.set_Checked(true);
				RadioButton obj = optAsteroids;
				RadioButton obj2 = optAsteroidSummary;
				RadioButton obj3 = optPlanetSummary;
				RadioButton obj4 = optTimes;
				RadioButton obj5 = optAstrometry;
				RadioButton obj6 = optDiameters;
				RadioButton obj7 = optDoubles;
				bool flag;
				optSatellites.set_Checked(flag = false);
				bool flag2;
				obj7.set_Checked(flag2 = flag);
				bool flag3;
				obj6.set_Checked(flag3 = flag2);
				bool flag4;
				obj5.set_Checked(flag4 = flag3);
				bool flag5;
				obj4.set_Checked(flag5 = flag4);
				bool flag6;
				obj3.set_Checked(flag6 = flag5);
				bool @checked;
				obj2.set_Checked(@checked = flag6);
				obj.set_Checked(@checked);
				optSaveEdited.set_Checked(true);
				optSaveForUse.set_Checked(false);
			}
		}

		private void optPlanetSummary_MouseClick(object sender, MouseEventArgs e)
		{
			if (!optPlanetSummary.get_Checked() && CheckSaved())
			{
				optPlanetSummary.set_Checked(true);
				RadioButton obj = optAsteroids;
				RadioButton obj2 = optAsteroidSummary;
				RadioButton obj3 = optPlanets;
				RadioButton obj4 = optTimes;
				RadioButton obj5 = optAstrometry;
				RadioButton obj6 = optDiameters;
				RadioButton obj7 = optDoubles;
				bool flag;
				optSatellites.set_Checked(flag = false);
				bool flag2;
				obj7.set_Checked(flag2 = flag);
				bool flag3;
				obj6.set_Checked(flag3 = flag2);
				bool flag4;
				obj5.set_Checked(flag4 = flag3);
				bool flag5;
				obj4.set_Checked(flag5 = flag4);
				bool flag6;
				obj3.set_Checked(flag6 = flag5);
				bool @checked;
				obj2.set_Checked(@checked = flag6);
				obj.set_Checked(@checked);
				optSaveEdited.set_Checked(true);
				optSaveForUse.set_Checked(false);
			}
		}

		private void optTimes_MouseClick(object sender, MouseEventArgs e)
		{
			if (!optTimes.get_Checked() && CheckSaved())
			{
				optTimes.set_Checked(true);
				RadioButton obj = optAsteroids;
				RadioButton obj2 = optAsteroidSummary;
				RadioButton obj3 = optPlanets;
				RadioButton obj4 = optPlanetSummary;
				RadioButton obj5 = optAstrometry;
				RadioButton obj6 = optDiameters;
				RadioButton obj7 = optDoubles;
				bool flag;
				optSatellites.set_Checked(flag = false);
				bool flag2;
				obj7.set_Checked(flag2 = flag);
				bool flag3;
				obj6.set_Checked(flag3 = flag2);
				bool flag4;
				obj5.set_Checked(flag4 = flag3);
				bool flag5;
				obj4.set_Checked(flag5 = flag4);
				bool flag6;
				obj3.set_Checked(flag6 = flag5);
				bool @checked;
				obj2.set_Checked(@checked = flag6);
				obj.set_Checked(@checked);
				optSaveEdited.set_Checked(true);
				optSaveForUse.set_Checked(false);
			}
		}

		private void optAstrometry_MouseClick(object sender, MouseEventArgs e)
		{
			if (!optAstrometry.get_Checked() && CheckSaved())
			{
				optAstrometry.set_Checked(true);
				RadioButton obj = optAsteroids;
				RadioButton obj2 = optAsteroidSummary;
				RadioButton obj3 = optPlanets;
				RadioButton obj4 = optPlanetSummary;
				RadioButton obj5 = optTimes;
				RadioButton obj6 = optDiameters;
				RadioButton obj7 = optDoubles;
				bool flag;
				optSatellites.set_Checked(flag = false);
				bool flag2;
				obj7.set_Checked(flag2 = flag);
				bool flag3;
				obj6.set_Checked(flag3 = flag2);
				bool flag4;
				obj5.set_Checked(flag4 = flag3);
				bool flag5;
				obj4.set_Checked(flag5 = flag4);
				bool flag6;
				obj3.set_Checked(flag6 = flag5);
				bool @checked;
				obj2.set_Checked(@checked = flag6);
				obj.set_Checked(@checked);
				optSaveEdited.set_Checked(true);
				optSaveForUse.set_Checked(false);
			}
		}

		private void optDiameters_MouseClick(object sender, MouseEventArgs e)
		{
			if (!optDiameters.get_Checked() && CheckSaved())
			{
				optDiameters.set_Checked(true);
				RadioButton obj = optAsteroids;
				RadioButton obj2 = optAsteroidSummary;
				RadioButton obj3 = optPlanets;
				RadioButton obj4 = optPlanetSummary;
				RadioButton obj5 = optAstrometry;
				RadioButton obj6 = optTimes;
				RadioButton obj7 = optDoubles;
				bool flag;
				optSatellites.set_Checked(flag = false);
				bool flag2;
				obj7.set_Checked(flag2 = flag);
				bool flag3;
				obj6.set_Checked(flag3 = flag2);
				bool flag4;
				obj5.set_Checked(flag4 = flag3);
				bool flag5;
				obj4.set_Checked(flag5 = flag4);
				bool flag6;
				obj3.set_Checked(flag6 = flag5);
				bool @checked;
				obj2.set_Checked(@checked = flag6);
				obj.set_Checked(@checked);
				optSaveEdited.set_Checked(true);
				optSaveForUse.set_Checked(false);
			}
		}

		private void optDoubles_MouseClick(object sender, MouseEventArgs e)
		{
			if (!optDoubles.get_Checked() && CheckSaved())
			{
				optDoubles.set_Checked(true);
				RadioButton obj = optAsteroids;
				RadioButton obj2 = optAsteroidSummary;
				RadioButton obj3 = optPlanets;
				RadioButton obj4 = optPlanetSummary;
				RadioButton obj5 = optAstrometry;
				RadioButton obj6 = optTimes;
				RadioButton obj7 = optDiameters;
				bool flag;
				optSatellites.set_Checked(flag = false);
				bool flag2;
				obj7.set_Checked(flag2 = flag);
				bool flag3;
				obj6.set_Checked(flag3 = flag2);
				bool flag4;
				obj5.set_Checked(flag4 = flag3);
				bool flag5;
				obj4.set_Checked(flag5 = flag4);
				bool flag6;
				obj3.set_Checked(flag6 = flag5);
				bool @checked;
				obj2.set_Checked(@checked = flag6);
				obj.set_Checked(@checked);
				optSaveEdited.set_Checked(true);
				optSaveForUse.set_Checked(false);
			}
		}

		private void optSatellites_MouseClick(object sender, MouseEventArgs e)
		{
			if (!optSatellites.get_Checked() && CheckSaved())
			{
				optSatellites.set_Checked(true);
				RadioButton obj = optAsteroids;
				RadioButton obj2 = optAsteroidSummary;
				RadioButton obj3 = optPlanets;
				RadioButton obj4 = optPlanetSummary;
				RadioButton obj5 = optAstrometry;
				RadioButton obj6 = optTimes;
				RadioButton obj7 = optDiameters;
				bool flag;
				optDoubles.set_Checked(flag = false);
				bool flag2;
				obj7.set_Checked(flag2 = flag);
				bool flag3;
				obj6.set_Checked(flag3 = flag2);
				bool flag4;
				obj5.set_Checked(flag4 = flag3);
				bool flag5;
				obj4.set_Checked(flag5 = flag4);
				bool flag6;
				obj3.set_Checked(flag6 = flag5);
				bool @checked;
				obj2.set_Checked(@checked = flag6);
				obj.set_Checked(@checked);
				optSaveEdited.set_Checked(true);
				optSaveForUse.set_Checked(false);
			}
		}

		private bool CheckSaved()
		{
			//IL_001e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0024: Invalid comparison between Unknown and I4
			if (!CurrentFileSaved && (int)MessageBox.Show("The currently loaded file has not been saved. \r\n\r\nDo you want to remove the displayed data for the current file without saving it?", "Current file not saved", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return false;
			}
			GCList.Clear();
			FillGrid_FromList();
			((Control)cmdSaveHeader).set_Enabled(false);
			return true;
		}

		private void PDS_Headers_FormClosing(object sender, FormClosingEventArgs e)
		{
			GCList.Clear();
			FillGrid_FromList();
			((Component)this).Dispose();
		}

		private void cmdDownloadHeaders_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("This download will replace the existing Header files with the\r\nversion of those files on the Occult server.\r\n\r\nAny changes you have made to your local Header files will be lost.\r\n\r\n\r\nDo you want to download the Header files?", "Confirm overwrite", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				string occultServer = Settings.Default.OccultServer;
				string fileName = "pds_headers.zip";
				string finalDestination = Utilities.AppPath + "\\Asteroid\\Results\\PDS Files\\";
				http.DownloadHTTP(occultServer, fileName, finalDestination, unzip: true, gunzip: false, ShowMessages: true);
			}
		}

		private void cmdDuplicates_Click(object sender, EventArgs e)
		{
			//IL_000b: Unknown result type (might be due to invalid IL or missing references)
			MessageBox.Show("This function works when you have sorted the Grid by the 'Label' column\r\n\r\nYou will need to sort the grid by the 'Line' before editing & saving", "Advice", (MessageBoxButtons)0);
			try
			{
				((Control)DD).Show();
			}
			catch
			{
				DD = new DisplayData();
				((Control)DD).Show();
			}
			((Control)DD).set_Text("Duplicate Header labels");
			string text = "";
			for (int i = 0; i < dataGridView.get_Rows().get_Count() - 2; i++)
			{
				if (dataGridView.get_Rows().get_Item(i).get_Cells()
					.get_Item(1)
					.get_Value()
					.ToString()!.Trim() != "" && dataGridView.get_Rows().get_Item(i).get_Cells()
					.get_Item(1)
					.get_Value()
					.ToString()!.Trim() == dataGridView.get_Rows().get_Item(i + 1).get_Cells()
					.get_Item(1)
					.get_Value()
					.ToString()!.Trim())
				{
					text = text + dataGridView.get_Rows().get_Item(i).get_Cells()
						.get_Item(0)
						.get_Value()
						.ToString() + " = " + dataGridView.get_Rows().get_Item(i + 1).get_Cells()
						.get_Item(0)
						.get_Value()
						.ToString() + " = " + dataGridView.get_Rows().get_Item(i).get_Cells()
						.get_Item(1)
						.get_Value()
						.ToString() + "\r\n";
				}
			}
			if (text == "")
			{
				((Control)DD.txtBox).set_Text("There are no duplicates");
			}
			else
			{
				((Control)DD.txtBox).set_Text("Duplicate entries are\r\n\r\n" + text);
			}
			((TextBoxBase)DD.txtBox).Select(0, 0);
		}

		private void cmdHelp_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", navigator, (object)"PDSheaders");
		}

		private void cmdExit_Click(object sender, EventArgs e)
		{
			//IL_001e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0024: Invalid comparison between Unknown and I4
			if (CurrentFileSaved || (int)MessageBox.Show("The currently loaded file has not been saved. \r\n\r\nDo you want to continue to Close the form, and lose any changes?", "Current file not saved", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				((Form)this).Close();
				((Component)this).Dispose();
			}
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
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_0007: Unknown result type (might be due to invalid IL or missing references)
			//IL_0011: Expected O, but got Unknown
			//IL_0012: Unknown result type (might be due to invalid IL or missing references)
			//IL_001c: Expected O, but got Unknown
			//IL_001d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0027: Expected O, but got Unknown
			//IL_0028: Unknown result type (might be due to invalid IL or missing references)
			//IL_0032: Expected O, but got Unknown
			//IL_0033: Unknown result type (might be due to invalid IL or missing references)
			//IL_003d: Expected O, but got Unknown
			//IL_003e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0048: Expected O, but got Unknown
			//IL_0049: Unknown result type (might be due to invalid IL or missing references)
			//IL_0053: Expected O, but got Unknown
			//IL_0054: Unknown result type (might be due to invalid IL or missing references)
			//IL_005e: Expected O, but got Unknown
			//IL_005f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0069: Expected O, but got Unknown
			//IL_006a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0074: Expected O, but got Unknown
			//IL_0075: Unknown result type (might be due to invalid IL or missing references)
			//IL_007f: Expected O, but got Unknown
			//IL_0080: Unknown result type (might be due to invalid IL or missing references)
			//IL_008a: Expected O, but got Unknown
			//IL_008b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0095: Expected O, but got Unknown
			//IL_0096: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a0: Expected O, but got Unknown
			//IL_00a1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ab: Expected O, but got Unknown
			//IL_00ac: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b6: Expected O, but got Unknown
			//IL_00b7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c1: Expected O, but got Unknown
			//IL_00c2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00cc: Expected O, but got Unknown
			//IL_00cd: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d7: Expected O, but got Unknown
			//IL_00d8: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e2: Expected O, but got Unknown
			//IL_00e3: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ed: Expected O, but got Unknown
			//IL_00ee: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f8: Expected O, but got Unknown
			//IL_00f9: Unknown result type (might be due to invalid IL or missing references)
			//IL_0103: Expected O, but got Unknown
			//IL_0104: Unknown result type (might be due to invalid IL or missing references)
			//IL_010e: Expected O, but got Unknown
			//IL_010f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0119: Expected O, but got Unknown
			//IL_011a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0124: Expected O, but got Unknown
			//IL_0125: Unknown result type (might be due to invalid IL or missing references)
			//IL_012f: Expected O, but got Unknown
			//IL_0130: Unknown result type (might be due to invalid IL or missing references)
			//IL_013a: Expected O, but got Unknown
			//IL_013b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0145: Expected O, but got Unknown
			//IL_0146: Unknown result type (might be due to invalid IL or missing references)
			//IL_0150: Expected O, but got Unknown
			//IL_0151: Unknown result type (might be due to invalid IL or missing references)
			//IL_015b: Expected O, but got Unknown
			//IL_015c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0166: Expected O, but got Unknown
			//IL_0167: Unknown result type (might be due to invalid IL or missing references)
			//IL_0171: Expected O, but got Unknown
			//IL_0172: Unknown result type (might be due to invalid IL or missing references)
			//IL_017c: Expected O, but got Unknown
			//IL_017d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0187: Expected O, but got Unknown
			//IL_03f5: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ff: Expected O, but got Unknown
			//IL_040c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0416: Expected O, but got Unknown
			//IL_079b: Unknown result type (might be due to invalid IL or missing references)
			//IL_07a5: Expected O, but got Unknown
			//IL_0848: Unknown result type (might be due to invalid IL or missing references)
			//IL_0852: Expected O, but got Unknown
			//IL_08f4: Unknown result type (might be due to invalid IL or missing references)
			//IL_08fe: Expected O, but got Unknown
			//IL_0a2a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a34: Expected O, but got Unknown
			//IL_0ad5: Unknown result type (might be due to invalid IL or missing references)
			//IL_0adf: Expected O, but got Unknown
			//IL_0bf3: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bfd: Expected O, but got Unknown
			//IL_0cb5: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cbf: Expected O, but got Unknown
			//IL_0df8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e02: Expected O, but got Unknown
			//IL_0ea4: Unknown result type (might be due to invalid IL or missing references)
			//IL_0eae: Expected O, but got Unknown
			//IL_0f50: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f5a: Expected O, but got Unknown
			//IL_0ffc: Unknown result type (might be due to invalid IL or missing references)
			//IL_1006: Expected O, but got Unknown
			//IL_10c0: Unknown result type (might be due to invalid IL or missing references)
			//IL_10ca: Expected O, but got Unknown
			//IL_116d: Unknown result type (might be due to invalid IL or missing references)
			//IL_1177: Expected O, but got Unknown
			//IL_1af5: Unknown result type (might be due to invalid IL or missing references)
			//IL_1aff: Expected O, but got Unknown
			DataGridViewCellStyle val = new DataGridViewCellStyle();
			cmdSaveHeader = new Button();
			cmdMove = new Button();
			dataGridView = new DataGridView();
			cmdInsert = new Button();
			cmdDelete = new Button();
			grpHeader = new GroupBox();
			optSatellites = new RadioButton();
			optDoubles = new RadioButton();
			optAstrometry = new RadioButton();
			panel2 = new Panel();
			optSaveEdited = new RadioButton();
			optSaveForUse = new RadioButton();
			panel1 = new Panel();
			optOpenEdited = new RadioButton();
			optOpenForUse = new RadioButton();
			cmdLoad = new Button();
			optPlanetSummary = new RadioButton();
			optPlanets = new RadioButton();
			optTimes = new RadioButton();
			optAsteroidSummary = new RadioButton();
			optAsteroids = new RadioButton();
			optDiameters = new RadioButton();
			updnFrom = new NumericUpDown();
			updnTo = new NumericUpDown();
			label1 = new Label();
			label2 = new Label();
			groupBox1 = new GroupBox();
			panel3 = new Panel();
			groupBox2 = new GroupBox();
			cmdDownloadHeaders = new Button();
			cmdDuplicates = new Button();
			groupBox3 = new GroupBox();
			label3 = new Label();
			cmdHelp = new Button();
			cmdExit = new Button();
			((ISupportInitialize)dataGridView).BeginInit();
			((Control)grpHeader).SuspendLayout();
			((Control)panel2).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((ISupportInitialize)updnFrom).BeginInit();
			((ISupportInitialize)updnTo).BeginInit();
			((Control)groupBox1).SuspendLayout();
			((Control)panel3).SuspendLayout();
			((Control)groupBox2).SuspendLayout();
			((Control)groupBox3).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)cmdSaveHeader).set_Enabled(false);
			((Control)cmdSaveHeader).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSaveHeader).set_Location(new Point(130, 72));
			((Control)cmdSaveHeader).set_Name("cmdSaveHeader");
			((Control)cmdSaveHeader).set_Size(new Size(78, 29));
			((Control)cmdSaveHeader).set_TabIndex(7);
			((Control)cmdSaveHeader).set_Text("Save File");
			((ButtonBase)cmdSaveHeader).set_UseVisualStyleBackColor(true);
			((Control)cmdSaveHeader).add_Click((EventHandler)cmdSaveHeader_Click);
			((Control)cmdMove).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdMove).set_Location(new Point(13, 2));
			((Control)cmdMove).set_Name("cmdMove");
			((Control)cmdMove).set_Size(new Size(90, 30));
			((Control)cmdMove).set_TabIndex(6);
			((Control)cmdMove).set_Text("Move");
			((ButtonBase)cmdMove).set_UseVisualStyleBackColor(true);
			((Control)cmdMove).add_Click((EventHandler)cmdMove_Click);
			dataGridView.set_ColumnHeadersHeightSizeMode((DataGridViewColumnHeadersHeightSizeMode)2);
			val.set_Alignment((DataGridViewContentAlignment)16);
			val.set_BackColor(SystemColors.Window);
			val.set_Font(new Font("Cascadia Code", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			val.set_ForeColor(SystemColors.ControlText);
			val.set_SelectionBackColor(SystemColors.Highlight);
			val.set_SelectionForeColor(SystemColors.HighlightText);
			val.set_WrapMode((DataGridViewTriState)2);
			dataGridView.set_DefaultCellStyle(val);
			((Control)dataGridView).set_Location(new Point(6, 142));
			((Control)dataGridView).set_Name("dataGridView");
			((Control)dataGridView).set_Size(new Size(1011, 523));
			((Control)dataGridView).set_TabIndex(4);
			dataGridView.add_CellFormatting(new DataGridViewCellFormattingEventHandler(dataGridView_CellFormatting));
			dataGridView.add_CellValueChanged(new DataGridViewCellEventHandler(dataGridView_CellValueChanged));
			((Control)cmdInsert).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdInsert).set_Location(new Point(15, 24));
			((Control)cmdInsert).set_Name("cmdInsert");
			((Control)cmdInsert).set_Size(new Size(146, 30));
			((Control)cmdInsert).set_TabIndex(8);
			((Control)cmdInsert).set_Text("Insert line at selected row");
			((ButtonBase)cmdInsert).set_UseVisualStyleBackColor(true);
			((Control)cmdInsert).add_Click((EventHandler)cmdInsert_Click);
			((Control)cmdDelete).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDelete).set_Location(new Point(15, 60));
			((Control)cmdDelete).set_Name("cmdDelete");
			((Control)cmdDelete).set_Size(new Size(146, 30));
			((Control)cmdDelete).set_TabIndex(9);
			((Control)cmdDelete).set_Text("Delete selected row");
			((ButtonBase)cmdDelete).set_UseVisualStyleBackColor(true);
			((Control)cmdDelete).add_Click((EventHandler)cmdDelete_Click);
			((Control)grpHeader).set_BackColor(Color.FloralWhite);
			((Control)grpHeader).get_Controls().Add((Control)(object)optSatellites);
			((Control)grpHeader).get_Controls().Add((Control)(object)optDoubles);
			((Control)grpHeader).get_Controls().Add((Control)(object)optAstrometry);
			((Control)grpHeader).get_Controls().Add((Control)(object)panel2);
			((Control)grpHeader).get_Controls().Add((Control)(object)panel1);
			((Control)grpHeader).get_Controls().Add((Control)(object)cmdLoad);
			((Control)grpHeader).get_Controls().Add((Control)(object)optPlanetSummary);
			((Control)grpHeader).get_Controls().Add((Control)(object)cmdSaveHeader);
			((Control)grpHeader).get_Controls().Add((Control)(object)optPlanets);
			((Control)grpHeader).get_Controls().Add((Control)(object)optTimes);
			((Control)grpHeader).get_Controls().Add((Control)(object)optAsteroidSummary);
			((Control)grpHeader).get_Controls().Add((Control)(object)optAsteroids);
			((Control)grpHeader).get_Controls().Add((Control)(object)optDiameters);
			((Control)grpHeader).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpHeader).set_Location(new Point(6, 3));
			((Control)grpHeader).set_Name("grpHeader");
			((Control)grpHeader).set_Size(new Size(332, 134));
			((Control)grpHeader).set_TabIndex(10);
			grpHeader.set_TabStop(false);
			((Control)grpHeader).set_Text("Header files");
			optSatellites.set_AutoCheck(false);
			((Control)optSatellites).set_AutoSize(true);
			((Control)optSatellites).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optSatellites).set_Location(new Point(178, 115));
			((Control)optSatellites).set_Name("optSatellites");
			((Control)optSatellites).set_Size(new Size(67, 17));
			((Control)optSatellites).set_TabIndex(12);
			((Control)optSatellites).set_Text("Satellites");
			((ButtonBase)optSatellites).set_UseVisualStyleBackColor(true);
			((Control)optSatellites).add_MouseClick(new MouseEventHandler(optSatellites_MouseClick));
			optDoubles.set_AutoCheck(false);
			((Control)optDoubles).set_AutoSize(true);
			((Control)optDoubles).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optDoubles).set_Location(new Point(88, 115));
			((Control)optDoubles).set_Name("optDoubles");
			((Control)optDoubles).set_Size(new Size(84, 17));
			((Control)optDoubles).set_TabIndex(11);
			((Control)optDoubles).set_Text("Double stars");
			((ButtonBase)optDoubles).set_UseVisualStyleBackColor(true);
			((Control)optDoubles).add_MouseClick(new MouseEventHandler(optDoubles_MouseClick));
			optAstrometry.set_AutoCheck(false);
			((Control)optAstrometry).set_AutoSize(true);
			((Control)optAstrometry).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optAstrometry).set_Location(new Point(10, 99));
			((Control)optAstrometry).set_Name("optAstrometry");
			((Control)optAstrometry).set_Size(new Size(74, 17));
			((Control)optAstrometry).set_TabIndex(5);
			((Control)optAstrometry).set_Text("Astrometry");
			((ButtonBase)optAstrometry).set_UseVisualStyleBackColor(true);
			((Control)optAstrometry).add_MouseClick(new MouseEventHandler(optAstrometry_MouseClick));
			((Control)panel2).get_Controls().Add((Control)(object)optSaveEdited);
			((Control)panel2).get_Controls().Add((Control)(object)optSaveForUse);
			((Control)panel2).set_Location(new Point(216, 66));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(107, 41));
			((Control)panel2).set_TabIndex(9);
			optSaveEdited.set_AutoCheck(false);
			((Control)optSaveEdited).set_AutoSize(true);
			optSaveEdited.set_Checked(true);
			((Control)optSaveEdited).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optSaveEdited).set_Location(new Point(3, 5));
			((Control)optSaveEdited).set_Name("optSaveEdited");
			((Control)optSaveEdited).set_Size(new Size(92, 17));
			((Control)optSaveEdited).set_TabIndex(2);
			optSaveEdited.set_TabStop(true);
			((Control)optSaveEdited).set_Text("Edited version");
			((ButtonBase)optSaveEdited).set_UseVisualStyleBackColor(true);
			((Control)optSaveEdited).add_MouseClick(new MouseEventHandler(optSaveEdited_MouseClick));
			optSaveForUse.set_AutoCheck(false);
			((Control)optSaveForUse).set_AutoSize(true);
			((Control)optSaveForUse).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optSaveForUse).set_Location(new Point(3, 21));
			((Control)optSaveForUse).set_Name("optSaveForUse");
			((Control)optSaveForUse).set_Size(new Size(95, 17));
			((Control)optSaveForUse).set_TabIndex(1);
			((Control)optSaveForUse).set_Text("Version for use");
			((ButtonBase)optSaveForUse).set_UseVisualStyleBackColor(true);
			((Control)optSaveForUse).add_MouseClick(new MouseEventHandler(optSaveForUse_MouseClick));
			((Control)panel1).get_Controls().Add((Control)(object)optOpenEdited);
			((Control)panel1).get_Controls().Add((Control)(object)optOpenForUse);
			((Control)panel1).set_Location(new Point(216, 10));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(107, 41));
			((Control)panel1).set_TabIndex(8);
			optOpenEdited.set_AutoCheck(false);
			((Control)optOpenEdited).set_AutoSize(true);
			((Control)optOpenEdited).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optOpenEdited).set_Location(new Point(3, 21));
			((Control)optOpenEdited).set_Name("optOpenEdited");
			((Control)optOpenEdited).set_Size(new Size(92, 17));
			((Control)optOpenEdited).set_TabIndex(1);
			((Control)optOpenEdited).set_Text("Edited version");
			((ButtonBase)optOpenEdited).set_UseVisualStyleBackColor(true);
			((Control)optOpenEdited).add_MouseClick(new MouseEventHandler(optOpenEdited_MouseClick));
			optOpenForUse.set_AutoCheck(false);
			((Control)optOpenForUse).set_AutoSize(true);
			optOpenForUse.set_Checked(true);
			((Control)optOpenForUse).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optOpenForUse).set_Location(new Point(3, 4));
			((Control)optOpenForUse).set_Name("optOpenForUse");
			((Control)optOpenForUse).set_Size(new Size(95, 17));
			((Control)optOpenForUse).set_TabIndex(0);
			optOpenForUse.set_TabStop(true);
			((Control)optOpenForUse).set_Text("Version for use");
			((ButtonBase)optOpenForUse).set_UseVisualStyleBackColor(true);
			((Control)optOpenForUse).add_MouseClick(new MouseEventHandler(optOpenForUse_MouseClick));
			((Control)cmdLoad).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdLoad).set_Location(new Point(130, 16));
			((Control)cmdLoad).set_Name("cmdLoad");
			((Control)cmdLoad).set_Size(new Size(78, 29));
			((Control)cmdLoad).set_TabIndex(6);
			((Control)cmdLoad).set_Text("Load file");
			((ButtonBase)cmdLoad).set_UseVisualStyleBackColor(true);
			((Control)cmdLoad).add_Click((EventHandler)cmdLoad_Click);
			optPlanetSummary.set_AutoCheck(false);
			((Control)optPlanetSummary).set_AutoSize(true);
			((Control)optPlanetSummary).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optPlanetSummary).set_Location(new Point(10, 67));
			((Control)optPlanetSummary).set_Name("optPlanetSummary");
			((Control)optPlanetSummary).set_Size(new Size(106, 17));
			((Control)optPlanetSummary).set_TabIndex(3);
			((Control)optPlanetSummary).set_Text("Planets Summary");
			((ButtonBase)optPlanetSummary).set_UseVisualStyleBackColor(true);
			((Control)optPlanetSummary).add_MouseClick(new MouseEventHandler(optPlanetSummary_MouseClick));
			optPlanets.set_AutoCheck(false);
			((Control)optPlanets).set_AutoSize(true);
			((Control)optPlanets).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optPlanets).set_Location(new Point(10, 51));
			((Control)optPlanets).set_Name("optPlanets");
			((Control)optPlanets).set_Size(new Size(60, 17));
			((Control)optPlanets).set_TabIndex(2);
			((Control)optPlanets).set_Text("Planets");
			((ButtonBase)optPlanets).set_UseVisualStyleBackColor(true);
			((Control)optPlanets).add_MouseClick(new MouseEventHandler(optPlanets_MouseClick));
			optTimes.set_AutoCheck(false);
			((Control)optTimes).set_AutoSize(true);
			((Control)optTimes).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optTimes).set_Location(new Point(10, 83));
			((Control)optTimes).set_Name("optTimes");
			((Control)optTimes).set_Size(new Size(53, 17));
			((Control)optTimes).set_TabIndex(4);
			((Control)optTimes).set_Text("Times");
			((ButtonBase)optTimes).set_UseVisualStyleBackColor(true);
			((Control)optTimes).add_MouseClick(new MouseEventHandler(optTimes_MouseClick));
			optAsteroidSummary.set_AutoCheck(false);
			((Control)optAsteroidSummary).set_AutoSize(true);
			((Control)optAsteroidSummary).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optAsteroidSummary).set_Location(new Point(10, 35));
			((Control)optAsteroidSummary).set_Name("optAsteroidSummary");
			((Control)optAsteroidSummary).set_Size(new Size(114, 17));
			((Control)optAsteroidSummary).set_TabIndex(1);
			((Control)optAsteroidSummary).set_Text("Asteroids Summary");
			((ButtonBase)optAsteroidSummary).set_UseVisualStyleBackColor(true);
			((Control)optAsteroidSummary).add_MouseClick(new MouseEventHandler(optAsteroidSummary_MouseClick));
			optAsteroids.set_AutoCheck(false);
			((Control)optAsteroids).set_AutoSize(true);
			optAsteroids.set_Checked(true);
			((Control)optAsteroids).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optAsteroids).set_Location(new Point(10, 19));
			((Control)optAsteroids).set_Name("optAsteroids");
			((Control)optAsteroids).set_Size(new Size(68, 17));
			((Control)optAsteroids).set_TabIndex(0);
			optAsteroids.set_TabStop(true);
			((Control)optAsteroids).set_Text("Asteroids");
			((ButtonBase)optAsteroids).set_UseVisualStyleBackColor(true);
			((Control)optAsteroids).add_MouseClick(new MouseEventHandler(optAsteroids_MouseClick));
			optDiameters.set_AutoCheck(false);
			((Control)optDiameters).set_AutoSize(true);
			((Control)optDiameters).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optDiameters).set_Location(new Point(10, 115));
			((Control)optDiameters).set_Name("optDiameters");
			((Control)optDiameters).set_Size(new Size(72, 17));
			((Control)optDiameters).set_TabIndex(10);
			((Control)optDiameters).set_Text("Diameters");
			((ButtonBase)optDiameters).set_UseVisualStyleBackColor(true);
			((Control)optDiameters).add_MouseClick(new MouseEventHandler(optDiameters_MouseClick));
			((Control)updnFrom).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnFrom).set_Location(new Point(11, 63));
			updnFrom.set_Maximum(new decimal(new int[4] { 500, 0, 0, 0 }));
			updnFrom.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnFrom).set_Name("updnFrom");
			((Control)updnFrom).set_Size(new Size(42, 20));
			((Control)updnFrom).set_TabIndex(11);
			updnFrom.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnTo).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnTo).set_Location(new Point(63, 63));
			updnTo.set_Maximum(new decimal(new int[4] { 500, 0, 0, 0 }));
			updnTo.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnTo).set_Name("updnTo");
			((Control)updnTo).set_Size(new Size(42, 20));
			((Control)updnTo).set_TabIndex(12);
			updnTo.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(14, 37));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(30, 26));
			((Control)label1).set_TabIndex(13);
			((Control)label1).set_Text("From\r\nRow");
			label1.set_TextAlign(ContentAlignment.TopCenter);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(66, 37));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(29, 26));
			((Control)label2).set_TabIndex(14);
			((Control)label2).set_Text("To\r\nRow");
			label2.set_TextAlign(ContentAlignment.MiddleLeft);
			((Control)groupBox1).set_BackColor(Color.LightGoldenrodYellow);
			((Control)groupBox1).get_Controls().Add((Control)(object)panel3);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdDelete);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdInsert);
			((Control)groupBox1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox1).set_Location(new Point(350, 3));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(292, 103));
			((Control)groupBox1).set_TabIndex(15);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Insert/Delete/Move");
			((Control)panel3).set_BackColor(Color.FloralWhite);
			((Control)panel3).get_Controls().Add((Control)(object)label2);
			((Control)panel3).get_Controls().Add((Control)(object)cmdMove);
			((Control)panel3).get_Controls().Add((Control)(object)label1);
			((Control)panel3).get_Controls().Add((Control)(object)updnFrom);
			((Control)panel3).get_Controls().Add((Control)(object)updnTo);
			((Control)panel3).set_Location(new Point(175, 10));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(116, 89));
			((Control)panel3).set_TabIndex(10);
			((Control)groupBox2).set_BackColor(Color.MintCream);
			((Control)groupBox2).get_Controls().Add((Control)(object)cmdDownloadHeaders);
			((Control)groupBox2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox2).set_Location(new Point(789, 3));
			((Control)groupBox2).set_Name("groupBox2");
			((Control)groupBox2).set_Size(new Size(117, 103));
			((Control)groupBox2).set_TabIndex(16);
			groupBox2.set_TabStop(false);
			((Control)groupBox2).set_Text("Download latest headers");
			((Control)cmdDownloadHeaders).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDownloadHeaders).set_Location(new Point(9, 36));
			((Control)cmdDownloadHeaders).set_Name("cmdDownloadHeaders");
			((Control)cmdDownloadHeaders).set_Size(new Size(99, 30));
			((Control)cmdDownloadHeaders).set_TabIndex(0);
			((Control)cmdDownloadHeaders).set_Text("Download");
			((ButtonBase)cmdDownloadHeaders).set_UseVisualStyleBackColor(true);
			((Control)cmdDownloadHeaders).add_Click((EventHandler)cmdDownloadHeaders_Click);
			((Control)cmdDuplicates).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDuplicates).set_Location(new Point(17, 36));
			((Control)cmdDuplicates).set_Name("cmdDuplicates");
			((Control)cmdDuplicates).set_Size(new Size(89, 30));
			((Control)cmdDuplicates).set_TabIndex(17);
			((Control)cmdDuplicates).set_Text("Find duplicates");
			((ButtonBase)cmdDuplicates).set_UseVisualStyleBackColor(true);
			((Control)cmdDuplicates).add_Click((EventHandler)cmdDuplicates_Click);
			((Control)groupBox3).set_BackColor(Color.LightCyan);
			((Control)groupBox3).get_Controls().Add((Control)(object)label3);
			((Control)groupBox3).get_Controls().Add((Control)(object)cmdDuplicates);
			((Control)groupBox3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox3).set_Location(new Point(654, 3));
			((Control)groupBox3).set_Name("groupBox3");
			((Control)groupBox3).set_Size(new Size(123, 103));
			((Control)groupBox3).set_TabIndex(18);
			groupBox3.set_TabStop(false);
			((Control)groupBox3).set_Text("Find Duplicate Labels");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(11, 73));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(95, 26));
			((Control)label3).set_TabIndex(18);
			((Control)label3).set_Text("Sort table by Label\r\nAfter, sort by Line ");
			((Control)cmdHelp).set_BackColor(Color.AliceBlue);
			((Control)cmdHelp).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((ButtonBase)cmdHelp).set_Image((Image)Resources.Help16x16);
			((ButtonBase)cmdHelp).set_ImageAlign(ContentAlignment.MiddleLeft);
			((Control)cmdHelp).set_Location(new Point(921, 18));
			((Control)cmdHelp).set_Name("cmdHelp");
			((Control)cmdHelp).set_Size(new Size(67, 39));
			((Control)cmdHelp).set_TabIndex(19);
			((Control)cmdHelp).set_Text("Help ");
			((ButtonBase)cmdHelp).set_TextAlign(ContentAlignment.MiddleRight);
			((ButtonBase)cmdHelp).set_UseVisualStyleBackColor(false);
			((Control)cmdHelp).add_Click((EventHandler)cmdHelp_Click);
			((Control)cmdExit).set_BackColor(Color.PeachPuff);
			((Control)cmdExit).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((ButtonBase)cmdExit).set_Image((Image)Resources.error);
			((ButtonBase)cmdExit).set_ImageAlign(ContentAlignment.MiddleLeft);
			((Control)cmdExit).set_Location(new Point(924, 65));
			((Control)cmdExit).set_Name("cmdExit");
			((Control)cmdExit).set_Size(new Size(63, 38));
			((Control)cmdExit).set_TabIndex(20);
			((Control)cmdExit).set_Text("Exit");
			((ButtonBase)cmdExit).set_UseVisualStyleBackColor(false);
			((Control)cmdExit).add_Click((EventHandler)cmdExit_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(1023, 672));
			((Control)this).get_Controls().Add((Control)(object)cmdExit);
			((Control)this).get_Controls().Add((Control)(object)cmdHelp);
			((Control)this).get_Controls().Add((Control)(object)groupBox3);
			((Control)this).get_Controls().Add((Control)(object)groupBox2);
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Control)this).get_Controls().Add((Control)(object)grpHeader);
			((Control)this).get_Controls().Add((Control)(object)dataGridView);
			((Control)this).set_Name("PDS_Headers");
			((Control)this).set_Text("PDS_Headers");
			((Form)this).add_FormClosing(new FormClosingEventHandler(PDS_Headers_FormClosing));
			((Control)this).add_Resize((EventHandler)PDS_Headers_Resize);
			((ISupportInitialize)dataGridView).EndInit();
			((Control)grpHeader).ResumeLayout(false);
			((Control)grpHeader).PerformLayout();
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((ISupportInitialize)updnFrom).EndInit();
			((ISupportInitialize)updnTo).EndInit();
			((Control)groupBox1).ResumeLayout(false);
			((Control)panel3).ResumeLayout(false);
			((Control)panel3).PerformLayout();
			((Control)groupBox2).ResumeLayout(false);
			((Control)groupBox3).ResumeLayout(false);
			((Control)groupBox3).PerformLayout();
			((Control)this).ResumeLayout(false);
		}
	}
}
