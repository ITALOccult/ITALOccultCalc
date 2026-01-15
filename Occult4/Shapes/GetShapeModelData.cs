using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Windows.Forms;
using Occult;
using Occult.Asteroid_Observations;
using Occult.Properties;

namespace Shapes
{
	internal class GetShapeModelData
	{
		internal string[] Shape = new string[6];

		internal string[] IAUSpin = new string[6];

		internal double[] L = new double[6];

		internal double[] B = new double[6];

		internal double[] P = new double[6];

		internal static List<Model_Parameters> DAMITModelsByAsteroidNumber = new List<Model_Parameters>();

		internal static List<Model_Parameters> ISAMmodelsByAsteroidNumber = new List<Model_Parameters>();

		internal static List<DamitModelIndex> DamitIndex = new List<DamitModelIndex>();

		internal static AsteroidFromDamitModel asteroidFromDamitModel;

		internal static List<ShapeModelIndex> ShapeModelList = new List<ShapeModelIndex>();

		internal static string ShapeModelDirectory = Utilities.AppPath + "\\ShapeModels\\";

		internal static string ShapeModelDirectory_Extras = Utilities.AppPath + "\\ShapeModels\\Extras";

		internal static string FileOfDAMITModels = ShapeModelDirectory + "DamitModels.csv";

		internal static string FileOfISAMModels = ShapeModelDirectory + "IsamModels.csv";

		private static int FailedCount = 0;

		internal static void InitialiseDAMITShapeModels()
		{
			if (DAMITModelsByAsteroidNumber.Count <= 20)
			{
				if (!Directory.Exists(ShapeModelDirectory))
				{
					Directory.CreateDirectory(ShapeModelDirectory);
				}
				if (File.Exists(FileOfDAMITModels) && new FileInfo(FileOfDAMITModels).Length < 300000)
				{
					File.Delete(FileOfDAMITModels);
				}
				if (!File.Exists(FileOfDAMITModels))
				{
					CreateDAMITModelList();
				}
				FillDAMITAsteroidModels();
			}
		}

		internal static void FillDAMITAsteroidModels()
		{
			DAMITModelsByAsteroidNumber.Clear();
			using (StreamReader streamReader = new StreamReader(FileOfDAMITModels))
			{
				do
				{
					string[] array = streamReader.ReadLine()!.Split(new char[1] { ',' });
					Model_Parameters model_Parameters = new Model_Parameters();
					model_Parameters.AsteroidNumber = int.Parse(array[0]);
					model_Parameters.ModelNumber_String = array[1];
					model_Parameters.Lambda = array[2];
					model_Parameters.Beta = array[3];
					model_Parameters.Period = array[4];
					model_Parameters.YORP = array[5];
					model_Parameters.JD0 = array[6];
					model_Parameters.Phi0 = array[7];
					model_Parameters.Quality = array[8];
					model_Parameters.Version = array[9];
					model_Parameters.Comment = array[10];
					model_Parameters.Created = array[11];
					model_Parameters.Modified = array[12];
					model_Parameters.DAMIT_Asteroid_ID = array[13];
					DAMITModelsByAsteroidNumber.Add(model_Parameters);
				}
				while (!streamReader.EndOfStream);
			}
			Model_Parameters.Sort_0bysteroidNumber_1byModelNumberNumeric_2forISAM = 0;
			DAMITModelsByAsteroidNumber.Sort();
		}

		internal static void ShowDAMITtoAsteroid()
		{
			try
			{
				((Control)asteroidFromDamitModel).Show();
			}
			catch
			{
				asteroidFromDamitModel = new AsteroidFromDamitModel();
				((Control)asteroidFromDamitModel).Show();
			}
			((Control)asteroidFromDamitModel).Focus();
		}

		internal static void FillDamitModelIndex()
		{
			DamitIndex.Clear();
			using (StreamReader streamReader = new StreamReader(FileOfDAMITModels))
			{
				do
				{
					string[] array = streamReader.ReadLine()!.Split(new char[1] { ',' });
					DamitModelIndex damitModelIndex = new DamitModelIndex();
					damitModelIndex.Asteroid_number = int.Parse(array[0]);
					damitModelIndex.Damit_number = int.Parse(array[1]);
					DamitIndex.Add(damitModelIndex);
				}
				while (!streamReader.EndOfStream);
			}
			DamitIndex.Sort();
		}

		internal static string AsteroidNumberFromDamitID(string ModelID)
		{
			if (DamitIndex.Count < 1)
			{
				FillDamitModelIndex();
			}
			if (int.TryParse(ModelID, out var result))
			{
				int num = 0;
				int num2 = DAMITModelsByAsteroidNumber.Count - 1;
				do
				{
					int num3 = (num2 + num) / 2;
					if (DamitIndex[num3].Damit_number == result)
					{
						return DamitIndex[num3].Asteroid_number.ToString();
					}
					if (DamitIndex[num3].Damit_number < result)
					{
						num = num3 + 1;
					}
					else
					{
						num2 = num3 - 1;
					}
				}
				while (num <= num2);
			}
			return "....";
		}

		internal static void CreateDAMITModelList()
		{
			//IL_0014: Unknown result type (might be due to invalid IL or missing references)
			//IL_02f7: Unknown result type (might be due to invalid IL or missing references)
			//IL_04ad: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This function requires internet access. You are not connected to the internet", "No Internet", (MessageBoxButtons)0, (MessageBoxIcon)16);
			}
			int result = -1;
			int num = -1;
			int num2 = -1;
			List<string> list = new List<string>();
			if (File.Exists(FileOfDAMITModels))
			{
				using StreamReader streamReader = new StreamReader(FileOfDAMITModels);
				do
				{
					string text = streamReader.ReadLine();
					if (int.Parse(text.Split(new char[1] { ',' })[1]) >= 100000)
					{
						list.Add(text);
					}
				}
				while (!streamReader.EndOfStream);
			}
			MessageForm messageForm = new MessageForm();
			((Control)messageForm.label).set_Text("Downloading and merging two files from DAMIT");
			((Control)messageForm).Show();
			Application.DoEvents();
			List<DAMIT_Model_Parameters_byDAMIT_Asteroid_ID> list2 = new List<DAMIT_Model_Parameters_byDAMIT_Asteroid_ID>();
			ServicePointManager.SecurityProtocol = SecurityProtocolType.Ssl3 | SecurityProtocolType.Tls | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls12;
			HttpWebRequest httpWebRequest = (HttpWebRequest)WebRequest.Create($"https://astro.troja.mff.cuni.cz/projects/damit/exports/table/asteroid_models");
			httpWebRequest.Method = "GET";
			try
			{
				using WebResponse webResponse = httpWebRequest.GetResponse();
				using Stream stream = webResponse.GetResponseStream();
				using StreamReader streamReader2 = new StreamReader(stream);
				string text = streamReader2.ReadLine();
				do
				{
					text = streamReader2.ReadLine();
					for (int i = 0; i < 4; i++)
					{
						result = text.IndexOf("\",");
						if (result < 0)
						{
							break;
						}
						num = text.LastIndexOf("\"", result - 1);
						num2 = text.IndexOf(",", num);
						if (num2 > 0 && num2 < result)
						{
							text = text.Remove(num2, 1).Insert(num2, "_");
						}
						text = text.Remove(result, 1).Remove(num, 1);
					}
					string[] array = text.Split(new char[1] { ',' });
					if (array.Length >= 2)
					{
						DAMIT_Model_Parameters_byDAMIT_Asteroid_ID dAMIT_Model_Parameters_byDAMIT_Asteroid_ID = new DAMIT_Model_Parameters_byDAMIT_Asteroid_ID();
						dAMIT_Model_Parameters_byDAMIT_Asteroid_ID.DAMIT_ModelNumber = array[0];
						int.TryParse(array[1], out result);
						dAMIT_Model_Parameters_byDAMIT_Asteroid_ID.DAMIT_Asteroid_ID = result;
						dAMIT_Model_Parameters_byDAMIT_Asteroid_ID.Lambda = array[2];
						dAMIT_Model_Parameters_byDAMIT_Asteroid_ID.Beta = array[3];
						dAMIT_Model_Parameters_byDAMIT_Asteroid_ID.Period = array[4];
						if (array[5] == "")
						{
							dAMIT_Model_Parameters_byDAMIT_Asteroid_ID.YORP = "0";
						}
						else
						{
							dAMIT_Model_Parameters_byDAMIT_Asteroid_ID.YORP = array[5];
						}
						dAMIT_Model_Parameters_byDAMIT_Asteroid_ID.JD0 = array[6];
						dAMIT_Model_Parameters_byDAMIT_Asteroid_ID.Phi0 = array[7];
						dAMIT_Model_Parameters_byDAMIT_Asteroid_ID.Quality = array[24];
						dAMIT_Model_Parameters_byDAMIT_Asteroid_ID.Version = array[26];
						dAMIT_Model_Parameters_byDAMIT_Asteroid_ID.Comment = RemoveQuotes(array[27]);
						dAMIT_Model_Parameters_byDAMIT_Asteroid_ID.Created = array[28].Trim();
						result = dAMIT_Model_Parameters_byDAMIT_Asteroid_ID.Created.IndexOf(" ");
						if (result > 0)
						{
							dAMIT_Model_Parameters_byDAMIT_Asteroid_ID.Created = dAMIT_Model_Parameters_byDAMIT_Asteroid_ID.Created.Substring(0, result);
						}
						dAMIT_Model_Parameters_byDAMIT_Asteroid_ID.Modified = array[29].Trim();
						result = dAMIT_Model_Parameters_byDAMIT_Asteroid_ID.Modified.IndexOf(" ");
						if (result > 0)
						{
							dAMIT_Model_Parameters_byDAMIT_Asteroid_ID.Modified = dAMIT_Model_Parameters_byDAMIT_Asteroid_ID.Modified.Substring(0, result);
						}
						list2.Add(dAMIT_Model_Parameters_byDAMIT_Asteroid_ID);
					}
				}
				while (!streamReader2.EndOfStream);
			}
			catch (Exception ex)
			{
				MessageBox.Show(ex.Message);
			}
			list2.Sort();
			int result2 = 0;
			int num3 = 0;
			int num4 = 0;
			httpWebRequest = (HttpWebRequest)WebRequest.Create($"https://astro.troja.mff.cuni.cz/projects/damit/exports/table/asteroids");
			httpWebRequest.Method = "GET";
			try
			{
				using WebResponse webResponse2 = httpWebRequest.GetResponse();
				using Stream stream2 = webResponse2.GetResponseStream();
				using StreamReader streamReader3 = new StreamReader(stream2);
				using StreamWriter streamWriter = new StreamWriter(FileOfDAMITModels);
				if (list.Count > 0)
				{
					num3 = int.Parse(list[num4].Split(new char[1] { ',' })[0]);
				}
				string text = streamReader3.ReadLine();
				int result3 = 0;
				do
				{
					string[] array = streamReader3.ReadLine()!.Split(new char[1] { ',' });
					int.TryParse(array[0], out result3);
					int.TryParse(array[1], out result2);
					if (num4 < list.Count)
					{
						while (result2 > num3)
						{
							streamWriter.WriteLine(list[num4]);
							num4++;
							if (num4 >= list.Count)
							{
								break;
							}
							num3 = int.Parse(list[num4].Split(new char[1] { ',' })[0]);
						}
					}
					for (int j = 0; j < list2.Count; j++)
					{
						if (result3 == list2[j].DAMIT_Asteroid_ID)
						{
							streamWriter.WriteLine(array[1] + "," + list2[j].ToString());
						}
					}
				}
				while (!streamReader3.EndOfStream);
			}
			catch (Exception ex2)
			{
				MessageBox.Show(ex2.Message);
			}
			((Form)messageForm).Close();
		}

		private static string RemoveQuotes(string In)
		{
			return In.Replace("\"", "");
		}

		internal static void DownloadAllDAMITModels()
		{
			//IL_01c4: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ca: Invalid comparison between Unknown and I4
			//IL_01da: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ed: Unknown result type (might be due to invalid IL or missing references)
			//IL_0538: Unknown result type (might be due to invalid IL or missing references)
			//IL_053e: Invalid comparison between Unknown and I4
			//IL_0599: Unknown result type (might be due to invalid IL or missing references)
			List<string> list = new List<string>();
			CreateDAMITModelList();
			FillDAMITAsteroidModels();
			bool flag = true;
			Cursor.set_Current(Cursors.get_WaitCursor());
			Application.DoEvents();
			for (int i = 0; i < DAMITModelsByAsteroidNumber.Count; i++)
			{
				flag = true;
				string text = DAMITModelsByAsteroidNumber[i].Modified.Trim();
				DateTime dateTime;
				if ((text.Length > 0) & Enumerable.Contains(text, '-'))
				{
					string[] array = text.Split(new char[1] { '-' });
					dateTime = new DateTime(int.Parse(array[0]), int.Parse(array[1]), int.Parse(array[2]));
				}
				else
				{
					dateTime = new DateTime(1990, 1, 1);
				}
				string path = ShapeModelDirectory + DAMITModelsByAsteroidNumber[i].ModelNumber_String + ".txt";
				if (File.Exists(path))
				{
					if (File.GetLastWriteTimeUtc(path) < dateTime)
					{
						flag = false;
					}
				}
				else
				{
					flag = false;
				}
				if (!flag)
				{
					list.Add(DAMITModelsByAsteroidNumber[i].ModelNumber_String);
				}
			}
			Cursor.set_Current(Cursors.get_Default());
			if (list.Count > 0)
			{
				string text2 = "";
				for (int j = 0; j < list.Count; j++)
				{
					text2 = text2 + list[j].ToString() + ", ";
					if (j > 9)
					{
						text2 = text2 + (list.Count - 10) + "more";
						break;
					}
				}
				if ((int)MessageBox.Show("There are " + list.Count + " models available for download\r\n" + text2 + "\r\n\r\nThe download may take some time!\r\n\r\nAlso, the combined file size of all shape models will be over 170 MB.\r\n\r\nDo you want to continue?", "Download / update DAMIT shape models", (MessageBoxButtons)1, (MessageBoxIcon)32) == 2)
				{
					return;
				}
				MessageBox.Show("If during the download an error is reported, simply click on the OK button to continue downloading models.", "Download Errors", (MessageBoxButtons)0, (MessageBoxIcon)64);
				PBar pBar = new PBar();
				pBar.pBarFTP.set_Minimum(0);
				pBar.pBarFTP.set_Maximum(list.Count);
				pBar.pBarFTP.set_Value(0);
				((Control)pBar).Show();
				for (int k = 0; k < list.Count; k++)
				{
					if (!File.Exists(ShapeModelDirectory + list[k].ToString() + ".txt"))
					{
						((Control)pBar).set_Text("Model " + list[k].ToString());
						DownloadDAMITShape(list[k].ToString(), out var _);
						ProgressBar pBarFTP = pBar.pBarFTP;
						int value = pBarFTP.get_Value();
						pBarFTP.set_Value(value + 1);
					}
					else
					{
						ProgressBar pBarFTP2 = pBar.pBarFTP;
						int value = pBarFTP2.get_Value();
						pBarFTP2.set_Value(value + 1);
					}
					Application.DoEvents();
				}
				((Form)pBar).Close();
				FillDAMITAsteroidModels();
			}
			Model_Parameters.Sort_0bysteroidNumber_1byModelNumberNumeric_2forISAM = 1;
			DAMITModelsByAsteroidNumber.Sort();
			string text3 = "";
			for (int l = 1; l < DAMITModelsByAsteroidNumber.Count; l++)
			{
				if (DAMITModelsByAsteroidNumber[l - 1].ModelNumber_String == DAMITModelsByAsteroidNumber[l].ModelNumber_String)
				{
					text3 = text3 + "(" + DAMITModelsByAsteroidNumber[l - 1].AsteroidNumber + ")  " + DAMITModelsByAsteroidNumber[l - 1].ModelNumber_String + " = (" + DAMITModelsByAsteroidNumber[l - 1].ModelNumber_String + ")" + DAMITModelsByAsteroidNumber[l - 1].ModelNumber_String + "\r\n";
				}
			}
			if (text3.Length > 0)
			{
				MessageBox.Show("The following duplicate shape model entries were found. They will need to be reported to DAMIT\r\n\r\n" + text3, "Duplicate shape models", (MessageBoxButtons)0, (MessageBoxIcon)48);
			}
			bool flag2 = false;
			List<int> list2 = new List<int>();
			FileInfo[] files = new DirectoryInfo(ShapeModelDirectory).GetFiles("*.txt");
			for (int value = 0; value < files.Length; value++)
			{
				if (!int.TryParse(Path.GetFileNameWithoutExtension(files[value].ToString()), out var result))
				{
					continue;
				}
				int num = 0;
				int num2 = DAMITModelsByAsteroidNumber.Count - 1;
				flag2 = false;
				do
				{
					int num3 = (num2 + num) / 2;
					if (DAMITModelsByAsteroidNumber[num3].ModelNumber_Numeric == result)
					{
						flag2 = true;
						break;
					}
					if (DAMITModelsByAsteroidNumber[num3].ModelNumber_Numeric < result)
					{
						num = num3 + 1;
					}
					else
					{
						num2 = num3 - 1;
					}
				}
				while (num <= num2);
				if (!flag2)
				{
					list2.Add(result);
				}
			}
			if (list2.Count > 0)
			{
				string text4 = "";
				for (int m = 0; m < list2.Count; m++)
				{
					if (m % 5 == 0)
					{
						text4 += "\r\n";
					}
					text4 = text4 + list2[m] + ".txt     ";
				}
				if ((int)MessageBox.Show("The following " + list2.Count + " shape model files on your computer have ceased to be valid. Click OK to delete these files\r\n" + text4, "Files to delete", (MessageBoxButtons)1, (MessageBoxIcon)32) == 1)
				{
					for (int n = 0; n < list2.Count; n++)
					{
						File.Delete(ShapeModelDirectory + list2[n] + ".txt");
					}
				}
			}
			Model_Parameters.Sort_0bysteroidNumber_1byModelNumberNumeric_2forISAM = 0;
			DAMITModelsByAsteroidNumber.Sort();
			MessageBox.Show("DAMIT shape model data is up-to-date", "DAMIT up-to-date", (MessageBoxButtons)0, (MessageBoxIcon)64);
		}

		internal static bool DownloadDAMITShape(string Model, out bool InternetAvailable)
		{
			//IL_0222: Unknown result type (might be due to invalid IL or missing references)
			//IL_0240: Unknown result type (might be due to invalid IL or missing references)
			//IL_0564: Unknown result type (might be due to invalid IL or missing references)
			//IL_057d: Unknown result type (might be due to invalid IL or missing references)
			bool flag = false;
			InternetAvailable = Utilities.InternetIsAvailable();
			if (!InternetAvailable)
			{
				return false;
			}
			List<Vector> list = new List<Vector>();
			List<Face> list2 = new List<Face>();
			ServicePointManager.SecurityProtocol = SecurityProtocolType.Ssl3 | SecurityProtocolType.Tls | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls12;
			HttpWebRequest httpWebRequest = (HttpWebRequest)WebRequest.Create($"https://astro.troja.mff.cuni.cz/projects/damit/generated_files/open/AsteroidModel/{Model}/shape.obj");
			httpWebRequest.Method = "GET";
			try
			{
				using WebResponse webResponse = httpWebRequest.GetResponse();
				using Stream stream = webResponse.GetResponseStream();
				using (StreamReader streamReader = new StreamReader(stream))
				{
					do
					{
						string text = streamReader.ReadLine();
						if (text.Substring(0, 1) == "v")
						{
							string[] array = Utilities.SplitStringWithCommas(text.Substring(1).Trim().Replace('\t', ' ')).Split(new char[1] { ',' });
							Vector vector = new Vector();
							vector.SetVectorValues(double.Parse(array[0]), double.Parse(array[1]), double.Parse(array[2]));
							list.Add(vector);
						}
						else
						{
							string[] array2 = Utilities.SplitStringWithCommas(text.Substring(1).Trim().Replace('\t', ' ')).Split(new char[1] { ',' });
							Face face = new Face();
							face.SetFaces(array2[0], array2[1], array2[2], Subtract1: true);
							list2.Add(face);
						}
					}
					while (!streamReader.EndOfStream);
					using StreamWriter streamWriter = new StreamWriter(ShapeModelDirectory + Model + ".txt");
					streamWriter.WriteLine(list.Count + ", " + list2.Count);
					for (int i = 0; i < list.Count; i++)
					{
						streamWriter.WriteLine(list[i].ToString());
					}
					streamWriter.WriteLine("");
					for (int j = 0; j < list2.Count; j++)
					{
						streamWriter.WriteLine(list2[j].ToString());
					}
				}
				flag = true;
			}
			catch (Exception ex)
			{
				MessageBox.Show(ex.Message);
				MessageBox.Show("Shape.obj file either does not exist, or has parsing issues, for " + Model + "\r\n\r\nWill now try to retrieve the Shape data file", "No model", (MessageBoxButtons)0, (MessageBoxIcon)16);
			}
			if (!flag)
			{
				list.Clear();
				list2.Clear();
				httpWebRequest = (HttpWebRequest)WebRequest.Create($"https://astro.troja.mff.cuni.cz/projects/damit/asteroid_models/view/{Model}");
				httpWebRequest.Method = "GET";
				try
				{
					using WebResponse webResponse2 = httpWebRequest.GetResponse();
					using (Stream stream2 = webResponse2.GetResponseStream())
					{
						using StreamReader streamReader2 = new StreamReader(stream2);
						string text2 = streamReader2.ReadToEnd();
						int startIndex = text2.IndexOf("var vertexes");
						startIndex = text2.IndexOf("[", startIndex);
						int startIndex2 = text2.IndexOf("var faces", startIndex);
						startIndex2 = text2.LastIndexOf(",", startIndex2);
						int num = text2.IndexOf("[", startIndex2);
						int startIndex3 = text2.IndexOf("</script>", num);
						startIndex3 = text2.LastIndexOf(",", startIndex3);
						string[] array3 = text2.Substring(startIndex + 1, startIndex2 - startIndex - 1).Replace("], [", ";").Split(new char[1] { ';' });
						for (int k = 0; k < array3.Length; k++)
						{
							Vector vector2 = new Vector();
							string[] array4 = array3[k].Replace("[", "").Replace("]", "").Split(new char[1] { ',' });
							vector2.X = double.Parse(array4[0]);
							vector2.Y = double.Parse(array4[1]);
							vector2.Z = double.Parse(array4[2]);
							list.Add(vector2);
						}
						string[] array5 = text2.Substring(num + 1, startIndex3 - num - 1).Replace("], [", ";").Split(new char[1] { ';' });
						for (int l = 0; l < array5.Length; l++)
						{
							Face face2 = new Face();
							string[] array6 = array5[l].Replace("[", "").Replace("]", "").Split(new char[1] { ',' });
							face2.F1 = int.Parse(array6[0]);
							face2.F2 = int.Parse(array6[1]);
							face2.F3 = int.Parse(array6[2]);
							list2.Add(face2);
						}
						using StreamWriter streamWriter2 = new StreamWriter(ShapeModelDirectory + Model + ".txt");
						streamWriter2.WriteLine(list.Count + ", " + list2.Count);
						for (int m = 0; m < list.Count; m++)
						{
							streamWriter2.WriteLine(list[m].ToString());
						}
						streamWriter2.WriteLine("");
						for (int n = 0; n < list2.Count; n++)
						{
							streamWriter2.WriteLine(list2[n].ToString());
						}
					}
					return true;
				}
				catch (Exception ex2)
				{
					MessageBox.Show(ex2.Message);
					MessageBox.Show("Shape.txt file does not exist for " + Model, "No model", (MessageBoxButtons)0, (MessageBoxIcon)16);
					return false;
				}
			}
			return flag;
		}

		internal static bool ManuallyConvertDAMITmodel()
		{
			//IL_003b: Unknown result type (might be due to invalid IL or missing references)
			//IL_005d: Unknown result type (might be due to invalid IL or missing references)
			//IL_006c: Unknown result type (might be due to invalid IL or missing references)
			//IL_008e: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ac: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d9: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e8: Expected O, but got Unknown
			//IL_0112: Unknown result type (might be due to invalid IL or missing references)
			//IL_0118: Invalid comparison between Unknown and I4
			//IL_01c9: Unknown result type (might be due to invalid IL or missing references)
			//IL_01e5: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ec: Expected O, but got Unknown
			//IL_0216: Unknown result type (might be due to invalid IL or missing references)
			//IL_021c: Invalid comparison between Unknown and I4
			//IL_02ac: Unknown result type (might be due to invalid IL or missing references)
			//IL_02f9: Unknown result type (might be due to invalid IL or missing references)
			//IL_036f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0375: Invalid comparison between Unknown and I4
			List<Vector> list = new List<Vector>();
			List<Face> list2 = new List<Face>();
			int result = 9999;
			int result2 = 3696;
			int num = 0;
			string input = "";
			string input2 = "";
			string value = "";
			Utilities.InputDialog("Specify Asteroid number", ref input);
			int.TryParse(input, out result2);
			if (result2 < 0)
			{
				MessageBox.Show("Invalid Asteroid. The model must be a numbered asteroid", "Invalid asteroid", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return false;
			}
			Utilities.InputDialog("Specify Model number (>= 100000) to use. ", ref input2);
			int.TryParse(input2, out result);
			if (result < 0)
			{
				MessageBox.Show("Invalid Model. The model must be a number >= 100000", "Invalid model", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return false;
			}
			if (result < 100000)
			{
				MessageBox.Show("Invalid Model. The model number for a manually added model must be 100000 or greater", "Invalid model number", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return false;
			}
			if (File.Exists(ShapeModelDirectory + input2 + ".txt"))
			{
				MessageBox.Show("A file already exists for this model", "Invalid model", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return false;
			}
			OpenFileDialog val = new OpenFileDialog();
			((FileDialog)val).set_Title("Specify the DAMIT Shape data file.");
			((FileDialog)val).set_Filter("tri files (*.tri)|*.tri|All files (*.*)|*.*");
			((FileDialog)val).set_FilterIndex(0);
			val.set_Multiselect(false);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				string fileName = ((FileDialog)val).get_FileName();
				using (StreamReader streamReader = new StreamReader(((FileDialog)val).get_FileName()))
				{
					string text = streamReader.ReadLine();
					text = streamReader.ReadLine();
					if (text.Length > 40)
					{
						try
						{
							text = text.Replace("v", "").Replace("f", "").Trim();
							int num2 = text.IndexOf(" ");
							string text2 = text.Substring(0, num2).Trim();
							int num3 = text.LastIndexOf(" ");
							string text3 = text.Substring(num3);
							string text4 = text.Substring(num2, num3 - num2);
						}
						catch
						{
							MessageBox.Show("The 2nd line of the file of DAMIT Shape data is not in the required format");
							return false;
						}
					}
				}
				OpenFileDialog val2 = new OpenFileDialog();
				((FileDialog)val2).set_Title("Specify the DAMIT Spin parameters file.");
				((FileDialog)val2).set_Filter("All files (*.*)|*.*");
				((FileDialog)val2).set_FilterIndex(0);
				val2.set_Multiselect(false);
				if ((int)((CommonDialog)val2).ShowDialog() == 1)
				{
					string fileName2 = ((FileDialog)val2).get_FileName();
					string[] array;
					using (StreamReader streamReader2 = new StreamReader(((FileDialog)val2).get_FileName()))
					{
						string text5 = "The file of Spin data does not provide values for one or more of lambda, beta, Period & JD in the first 4 fields";
						array = streamReader2.ReadToEnd().Trim().Replace('\n', ' ')
							.Replace("  ", " ")
							.Replace("  ", " ")
							.Replace("  ", " ")
							.Replace("  ", " ")
							.Split(new char[1] { ' ' });
						if (array.Length < 5)
						{
							MessageBox.Show(text5);
							return false;
						}
						if (!double.TryParse(array[0], out var result3) | !double.TryParse(array[1], out result3) | !double.TryParse(array[2], out result3) | !double.TryParse(array[0], out result3))
						{
							MessageBox.Show(text5);
							return false;
						}
					}
					if ((int)MessageBox.Show(" You are going to convert Shape model file\r\n  " + fileName + "\r\n\r\n and the file containing rotation parameters\r\n  " + fileName2 + "\r\n\r\nUsing Asteroid number " + input + ",\r\n and model number " + input2 + "\r\n\r\nIs this correct?", "Confirm selected files", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
					{
						return false;
					}
					using (StreamReader streamReader3 = new StreamReader(((FileDialog)val).get_FileName()))
					{
						string text6 = streamReader3.ReadLine();
						do
						{
							text6 = streamReader3.ReadLine();
							if (text6.Length >= 3)
							{
								text6 = text6.Trim();
								if (text6.Length > 40)
								{
									text6 = text6.Replace("v", "").Replace("f", "").Trim();
									int num2 = text6.IndexOf(" ");
									string text2 = text6.Substring(0, num2).Trim();
									int num3 = text6.LastIndexOf(" ");
									string text3 = text6.Substring(num3);
									string text4 = text6.Substring(num2, num3 - num2);
									Vector vector = new Vector();
									vector.SetVectorValues(double.Parse(text2), double.Parse(text4), double.Parse(text3));
									list.Add(vector);
								}
								else
								{
									text6 = text6.Replace("v", "").Replace("f", "").Trim();
									int num2 = text6.IndexOf(" ", 1);
									string face = text6.Substring(0, num2).Trim();
									int num3 = text6.LastIndexOf(" ");
									string face2 = text6.Substring(num3).Trim();
									string face3 = text6.Substring(num2, num3 - num2).Trim();
									Face face4 = new Face();
									face4.SetFaces(face, face3, face2, Subtract1: true);
									list2.Add(face4);
								}
							}
						}
						while (!streamReader3.EndOfStream);
						using (StreamWriter streamWriter = new StreamWriter(ShapeModelDirectory + input2 + ".txt"))
						{
							streamWriter.WriteLine(list.Count + ", " + list2.Count);
							for (int i = 0; i < list.Count; i++)
							{
								streamWriter.WriteLine(list[i].ToString());
							}
							streamWriter.WriteLine("");
							for (int j = 0; j < list2.Count; j++)
							{
								streamWriter.WriteLine(list2[j].ToString());
							}
						}
						using (StreamReader streamReader4 = new StreamReader(((FileDialog)val2).get_FileName()))
						{
							array = streamReader4.ReadToEnd().Trim().Replace('\n', ' ')
								.Replace("  ", " ")
								.Replace("  ", " ")
								.Replace("  ", " ")
								.Replace("  ", " ")
								.Split(new char[1] { ' ' });
						}
						Model_Parameters model_Parameters = new Model_Parameters();
						num = (model_Parameters.AsteroidNumber = int.Parse(input));
						model_Parameters.ModelNumber_String = input2;
						model_Parameters.Lambda = array[0];
						model_Parameters.Beta = array[1];
						model_Parameters.Period = array[2];
						model_Parameters.YORP = "0";
						model_Parameters.JD0 = array[3];
						model_Parameters.Phi0 = "0";
						model_Parameters.Quality = "";
						model_Parameters.Version = "";
						model_Parameters.Comment = "Manual addition";
						model_Parameters.Created = "";
						model_Parameters.Modified = "";
						model_Parameters.DAMIT_Asteroid_ID = "9999";
						DAMITModelsByAsteroidNumber.Add(model_Parameters);
						value = model_Parameters.ToString();
					}
					string text7 = ShapeModelDirectory + "DamitModels" + DateTime.Now.ToShortDateString();
					for (int k = 0; k < 100; k++)
					{
						text7 = ShapeModelDirectory + "DamitModels " + DateTime.Now.ToShortDateString() + "_" + k + ".csv";
						if (!File.Exists(text7))
						{
							break;
						}
					}
					File.Move(ShapeModelDirectory + "DamitModels.csv", text7);
					string text8 = "";
					int num5 = 0;
					bool flag = false;
					using (StreamWriter streamWriter2 = new StreamWriter(ShapeModelDirectory + "DamitModels.csv"))
					{
						using StreamReader streamReader5 = new StreamReader(text7);
						do
						{
							text8 = streamReader5.ReadLine();
							num5 = int.Parse(text8.Split(new char[1] { ',' })[0]);
							if (!flag && num5 > num)
							{
								streamWriter2.WriteLine(value);
								flag = true;
							}
							streamWriter2.WriteLine(text8);
						}
						while (!streamReader5.EndOfStream);
						if (!flag)
						{
							streamWriter2.WriteLine(value);
						}
					}
					FillDAMITAsteroidModels();
					return true;
				}
				return false;
			}
			return false;
		}

		internal static void Initialise_ISAM_ShapeModels()
		{
			if (!Directory.Exists(ShapeModelDirectory))
			{
				Directory.CreateDirectory(ShapeModelDirectory);
			}
			ISAMmodelsByAsteroidNumber.Clear();
			if (File.Exists(FileOfISAMModels))
			{
				Fill_ISAM_AsteroidModels();
			}
		}

		internal static void Fill_ISAM_AsteroidModels()
		{
			ISAMmodelsByAsteroidNumber.Clear();
			if (File.Exists(FileOfISAMModels))
			{
				using StreamReader streamReader = new StreamReader(FileOfISAMModels);
				do
				{
					string[] array = streamReader.ReadLine()!.Split(new char[1] { ',' });
					Model_Parameters model_Parameters = new Model_Parameters();
					model_Parameters.AsteroidNumber = int.Parse(array[0]);
					model_Parameters.ModelNumber_String = array[1];
					model_Parameters.Lambda = array[2];
					model_Parameters.Beta = array[3];
					model_Parameters.Period = array[4];
					model_Parameters.YORP = array[5];
					model_Parameters.JD0 = array[6];
					model_Parameters.Phi0 = array[7];
					model_Parameters.Quality = array[8];
					model_Parameters.Version = array[9];
					model_Parameters.Comment = array[10];
					model_Parameters.Created = array[11];
					model_Parameters.Modified = array[12];
					model_Parameters.DAMIT_Asteroid_ID = array[13];
					if (array[10].Contains("Version"))
					{
						Extract_Version_Created_FromComment(array[10].Trim(), out var Version, out var Comment, out var Created);
						model_Parameters.Version = Version;
						model_Parameters.Comment = Comment;
						model_Parameters.Created = Created;
					}
					ISAMmodelsByAsteroidNumber.Add(model_Parameters);
				}
				while (!streamReader.EndOfStream);
			}
			Model_Parameters.Sort_0bysteroidNumber_1byModelNumberNumeric_2forISAM = 2;
			ISAMmodelsByAsteroidNumber.Sort();
		}

		internal static string ISAM_Model_Number(int AsteroidNumber, int ModelNumber)
		{
			return "A" + AsteroidNumber + "-" + ModelNumber;
		}

		internal static bool RefreshISAMDownoadedModels()
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_0026: Unknown result type (might be due to invalid IL or missing references)
			//IL_0310: Unknown result type (might be due to invalid IL or missing references)
			//IL_0316: Invalid comparison between Unknown and I4
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
				return false;
			}
			MessageBox.Show("Before running this option, you need to have an \r\nup-to-date file of ISAM shape models.\r\n\r\nTo update that file (ShapeModels\\ISAM_AvailableAsteroids.csv)\r\nuse the menu item \r\nCreate list of available ISAM shape models not in DAMIT", "Missing file", (MessageBoxButtons)0, (MessageBoxIcon)48);
			_ = Directory.GetFiles(ShapeModelDirectory, "A*.*").Length;
			MessageForm messageForm = new MessageForm();
			((Control)messageForm).Show();
			((Control)messageForm.cmdCancel).set_Visible(true);
			((Control)messageForm.cmdExit).set_Visible(false);
			PBar pBar = new PBar();
			pBar.pBarFTP.set_Minimum(0);
			pBar.pBarFTP.set_Value(0);
			((Control)pBar).Show();
			int num = 0;
			bool flag = false;
			FailedCount = 0;
			if (!Directory.Exists(ShapeModelDirectory_Extras))
			{
				Directory.CreateDirectory(ShapeModelDirectory_Extras);
			}
			http.DownloadHTTP(Settings.Default.OccultServer, "shapemodels_isam_extra.zip", ShapeModelDirectory_Extras, unzip: true, gunzip: false, ShowMessages: false);
			File.Delete(FileOfISAMModels);
			string[] array;
			using (StreamReader streamReader = new StreamReader(ShapeModelDirectory_Extras + "\\ExtraModels.txt"))
			{
				array = streamReader.ReadToEnd().Replace("\r\n", "").Split(new char[1] { ',' });
				using StreamWriter streamWriter = new StreamWriter(FileOfISAMModels);
				for (int i = 0; i < array.Length; i++)
				{
					string path = ShapeModelDirectory_Extras + "\\" + array[i] + "_Params.txt";
					streamWriter.Write(new StreamReader(path).ReadToEnd());
					string text = "\\" + array[i] + ".txt";
					File.Copy(ShapeModelDirectory_Extras + text, ShapeModelDirectory + text, overwrite: true);
				}
			}
			using (StreamReader streamReader2 = new StreamReader(Utilities.AppPath + "\\ShapeModels\\ISAM_AvailableAsteroids.csv"))
			{
				string s = streamReader2.ReadLine()!.Substring(4);
				pBar.pBarFTP.set_Maximum(int.Parse(s) + 1);
				do
				{
					string[] array2 = streamReader2.ReadLine()!.Split(new char[1] { ',' });
					int num2 = int.Parse(array2[0]);
					int num3 = int.Parse(array2[1]);
					bool flag2 = false;
					for (int j = 0; j < array.Length; j++)
					{
						if (num2 == int.Parse(array[j].Substring(0, array[j].IndexOf("-")).Replace("A", "")))
						{
							flag2 = true;
						}
					}
					if (flag2)
					{
						continue;
					}
					int[] array3 = new int[num3];
					for (int k = 0; k < num3; k++)
					{
						array3[k] = int.Parse(array2[2 * k + 2]);
					}
					num++;
					((Control)messageForm.label).set_Text($"Downloading {num3} models for asteroid #{num2}");
					for (int l = 0; l < num3; l++)
					{
						Add_Additional_ISAM_Model(num2, array3[l], initialiseISAM: false);
						if ((FailedCount == 5) | (FailedCount == 50))
						{
							string text2 = ((FailedCount != 5) ? "There have been 50 unsuccessful downloads of shape models, where the ISAM site has reported 'Query failed'. This is a definite indication of problems with the ISAM site, and continuing with the download will be a waste of time." : "There have been 5 unsuccessful downloads of shape models, where the ISAM site has reported 'Query failed'. This may indicate a problem with the ISAM site.");
							if ((int)MessageBox.Show(text2 + "\r\n\r\nDo you want to continue with the downloads?", "ISAM site problems", (MessageBoxButtons)4, (MessageBoxIcon)16, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
							{
								flag = true;
								break;
							}
						}
						if (flag)
						{
							break;
						}
					}
					Application.DoEvents();
					if (messageForm.Cancel || flag)
					{
						break;
					}
					pBar.pBarFTP.set_Value(num);
					Application.DoEvents();
				}
				while (!streamReader2.EndOfStream);
				((Form)messageForm).Close();
				((Form)pBar).Close();
			}
			return true;
		}

		internal static bool Add_Additional_ISAM_Model(int Asteroid, int ModelNumber, bool initialiseISAM)
		{
			//IL_001e: Unknown result type (might be due to invalid IL or missing references)
			//IL_01dd: Unknown result type (might be due to invalid IL or missing references)
			if (Asteroid == 1 && ModelNumber == 1)
			{
				return false;
			}
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
				return false;
			}
			if (initialiseISAM)
			{
				if (!Directory.Exists(ShapeModelDirectory_Extras))
				{
					Directory.CreateDirectory(ShapeModelDirectory_Extras);
				}
				http.DownloadHTTP(Settings.Default.OccultServer, "shapemodels_isam_extra.zip", Utilities.AppPath + "\\ShapeModels\\Extras", unzip: true, gunzip: false, ShowMessages: false);
				Initialise_ISAM_ShapeModels();
			}
			Model_Parameters ModelParameters = new Model_Parameters();
			ModelParameters.AsteroidNumber = Asteroid;
			ModelParameters.ModelNumber_String = ISAM_Model_Number(ModelParameters.AsteroidNumber, ModelNumber);
			ModelParameters.DAMIT_Asteroid_ID = "";
			ModelParameters.Modified = "";
			ModelParameters.Quality = "";
			ModelParameters.Created = "";
			ModelParameters.YORP = "0";
			List<Vector> vertices = new List<Vector>();
			List<Face> faces = new List<Face>();
			bool flag = false;
			HttpWebRequest httpWebRequest = (HttpWebRequest)WebRequest.Create($"http://isam.astro.amu.edu.pl/model.php?nr_planet={Asteroid}&nr_modelu={ModelNumber}");
			try
			{
				using (WebResponse webResponse = httpWebRequest.GetResponse())
				{
					using Stream iSAMstream = webResponse.GetResponseStream();
					flag = Parse_ISAM_ShapeModel_File(Asteroid, ModelNumber, ref ModelParameters, vertices, faces, iSAMstream);
				}
				if (flag)
				{
					ISAMmodelsByAsteroidNumber.Add(ModelParameters);
					Model_Parameters.Sort_0bysteroidNumber_1byModelNumberNumeric_2forISAM = 0;
					ISAMmodelsByAsteroidNumber.Sort();
					using (StreamWriter streamWriter = new StreamWriter(FileOfISAMModels))
					{
						for (int i = 0; i < ISAMmodelsByAsteroidNumber.Count; i++)
						{
							streamWriter.WriteLine(ISAMmodelsByAsteroidNumber[i].ToString());
						}
					}
					Model_Parameters.Sort_0bysteroidNumber_1byModelNumberNumeric_2forISAM = 2;
					ISAMmodelsByAsteroidNumber.Sort();
				}
				else
				{
					FailedCount++;
				}
			}
			catch (Exception ex)
			{
				MessageBox.Show("The ISAM Shape.obj file either does not exist, or has parsing issues, for " + ModelParameters.ModelNumber_String + "\r\n\r\n" + ex.Message, "No model", (MessageBoxButtons)0, (MessageBoxIcon)16);
			}
			return true;
		}

		private static void Extract_Version_Created_FromComment(string Source, out string Version, out string Comment, out string Created)
		{
			Version = (Created = "");
			Comment = Source;
			int num = Source.IndexOf("(Version:");
			if (num > 0)
			{
				Version = (Created = Source.Substring(num + 10, 10));
				Comment = Source.Remove(num, 20);
				Comment = Comment.Substring(0, Comment.Length - 1).Trim();
				if (Comment.Length > num)
				{
					Comment = Comment.Insert(num - 1, ";");
				}
				Comment = Comment.Replace("   ", " ").Replace("  ", " ");
			}
		}

		private static bool Parse_ISAM_ShapeModel_File(int AsteroidNumber, int ModelNumber, ref Model_Parameters ModelParameters, List<Vector> Vertices, List<Face> Faces, Stream ISAMstream)
		{
			bool flag = true;
			using StreamReader streamReader = new StreamReader(ISAMstream);
			do
			{
				string text = streamReader.ReadLine();
				if (text.Length == 0 || text.Contains("#target"))
				{
					continue;
				}
				if (text.Contains("failed"))
				{
					flag = false;
					break;
				}
				if (text.Contains("#method"))
				{
					Extract_Version_Created_FromComment(text.Substring(9).Trim(), out var Version, out var Comment, out var Created);
					ModelParameters.Version = Version;
					ModelParameters.Comment = Comment;
					ModelParameters.Created = Created;
				}
				else if (text.Contains("#period"))
				{
					ModelParameters.Period = text.Substring(11).Trim();
				}
				else if (text.Contains("#lambda"))
				{
					ModelParameters.Lambda = text.Substring(8).Trim();
				}
				else if (text.Contains("#beta"))
				{
					ModelParameters.Beta = text.Substring(6).Trim();
				}
				else if (text.Contains("#gamma"))
				{
					ModelParameters.Phi0 = text.Substring(7).Trim();
				}
				else if (text.Contains("#jd_gamma0"))
				{
					ModelParameters.JD0 = text.Substring(12).Trim();
				}
				else if (!text.Contains("#yorp"))
				{
					if (text.Substring(0, 1) == "v")
					{
						string[] array = Utilities.SplitStringWithCommas(text.Substring(1).Trim().Replace('\t', ' ')).Split(new char[1] { ',' });
						Vector vector = new Vector();
						vector.SetVectorValues(double.Parse(array[0]), double.Parse(array[1]), double.Parse(array[2]));
						Vertices.Add(vector);
					}
					else if (text.Substring(0, 1) == "f")
					{
						string[] array2 = Utilities.SplitStringWithCommas(text.Substring(1).Trim().Replace('\t', ' ')).Split(new char[1] { ',' });
						Face face = new Face();
						face.SetFaces(array2[0], array2[1], array2[2], Subtract1: true);
						Faces.Add(face);
					}
				}
			}
			while (!streamReader.EndOfStream);
			if (flag)
			{
				using (StreamWriter streamWriter = new StreamWriter(ShapeModelDirectory + ISAM_Model_Number(AsteroidNumber, ModelNumber) + ".txt"))
				{
					streamWriter.WriteLine(Vertices.Count + ", " + Faces.Count);
					for (int i = 0; i < Vertices.Count; i++)
					{
						streamWriter.WriteLine(Vertices[i].ToString());
					}
					streamWriter.WriteLine("");
					for (int j = 0; j < Faces.Count; j++)
					{
						streamWriter.WriteLine(Faces[j].ToString());
					}
					return flag;
				}
			}
			return flag;
		}

		internal static void ManuallyAddToISAM()
		{
			//IL_0029: Unknown result type (might be due to invalid IL or missing references)
			//IL_002f: Invalid comparison between Unknown and I4
			int[] array = new int[3] { 1, 4, 486958 };
			Clipboard.SetText("1,1,1,2016-01-01  \r\n 4,1,1,2016-01-01\r\n 486958,1,1,2016-01-01");
			if ((int)MessageBox.Show(" This option is to manually add Ceres, Vesta and Arrokoth shape models\r\nthat are not on the ISAM Site.\r\n\r\nThe first step is to ensure the shape model file(s) are:\r\n  d:\\Astronomy\\Catalogues\\DAWN CeresShape_4_01\\Ceres Dawn.obj.txt  .. and\r\n  d:\\Astronomy\\Catalogues\\DAWN VestaShape\\Vesta DAWNobj.txt\r\n\r\nThe 2nd step is to run this routine.\r\n\r\nThe final step is to manually edit the file \r\nShapeModels\\ISAM_AvailableAsteroids.csv\r\nto include entries for these asteroids. The entries are1,1,1,2016-01-01 \r\n4,1,1,2016-01-01 \r\n486958, 1,1, 2016-01-01These have been copied to the Clipboard for retrieval\r\n\r\nDo you want to continue?", "Add Ceres and Vesta", (MessageBoxButtons)4, (MessageBoxIcon)32) == 7)
			{
				return;
			}
			string text = "";
			for (int i = 0; i < array.Length; i++)
			{
				int num = array[i];
				int asteroidNumber = num;
				int modelNumber = 110;
				text = "d:\\Astronomy\\Catalogues\\ShapeModelsToAdd\\" + num + ".obj.txt";
				Initialise_ISAM_ShapeModels();
				Model_Parameters.Sort_0bysteroidNumber_1byModelNumberNumeric_2forISAM = 0;
				ISAMmodelsByAsteroidNumber.Sort();
				bool flag = false;
				for (int j = 0; j < 10; j++)
				{
					if (ISAMmodelsByAsteroidNumber[j].ModelNumber_String == "A" + num + "-110")
					{
						flag = true;
						break;
					}
				}
				if (flag)
				{
					continue;
				}
				Model_Parameters model_Parameters = new Model_Parameters();
				model_Parameters.AsteroidNumber = asteroidNumber;
				model_Parameters.ModelNumber_String = ISAM_Model_Number(model_Parameters.AsteroidNumber, modelNumber);
				model_Parameters.DAMIT_Asteroid_ID = "";
				model_Parameters.Modified = "";
				model_Parameters.Quality = "";
				model_Parameters.Created = "";
				model_Parameters.YORP = "0";
				List<Vector> list = new List<Vector>();
				List<Face> list2 = new List<Face>();
				using (StreamReader streamReader = new StreamReader(text))
				{
					do
					{
						string text2 = streamReader.ReadLine();
						if (text2.Length == 0 || text2.Contains("#target"))
						{
							continue;
						}
						if (text2.Contains("#method"))
						{
							Extract_Version_Created_FromComment(text2.Substring(9).Trim(), out var Version, out var Comment, out var Created);
							model_Parameters.Version = Version;
							model_Parameters.Comment = Comment;
							model_Parameters.Created = Created;
						}
						else if (text2.Contains("#period"))
						{
							model_Parameters.Period = text2.Substring(11).Trim();
						}
						else if (text2.Contains("#lambda"))
						{
							model_Parameters.Lambda = text2.Substring(8).Trim();
						}
						else if (text2.Contains("#beta"))
						{
							model_Parameters.Beta = text2.Substring(6).Trim();
						}
						else if (text2.Contains("#gamma"))
						{
							model_Parameters.Phi0 = text2.Substring(7).Trim();
						}
						else if (text2.Contains("#jd_gamma0"))
						{
							model_Parameters.JD0 = text2.Substring(12).Trim();
						}
						else if (!text2.Contains("#yorp"))
						{
							if (text2.Substring(0, 1) == "v")
							{
								string[] array2 = Utilities.SplitStringWithCommas(text2.Substring(1).Trim().Replace('\t', ' ')).Split(new char[1] { ',' });
								Vector vector = new Vector();
								vector.SetVectorValues(double.Parse(array2[0]), double.Parse(array2[1]), double.Parse(array2[2]));
								list.Add(vector);
							}
							else if (text2.Substring(0, 1) == "f")
							{
								string[] array3 = Utilities.SplitStringWithCommas(text2.Substring(1).Trim().Replace('\t', ' ')).Split(new char[1] { ',' });
								Face face = new Face();
								face.SetFaces(array3[0], array3[1], array3[2], Subtract1: true);
								list2.Add(face);
							}
						}
					}
					while (!streamReader.EndOfStream);
				}
				using (StreamWriter streamWriter = new StreamWriter(ShapeModelDirectory + ISAM_Model_Number(asteroidNumber, modelNumber) + ".txt"))
				{
					streamWriter.WriteLine(list.Count + ", " + list2.Count);
					for (int k = 0; k < list.Count; k++)
					{
						streamWriter.WriteLine(list[k].ToString());
					}
					streamWriter.WriteLine("");
					for (int l = 0; l < list2.Count; l++)
					{
						streamWriter.WriteLine(list2[l].ToString());
					}
				}
				ISAMmodelsByAsteroidNumber.Add(model_Parameters);
				Model_Parameters.Sort_0bysteroidNumber_1byModelNumberNumeric_2forISAM = 0;
				ISAMmodelsByAsteroidNumber.Sort();
				using (StreamWriter streamWriter2 = new StreamWriter(FileOfISAMModels))
				{
					for (int m = 0; m < ISAMmodelsByAsteroidNumber.Count; m++)
					{
						streamWriter2.WriteLine(ISAMmodelsByAsteroidNumber[m].ToString());
					}
				}
				Model_Parameters.Sort_0bysteroidNumber_1byModelNumberNumeric_2forISAM = 2;
				ISAMmodelsByAsteroidNumber.Sort();
			}
		}
	}
}
