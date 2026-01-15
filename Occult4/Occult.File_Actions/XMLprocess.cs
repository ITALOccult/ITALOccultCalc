using System;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Windows.Forms;
using System.Xml;
using Occult.Asteroids;

namespace Occult.File_Actions
{
	public class XMLprocess
	{
		internal static int ErrorCount;

		internal static bool GetMiraideEphemeris(int AsteroidIDnumber, double EventReferenceTime, ref List<BinaryAsteroidOffsets> BinaryOffsets, bool SaveMiriadeResponse)
		{
			//IL_0144: Unknown result type (might be due to invalid IL or missing references)
			if (ErrorCount > 2)
			{
				return false;
			}
			Cursor.set_Current(Cursors.get_WaitCursor());
			bool result = false;
			string text = "";
			if (Utilities.InternetIsAvailable())
			{
				string requestUriString = "https://ssp.imcce.fr/webservices/miriade/api/ephemsys.php?-name=a:" + AsteroidIDnumber + "&-ep=" + EventReferenceTime + "&-tscale=TT&-gensol=0&-mime=votable&-from=Occult_v." + Utilities.OccultVersion_Short;
				try
				{
					ServicePointManager.SecurityProtocol = SecurityProtocolType.Ssl3 | SecurityProtocolType.Tls | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls12;
					HttpWebRequest obj = (HttpWebRequest)WebRequest.Create(requestUriString);
					obj.Timeout = 8000;
					obj.Method = "GET";
					using (WebResponse webResponse = obj.GetResponse())
					{
						using Stream stream = webResponse.GetResponseStream();
						using StreamReader streamReader = new StreamReader(stream);
						text = streamReader.ReadToEnd();
					}
					if (SaveMiriadeResponse)
					{
						using StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Downloaded Files\\LastMiriadeResponse.xml");
						streamWriter.Write(text);
					}
					ReadXML(text, ref BinaryOffsets);
					result = true;
					ErrorCount = 0;
				}
				catch (Exception ex)
				{
					ErrorCount++;
					if (ErrorCount > 2)
					{
						MessageBox.Show("Error requesting Miriade data : \r\n\r\n" + ex.Message, "Error");
					}
				}
			}
			Cursor.set_Current(Cursors.get_Default());
			return result;
		}

		internal static void ReadXML(string DownloadText, ref List<BinaryAsteroidOffsets> X)
		{
			string solutionType = "";
			string solutionID = "";
			double result = 0.0;
			double result2 = 0.0;
			double result3 = 0.0;
			double result4 = 0.0;
			List<string> list = new List<string>();
			XmlDocument xmlDocument = new XmlDocument();
			xmlDocument.PreserveWhitespace = false;
			xmlDocument.LoadXml(DownloadText);
			XmlNamespaceManager xmlNamespaceManager = new XmlNamespaceManager(xmlDocument.NameTable);
			xmlNamespaceManager = new XmlNamespaceManager(xmlDocument.NameTable);
			xmlNamespaceManager.AddNamespace("vot", "http://www.ivoa.net/xml/VOTable/v1.3");
			XmlNodeList xmlNodeList = xmlDocument.DocumentElement!.SelectNodes("//vot:RESOURCE", xmlNamespaceManager);
			if (xmlNodeList.Count == 0)
			{
				xmlNamespaceManager = new XmlNamespaceManager(xmlDocument.NameTable);
				xmlNamespaceManager.AddNamespace("vot", "http://vo.imcce.fr/xml/VOTable/v1.3");
				xmlNodeList = xmlDocument.DocumentElement!.SelectNodes("//vot:RESOURCE", xmlNamespaceManager);
			}
			foreach (XmlNode item in xmlNodeList)
			{
				foreach (XmlNode item2 in item)
				{
					if (item2.InnerText.Contains("compute the orbit"))
					{
						foreach (XmlAttribute item3 in item2.Attributes!)
						{
							if (item3.Name == "value")
							{
								solutionType = item3.Value;
							}
						}
					}
					if (!item2.InnerText.Contains("ID of the Genoide"))
					{
						continue;
					}
					foreach (XmlAttribute item4 in item2.Attributes!)
					{
						if (item4.Name == "value")
						{
							solutionID = item4.Value;
						}
					}
				}
				foreach (XmlNode item5 in item.SelectNodes("vot:TABLE", xmlNamespaceManager)!)
				{
					BinaryAsteroidOffsets binaryAsteroidOffsets = new BinaryAsteroidOffsets();
					binaryAsteroidOffsets.SolutionType = solutionType;
					binaryAsteroidOffsets.SolutionID = solutionID;
					foreach (XmlNode item6 in item5.SelectNodes("vot:PARAM", xmlNamespaceManager)!)
					{
						if (item6.InnerText.Contains("Name of the component"))
						{
							foreach (XmlAttribute item7 in item6.Attributes!)
							{
								if (!(item7.Name == "value"))
								{
									continue;
								}
								binaryAsteroidOffsets.ComponentName = item7.Value;
								int num = -1;
								for (int i = 0; i < list.Count; i++)
								{
									if (binaryAsteroidOffsets.ComponentName == list[i])
									{
										num = i;
									}
								}
								if (num < 0)
								{
									list.Add(binaryAsteroidOffsets.ComponentName);
									num = list.Count - 1;
								}
								binaryAsteroidOffsets.ComponentSeqNum = num;
							}
						}
						if (item6.InnerText.Contains("Mean equatorial radius"))
						{
							foreach (XmlAttribute item8 in item6.Attributes!)
							{
								if (item8.Name == "value")
								{
									binaryAsteroidOffsets.ComponentDiameter = 2.0 * double.Parse(item8.Value);
								}
							}
						}
						if (!item6.InnerText.Contains("Error on the mean equatorial radius"))
						{
							continue;
						}
						foreach (XmlAttribute item9 in item6.Attributes!)
						{
							if (item9.Name == "value")
							{
								binaryAsteroidOffsets.ComponentDiameterUncertainty = 2.0 * double.Parse(item9.Value);
							}
						}
					}
					XmlNodeList? xmlNodeList2 = item5.SelectNodes("vot:DATA/vot:TABLEDATA/vot:TR", xmlNamespaceManager);
					int num2 = 0;
					foreach (XmlNode item10 in xmlNodeList2!)
					{
						XmlNodeList childNodes = item10.ChildNodes;
						if (childNodes.Count < 20)
						{
							binaryAsteroidOffsets.Offset_JD[num2] = double.Parse(childNodes[0]!.InnerText);
							binaryAsteroidOffsets.Offset_RA_mas[num2] = double.Parse(childNodes[1]!.InnerText);
							binaryAsteroidOffsets.Offset_Dec_mas[num2] = double.Parse(childNodes[2]!.InnerText);
							if (num2 == 1)
							{
								double.TryParse(childNodes[4]!.InnerText, out result);
								double.TryParse(childNodes[5]!.InnerText, out result2);
								double.TryParse(childNodes[6]!.InnerText, out result3);
								double.TryParse(childNodes[7]!.InnerText, out result4);
							}
						}
						else
						{
							string[] array = item10.InnerText.Split(new char[1] { '\n' });
							binaryAsteroidOffsets.Offset_JD[num2] = double.Parse(array[1]);
							binaryAsteroidOffsets.Offset_RA_mas[num2] = double.Parse(array[2]);
							binaryAsteroidOffsets.Offset_Dec_mas[num2] = double.Parse(array[3]);
							if (num2 == 1)
							{
								double.TryParse(array[4], out result);
								double.TryParse(array[5], out result2);
								double.TryParse(array[6], out result3);
								double.TryParse(array[7], out result4);
							}
						}
						if (num2 == 1)
						{
							binaryAsteroidOffsets.ErrorEllipseOffsetRA_mas = (result + result3) / 2.0;
							binaryAsteroidOffsets.ErrorEllipseOffsetDec_mas = (result2 + result4) / 2.0;
							double num3 = Math.Abs((result3 - result) / 2.0);
							double num4 = Math.Abs((result4 - result2) / 2.0);
							binaryAsteroidOffsets.UncertaintyInX_mas = num3;
							binaryAsteroidOffsets.UncertaintyInY_mas = num4;
							binaryAsteroidOffsets.SigmaMajor_mas = Math.Sqrt(num3 * num3 + num4 * num4);
							if (binaryAsteroidOffsets.SigmaMajor_mas > 0.0)
							{
								if (num3 > num4)
								{
									binaryAsteroidOffsets.SigmaMajor_mas = num3;
									binaryAsteroidOffsets.SigmaMinor_mas = num4;
									binaryAsteroidOffsets.SigmaPAMajor_deg = 90.0;
								}
								else
								{
									binaryAsteroidOffsets.SigmaMajor_mas = num4;
									binaryAsteroidOffsets.SigmaMinor_mas = num3;
									binaryAsteroidOffsets.SigmaPAMajor_deg = 0.0;
								}
							}
							else
							{
								double num6 = (binaryAsteroidOffsets.SigmaPAMajor_deg = 0.0);
								double num8 = (binaryAsteroidOffsets.SigmaMinor_mas = num6);
								double num11 = (binaryAsteroidOffsets.SigmaMinor_mas = (binaryAsteroidOffsets.SigmaMajor_mas = num8));
							}
						}
						num2++;
					}
					X.Add(binaryAsteroidOffsets);
				}
			}
		}
	}
}
